#include "StdAfx.h"

#include "Actor.h"
#include "ImUtils.h"
#include "../HUDManager.h"
#include "../HUDTarget.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "../../xrEngine/pure.h"
#include "../../xrEngine/ImGuiManager.h"
#include "../../xrCore/Kernel/EngineExternal.h"
#include "imgui_internal.h"
#include <filesystem>

CBlend* PlayMotionByParts(IKinematicsAnimated* sa, MotionID motion_ID, bool bMixIn, PlayCallback Callback, LPVOID CallbackParam);

namespace CinemaFilmMaker
{
constexpr const char* kCinemaClipPayload = "CINEMA_ANIMATION";
constexpr const char* kCinemaClipIndexPayload = "CINEMA_ANIMATION_INDEX";
constexpr const char* kCinemaSceneFolder = "cinema_scenes\\";
constexpr const char* kCinemaSceneExt = ".ltx";
constexpr s32 kCinemaSceneVersion = 1;
constexpr const char* kCinemaSettingsFile = "cinema_editor\\settings.ltx";
constexpr const char* kCinemaPresetsFile = "cinema_editor\\presets.ltx";
constexpr s32 kCinemaPresetVersion = 1;
constexpr u32 kCinemaMaxPresets = 128;
constexpr float kCinemaMinGroundNormalY = 0.5f;
constexpr u32 kCinemaSceneMaxActors = 1024;
constexpr u32 kCinemaSceneMaxPlaylist = 256;

enum class EGizmoMode : u8
{
	Translate,
	Rotate
};

enum class EGizmoAxis : u8
{
	None,
	X,
	Y,
	Z
};

struct SAnimationClip
{
	shared_str Name;
	bool Fx = false;
};

struct SAnimationSource
{
	shared_str Id;
	u32 BoneCount = 0;
	xr_vector<shared_str> BoneNames;
	xr_vector<SAnimationClip> Clips;
};

struct SPlaylistEntry
{
	shared_str Source;
	shared_str Name;
	bool Fx = false;
};

struct STrimSegment
{
	float Start = 0.0f;
	float End = 0.0f;
};

struct STrimRange
{
	shared_str Source;
	shared_str Name;
	xr_vector<STrimSegment> Segments;
	float Speed = 1.0f;
	float Blend = 0.0f;
};
struct SHandItem
{
	shared_str Section;
	shared_str Visual;
};

struct SNpcPreset
{
	xr_string Name;
	shared_str Visual;
	shared_str Item;
	Fvector Scale = Fvector().set(1.0f, 1.0f, 1.0f); 
	bool Loop = false;
	float Speed = 1.0f;
	xr_vector<SPlaylistEntry> Playlist;
	xr_vector<STrimRange> Trims;
	bool PingPong = false;
};

struct SSceneActorData
{
	xr_string Name;
	shared_str Visual;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Rotation = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Scale = Fvector().set(1.0f, 1.0f, 1.0f);
	bool Loop = false;
	float Speed = 1.0f;
	shared_str Item;
	xr_vector<SPlaylistEntry> Playlist;
	xr_vector<STrimRange> Trims;
	bool PingPong = false;
	shared_str PoseSource;
	shared_str PoseClip;
	bool PoseFx = false;
	float PoseTime = 0.0f;
	bool PosePaused = false;
};

struct SClipDragPayload
{
	string_path Source = {};
	string_path Name = {};
	bool Fx = false;
};

struct SIndexDragPayload
{
	s32 Index = -1;
};

struct SToolbarLayout
{
	explicit SToolbarLayout(float available)
		: Available(available)
	{
	}

	void Item(float width)
	{
		const float spacing = ImGui::GetStyle().ItemSpacing.x;

		if (!HasAny)
		{
			HasAny = true;
			RowWidth = width;
			return;
		}

		if (RowWidth + spacing + width > Available)
		{
			ImGui::NewLine();
			RowWidth = width;
			return;
		}

		ImGui::SameLine();
		RowWidth += spacing + width;
	}

	float Available = 0.0f;
	float RowWidth = 0.0f;
	bool HasAny = false;
};

static float CinemaButtonWidth(const char* label)
{
	return ImGui::CalcTextSize(label).x + ImGui::GetStyle().FramePadding.x * 2.0f;
}

static const char* CinemaFileName(const char* path)
{
	if (path == nullptr)
	{
		return "";
	}

	const char* backslash = strrchr(path, '\\');
	const char* slash = strrchr(path, '/');
	const char* last = backslash != nullptr && slash != nullptr ? (backslash > slash ? backslash : slash) : (backslash != nullptr ? backslash : slash);

	return last != nullptr ? last + 1 : path;
}

struct SProjectedPoint
{
	ImVec2 Screen = ImVec2(0.0f, 0.0f);
	float W = 0.0f;
	bool Visible = false;
};

struct SCinemaActor;

struct SGizmoDrag
{
	bool Active = false;
	SCinemaActor* Actor = nullptr;
	EGizmoMode Mode = EGizmoMode::Translate;
	EGizmoAxis Axis = EGizmoAxis::None;
	Fvector AxisDir = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector Pivot = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector StartPosition = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix StartOrientation;
	float StartParam = 0.0f;
	float LastAngle = 0.0f;
	float AccumulatedAngle = 0.0f;
	bool UseScreenAngle = false;
};

struct SBindBoneParam
{
	Fmatrix Matrix;
};

class CCinemaActorObject final : public CObject
{
public:
	IRenderVisual* ItemVisual = nullptr;
	Fmatrix ItemTransform;

	CCinemaActorObject()
	{
		cName_set("cinema_fake_npc");
		cNameSect_set("cinema_fake_npc");
		SpatialComponent->type = ESPATIAL_TYPE::RENDERABLE;
	}

	~CCinemaActorObject() override
	{
		renderable.visual = nullptr;
	}

	void Bind(IRenderVisual* visual, const Fmatrix& transform)
	{
		if (visual == nullptr)
		{
			Unbind();
			return;
		}

		renderable.visual = visual;
		renderable.xform = transform;

		if (InSpace)
		{
			spatial_move();
			return;
		}

		InSpace = true;
		spatial_register();
	}

	void Unbind()
	{
		renderable.visual = nullptr;

		if (!InSpace)
		{
			return;
		}

		InSpace = false;
		spatial_unregister();
	}

	void renderable_Render() override
	{
		if (renderable.visual == nullptr)
		{
			return;
		}

		::Render->set_Transform(&renderable.xform);
		::Render->add_Visual(renderable.visual);
		renderable.visual->getVisData().hom_frame = Device.dwFrame;

		if (ItemVisual != nullptr)
		{
			::Render->set_Transform(&ItemTransform);
			::Render->add_Visual(ItemVisual);
			ItemVisual->getVisData().hom_frame = Device.dwFrame;
		}
	}

	bool renderable_ShadowGenerate() override
	{
		return false;
	}

	bool register_schedule() const override
	{
		return false;
	}

private:
	bool InSpace = false;
};

struct SCinemaActor
{
	xr_string Name;
	shared_str Visual;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix Orientation;
	Fvector Scale = Fvector().set(1.0f, 1.0f, 1.0f);
	Fbox LocalBox;
	IRenderVisual* Model = nullptr;
	IKinematics* Skeleton = nullptr;
	IKinematicsAnimated* Animated = nullptr;
	CCinemaActorObject Object;
	xr_vector<xr_unique_ptr<SBindBoneParam>> BindPoseBones;
	bool BindPose = false;
	xr_vector<SAnimationSource> Sources;
	xr_vector<shared_str> AppliedExtraPaths;
	xr_vector<SPlaylistEntry> Playlist;
	xr_vector<STrimRange> Trims;
	bool PingPong = false;
	float PlayDirection = 1.0f;
	s32 PlaylistIndex = -1;
	s32 PlaylistSelected = -1;
	bool Loop = false;
	float Speed = 1.0f;
	bool Paused = false;
	MotionID Motion;
	shared_str MotionName;
	shared_str MotionSource;
	bool MotionFx = false;
	float FxDeadline = 0.0f;
	shared_str ItemSection;
	IRenderVisual* ItemModel = nullptr;
	bool ItemWeaponStyle = false;
	s32 ItemHandDependence = 2;
	u16 ItemBoneR = u16(-1);
	u16 ItemBoneL = u16(-1);
	u16 ItemBoneR2 = u16(-1);
	u16 ItemBoneAttach = u16(-1);
	shared_str ItemBoneName;
	Fvector ItemPosition = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector ItemRotation = Fvector().set(0.0f, 0.0f, 0.0f);

	SCinemaActor()
	{
		Orientation.identity();
		LocalBox.invalidate();
	}
};

struct SEditorTheme
{
	const char* Id;
	const char* Name;

	ImVec4 WindowBg;
	ImVec4 ChildBg;
	ImVec4 PopupBg;
	ImVec4 Text;
	ImVec4 TextDisabled;
	ImVec4 FrameBg;
	ImVec4 FrameBgHovered;
	ImVec4 FrameBgActive;
	ImVec4 Button;
	ImVec4 ButtonHovered;
	ImVec4 ButtonActive;
	ImVec4 Header;
	ImVec4 HeaderHovered;
	ImVec4 HeaderActive;
	ImVec4 Tab;
	ImVec4 TabHovered;
	ImVec4 TabSelected;
	ImVec4 TitleBg;
	ImVec4 MenuBarBg;
	ImVec4 Separator;
	ImVec4 Border;
	ImVec4 CheckMark;
	ImVec4 SliderGrab;
	ImVec4 SliderGrabActive;
	ImVec4 ScrollbarBg;
	ImVec4 ScrollbarGrab;

	ImU32 TimelineBackground;
	ImU32 TimelineElapsed;
	ImU32 TimelineMark;
	ImU32 TimelineTickMinor;
	ImU32 TimelineTickMajor;
	ImU32 TimelineText;
	ImU32 TimelinePlayhead;
	ImU32 TimelineBorder;

	u32 AxisX;
	u32 AxisY;
	u32 AxisZ;
	u32 AxisHighlighted;
	u32 AxisPivot;
};

struct SThemeColorLink
{
	ImGuiCol Color;
	ImVec4 SEditorTheme::* Value;
};

static const SThemeColorLink kCinemaThemeColors[] =
{
	{ ImGuiCol_WindowBg, &SEditorTheme::WindowBg },
	{ ImGuiCol_ChildBg, &SEditorTheme::ChildBg },
	{ ImGuiCol_PopupBg, &SEditorTheme::PopupBg },
	{ ImGuiCol_Text, &SEditorTheme::Text },
	{ ImGuiCol_TextDisabled, &SEditorTheme::TextDisabled },
	{ ImGuiCol_FrameBg, &SEditorTheme::FrameBg },
	{ ImGuiCol_FrameBgHovered, &SEditorTheme::FrameBgHovered },
	{ ImGuiCol_FrameBgActive, &SEditorTheme::FrameBgActive },
	{ ImGuiCol_Button, &SEditorTheme::Button },
	{ ImGuiCol_ButtonHovered, &SEditorTheme::ButtonHovered },
	{ ImGuiCol_ButtonActive, &SEditorTheme::ButtonActive },
	{ ImGuiCol_Header, &SEditorTheme::Header },
	{ ImGuiCol_HeaderHovered, &SEditorTheme::HeaderHovered },
	{ ImGuiCol_HeaderActive, &SEditorTheme::HeaderActive },
	{ ImGuiCol_Tab, &SEditorTheme::Tab },
	{ ImGuiCol_TabHovered, &SEditorTheme::TabHovered },
	{ ImGuiCol_TabSelected, &SEditorTheme::TabSelected },
	{ ImGuiCol_TitleBg, &SEditorTheme::TitleBg },
	{ ImGuiCol_MenuBarBg, &SEditorTheme::MenuBarBg },
	{ ImGuiCol_Separator, &SEditorTheme::Separator },
	{ ImGuiCol_Border, &SEditorTheme::Border },
	{ ImGuiCol_CheckMark, &SEditorTheme::CheckMark },
	{ ImGuiCol_SliderGrab, &SEditorTheme::SliderGrab },
	{ ImGuiCol_SliderGrabActive, &SEditorTheme::SliderGrabActive },
	{ ImGuiCol_ScrollbarBg, &SEditorTheme::ScrollbarBg },
	{ ImGuiCol_ScrollbarGrab, &SEditorTheme::ScrollbarGrab },
};

static const SEditorTheme kCinemaThemes[] =
{
	{
		.Id = "default",
		.Name = "Default",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, kGeneralAlphaLevelForImGuiWindows),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.06f, 0.06f, 0.07f, 0.94f),
		.Text = ImVec4(0.90f, 0.90f, 0.92f, 1.00f),
		.TextDisabled = ImVec4(0.55f, 0.55f, 0.58f, 1.00f),
		.FrameBg = ImVec4(0.16f, 0.16f, 0.18f, 1.00f),
		.FrameBgHovered = ImVec4(0.22f, 0.22f, 0.25f, 1.00f),
		.FrameBgActive = ImVec4(0.28f, 0.28f, 0.32f, 1.00f),
		.Button = ImVec4(0.18f, 0.18f, 0.21f, 1.00f),
		.ButtonHovered = ImVec4(0.25f, 0.25f, 0.29f, 1.00f),
		.ButtonActive = ImVec4(0.32f, 0.32f, 0.37f, 1.00f),
		.Header = ImVec4(0.25f, 0.35f, 0.45f, 1.00f),
		.HeaderHovered = ImVec4(0.30f, 0.42f, 0.54f, 1.00f),
		.HeaderActive = ImVec4(0.36f, 0.50f, 0.64f, 1.00f),
		.Tab = ImVec4(0.12f, 0.12f, 0.14f, 1.00f),
		.TabHovered = ImVec4(0.20f, 0.28f, 0.34f, 1.00f),
		.TabSelected = ImVec4(0.22f, 0.32f, 0.42f, 1.00f),
		.TitleBg = ImVec4(0.05f, 0.05f, 0.06f, 1.00f),
		.MenuBarBg = ImVec4(0.07f, 0.07f, 0.08f, 1.00f),
		.Separator = ImVec4(0.35f, 0.35f, 0.38f, 1.00f),
		.Border = ImVec4(0.45f, 0.45f, 0.50f, 1.00f),
		.CheckMark = ImVec4(0.60f, 0.80f, 0.95f, 1.00f),
		.SliderGrab = ImVec4(0.35f, 0.55f, 0.70f, 1.00f),
		.SliderGrabActive = ImVec4(0.50f, 0.70f, 0.90f, 1.00f),
		.ScrollbarBg = ImVec4(0.08f, 0.08f, 0.09f, 1.00f),
		.ScrollbarGrab = ImVec4(0.28f, 0.28f, 0.32f, 1.00f),
		.TimelineBackground = IM_COL32(22, 22, 24, 255),
		.TimelineElapsed = IM_COL32(52, 78, 52, 140),
		.TimelineMark = IM_COL32(78, 121, 176, 110),
		.TimelineTickMinor = IM_COL32(58, 58, 62, 255),
		.TimelineTickMajor = IM_COL32(105, 105, 112, 255),
		.TimelineText = IM_COL32(180, 180, 186, 255),
		.TimelinePlayhead = IM_COL32(222, 170, 64, 255),
		.TimelineBorder = IM_COL32(80, 80, 86, 255),
		.AxisX = color_rgba(220, 70, 70, 255),
		.AxisY = color_rgba(80, 210, 90, 255),
		.AxisZ = color_rgba(70, 130, 235, 255),
		.AxisHighlighted = color_rgba(255, 220, 70, 255),
		.AxisPivot = color_rgba(240, 240, 240, 220),
	},
	{
		.Id = "oled_black",
		.Name = "OLED Black",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, 0.94f),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.03f, 0.03f, 0.04f, 0.98f),
		.Text = ImVec4(0.94f, 0.95f, 0.96f, 1.00f),
		.TextDisabled = ImVec4(0.60f, 0.62f, 0.66f, 1.00f),
		.FrameBg = ImVec4(0.10f, 0.10f, 0.12f, 1.00f),
		.FrameBgHovered = ImVec4(0.16f, 0.17f, 0.20f, 1.00f),
		.FrameBgActive = ImVec4(0.23f, 0.24f, 0.28f, 1.00f),
		.Button = ImVec4(0.12f, 0.13f, 0.16f, 1.00f),
		.ButtonHovered = ImVec4(0.19f, 0.21f, 0.25f, 1.00f),
		.ButtonActive = ImVec4(0.27f, 0.30f, 0.35f, 1.00f),
		.Header = ImVec4(0.10f, 0.27f, 0.35f, 1.00f),
		.HeaderHovered = ImVec4(0.13f, 0.36f, 0.47f, 1.00f),
		.HeaderActive = ImVec4(0.17f, 0.46f, 0.60f, 1.00f),
		.Tab = ImVec4(0.06f, 0.06f, 0.08f, 1.00f),
		.TabHovered = ImVec4(0.14f, 0.21f, 0.26f, 1.00f),
		.TabSelected = ImVec4(0.10f, 0.27f, 0.35f, 1.00f),
		.TitleBg = ImVec4(0.02f, 0.02f, 0.03f, 1.00f),
		.MenuBarBg = ImVec4(0.03f, 0.03f, 0.04f, 1.00f),
		.Separator = ImVec4(0.24f, 0.26f, 0.30f, 1.00f),
		.Border = ImVec4(0.32f, 0.35f, 0.40f, 1.00f),
		.CheckMark = ImVec4(0.35f, 0.90f, 1.00f, 1.00f),
		.SliderGrab = ImVec4(0.30f, 0.72f, 0.88f, 1.00f),
		.SliderGrabActive = ImVec4(0.45f, 0.90f, 1.00f, 1.00f),
		.ScrollbarBg = ImVec4(0.02f, 0.02f, 0.03f, 1.00f),
		.ScrollbarGrab = ImVec4(0.22f, 0.25f, 0.30f, 1.00f),
		.TimelineBackground = IM_COL32(4, 4, 6, 255),
		.TimelineElapsed = IM_COL32(26, 72, 92, 190),
		.TimelineMark = IM_COL32(40, 116, 150, 120),
		.TimelineTickMinor = IM_COL32(40, 42, 48, 255),
		.TimelineTickMajor = IM_COL32(112, 122, 134, 255),
		.TimelineText = IM_COL32(205, 214, 224, 255),
		.TimelinePlayhead = IM_COL32(255, 190, 70, 255),
		.TimelineBorder = IM_COL32(58, 64, 72, 255),
		.AxisX = color_rgba(255, 120, 60, 255),
		.AxisY = color_rgba(130, 235, 130, 255),
		.AxisZ = color_rgba(90, 170, 255, 255),
		.AxisHighlighted = color_rgba(255, 240, 150, 255),
		.AxisPivot = color_rgba(240, 240, 240, 220),
	},
	{
		.Id = "oled_dim",
		.Name = "OLED Dim",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, 0.97f),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.02f, 0.02f, 0.03f, 0.98f),
		.Text = ImVec4(0.74f, 0.75f, 0.78f, 1.00f),
		.TextDisabled = ImVec4(0.44f, 0.45f, 0.48f, 1.00f),
		.FrameBg = ImVec4(0.07f, 0.07f, 0.08f, 1.00f),
		.FrameBgHovered = ImVec4(0.11f, 0.12f, 0.14f, 1.00f),
		.FrameBgActive = ImVec4(0.16f, 0.17f, 0.20f, 1.00f),
		.Button = ImVec4(0.09f, 0.09f, 0.11f, 1.00f),
		.ButtonHovered = ImVec4(0.14f, 0.15f, 0.18f, 1.00f),
		.ButtonActive = ImVec4(0.20f, 0.22f, 0.26f, 1.00f),
		.Header = ImVec4(0.08f, 0.14f, 0.20f, 1.00f),
		.HeaderHovered = ImVec4(0.11f, 0.20f, 0.28f, 1.00f),
		.HeaderActive = ImVec4(0.15f, 0.27f, 0.38f, 1.00f),
		.Tab = ImVec4(0.05f, 0.05f, 0.06f, 1.00f),
		.TabHovered = ImVec4(0.11f, 0.16f, 0.20f, 1.00f),
		.TabSelected = ImVec4(0.09f, 0.17f, 0.24f, 1.00f),
		.TitleBg = ImVec4(0.01f, 0.01f, 0.02f, 1.00f),
		.MenuBarBg = ImVec4(0.02f, 0.02f, 0.03f, 1.00f),
		.Separator = ImVec4(0.17f, 0.18f, 0.21f, 1.00f),
		.Border = ImVec4(0.24f, 0.26f, 0.30f, 1.00f),
		.CheckMark = ImVec4(0.55f, 0.72f, 0.88f, 1.00f),
		.SliderGrab = ImVec4(0.45f, 0.60f, 0.75f, 1.00f),
		.SliderGrabActive = ImVec4(0.62f, 0.78f, 0.95f, 1.00f),
		.ScrollbarBg = ImVec4(0.02f, 0.02f, 0.02f, 1.00f),
		.ScrollbarGrab = ImVec4(0.16f, 0.17f, 0.20f, 1.00f),
		.TimelineBackground = IM_COL32(2, 2, 3, 255),
		.TimelineElapsed = IM_COL32(20, 44, 58, 190),
		.TimelineMark = IM_COL32(28, 74, 96, 110),
		.TimelineTickMinor = IM_COL32(32, 34, 38, 255),
		.TimelineTickMajor = IM_COL32(84, 92, 102, 255),
		.TimelineText = IM_COL32(168, 174, 184, 255),
		.TimelinePlayhead = IM_COL32(210, 158, 66, 255),
		.TimelineBorder = IM_COL32(44, 48, 54, 255),
		.AxisX = color_rgba(230, 110, 60, 255),
		.AxisY = color_rgba(110, 200, 120, 255),
		.AxisZ = color_rgba(80, 150, 230, 255),
		.AxisHighlighted = color_rgba(240, 220, 120, 255),
		.AxisPivot = color_rgba(220, 220, 220, 220),
	},
	{
		.Id = "oled_amber",
		.Name = "OLED Amber",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, 0.95f),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.04f, 0.03f, 0.01f, 0.98f),
		.Text = ImVec4(0.98f, 0.80f, 0.38f, 1.00f),
		.TextDisabled = ImVec4(0.62f, 0.48f, 0.20f, 1.00f),
		.FrameBg = ImVec4(0.12f, 0.09f, 0.03f, 1.00f),
		.FrameBgHovered = ImVec4(0.19f, 0.14f, 0.05f, 1.00f),
		.FrameBgActive = ImVec4(0.27f, 0.20f, 0.07f, 1.00f),
		.Button = ImVec4(0.14f, 0.11f, 0.04f, 1.00f),
		.ButtonHovered = ImVec4(0.22f, 0.16f, 0.06f, 1.00f),
		.ButtonActive = ImVec4(0.31f, 0.23f, 0.08f, 1.00f),
		.Header = ImVec4(0.27f, 0.20f, 0.06f, 1.00f),
		.HeaderHovered = ImVec4(0.37f, 0.27f, 0.09f, 1.00f),
		.HeaderActive = ImVec4(0.47f, 0.35f, 0.12f, 1.00f),
		.Tab = ImVec4(0.09f, 0.07f, 0.02f, 1.00f),
		.TabHovered = ImVec4(0.19f, 0.14f, 0.05f, 1.00f),
		.TabSelected = ImVec4(0.27f, 0.20f, 0.06f, 1.00f),
		.TitleBg = ImVec4(0.03f, 0.02f, 0.01f, 1.00f),
		.MenuBarBg = ImVec4(0.04f, 0.03f, 0.01f, 1.00f),
		.Separator = ImVec4(0.38f, 0.29f, 0.11f, 1.00f),
		.Border = ImVec4(0.50f, 0.38f, 0.14f, 1.00f),
		.CheckMark = ImVec4(1.00f, 0.86f, 0.40f, 1.00f),
		.SliderGrab = ImVec4(0.78f, 0.60f, 0.22f, 1.00f),
		.SliderGrabActive = ImVec4(0.98f, 0.78f, 0.34f, 1.00f),
		.ScrollbarBg = ImVec4(0.02f, 0.02f, 0.01f, 1.00f),
		.ScrollbarGrab = ImVec4(0.24f, 0.18f, 0.06f, 1.00f),
		.TimelineBackground = IM_COL32(6, 4, 1, 255),
		.TimelineElapsed = IM_COL32(96, 66, 12, 190),
		.TimelineMark = IM_COL32(128, 88, 20, 120),
		.TimelineTickMinor = IM_COL32(46, 35, 10, 255),
		.TimelineTickMajor = IM_COL32(132, 100, 32, 255),
		.TimelineText = IM_COL32(246, 206, 116, 255),
		.TimelinePlayhead = IM_COL32(255, 236, 150, 255),
		.TimelineBorder = IM_COL32(96, 72, 22, 255),
		.AxisX = color_rgba(255, 140, 50, 255),
		.AxisY = color_rgba(200, 220, 90, 255),
		.AxisZ = color_rgba(90, 180, 240, 255),
		.AxisHighlighted = color_rgba(255, 240, 170, 255),
		.AxisPivot = color_rgba(250, 220, 150, 220),
	},
	{
		.Id = "oled_mint",
		.Name = "OLED Mint",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, 0.95f),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.01f, 0.03f, 0.02f, 0.98f),
		.Text = ImVec4(0.82f, 0.98f, 0.90f, 1.00f),
		.TextDisabled = ImVec4(0.48f, 0.62f, 0.54f, 1.00f),
		.FrameBg = ImVec4(0.05f, 0.09f, 0.08f, 1.00f),
		.FrameBgHovered = ImVec4(0.08f, 0.15f, 0.12f, 1.00f),
		.FrameBgActive = ImVec4(0.12f, 0.22f, 0.18f, 1.00f),
		.Button = ImVec4(0.06f, 0.11f, 0.09f, 1.00f),
		.ButtonHovered = ImVec4(0.10f, 0.18f, 0.15f, 1.00f),
		.ButtonActive = ImVec4(0.14f, 0.26f, 0.21f, 1.00f),
		.Header = ImVec4(0.06f, 0.24f, 0.18f, 1.00f),
		.HeaderHovered = ImVec4(0.09f, 0.33f, 0.25f, 1.00f),
		.HeaderActive = ImVec4(0.13f, 0.42f, 0.32f, 1.00f),
		.Tab = ImVec4(0.03f, 0.06f, 0.05f, 1.00f),
		.TabHovered = ImVec4(0.07f, 0.16f, 0.13f, 1.00f),
		.TabSelected = ImVec4(0.06f, 0.24f, 0.18f, 1.00f),
		.TitleBg = ImVec4(0.01f, 0.02f, 0.02f, 1.00f),
		.MenuBarBg = ImVec4(0.02f, 0.03f, 0.03f, 1.00f),
		.Separator = ImVec4(0.16f, 0.30f, 0.26f, 1.00f),
		.Border = ImVec4(0.22f, 0.40f, 0.34f, 1.00f),
		.CheckMark = ImVec4(0.40f, 1.00f, 0.78f, 1.00f),
		.SliderGrab = ImVec4(0.30f, 0.80f, 0.62f, 1.00f),
		.SliderGrabActive = ImVec4(0.45f, 0.98f, 0.78f, 1.00f),
		.ScrollbarBg = ImVec4(0.01f, 0.02f, 0.02f, 1.00f),
		.ScrollbarGrab = ImVec4(0.12f, 0.22f, 0.19f, 1.00f),
		.TimelineBackground = IM_COL32(2, 5, 4, 255),
		.TimelineElapsed = IM_COL32(18, 70, 56, 190),
		.TimelineMark = IM_COL32(28, 110, 88, 120),
		.TimelineTickMinor = IM_COL32(30, 40, 36, 255),
		.TimelineTickMajor = IM_COL32(90, 112, 104, 255),
		.TimelineText = IM_COL32(190, 230, 215, 255),
		.TimelinePlayhead = IM_COL32(190, 255, 225, 255),
		.TimelineBorder = IM_COL32(44, 58, 54, 255),
		.AxisX = color_rgba(255, 130, 70, 255),
		.AxisY = color_rgba(120, 240, 140, 255),
		.AxisZ = color_rgba(90, 175, 255, 255),
		.AxisHighlighted = color_rgba(255, 245, 160, 255),
		.AxisPivot = color_rgba(235, 245, 240, 220),
	},
	{
		.Id = "oled_magenta",
		.Name = "OLED Magenta",
		.WindowBg = ImVec4(0.00f, 0.00f, 0.00f, 0.95f),
		.ChildBg = ImVec4(0.00f, 0.00f, 0.00f, 0.00f),
		.PopupBg = ImVec4(0.03f, 0.01f, 0.03f, 0.98f),
		.Text = ImVec4(0.97f, 0.88f, 0.95f, 1.00f),
		.TextDisabled = ImVec4(0.62f, 0.50f, 0.58f, 1.00f),
		.FrameBg = ImVec4(0.10f, 0.05f, 0.09f, 1.00f),
		.FrameBgHovered = ImVec4(0.17f, 0.08f, 0.15f, 1.00f),
		.FrameBgActive = ImVec4(0.24f, 0.12f, 0.21f, 1.00f),
		.Button = ImVec4(0.12f, 0.06f, 0.11f, 1.00f),
		.ButtonHovered = ImVec4(0.20f, 0.10f, 0.18f, 1.00f),
		.ButtonActive = ImVec4(0.29f, 0.14f, 0.26f, 1.00f),
		.Header = ImVec4(0.30f, 0.08f, 0.24f, 1.00f),
		.HeaderHovered = ImVec4(0.42f, 0.11f, 0.34f, 1.00f),
		.HeaderActive = ImVec4(0.55f, 0.15f, 0.44f, 1.00f),
		.Tab = ImVec4(0.08f, 0.04f, 0.07f, 1.00f),
		.TabHovered = ImVec4(0.18f, 0.09f, 0.16f, 1.00f),
		.TabSelected = ImVec4(0.28f, 0.10f, 0.23f, 1.00f),
		.TitleBg = ImVec4(0.02f, 0.01f, 0.02f, 1.00f),
		.MenuBarBg = ImVec4(0.03f, 0.01f, 0.03f, 1.00f),
		.Separator = ImVec4(0.38f, 0.18f, 0.32f, 1.00f),
		.Border = ImVec4(0.50f, 0.24f, 0.42f, 1.00f),
		.CheckMark = ImVec4(1.00f, 0.55f, 0.92f, 1.00f),
		.SliderGrab = ImVec4(0.82f, 0.35f, 0.72f, 1.00f),
		.SliderGrabActive = ImVec4(1.00f, 0.55f, 0.92f, 1.00f),
		.ScrollbarBg = ImVec4(0.02f, 0.01f, 0.02f, 1.00f),
		.ScrollbarGrab = ImVec4(0.24f, 0.12f, 0.20f, 1.00f),
		.TimelineBackground = IM_COL32(5, 2, 5, 255),
		.TimelineElapsed = IM_COL32(80, 20, 66, 190),
		.TimelineMark = IM_COL32(110, 30, 90, 120),
		.TimelineTickMinor = IM_COL32(42, 30, 40, 255),
		.TimelineTickMajor = IM_COL32(120, 90, 110, 255),
		.TimelineText = IM_COL32(230, 205, 225, 255),
		.TimelinePlayhead = IM_COL32(255, 180, 120, 255),
		.TimelineBorder = IM_COL32(80, 50, 74, 255),
		.AxisX = color_rgba(255, 120, 90, 255),
		.AxisY = color_rgba(150, 230, 180, 255),
		.AxisZ = color_rgba(130, 150, 255, 255),
		.AxisHighlighted = color_rgba(255, 235, 180, 255),
		.AxisPivot = color_rgba(240, 235, 241, 220),
	},
};

constexpr s32 kCinemaThemeCount = static_cast<s32>(std::size(kCinemaThemes));

static bool CinemaContainsFilter(const char* text, const char* filter)
{
	if (filter[0] == '\0')
	{
		return true;
	}

	const size_t length = xr_strlen(filter);

	for (const char* position = text; *position; ++position)
	{
		if (_strnicmp(position, filter, length) == 0)
		{
			return true;
		}
	}

	return false;
}

static bool CinemaIsOmfPath(const shared_str& source)
{
	const u32 length = source.size();

	return length > 4 && !_stricmp(source.c_str() + length - 4, ".omf");
}

static void CinemaBindPoseCallback(CBoneInstance* instance)
{
	SBindBoneParam* parameter = static_cast<SBindBoneParam*>(instance->callback_param());

	if (parameter != nullptr)
	{
		instance->mTransform = parameter->Matrix;
	}
}

static void CinemaClearBindPose(SCinemaActor& actor)
{
	if (!actor.BindPose)
	{
		return;
	}

	if (actor.Skeleton != nullptr)
	{
		for (u16 bone = 0, count = actor.Skeleton->LL_BoneCount(); bone < count; ++bone)
		{
			CBoneInstance& instance = actor.Skeleton->LL_GetBoneInstance(bone);

			if (instance.callback() == CinemaBindPoseCallback)
			{
				instance.reset_callback();
			}
		}
	}

	actor.BindPoseBones.clear();
	actor.BindPose = false;
}

static bool CinemaSetBindPose(SCinemaActor& actor)
{
	if (actor.Skeleton == nullptr)
	{
		return false;
	}

	CinemaClearBindPose(actor);

	xr_vector<Fmatrix> bindTransforms;
	actor.Skeleton->LL_GetBindTransform(bindTransforms);

	const u16 boneCount = actor.Skeleton->LL_BoneCount();

	if (bindTransforms.size() < boneCount)
	{
		return false;
	}

	actor.BindPoseBones.reserve(boneCount);

	for (u16 bone = 0; bone < boneCount; ++bone)
	{
		auto parameter = xr_make_unique<SBindBoneParam>();
		parameter->Matrix = bindTransforms[bone];

		SBindBoneParam* parameterPtr = parameter.get();
		actor.BindPoseBones.push_back(std::move(parameter));
		actor.Skeleton->LL_GetBoneInstance(bone).set_callback(bctCustom, CinemaBindPoseCallback, parameterPtr, true);
	}

	actor.BindPose = true;
	actor.Skeleton->CalculateBones_Invalidate();
	actor.Skeleton->CalculateBones(true);
	return true;
}

static const SAnimationClip* CinemaFindEditorClip(const SCinemaActor& actor, const SAnimationSource** source)
{
	for (const SAnimationSource& animationSource : actor.Sources)
	{
		for (const SAnimationClip& clip : animationSource.Clips)
		{
			if (!clip.Fx && _stricmp(clip.Name.c_str(), "$editor") == 0)
			{
				if (source != nullptr)
				{
					*source = &animationSource;
				}

				return &clip;
			}
		}
	}

	return nullptr;
}

static const SAnimationClip* CinemaFindDefaultIdle(const SCinemaActor& actor, const SAnimationSource** source)
{
	static const char* idleNames[] =
	{
		"idle",
		"stand_idle_0",
		"stand_idle_1",
		"stand_idle_2",
		"norm_idle_0",
		"norm_idle_1",
		"idle_0",
		"idle_1"
	};

	for (const char* idleName : idleNames)
	{
		for (const SAnimationSource& animationSource : actor.Sources)
		{
			for (const SAnimationClip& clip : animationSource.Clips)
			{
				if (!clip.Fx && xr_strcmp(clip.Name.c_str(), idleName) == 0)
				{
					if (source != nullptr)
					{
						*source = &animationSource;
					}

					return &clip;
				}
			}
		}
	}

	for (const SAnimationSource& animationSource : actor.Sources)
	{
		for (const SAnimationClip& clip : animationSource.Clips)
		{
			if (!clip.Fx && CinemaContainsFilter(clip.Name.c_str(), "idle"))
			{
				if (source != nullptr)
				{
					*source = &animationSource;
				}

				return &clip;
			}
		}
	}

	for (const SAnimationSource& animationSource : actor.Sources)
	{
		for (const SAnimationClip& clip : animationSource.Clips)
		{
			if (!clip.Fx)
			{
				if (source != nullptr)
				{
					*source = &animationSource;
				}

				return &clip;
			}
		}
	}

	return nullptr;
}

static bool CinemaIsCompatible(const SAnimationSource& source, IKinematics* target)
{
	if (target == nullptr)
	{
		return false;
	}

	if (source.BoneCount != target->LL_BoneCount())
	{
		return false;
	}

	for (const shared_str& bone : source.BoneNames)
	{
		if (target->LL_BoneID(bone) == BI_NONE)
		{
			return false;
		}
	}

	return true;
}

static void FillSourceBones(SAnimationSource& source, IKinematics* skeleton)
{
	source.BoneCount = skeleton->LL_BoneCount();
	source.BoneNames.reserve(source.BoneCount);

	if (IKinematics::accel* bones = skeleton->LL_Bones())
	{
		for (const auto& bone : *bones)
		{
			source.BoneNames.push_back(bone.first);
		}
	}
}

static MotionID CinemaFindMotion(IKinematicsAnimated* kinematics, const SAnimationSource& source, const SAnimationClip& clip)
{
	if (kinematics == nullptr)
	{
		return MotionID();
	}

	for (u16 slot = 0, count = kinematics->LL_MotionsSlotCount(); slot < count; ++slot)
	{
		shared_motions motions = kinematics->LL_MotionsSlot(slot);

		if (motions.motion_map() == nullptr || xr_strcmp(motions.id().c_str(), source.Id.c_str()) != 0)
		{
			continue;
		}

		accel_map* map = clip.Fx ? motions.fx() : motions.cycle();

		if (map == nullptr)
		{
			continue;
		}

		const auto iterator = map->find(clip.Name);

		if (iterator != map->end())
		{
			return MotionID(slot, iterator->second);
		}
	}

	return MotionID();
}

static CBlend* CinemaFindBlend(IKinematicsAnimated* kinematics, MotionID motion)
{
	if (kinematics == nullptr || !motion.valid())
	{
		return nullptr;
	}

	for (u16 part = 0; part < MAX_PARTS; ++part)
	{
		for (u32 i = 0, count = kinematics->LL_PartBlendsCount(part); i < count; ++i)
		{
			CBlend* blend = kinematics->LL_PartBlend(part, i);

			if (blend && blend->motionID == motion && blend->blend_state() == CBlend::eAccrue)
			{
				return blend;
			}
		}
	}

	return nullptr;
}

template <typename Func>
static void CinemaForEachBlend(IKinematicsAnimated* kinematics, MotionID motion, const Func& func)
{
	if (kinematics == nullptr || !motion.valid())
	{
		return;
	}

	for (u16 part = 0; part < MAX_PARTS; ++part)
	{
		for (u32 i = 0, count = kinematics->LL_PartBlendsCount(part); i < count; ++i)
		{
			CBlend* blend = kinematics->LL_PartBlend(part, i);

			if (blend && blend->motionID == motion && blend->blend_state() == CBlend::eAccrue)
			{
				func(*blend);
			}
		}
	}
}

static void CinemaSetBlendTime(IKinematicsAnimated* kinematics, MotionID motion, float time)
{
	CinemaForEachBlend(kinematics, motion, [time](CBlend& blend)
	{
		blend.timeCurrent = time;
	});
}

static void CinemaSetBlendPlaying(IKinematicsAnimated* kinematics, MotionID motion, bool playing)
{
	CinemaForEachBlend(kinematics, motion, [playing](CBlend& blend)
	{
		blend.playing = playing;
	});
}

static void CinemaSetBlendLoop(IKinematicsAnimated* kinematics, MotionID motion, bool loop)
{
	CinemaForEachBlend(kinematics, motion, [loop](CBlend& blend)
	{
		blend.stop_at_end = !loop;
	});
}

static void CinemaSetBlendSpeed(IKinematicsAnimated* kinematics, MotionID motion, float speed)
{
	CinemaForEachBlend(kinematics, motion, [speed](CBlend& blend)
	{
		blend.speed = speed;
	});
}

static void AppendClips(shared_motions motions, xr_vector<SAnimationClip>& clips)
{
	if (accel_map* cycles = motions.cycle())
	{
		for (const auto& pair : *cycles)
		{
			clips.push_back({ .Name = pair.first, .Fx = false });
		}
	}

	if (accel_map* effects = motions.fx())
	{
		for (const auto& pair : *effects)
		{
			clips.push_back({ .Name = pair.first, .Fx = true });
		}
	}
}

static bool VisualPathExists(const char* visual)
{
	if (visual == nullptr || visual[0] == '\0')
	{
		return false;
	}

	string_path fileName = {};
	xr_strcpy(fileName, visual);

	if (strext(fileName) == nullptr)
	{
		xr_strcat(fileName, ".ogf");
	}

	string_path resolved = {};

	if (FS.exist(resolved, _game_meshes_, fileName))
	{
		return true;
	}

	if (FS.exist(resolved, "$level$", fileName))
	{
		return true;
	}

	return false;
}

static void ReleaseActorModel(SCinemaActor& actor)
{
	CinemaClearBindPose(actor);
	actor.Object.Unbind();

	if (actor.Model && ::Render)
	{
		::Render->model_Delete(actor.Model);
	}

	actor.Model = nullptr;
	actor.Skeleton = nullptr;
	actor.Animated = nullptr;
	actor.LocalBox.invalidate();
	actor.Sources.clear();
	actor.AppliedExtraPaths.clear();
}

static void ReleaseActorItem(SCinemaActor& actor)
{
	actor.Object.ItemVisual = nullptr;
	actor.ItemSection = "";
	actor.ItemBoneName = "";
	actor.ItemWeaponStyle = false;
	actor.ItemHandDependence = 2;
	actor.ItemBoneR = u16(-1);
	actor.ItemBoneL = u16(-1);
	actor.ItemBoneR2 = u16(-1);
	actor.ItemBoneAttach = u16(-1);
	actor.ItemPosition.set(0.0f, 0.0f, 0.0f);
	actor.ItemRotation.set(0.0f, 0.0f, 0.0f);

	if (actor.ItemModel != nullptr && ::Render != nullptr)
	{
		::Render->model_Delete(actor.ItemModel);
	}

	actor.ItemModel = nullptr;
}

static void UpdateActorItemPlacement(SCinemaActor& actor)
{
	actor.Object.ItemVisual = nullptr;

	if (actor.ItemModel == nullptr || actor.Skeleton == nullptr)
	{
		return;
	}

	const Fmatrix& placement = actor.Object.renderable.xform;
	Fmatrix transform;

	if (actor.ItemWeaponStyle)
	{
		const u16 boneR = actor.ItemBoneR;
		const u16 boneL = actor.ItemHandDependence == 1 ? actor.ItemBoneR2 : actor.ItemBoneL;

		if (boneR == u16(-1) || boneL == u16(-1))
		{
			return;
		}

		const Fmatrix& mL = actor.Skeleton->LL_GetTransform(boneL);
		const Fmatrix& mR = actor.Skeleton->LL_GetTransform(boneR);

		Fvector direction = {};
		direction.sub(mL.c, mR.c);

		if (fis_zero(direction.magnitude()))
		{
			transform = placement;
			placement.transform_tiny(transform.c, mR.c);
		}
		else
		{
			direction.normalize();

			Fvector right = {};
			right.crossproduct(mR.j, direction);
			right.normalize();

			Fvector normal = {};
			normal.crossproduct(direction, right);
			normal.normalize();

			transform.set(right, normal, direction, mR.c);
			transform.mulA_43(placement);
		}

		Fmatrix offset;
		offset.setHPB(
			actor.ItemRotation.x * PI / 180.0f,
			actor.ItemRotation.y * PI / 180.0f,
			actor.ItemRotation.z * PI / 180.0f);
		offset.translate_over(actor.ItemPosition);

		actor.Object.ItemTransform.mul(transform, offset);
	}
	else
	{
		if (actor.ItemBoneAttach == u16(-1))
		{
			return;
		}

		Fmatrix boneMatrix;
		actor.Skeleton->Bone_GetAnimPos(boneMatrix, actor.ItemBoneAttach, u8(-1), false);

		Fmatrix offset;
		offset.setHPB(actor.ItemRotation.x, actor.ItemRotation.y, actor.ItemRotation.z);
		offset.translate_over(actor.ItemPosition);

		transform.mul_43(boneMatrix, offset);
		transform.mulA_43(placement);
		actor.Object.ItemTransform = transform;
	}

	actor.Object.ItemVisual = actor.ItemModel;
}

static void UpdateActorPlacement(SCinemaActor& actor)
{
	if (actor.Skeleton != nullptr)
	{
		actor.Skeleton->CalculateBones(true);
	}

	Fmatrix transform = actor.Orientation;
	transform.c = actor.Position;
	transform.i.mul(actor.Scale.x);
	transform.j.mul(actor.Scale.y);
	transform.k.mul(actor.Scale.z);
	actor.Object.Bind(actor.Model, transform);

	UpdateActorItemPlacement(actor);
}

static Fvector AxisVector(EGizmoAxis axis)
{
	switch (axis)
	{
	case EGizmoAxis::X: return Fvector().set(1.0f, 0.0f, 0.0f);
	case EGizmoAxis::Y: return Fvector().set(0.0f, 1.0f, 0.0f);
	case EGizmoAxis::Z: return Fvector().set(0.0f, 0.0f, 1.0f);
	default: return Fvector().set(0.0f, 0.0f, 0.0f);
	}
}

static void RotationBasis(EGizmoAxis axis, Fvector& u, Fvector& v)
{
	switch (axis)
	{
	case EGizmoAxis::X:
	{
		u.set(0.0f, 1.0f, 0.0f);
		v.set(0.0f, 0.0f, 1.0f);
		break;
	}
	case EGizmoAxis::Y:
	{
		u.set(1.0f, 0.0f, 0.0f);
		v.set(0.0f, 0.0f, -1.0f);
		break;
	}
	case EGizmoAxis::Z:
	{
		u.set(1.0f, 0.0f, 0.0f);
		v.set(0.0f, 1.0f, 0.0f);
		break;
	}
	default:
	{
		u.set(1.0f, 0.0f, 0.0f);
		v.set(0.0f, 1.0f, 0.0f);
		break;
	}
	}
}

static SProjectedPoint ProjectWorld(const Fvector& world)
{
	SProjectedPoint result;
	Fvector4 clip = {};
	Device.mFullTransform.transform(clip, world);

	result.W = clip.w;

	if (clip.w <= 0.0001f)
	{
		return result;
	}

	const ImGuiIO& io = ImGui::GetIO();
	result.Screen.x = (clip.x + 1.0f) * 0.5f * io.DisplaySize.x;
	result.Screen.y = (1.0f - clip.y) * 0.5f * io.DisplaySize.y;
	result.Visible = true;
	return result;
}

static bool MouseRay(Fvector& origin, Fvector& direction)
{
	const ImGuiIO& io = ImGui::GetIO();

	if (io.DisplaySize.x <= 0.0f || io.DisplaySize.y <= 0.0f)
	{
		return false;
	}

	const float ndcX = (io.MousePos.x / io.DisplaySize.x) * 2.0f - 1.0f;
	const float ndcY = 1.0f - (io.MousePos.y / io.DisplaySize.y) * 2.0f;

	Fvector nearPoint = {};
	Fvector farPoint = {};
	Device.mInvFullTransform.transform(nearPoint, Fvector().set(ndcX, ndcY, 0.0f));
	Device.mInvFullTransform.transform(farPoint, Fvector().set(ndcX, ndcY, 1.0f));

	direction.sub(farPoint, nearPoint);

	if (direction.square_magnitude() < EPS)
	{
		return false;
	}

	direction.normalize();
	origin.set(nearPoint);
	return true;
}

static bool RaySphere(const Fvector& origin, const Fvector& direction, const Fvector& center, float radius, float& distance)
{
	Fvector offset = {};
	offset.sub(origin, center);

	const float b = offset.dotproduct(direction);
	const float c = offset.square_magnitude() - radius * radius;
	const float discriminant = b * b - c;

	if (discriminant < 0.0f)
	{
		return false;
	}

	const float root = sqrtf(discriminant);
	float nearDistance = -b - root;

	if (nearDistance < 0.0f)
	{
		nearDistance = -b + root;
	}

	if (nearDistance < 0.0f)
	{
		return false;
	}

	distance = nearDistance;
	return true;
}

static bool RayPlaneIntersect(const Fvector& origin, const Fvector& direction, const Fvector& point, const Fvector& normal, Fvector& intersection)
{
	const float denominator = direction.dotproduct(normal);

	if (fabsf(denominator) < 1e-4f)
	{
		return false;
	}

	Fvector offset = {};
	offset.sub(point, origin);

	const float distance = offset.dotproduct(normal) / denominator;

	if (distance < 0.0f)
	{
		return false;
	}

	intersection.mad(origin, direction, distance);
	return true;
}

static bool ClosestPointOnAxis(const Fvector& point, const Fvector& axis, const Fvector& origin, const Fvector& direction, float& parameter)
{
	Fvector offset = {};
	offset.sub(point, origin);

	const float b = axis.dotproduct(direction);
	const float d = axis.dotproduct(offset);
	const float e = direction.dotproduct(offset);
	const float denominator = 1.0f - b * b;

	if (fabsf(denominator) < 1e-5f)
	{
		return false;
	}

	parameter = (b * e - d) / denominator;
	return true;
}

static float DistanceToSegment(const ImVec2& point, const ImVec2& a, const ImVec2& b)
{
	const float dx = b.x - a.x;
	const float dy = b.y - a.y;
	const float lengthSquared = dx * dx + dy * dy;

	if (lengthSquared <= 1e-6f)
	{
		const float px = point.x - a.x;
		const float py = point.y - a.y;
		return sqrtf(px * px + py * py);
	}

	float t = ((point.x - a.x) * dx + (point.y - a.y) * dy) / lengthSquared;
	t = std::clamp(t, 0.0f, 1.0f);

	const float cx = a.x + dx * t;
	const float cy = a.y + dy * t;
	const float px = point.x - cx;
	const float py = point.y - cy;
	return sqrtf(px * px + py * py);
}

static bool ComputeMouseAngle(const Fvector& pivot, EGizmoAxis axis, float& angle, bool& screenFallback)
{
	Fvector origin = {};
	Fvector direction = {};

	if (!MouseRay(origin, direction))
	{
		return false;
	}

	const Fvector normal = AxisVector(axis);
	Fvector intersection = {};

	if (RayPlaneIntersect(origin, direction, pivot, normal, intersection))
	{
		Fvector offset = {};
		offset.sub(intersection, pivot);
		offset.mad(normal, -offset.dotproduct(normal));

		if (offset.square_magnitude() > 1e-6f)
		{
			offset.normalize();

			Fvector u = {};
			Fvector v = {};
			RotationBasis(axis, u, v);
			angle = atan2f(offset.dotproduct(v), offset.dotproduct(u));
			screenFallback = false;
			return true;
		}
	}

	const SProjectedPoint center = ProjectWorld(pivot);

	if (!center.Visible)
	{
		return false;
	}

	const ImGuiIO& io = ImGui::GetIO();
	angle = atan2f(io.MousePos.y - center.Screen.y, io.MousePos.x - center.Screen.x);
	screenFallback = true;
	return true;
}

static void SortSegmentList(xr_vector<STrimSegment>& segments)
{
	std::sort(segments.begin(), segments.end(), [](const STrimSegment& left, const STrimSegment& right)
	{
		return left.Start < right.Start;
	});
}

static bool DrawAnimationTimeline(const char* id, float& time, float total, const CMotionDef* motionDef, const SEditorTheme& theme,
	STrimRange* trim, s32& dragMarker, s32& dragIndex, xr_string& dragKey, const char* trimKey)
{
	const float width = std::max(ImGui::GetContentRegionAvail().x, 1.0f);
	constexpr float height = 64.0f;
	const ImVec2 origin = ImGui::GetCursorScreenPos();
	const ImVec2 size = ImVec2(width, height);
	const ImVec2 end = ImVec2(origin.x + size.x, origin.y + size.y);

	ImGui::InvisibleButton(id, size);

	bool changed = false;

	const float pixelsPerSecond = total > 0.0f ? width / total : 0.0f;
	const ImVec2 mouse = ImGui::GetIO().MousePos;
	const bool ctrlHeld = ImGui::GetIO().KeyCtrl;

	const auto timeToX = [&](float value)
	{
		return origin.x + std::clamp(value, 0.0f, total) * pixelsPerSecond;
	};

	const auto xToTime = [&](float x)
	{
		return std::clamp((x - origin.x) / width * total, 0.0f, total);
	};

	if (total > 0.0f && trim != nullptr)
	{
		s32 hoverSegment = -1;
		s32 hoverFlag = 0;
		float hoverDistance = 8.0f;

		for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
		{
			const STrimSegment& segment = trim->Segments[index];
			const float distanceIn = fabsf(mouse.x - timeToX(segment.Start));
			const float distanceOut = fabsf(mouse.x - timeToX(segment.End));

			if (distanceIn < hoverDistance)
			{
				hoverDistance = distanceIn;
				hoverSegment = index;
				hoverFlag = 1;
			}

			if (distanceOut < hoverDistance)
			{
				hoverDistance = distanceOut;
				hoverSegment = index;
				hoverFlag = 2;
			}
		}

		if (ImGui::IsItemHovered())
		{
			if (dragMarker == 0 && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
			{
				if (ctrlHeld)
				{
					STrimSegment segment;
					segment.Start = xToTime(mouse.x);
					segment.End = std::min(segment.Start + 0.2f, total);

					for (const STrimSegment& existing : trim->Segments)
					{
						if (segment.End > existing.Start && segment.Start < existing.End)
						{
							segment.End = existing.Start - 0.01f;
						}
					}

					if (segment.End - segment.Start >= 0.02f)
					{
						trim->Segments.push_back(segment);
						SortSegmentList(trim->Segments);

						for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
						{
							if (trim->Segments[index].Start == segment.Start && trim->Segments[index].End == segment.End)
							{
								dragIndex = index;
								break;
							}
						}

						dragMarker = 3;
						dragKey = trimKey;
					}
				}
				else if (hoverFlag != 0)
				{
					dragMarker = hoverFlag;
					dragIndex = hoverSegment;
					dragKey = trimKey;
				}
			}

			if (dragMarker == 0 && ImGui::IsMouseClicked(ImGuiMouseButton_Right))
			{
				const float value = xToTime(mouse.x);

				for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
				{
					const STrimSegment& segment = trim->Segments[index];

					if (value >= segment.Start && value <= segment.End)
					{
						trim->Segments.erase(trim->Segments.begin() + index);
						break;
					}
				}
			}
		}

		if (!ImGui::IsMouseDown(ImGuiMouseButton_Left) && dragMarker != 0)
		{
			dragMarker = 0;
			dragIndex = -1;
			dragKey.clear();
		}

		if (dragMarker != 0 && dragKey == trimKey && dragIndex >= 0 && dragIndex < static_cast<s32>(trim->Segments.size()))
		{
			STrimSegment& segment = trim->Segments[dragIndex];
			const float value = xToTime(mouse.x);
			const float lowerBound = dragIndex > 0 ? trim->Segments[dragIndex - 1].End : 0.0f;
			const float upperBound = dragIndex + 1 < static_cast<s32>(trim->Segments.size()) ? trim->Segments[dragIndex + 1].Start : total;

			if (dragMarker == 1)
			{
				segment.Start = std::clamp(value, lowerBound, std::max(lowerBound, segment.End - 0.02f));
			}
			else
			{
				segment.End = std::clamp(value, segment.Start + 0.02f, std::max(segment.Start + 0.02f, upperBound));
			}
		}

		if (dragMarker == 0 && ImGui::IsItemActive())
		{
			time = xToTime(mouse.x);
			changed = true;
		}
	}

	ImDrawList* draw = ImGui::GetWindowDrawList();
	const ImU32 colorBackground = theme.TimelineBackground;
	const ImU32 colorElapsed = theme.TimelineElapsed;
	const ImU32 colorMark = theme.TimelineMark;
	const ImU32 colorTickMinor = theme.TimelineTickMinor;
	const ImU32 colorTickMajor = theme.TimelineTickMajor;
	const ImU32 colorText = theme.TimelineText;
	const ImU32 colorPlayhead = theme.TimelinePlayhead;
	const ImU32 colorBorder = theme.TimelineBorder;
	const ImU32 colorTrimIn = IM_COL32(120, 230, 150, 240);
	const ImU32 colorTrimOut = IM_COL32(255, 150, 80, 240);

	draw->AddRectFilled(origin, end, colorBackground);

	if (total > 0.0f)
	{
		const float playedX = origin.x + std::clamp(time, 0.0f, total) * pixelsPerSecond;

		draw->AddRectFilled(ImVec2(origin.x + 1.0f, origin.y + 1.0f), ImVec2(playedX, origin.y + height - 1.0f), colorElapsed);

		if (motionDef)
		{
			for (const motion_marks& mark : motionDef->marks)
			{
				for (const motion_marks::interval& interval : mark.intervals)
				{
					const float x0 = origin.x + std::max(0.0f, interval.first) * pixelsPerSecond;
					const float x1 = origin.x + std::max(0.0f, interval.second) * pixelsPerSecond;
					draw->AddRectFilled(ImVec2(x0, origin.y + 1.0f), ImVec2(std::max(x1, x0 + 1.0f), origin.y + height - 1.0f), colorMark);
				}
			}
		}

		const float steps[] = { 0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1.0f, 2.0f, 5.0f, 10.0f, 30.0f, 60.0f };
		float step = steps[IM_ARRAYSIZE(steps) - 1];

		for (const float candidate : steps)
		{
			if (candidate * pixelsPerSecond >= 64.0f)
			{
				step = candidate;
				break;
			}
		}

		const int majorCount = std::max(1, iFloor(total / step + 0.5f));
		string32 label = {};

		for (int major = 0; major <= majorCount; ++major)
		{
			const float majorTime = major * step;
			const float majorX = origin.x + majorTime * pixelsPerSecond;

			draw->AddLine(ImVec2(majorX, origin.y + 1.0f), ImVec2(majorX, origin.y + height - 1.0f), colorTickMajor);

			if (major < majorCount)
			{
				xr_sprintf(label, step >= 1.0f ? "%.1f" : "%.2f", majorTime);
				draw->AddText(ImVec2(majorX + 3.0f, origin.y + 3.0f), colorText, label);

				for (int minor = 1; minor < 5; ++minor)
				{
					const float minorTime = majorTime + step * minor / 5.0f;

					if (minorTime > total)
					{
						break;
					}

					const float minorX = origin.x + minorTime * pixelsPerSecond;
					draw->AddLine(ImVec2(minorX, origin.y + height - 10.0f), ImVec2(minorX, origin.y + height - 1.0f), colorTickMinor);
				}
			}
		}

		if (trim != nullptr && !trim->Segments.empty())
		{
			float cursor = 0.0f;

			for (const STrimSegment& segment : trim->Segments)
			{
				if (segment.Start > cursor)
				{
					draw->AddRectFilled(ImVec2(timeToX(cursor), origin.y + 1.0f), ImVec2(timeToX(segment.Start), origin.y + height - 1.0f), IM_COL32(0, 0, 0, 120));
				}

				draw->AddRectFilled(ImVec2(timeToX(segment.Start), origin.y + 1.0f), ImVec2(timeToX(segment.End), origin.y + height - 1.0f), IM_COL32(255, 255, 255, 14));
				cursor = std::max(cursor, segment.End);
			}

			if (cursor < total)
			{
				draw->AddRectFilled(ImVec2(timeToX(cursor), origin.y + 1.0f), ImVec2(end.x - 1.0f, origin.y + height - 1.0f), IM_COL32(0, 0, 0, 120));
			}
		}

		draw->AddLine(ImVec2(playedX, origin.y), ImVec2(playedX, origin.y + height), colorPlayhead, 2.0f);
		draw->AddTriangleFilled(ImVec2(playedX - 5.0f, origin.y), ImVec2(playedX + 5.0f, origin.y), ImVec2(playedX, origin.y + 7.0f), colorPlayhead);

		if (trim != nullptr)
		{
			for (const STrimSegment& segment : trim->Segments)
			{
				const float inX = timeToX(segment.Start);
				const float outX = timeToX(segment.End);

				draw->AddLine(ImVec2(inX, origin.y), ImVec2(inX, origin.y + height), colorTrimIn, 2.0f);
				draw->AddTriangleFilled(ImVec2(inX, origin.y), ImVec2(inX + 7.0f, origin.y), ImVec2(inX, origin.y + 7.0f), colorTrimIn);

				draw->AddLine(ImVec2(outX, origin.y), ImVec2(outX, origin.y + height), colorTrimOut, 2.0f);
				draw->AddTriangleFilled(ImVec2(outX, origin.y + height), ImVec2(outX, origin.y + height - 7.0f), ImVec2(outX - 7.0f, origin.y + height), colorTrimOut);
			}
		}

		if (ImGui::IsItemHovered())
		{
			const float hoverTime = xToTime(mouse.x);
			ImGui::SetTooltip("Time: %.3f s\nFrame: %d\nCtrl+drag - add segment, drag a flag - move it, right click on a segment - delete", hoverTime, iFloor(hoverTime / SAMPLE_SPF + 0.5f));
		}
	}
	else
	{
		const char* message = "no animation";
		const ImVec2 messageSize = ImGui::CalcTextSize(message);
		draw->AddText(ImVec2(origin.x + (width - messageSize.x) * 0.5f, origin.y + (height - messageSize.y) * 0.5f), colorText, message);
	}

	draw->AddRect(origin, end, colorBorder);
	return changed;
}


class CCinemaFilmMakerManager final : public pureFrame
{
public:
	virtual ~CCinemaFilmMakerManager() = default;

	enum class ESceneDialog : u8
	{
		None,
		Info,
		Save,
		Load,
		Confirm,
		Preset
	};

	void OnFrame() override;
	void Render();

	void DrawPanel();
	void DrawGizmo();

	void PushThemeStyle();
	void PopThemeStyle();

	void Shutdown();

private:
	void ClearActors();
	void ClearGizmo();
	void ResetSelection();
	void SelectActor(s32 index);
	void LoadSettings();
	void SaveSettings();
	const SEditorTheme& GetTheme() const;

	void BuildModelFiles();
	void BuildHandItems();
	void BuildLibrary();
	void BuildActorSources(SCinemaActor& actor);

	bool AddActor(const Fvector& position, const char* visual, bool adjustGround);
	bool AddActorInFront(const char* visual);
	bool SetActorVisual(SCinemaActor& actor, const char* visual);
	bool SnapActorToGround(SCinemaActor& actor); 
	void SetActorItem(SCinemaActor& actor, const shared_str& section);
	void RefreshActorItemBones(SCinemaActor& actor);
	void DuplicateSelectedActor(bool withPlaylist, bool startPlayback);
	void DeleteSelectedActor();

	void RequestSaveScene();
	void SaveSceneFile(const char* name);
	void RequestLoadScene();
	void BuildSceneList();
	void LoadSceneFile(const char* name);
	void ApplyLoadedScene();
	void ShowSceneDialog(ESceneDialog mode, const char* title, const xr_vector<xr_string>& lines);
	void DrawSceneDialog();
	bool RestoreActorPose(SCinemaActor& actor, const SSceneActorData& data);

	void ApplyExtraSources(SCinemaActor& actor);

	const STrimRange* FindTrim(const SCinemaActor& actor, const shared_str& source, const shared_str& name) const;
	STrimRange* FindTrimEdit(SCinemaActor& actor, const shared_str& source, const shared_str& name);
	void UpdateActorTrim(SCinemaActor& actor);
	void StartBlendJump(SCinemaActor& actor, float targetTime, float blendTime);
	bool DrawTimelineWidget(float& time, float total, const CMotionDef* motionDef, SCinemaActor& actor);
	void AddExtraAnimationPath(const char* path);
	void RemoveExtraAnimationPath(s32 index);
	void DrawExtraSources();

	void LoadPresets();
	void SavePresets();
	void RequestSavePreset();
	void SavePresetFromActor(const char* name);
	void SpawnFromPreset(s32 index);
	void ApplyPresetToActor(s32 index);
	void DeletePreset(s32 index);
	s32 FindPreset(const char* name) const;
	void DrawPresetTab();

	void DrawToolbar();
	void DrawSelectedActor();
	void DrawModelTab();
	void DrawItemTab();
	void DrawAnimationTab();
	void DrawPlaylist(SCinemaActor& actor);
	void DrawClipTimelines(SCinemaActor& actor);
	void DrawClipTimeline(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip);
	float GetClipSpeed(const SCinemaActor& actor, const STrimRange* trim) const;
	void RemoveTrim(SCinemaActor& actor, const shared_str& source, const shared_str& name);

	u32 AxisColor(EGizmoAxis axis, bool highlighted) const;
	void AppendTranslateGizmo(LevelInspector& inspector, const Fvector& pivot, float size, EGizmoAxis hoveredAxis);
	void AppendRotateGizmo(LevelInspector& inspector, const Fvector& pivot, float size, EGizmoAxis hoveredAxis);
	void AppendWorldGizmo();
	EGizmoAxis HitTranslateAxis(const Fvector& pivot, float size, const ImVec2& mouse) const;
	EGizmoAxis HitRotateAxis(const Fvector& pivot, float size, const ImVec2& mouse) const;
	void BeginGizmo(SCinemaActor& actor, EGizmoAxis axis);
	void UpdateGizmo(SCinemaActor& actor);
	void PickActorAtMouse();

	void UpdateActorPlayback(SCinemaActor& actor);
	void AdvanceActorPlaylist(SCinemaActor& actor);
	bool TryPlayPlaylistFrom(SCinemaActor& actor, s32 start);
	bool PlayPlaylistEntry(SCinemaActor& actor, s32 index);
	bool PlayClip(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip, bool playlist);
	bool EnsureSource(SCinemaActor& actor, const shared_str& source);
	bool ApplyDefaultPose(SCinemaActor& actor);
	void PreviewClip(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip);
	void StartPlaylist(SCinemaActor& actor, s32 start);
	void StartActorPlayback(SCinemaActor& actor);
	void StartPlayAll();
	void StopActor(SCinemaActor& actor);
	void StopAll();
	void TogglePause(SCinemaActor& actor);
	void ApplySpeed(SCinemaActor& actor);
	void MovePlaylistEntry(SCinemaActor& actor, s32 from, s32 to);
	void RemovePlaylistEntry(SCinemaActor& actor, s32 index);
	void AddClipToPlaylist(SCinemaActor& actor, const shared_str& source, const shared_str& name, bool fx);

	const SAnimationSource* FindSource(const SCinemaActor& actor, const shared_str& id) const;
	const SAnimationClip* FindClip(const SAnimationSource& source, const shared_str& name, bool fx) const;
	bool IsActorPlaying(const SCinemaActor& actor) const;
	SCinemaActor* GetSelectedActor();
	const SCinemaActor* GetSelectedActor() const;

	xr_vector<xr_unique_ptr<SCinemaActor>> Actors;
	xr_vector<SAnimationSource> Library;
	xr_vector<shared_str> ModelFiles;
	xr_vector<SHandItem> HandItems;
	IGame_Level* Level = nullptr;
	s32 Selected = -1;
	s32 SelectedOgf = -1;
	s32 SelectedSource = -1;
	s32 SelectedClip = -1;
	s32 NameEditActor = -1;
	shared_str SelectedVisual;
	u32 NextNameIndex = 1;
	bool LibraryBuilt = false;
	bool ModelFilesBuilt = false;
	bool HandItemsBuilt = false;
	bool Snap = false;
	float TranslateStep = 0.1f;
	float RotateStepDeg = 5.0f;
	char ModelFileFilter[128] = {};
	char ItemFilter[128] = {};
	char ClipFilter[128] = {};
	char NameEdit[128] = {};
	EGizmoMode GizmoMode = EGizmoMode::Translate;
	EGizmoAxis HoveredAxis = EGizmoAxis::None;
	SGizmoDrag Gizmo;
	xr_vector<SSceneActorData> SceneData;
	xr_vector<xr_string> SceneDialogLines;
	xr_vector<xr_string> SceneList;
	xr_string SceneDialogTitle;
	xr_string SceneListSelected;
	xr_string SceneNameLast;
	char SceneNameEdit[128] = {};
	ESceneDialog SceneDialogMode = ESceneDialog::None;
	bool SceneDialogRequested = false;
	bool SceneLoadRequested = false;
	s32 ThemeIndex = 0;
	bool SettingsLoaded = false;
	bool SpawnFromCamera = false;
	bool SnapToGround = true;
	s32 TrimDragMarker = 0;
	s32 TrimDragIndex = -1;
	xr_string TrimDragKey;
	bool TimelinesOpenByDefault = true;
	bool TimelinesOpenDirty = true;
	xr_vector<SNpcPreset> Presets;
	xr_vector<xr_string> ExtraAnimationPaths;
	xr_string ExtraPathStatus;
	char ExtraPathEdit[260] = {};
	s32 PresetSelected = -1;
	bool PresetsLoaded = false;
	char PresetNameEdit[128] = {};
};

CCinemaFilmMakerManager* g_CinemaFilmMaker = nullptr;

void CCinemaFilmMakerManager::Shutdown()
{
	ClearGizmo();
	ClearActors();
	Library.clear();
	LibraryBuilt = false;
	ModelFiles.clear();
	ModelFilesBuilt = false;
	HandItems.clear();
	HandItemsBuilt = false;
	SceneData.clear();
	SceneDialogLines.clear();
	SceneList.clear();
	SceneListSelected.clear();
	SceneNameLast.clear();
	Presets.clear();
	PresetsLoaded = false;
	PresetSelected = -1;
	SceneDialogMode = ESceneDialog::None;
	SceneDialogRequested = false;
	SceneLoadRequested = false;
	Level = nullptr;
	Selected = -1;
}

const SEditorTheme& CCinemaFilmMakerManager::GetTheme() const
{
	if (ThemeIndex < 0 || ThemeIndex >= kCinemaThemeCount)
	{
		return kCinemaThemes[0];
	}

	return kCinemaThemes[ThemeIndex];
}

void CCinemaFilmMakerManager::PushThemeStyle()
{
	LoadSettings();

	const SEditorTheme& theme = GetTheme();

	for (const SThemeColorLink& link : kCinemaThemeColors)
	{
		ImGui::PushStyleColor(link.Color, theme.*link.Value);
	}

	ImGui::PushStyleColor(ImGuiCol_TreeLines, ImVec4(theme.Text.x, theme.Text.y, theme.Text.z, 0.3f));
}

void CCinemaFilmMakerManager::PopThemeStyle()
{
	ImGui::PopStyleColor(static_cast<int>(std::size(kCinemaThemeColors)) + 1);
}

void CCinemaFilmMakerManager::LoadSettings()
{
	if (SettingsLoaded)
	{
		return;
	}

	SettingsLoaded = true;

	string_path path = {};
	FS.update_path(path, "$app_data_root$", kCinemaSettingsFile);

	std::error_code error;

	if (!std::filesystem::exists(path, error))
	{
		return;
	}

	CInifile ini(path, TRUE, TRUE, FALSE);

	if (!ini.section_exist("editor"))
	{
		return;
	}

	SpawnFromCamera = ini.read_if_exists<bool>("editor", "spawn_from_camera", SpawnFromCamera);
	SnapToGround = ini.read_if_exists<bool>("editor", "snap_to_ground", SnapToGround);
	TimelinesOpenByDefault = ini.read_if_exists<bool>("editor", "timelines_open", TimelinesOpenByDefault);

	for (s32 index = 0; index < 64; ++index)
	{
		string32 key = {};
		xr_sprintf(key, "extra_%d", index);

		if (!ini.line_exist("editor", key))
		{
			break;
		}

		const char* value = ini.r_string("editor", key);

		if (value != nullptr && value[0] != '\0')
		{
			ExtraAnimationPaths.emplace_back(value);
		}
	}

	const char* themeId = ini.line_exist("editor", "theme") ? ini.r_string("editor", "theme") : nullptr;

	if (themeId == nullptr)
	{
		return;
	}

	for (s32 index = 0; index < kCinemaThemeCount; ++index)
	{
		if (xr_strcmp(kCinemaThemes[index].Id, themeId) == 0)
		{
			ThemeIndex = index;
			return;
		}
	}
}

void CCinemaFilmMakerManager::SaveSettings()
{
	string_path path = {};
	FS.update_path(path, "$app_data_root$", kCinemaSettingsFile);

	CInifile ini(nullptr, FALSE, FALSE, FALSE);
	ini.w_string("editor", "theme", GetTheme().Id);
	ini.w_bool("editor", "spawn_from_camera", SpawnFromCamera);
	ini.w_bool("editor", "snap_to_ground", SnapToGround);
	ini.w_bool("editor", "timelines_open", TimelinesOpenByDefault);

	for (u32 index = 0; index < static_cast<u32>(ExtraAnimationPaths.size()); ++index)
	{
		string32 key = {};
		xr_sprintf(key, "extra_%u", index);
		ini.w_string("editor", key, ExtraAnimationPaths[index].c_str());
	}

	if (ini.save_as(path))
	{
		FS.file_update(path);
	}
}

u32 CCinemaFilmMakerManager::AxisColor(EGizmoAxis axis, bool highlighted) const
{
	const SEditorTheme& theme = GetTheme();

	if (highlighted)
	{
		return theme.AxisHighlighted;
	}

	switch (axis)
	{
	case EGizmoAxis::X:
		return theme.AxisX;
	case EGizmoAxis::Y:
		return theme.AxisY;
	case EGizmoAxis::Z:
		return theme.AxisZ;
	default:
		return theme.AxisPivot;
	}
}


void CCinemaFilmMakerManager::ClearGizmo()
{
	Gizmo = SGizmoDrag();
}

void CCinemaFilmMakerManager::ClearActors()
{
	for (auto& actor : Actors)
	{
		actor->PlaylistIndex = -1;
		actor->Paused = false;
		ReleaseActorItem(*actor);
		ReleaseActorModel(*actor);
	}

	Actors.clear();
	Selected = -1;
	SelectedSource = -1;
	SelectedClip = -1;
	NameEditActor = -1;
}

void CCinemaFilmMakerManager::ResetSelection()
{
	SelectedSource = -1;
	SelectedClip = -1;
	NameEditActor = -1;
}

void CCinemaFilmMakerManager::SelectActor(s32 index)
{
	if (index < -1 || index >= static_cast<s32>(Actors.size()))
	{
		index = -1;
	}

	if (Selected == index)
	{
		return;
	}

	ClearGizmo();
	Selected = index;
	TimelinesOpenDirty = true;
	TrimDragMarker = 0;
	TrimDragIndex = -1;
	TrimDragKey.clear();
	ResetSelection();
}

SCinemaActor* CCinemaFilmMakerManager::GetSelectedActor()
{
	if (Selected < 0 || Selected >= static_cast<s32>(Actors.size()))
	{
		return nullptr;
	}

	return Actors[Selected].get();
}

const SCinemaActor* CCinemaFilmMakerManager::GetSelectedActor() const
{
	if (Selected < 0 || Selected >= static_cast<s32>(Actors.size()))
	{
		return nullptr;
	}

	return Actors[Selected].get();
}

void CCinemaFilmMakerManager::OnFrame()
{
	if (!g_CinemaFilmMaker)
	{
		return;
	}

	if (Level != g_pGameLevel)
	{
		ClearGizmo();
		ClearActors();
		Library.clear();
		LibraryBuilt = false;
		SceneData.clear();
		SceneList.clear();
		SceneListSelected.clear();
		SceneLoadRequested = false;
		TrimDragMarker = 0;
		TrimDragIndex = -1;
		TrimDragKey.clear();
		Level = g_pGameLevel;
		Selected = -1;
		SelectedSource = -1;
		SelectedClip = -1;
		NameEditActor = -1;
	}

	if (Level == nullptr)
	{
		return;
	}

	if (!LibraryBuilt && Level->bReady)
	{
		BuildLibrary();
	}

	for (auto& actor : Actors)
	{
		UpdateActorTrim(*actor);
		UpdateActorPlayback(*actor);
		UpdateActorPlacement(*actor);
	}
}

void CCinemaFilmMakerManager::Render()
{
	AppendWorldGizmo();
}

void CCinemaFilmMakerManager::BuildModelFiles()
{
	if (ModelFilesBuilt)
	{
		return;
	}

	ModelFilesBuilt = true;
	ModelFiles.clear();

	FS_FileSet files;
	FS.file_list(files, _game_meshes_, FS_ListFiles, "*.ogf");

	ModelFiles.reserve(files.size());

	for (const FS_File& file : files)
	{
		ModelFiles.emplace_back(file.name.c_str());
	}

	std::sort(ModelFiles.begin(), ModelFiles.end(), [](const shared_str& left, const shared_str& right)
	{
		return xr_strcmp(left.c_str(), right.c_str()) < 0;
	});

	if (SelectedVisual.size() == 0 && !ModelFiles.empty())
	{
		SelectedOgf = 0;
		SelectedVisual = ModelFiles.front();
	}
}

void CCinemaFilmMakerManager::BuildHandItems()
{
	if (HandItemsBuilt)
	{
		return;
	}

	HandItemsBuilt = true;
	HandItems.clear();

	if (pSettings == nullptr)
	{
		return;
	}

	for (CInifile::Sect& section : pSettings->sections())
	{
		if (!section.line_exist("visual"))
		{
			continue;
		}

		const bool weaponStyle = section.line_exist("position") && section.line_exist("orientation");
		bool attachStyle = section.line_exist("attach_bone_name") && section.line_exist("attach_angle_offset") && section.line_exist("attach_position_offset");

		if (attachStyle)
		{
			const shared_str boneName = pSettings->r_string(section.Name.c_str(), "attach_bone_name");
			attachStyle = boneName.size() > 0;
		}

		if (!weaponStyle && !attachStyle)
		{
			continue;
		}

		const shared_str visual = pSettings->r_string(section.Name.c_str(), "visual");

		if (!VisualPathExists(visual.c_str()))
		{
			continue;
		}

		HandItems.push_back({ .Section = section.Name, .Visual = visual });
	}

	std::sort(HandItems.begin(), HandItems.end(), [](const SHandItem& left, const SHandItem& right)
	{
		return xr_strcmp(left.Section.c_str(), right.Section.c_str()) < 0;
	});
}

void CCinemaFilmMakerManager::BuildLibrary()
{
	Library.clear();

	if (g_pGameLevel == nullptr || !g_pGameLevel->bReady)
	{
		LibraryBuilt = false;
		return;
	}

	LibraryBuilt = true;

	xr_set<shared_str> processed;

	auto collect = [&](IKinematicsAnimated* kinematics)
	{
		if (kinematics == nullptr)
		{
			return;
		}

		IKinematics* skeleton = kinematics->dcast_PKinematics();

		if (skeleton == nullptr)
		{
			return;
		}

		for (u16 slot = 0, count = kinematics->LL_MotionsSlotCount(); slot < count; ++slot)
		{
			shared_motions motions = kinematics->LL_MotionsSlot(slot);

			if (motions.motion_map() == nullptr)
			{
				continue;
			}

			const shared_str& id = motions.id();

			if (processed.contains(id))
			{
				continue;
			}

			processed.insert(id);

			SAnimationSource source;
			source.Id = id;
			FillSourceBones(source, skeleton);
			AppendClips(motions, source.Clips);

			if (!source.Clips.empty())
			{
				Library.push_back(std::move(source));
			}
		}
	};

	if (g_actor != nullptr && g_actor->Visual() != nullptr)
	{
		collect(g_actor->Visual()->dcast_PKinematicsAnimated());
	}

	if (g_pGameLevel->bReady)
	{
		for (u32 i = 0; i < g_pGameLevel->Objects.o_count(); ++i)
		{
			CObject* object = g_pGameLevel->Objects.o_get_by_iterator(i);

			if (object != nullptr && object->Visual() != nullptr)
			{
				collect(object->Visual()->dcast_PKinematicsAnimated());
			}
		}
	}

	std::sort(Library.begin(), Library.end(), [](const SAnimationSource& left, const SAnimationSource& right)
	{
		return xr_strcmp(left.Id.c_str(), right.Id.c_str()) < 0;
	});

	for (auto& actor : Actors)
	{
		BuildActorSources(*actor);
	}

	SelectedSource = -1;
	SelectedClip = -1;
}

void CCinemaFilmMakerManager::BuildActorSources(SCinemaActor& actor)
{
	actor.Sources.clear();

	if (actor.Animated == nullptr || actor.Skeleton == nullptr)
	{
		return;
	}

	ApplyExtraSources(actor);

	xr_set<shared_str> added;

	for (u16 slot = 0, count = actor.Animated->LL_MotionsSlotCount(); slot < count; ++slot)
	{
		shared_motions motions = actor.Animated->LL_MotionsSlot(slot);

		if (motions.motion_map() == nullptr)
		{
			continue;
		}

		const shared_str& id = motions.id();

		if (added.contains(id))
		{
			continue;
		}

		added.insert(id);

		SAnimationSource source;
		source.Id = id;
		FillSourceBones(source, actor.Skeleton);
		AppendClips(motions, source.Clips);

		if (!source.Clips.empty())
		{
			actor.Sources.push_back(std::move(source));
		}
	}

	for (const SAnimationSource& source : Library)
	{
		if (added.contains(source.Id))
		{
			continue;
		}

		if (!CinemaIsCompatible(source, actor.Skeleton))
		{
			continue;
		}

		if (!CinemaIsOmfPath(source.Id))
		{
			continue;
		}

		added.insert(source.Id);
		actor.Sources.push_back(source);
	}

	std::sort(actor.Sources.begin(), actor.Sources.end(), [](const SAnimationSource& left, const SAnimationSource& right)
	{
		return xr_strcmp(left.Id.c_str(), right.Id.c_str()) < 0;
	});
}

const SAnimationSource* CCinemaFilmMakerManager::FindSource(const SCinemaActor& actor, const shared_str& id) const
{
	for (const SAnimationSource& source : actor.Sources)
	{
		if (xr_strcmp(source.Id.c_str(), id.c_str()) == 0)
		{
			return &source;
		}
	}

	return nullptr;
}

const SAnimationClip* CCinemaFilmMakerManager::FindClip(const SAnimationSource& source, const shared_str& name, bool fx) const
{
	for (const SAnimationClip& clip : source.Clips)
	{
		if (clip.Fx == fx && clip.Name == name)
		{
			return &clip;
		}
	}

	return nullptr;
}

bool CCinemaFilmMakerManager::EnsureSource(SCinemaActor& actor, const shared_str& source)
{
	if (actor.Animated == nullptr)
	{
		return false;
	}

	for (u16 slot = 0, count = actor.Animated->LL_MotionsSlotCount(); slot < count; ++slot)
	{
		if (xr_strcmp(actor.Animated->LL_MotionsSlot(slot).id().c_str(), source.c_str()) == 0)
		{
			return true;
		}
	}

	if (!CinemaIsOmfPath(source))
	{
		return false;
	}

	string_path path = {};
	xr_strcpy(path, source.c_str());
	path[source.size() - 4] = '\0';

	actor.Animated->append_motion_from_path(actor.Visual.c_str(), path);
	return true;
}

bool CCinemaFilmMakerManager::ApplyDefaultPose(SCinemaActor& actor)
{
	if (actor.Animated == nullptr)
	{
		return false;
	}

	const SAnimationSource* source = nullptr;
	const SAnimationClip* clip = CinemaFindEditorClip(actor, &source);

	if (clip == nullptr)
	{
		clip = CinemaFindDefaultIdle(actor, &source);
	}

	if (source != nullptr && clip != nullptr && PlayClip(actor, *source, *clip, false))
	{
		CinemaSetBlendLoop(actor.Animated, actor.Motion, true);
		actor.PlaylistIndex = -1;
		actor.Paused = false;
		return true;
	}

	return CinemaSetBindPose(actor);
}

bool CCinemaFilmMakerManager::PlayClip(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip, bool playlist)
{
	if (actor.Animated == nullptr)
	{
		return false;
	}

	CinemaClearBindPose(actor);

	if (!EnsureSource(actor, source.Id))
	{
		return false;
	}

	const MotionID motion = CinemaFindMotion(actor.Animated, source, clip);

	if (!motion.valid())
	{
		return false;
	}

	if (clip.Fx)
	{
		actor.Animated->PlayFX(motion, 1.0f);
		actor.FxDeadline = Device.fTimeGlobal + actor.Animated->get_animation_length(motion);
	}
	else
	{
		const STrimRange* clipSetup = FindTrim(actor, source.Id, clip.Name);
		const float clipSpeed = GetClipSpeed(actor, clipSetup);

		PlayMotionByParts(actor.Animated, motion, true, nullptr, nullptr);
		CinemaSetBlendLoop(actor.Animated, motion, playlist ? false : actor.Loop);
		CinemaSetBlendSpeed(actor.Animated, motion, clipSpeed);
		actor.PlayDirection = clipSpeed >= 0.0f ? 1.0f : -1.0f;
	}

	actor.Motion = motion;
	actor.MotionName = clip.Name;
	actor.MotionSource = source.Id;
	actor.MotionFx = clip.Fx;
	actor.Paused = false;

	if (const STrimRange* trim = FindTrim(actor, actor.MotionSource, actor.MotionName))
	{
		if (!trim->Segments.empty())
		{
			const float total = actor.Animated->get_animation_length(actor.Motion);
			const float start = std::clamp(trim->Segments.front().Start, 0.0f, std::max(0.0f, total - 0.02f));
			CinemaSetBlendTime(actor.Animated, actor.Motion, start);
		}
	}

	return true;
}

void CCinemaFilmMakerManager::PreviewClip(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip)
{
	actor.PlaylistIndex = -1;
	actor.Paused = false;
	PlayClip(actor, source, clip, false);
}

bool CCinemaFilmMakerManager::PlayPlaylistEntry(SCinemaActor& actor, s32 index)
{
	if (index < 0 || index >= static_cast<s32>(actor.Playlist.size()))
	{
		return false;
	}

	const SPlaylistEntry& entry = actor.Playlist[index];
	const SAnimationSource* source = FindSource(actor, entry.Source);

	if (source == nullptr)
	{
		return false;
	}

	const SAnimationClip* clip = FindClip(*source, entry.Name, entry.Fx);

	if (clip == nullptr)
	{
		return false;
	}

	if (!PlayClip(actor, *source, *clip, true))
	{
		return false;
	}

	actor.PlaylistIndex = index;
	return true;
}

bool CCinemaFilmMakerManager::TryPlayPlaylistFrom(SCinemaActor& actor, s32 start)
{
	const s32 count = static_cast<s32>(actor.Playlist.size());

	if (count == 0)
	{
		return false;
	}

	if (start < 0 || start >= count)
	{
		start = 0;
	}

	if (actor.Loop)
	{
		for (s32 i = 0; i < count; ++i)
		{
			const s32 index = (start + i) % count;

			if (PlayPlaylistEntry(actor, index))
			{
				return true;
			}
		}
	}
	else
	{
		for (s32 index = start; index < count; ++index)
		{
			if (PlayPlaylistEntry(actor, index))
			{
				return true;
			}
		}
	}

	actor.PlaylistIndex = -1;
	return false;
}

void CCinemaFilmMakerManager::AdvanceActorPlaylist(SCinemaActor& actor)
{
	const s32 count = static_cast<s32>(actor.Playlist.size());

	if (count == 0)
	{
		actor.PlaylistIndex = -1;
		return;
	}

	s32 start = actor.PlaylistIndex + 1;

	if (start >= count)
	{
		if (!actor.Loop)
		{
			actor.PlaylistIndex = -1;
			return;
		}

		start = 0;
	}

	if (!TryPlayPlaylistFrom(actor, start))
	{
		actor.PlaylistIndex = -1;
	}
}

static bool CinemaSameText(const shared_str& left, const shared_str& right)
{
	if (left.size() == 0 || right.size() == 0)
	{
		return left.size() == right.size();
	}

	return xr_strcmp(left.c_str(), right.c_str()) == 0;
}

const STrimRange* CCinemaFilmMakerManager::FindTrim(const SCinemaActor& actor, const shared_str& source, const shared_str& name) const
{
	for (const STrimRange& trim : actor.Trims)
	{
		if (CinemaSameText(trim.Source, source) && CinemaSameText(trim.Name, name))
		{
			return &trim;
		}
	}

	return nullptr;
}

STrimRange* CCinemaFilmMakerManager::FindTrimEdit(SCinemaActor& actor, const shared_str& source, const shared_str& name)
{
	for (STrimRange& trim : actor.Trims)
	{
		if (CinemaSameText(trim.Source, source) && CinemaSameText(trim.Name, name))
		{
			return &trim;
		}
	}

	return nullptr;
}

float CCinemaFilmMakerManager::GetClipSpeed(const SCinemaActor& actor, const STrimRange* trim) const
{
	return trim != nullptr ? trim->Speed : actor.Speed;
}

void CCinemaFilmMakerManager::RemoveTrim(SCinemaActor& actor, const shared_str& source, const shared_str& name)
{
	for (size_t index = 0; index < actor.Trims.size();)
	{
		if (CinemaSameText(actor.Trims[index].Source, source) && CinemaSameText(actor.Trims[index].Name, name))
		{
			actor.Trims.erase(actor.Trims.begin() + index);
			continue;
		}

		++index;
	}
}

void CCinemaFilmMakerManager::StartBlendJump(SCinemaActor& actor, float targetTime, float blendTime)
{
	if (actor.Animated == nullptr || !actor.Motion.valid())
	{
		return;
	}

	const float total = actor.Animated->get_animation_length(actor.Motion);
	const float time = std::clamp(targetTime, 0.0f, std::max(0.0f, total - END_EPS));

	if (blendTime <= 0.0f)
	{
		CinemaSetBlendTime(actor.Animated, actor.Motion, time);
		return;
	}

	CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);

	if (PlayMotionByParts(actor.Animated, actor.Motion, true, nullptr, nullptr) == nullptr)
	{
		CinemaSetBlendTime(actor.Animated, actor.Motion, time);
		return;
	}

	CinemaSetBlendLoop(actor.Animated, actor.Motion, false);
	CinemaSetBlendSpeed(actor.Animated, actor.Motion, 0.0f);
	CinemaSetBlendTime(actor.Animated, actor.Motion, time);

	if (CBlend* fresh = CinemaFindBlend(actor.Animated, actor.Motion))
	{
		fresh->blendAccrue = 1.0f / blendTime;
	}
}

void CCinemaFilmMakerManager::UpdateActorTrim(SCinemaActor& actor)
{
	if (actor.Animated == nullptr || !actor.Motion.valid() || actor.MotionFx || actor.Paused)
	{
		return;
	}

	CBlend* blend = CinemaFindBlend(actor.Animated, actor.Motion);

	if (blend == nullptr || !blend->playing || blend->timeTotal <= 0.0f)
	{
		return;
	}

	const float total = blend->timeTotal;
	const STrimRange* trim = FindTrim(actor, actor.MotionSource, actor.MotionName);
	const bool pingPong = actor.PingPong;
	const float speed = GetClipSpeed(actor, trim);
	const bool hasSegments = trim != nullptr && !trim->Segments.empty();
	const float blendTime = trim != nullptr ? trim->Blend : 0.0f;

	if (!hasSegments && !pingPong)
	{
		blend->speed = speed;
		actor.PlayDirection = speed >= 0.0f ? 1.0f : -1.0f;
		return;
	}

	if (actor.PlayDirection < 0.0f && speed >= 0.0f)
	{
		actor.PlayDirection = 1.0f;
	}
	else if (actor.PlayDirection > 0.0f && speed < 0.0f)
	{
		actor.PlayDirection = -1.0f;
	}

	blend->speed = 0.0f;

	const float delta = Device.fTimeDelta * fabsf(speed);

	if (delta <= 0.0f)
	{
		return;
	}

	STrimSegment chain;
	chain.Start = 0.0f;
	chain.End = total - END_EPS;

	if (hasSegments)
	{
		chain.Start = trim->Segments.front().Start;
		chain.End = trim->Segments.back().End;
	}

	const bool forward = actor.PlayDirection > 0.0f;
	float time = blend->timeCurrent + actor.PlayDirection * delta;

	if (hasSegments)
	{
		s32 currentIndex = -1;

		for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
		{
			const STrimSegment& segment = trim->Segments[index];

			if (blend->timeCurrent >= segment.Start && blend->timeCurrent <= segment.End)
			{
				currentIndex = index;
				break;
			}
		}

		s32 targetIndex = -1;

		for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
		{
			const STrimSegment& segment = trim->Segments[index];

			if (time >= segment.Start && time <= segment.End)
			{
				targetIndex = index;
				break;
			}
		}

		if (targetIndex < 0)
		{
			for (s32 index = 0; index < static_cast<s32>(trim->Segments.size()); ++index)
			{
				const STrimSegment& segment = trim->Segments[index];

				if (forward && segment.Start > time)
				{
					targetIndex = index;
					break;
				}

				if (!forward && segment.End < time)
				{
					targetIndex = index;
				}
			}
		}

		if (targetIndex >= 0)
		{
			const STrimSegment& target = trim->Segments[targetIndex];
			const bool jumped = currentIndex != targetIndex;
			const float targetTime = jumped ? (forward ? target.Start : target.End) : time;

			if (jumped && blendTime > 0.0f)
			{
				StartBlendJump(actor, targetTime, blendTime);
				return;
			}

			blend->timeCurrent = std::clamp(targetTime, target.Start, target.End);
			return;
		}
	}

	if (forward)
	{
		blend->timeCurrent = chain.End;

		if (pingPong)
		{
			actor.PlayDirection = -1.0f;
		}
		else if (actor.PlaylistIndex >= 0)
		{
			AdvanceActorPlaylist(actor);
		}
		else if (actor.Loop)
		{
			if (blendTime > 0.0f)
			{
				StartBlendJump(actor, chain.Start, blendTime);
			}
			else
			{
				blend->timeCurrent = chain.Start;
			}
		}
		else
		{
			CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
		}

		return;
	}

	blend->timeCurrent = chain.Start;

	if (pingPong)
	{
		actor.PlayDirection = 1.0f;
	}
	else if (actor.Loop)
	{
		if (blendTime > 0.0f)
		{
			StartBlendJump(actor, chain.End, blendTime);
		}
		else
		{
			blend->timeCurrent = chain.End;
		}
	}
	else
	{
		CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
	}
}


void CCinemaFilmMakerManager::UpdateActorPlayback(SCinemaActor& actor)
{
	if (actor.PlaylistIndex < 0 || actor.Paused || actor.PlaylistIndex >= static_cast<s32>(actor.Playlist.size()))
	{
		return;
	}

	const SPlaylistEntry& entry = actor.Playlist[actor.PlaylistIndex];

	if (entry.Fx)
	{
		if (Device.fTimeGlobal >= actor.FxDeadline)
		{
			AdvanceActorPlaylist(actor);
		}

		return;
	}

	if (!actor.Motion.valid())
	{
		AdvanceActorPlaylist(actor);
		return;
	}

	CBlend* blend = CinemaFindBlend(actor.Animated, actor.Motion);

	if (blend == nullptr)
	{
		AdvanceActorPlaylist(actor);
		return;
	}

	if (blend->speed < 0.0f)
	{
		if (blend->timeCurrent <= END_EPS)
		{
			AdvanceActorPlaylist(actor);
		}
	}
	else if (blend->timeTotal > 0.0f && blend->timeCurrent >= blend->timeTotal - END_EPS)
	{
		AdvanceActorPlaylist(actor);
	}
}

void CCinemaFilmMakerManager::StartPlaylist(SCinemaActor& actor, s32 start)
{
	if (actor.Playlist.empty())
	{
		return;
	}

	actor.Paused = false;
	actor.PlaylistIndex = -1;
	TryPlayPlaylistFrom(actor, start);
}

void CCinemaFilmMakerManager::StartActorPlayback(SCinemaActor& actor)
{
	if (!actor.Playlist.empty())
	{
		StartPlaylist(actor, actor.PlaylistSelected >= 0 ? actor.PlaylistSelected : 0);
		return;
	}

	if (actor.Motion.valid())
	{
		const SAnimationSource* source = FindSource(actor, actor.MotionSource);

		if (source != nullptr)
		{
			const SAnimationClip* clip = FindClip(*source, actor.MotionName, actor.MotionFx);

			if (clip != nullptr)
			{
				actor.Paused = false;
				PlayClip(actor, *source, *clip, false);
			}
		}
	}
}

void CCinemaFilmMakerManager::StartPlayAll()
{
	for (auto& actor : Actors)
	{
		if (!actor->Playlist.empty())
		{
			StartPlaylist(*actor, 0);
		}
		else
		{
			StartActorPlayback(*actor);
		}
	}
}

void CCinemaFilmMakerManager::StopActor(SCinemaActor& actor)
{
	if (actor.Animated != nullptr && actor.Motion.valid())
	{
		CinemaForEachBlend(actor.Animated, actor.Motion, [](CBlend& blend)
		{
			blend.playing = false;
			blend.timeCurrent = 0.0f;
		});
	}

	actor.PlaylistIndex = -1;
	actor.Paused = false;
	actor.FxDeadline = 0.0f;
	actor.Motion.invalidate();
	actor.MotionName = "";
	actor.MotionSource = "";
	actor.MotionFx = false;
	actor.PlayDirection = 1.0f;

	ApplyDefaultPose(actor);
}

void CCinemaFilmMakerManager::StopAll()
{
	for (auto& actor : Actors)
	{
		StopActor(*actor);
	}
}

void CCinemaFilmMakerManager::TogglePause(SCinemaActor& actor)
{
	if (!actor.Motion.valid() || actor.MotionFx)
	{
		return;
	}

	actor.Paused = !actor.Paused;
	CinemaSetBlendPlaying(actor.Animated, actor.Motion, !actor.Paused);
}

void CCinemaFilmMakerManager::ApplySpeed(SCinemaActor& actor)
{
	if (actor.Animated != nullptr && actor.Motion.valid() && !actor.MotionFx)
	{
		const STrimRange* trim = FindTrim(actor, actor.MotionSource, actor.MotionName);
		const float speed = GetClipSpeed(actor, trim);

		CinemaSetBlendSpeed(actor.Animated, actor.Motion, speed);
		actor.PlayDirection = speed >= 0.0f ? 1.0f : -1.0f;
	}
}

bool CCinemaFilmMakerManager::IsActorPlaying(const SCinemaActor& actor) const
{
	if (actor.PlaylistIndex >= 0 && !actor.Paused)
	{
		return true;
	}

	if (!actor.Motion.valid())
	{
		return false;
	}

	if (actor.MotionFx)
	{
		return Device.fTimeGlobal < actor.FxDeadline;
	}

	CBlend* blend = CinemaFindBlend(actor.Animated, actor.Motion);
	return blend != nullptr && blend->playing;
}

void CCinemaFilmMakerManager::AddClipToPlaylist(SCinemaActor& actor, const shared_str& source, const shared_str& name, bool fx)
{
	const SAnimationSource* animationSource = FindSource(actor, source);

	if (animationSource == nullptr)
	{
		return;
	}

	const SAnimationClip* clip = FindClip(*animationSource, name, fx);

	if (clip == nullptr)
	{
		return;
	}

	actor.Playlist.push_back({ .Source = source, .Name = name, .Fx = fx });
	actor.PlaylistSelected = static_cast<s32>(actor.Playlist.size()) - 1;
}

void CCinemaFilmMakerManager::MovePlaylistEntry(SCinemaActor& actor, s32 from, s32 to)
{
	const s32 count = static_cast<s32>(actor.Playlist.size());

	if (from == to || from < 0 || from >= count || to < 0 || to > count)
	{
		return;
	}

	s32 insertAt = to;

	if (insertAt > from)
	{
		--insertAt;
	}

	const SPlaylistEntry entry = actor.Playlist[from];
	actor.Playlist.erase(actor.Playlist.begin() + from);
	actor.Playlist.insert(actor.Playlist.begin() + insertAt, entry);

	if (actor.PlaylistIndex == from)
	{
		actor.PlaylistIndex = insertAt;
	}
	else
	{
		if (from < actor.PlaylistIndex)
		{
			--actor.PlaylistIndex;
		}

		if (insertAt <= actor.PlaylistIndex)
		{
			++actor.PlaylistIndex;
		}
	}

	if (actor.PlaylistSelected == from)
	{
		actor.PlaylistSelected = insertAt;
	}
	else if (actor.PlaylistSelected > from && actor.PlaylistSelected <= insertAt)
	{
		--actor.PlaylistSelected;
	}
	else if (actor.PlaylistSelected >= insertAt && actor.PlaylistSelected < from)
	{
		++actor.PlaylistSelected;
	}
}

void CCinemaFilmMakerManager::RemovePlaylistEntry(SCinemaActor& actor, s32 index)
{
	if (index < 0 || index >= static_cast<s32>(actor.Playlist.size()))
	{
		return;
	}

	const bool wasPlaying = actor.PlaylistIndex == index;

	actor.Playlist.erase(actor.Playlist.begin() + index);

	if (wasPlaying)
	{
		StopActor(actor);
	}
	else if (actor.PlaylistIndex > index)
	{
		--actor.PlaylistIndex;
	}

	if (actor.PlaylistSelected == index)
	{
		actor.PlaylistSelected = -1;
	}
	else if (actor.PlaylistSelected > index)
	{
		--actor.PlaylistSelected;
	}
}

bool CCinemaFilmMakerManager::AddActor(const Fvector& position, const char* visual, bool adjustGround)
{
	if (visual == nullptr || visual[0] == '\0' || !VisualPathExists(visual))
	{
		return false;
	}

	auto actor = xr_make_unique<SCinemaActor>();
	string64 name = {};
	xr_sprintf(name, "FakeNPC_%02u", NextNameIndex++);
	actor->Name = name;
	actor->Position = position;

	Fvector direction = {};
	direction.sub(Device.vCameraPosition, position);
	const float yaw = direction.square_magnitude() > EPS ? -atan2f(direction.x, direction.z) : 0.0f;
	actor->Orientation.setHPB(yaw, 0.0f, 0.0f);

	if (!SetActorVisual(*actor, visual))
	{
		return false;
	}

	if (adjustGround)
	{
		SnapActorToGround(*actor);
	}

	Actors.push_back(std::move(actor));
	Selected = static_cast<s32>(Actors.size()) - 1;
	ResetSelection();
	return true;
}

bool CCinemaFilmMakerManager::AddActorInFront(const char* visual)
{
	if (g_pGameLevel == nullptr || visual == nullptr || visual[0] == '\0')
	{
		return false;
	}

	Fvector position = Device.vCameraPosition;
	Fvector direction = Device.vCameraDirection;

	if (!SpawnFromCamera && g_actor != nullptr)
	{
		position = g_actor->Position();
		direction = g_actor->Direction();
	}

	direction.normalize_safe();
	position.mad(direction, 3.0f);
	return AddActor(position, visual, SnapToGround);
}

bool CCinemaFilmMakerManager::SetActorVisual(SCinemaActor& actor, const char* visual)
{
	if (visual == nullptr || visual[0] == '\0')
	{
		return false;
	}

	if (actor.Model != nullptr && xr_strcmp(actor.Visual.c_str(), visual) == 0)
	{
		return true;
	}

	if (!VisualPathExists(visual))
	{
		return false;
	}

	IRenderVisual* model = ::Render->model_Create(visual);

	if (model == nullptr)
	{
		return false;
	}

	vis_data& visData = model->getVisData();
	visData.marker = 0;
	visData.accept_frame = 0;
	visData.hom_frame = 0;

	IKinematics* skeleton = model->dcast_PKinematics();
	IKinematicsAnimated* animated = model->dcast_PKinematicsAnimated();

	if (skeleton != nullptr)
	{
		skeleton->CalculateBones(true);
		skeleton->CalculateBBox(true);
	}

	if (actor.Model != nullptr)
	{
		CinemaClearBindPose(actor);
		actor.Object.Unbind();
		::Render->model_Delete(actor.Model);
	}

	actor.Model = model;
	actor.Skeleton = skeleton;
	actor.Animated = animated;
	actor.Visual = visual;
	actor.LocalBox = model->getVisData().box;
	actor.PlaylistIndex = -1;
	actor.Paused = false;

	if (actor.PlaylistSelected >= static_cast<s32>(actor.Playlist.size()))
	{
		actor.PlaylistSelected = static_cast<s32>(actor.Playlist.size()) - 1;
	}
	actor.Motion.invalidate();
	actor.MotionName = "";
	actor.MotionSource = "";
	actor.MotionFx = false;
	actor.FxDeadline = 0.0f;

	BuildActorSources(actor);
	ApplyDefaultPose(actor);
	RefreshActorItemBones(actor);
	SelectedSource = -1;
	SelectedClip = -1;
	return true;
}

bool CCinemaFilmMakerManager::SnapActorToGround(SCinemaActor& actor)
{
	if (actor.Model == nullptr || g_pGameLevel == nullptr)
	{
		return false;
	}

	float bottom = 0.0f;
	float top = 2.0f;

	if (actor.LocalBox.is_valid())
	{
		bottom = actor.LocalBox.min.y * actor.Scale.y;
		top = actor.LocalBox.max.y * actor.Scale.y;
	}

	Fvector traceStart = actor.Position;
	traceStart.y += top + 0.5f;

	collide::rq_result result = {};

	if (!g_pGameLevel->ObjectSpace.RayPick(traceStart, Fvector().set(0.0f, -1.0f, 0.0f), top + 200.0f, collide::rqtStatic, result, nullptr))
	{
		return false;
	}

	Fvector up = {};
	up.set(0.0f, 1.0f, 0.0f);

	if (result.element >= 0)
	{
		xr_vector<CDB::TRI>& triangles = g_pGameLevel->ObjectSpace.GetStaticTris();
		xr_vector<Fvector>& vertices = g_pGameLevel->ObjectSpace.GetStaticVerts();

		if (static_cast<size_t>(result.element) < triangles.size())
		{
			const CDB::TRI& triangle = triangles[result.element];
			const size_t vertexCount = vertices.size();

			if (triangle.verts[0] < vertexCount && triangle.verts[1] < vertexCount && triangle.verts[2] < vertexCount)
			{
				Fvector edge1 = {};
				Fvector edge2 = {};

				edge1.sub(vertices[triangle.verts[1]], vertices[triangle.verts[0]]);
				edge2.sub(vertices[triangle.verts[2]], vertices[triangle.verts[0]]);

				up.crossproduct(edge1, edge2);
				up.normalize_safe();

				if (up.y < 0.0f)
				{
					up.invert();
				}
			}
		}
	}

	if (up.y < kCinemaMinGroundNormalY)
	{
		up.set(0.0f, 1.0f, 0.0f);
	}

	Fvector hit = {};
	hit.set(actor.Position.x, traceStart.y - result.range, actor.Position.z);

	const Fvector forward = actor.Orientation.k;
	const float yaw = -atan2f(forward.x, forward.z);

	Fvector flat = {};
	flat.set(-sinf(yaw), 0.0f, cosf(yaw));

	Fvector slopeDir = {};
	slopeDir.mad(flat, up, -flat.dotproduct(up));

	if (slopeDir.square_magnitude() > EPS)
	{
		slopeDir.normalize();

		Fvector slopeRight = {};
		slopeRight.crossproduct(up, slopeDir);

		actor.Orientation.i.set(slopeRight);
		actor.Orientation.j.set(up);
		actor.Orientation.k.set(slopeDir);
	}

	actor.Position.mad(hit, up, -bottom);
	return true;
}

static u16 CinemaFindBone(IKinematics* skeleton, const char* name)
{
	if (skeleton == nullptr || name == nullptr || name[0] == '\0')
	{
		return u16(-1);
	}

	return skeleton->LL_BoneID(name);
}

void CCinemaFilmMakerManager::RefreshActorItemBones(SCinemaActor& actor)
{
	actor.ItemBoneR = u16(-1);
	actor.ItemBoneL = u16(-1);
	actor.ItemBoneR2 = u16(-1);
	actor.ItemBoneAttach = u16(-1);

	if (actor.Skeleton == nullptr)
	{
		return;
	}

	if (!actor.ItemWeaponStyle)
	{
		actor.ItemBoneAttach = CinemaFindBone(actor.Skeleton, actor.ItemBoneName.c_str());
		return;
	}

	actor.ItemBoneR = CinemaFindBone(actor.Skeleton, "bip01_r_finger1");

	if (actor.ItemBoneR == u16(-1))
	{
		actor.ItemBoneR = CinemaFindBone(actor.Skeleton, "bip01_r_hand");
	}

	actor.ItemBoneL = CinemaFindBone(actor.Skeleton, "bip01_l_finger1");

	if (actor.ItemBoneL == u16(-1))
	{
		actor.ItemBoneL = CinemaFindBone(actor.Skeleton, "bip01_l_hand");
	}

	actor.ItemBoneR2 = CinemaFindBone(actor.Skeleton, "bip01_r_finger11");

	if (actor.ItemBoneR2 == u16(-1))
	{
		actor.ItemBoneR2 = actor.ItemBoneR;
	}
}

void CCinemaFilmMakerManager::SetActorItem(SCinemaActor& actor, const shared_str& section)
{
	ReleaseActorItem(actor);

	if (section.size() == 0 || pSettings == nullptr || ::Render == nullptr)
	{
		return;
	}

	if (!pSettings->section_exist(section.c_str()) || !pSettings->line_exist(section.c_str(), "visual"))
	{
		return;
	}

	const shared_str visual = pSettings->r_string(section.c_str(), "visual");
	IRenderVisual* model = ::Render->model_Create(visual.c_str());

	if (model == nullptr)
	{
		return;
	}

	vis_data& visData = model->getVisData();
	visData.marker = 0;
	visData.accept_frame = 0;
	visData.hom_frame = 0;

	actor.ItemSection = section;
	actor.ItemModel = model;
	actor.ItemWeaponStyle = pSettings->line_exist(section.c_str(), "position") && pSettings->line_exist(section.c_str(), "orientation");

	if (actor.ItemWeaponStyle)
	{
		actor.ItemHandDependence = pSettings->line_exist(section.c_str(), "hand_dependence") ? pSettings->r_s32(section.c_str(), "hand_dependence") : 2;
		actor.ItemPosition = pSettings->r_fvector3(section.c_str(), "position");
		actor.ItemRotation = pSettings->r_fvector3(section.c_str(), "orientation");
	}
	else
	{
		actor.ItemHandDependence = 2;
		actor.ItemBoneName = pSettings->r_string(section.c_str(), "attach_bone_name");
		actor.ItemPosition = pSettings->line_exist(section.c_str(), "attach_position_offset") ? pSettings->r_fvector3(section.c_str(), "attach_position_offset") : Fvector().set(0.0f, 0.0f, 0.0f);
		actor.ItemRotation = pSettings->line_exist(section.c_str(), "attach_angle_offset") ? pSettings->r_fvector3(section.c_str(), "attach_angle_offset") : Fvector().set(0.0f, 0.0f, 0.0f);
	}

	RefreshActorItemBones(actor);
}

void CCinemaFilmMakerManager::DuplicateSelectedActor(bool withPlaylist, bool startPlayback)
{
	const SCinemaActor* source = GetSelectedActor();

	if (source == nullptr)
	{
		return;
	}

	auto actor = xr_make_unique<SCinemaActor>();
	string256 name = {};
	xr_sprintf(name, "%s_copy", source->Name.c_str());
	actor->Name = name;
	actor->Position = source->Position;
	actor->Position.mad(Device.vCameraRight, 1.0f);
	actor->Orientation = source->Orientation;
	actor->Scale = source->Scale;

	if (withPlaylist)
	{
		actor->Loop = source->Loop;
		actor->Speed = source->Speed;
	}

	if (!SetActorVisual(*actor, source->Visual.c_str()))
	{
		return;
	}

	if (source->ItemSection.size() > 0)
	{
		SetActorItem(*actor, source->ItemSection);
	}

	if (withPlaylist)
	{
		actor->Playlist = source->Playlist;
		actor->PlaylistSelected = source->PlaylistSelected;
	}

	SCinemaActor* created = actor.get();
	Actors.push_back(std::move(actor));
	Selected = static_cast<s32>(Actors.size()) - 1;
	ResetSelection();

	if (startPlayback)
	{
		StartPlaylist(*created, 0);
	}
}

void CCinemaFilmMakerManager::DeleteSelectedActor()
{
	if (Selected < 0 || Selected >= static_cast<s32>(Actors.size()))
	{
		return;
	}

	ClearGizmo();
	Actors[Selected]->PlaylistIndex = -1;
	Actors[Selected]->Paused = false;
	ReleaseActorItem(*Actors[Selected]);
	ReleaseActorModel(*Actors[Selected]);
	Actors.erase(Actors.begin() + Selected);

	if (Selected >= static_cast<s32>(Actors.size()))
	{
		Selected = static_cast<s32>(Actors.size()) - 1;
	}

	ResetSelection();
}


static void CinemaSceneFolder(string_path& path, const shared_str& levelName)
{
	string_path relative = {};
	xr_sprintf(relative, "%s%s\\", kCinemaSceneFolder, levelName.c_str());
	FS.update_path(path, "$app_data_root$", relative);
}

static void CinemaPresetsPath(string_path& path)
{
	FS.update_path(path, "$app_data_root$", kCinemaPresetsFile);
}

static void CinemaScenePath(string_path& path, const shared_str& levelName, const char* sceneName)
{
	CinemaSceneFolder(path, levelName);
	xr_strcat(path, sceneName);
	xr_strcat(path, kCinemaSceneExt);
}

static void CinemaSceneCleanName(const char* source, xr_string& result)
{
	result.clear();

	if (source == nullptr)
	{
		return;
	}

	for (const char* symbol = source; *symbol != '\0'; ++symbol)
	{
		const char value = *symbol;

		if (value < 32 || value == '\\' || value == '/' || value == ':' || value == '*' || value == '?' || value == '"' || value == '<' || value == '>' || value == '|')
		{
			continue;
		}

		result.push_back(value);
	}

	while (!result.empty() && (result.back() == ' ' || result.back() == '.'))
	{
		result.pop_back();
	}

	while (!result.empty() && result.front() == ' ')
	{
		result.erase(result.begin());
	}
}

static void SplitTrimValue(const xr_string& text, xr_vector<xr_string>& parts)
{
	parts.clear();
	size_t cursor = 0;

	while (true)
	{
		const size_t found = text.find('|', cursor);

		if (found == xr_string::npos)
		{
			parts.push_back(text.substr(cursor));
			return;
		}

		parts.push_back(text.substr(cursor, found - cursor));
		cursor = found + 1;
	}
}

static bool CinemaParseTrim(const shared_str& value, STrimRange& trim)
{
	if (value.size() == 0)
	{
		return false;
	}

	const xr_string text = value.c_str();
	xr_vector<xr_string> parts;
	SplitTrimValue(text, parts);

	if (parts.empty())
	{
		return false;
	}

	if (parts[0] == "v2")
	{
		if (parts.size() < 4)
		{
			return false;
		}

		trim.Source = parts[1].c_str();
		trim.Name = parts[2].c_str();
		trim.Speed = static_cast<float>(atof(parts[3].c_str()));

		size_t firstSegment = 4;

		if (parts.size() > 4 && parts[4].find(',') == xr_string::npos)
		{
			trim.Blend = static_cast<float>(atof(parts[4].c_str()));
			firstSegment = 5;
		}

		for (size_t index = firstSegment; index < parts.size(); ++index)
		{
			const size_t comma = parts[index].find(',');

			if (comma == xr_string::npos)
			{
				continue;
			}

			STrimSegment segment;
			segment.Start = static_cast<float>(atof(parts[index].substr(0, comma).c_str()));
			segment.End = static_cast<float>(atof(parts[index].c_str() + comma + 1));

			if (segment.Start != segment.Start || segment.End != segment.End || segment.End <= segment.Start)
			{
				continue;
			}

			trim.Segments.push_back(segment);
		}
	}
	else
	{
		if (parts.size() < 4)
		{
			return false;
		}

		trim.Source = parts[0].c_str();
		trim.Name = parts[1].c_str();

		STrimSegment segment;
		segment.Start = static_cast<float>(atof(parts[2].c_str()));
		segment.End = static_cast<float>(atof(parts[3].c_str()));

		if (parts.size() > 4)
		{
			trim.Speed = static_cast<float>(atof(parts[parts.size() - 1].c_str()));
		}

		if (segment.Start == segment.Start && segment.End == segment.End && segment.End > segment.Start)
		{
			trim.Segments.push_back(segment);
		}
	}

	if (trim.Source.size() == 0 || trim.Name.size() == 0 || trim.Segments.empty())
	{
		return false;
	}

	SortSegmentList(trim.Segments);
	trim.Speed = std::clamp(trim.Speed, -4.0f, 4.0f);
	trim.Blend = std::clamp(trim.Blend, 0.0f, 2.0f);

	for (STrimSegment& segment : trim.Segments)
	{
		segment.Start = std::max(0.0f, segment.Start);
	}

	return true;
}

static shared_str CinemaReadString(CInifile& ini, const char* section, const char* line)
{
	if (!ini.line_exist(section, line))
	{
		return shared_str();
	}

	const char* value = ini.r_string(section, line);

	return value != nullptr ? shared_str(value) : shared_str();
}

bool CCinemaFilmMakerManager::RestoreActorPose(SCinemaActor& actor, const SSceneActorData& data)
{
	if (data.PoseClip.size() == 0)
	{
		return false;
	}

	const SAnimationSource* source = FindSource(actor, data.PoseSource);

	if (source == nullptr)
	{
		return false;
	}

	const SAnimationClip* clip = FindClip(*source, data.PoseClip, data.PoseFx);

	if (clip == nullptr)
	{
		return false;
	}

	if (!PlayClip(actor, *source, *clip, false))
	{
		return false;
	}

	CinemaSetBlendLoop(actor.Animated, actor.Motion, !data.PosePaused);

	if (!data.PoseFx && data.PoseTime > 0.0f)
	{
		CinemaSetBlendTime(actor.Animated, actor.Motion, data.PoseTime);
	}

	if (data.PosePaused)
	{
		actor.Paused = true;
		CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
	}

	return true;
}

void CCinemaFilmMakerManager::ShowSceneDialog(ESceneDialog mode, const char* title, const xr_vector<xr_string>& lines)
{
	SceneDialogMode = mode;
	SceneDialogTitle = title != nullptr ? title : "";
	SceneDialogLines = lines;
	SceneDialogRequested = true;
}

void CCinemaFilmMakerManager::DrawSceneDialog()
{
	if (SceneDialogRequested)
	{
		SceneDialogRequested = false;
		ImGui::OpenPopup("Scene##cinema_scene_dialog");
	}

	if (!ImGui::BeginPopupModal("Scene##cinema_scene_dialog", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		return;
	}

	ImGui::TextUnformatted(SceneDialogTitle.c_str());
	ImGui::Separator();

	const auto drawLines = [this]()
	{
		for (const xr_string& line : SceneDialogLines)
		{
			ImGui::Bullet();
			ImGui::TextWrapped("%s", line.c_str());
		}
	};

	switch (SceneDialogMode)
	{
	case ESceneDialog::Save:
	{
		drawLines();

		ImGui::SetNextItemWidth(420.0f);

		if (ImGui::IsWindowAppearing())
		{
			ImGui::SetKeyboardFocusHere();
		}

		ImGui::InputTextWithHint("##cinema_scene_name", "Scene name", SceneNameEdit, sizeof(SceneNameEdit));

		xr_string cleanName;
		CinemaSceneCleanName(SceneNameEdit, cleanName);

		bool exists = false;
		string_path path = {};

		if (!cleanName.empty() && Level != nullptr)
		{
			CinemaScenePath(path, Level->name(), cleanName.c_str());

			std::error_code error;
			exists = std::filesystem::exists(path, error);

			ImGui::TextDisabled("%s", path);
		}

		if (exists)
		{
			ImGui::TextDisabled("This scene already exists and will be overwritten.");
		}

		ImGui::Separator();

		ImGui::BeginDisabled(cleanName.empty());

		if (ImGui::Button("Save", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
			SaveSceneFile(cleanName.c_str());
		}

		ImGui::EndDisabled();

		ImGui::SameLine();

		if (ImGui::Button("Cancel", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
		}

		break;
	}

	case ESceneDialog::Preset:
	{
		drawLines();

		ImGui::SetNextItemWidth(420.0f);

		if (ImGui::IsWindowAppearing())
		{
			ImGui::SetKeyboardFocusHere();
		}

		ImGui::InputTextWithHint("##cinema_preset_name", "Preset name", PresetNameEdit, sizeof(PresetNameEdit));

		xr_string cleanName;
		CinemaSceneCleanName(PresetNameEdit, cleanName);

		bool exists = false;
		string_path path = {};

		if (!cleanName.empty())
		{
			CinemaPresetsPath(path);

			exists = FindPreset(cleanName.c_str()) >= 0;

			ImGui::TextDisabled("%s", path);
		}

		if (exists)
		{
			ImGui::TextDisabled("This preset already exists and will be replaced.");
		}

		ImGui::Separator();

		ImGui::BeginDisabled(cleanName.empty());

		if (ImGui::Button("Save", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
			SavePresetFromActor(cleanName.c_str());
		}

		ImGui::EndDisabled();

		ImGui::SameLine();

		if (ImGui::Button("Cancel", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
		}

		break;
	}

	case ESceneDialog::Load:
	{
		ImGui::TextDisabled("Saved scenes for level \"%s\":", Level != nullptr ? Level->name().c_str() : "");

		if (SceneList.empty())
		{
			ImGui::TextDisabled("Nothing saved yet.");
		}
		else
		{
			ImGui::BeginChild("##cinema_scene_list", ImVec2(420.0f, 260.0f), true);

			for (const xr_string& sceneName : SceneList)
			{
				if (ImGui::Selectable(sceneName.c_str(), SceneListSelected == sceneName))
				{
					SceneListSelected = sceneName;
				}

				if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
				{
					SceneListSelected = sceneName;
					SceneLoadRequested = true;
				}
			}

			ImGui::EndChild();
		}

		ImGui::TextDisabled("Loading replaces the current scene (%u NPC).", static_cast<u32>(Actors.size()));
		ImGui::Separator();

		ImGui::BeginDisabled(SceneListSelected.empty());

		if (ImGui::Button("Load", ImVec2(120.0f, 0.0f)))
		{
			SceneLoadRequested = true;
		}

		ImGui::EndDisabled();

		ImGui::SameLine();

		if (ImGui::Button("Cancel", ImVec2(120.0f, 0.0f)))
		{
			SceneListSelected.clear();
			ImGui::CloseCurrentPopup();
		}

		if (SceneLoadRequested)
		{
			SceneLoadRequested = false;

			const xr_string sceneName = SceneListSelected;
			ImGui::CloseCurrentPopup();
			LoadSceneFile(sceneName.c_str());
		}

		break;
	}

	case ESceneDialog::Confirm:
	{
		drawLines();
		ImGui::Separator();

		if (ImGui::Button("Load", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
			ApplyLoadedScene();
		}

		ImGui::SameLine();

		if (ImGui::Button("Cancel", ImVec2(120.0f, 0.0f)))
		{
			SceneData.clear();
			ImGui::CloseCurrentPopup();
		}

		break;
	}

	case ESceneDialog::Info:
	{
		drawLines();
		ImGui::Separator();

		if (ImGui::Button("OK", ImVec2(120.0f, 0.0f)))
		{
			ImGui::CloseCurrentPopup();
		}

		break;
	}

	case ESceneDialog::None:
	default:
		break;
	}

	ImGui::EndPopup();
}

void CCinemaFilmMakerManager::RequestSaveScene()
{
	if (Level == nullptr)
	{
		return;
	}

	xr_vector<xr_string> lines;

	if (Actors.empty())
	{
		lines.push_back("The scene is empty: there is nothing to save.");
	}

	xr_string defaultName = SceneNameLast;

	if (defaultName.empty())
	{
		defaultName = "scene";

		std::error_code error;

		for (s32 index = 2; index < 1000; ++index)
		{
			string_path path = {};
			CinemaScenePath(path, Level->name(), defaultName.c_str());

			if (!std::filesystem::exists(path, error))
			{
				break;
			}

			string64 candidate = {};
			xr_sprintf(candidate, "scene_%d", index);
			defaultName = candidate;
		}
	}

	xr_strcpy(SceneNameEdit, defaultName.c_str());

	ShowSceneDialog(ESceneDialog::Save, "Save scene", lines);
}

void CCinemaFilmMakerManager::SaveSceneFile(const char* name)
{
	if (Level == nullptr)
	{
		return;
	}

	xr_vector<xr_string> dialog;

	xr_string cleanName;
	CinemaSceneCleanName(name, cleanName);

	if (cleanName.empty())
	{
		dialog.push_back("Enter a scene name.");
		ShowSceneDialog(ESceneDialog::Info, "Save scene", dialog);
		return;
	}

	if (Actors.empty())
	{
		dialog.push_back("The scene is empty: there is nothing to save.");
		ShowSceneDialog(ESceneDialog::Info, "Save scene", dialog);
		return;
	}

	string_path path = {};
	CinemaScenePath(path, Level->name(), cleanName.c_str());

	CInifile ini(nullptr, FALSE, FALSE, FALSE);

	ini.w_s32("scene", "version", kCinemaSceneVersion);
	ini.w_string("scene", "name", cleanName.c_str());
	ini.w_string("scene", "level", Level->name().c_str());
	ini.w_string("scene", "platform", EngineExternal().GetCurrentPlatformName());
	ini.w_u32("scene", "actors", static_cast<u32>(Actors.size()));

	for (u32 i = 0; i < static_cast<u32>(Actors.size()); ++i)
	{
		const SCinemaActor& actor = *Actors[i];

		string32 section = {};
		xr_sprintf(section, "actor_%03u", i);

		Fvector rotation = {};
		actor.Orientation.getHPB(rotation);
		rotation.mul(rad2deg(1.0f));

		ini.w_string(section, "name", actor.Name.c_str());
		ini.w_string(section, "visual", actor.Visual.c_str());
		ini.w_fvector3(section, "position", actor.Position);
		ini.w_fvector3(section, "rotation", rotation);
		ini.w_fvector3(section, "scale", actor.Scale);
		ini.w_bool(section, "loop", actor.Loop);
		ini.w_float(section, "speed", actor.Speed);

		if (actor.ItemSection.size() > 0)
		{
			ini.w_string(section, "item", actor.ItemSection.c_str());
		}

		if (actor.Motion.valid())
		{
			ini.w_string(section, "pose_source", actor.MotionSource.c_str());
			ini.w_string(section, "pose_clip", actor.MotionName.c_str());
			ini.w_bool(section, "pose_fx", actor.MotionFx);
			ini.w_bool(section, "pose_paused", actor.Paused);

			if (actor.Animated != nullptr && !actor.MotionFx)
			{
				if (CBlend* blend = CinemaFindBlend(actor.Animated, actor.Motion))
				{
					ini.w_float(section, "pose_time", blend->timeCurrent);
				}
			}
		}

		ini.w_u32(section, "playlist", static_cast<u32>(actor.Playlist.size()));

		for (u32 j = 0; j < static_cast<u32>(actor.Playlist.size()); ++j)
		{
			const SPlaylistEntry& entry = actor.Playlist[j];

			string32 line = {};
			xr_sprintf(line, "playlist_%03u", j);

			string1024 value = {};
			xr_sprintf(value, "%s|%s|%u", entry.Source.c_str(), entry.Name.c_str(), entry.Fx ? 1u : 0u);
			ini.w_string(section, line, value);
		}

		ini.w_bool(section, "pingpong", actor.PingPong);
		ini.w_u32(section, "trims", static_cast<u32>(actor.Trims.size()));

		for (u32 j = 0; j < static_cast<u32>(actor.Trims.size()); ++j)
		{
			const STrimRange& trim = actor.Trims[j];

			if (trim.Source.size() == 0 || trim.Name.size() == 0)
			{
				continue;
			}

			if (trim.Segments.empty())
			{
				continue;
			}

			string32 line = {};
			xr_sprintf(line, "trim_%03u", j);

			xr_string value = "v2|";
			value += trim.Source.c_str();
			value += "|";
			value += trim.Name.c_str();

			string64 speedText = {};
			xr_sprintf(speedText, "|%.4f|%.4f|", trim.Speed, trim.Blend);
			value += speedText;

			for (size_t segmentIndex = 0; segmentIndex < trim.Segments.size(); ++segmentIndex)
			{
				string64 segmentText = {};
				xr_sprintf(segmentText, segmentIndex > 0 ? "%.4f,%.4f" : "%.4f,%.4f", trim.Segments[segmentIndex].Start, trim.Segments[segmentIndex].End);
				value += segmentText;

				if (segmentIndex + 1 < trim.Segments.size())
				{
					value += "|";
				}
			}

			ini.w_string(section, line, value.c_str());
		}
	}

	if (!ini.save_as(path))
	{
		dialog.push_back("Could not write the scene file:");
		dialog.push_back(path);
		ShowSceneDialog(ESceneDialog::Info, "Save scene", dialog);
		return;
	}

	FS.file_update(path);

	SceneNameLast = cleanName;

	string256 summary = {};
	xr_sprintf(summary, "Saved %u NPC to \"%s\".", static_cast<u32>(Actors.size()), cleanName.c_str());

	dialog.push_back(summary);
	dialog.push_back(path);
	ShowSceneDialog(ESceneDialog::Info, "Save scene", dialog);
}

void CCinemaFilmMakerManager::BuildSceneList()
{
	SceneList.clear();

	if (Level == nullptr)
	{
		return;
	}

	string_path folder = {};
	CinemaSceneFolder(folder, Level->name());

	std::error_code error;
	std::filesystem::directory_iterator iterator(folder, error);

	if (error)
	{
		return;
	}

	const size_t extLength = xr_strlen(kCinemaSceneExt);

	for (const std::filesystem::directory_entry& entry : iterator)
	{
		if (!entry.is_regular_file(error))
		{
			continue;
		}

		xr_string fileName = entry.path().filename().string().c_str();

		if (fileName.size() <= extLength)
		{
			continue;
		}

		if (_stricmp(fileName.c_str() + fileName.size() - extLength, kCinemaSceneExt) != 0)
		{
			continue;
		}

		fileName.erase(fileName.size() - extLength);
		SceneList.push_back(fileName);
	}

	std::sort(SceneList.begin(), SceneList.end(), [](const xr_string& left, const xr_string& right)
	{
		return xr_strcmp(left.c_str(), right.c_str()) < 0;
	});
}

void CCinemaFilmMakerManager::RequestLoadScene()
{
	if (Level == nullptr)
	{
		return;
	}

	BuildSceneList();

	SceneListSelected.clear();
	SceneData.clear();

	ShowSceneDialog(ESceneDialog::Load, "Load scene", {});
}

void CCinemaFilmMakerManager::LoadSceneFile(const char* name)
{
	if (Level == nullptr)
	{
		return;
	}

	xr_vector<xr_string> dialog;
	xr_vector<xr_string> warnings;

	SceneData.clear();

	string_path path = {};
	CinemaScenePath(path, Level->name(), name);

	std::error_code error;

	if (!std::filesystem::exists(path, error))
	{
		dialog.push_back("Scene file not found:");
		dialog.push_back(path);
		ShowSceneDialog(ESceneDialog::Info, "Load scene", dialog);
		return;
	}

	CInifile ini(path, TRUE, TRUE, FALSE);

	if (!ini.section_exist("scene"))
	{
		dialog.push_back("The file is not a Cinema Film Maker scene:");
		dialog.push_back(path);
		ShowSceneDialog(ESceneDialog::Info, "Load scene", dialog);
		return;
	}

	const s32 version = ini.line_exist("scene", "version") ? ini.r_s32("scene", "version") : 0;

	if (version <= 0 || version > kCinemaSceneVersion)
	{
		dialog.push_back("Unsupported scene version: the file was saved by another editor build.");
		dialog.push_back(path);
		ShowSceneDialog(ESceneDialog::Info, "Load scene", dialog);
		return;
	}

	const shared_str fileLevel = CinemaReadString(ini, "scene", "level");
	const shared_str filePlatform = CinemaReadString(ini, "scene", "platform");
	const shared_str currentLevel = Level->name();
	const shared_str currentPlatform = EngineExternal().GetCurrentPlatformName();

	if (fileLevel.size() > 0 && xr_strcmp(fileLevel.c_str(), currentLevel.c_str()) != 0)
	{
		warnings.push_back(xr_string("Scene was saved on level \"") + fileLevel.c_str() + "\", current level is \"" + currentLevel.c_str() + "\": NPC positions may not match this level.");
	}

	if (filePlatform.size() > 0 && xr_strcmp(filePlatform.c_str(), currentPlatform.c_str()) != 0)
	{
		warnings.push_back(xr_string("Scene was saved for platform \"") + filePlatform.c_str() + "\", current platform is \"" + currentPlatform.c_str() + "\": models, animations and items may be missing.");
	}

	for (CInifile::Sect& section : ini.sections())
	{
		if (section.Name.size() == 0 || _strnicmp(section.Name.c_str(), "actor_", 6) != 0)
		{
			continue;
		}

		if (SceneData.size() >= kCinemaSceneMaxActors)
		{
			warnings.push_back("The scene contains too many NPC: the rest was ignored.");
			break;
		}

		const char* sect = section.Name.c_str();

		SSceneActorData data;

		const shared_str actorName = CinemaReadString(ini, sect, "name");

		if (actorName.size() > 0)
		{
			data.Name = actorName.c_str();
		}

		if (data.Name.empty())
		{
			string32 generated = {};
			xr_sprintf(generated, "FakeNPC_%02u", static_cast<u32>(SceneData.size()) + 1);
			data.Name = generated;
		}

		data.Visual = CinemaReadString(ini, sect, "visual");

		if (data.Visual.size() == 0 || !VisualPathExists(data.Visual.c_str()))
		{
			warnings.push_back(xr_string("Model not found for \"") + data.Name + "\": " + (data.Visual.size() > 0 ? data.Visual.c_str() : "<empty>") + " - NPC skipped.");
			continue;
		}

		if (ini.line_exist(sect, "position"))
		{
			data.Position = ini.r_fvector3(sect, "position");
		}

		if (ini.line_exist(sect, "rotation"))
		{
			data.Rotation = ini.r_fvector3(sect, "rotation");
		}

		if (ini.line_exist(sect, "scale"))
		{
			data.Scale = ini.r_fvector3(sect, "scale");
		}

		if (!_valid(data.Position) || !_valid(data.Rotation) || !_valid(data.Scale))
		{
			warnings.push_back(xr_string("Broken coordinates for \"") + data.Name + "\" - NPC skipped.");
			continue;
		}

		data.Scale.x = std::clamp(data.Scale.x, 0.1f, 5.0f);
		data.Scale.y = std::clamp(data.Scale.y, 0.1f, 5.0f);
		data.Scale.z = std::clamp(data.Scale.z, 0.1f, 5.0f);

		data.Loop = ini.line_exist(sect, "loop") && ini.r_bool(sect, "loop");
		data.Speed = ini.line_exist(sect, "speed") ? ini.r_float(sect, "speed") : 1.0f;

		if (data.Speed != data.Speed || data.Speed < -4.0f || data.Speed > 4.0f)
		{
			data.Speed = std::clamp(data.Speed, -4.0f, 4.0f);
		}

		if (ini.line_exist(sect, "item"))
		{
			const shared_str item = CinemaReadString(ini, sect, "item");

			if (item.size() > 0)
			{
				const bool available = pSettings != nullptr && pSettings->section_exist(item.c_str()) && pSettings->line_exist(item.c_str(), "visual");

				if (available)
				{
					data.Item = item;
				}
				else
				{
					warnings.push_back(xr_string("Item \"") + item.c_str() + "\" is not available in this build - removed from \"" + data.Name + "\".");
				}
			}
		}

		if (ini.line_exist(sect, "pose_clip"))
		{
			data.PoseClip = CinemaReadString(ini, sect, "pose_clip");
			data.PoseSource = CinemaReadString(ini, sect, "pose_source");
			data.PoseFx = ini.line_exist(sect, "pose_fx") && ini.r_bool(sect, "pose_fx");
			data.PoseTime = ini.line_exist(sect, "pose_time") ? ini.r_float(sect, "pose_time") : 0.0f;
			data.PosePaused = ini.line_exist(sect, "pose_paused") && ini.r_bool(sect, "pose_paused");
		}

		const u32 playlistCount = ini.line_exist(sect, "playlist") ? ini.r_u32(sect, "playlist") : 0;
		const u32 playlistLimit = std::min(playlistCount, kCinemaSceneMaxPlaylist);

		for (u32 j = 0; j < playlistLimit; ++j)
		{
			string32 line = {};
			xr_sprintf(line, "playlist_%03u", j);

			if (!ini.line_exist(sect, line))
			{
				continue;
			}

			const shared_str value = CinemaReadString(ini, sect, line);

			if (value.size() == 0)
			{
				continue;
			}

			const xr_string text = value.c_str();
			const size_t first = text.find('|');

			if (first == xr_string::npos)
			{
				continue;
			}

			const size_t second = text.find('|', first + 1);

			if (second == xr_string::npos)
			{
				continue;
			}

			SPlaylistEntry entry;
			entry.Source = text.substr(0, first).c_str();
			entry.Name = text.substr(first + 1, second - first - 1).c_str();
			entry.Fx = text.compare(second + 1, xr_string::npos, "1") == 0;
			data.Playlist.push_back(entry);
		}

		data.PingPong = ini.line_exist(sect, "pingpong") && ini.r_bool(sect, "pingpong");

		const u32 trimCount = ini.line_exist(sect, "trims") ? ini.r_u32(sect, "trims") : 0;
		const u32 trimLimit = std::min(trimCount, kCinemaSceneMaxPlaylist);

		for (u32 j = 0; j < trimLimit; ++j)
		{
			string32 line = {};
			xr_sprintf(line, "trim_%03u", j);

			if (!ini.line_exist(sect, line))
			{
				continue;
			}

			const shared_str value = CinemaReadString(ini, sect, line);

			if (value.size() == 0)
			{
				continue;
			}

			const xr_string text = value.c_str();
			const size_t first = text.find('|');

			if (first == xr_string::npos)
			{
				continue;
			}

			const size_t second = text.find('|', first + 1);

			if (second == xr_string::npos)
			{
				continue;
			}

			const size_t third = text.find('|', second + 1);

			if (third == xr_string::npos)
			{
				continue;
			}

			STrimRange trim;

			if (!CinemaParseTrim(value, trim))
			{
				continue;
			}

			data.Trims.push_back(trim);
		}

		SceneData.push_back(std::move(data));
	}

	if (SceneData.empty())
	{
		dialog = warnings;
		dialog.push_back("There is nothing to load.");
		ShowSceneDialog(ESceneDialog::Info, "Load scene", dialog);
		return;
	}

	string256 summary = {};
	xr_sprintf(summary, "Load scene \"%s\"? The current scene (%u NPC) will be replaced by the saved one (%u NPC).", name, static_cast<u32>(Actors.size()), static_cast<u32>(SceneData.size()));

	dialog.push_back(summary);
	dialog.insert(dialog.end(), warnings.begin(), warnings.end());

	ShowSceneDialog(ESceneDialog::Confirm, "Load scene", dialog);
}


void CCinemaFilmMakerManager::ApplyLoadedScene()
{
	if (SceneData.empty())
	{
		return;
	}

	ClearGizmo();
	ClearActors();

	xr_vector<xr_string> issues;
	u32 droppedAnimations = 0;

	for (const SSceneActorData& data : SceneData)
	{
		auto actor = xr_make_unique<SCinemaActor>();
		actor->Name = data.Name;
		actor->Position = data.Position;
		actor->Orientation.setHPB(deg2rad(data.Rotation.x), deg2rad(data.Rotation.y), deg2rad(data.Rotation.z));
		actor->Scale = data.Scale;
		actor->Loop = data.Loop;
		actor->Speed = data.Speed;

		if (!SetActorVisual(*actor, data.Visual.c_str()))
		{
			issues.push_back(xr_string("Skipped \"") + data.Name + "\": the model could not be loaded.");
			continue;
		}

		for (const SPlaylistEntry& entry : data.Playlist)
		{
			const SAnimationSource* source = FindSource(*actor, entry.Source);

			if (source == nullptr || FindClip(*source, entry.Name, entry.Fx) == nullptr)
			{
				++droppedAnimations;
				continue;
			}

			actor->Playlist.push_back(entry);
		}

		actor->PlaylistSelected = actor->Playlist.empty() ? -1 : 0;
		actor->Trims = data.Trims;
		actor->PingPong = data.PingPong;

		if (data.Item.size() > 0)
		{
			SetActorItem(*actor, data.Item);

			if (actor->ItemModel == nullptr)
			{
				issues.push_back(xr_string("Item \"") + data.Item.c_str() + "\" was not applied to \"" + data.Name + "\".");
			}
		}

		RestoreActorPose(*actor, data);

		Actors.push_back(std::move(actor));
	}

	SceneData.clear();

	Selected = Actors.empty() ? -1 : 0;
	ResetSelection();

	if (droppedAnimations > 0)
	{
		string256 line = {};
		xr_sprintf(line, "%u playlist entries were dropped: not available for these models in this build.", droppedAnimations);
		issues.push_back(line);
	}

	string256 summary = {};
	xr_sprintf(summary, "Loaded %u NPC.", static_cast<u32>(Actors.size()));
	issues.insert(issues.begin(), summary);

	ShowSceneDialog(ESceneDialog::Info, "Load scene", issues);
}

s32 CCinemaFilmMakerManager::FindPreset(const char* name) const
{
	if (name == nullptr || name[0] == '\0')
	{
		return -1;
	}

	for (s32 index = 0; index < static_cast<s32>(Presets.size()); ++index)
	{
		if (xr_strcmp(Presets[index].Name.c_str(), name) == 0)
		{
			return index;
		}
	}

	return -1;
}

static void CinemaNormalizeOmfPath(const char* path, xr_string& result)
{
	result.clear();

	if (path == nullptr)
	{
		return;
	}

	xr_string text = path;

	while (!text.empty() && (text.front() == ' ' || text.front() == '\t'))
	{
		text.erase(text.begin());
	}

	while (!text.empty() && (text.back() == ' ' || text.back() == '\t'))
	{
		text.pop_back();
	}

	for (char& symbol : text)
	{
		if (symbol == '/')
		{
			symbol = '\\';
		}
	}

	while (!text.empty() && text.front() == '\\')
	{
		text.erase(text.begin());
	}

	const xr_string_view meshPrefix = "gamedata\\meshes\\";

	if (text.size() > meshPrefix.size() && _strnicmp(text.c_str(), meshPrefix.data(), static_cast<size_t>(meshPrefix.size())) == 0)
	{
		text.erase(0, meshPrefix.size());
	}

	const size_t length = text.size();

	if (length > 6 && _stricmp(text.c_str() + length - 6, "\\*.omf") == 0)
	{
		result = text;
		return;
	}

	if (length > 4 && _stricmp(text.c_str() + length - 4, ".omf") == 0)
	{
		text.erase(length - 4);
	}

	result = text;
}

static bool CinemaExtraPathHasOmf(const char* path)
{
	if (strstr(path, "\\*.omf") != nullptr)
	{
		FS_FileSet files;
		FS.file_list(files, _game_meshes_, FS_ListFiles, path);
		FS.file_list(files, "$level$", FS_ListFiles, path);
		return !files.empty();
	}

	string_path name = {};
	xr_strconcat(name, path, ".omf");

	return FS.exist(_game_meshes_, name) != nullptr || FS.exist("$level$", name) != nullptr;
}

void CCinemaFilmMakerManager::ApplyExtraSources(SCinemaActor& actor)
{
	if (actor.Animated == nullptr || ExtraAnimationPaths.empty())
	{
		return;
	}

	for (const xr_string& path : ExtraAnimationPaths)
	{
		bool applied = false;

		for (const shared_str& done : actor.AppliedExtraPaths)
		{
			if (xr_strcmp(done.c_str(), path.c_str()) == 0)
			{
				applied = true;
				break;
			}
		}

		if (applied)
		{
			continue;
		}

		actor.Animated->append_motion_from_path(actor.Visual.c_str(), path.c_str());
		actor.AppliedExtraPaths.emplace_back(path.c_str());
	}
}

void CCinemaFilmMakerManager::AddExtraAnimationPath(const char* path)
{
	xr_string clean;
	CinemaNormalizeOmfPath(path, clean);

	if (clean.empty())
	{
		return;
	}

	for (const xr_string& existing : ExtraAnimationPaths)
	{
		if (_stricmp(existing.c_str(), clean.c_str()) == 0)
		{
			SCinemaActor* actor = GetSelectedActor();

			if (actor != nullptr)
			{
				BuildActorSources(*actor);
			}

			return;
		}
	}

	ExtraAnimationPaths.push_back(clean);
	SaveSettings();

	for (auto& actor : Actors)
	{
		BuildActorSources(*actor);
	}

	SelectedSource = -1;
	SelectedClip = -1;
}

void CCinemaFilmMakerManager::RemoveExtraAnimationPath(s32 index)
{
	if (index < 0 || index >= static_cast<s32>(ExtraAnimationPaths.size()))
	{
		return;
	}

	ExtraAnimationPaths.erase(ExtraAnimationPaths.begin() + index);
	SaveSettings();
}

void CCinemaFilmMakerManager::DrawExtraSources()
{
	if (!ImGui::CollapsingHeader("Extra animation sources"))
	{
		return;
	}

	ImGui::TextDisabled("Path to .omf files relative to meshes or the level folder: animations\\mod\\*.omf or animations\\mod\\walk");

	const float addWidth = CinemaButtonWidth("Add");
	ImGui::SetNextItemWidth(std::max(120.0f, ImGui::GetContentRegionAvail().x - addWidth - ImGui::GetStyle().ItemSpacing.x));
	ImGui::InputTextWithHint("##cinema_extra_path", "animations\\mod\\*.omf", ExtraPathEdit, sizeof(ExtraPathEdit));

	ImGui::SameLine();

	if (ImGui::Button("Add##extra_path"))
	{
		xr_string clean;
		CinemaNormalizeOmfPath(ExtraPathEdit, clean);

		if (clean.empty())
		{
			ExtraPathStatus = "Enter a path first";
		}
		else if (!CinemaExtraPathHasOmf(clean.c_str()))
		{
			ExtraPathStatus = xr_string("No .omf files found: ") + clean;
		}
		else
		{
			AddExtraAnimationPath(clean.c_str());
			ExtraPathStatus.clear();
			ExtraPathEdit[0] = '\0';
		}
	}

	if (!ExtraPathStatus.empty())
	{
		ImGui::TextDisabled("%s", ExtraPathStatus.c_str());
	}

	if (ExtraAnimationPaths.empty())
	{
		ImGui::TextDisabled("No extra sources added");
		return;
	}

	for (s32 index = 0; index < static_cast<s32>(ExtraAnimationPaths.size()); ++index)
	{
		ImGui::PushID(index);

		ImGui::TextUnformatted(ExtraAnimationPaths[index].c_str());
		ImGui::SameLine();

		if (ImGui::SmallButton("Remove##extra_path"))
		{
			RemoveExtraAnimationPath(index);
			ImGui::PopID();
			break;
		}

		ImGui::PopID();
	}

	ImGui::TextDisabled("Animations already loaded stay on NPC that were created before removal");
}

void CCinemaFilmMakerManager::LoadPresets()
{
	if (PresetsLoaded)
	{
		return;
	}

	PresetsLoaded = true;
	Presets.clear();

	string_path path = {};
	CinemaPresetsPath(path);

	std::error_code error;

	if (!std::filesystem::exists(path, error))
	{
		return;
	}

	CInifile ini(path, TRUE, TRUE, FALSE);

	for (CInifile::Sect& section : ini.sections())
	{
		if (section.Name.size() == 0 || _strnicmp(section.Name.c_str(), "preset_", 7) != 0)
		{
			continue;
		}

		if (Presets.size() >= kCinemaMaxPresets)
		{
			break;
		}

		const char* sect = section.Name.c_str();
		const shared_str presetName = CinemaReadString(ini, sect, "name");
		const shared_str visual = CinemaReadString(ini, sect, "visual");

		if (presetName.size() == 0 || visual.size() == 0)
		{
			continue;
		}

		SNpcPreset preset;
		preset.Name = presetName.c_str();
		preset.Visual = visual;
		preset.Item = CinemaReadString(ini, sect, "item");

		if (ini.line_exist(sect, "scale"))
		{
			preset.Scale = ini.r_fvector3(sect, "scale");
		}

		preset.Scale.x = std::clamp(preset.Scale.x, 0.1f, 5.0f);
		preset.Scale.y = std::clamp(preset.Scale.y, 0.1f, 5.0f);
		preset.Scale.z = std::clamp(preset.Scale.z, 0.1f, 5.0f);
		preset.Loop = ini.line_exist(sect, "loop") && ini.r_bool(sect, "loop");
		preset.Speed = ini.line_exist(sect, "speed") ? ini.r_float(sect, "speed") : 1.0f;

		if (preset.Speed != preset.Speed || preset.Speed < -4.0f || preset.Speed > 4.0f)
		{
			preset.Speed = std::clamp(preset.Speed, -4.0f, 4.0f);
		}

		const u32 playlistCount = ini.line_exist(sect, "playlist") ? ini.r_u32(sect, "playlist") : 0;
		const u32 playlistLimit = std::min(playlistCount, kCinemaSceneMaxPlaylist);

		for (u32 index = 0; index < playlistLimit; ++index)
		{
			string32 line = {};
			xr_sprintf(line, "playlist_%03u", index);

			if (!ini.line_exist(sect, line))
			{
				continue;
			}

			const shared_str value = CinemaReadString(ini, sect, line);

			if (value.size() == 0)
			{
				continue;
			}

			const xr_string text = value.c_str();
			const size_t first = text.find('|');

			if (first == xr_string::npos)
			{
				continue;
			}

			const size_t second = text.find('|', first + 1);

			if (second == xr_string::npos)
			{
				continue;
			}

			SPlaylistEntry entry;
			entry.Source = text.substr(0, first).c_str();
			entry.Name = text.substr(first + 1, second - first - 1).c_str();
			entry.Fx = text.compare(second + 1, xr_string::npos, "1") == 0;
			preset.Playlist.push_back(entry);
		}

		preset.PingPong = ini.line_exist(sect, "pingpong") && ini.r_bool(sect, "pingpong");

		const u32 trimCount = ini.line_exist(sect, "trims") ? ini.r_u32(sect, "trims") : 0;
		const u32 trimLimit = std::min(trimCount, kCinemaSceneMaxPlaylist);

		for (u32 trimIndex = 0; trimIndex < trimLimit; ++trimIndex)
		{
			string32 line = {};
			xr_sprintf(line, "trim_%03u", trimIndex);

			if (!ini.line_exist(sect, line))
			{
				continue;
			}

			const shared_str value = CinemaReadString(ini, sect, line);

			if (value.size() == 0)
			{
				continue;
			}

			const xr_string text = value.c_str();
			const size_t first = text.find('|');

			if (first == xr_string::npos)
			{
				continue;
			}

			const size_t second = text.find('|', first + 1);

			if (second == xr_string::npos)
			{
				continue;
			}

			const size_t third = text.find('|', second + 1);

			if (third == xr_string::npos)
			{
				continue;
			}

			STrimRange trim;

			if (!CinemaParseTrim(value, trim))
			{
				continue;
			}

			preset.Trims.push_back(trim);
		}

		Presets.push_back(std::move(preset));
	}

	if (PresetSelected >= static_cast<s32>(Presets.size()))
	{
		PresetSelected = static_cast<s32>(Presets.size()) - 1;
	}
}

void CCinemaFilmMakerManager::SavePresets()
{
	string_path path = {};
	CinemaPresetsPath(path);

	CInifile ini(nullptr, FALSE, FALSE, FALSE);

	ini.w_s32("preset", "version", kCinemaPresetVersion);

	for (u32 index = 0; index < static_cast<u32>(Presets.size()); ++index)
	{
		const SNpcPreset& preset = Presets[index];

		string32 section = {};
		xr_sprintf(section, "preset_%03u", index);

		ini.w_string(section, "name", preset.Name.c_str());
		ini.w_string(section, "visual", preset.Visual.c_str());

		if (preset.Item.size() > 0)
		{
			ini.w_string(section, "item", preset.Item.c_str());
		}

		ini.w_fvector3(section, "scale", preset.Scale);
		ini.w_bool(section, "loop", preset.Loop);
		ini.w_float(section, "speed", preset.Speed);
		ini.w_u32(section, "playlist", static_cast<u32>(preset.Playlist.size()));

		for (u32 entryIndex = 0; entryIndex < static_cast<u32>(preset.Playlist.size()); ++entryIndex)
		{
			const SPlaylistEntry& entry = preset.Playlist[entryIndex];

			string32 line = {};
			xr_sprintf(line, "playlist_%03u", entryIndex);

			string1024 value = {};
			xr_sprintf(value, "%s|%s|%u", entry.Source.c_str(), entry.Name.c_str(), entry.Fx ? 1u : 0u);
			ini.w_string(section, line, value);
		}

		ini.w_bool(section, "pingpong", preset.PingPong);
		ini.w_u32(section, "trims", static_cast<u32>(preset.Trims.size()));

		for (u32 trimIndex = 0; trimIndex < static_cast<u32>(preset.Trims.size()); ++trimIndex)
		{
			const STrimRange& trim = preset.Trims[trimIndex];

			if (trim.Source.size() == 0 || trim.Name.size() == 0)
			{
				continue;
			}

			if (trim.Segments.empty())
			{
				continue;
			}

			string32 line = {};
			xr_sprintf(line, "trim_%03u", trimIndex);

			xr_string value = "v2|";
			value += trim.Source.c_str();
			value += "|";
			value += trim.Name.c_str();

			string64 speedText = {};
			xr_sprintf(speedText, "|%.4f|%.4f|", trim.Speed, trim.Blend);
			value += speedText;

			for (size_t segmentIndex = 0; segmentIndex < trim.Segments.size(); ++segmentIndex)
			{
				string64 segmentText = {};
				xr_sprintf(segmentText, "%.4f,%.4f", trim.Segments[segmentIndex].Start, trim.Segments[segmentIndex].End);
				value += segmentText;

				if (segmentIndex + 1 < trim.Segments.size())
				{
					value += "|";
				}
			}

			ini.w_string(section, line, value.c_str());
		}
	}

	if (ini.save_as(path))
	{
		FS.file_update(path);
	}
}

void CCinemaFilmMakerManager::RequestSavePreset()
{
	xr_vector<xr_string> lines;

	const SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		lines.push_back("Select an NPC first: a preset is taken from the configured NPC.");
		ShowSceneDialog(ESceneDialog::Info, "Save preset", lines);
		return;
	}

	LoadPresets();

	xr_string defaultName = CinemaFileName(actor->Visual.c_str());
	const size_t dot = defaultName.rfind('.');

	if (dot != xr_string::npos)
	{
		defaultName.erase(dot);
	}

	if (defaultName.empty())
	{
		defaultName = "preset";
	}

	xr_strcpy(PresetNameEdit, defaultName.c_str());

	ShowSceneDialog(ESceneDialog::Preset, "Save preset", lines);
}

void CCinemaFilmMakerManager::SavePresetFromActor(const char* name)
{
	xr_vector<xr_string> lines;

	const SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		lines.push_back("Select an NPC first: a preset is taken from the configured NPC.");
		ShowSceneDialog(ESceneDialog::Info, "Save preset", lines);
		return;
	}

	xr_string cleanName;
	CinemaSceneCleanName(name, cleanName);

	if (cleanName.empty())
	{
		lines.push_back("Enter a preset name.");
		ShowSceneDialog(ESceneDialog::Info, "Save preset", lines);
		return;
	}

	const s32 existing = FindPreset(cleanName.c_str());

	if (existing < 0 && Presets.size() >= kCinemaMaxPresets)
	{
		lines.push_back("There are too many presets: delete one first.");
		ShowSceneDialog(ESceneDialog::Info, "Save preset", lines);
		return;
	}

	SNpcPreset preset;
	preset.Name = cleanName;
	preset.Visual = actor->Visual;
	preset.Item = actor->ItemSection;
	preset.Scale = actor->Scale;
	preset.Loop = actor->Loop;
	preset.Speed = actor->Speed;
	preset.Playlist = actor->Playlist;
	preset.Trims = actor->Trims;
	preset.PingPong = actor->PingPong;

	if (existing >= 0)
	{
		Presets[existing] = preset;
		PresetSelected = existing;
	}
	else
	{
		Presets.push_back(preset);
		PresetSelected = static_cast<s32>(Presets.size()) - 1;
	}

	SavePresets();

	string256 summary = {};
	xr_sprintf(summary, "Preset \"%s\" saved (%u animations).", cleanName.c_str(), static_cast<u32>(preset.Playlist.size()));
	lines.push_back(summary);
	ShowSceneDialog(ESceneDialog::Info, "Save preset", lines);
}

void CCinemaFilmMakerManager::SpawnFromPreset(s32 index)
{
	if (index < 0 || index >= static_cast<s32>(Presets.size()))
	{
		return;
	}

	const SNpcPreset& preset = Presets[index];
	xr_vector<xr_string> lines;

	if (!AddActorInFront(preset.Visual.c_str()))
	{
		lines.push_back(xr_string("Could not create an NPC from preset \"") + preset.Name + "\": model is missing.");
		ShowSceneDialog(ESceneDialog::Info, "Presets", lines);
		return;
	}

	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		return;
	}

	actor->Scale = preset.Scale;
	actor->Loop = preset.Loop;
	actor->Speed = preset.Speed;
	actor->Trims = preset.Trims;
	actor->PingPong = preset.PingPong;

	if (preset.Item.size() > 0)
	{
		SetActorItem(*actor, preset.Item);
	}

	for (const SPlaylistEntry& entry : preset.Playlist)
	{
		const SAnimationSource* source = FindSource(*actor, entry.Source);

		if (source == nullptr || FindClip(*source, entry.Name, entry.Fx) == nullptr)
		{
			continue;
		}

		actor->Playlist.push_back(entry);
	}

	actor->PlaylistSelected = actor->Playlist.empty() ? -1 : 0;
}

void CCinemaFilmMakerManager::ApplyPresetToActor(s32 index)
{
	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr || index < 0 || index >= static_cast<s32>(Presets.size()))
	{
		return;
	}

	const SNpcPreset& preset = Presets[index];

	StopActor(*actor);

	if (!SetActorVisual(*actor, preset.Visual.c_str()))
	{
		xr_vector<xr_string> lines;
		lines.push_back(xr_string("Could not apply preset \"") + preset.Name + "\": model is missing.");
		ShowSceneDialog(ESceneDialog::Info, "Presets", lines);
		return;
	}

	actor->Scale = preset.Scale;
	actor->Loop = preset.Loop;
	actor->Speed = preset.Speed;
	actor->Trims = preset.Trims;
	actor->PingPong = preset.PingPong;
	actor->Playlist.clear();

	for (const SPlaylistEntry& entry : preset.Playlist)
	{
		const SAnimationSource* source = FindSource(*actor, entry.Source);

		if (source == nullptr || FindClip(*source, entry.Name, entry.Fx) == nullptr)
		{
			continue;
		}

		actor->Playlist.push_back(entry);
	}

	actor->PlaylistSelected = actor->Playlist.empty() ? -1 : 0;

	ReleaseActorItem(*actor);

	if (preset.Item.size() > 0)
	{
		SetActorItem(*actor, preset.Item);
	}
}

void CCinemaFilmMakerManager::DeletePreset(s32 index)
{
	if (index < 0 || index >= static_cast<s32>(Presets.size()))
	{
		return;
	}

	Presets.erase(Presets.begin() + index);

	if (PresetSelected >= static_cast<s32>(Presets.size()))
	{
		PresetSelected = static_cast<s32>(Presets.size()) - 1;
	}

	SavePresets();
}

void CCinemaFilmMakerManager::DrawPresetTab()
{
	LoadPresets();

	SCinemaActor* actor = GetSelectedActor();

	ImGui::TextDisabled("A preset stores the model, held item, loop and speed, and the animation playlist.");

	ImGui::BeginDisabled(actor == nullptr);

	if (ImGui::Button("Save selected NPC as preset"))
	{
		RequestSavePreset();
	}

	ImGui::EndDisabled();

	ImGui::Separator();

	if (Presets.empty())
	{
		ImGui::TextDisabled("No presets yet.");
		return;
	}

	const float listHeight = std::clamp(ImGui::GetContentRegionAvail().y - 90.0f, 120.0f, 420.0f);

	ImGui::BeginChild("##cinema_preset_list", ImVec2(0.0f, listHeight), true);

	s32 spawnRequest = -1;

	for (s32 index = 0; index < static_cast<s32>(Presets.size()); ++index)
	{
		const SNpcPreset& preset = Presets[index];

		ImGui::PushID(index);

		string256 label = {};
		xr_sprintf(label, "%s   (%u animations)", preset.Name.c_str(), static_cast<u32>(preset.Playlist.size()));

		if (ImGui::Selectable(label, PresetSelected == index))
		{
			PresetSelected = index;
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("%s\nitem: %s", preset.Visual.c_str(), preset.Item.size() > 0 ? preset.Item.c_str() : "<none>");

			if (ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
			{
				PresetSelected = index;
				spawnRequest = index;
			}
		}

		ImGui::PopID();
	}

	ImGui::EndChild();

	if (spawnRequest >= 0)
	{
		SpawnFromPreset(spawnRequest);
	}

	SToolbarLayout actions(ImGui::GetContentRegionAvail().x);

	ImGui::BeginDisabled(PresetSelected < 0);

	actions.Item(CinemaButtonWidth("Spawn") + 10.0f);

	if (ImGui::Button("Spawn##preset"))
	{
		SpawnFromPreset(PresetSelected);
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Create a new NPC in front of you from this preset");
	}

	ImGui::BeginDisabled(actor == nullptr);

	actions.Item(CinemaButtonWidth("Apply to selected") + 10.0f);

	if (ImGui::Button("Apply to selected##preset"))
	{
		ApplyPresetToActor(PresetSelected);
	}

	ImGui::EndDisabled();

	actions.Item(CinemaButtonWidth("Delete") + 10.0f);

	if (ImGui::Button("Delete##preset"))
	{
		DeletePreset(PresetSelected);
	}

	ImGui::EndDisabled();
}

void CCinemaFilmMakerManager::DrawToolbar()
{
	const bool hasModel = SelectedVisual.size() > 0 && VisualPathExists(SelectedVisual.c_str());
	const float available = ImGui::GetContentRegionAvail().x;
	SToolbarLayout layout(available);

	SCinemaActor* selectedActor = GetSelectedActor();
	const float comboWidth = std::clamp(available * 0.30f, 150.0f, 260.0f);

	string256 actorLabel = {};
	xr_sprintf(actorLabel, "%s%s", selectedActor != nullptr ? selectedActor->Name.c_str() : "No actor", selectedActor != nullptr && IsActorPlaying(*selectedActor) ? "  [playing]" : "");

	layout.Item(comboWidth);
	ImGui::SetNextItemWidth(comboWidth);

	if (ImGui::BeginCombo("##cinema_actor_combo", actorLabel))
	{
		if (Actors.empty())
		{
			ImGui::TextDisabled("No actors");
		}

		for (s32 i = 0; i < static_cast<s32>(Actors.size()); ++i)
		{
			const SCinemaActor& actor = *Actors[i];

			ImGui::PushID(i);

			string256 label = {};
			xr_sprintf(label, "%s%s", actor.Name.c_str(), IsActorPlaying(actor) ? "  [playing]" : "");

			if (ImGui::Selectable(label, Selected == i))
			{
				SelectActor(i);
			}

			ImGui::PopID();
		}

		ImGui::EndCombo();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Selected actor");
	}

	layout.Item(CinemaButtonWidth("Add"));
	ImGui::BeginDisabled(!hasModel);

	if (ImGui::Button("Add##toolbar"))
	{
		AddActorInFront(SelectedVisual.c_str());
	}

	ImGui::EndDisabled();

	if (ImGui::IsItemHovered() && SelectedVisual.size() > 0)
	{
		ImGui::SetTooltip("Add the selected model\n%s", SelectedVisual.c_str());
	}

	layout.Item(CinemaButtonWidth("Camera spawn") + 26.0f);

	if (ImGui::Checkbox("Camera spawn##spawn", &SpawnFromCamera))
	{
		SaveSettings();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("New NPC appears where the camera looks instead of in front of the player");
	}

	layout.Item(CinemaButtonWidth("Snap to ground") + 26.0f);

	if (ImGui::Checkbox("Snap to ground##spawn", &SnapToGround))
	{
		SaveSettings();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Place the NPC on the ground on spawn, tilt it to the slope and keep it on surfaces when dragging the X and Z axes");
	}

	ImGui::BeginDisabled(selectedActor == nullptr);

	layout.Item(CinemaButtonWidth("Duplicate"));
	if (ImGui::Button("Duplicate##toolbar"))
	{
		ImGui::OpenPopup("##cinema_duplicate_popup");
	}

	layout.Item(CinemaButtonWidth("Delete"));
	if (ImGui::Button("Delete##toolbar"))
	{
		DeleteSelectedActor();
	}

	ImGui::EndDisabled();

	if (ImGui::BeginPopup("##cinema_duplicate_popup"))
	{
		if (ImGui::MenuItem("Model only"))
		{
			DuplicateSelectedActor(false, false);
		}

		if (ImGui::MenuItem("Model + playlist"))
		{
			DuplicateSelectedActor(true, false);
		}

		if (ImGui::MenuItem("Model + playlist + play"))
		{
			DuplicateSelectedActor(true, true);
		}

		ImGui::EndPopup();
	}

	ImGui::BeginDisabled(Actors.empty());

	layout.Item(CinemaButtonWidth("Play All"));
	if (ImGui::Button("Play All##toolbar"))
	{
		StartPlayAll();
	}

	layout.Item(CinemaButtonWidth("Stop All"));
	if (ImGui::Button("Stop All##toolbar"))
	{
		StopAll();
	}

	ImGui::EndDisabled();

	layout.Item(CinemaButtonWidth("More"));
	if (ImGui::Button("More##toolbar"))
	{
		ImGui::OpenPopup("##cinema_more_popup");
	}

	if (ImGui::BeginPopup("##cinema_more_popup"))
	{
		if (ImGui::MenuItem("Reload library"))
		{
			BuildLibrary();
		}

		ImGui::Separator();

		if (ImGui::MenuItem("Save scene"))
		{
			RequestSaveScene();
		}

		if (ImGui::MenuItem("Load scene"))
		{
			RequestLoadScene();
		}

		ImGui::Separator();

		if (ImGui::BeginMenu("Theme"))
		{
			for (s32 index = 0; index < kCinemaThemeCount; ++index)
			{
				if (ImGui::MenuItem(kCinemaThemes[index].Name, nullptr, ThemeIndex == index))
				{
					ThemeIndex = index;
					SaveSettings();
				}
			}

			ImGui::EndMenu();
		}

		ImGui::Separator();

		if (Level != nullptr)
		{
			string_path sceneFolder = {};
			CinemaSceneFolder(sceneFolder, Level->name());
			ImGui::TextDisabled("%s", sceneFolder);
		}

		ImGui::EndPopup();
	}

	layout.Item(CinemaButtonWidth("Translate") + 20.0f);
	if (ImGui::RadioButton("Translate##gizmo_mode", GizmoMode == EGizmoMode::Translate))
	{
		GizmoMode = EGizmoMode::Translate;
	}

	layout.Item(CinemaButtonWidth("Rotate") + 20.0f);
	if (ImGui::RadioButton("Rotate##gizmo_mode", GizmoMode == EGizmoMode::Rotate))
	{
		GizmoMode = EGizmoMode::Rotate;
	}

	layout.Item(CinemaButtonWidth("Snap") + 20.0f);
	ImGui::Checkbox("Snap##gizmo", &Snap);

	if (Snap)
	{
		layout.Item(110.0f);
		ImGui::SetNextItemWidth(100.0f);
		ImGui::DragFloat("Move##snap_step", &TranslateStep, 0.01f, 0.01f, 10.0f, "%.2f");

		layout.Item(140.0f);
		ImGui::SetNextItemWidth(100.0f);
		ImGui::DragFloat("Rotate step##snap_step", &RotateStepDeg, 0.5f, 0.5f, 180.0f, "%.1f");
	}
}

void CCinemaFilmMakerManager::DrawSelectedActor()
{
	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		ImGui::TextDisabled("No actor selected");
		ImGui::TextDisabled("Choose a model in the Model tab and press Add.");
		return;
	}

	if (NameEditActor != Selected)
	{
		xr_strcpy(NameEdit, actor->Name.c_str());
		NameEditActor = Selected;
	}

	ImGui::SetNextItemWidth(-1.0f);

	if (ImGui::InputTextWithHint("##cinema_name", "Actor name", NameEdit, sizeof(NameEdit)))
	{
		actor->Name = NameEdit;
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Actor name");
	}

	ImGui::TextDisabled("Model");
	ImGui::SameLine();
	ImGui::TextUnformatted(CinemaFileName(actor->Visual.c_str()));

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("%s", actor->Visual.c_str());
	}

	const float available = ImGui::GetContentRegionAvail().x;
	SToolbarLayout controls(available);

	controls.Item(CinemaButtonWidth("Play"));
	if (ImGui::Button("Play##actor"))
	{
		StartActorPlayback(*actor);
	}

	controls.Item(CinemaButtonWidth(actor->Paused ? "Resume" : "Pause"));
	if (ImGui::Button(actor->Paused ? "Resume##actor" : "Pause##actor"))
	{
		TogglePause(*actor);
	}

	controls.Item(CinemaButtonWidth("Stop"));
	if (ImGui::Button("Stop##actor"))
	{
		StopActor(*actor);
	}

	bool loop = actor->Loop;

	controls.Item(CinemaButtonWidth("Loop") + 20.0f);
	if (ImGui::Checkbox("Loop##actor", &loop))
	{
		actor->Loop = loop;

		if (actor->Animated != nullptr && actor->Motion.valid() && !actor->MotionFx && actor->PlaylistIndex < 0)
		{
			CinemaSetBlendLoop(actor->Animated, actor->Motion, actor->Loop);
		}
	}

	bool pingPong = actor->PingPong;

	controls.Item(CinemaButtonWidth("Ping-pong") + 22.0f);

	if (ImGui::Checkbox("Ping-pong##actor", &pingPong))
	{
		actor->PingPong = pingPong;
		actor->PlayDirection = actor->Speed >= 0.0f ? 1.0f : -1.0f;
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Play the clip (or its trim range) back and forth");
	}

	float speed = actor->Speed;

	controls.Item(170.0f);
	ImGui::SetNextItemWidth(110.0f);

	if (ImGui::DragFloat("Speed##actor", &speed, 0.01f, -4.0f, 4.0f, "%.2f"))
	{
		actor->Speed = speed;
		ApplySpeed(*actor);
	}

	if (actor->PlaylistIndex >= 0)
	{
		ImGui::TextDisabled("Playlist: %d / %u", actor->PlaylistIndex + 1, static_cast<u32>(actor->Playlist.size()));
	}
	else if (actor->Motion.valid())
	{
		ImGui::TextDisabled("Clip: %s%s", actor->MotionName.c_str(), actor->MotionFx ? " [fx]" : "");
	}
	else
	{
		ImGui::TextDisabled("Not playing");
	}

	if (ImGui::TreeNodeEx("Timeline", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_DrawLinesToNodes | ImGuiTreeNodeFlags_SpanAvailWidth))
	{
		DrawClipTimelines(*actor);
		ImGui::TreePop();
	}

	if (ImGui::CollapsingHeader("Transform", ImGuiTreeNodeFlags_DefaultOpen))
	{
		Fvector position = actor->Position;

		if (ImGui::DragFloat3("Position", &position.x, 0.01f, -100000.0f, 100000.0f, "%.2f"))
		{
			actor->Position = position;
		}

		Fvector hpb = {};
		actor->Orientation.getHPB(hpb);
		Fvector degrees = hpb;
		degrees.mul(rad2deg(1.0f));

		if (ImGui::DragFloat3("Rotation", &degrees.x, 0.5f, -3600.0f, 3600.0f, "%.1f"))
		{
			actor->Orientation.setHPB(deg2rad(degrees.x), deg2rad(degrees.y), deg2rad(degrees.z));
		}

		ImGui::TextDisabled("H / P / B, degrees");

		if (ImGui::Button("Reset tilt##actor"))
		{
			const Fvector forward = actor->Orientation.k;
			const float heading = -atan2f(forward.x, forward.z);
			actor->Orientation.setHPB(heading, 0.0f, 0.0f);
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Straighten the NPC: keep the heading and drop the slope tilt"); 
		}

		Fvector scale = actor->Scale;

		if (ImGui::DragFloat3("Scale", &scale.x, 0.01f, 0.1f, 5.0f, "%.2f"))
		{
			actor->Scale = scale;
		}

		ImGui::SameLine();

		if (ImGui::SmallButton("Reset##scale"))
		{
			actor->Scale.set(1.0f, 1.0f, 1.0f);
		}

		ImGui::TextDisabled("XYZ scale of the model");
	}
}

void CCinemaFilmMakerManager::DrawClipTimelines(SCinemaActor& actor)
{
	if (actor.Animated == nullptr)
	{
		ImGui::TextDisabled("The selected model has no animations");
		return;
	}

	if (ImGui::Checkbox("Open timelines by default##cinema", &TimelinesOpenByDefault))
	{
		TimelinesOpenDirty = true;
		SaveSettings();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Whether every animation timeline starts expanded");
	}

	struct SClipRef
	{
		const SAnimationSource* Source = nullptr;
		const SAnimationClip* Clip = nullptr;
	};

	xr_vector<SClipRef> clips;
	xr_set<shared_str> seen;

	const auto addClip = [&clips, &seen](const SAnimationSource& source, const SAnimationClip& clip)
	{
		string512 key = {};
		xr_sprintf(key, "%s|%s|%u", source.Id.c_str(), clip.Name.c_str(), clip.Fx ? 1u : 0u);
		shared_str id = key;

		if (seen.contains(id))
		{
			return;
		}

		seen.insert(id);
		clips.push_back({ &source, &clip });
	};

	for (const SPlaylistEntry& entry : actor.Playlist)
	{
		const SAnimationSource* source = FindSource(actor, entry.Source);

		if (source == nullptr)
		{
			continue;
		}

		const SAnimationClip* clip = FindClip(*source, entry.Name, entry.Fx);

		if (clip != nullptr && !clip->Fx)
		{
			addClip(*source, *clip);
		}
	}

	if (actor.Motion.valid() && !actor.MotionFx)
	{
		if (const SAnimationSource* source = FindSource(actor, actor.MotionSource))
		{
			if (const SAnimationClip* clip = FindClip(*source, actor.MotionName, false))
			{
				addClip(*source, *clip);
			}
		}
	}

	if (clips.empty())
	{
		ImGui::TextDisabled("Add animations to the playlist to get a timeline for each animation");
		return;
	}

	ImGuiTreeNodeFlags headerFlags = ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DrawLinesToNodes | ImGuiTreeNodeFlags_SpanAvailWidth;

	if (TimelinesOpenByDefault)
	{
		headerFlags |= ImGuiTreeNodeFlags_DefaultOpen;
	}

	for (s32 index = 0; index < static_cast<s32>(clips.size()); ++index)
	{
		const SClipRef& reference = clips[index];

		ImGui::PushID(index);

		if (TimelinesOpenDirty)
		{
			ImGui::SetNextItemOpen(TimelinesOpenByDefault, ImGuiCond_Always);
		}

		string256 label = {};
		xr_sprintf(label, "%s — %s", reference.Clip->Name.c_str(), CinemaFileName(reference.Source->Id.c_str()));

		if (ImGui::TreeNodeEx(label, headerFlags))
		{
			DrawClipTimeline(actor, *reference.Source, *reference.Clip);
			ImGui::TreePop();
		}

		ImGui::PopID();
	}

	TimelinesOpenDirty = false;
}

void CCinemaFilmMakerManager::DrawClipTimeline(SCinemaActor& actor, const SAnimationSource& source, const SAnimationClip& clip)
{
	const MotionID motion = CinemaFindMotion(actor.Animated, source, clip);

	if (!motion.valid())
	{
		ImGui::TextDisabled("This animation is not loaded for the model");
		return;
	}

	const float total = actor.Animated->get_animation_length(motion);
	const bool playing = actor.Motion.valid() && actor.Motion == motion;
	CBlend* blend = playing ? CinemaFindBlend(actor.Animated, motion) : nullptr;
	float time = blend != nullptr ? blend->timeCurrent : 0.0f;

	STrimRange* trim = FindTrimEdit(actor, source.Id, clip.Name);

	if (trim == nullptr && total > 0.0f)
	{
		STrimRange entry;
		entry.Source = source.Id;
		entry.Name = clip.Name;
		entry.Speed = actor.Speed != 0.0f ? actor.Speed : 1.0f;
		actor.Trims.push_back(entry);
		trim = &actor.Trims.back();
	}

	if (!playing)
	{
		time = trim != nullptr && !trim->Segments.empty() ? trim->Segments.front().Start : 0.0f;
	}

	string512 trimKey = {};
	xr_sprintf(trimKey, "%s|%s", source.Id.c_str(), clip.Name.c_str());

	if (DrawAnimationTimeline("##cinema_clip_timeline", time, total, actor.Animated->LL_GetMotionDef(motion), GetTheme(), trim, TrimDragMarker, TrimDragIndex, TrimDragKey, trimKey))
	{
		if (!playing)
		{
			PreviewClip(actor, source, clip);
		}

		actor.Paused = true;
		CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
		CinemaSetBlendTime(actor.Animated, actor.Motion, time);
	}

	if (ImGui::IsItemHovered() || ImGui::IsItemActive())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
	}

	if (blend != nullptr)
	{
		const u32 frame = static_cast<u32>(iFloor(time / SAMPLE_SPF + 0.5f));
		const u32 frames = static_cast<u32>(iFloor(total / SAMPLE_SPF + 0.5f));
		ImGui::Text("Frame: %u / %u    Time: %.3f / %.3f s", frame, frames, time, total);
	}
	else
	{
		ImGui::TextDisabled("Clip length: %.3f s", total);
	}

	if (ImGui::TreeNodeEx("Segments###clip_segments", ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
	{
		const s32 segmentCount = trim != nullptr ? static_cast<s32>(trim->Segments.size()) : 0;

		if (segmentCount > 0)
		{
			for (s32 index = 0; index < segmentCount; ++index)
			{
				const STrimSegment& segment = trim->Segments[index];

				ImGui::PushID(index);
				ImGui::Text("#%d  %.3f - %.3f s  (%.3f s)", index + 1, segment.Start, segment.End, segment.End - segment.Start);
				ImGui::SameLine();

				if (ImGui::SmallButton("Remove##segment"))
				{
					trim->Segments.erase(trim->Segments.begin() + index);
					ImGui::PopID();
					ImGui::TreePop();
					return;
				}

				ImGui::PopID();
			}

			if (ImGui::SmallButton("Clear segments##clip"))
			{
				trim->Segments.clear();
			}
		}
		else
		{
			ImGui::TextDisabled("Whole clip is played. Ctrl+drag on the timeline to add an IN/OUT segment chain.");
		}

		ImGui::TreePop();
	}

	if (ImGui::TreeNodeEx("Playback###clip_playback", ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DrawLinesToNodes))
	{
		float speed = trim != nullptr ? trim->Speed : actor.Speed;

		ImGui::SetNextItemWidth(90.0f);

		if (trim != nullptr && ImGui::DragFloat("Speed##clip", &speed, 0.01f, -4.0f, 4.0f, "%.2f"))
		{
			trim->Speed = speed;
			ApplySpeed(actor);
		}

		ImGui::SameLine();
		ImGui::TextDisabled("base: %.2f", actor.Speed);

		float blendTime = trim != nullptr ? trim->Blend : 0.0f;

		ImGui::SetNextItemWidth(90.0f);

		if (trim != nullptr && ImGui::DragFloat("Blend##clip", &blendTime, 0.005f, 0.0f, 1.0f, "%.2f s"))
		{
			trim->Blend = blendTime;
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Smooth transition between segments, in seconds (0 - hard cut)");
		}

		ImGui::SameLine();
		ImGui::TextDisabled("0 - hard cut");

		ImGui::TreePop();
	}

	if (trim != nullptr && trim->Segments.empty() && fabsf(trim->Speed - actor.Speed) <= 0.001f && trim->Blend <= 0.0f)
	{
		RemoveTrim(actor, source.Id, clip.Name);
	}
}

void CCinemaFilmMakerManager::DrawModelTab()
{
	if (!ModelFilesBuilt)
	{
		BuildModelFiles();
	}

	const float availableHeight = ImGui::GetContentRegionAvail().y;
	const float listHeight = std::max(150.0f, availableHeight - 105.0f);
	const bool hasModel = SelectedVisual.size() > 0 && VisualPathExists(SelectedVisual.c_str());

	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputTextWithHint("##cinema_ogf_filter", "Search .ogf", ModelFileFilter, sizeof(ModelFileFilter));

	xr_vector<s32> filteredFiles;
	filteredFiles.reserve(ModelFiles.size());

	for (s32 i = 0; i < static_cast<s32>(ModelFiles.size()); ++i)
	{
		if (CinemaContainsFilter(ModelFiles[i].c_str(), ModelFileFilter))
		{
			filteredFiles.push_back(i);
		}
	}

	ImGui::BeginChild("##cinema_ogf_list", ImVec2(0.0f, listHeight), true);

	ImGuiListClipper fileClipper;
	fileClipper.Begin(static_cast<int>(filteredFiles.size()));

	while (fileClipper.Step())
	{
		for (int row = fileClipper.DisplayStart; row < fileClipper.DisplayEnd; ++row)
		{
			const s32 index = filteredFiles[row];
			const shared_str& file = ModelFiles[index];

			ImGui::PushID(index);

			if (ImGui::Selectable(file.c_str(), SelectedOgf == index))
			{
				SelectedOgf = index;
				SelectedVisual = file;

				if (SCinemaActor* actor = GetSelectedActor())
				{
					SetActorVisual(*actor, SelectedVisual.c_str());
				}
			}

			if (ImGui::IsItemHovered())
			{
				ImGui::SetTooltip("%s", file.c_str());
			}

			ImGui::PopID();
		}
	}

	if (filteredFiles.empty())
	{
		ImGui::TextDisabled("Nothing found");
	}

	ImGui::EndChild();

	ImGui::Separator();
	ImGui::TextDisabled("Model");
	ImGui::SameLine();
	ImGui::TextUnformatted(SelectedVisual.size() > 0 ? CinemaFileName(SelectedVisual.c_str()) : "<none>");

	if (ImGui::IsItemHovered() && SelectedVisual.size() > 0)
	{
		ImGui::SetTooltip("%s", SelectedVisual.c_str());
	}

	SToolbarLayout actions(ImGui::GetContentRegionAvail().x);

	actions.Item(CinemaButtonWidth("Apply to selected"));
	ImGui::BeginDisabled(!hasModel || GetSelectedActor() == nullptr);

	if (ImGui::Button("Apply to selected"))
	{
		if (SCinemaActor* actor = GetSelectedActor())
		{
			SetActorVisual(*actor, SelectedVisual.c_str());
		}
	}

	ImGui::EndDisabled();

	actions.Item(CinemaButtonWidth("Add"));
	ImGui::BeginDisabled(!hasModel);

	if (ImGui::Button("Add##model"))
	{
		AddActorInFront(SelectedVisual.c_str());
	}

	ImGui::EndDisabled();
}

void CCinemaFilmMakerManager::DrawItemTab()
{
	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		ImGui::TextDisabled("Select a fake NPC to give it a held item");
		return;
	}

	BuildHandItems();

	const float availableHeight = ImGui::GetContentRegionAvail().y;
	const float listHeight = std::max(150.0f, availableHeight - 105.0f);

	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputTextWithHint("##cinema_item_filter", "Search items", ItemFilter, sizeof(ItemFilter));

	xr_vector<s32> filteredItems;
	filteredItems.reserve(HandItems.size());

	for (s32 i = 0; i < static_cast<s32>(HandItems.size()); ++i)
	{
		if (CinemaContainsFilter(HandItems[i].Section.c_str(), ItemFilter) || CinemaContainsFilter(HandItems[i].Visual.c_str(), ItemFilter))
		{
			filteredItems.push_back(i);
		}
	}

	ImGui::BeginChild("##cinema_items_list", ImVec2(0.0f, listHeight), true);

	ImGuiListClipper itemClipper;
	itemClipper.Begin(static_cast<int>(filteredItems.size()));

	while (itemClipper.Step())
	{
		for (int row = itemClipper.DisplayStart; row < itemClipper.DisplayEnd; ++row)
		{
			const s32 index = filteredItems[row];
			const SHandItem& item = HandItems[index];
			const bool selected = actor->ItemSection.size() > 0 && xr_strcmp(actor->ItemSection.c_str(), item.Section.c_str()) == 0;

			ImGui::PushID(index);

			if (ImGui::Selectable(item.Section.c_str(), selected))
			{
				SetActorItem(*actor, item.Section);
			}

			if (ImGui::IsItemHovered())
			{
				ImGui::SetTooltip("%s\n%s", item.Section.c_str(), item.Visual.c_str());
			}

			ImGui::PopID();
		}
	}

	if (filteredItems.empty())
	{
		ImGui::TextDisabled("Nothing found");
	}

	ImGui::EndChild();

	ImGui::Separator();
	ImGui::TextDisabled("Item");
	ImGui::SameLine();
	ImGui::TextUnformatted(actor->ItemSection.size() > 0 ? actor->ItemSection.c_str() : "<none>");

	if (actor->ItemSection.size() > 0 && actor->Skeleton != nullptr)
	{
		const u16 boneL = actor->ItemHandDependence == 1 ? actor->ItemBoneR2 : actor->ItemBoneL;
		const bool resolvable = actor->ItemWeaponStyle ? (actor->ItemBoneR != u16(-1) && boneL != u16(-1)) : (actor->ItemBoneAttach != u16(-1));

		if (!resolvable)
		{
			ImGui::TextDisabled("This model has no bones for this item");
		}
	}

	SToolbarLayout actions(ImGui::GetContentRegionAvail().x);

	actions.Item(CinemaButtonWidth("Remove"));
	ImGui::BeginDisabled(actor->ItemSection.size() == 0);

	if (ImGui::Button("Remove##item"))
	{
		SetActorItem(*actor, shared_str());
	}

	ImGui::EndDisabled();
}

void CCinemaFilmMakerManager::DrawAnimationTab()
{
	DrawExtraSources();

	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr)
	{
		ImGui::TextDisabled("Select a fake NPC to edit its animation playlist");
		return;
	}

	if (actor->Animated == nullptr)
	{
		ImGui::TextDisabled("The selected model has no animations");
		return;
	}

	if (actor->Sources.empty())
	{
		ImGui::TextDisabled("No animation sources are available for this skeleton");
		return;
	}

	const float available = ImGui::GetContentRegionAvail().y;
	const float libraryHeight = std::clamp(available * 0.55f, 120.0f, std::max(120.0f, available - 120.0f));
	const float availableWidth = ImGui::GetContentRegionAvail().x;

	bool hasSelectedClip = false;

	if (SelectedSource >= 0 && SelectedSource < static_cast<s32>(actor->Sources.size()))
	{
		hasSelectedClip = SelectedClip >= 0 && SelectedClip < static_cast<s32>(actor->Sources[SelectedSource].Clips.size());
	}

	const float addWidth = CinemaButtonWidth("Add");
	ImGui::SetNextItemWidth(std::max(80.0f, availableWidth - addWidth - ImGui::GetStyle().ItemSpacing.x));
	ImGui::InputTextWithHint("##cinema_clip_filter", "Search animations and sources", ClipFilter, sizeof(ClipFilter));
	ImGui::SameLine();
	ImGui::BeginDisabled(!hasSelectedClip);

	if (ImGui::Button("Add##clip"))
	{
		const SAnimationSource& source = actor->Sources[SelectedSource];
		const SAnimationClip& clip = source.Clips[SelectedClip];
		AddClipToPlaylist(*actor, source.Id, clip.Name, clip.Fx);
	}

	ImGui::EndDisabled();

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Add the selected animation to the playlist");
	}

	ImGui::BeginChild("##cinema_animation_library", ImVec2(0.0f, libraryHeight), true);

	bool anySource = false;

	for (s32 sourceIndex = 0; sourceIndex < static_cast<s32>(actor->Sources.size()); ++sourceIndex)
	{
		const SAnimationSource& source = actor->Sources[sourceIndex];
		const bool sourceMatches = CinemaContainsFilter(source.Id.c_str(), ClipFilter);
		bool hasMatchingClip = false;

		for (const SAnimationClip& clip : source.Clips)
		{
			if (sourceMatches || CinemaContainsFilter(clip.Name.c_str(), ClipFilter))
			{
				hasMatchingClip = true;
				break;
			}
		}

		if (!sourceMatches && !hasMatchingClip)
		{
			continue;
		}

		anySource = true;

		ImGui::PushID(sourceIndex);

		string256 sourceLabel = {};
		xr_sprintf(sourceLabel, "%s (%u)", CinemaFileName(source.Id.c_str()), static_cast<u32>(source.Clips.size()));

		ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_SpanAvailWidth;

		if (SelectedSource == sourceIndex)
		{
			flags |= ImGuiTreeNodeFlags_Selected;
		}

		if (SelectedSource == sourceIndex || (SelectedSource < 0 && sourceIndex == 0) || (ClipFilter[0] != '\0' && hasMatchingClip))
		{
			flags |= ImGuiTreeNodeFlags_DefaultOpen;
		}

		const bool open = ImGui::TreeNodeEx(sourceLabel, flags);

		if (ImGui::IsItemClicked())
		{
			SelectedSource = sourceIndex;
			SelectedClip = -1;
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("%s", source.Id.c_str());
		}

		if (open)
		{
			for (s32 clipIndex = 0; clipIndex < static_cast<s32>(source.Clips.size()); ++clipIndex)
			{
				const SAnimationClip& clip = source.Clips[clipIndex];

				if (!sourceMatches && !CinemaContainsFilter(clip.Name.c_str(), ClipFilter))
				{
					continue;
				}

				ImGui::PushID(clipIndex);

				string256 clipLabel = {};
				xr_sprintf(clipLabel, "%s%s", clip.Name.c_str(), clip.Fx ? " [fx]" : "");

				const bool selected = SelectedSource == sourceIndex && SelectedClip == clipIndex;

				if (ImGui::Selectable(clipLabel, selected))
				{
					SelectedSource = sourceIndex;
					SelectedClip = clipIndex;
					PreviewClip(*actor, source, clip);
				}

				if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
				{
					SelectedSource = sourceIndex;
					SelectedClip = clipIndex;
					AddClipToPlaylist(*actor, source.Id, clip.Name, clip.Fx);
				}

				if (ImGui::IsItemHovered())
				{
					ImGui::SetTooltip("%s\n%s", clip.Name.c_str(), source.Id.c_str());
				}

				if (ImGui::BeginPopupContextItem("##cinema_clip_context"))
				{
					if (ImGui::MenuItem("Add to playlist"))
					{
						SelectedSource = sourceIndex;
						SelectedClip = clipIndex;
						AddClipToPlaylist(*actor, source.Id, clip.Name, clip.Fx);
					}

					if (ImGui::MenuItem("Play preview"))
					{
						SelectedSource = sourceIndex;
						SelectedClip = clipIndex;
						PreviewClip(*actor, source, clip);
					}

					ImGui::EndPopup();
				}

				if (ImGui::BeginDragDropSource())
				{
					SClipDragPayload payload = {};
					xr_strcpy(payload.Source, source.Id.c_str());
					xr_strcpy(payload.Name, clip.Name.c_str());
					payload.Fx = clip.Fx;

					ImGui::SetDragDropPayload(kCinemaClipPayload, &payload, sizeof(payload));
					ImGui::TextUnformatted(clipLabel);
					ImGui::EndDragDropSource();
				}

				ImGui::PopID();
			}

			ImGui::TreePop();
		}

		ImGui::PopID();
	}

	if (!anySource)
	{
		ImGui::TextDisabled("Nothing found");
	}

	ImGui::EndChild();

	DrawPlaylist(*actor);
}

void CCinemaFilmMakerManager::DrawPlaylist(SCinemaActor& actor)
{
	ImGui::SeparatorText("Playlist");

	SToolbarLayout playlistControls(ImGui::GetContentRegionAvail().x);

	playlistControls.Item(CinemaButtonWidth("Play"));
	if (ImGui::Button("Play##playlist"))
	{
		StartPlaylist(actor, actor.PlaylistSelected >= 0 ? actor.PlaylistSelected : 0);
	}

	playlistControls.Item(CinemaButtonWidth("Stop"));
	if (ImGui::Button("Stop##playlist"))
	{
		StopActor(actor);
	}

	playlistControls.Item(CinemaButtonWidth("Clear"));
	ImGui::BeginDisabled(actor.Playlist.empty());

	if (ImGui::Button("Clear##playlist"))
	{
		StopActor(actor);
		actor.Playlist.clear();
		actor.PlaylistSelected = -1;
	}

	ImGui::EndDisabled();

	playlistControls.Item(CinemaButtonWidth("Entries: 00"));
	ImGui::TextDisabled("Entries: %u", static_cast<u32>(actor.Playlist.size()));

	bool loop = actor.Loop;

	playlistControls.Item(CinemaButtonWidth("Loop") + 20.0f);
	if (ImGui::Checkbox("Loop##animations", &loop))
	{
		actor.Loop = loop;

		if (actor.Animated != nullptr && actor.Motion.valid() && !actor.MotionFx && actor.PlaylistIndex < 0)
		{
			CinemaSetBlendLoop(actor.Animated, actor.Motion, actor.Loop);
		}
	}

	ImGui::BeginChild("##cinema_playlist_items", ImVec2(0.0f, 0.0f), true);

	if (ImGui::BeginDragDropTarget())
	{
		if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload(kCinemaClipPayload))
		{
			const SClipDragPayload* clip = static_cast<const SClipDragPayload*>(payload->Data);
			AddClipToPlaylist(actor, clip->Source, clip->Name, clip->Fx);
		}
		else if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload(kCinemaClipIndexPayload))
		{
			const SIndexDragPayload* data = static_cast<const SIndexDragPayload*>(payload->Data);
			MovePlaylistEntry(actor, data->Index, static_cast<s32>(actor.Playlist.size()));
		}

		ImGui::EndDragDropTarget();
	}

	if (actor.Playlist.empty())
	{
		ImGui::TextDisabled("Drag animations here or use the context menu");
	}

	for (s32 i = 0; i < static_cast<s32>(actor.Playlist.size()); ++i)
	{
		const SPlaylistEntry& entry = actor.Playlist[i];

		ImGui::PushID(i);

		string256 label = {};
		xr_sprintf(label, "%d. %s%s%s", i + 1, entry.Name.c_str(), entry.Fx ? " [fx]" : "", actor.PlaylistIndex == i ? "  [playing]" : "");

		if (ImGui::Selectable(label, actor.PlaylistSelected == i))
		{
			actor.PlaylistSelected = i;
		}

		if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
		{
			StartPlaylist(actor, i);
		}

		if (ImGui::BeginDragDropSource())
		{
			SIndexDragPayload payload = {};
			payload.Index = i;

			ImGui::SetDragDropPayload(kCinemaClipIndexPayload, &payload, sizeof(payload));
			ImGui::TextUnformatted(label);
			ImGui::EndDragDropSource();
		}

		if (ImGui::BeginDragDropTarget())
		{
			if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload(kCinemaClipIndexPayload))
			{
				const SIndexDragPayload* data = static_cast<const SIndexDragPayload*>(payload->Data);
				MovePlaylistEntry(actor, data->Index, i);
			}
			else if (const ImGuiPayload* payload = ImGui::AcceptDragDropPayload(kCinemaClipPayload))
			{
				const SClipDragPayload* clip = static_cast<const SClipDragPayload*>(payload->Data);
				AddClipToPlaylist(actor, clip->Source, clip->Name, clip->Fx);
			}

			ImGui::EndDragDropTarget();
		}

		if (ImGui::BeginPopupContextItem("##cinema_playlist_context"))
		{
			if (ImGui::MenuItem("Play from here"))
			{
				StartPlaylist(actor, i);
			}

			if (ImGui::MenuItem("Remove"))
			{
				RemovePlaylistEntry(actor, i);
				ImGui::EndPopup();
				ImGui::PopID();
				break;
			}

			ImGui::EndPopup();
		}

		ImGui::PopID();
	}

	ImGui::EndChild();
}

void CCinemaFilmMakerManager::DrawPanel()
{
	if (!g_pGameLevel)
	{
		ImGui::TextDisabled("Load a level to use the cinema editor");
		return;
	}

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(4.0f, 4.0f));
	ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(5.0f, 3.0f));

	if (!ModelFilesBuilt)
	{
		BuildModelFiles();
	}

	DrawToolbar();
	ImGui::Separator();

	ImGui::BeginChild("##cinema_tab_content", ImVec2(0.0f, 0.0f), false);

	if (ImGui::BeginTabBar("##cinema_tabs"))
	{
		if (ImGui::BeginTabItem("Actor"))
		{
			DrawSelectedActor();
			ImGui::EndTabItem();
		}

		if (ImGui::BeginTabItem("Model"))
		{
			DrawModelTab();
			ImGui::EndTabItem();
		}

		if (ImGui::BeginTabItem("Presets"))
		{
			DrawPresetTab();
			ImGui::EndTabItem();
		}

		if (ImGui::BeginTabItem("Item"))
		{
			DrawItemTab();
			ImGui::EndTabItem();
		}

		if (ImGui::BeginTabItem("Animations"))
		{
			DrawAnimationTab();
			ImGui::EndTabItem();
		}

		ImGui::EndTabBar();
	}

	ImGui::EndChild();

	ImGui::PopStyleVar(2);

	DrawSceneDialog();
}

EGizmoAxis CCinemaFilmMakerManager::HitTranslateAxis(const Fvector& pivot, float size, const ImVec2& mouse) const
{
	const SProjectedPoint origin = ProjectWorld(pivot);

	if (!origin.Visible)
	{
		return EGizmoAxis::None;
	}

	EGizmoAxis bestAxis = EGizmoAxis::None;
	float bestDistance = 8.0f;

	for (const EGizmoAxis axis : { EGizmoAxis::X, EGizmoAxis::Y, EGizmoAxis::Z })
	{
		const Fvector endWorld = Fvector().mad(pivot, AxisVector(axis), size);
		const SProjectedPoint end = ProjectWorld(endWorld);

		if (!end.Visible)
		{
			continue;
		}

		const float distance = DistanceToSegment(mouse, origin.Screen, end.Screen);

		if (distance < bestDistance)
		{
			bestDistance = distance;
			bestAxis = axis;
		}
	}

	return bestAxis;
}

EGizmoAxis CCinemaFilmMakerManager::HitRotateAxis(const Fvector& pivot, float size, const ImVec2& mouse) const
{
	constexpr int segments = 64;

	EGizmoAxis bestAxis = EGizmoAxis::None;
	float bestDistance = 8.0f;

	for (const EGizmoAxis axis : { EGizmoAxis::X, EGizmoAxis::Y, EGizmoAxis::Z })
	{
		Fvector u = {};
		Fvector v = {};
		RotationBasis(axis, u, v);

		ImVec2 previous = ImVec2(0.0f, 0.0f);
		bool hasPrevious = false;

		for (int segment = 0; segment <= segments; ++segment)
		{
			const float angle = PI_MUL_2 * static_cast<float>(segment) / static_cast<float>(segments);
			const Fvector point = Fvector().mad(pivot, u, cosf(angle) * size).mad(v, sinf(angle) * size);
			const SProjectedPoint projected = ProjectWorld(point);

			if (projected.Visible)
			{
				if (hasPrevious)
				{
					const float distance = DistanceToSegment(mouse, previous, projected.Screen);

					if (distance < bestDistance)
					{
						bestDistance = distance;
						bestAxis = axis;
					}
				}

				previous = projected.Screen;
				hasPrevious = true;
			}
			else
			{
				hasPrevious = false;
			}
		}
	}

	return bestAxis;
}

void CCinemaFilmMakerManager::AppendTranslateGizmo(LevelInspector& inspector, const Fvector& pivot, float size, EGizmoAxis hoveredAxis)
{
	for (const EGizmoAxis axis : { EGizmoAxis::X, EGizmoAxis::Y, EGizmoAxis::Z })
	{
		const bool highlighted = axis == hoveredAxis || (Gizmo.Active && Gizmo.Axis == axis);
		const u32 color = AxisColor(axis, highlighted);
		const Fvector direction = AxisVector(axis);
		const Fvector end = Fvector().mad(pivot, direction, size);

		inspector.append_lines_arrow(pivot, direction, size, color);

		const char* label = axis == EGizmoAxis::X ? "X" : axis == EGizmoAxis::Y ? "Y" : "Z";
		inspector.append_text3d(end, label, color);
	}

	inspector.append_sphere(pivot, size * 0.05f, GetTheme().AxisPivot);
}

void CCinemaFilmMakerManager::AppendRotateGizmo(LevelInspector& inspector, const Fvector& pivot, float size, EGizmoAxis hoveredAxis)
{
	constexpr int segments = 64;

	for (const EGizmoAxis axis : { EGizmoAxis::X, EGizmoAxis::Y, EGizmoAxis::Z })
	{
		const bool highlighted = axis == hoveredAxis || (Gizmo.Active && Gizmo.Axis == axis);
		const u32 color = AxisColor(axis, highlighted);
		Fvector u = {};
		Fvector v = {};
		RotationBasis(axis, u, v);

		Fvector previous = {};
		bool hasPrevious = false;

		for (int segment = 0; segment <= segments; ++segment)
		{
			const float angle = PI_MUL_2 * static_cast<float>(segment) / static_cast<float>(segments);
			const Fvector point = Fvector().mad(pivot, u, cosf(angle) * size).mad(v, sinf(angle) * size);

			if (hasPrevious)
			{
				inspector.append_line(previous, point, color);
			}

			previous = point;
			hasPrevious = true;
		}
	}

	inspector.append_sphere(pivot, size * 0.035f, GetTheme().AxisPivot);
}

void CCinemaFilmMakerManager::AppendWorldGizmo()
{
	if (g_pGameLevel == nullptr || g_hud == nullptr)
	{
		return;
	}

	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)])
	{
		return;
	}

	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr || actor->Model == nullptr)
	{
		return;
	}

	const float distance = Device.vCameraPosition.distance_to(actor->Position);
	const float size = std::clamp(distance * 0.15f, 0.5f, 8.0f);
	const EGizmoAxis hoveredAxis = Gizmo.Active ? Gizmo.Axis : HoveredAxis;
	LevelInspector& inspector = HUD().world_prims;

	if (GizmoMode == EGizmoMode::Translate)
	{
		AppendTranslateGizmo(inspector, actor->Position, size, hoveredAxis);
	}
	else
	{
		AppendRotateGizmo(inspector, actor->Position, size, hoveredAxis);
	}
}

void CCinemaFilmMakerManager::BeginGizmo(SCinemaActor& actor, EGizmoAxis axis)
{
	if (axis == EGizmoAxis::None)
	{
		return;
	}

	Gizmo = SGizmoDrag();
	Gizmo.Active = true;
	Gizmo.Actor = &actor;
	Gizmo.Mode = GizmoMode;
	Gizmo.Axis = axis;
	Gizmo.AxisDir = AxisVector(axis);
	Gizmo.Pivot = actor.Position;
	Gizmo.StartPosition = actor.Position;
	Gizmo.StartOrientation = actor.Orientation;

	Fvector origin = {};
	Fvector direction = {};

	if (!MouseRay(origin, direction))
	{
		ClearGizmo();
		return;
	}

	if (Gizmo.Mode == EGizmoMode::Translate)
	{
		if (!ClosestPointOnAxis(Gizmo.Pivot, Gizmo.AxisDir, origin, direction, Gizmo.StartParam))
		{
			ClearGizmo();
		}

		return;
	}

	float angle = 0.0f;
	bool screenFallback = false;

	if (!ComputeMouseAngle(Gizmo.Pivot, Gizmo.Axis, angle, screenFallback))
	{
		ClearGizmo();
		return;
	}

	Gizmo.LastAngle = angle;
	Gizmo.AccumulatedAngle = 0.0f;
	Gizmo.UseScreenAngle = screenFallback;
}

void CCinemaFilmMakerManager::UpdateGizmo(SCinemaActor& actor)
{
	if (!Gizmo.Active || Gizmo.Actor != &actor)
	{
		return;
	}

	Fvector origin = {};
	Fvector direction = {};

	if (!MouseRay(origin, direction))
	{
		return;
	}

	if (Gizmo.Mode == EGizmoMode::Translate)
	{
		float parameter = 0.0f;

		if (!ClosestPointOnAxis(Gizmo.Pivot, Gizmo.AxisDir, origin, direction, parameter))
		{
			return;
		}

		float delta = parameter - Gizmo.StartParam;

		if (Snap)
		{
			delta = roundf(delta / TranslateStep) * TranslateStep;
		}

		actor.Position.mad(Gizmo.StartPosition, Gizmo.AxisDir, delta);

		if (SnapToGround && Gizmo.Axis != EGizmoAxis::Y)
		{
			SnapActorToGround(actor);
		}

		return;
	}

	float angle = 0.0f;
	bool screenFallback = false;

	if (!ComputeMouseAngle(Gizmo.Pivot, Gizmo.Axis, angle, screenFallback))
	{
		return;
	}

	const float delta = angle_difference_signed(angle, Gizmo.LastAngle);

	Gizmo.LastAngle = angle;
	Gizmo.AccumulatedAngle += delta;

	float accumulated = Gizmo.AccumulatedAngle;

	if (Snap)
	{
		const float degrees = rad2deg(accumulated);
		accumulated = deg2rad(roundf(degrees / RotateStepDeg) * RotateStepDeg);
	}

	Fmatrix rotation;
	rotation.rotation(Gizmo.AxisDir, accumulated);

	actor.Orientation.mul(Gizmo.StartOrientation, rotation);

	Fvector offset = {};
	offset.sub(Gizmo.StartPosition, Gizmo.Pivot);
	rotation.transform_dir(offset, offset);
	actor.Position.add(Gizmo.Pivot, offset);
}

void CCinemaFilmMakerManager::PickActorAtMouse()
{
	Fvector origin = {};
	Fvector direction = {};

	if (!MouseRay(origin, direction))
	{
		return;
	}

	s32 bestIndex = -1;
	float bestDistance = flt_max;

	for (s32 i = 0; i < static_cast<s32>(Actors.size()); ++i)
	{
		const SCinemaActor& actor = *Actors[i];

		if (actor.Model == nullptr)
		{
			continue;
		}

		Fmatrix transform = actor.Orientation;
		transform.c = actor.Position;

		Fvector center = {};
		transform.transform_tiny(center, actor.Model->getVisData().sphere.P);

		float radius = actor.Model->getVisData().sphere.R;

		if (radius < 0.2f)
		{
			radius = 0.5f;
		}

		radius *= std::max(actor.Scale.x, std::max(actor.Scale.y, actor.Scale.z));

		float distance = 0.0f;

		if (RaySphere(origin, direction, center, radius, distance) && distance < bestDistance)
		{
			bestDistance = distance;
			bestIndex = i;
		}
	}

	if (bestIndex < 0)
	{
		const ImVec2 mouse = ImGui::GetIO().MousePos;
		float bestScreenDistance = 32.0f;

		for (s32 i = 0; i < static_cast<s32>(Actors.size()); ++i)
		{
			const SCinemaActor& actor = *Actors[i];

			if (actor.Model == nullptr)
			{
				continue;
			}

			Fmatrix transform = actor.Orientation;
			transform.c = actor.Position;

			Fvector center = {};
			transform.transform_tiny(center, actor.Model->getVisData().sphere.P);
			const SProjectedPoint projected = ProjectWorld(center);

			if (!projected.Visible)
			{
				continue;
			}

			const float dx = projected.Screen.x - mouse.x;
			const float dy = projected.Screen.y - mouse.y;
			const float screenDistance = sqrtf(dx * dx + dy * dy);

			if (screenDistance < bestScreenDistance)
			{
				bestScreenDistance = screenDistance;
				bestIndex = i;
			}
		}
	}

	SelectActor(bestIndex);
}

void CCinemaFilmMakerManager::DrawGizmo()
{
	const bool capturing = CImGuiManager::Instance().IsCapturingInputs();
	const bool mouseOverUi = ImGui::IsWindowHovered(ImGuiHoveredFlags_AnyWindow);
	const bool canInteract = capturing && !mouseOverUi && !ImGui::IsAnyItemActive();

	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr || actor->Model == nullptr || g_pGameLevel == nullptr)
	{
		ClearGizmo();
		HoveredAxis = EGizmoAxis::None;

		if (canInteract && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
		{
			PickActorAtMouse();
		}

		return;
	}

	if (Gizmo.Active && Gizmo.Actor != actor)
	{
		ClearGizmo();
	}

	const ImVec2 mouse = ImGui::GetIO().MousePos;
	const float distance = Device.vCameraPosition.distance_to(actor->Position);
	const float size = std::clamp(distance * 0.15f, 0.5f, 8.0f);

	EGizmoAxis hoveredAxis = EGizmoAxis::None;

	if (!Gizmo.Active && canInteract)
	{
		hoveredAxis = GizmoMode == EGizmoMode::Translate ? HitTranslateAxis(actor->Position, size, mouse) : HitRotateAxis(actor->Position, size, mouse);
	}

	if (Gizmo.Active)
	{
		hoveredAxis = Gizmo.Axis;
	}

	HoveredAxis = hoveredAxis;

	if (Gizmo.Active)
	{
		if (ImGui::IsMouseReleased(ImGuiMouseButton_Left))
		{
			ClearGizmo();
			return;
		}

		if (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right))
		{
			actor->Position = Gizmo.StartPosition;
			actor->Orientation = Gizmo.StartOrientation;
			ClearGizmo();
			return;
		}

		UpdateGizmo(*actor);
		return;
	}

	if (!canInteract)
	{
		return;
	}

	if (ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		if (hoveredAxis != EGizmoAxis::None)
		{
			BeginGizmo(*actor, hoveredAxis);
		}
		else
		{
			PickActorAtMouse();
		}
	}

	if (hoveredAxis != EGizmoAxis::None)
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
	}
}

}

void CinemaFilmMaker_Render()
{
	if (CinemaFilmMaker::g_CinemaFilmMaker != nullptr)
	{
		CinemaFilmMaker::g_CinemaFilmMaker->Render();
	}
}

void CinemaFilmMaker_Shutdown()
{
	if (CinemaFilmMaker::g_CinemaFilmMaker == nullptr)
	{
		return;
	}

	CinemaFilmMaker::g_CinemaFilmMaker->Shutdown();
	Device.seqFrame.Remove(CinemaFilmMaker::g_CinemaFilmMaker);
	xr_delete(CinemaFilmMaker::g_CinemaFilmMaker);
	CinemaFilmMaker::g_CinemaFilmMaker = nullptr;
}

void RenderCinemaFilmMakerEditor()
{
	if (CinemaFilmMaker::g_CinemaFilmMaker == nullptr)
	{
		CinemaFilmMaker::g_CinemaFilmMaker = new CinemaFilmMaker::CCinemaFilmMakerManager();
		Device.seqFrame.Add(CinemaFilmMaker::g_CinemaFilmMaker, REG_PRIORITY_LOW);
	}

	bool& state = Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CinemaFilmMakerEditor)];

	if (!state)
	{
		return;
	}

	CinemaFilmMaker::g_CinemaFilmMaker->DrawGizmo();

	ImGui::SetNextWindowSize(ImVec2(760.0f, 560.0f), ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints(ImVec2(300.0f, 220.0f), ImVec2(FLT_MAX, FLT_MAX));
	CinemaFilmMaker::g_CinemaFilmMaker->PushThemeStyle();

	if (ImGui::Begin("Cinema Film Maker Editor", &state))
	{
		CinemaFilmMaker::g_CinemaFilmMaker->DrawPanel();
	}

	ImGui::End();
	CinemaFilmMaker::g_CinemaFilmMaker->PopThemeStyle();
}
