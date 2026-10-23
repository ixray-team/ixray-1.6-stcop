#include "StdAfx.h"
#include "ImUtils.h"
#include "../HUDManager.h"
#include "../HUDTarget.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "../../xrEngine/pure.h"
#include "../../xrEngine/ImGuiManager.h"
#include "imgui_internal.h"

CBlend* PlayMotionByParts(IKinematicsAnimated* sa, MotionID motion_ID, bool bMixIn, PlayCallback Callback, LPVOID CallbackParam);

namespace CinemaFilmMaker
{
constexpr ImVec4 kCinemaError = ImVec4(0.90f, 0.30f, 0.30f, 1.0f);
constexpr ImVec4 kCinemaWarning = ImVec4(0.95f, 0.75f, 0.30f, 1.0f);
constexpr const char* kCinemaClipPayload = "CINEMA_ANIMATION";
constexpr const char* kCinemaClipIndexPayload = "CINEMA_ANIMATION_INDEX";

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

struct SModelSection
{
	shared_str Name;
	shared_str Visual;
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

struct SCinemaActor
{
	xr_string Name;
	shared_str Visual;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix Orientation;
	Fbox LocalBox;
	IRenderVisual* Model = nullptr;
	IKinematics* Skeleton = nullptr;
	IKinematicsAnimated* Animated = nullptr;
	xr_vector<xr_unique_ptr<SBindBoneParam>> BindPoseBones;
	bool BindPose = false;
	xr_vector<SAnimationSource> Sources;
	xr_vector<SPlaylistEntry> Playlist;
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

	SCinemaActor()
	{
		Orientation.identity();
		LocalBox.invalidate();
	}
};

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

	if (actor.Model && ::Render)
	{
		::Render->model_Delete(actor.Model);
	}

	actor.Model = nullptr;
	actor.Skeleton = nullptr;
	actor.Animated = nullptr;
	actor.LocalBox.invalidate();
	actor.Sources.clear();
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

static u32 AxisColor(EGizmoAxis axis, bool highlighted)
{
	if (highlighted)
	{
		return color_rgba(255, 220, 70, 255);
	}

	switch (axis)
	{
	case EGizmoAxis::X: return color_rgba(220, 70, 70, 255);
	case EGizmoAxis::Y: return color_rgba(80, 210, 90, 255);
	case EGizmoAxis::Z: return color_rgba(70, 130, 235, 255);
	default: return color_rgba(220, 220, 220, 255);
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

static bool DrawAnimationTimeline(const char* id, float& time, float total, const CMotionDef* motionDef)
{
	const float width = std::max(ImGui::GetContentRegionAvail().x, 1.0f);
	constexpr float height = 64.0f;
	const ImVec2 origin = ImGui::GetCursorScreenPos();
	const ImVec2 size = ImVec2(width, height);
	const ImVec2 end = ImVec2(origin.x + size.x, origin.y + size.y);

	ImGui::InvisibleButton(id, size);

	bool changed = false;

	if (total > 0.0f && ImGui::IsItemActive())
	{
		const float normalized = std::clamp((ImGui::GetIO().MousePos.x - origin.x) / width, 0.0f, 1.0f);
		time = normalized * total;
		changed = true;
	}

	ImDrawList* draw = ImGui::GetWindowDrawList();
	const ImU32 colorBackground = IM_COL32(22, 22, 24, 255);
	const ImU32 colorElapsed = IM_COL32(52, 78, 52, 140);
	const ImU32 colorMark = IM_COL32(78, 121, 176, 110);
	const ImU32 colorTickMinor = IM_COL32(58, 58, 62, 255);
	const ImU32 colorTickMajor = IM_COL32(105, 105, 112, 255);
	const ImU32 colorText = IM_COL32(180, 180, 186, 255);
	const ImU32 colorPlayhead = IM_COL32(222, 170, 64, 255);
	const ImU32 colorBorder = IM_COL32(80, 80, 86, 255);

	draw->AddRectFilled(origin, end, colorBackground);

	if (total > 0.0f)
	{
		const float pixelsPerSecond = width / total;
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

		draw->AddLine(ImVec2(playedX, origin.y), ImVec2(playedX, origin.y + height), colorPlayhead, 2.0f);
		draw->AddTriangleFilled(ImVec2(playedX - 5.0f, origin.y), ImVec2(playedX + 5.0f, origin.y), ImVec2(playedX, origin.y + 7.0f), colorPlayhead);

		if (ImGui::IsItemHovered())
		{
			const float hoverTime = std::clamp((ImGui::GetIO().MousePos.x - origin.x) / width * total, 0.0f, total);
			ImGui::SetTooltip("Time: %.3f s\nFrame: %d", hoverTime, iFloor(hoverTime / SAMPLE_SPF + 0.5f));
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

	void OnFrame() override;
	void Render();

	void DrawPanel();
	void DrawGizmo();

	void Shutdown();

private:
	void ClearActors();
	void ClearGizmo();
	void ResetSelection();
	void SelectActor(s32 index);

	void BuildModelSections();
	void BuildModelFiles();
	void BuildLibrary();
	void BuildActorSources(SCinemaActor& actor);

	bool AddActor(const Fvector& position, const char* visual, bool adjustGround);
	bool AddActorAtAim(const char* visual);
	bool AddActorInFront(const char* visual);
	bool SetActorVisual(SCinemaActor& actor, const char* visual, bool adjustGround);
	void DuplicateSelectedActor();
	void DeleteSelectedActor();

	void DrawToolbar();
	void DrawSelectedActor();
	void DrawModelTab();
	void DrawAnimationTab();
	void DrawPlaylist(SCinemaActor& actor);
	void DrawActorTimeline(SCinemaActor& actor);

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
	xr_vector<SModelSection> ModelSections;
	xr_vector<shared_str> ModelFiles;
	IGame_Level* Level = nullptr;
	s32 Selected = -1;
	s32 SelectedSection = -1;
	s32 SelectedOgf = -1;
	s32 SelectedSource = -1;
	s32 SelectedClip = -1;
	s32 NameEditActor = -1;
	shared_str SelectedVisual;
	u32 NextNameIndex = 1;
	bool LibraryBuilt = false;
	bool ModelSectionsBuilt = false;
	bool ModelFilesBuilt = false;
	bool ManualVisualValid = false;
	bool Snap = false;
	float TranslateStep = 0.1f;
	float RotateStepDeg = 5.0f;
	char ModelFilter[128] = {};
	char ModelFileFilter[128] = {};
	char ClipFilter[128] = {};
	char ManualVisual[256] = {};
	char NameEdit[128] = {};
	EGizmoMode GizmoMode = EGizmoMode::Translate;
	EGizmoAxis HoveredAxis = EGizmoAxis::None;
	SGizmoDrag Gizmo;
};

CCinemaFilmMakerManager* g_CinemaFilmMaker = nullptr;

void CCinemaFilmMakerManager::Shutdown()
{
	ClearGizmo();
	ClearActors();
	Library.clear();
	LibraryBuilt = false;
	ModelSections.clear();
	ModelSectionsBuilt = false;
	ModelFiles.clear();
	ModelFilesBuilt = false;
	Level = nullptr;
	Selected = -1;
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
		UpdateActorPlayback(*actor);
	}
}

void CCinemaFilmMakerManager::Render()
{
	if (!g_CinemaFilmMaker || Actors.empty() || g_pGameLevel == nullptr)
	{
		return;
	}

	const bool hudMode = ::Render->get_HUD();
	::Render->set_HUD(false);
	::Render->set_Object(nullptr);

	for (auto& actor : Actors)
	{
		if (actor->Model == nullptr)
		{
			continue;
		}

		if (actor->Skeleton)
		{
			actor->Skeleton->CalculateBones(true);
		}

		Fmatrix transform = actor->Orientation;
		transform.c = actor->Position;

		::Render->set_Transform(&transform);
		::Render->add_Visual(actor->Model, true, true);
		actor->Model->getVisData().hom_frame = Device.dwFrame;
	}

	::Render->set_Transform(&Fidentity);
	::Render->set_HUD(hudMode);

	AppendWorldGizmo();
}

void CCinemaFilmMakerManager::BuildModelSections()
{
	if (ModelSectionsBuilt)
	{
		return;
	}

	ModelSections.clear();

	if (pSettings == nullptr || g_pClsidManager == nullptr)
	{
		return;
	}

	ModelSectionsBuilt = true;

	for (CInifile::Sect& section : pSettings->sections())
	{
		if (!section.line_exist("class") || !section.line_exist("visual"))
		{
			continue;
		}

		const CLASS_ID classId = pSettings->r_clsid(section.Name.c_str(), "class");

		if (!g_pClsidManager->is_npc(classId) && !g_pClsidManager->is_monster(classId))
		{
			continue;
		}

		const shared_str visual = pSettings->r_string(section.Name.c_str(), "visual");

		if (!VisualPathExists(visual.c_str()))
		{
			continue;
		}

		ModelSections.push_back({ .Name = section.Name, .Visual = visual });
	}

	std::sort(ModelSections.begin(), ModelSections.end(), [](const SModelSection& left, const SModelSection& right)
	{
		return xr_strcmp(left.Name.c_str(), right.Name.c_str()) < 0;
	});

	if (SelectedVisual.size() == 0 && !ModelSections.empty())
	{
		SelectedSection = 0;
		SelectedVisual = ModelSections.front().Visual;
	}
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
	const SAnimationClip* clip = CinemaFindDefaultIdle(actor, &source);

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
		PlayMotionByParts(actor.Animated, motion, true, nullptr, nullptr);
		CinemaSetBlendLoop(actor.Animated, motion, playlist ? false : actor.Loop);
		CinemaSetBlendSpeed(actor.Animated, motion, actor.Speed);
	}

	actor.Motion = motion;
	actor.MotionName = clip.Name;
	actor.MotionSource = source.Id;
	actor.MotionFx = clip.Fx;
	actor.Paused = false;
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
		CinemaSetBlendSpeed(actor.Animated, actor.Motion, actor.Speed);
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

	if (!SetActorVisual(*actor, visual, adjustGround))
	{
		return false;
	}

	Actors.push_back(std::move(actor));
	Selected = static_cast<s32>(Actors.size()) - 1;
	ResetSelection();
	return true;
}

bool CCinemaFilmMakerManager::AddActorAtAim(const char* visual)
{
	if (g_pGameLevel == nullptr || g_hud == nullptr || visual == nullptr || visual[0] == '\0')
	{
		return false;
	}

	const collide::rq_result& query = HUD().GetCurrentRayQuery();
	const float range = (query.O != nullptr && query.range > 0.1f) ? query.range : 3.0f;
	Fvector position = {};
	position.mad(Device.vCameraPosition, Device.vCameraDirection, range);
	return AddActor(position, visual, true);
}

bool CCinemaFilmMakerManager::AddActorInFront(const char* visual)
{
	if (g_pGameLevel == nullptr || g_hud == nullptr || visual == nullptr || visual[0] == '\0')
	{
		return false;
	}

	Fvector position = {};
	position.mad(Device.vCameraPosition, Device.vCameraDirection, 3.0f);
	return AddActor(position, visual, false);
}

bool CCinemaFilmMakerManager::SetActorVisual(SCinemaActor& actor, const char* visual, bool adjustGround)
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
		::Render->model_Delete(actor.Model);
	}

	actor.Model = model;
	actor.Skeleton = skeleton;
	actor.Animated = animated;
	actor.Visual = visual;
	actor.LocalBox = model->getVisData().box;
	actor.Playlist.clear();
	actor.PlaylistIndex = -1;
	actor.PlaylistSelected = -1;
	actor.Paused = false;
	actor.Motion.invalidate();
	actor.MotionName = "";
	actor.MotionSource = "";
	actor.MotionFx = false;
	actor.FxDeadline = 0.0f;

	if (adjustGround && actor.LocalBox.is_valid())
	{
		actor.Position.y -= actor.LocalBox.min.y;
	}

	BuildActorSources(actor);
	ApplyDefaultPose(actor);
	SelectedSource = -1;
	SelectedClip = -1;
	return true;
}

void CCinemaFilmMakerManager::DuplicateSelectedActor()
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
	actor->Loop = source->Loop;
	actor->Speed = source->Speed;

	if (!SetActorVisual(*actor, source->Visual.c_str(), false))
	{
		return;
	}

	actor->Playlist = source->Playlist;
	actor->PlaylistSelected = source->PlaylistSelected;
	Actors.push_back(std::move(actor));
	Selected = static_cast<s32>(Actors.size()) - 1;
	ResetSelection();
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
	ReleaseActorModel(*Actors[Selected]);
	Actors.erase(Actors.begin() + Selected);

	if (Selected >= static_cast<s32>(Actors.size()))
	{
		Selected = static_cast<s32>(Actors.size()) - 1;
	}

	ResetSelection();
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
		ImGui::OpenPopup("##cinema_add_popup");
	}

	ImGui::EndDisabled();

	if (ImGui::IsItemHovered() && SelectedVisual.size() > 0)
	{
		ImGui::SetTooltip("Add the selected model\n%s", SelectedVisual.c_str());
	}

	if (ImGui::BeginPopup("##cinema_add_popup"))
	{
		if (ImGui::MenuItem("At crosshair"))
		{
			AddActorAtAim(SelectedVisual.c_str());
		}

		if (ImGui::MenuItem("In front of camera"))
		{
			AddActorInFront(SelectedVisual.c_str());
		}

		ImGui::EndPopup();
	}

	ImGui::BeginDisabled(selectedActor == nullptr);

	layout.Item(CinemaButtonWidth("Duplicate"));
	if (ImGui::Button("Duplicate##toolbar"))
	{
		DuplicateSelectedActor();
	}

	layout.Item(CinemaButtonWidth("Delete"));
	if (ImGui::Button("Delete##toolbar"))
	{
		DeleteSelectedActor();
	}

	ImGui::EndDisabled();

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

	if (ImGui::CollapsingHeader("Timeline"))
	{
		DrawActorTimeline(*actor);
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
	}
}

void CCinemaFilmMakerManager::DrawActorTimeline(SCinemaActor& actor)
{
	if (actor.Animated == nullptr || !actor.Motion.valid() || actor.MotionFx)
	{
		return;
	}

	CBlend* blend = CinemaFindBlend(actor.Animated, actor.Motion);
	const float total = blend ? blend->timeTotal : actor.Animated->get_animation_length(actor.Motion);
	float time = blend ? blend->timeCurrent : 0.0f;

	if (DrawAnimationTimeline("##cinema_timeline", time, total, actor.Animated->LL_GetMotionDef(actor.Motion)))
	{
		actor.Paused = true;
		CinemaSetBlendPlaying(actor.Animated, actor.Motion, false);
		CinemaSetBlendTime(actor.Animated, actor.Motion, time);
	}

	if (ImGui::IsItemHovered() || ImGui::IsItemActive())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
	}

	if (blend)
	{
		const u32 frame = static_cast<u32>(iFloor(time / SAMPLE_SPF + 0.5f));
		const u32 frames = static_cast<u32>(iFloor(total / SAMPLE_SPF + 0.5f));
		ImGui::Text("Frame: %u / %u    Time: %.3f / %.3f s", frame, frames, time, total);
	}
	else
	{
		ImGui::TextDisabled("No animation loaded");
	}
}

void CCinemaFilmMakerManager::DrawModelTab()
{
	BuildModelSections();

	if (!ModelFilesBuilt)
	{
		BuildModelFiles();
	}

	const float availableHeight = ImGui::GetContentRegionAvail().y;
	const float listHeight = std::max(150.0f, availableHeight - 105.0f);
	const bool hasModel = SelectedVisual.size() > 0 && VisualPathExists(SelectedVisual.c_str());

	if (ImGui::BeginTabBar("##cinema_model_tabs"))
	{
		if (ImGui::BeginTabItem("Sections"))
		{
			ImGui::SetNextItemWidth(-1.0f);
			ImGui::InputTextWithHint("##cinema_section_filter", "Search sections", ModelFilter, sizeof(ModelFilter));

			xr_vector<s32> filtered;
			filtered.reserve(ModelSections.size());

			for (s32 i = 0; i < static_cast<s32>(ModelSections.size()); ++i)
			{
				const SModelSection& section = ModelSections[i];

				if (CinemaContainsFilter(section.Name.c_str(), ModelFilter) || CinemaContainsFilter(section.Visual.c_str(), ModelFilter))
				{
					filtered.push_back(i);
				}
			}

			ImGui::BeginChild("##cinema_sections_list", ImVec2(0.0f, listHeight), true);

			ImGuiListClipper clipper;
			clipper.Begin(static_cast<int>(filtered.size()));

			while (clipper.Step())
			{
				for (int row = clipper.DisplayStart; row < clipper.DisplayEnd; ++row)
				{
					const s32 index = filtered[row];
					const SModelSection& section = ModelSections[index];

					ImGui::PushID(index);

					if (ImGui::Selectable(section.Name.c_str(), SelectedSection == index))
					{
						SelectedSection = index;
						SelectedOgf = -1;
						SelectedVisual = section.Visual;
					}

					if (ImGui::IsItemHovered())
					{
						ImGui::SetTooltip("%s\n%s", section.Name.c_str(), section.Visual.c_str());
					}

					ImGui::PopID();
				}
			}

			if (filtered.empty())
			{
				ImGui::TextDisabled("Nothing found");
			}

			ImGui::EndChild();
			ImGui::EndTabItem();
		}

		if (ImGui::BeginTabItem(".ogf"))
		{
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
						SelectedSection = -1;
						SelectedVisual = file;
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
			ImGui::EndTabItem();
		}

		if (ImGui::BeginTabItem("Manual"))
		{
			ImGui::TextDisabled("Path relative to meshes, with or without .ogf");
			ImGui::SetNextItemWidth(-1.0f);

			if (ImGui::InputTextWithHint("##cinema_manual_visual", "actors\\stalker_hero\\stalker_hero_1.ogf", ManualVisual, sizeof(ManualVisual)))
			{
				ManualVisualValid = VisualPathExists(ManualVisual);
			}

			if (ManualVisual[0] != '\0' && !ManualVisualValid)
			{
				ImGui::TextColored(kCinemaError, "Visual not found");
			}
			else if (ManualVisual[0] != '\0')
			{
				ImGui::TextColored(kCinemaWarning, "Visual is valid");
			}

			ImGui::BeginDisabled(!ManualVisualValid);

			if (ImGui::Button("Use this model", ImVec2(-1.0f, 0.0f)))
			{
				SelectedVisual = ManualVisual;
				SelectedSection = -1;
				SelectedOgf = -1;
			}

			ImGui::EndDisabled();
			ImGui::EndTabItem();
		}

		ImGui::EndTabBar();
	}

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
			SetActorVisual(*actor, SelectedVisual.c_str(), false);
		}
	}

	ImGui::EndDisabled();

	actions.Item(CinemaButtonWidth("Add"));
	ImGui::BeginDisabled(!hasModel);

	if (ImGui::Button("Add##model"))
	{
		ImGui::OpenPopup("##cinema_model_add_popup");
	}

	ImGui::EndDisabled();

	if (ImGui::BeginPopup("##cinema_model_add_popup"))
	{
		if (ImGui::MenuItem("At crosshair"))
		{
			AddActorAtAim(SelectedVisual.c_str());
		}

		if (ImGui::MenuItem("In front of camera"))
		{
			AddActorInFront(SelectedVisual.c_str());
		}

		ImGui::EndPopup();
	}
}

void CCinemaFilmMakerManager::DrawAnimationTab()
{
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

	BuildModelSections();
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

		if (ImGui::BeginTabItem("Animations"))
		{
			DrawAnimationTab();
			ImGui::EndTabItem();
		}

		ImGui::EndTabBar();
	}

	ImGui::EndChild();

	ImGui::PopStyleVar(2);
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

	inspector.append_sphere(pivot, size * 0.05f, color_rgba(240, 240, 240, 220));
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

	inspector.append_sphere(pivot, size * 0.035f, color_rgba(240, 240, 240, 220));
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
	SCinemaActor* actor = GetSelectedActor();

	if (actor == nullptr || actor->Model == nullptr || g_pGameLevel == nullptr)
	{
		ClearGizmo();
		return;
	}

	if (Gizmo.Active && Gizmo.Actor != actor)
	{
		ClearGizmo();
	}

	const bool capturing = CImGuiManager::Instance().IsCapturingInputs();
	const bool mouseOverUi = ImGui::IsWindowHovered(ImGuiHoveredFlags_AnyWindow);
	const ImVec2 mouse = ImGui::GetIO().MousePos;
	const float distance = Device.vCameraPosition.distance_to(actor->Position);
	const float size = std::clamp(distance * 0.15f, 0.5f, 8.0f);

	EGizmoAxis hoveredAxis = EGizmoAxis::None;

	if (!Gizmo.Active && capturing && !mouseOverUi)
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

	if (!capturing || mouseOverUi || ImGui::IsAnyItemActive())
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
	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kGeneralAlphaLevelForImGuiWindows));

	if (ImGui::Begin("Cinema Film Maker Editor", &state))
	{
		CinemaFilmMaker::g_CinemaFilmMaker->DrawPanel();
	}

	ImGui::End();
	ImGui::PopStyleColor();
}
