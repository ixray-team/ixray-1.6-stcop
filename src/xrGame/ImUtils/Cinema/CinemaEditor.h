#pragma once
#include "imgui.h"
#include "imgui_internal.h"
#include "ImGuizmo.h"
#include "../../Cinema/CinemaPlayer.h"
#include "../../Cinema/CinemaHistory.h"
#include "../../Cinema/CinemaSceneIo.h"

extern float g_fov;

namespace Cinema
{
inline constexpr ImVec4 ErrorColor = ImVec4(0.95f, 0.35f, 0.35f, 1.0f);
inline constexpr ImVec4 OverrideColor = ImVec4(1.0f, 165.0f / 255.0f, 0.0f, 1.0f);
inline constexpr const char* TypeNames[4] = { "Actor", "Camera", "Light", "Particle" };

inline void Timecode(string32& Out, float Time, u32 Rate)
{
	const u32 Frames = static_cast<u32>(std::floor(std::max(Time, 0.0f) * Rate + 1e-4f));
	xr_sprintf(Out, "%02u:%02u:%02u", Frames / Rate / 60, Frames / Rate % 60, Frames % Rate);
}

inline u32 Lighten(u32 Color)
{
	return 0xFF000000u | (((Color & 0x00FEFEFEu) >> 1) + 0x007F7F7Fu);
}

inline xr_string ModelName(const shared_str& Visual)
{
	return Platform::ANSI_TO_UTF8(NameFromVisual(Visual.c_str()));
}

inline xr_string DefaultName(const Object& Item)
{
	const char* const Fixed[4] = { nullptr, "Camera", Item.Light.Kind == LightKind::Spot ? "Spot light" : "Point light", nullptr };
	xr_string Name = Fixed[static_cast<u32>(Item.Type)] ? xr_string(Fixed[static_cast<u32>(Item.Type)]) : Item.Type == ObjectType::Particle ? Platform::ANSI_TO_UTF8(EffectName(Item.Particle.Effect)) : ModelName(Item.Actor.Visual);
	size_t Size = std::min<size_t>(Name.size(), MaxNameBytes);
	while (Size < Name.size() && (static_cast<u8>(Name[Size]) & 0xC0) == 0x80)
	{
		--Size;
	}
	Name.resize(Size);
	return Name;
}

inline bool IsParamShown(const Object& Target, Param Id)
{
	return !(Target.Type == ObjectType::Light && Target.Light.Kind == LightKind::Point && (Id == Param::Rotation || Id == Param::Cone));
}

inline const char* DisplayName(const Object* Item)
{
	return !Item ? "missing" : Item->Name.empty() ? TypeNames[static_cast<u32>(Item->Type)] : Item->Name.c_str();
}

inline const char* RuntimeError(const Player& Core, const Object& Target)
{
	const ActorRuntime* Body = Target.Type == ObjectType::Actor ? Core.FindActor(Target.Id) : nullptr;
	const LightRuntime* Lamp = Target.Type == ObjectType::Light ? Core.FindLight(Target.Id) : nullptr;
	const ParticleRuntime* Emitter = Target.Type == ObjectType::Particle ? Core.FindParticle(Target.Id) : nullptr;
	const shared_str* Error = Body ? &Body->Error : Lamp ? &Lamp->Error : Emitter ? &Emitter->Error : nullptr;
	return Error && Error->size() ? Error->c_str() : Body && !Body->IsComplete() ? "missing" : nullptr;
}

inline constexpr s32 ShakeBase = 1024;

inline bool HasOwner(const Object& Target, s32 Block)
{
	return Block < 0 || (Block >= ShakeBase ? Block - ShakeBase < static_cast<s32>(Target.Camera.Shakes.size()) : Block < static_cast<s32>(Target.Camera.Blocks.size()));
}

inline xr_vector<Lane>& LaneList(Object& Target, s32 Block)
{
	return Block < 0 ? Target.Lanes : Block >= ShakeBase ? Target.Camera.Shakes[Block - ShakeBase].Lanes : Target.Camera.Blocks[Block].Lanes;
}

inline const xr_vector<Lane>& LaneList(const Object& Target, s32 Block)
{
	return LaneList(const_cast<Object&>(Target), Block);
}

inline float* StaticField(Object& Target, s32 Block, Param Id, u8 Layer, u8 Component)
{
	if (Block < 0 && Id == Param::LayerWeight)
	{
		return Target.Type == ObjectType::Actor && Layer <= Target.Actor.Layers.size() ? &RowOf(Target.Actor, Layer).Weight : nullptr;
	}
	if (Block < 0)
	{
		Fvector* Place = PlacementOf(Target, Id == Param::Rotation ? 1 : 0);
		ActorData& Actor = Target.Actor;
		LightData& Light = Target.Light;
		ParticleData& Particle = Target.Particle;
		float* const Fields[static_cast<u32>(Param::Count)] = { Place ? &(*Place)[Component] : nullptr, Place ? &(*Place)[Component] : nullptr, &Actor.Scale[Component], &Actor.TimeScale, nullptr, &Light.Color[Component], &Light.Intensity, &Light.Range, &Light.Cone, &Light.VirtualSize, &Light.VolumeIntensity, &Light.VolumeDistance, &Light.VolumeQuality, &Particle.Size, &Particle.Speed, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr };
		return (Info(Id).Where & ObjectWhere[static_cast<u32>(Target.Type)]) ? Fields[static_cast<u32>(Id)] : nullptr;
	}
	if (Block >= ShakeBase)
	{
		ShakeClip& Clip = Target.Camera.Shakes[Block - ShakeBase];
		float* const Shaken[3] = { &Clip.Setup.Influence, &Clip.Setup.PositionScale, &Clip.Setup.Speed };
		return Id >= Param::ShakeInfluence && Clip.Row == Layer ? Shaken[static_cast<u32>(Id) - static_cast<u32>(Param::ShakeInfluence)] : nullptr;
	}
	CameraBlock& Shot = Target.Camera.Blocks[Block];
	float* const Shots[] = { &Shot.TimeScale, &Shot.Fov, &Shot.Focus, &Shot.FocusOffset, &Shot.NearRange, &Shot.FarRange, &Shot.Blur };
	const u32 At = Id == Param::TimeScale ? 0 : Id == Param::Fov ? 1 : Id >= Param::DofFocus && Id <= Param::DofBlur ? 2 + static_cast<u32>(Id) - static_cast<u32>(Param::DofFocus) : 7;
	return At < 7 ? Shots[At] : nullptr;
}

inline float TargetStart(const Object& Target, s32 Block)
{
	return Target.Span.Start + (Block < 0 ? 0.0f : Block >= ShakeBase ? Target.Camera.Shakes[Block - ShakeBase].Start : Cameras::BlockStart(Target.Camera, static_cast<u32>(Block)));
}

inline float OwnerLength(const Object& Target, s32 Block)
{
	return Block < 0 ? Target.Span.Length : Block >= ShakeBase ? Target.Camera.Shakes[Block - ShakeBase].Length : Target.Camera.Blocks[Block].Length;
}

inline u8 ShakeRows(const CameraData& Data)
{
	u8 Rows = 0;
	for (const ShakeClip& Clip : Data.Shakes)
	{
		Rows = std::max(Rows, static_cast<u8>(Clip.Row + 1));
	}
	return Rows;
}

inline s32 CameraOwnerAt(const Object& Target, Param Id, u8 Layer, float Local)
{
	const bool IsShake = Id >= Param::ShakeInfluence;
	s32 Found = -1;
	for (u32 Index = 0; IsShake && Index < Target.Camera.Shakes.size(); ++Index)
	{
		const ShakeClip& Clip = Target.Camera.Shakes[Index];
		const bool IsInside = Clip.Row == Layer && Local >= Clip.Start && Local <= Clip.Start + Clip.Length;
		Found = IsInside && (Found < 0 || Clip.Start >= Target.Camera.Shakes[Found - ShakeBase].Start) ? ShakeBase + static_cast<s32>(Index) : Found;
	}
	return IsShake ? Found : Cameras::BlockAt(Target.Camera, Local);

}

inline float GameFov()
{
	return std::clamp(g_fov, 1.0f, 179.0f);
}

enum class ModalKind : u8
{
	None,
	Open,
	Import,
	SaveAs,
	Discard,
	Message,
	SavePreset,
	DeletePreset,
	ShiftTime
};

enum class PendingKind : u8
{
	None,
	New,
	Open,
	Import
};

enum class DragKind : u8
{
	None,
	Scrub,
	LoopRange,
	LoopMove,
	LoopIn,
	LoopOut,
	MarkerMove,
	RowMove,
	ClipMove,
	ClipLength,
	BlockMove,
	BlockIn,
	BlockOut,
	BlockBlend,
	BlockLength,
	BlockStretch,
	PoseKey,
	BlockFadeIn,
	BlockFadeOut,
	ShakeMove,
	ShakeStart,
	ShakeEnd,
	ShakeFadeIn,
	ShakeFadeOut,
	LaneKeys,
	LaneSegment,
	LaneBend,
	HandleIn,
	HandleOut,
	BoxClips,
	BoxKeys
};

enum class ChooserKind : u8
{
	Model,
	Item,
	Animation,
	Pack,
	Preset,
	Effect,
	Texture
};

inline constexpr const char* AddChoices[3] = { "Actor...", "Actor from preset...", "Particle..." };
inline constexpr ChooserKind AddKinds[3] = { ChooserKind::Model, ChooserKind::Preset, ChooserKind::Effect };

enum class KeyState : u8
{
	None,
	Static,
	Animated,
	OnKey
};

enum class RowAction : u8
{
	None,
	Reset,
	RemoveAnimation
};

enum class FocusKind : u8
{
	Objects,
	Block,
	Keys
};

enum class WriteKind : u8
{
	Static,
	OnKey,
	Offset,
	Insert,
	Record
};

enum class GizmoMode : u8
{
	Translate,
	Rotate,
	Scale,
	Range,
	Universal
};

struct GizmoItem
{
	u32 Id = 0;
	u32 Leader = 0;
	s16 Block = -1;
	u16 Written = 0;
	Fvector Values[3] = {};
	Fmatrix Start = Fidentity;
	LinkFrames Frames;
};

struct WriteLatch
{
	u32 ObjectId = 0;
	s16 Block = -1;
	Param Id = Param::Position;
	u8 Layer = 0;
	u8 Component = 0;
	WriteKind Kind = WriteKind::Static;
	u32 KeyIndex = 0;
	float Begin = 0.0f;
	float Last = 0.0f;
	float Value = 0.0f;
	float Start = 0.0f;
};

struct RowItem
{
	u32 Id = 0;
	bool IsFolder = false;
	u8 Depth = 0;
	u32 Order = 0;
	float Y = 0.0f;
	float Height = 0.0f;
	float Track = 0.0f;
};

struct KeyRef
{
	u32 ObjectId = 0;
	s16 Block = -1;
	Param Id = Param::Position;
	u8 Layer = 0;
	u8 Component = 0;
	u32 Index = 0;
	auto operator<=>(const KeyRef&) const = default;
};

struct LaneSlot
{
	u32 ObjectId = 0;
	Param Id = Param::Position;
	u8 Layer = 0;
	u8 Mask = 7;
	bool IsSplit = false;
	bool IsFitted = false;
	float Lo = 0.0f;
	float Hi = 1.0f;
};

struct LaneTarget
{
	s16 Block = -1;
	float Start = 0.0f;
	float Length = 0.0f;
};

struct ShakePin
{
	s32 Block = -1;
	float Offset = 0.0f;
	float Length = 0.0f;
	float FadeIn = 0.0f;
	float FadeOut = 0.0f;
	u8 Row = 0;
};

struct LaneDrag
{
	KeyRef Ref;
	float Span = 1.0f;
	Curve Base;
	Curve* Live = nullptr;
};

struct KeyMove
{
	u32 Source = 0;
	u32 Index = 0;
};

enum class ClipKind : u8
{
	None,
	Objects,
	ActorBlocks,
	CameraBlocks,
	Shakes,
	Keys
};

struct ClipKey
{
	u32 ObjectId = 0;
	Param Id = Param::Position;
	u8 Layer = 0;
	u8 Component = 0;
	Key Item;
};

struct Clipboard
{
	ClipKind Kind = ClipKind::None;
	ObjectType Owner = ObjectType::Actor;
	u32 Generation = 0;
	float Origin = 0.0f;
	float Extent = 0.0f;
	xr_vector<Object> Objects;
	xr_vector<ActorBlock> Blocks;
	xr_vector<shared_str> Packs;
	xr_vector<CameraBlock> Shots;
	xr_vector<ShakeClip> Shakes;
	xr_vector<ClipKey> Keys;
};

struct EditorSettings
{
	bool IsSnapping = true;
	u8 SnapStep = 0;
	float FlySpeed = 4.0f;
	float MoveStep = 0.1f;
	float RotateStep = 5.0f;
	float ScaleStep = 0.1f;
	float RangeStep = 0.1f;
	bool ShowHelpers = true;
	bool DofPreview = true;
	bool ShowInspector = true;
	bool ShowPresets = false;
	float HeaderWidth = 220.0f;
	float Opacity = 0.5f;
	float DefaultRow = 1.0f;
	float DefaultLane = 2.0f;
	bool operator==(const EditorSettings&) const = default;
};

inline constexpr float MinHeaderWidth = 140.0f, MaxHeaderWidth = 600.0f;
inline ActorLayer NewRow(const ActorRuntime* Runtime)
{
	ActorLayer Row;
	for (u16 Part = 0; Runtime && Part < MAX_PARTS; ++Part)
	{
		if (const char* Name = Runtime->PartName(Part))
		{
			Row.Parts.push_back(Name);
		}
	}
	return Row;
}

inline constexpr const char* FrozenClips = "The model or an animation of this actor is missing or still loading; its clips cannot be edited.";

class Editor
{
public:
	Editor();
	void Draw(Player& Core, bool& IsOpen);
	void Close(Player& Core);
	float Opacity() const { return Settings.Opacity; }
	bool WasOpen = false;

private:
	void BeginFrame(Player& Core);
	void EndFrame(Player& Core);
	void EndStep(Player& Core);
	void ResetScene(bool IsSaved);
	void DrawMenuBar(Player& Core);
	void DrawModals(Player& Core);
	void RequestScene(Player& Core, PendingKind Kind, bool IsConfirmed);
	void ShowModal(const Player& Core, ModalKind Kind);
	void CheckName();
	void OpenScene(Player& Core, const char* Path, const xr_string& Name, bool IsNamed);
	void Save(Player& Core);
	void SaveScene(const Player& Core, const xr_string& File);
	void Message(const char* Title, xr_vector<xr_string> Lines);
	void SetStatus(bool IsError, const char* Format, ...);
	void HandleShortcuts(Player& Core);
	void UndoRedo(Player& Core, bool IsRedo);
	void LoadSettings();
	void SaveSettings();
	void UpdateFly(Player& Core);
	void UseEditorCamera(Player& Core);
	void DrawHelpers(const Player& Core) const;
	s32 HelperBlock(const Player& Core, const Object& Rig) const;
	bool IsViewportFree() const;
	void CollectGizmo(const Player& Core, bool HasCamera, GizmoMode Mode = GizmoMode::Translate);
	bool HasSelectedParent(const Scene& Data, u32 Parent, GizmoMode Mode) const;
	void PutTransform(Player& Core, GizmoItem& Item, u32 Kind, const Fvector& NewValue);
	void PutWorld(Player& Core, GizmoItem& Item, Fmatrix World, u32 Kinds);
	void DrawGizmo(Player& Core);
	void DrawRangeHandle(Player& Core);
	void DrawTangents(Player& Core);
	void DrawViewCube(Player& Core);
	void Pick(Player& Core);
	void DrawViewportPopup(Player& Core);
	static Fvector FacingRotation(const Fvector& Position, const Fvector* Normal);

	Object* Edit(Player& Core, u32 Id, const char* Label);
	Object* CreateObject(Player& Core, ObjectType Type, float Start);
	Object* AddObject(Player& Core, ObjectType Type, const shared_str& Source, float Start, const Fvector& Position, const Fvector& Rotation);
	void AddLight(Player& Core, float Start, const Fvector& Position);
	void LoadPresets();
	bool StorePresets(xr_vector<ActorPreset>&& Next, const xr_string& Removed);
	s32 FindPreset(const char* Name) const;
	void ApplyPreset(Player& Core, const xr_vector<u32>& Ids, const char* Name);
	void AddActorFromPreset(Player& Core, const char* Name, float Start, const Fvector& Position, const Fvector& Rotation);
	static void SpawnPose(Fvector& Position, Fvector& Rotation);
	static bool StaticRay(const Fvector& Origin, const Fvector& Direction, float Range, Fvector& Point, Fvector& Normal);
	void Refit(Player& Core, const Object& Target);
	void RemoveBlock(Player& Core, u32 Id, s32 Block, u8 Layer = 0);
	void AddLayer(Player& Core, u32 Id);
	void RemoveLayer(Player& Core, u32 Id, u8 Layer);
	void SetRowMode(Player& Core, u32 Id, u8 Row, bool IsAdd);
	void AddAnimationAt(Player& Core, u32 Id, u8 Row, float Time, const shared_str& Source, const shared_str& Name);
	bool PartsCombo(Player& Core, const Object& Target, u8 Layer, float Width);
	void AddFolder(Player& Core);
	void AddCameraMove(Player& Core, float Time, const PoseKey* Pose = nullptr);
	void AddShake(Player& Core, float Time, s32 Row);
	void PinShakes(const Object& Rig);
	void FollowShakes(Object& Rig, s32 Stretched = -1, float Factor = 1.0f);
	void Crossfade(Player& Core, s32 Block, bool IsOn);
	bool InsertShots(Player& Core, float Time, xr_vector<CameraBlock>& Shots, bool ShouldFit);
	bool IsClipFree(const CameraData& Data, s32 Skip, float Start, float Length);
	float FitClip(const CameraData& Data, s32 Skip, float Start, float End, u8 Edge);
	void TrimHead(CameraBlock& Shot, const CameraBlock& Base, float Delta) const;
	s32 SortShot(Object& Rig, s32 Block);
	void CaptureStarts(Scene& Data, bool IsLength);
	void AddMarker(Player& Core, float Time);
	void RemoveMarker(Player& Core, s32 Index);
	void ShiftTime(Player& Core, float At, float Amount);
	bool Copy(Player& Core, Clipboard& Out);
	void Paste(Player& Core, const Clipboard& Source, float Time);
	void PasteObjects(Player& Core, const Clipboard& Source, float Time);
	void PasteBlocks(Player& Core, const Clipboard& Source, u32 OnlyId);
	void PasteKeys(Player& Core, const Clipboard& Source, float Time, bool IsSameOwners);
	void Relink(Scene& Data, u32 Owner, const Attachment& Wanted, Attachment& Out, bool IsSameScene);
	void Duplicate(Player& Core);
	void KeyFromView(Player& Core, float Time, bool IsEye);
	void ViewPose(const Player& Core, bool IsEye, PoseKey& Pose) const;
	const BlockCache* PoseAt(const Player& Core, const CameraBlock& Shot, s32 Block, float Local, PoseKey& Pose, u32& At) const;
	void PutPose(CameraBlock& Shot, const BlockCache& Cache, float Local, float HalfFrame, PoseKey Pose);
	static void ToSpace(const LinkFrames& Frames, PoseKey& Pose);
	static u32 PoseKeyAt(const CameraBlock& Shot, const BlockCache& Cache, float Local, float HalfFrame);
	void RemovePose(Player& Core, s32 Block, u32 Index);
	static void UseGameFov(Object& Target, s32 Block, Param Id);
	void SetMuted(Player& Core, u32 Id, bool IsMuted);
	void ResetLength(Player& Core);
	void DeleteSelection(Player& Core);
	void Select(u32 Id, bool IsCtrl, bool IsShift);
	bool IsSelected(u32 Id) const;
	void PruneSelection(const Scene& Data);
	void StartRename(const Scene& Data, u32 Id, bool IsFolder);
	void CommitRename(Player& Core);
	static Folder* FindFolder(Scene& Data, u32 Id);
	static u32 NextRow(const Scene& Data, u32 FolderId);

	void DrawTimeline(Player& Core, bool& IsOpen);
	void DrawTransport(Player& Core);
	void BuildRows(const Scene& Data);
	void DrawRuler(Player& Core, const ImRect& Rect);
	void DrawMarkers(const Player& Core, const ImRect& Track, bool IsRuler) const;
	s32 MarkerAt(const SceneHeader& Header, float X) const;
	void DrawRowHeader(Player& Core, const RowItem& Row, const ImRect& Rect);
	void HandleTrack(Player& Core, const Object* Target);
	void DrawClip(const Player& Core, const Object& Target, const ImRect& Rect);
	void DrawActorBlocks(const Object& Target, const ActorRuntime& Runtime, const ImRect& Rect, u8 Layer = 0);
	s32 BlockHit(const Object& Target, const ActorRuntime& Runtime, u8 Layer, const ImVec2& Mouse, float Top, DragKind& Kind, s32& Hit, double& Rep) const;
	void PressBlock(Player& Core, const Object& Target, const ActorRuntime& Runtime, u8 Layer, DragKind Kind, s32 Hit, double Rep);
	void DrawLayerRow(Player& Core, const Object& Target, u8 Layer, const ImRect& Header, const ImRect& Track);
	u8 LayerAt(const Scene& Data, const Object& Target, float Y) const;
	void DrawCameraBlocks(const Player& Core, const Object& Target, const ImRect& Rect);
	u8 SubRows(const Object& Target) const;
	void DrawSubRow(Player& Core, const Object& Target, u8 Sub, const ImRect& Header, const ImRect& Track);
	void DrawShakeRow(Player& Core, const Object& Rig, u8 Row, const ImRect& Header, const ImRect& Track);
	void DragShake(Player& Core);
	void DrawShakeMenu(Player& Core);
	s32 CameraHit(const Player& Core, const Object& Target, const ImVec2& Mouse, float Top, float Bottom, DragKind& Kind, s32& Pose) const;
	void DragCameraBlock(Player& Core, const Object& Target);
	bool CutShot(Player& Core, s32 Block, float From, float To, bool ShouldApply);
	void DrawPoseMenu(Player& Core);
	void DrawTimelinePopups(Player& Core);
	void Press(Player& Core, DragKind Kind, u32 Id, bool IsFolder);
	void OpenRowMenu(u32 Id, bool IsFolder);
	void UpdateDrag(Player& Core);
	void StartBox(DragKind Kind);
	void UpdateBox(Player& Core);
	void StartClipDrag(Player& Core);
	void ShiftClips(Player& Core, float Offset);
	void DropRow(Player& Core);
	s32 SpanAt(const Object& Target, const ActorRuntime& Runtime, float Local, double& Rep, float& Sequence, u8 Layer = 0) const;
	float SequenceToX(const Object& Target, const ActorRuntime& Runtime, double Rep, float Sequence) const;
	double AreaAt(const Object& Target, const ActorRuntime& Runtime, float Time) const;
	float TimeAt(const Object& Target, const ActorRuntime& Runtime, double Area) const;
	void PlaceBlocks(const Object& Target, const ActorRuntime& Runtime, xr_vector<ActorBlock>& Blocks, const xr_vector<float>& Starts) const;
	bool IsTrackValid(const Object& Target, const ActorRuntime& Runtime, const xr_vector<ActorBlock>& Blocks) const;
	bool RemoveCrossfade(Player& Core, u32 Id, u8 Layer, s32 Block, bool ShouldApply);
	u8 PlaceRow(Object& Item, const ActorRuntime& Runtime, u8 Row, const ActorBlock& Block, bool IsStrict, bool IsSearching);
	void DragBlockEdge(Player& Core);
	void DragActorBlock(Player& Core, const Object& Target, const ActorRuntime& Runtime);
	bool SplitBlock(Player& Core, u32 Id, s32 Block, bool ShouldApply, u8 Layer = 0);
	bool CutSequence(Player& Core, u32 Id, float From, float To, bool ShouldApply, u8 Layer = 0, s32 Only = -1);
	void CollectSnapTargets(const Player& Core, bool HasPlayhead);
	bool Magnet(float Time, float& Offset) const;
	float SnapTime(float Time, bool IsShift) const;
	float SnapSpan(float Start, float Length, bool IsShift) const;
	float TimeToX(float Time) const;
	float XToTime(float X) const;
	float LanesHeight(const Scene& Data, u32 Id) const;
	float SizeOf(const Scene& Data, u32 ObjectId, u8 Kind, Param Id, u8 Layer) const;
	void SizeEdge(Player& Core, const RowSize& Key, float Left, float Right, float Top, float Bottom);
	void SetTangentKey(const Player& Core, s32 Block, s32 Key);
	u8 SubAt(const Scene& Data, const Object& Target, float Y) const;
	LaneSlot* FindSlot(u32 ObjectId, Param Id, u8 Layer);
	void CollectLaneTargets(const Object& Target);
	void DrawLanes(Player& Core, const Object& Target, float Left, float Top);
	void DrawLaneHeader(Player& Core, const Object& Target, LaneSlot& Slot, s32 Component, const ImRect& Rect, bool& ShouldRemove);
	void DrawLane(Player& Core, const Object& Target, LaneSlot& Slot, s32 Component, const ImRect& Rect);
	void DrawPoseLane(Player& Core, const Object& Rig, const LaneSlot& Slot, s32 Component, const ImRect& Rect);
	void DrawKeyTicks(ImDrawList* List, const xr_vector<Lane>& Lanes, float Start, float Y, u32 Color) const;
	void SetLooping(Player& Core, bool IsOn);
	void LoopSelection(Player& Core);
	void FitLane(const Object& Target, LaneSlot& Slot);
	void AddLaneKey(Player& Core, const Object& Target, const LaneSlot& Slot, s32 Component, float Time, float NewValue);
	void GroupKeys(const Object& Target, s32 Block, const LaneSlot& Slot, s32 Component, const KeyRef& Hit, float HalfFrame);
	void SelectKeys(bool IsToggle);
	bool IsKeySelected(const KeyRef& Ref) const;
	void DragLane(Player& Core);
	void BeginKeyMove(Player& Core, const char* Label);
	void MoveKeys(Player& Core, float Shift, float Rise);
	void DrawKeyMenu(Player& Core);
	void RemoveKeys(Player& Core);
	template <typename TFunc>
	void ForSelectedKeys(Player& Core, const char* Label, const TFunc& Visit);
	static void RemoveLane(Object& Target, s32 Block, Param Id, u8 Layer, float Local);

	void DrawInspector(Player& Core);
	void DrawPresets();
	void DrawHeader(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void DrawSceneSection(Player& Core);
	void DrawActorSection(Player& Core, const xr_vector<u32>& Ids);
	void DrawBlockSection(Player& Core, const Object& Target);
	void DrawLightSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void DrawParticleSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void ClipRow(Player& Core, const xr_vector<u32>& Ids);
	bool GroupHeader(Player& Core, const xr_vector<u32>& Ids, const char* Title, const char* Label, bool LightData::*Field);
	void PickColor(Player& Core);
	void PickFocus(Player& Core);
	void DrawCameraSection(Player& Core, const xr_vector<u32>& Ids, bool HasPin);
	void DrawShake(Player& Core, const xr_vector<u32>& Ids, s32 Owner);
	void PoseRow(Player& Core, const xr_vector<u32>& Ids, s32 Block, Param Id);
	void ParamRow(Player& Core, const xr_vector<u32>& Ids, s32 Block, Param Id, u8 Layer, float Speed, const char* Format, u8 ResetMask, float Default);
	void KeyRow(Player& Core, const xr_vector<u32>& Ids);
	void DrawAttach(Player& Core, const xr_vector<u32>& Ids, s32 Block);
	bool AttachCombo(const Player& Core, const char* Label, u32 Owner, AttachLink& Link);
	bool BoneCombo(const Player& Core, const char* Label, AttachLink& Link);
	bool CanAttach(const Scene& Data, u32 From, u32 To);
	void DrawGizmoRow(bool HasScale, bool IsRange = false, bool HasRotate = true);
	RowAction BeginRow(const char* Name, bool IsOverridden, bool HasLane);
	bool EndRow(KeyState Mark);
	bool TimeField(const char* Label, float& Seconds, float Lo, float Hi, u32 Rate);
	bool OffsetField(const char* Label, float& Seconds, u32 Rate);
	s32 FocusedBlock(const Player& Core, const Object& Target, u8* Layer = nullptr) const;
	void BuildLibrary();
	bool Chooser(Player& Core, ChooserKind Kind, const char* Label, const char* Preview, const xr_vector<u32>& Owners, shared_str& Source, shared_str& Name);
	bool ChooserList(Player& Core, ChooserKind Kind, const xr_vector<u32>& Owners, shared_str& Source, shared_str& Name);
	void ToggleKey(Player& Core, const xr_vector<u32>& Ids, s32 Block, Param Id, u8 Layer, bool IsAdding = false);
	void KeyPlayhead(Player& Core);
	void HoverLane(u32 ObjectId, const LaneSlot& Slot);


	void WriteParam(Player& Core, Object& Target, s32 Block, Param Id, u8 Layer, u8 Component, float NewValue, bool IsTotal = false);
	void Record(Player& Core, WriteLatch& Latch);

	History Undo;
	u64 SavedSerial = 0;
	EditorSettings Settings, SavedSettings;
	u32 SeenGeneration = 0;
	xr_string SceneName;
	xr_string SceneFile;
	ModalKind Modal = ModalKind::None;
	PendingKind Pending = PendingKind::None;
	char NameBuffer[256] = {};
	xr_string NameFile;
	xr_string NameError;
	bool IsNameTaken = false;
	xr_vector<xr_string> FileList;
	s32 FileSelected = -1;
	bool HasUnsavedFile = false;
	xr_string MessageTitle;
	xr_vector<xr_string> MessageLines;
	xr_string StatusText;
	bool IsStatusError = false;
	bool IsOwnWindowFocused = false;

	xr_vector<u32> Selected;
	u32 Anchor = 0, SelectedFolder = 0;
	u32 RenameId = 0;
	bool IsRenameFolder = false;
	u8 RenameFrames = 0;
	char RenameBuffer[MaxNameBytes + 1] = {};

	float Zoom = 60.0f, ScrollTime = 0.0f;
	ImRect Marquee;
	double AddedKeyTime = -1.0;
	xr_vector<RowSize> SizesBefore;
	float SizeGrab = 0.0f;
	bool WasLayoutDirty = false, IsSizeCancelled = false;
	float SizeTop = 0.0f;
	bool IsLayoutDirty = false;
	float TrackLeft = 0.0f, TrackWidth = 1.0f, RowsTop = 0.0f, RowHeight = 1.0f, SnapGrid = 1.0f / 30.0f;
	xr_vector<RowItem> Rows, RowScratch, MemberScratch;
	xr_vector<u32> Collapsed;
	DragKind Drag = DragKind::None;
	float DragTime = 0.0f;
	u32 DragObject = 0;
	bool IsDragFolder = false, IsDragCopy = false, HasDragMoved = false, WasPressSelected = false, IsDragFlat = false;
	s32 DragPoseAxis = -1;
	bool IsPoseRetimed = false;
	float DragLoop[2] = {};
	xr_vector<u32> DragIds, DragPrevious;
	xr_vector<float> DragStarts, SnapTargets;
	u32 PopupRow = 0;
	bool IsPopupFolder = false;
	float PopupTime = 0.0f;
	const char* PopupRequest = nullptr;
	s32 DragBlock = -1, PopupBlock = -1, DragPose = -1, PopupPose = -1, HoverBlock = -1, HoverPose = -1;
	u32 HoverLaneObject = 0;
	Param HoverLaneId = Param::Count;
	u8 HoverLaneLayer = 0;
	xr_vector<u32> KeyIds;
	float DragSequence = 0.0f, DragSign = 1.0f;
	bool IsDragSpeed = false;
	xr_vector<ActorBlock> DragBlocks[MaxActorLayers + 1], CutBlocks;
	xr_vector<float> DragBlockStarts[MaxActorLayers + 1], BlockStarts;
	u8 DragLayer = 0, PopupLayer = 0, FocusLayer = 0, DragRows = 1, DragValidLayer = 0;
	float DragValidStart = 0.0f;
	s8 UndoRequest = 0;
	CameraBlock DragShot;
	Spline DragPath;
	Curves::AreaTable DragArea;
	xr_vector<float> DragClipStarts;
	float DragClipSpan = 0.0f;
	xr_vector<Cameras::ClipSpan> SpanScratch;
	xr_vector<s32> InsertedShots;
	ShakeClip DragShakeStart;
	xr_vector<ShakePin> Pins;
	xr_vector<u8> ShakeMoved;
	s32 DragShakeIndex = -1;
	u8 DragRow = 0, PopupShakeRow = 0;
	bool IsShakeCopied = false, IsShakeSlide = false, IsPopupCross = false, IsShotCopied = false;

	float LastBlend = 0.5f;
	bool IsPopupChoosing = false;
	xr_vector<u32> Expanded;
	xr_vector<LaneSlot> Slots;
	xr_vector<LaneTarget> LaneTargets;
	xr_vector<KeyRef> SelectedKeys, KeyGroup, DragKeyPrevious;
	xr_vector<LaneDrag> DragCurves;
	xr_vector<KeyMove> DragMoves;
	KeyRef DragKey;
	Key DragKeyStart;
	float DragKeyTime = 0.0f, DragOriginY = 0.0f, DragLaneHeight = 1.0f, DragSpan = 1.0f;
	s32 DragMarker = -1, HoverMarker = -1, PopupMarker = -1, RenameMarker = -1;
	ImGuiID CanvasId = 0;
	Clipboard Copied, Scratch;
	xr_vector<std::pair<u32, u32>> IdMap;
	float OffsetValue = 0.0f;
	bool IsOffsetKeys = false;
	float ShiftAmount = 1.0f;
	bool IsRemovingTime = false;
	xr_vector<ActorPreset> Presets;
	xr_vector<shared_str> PresetNames;
	xr_vector<xr_string> HiddenPresets, OldPresetNames;
	xr_vector<u32> PresetRows;
	ActorPreset PresetDraft;
	bool IsPresetsLoaded = false, IsPresetsReadOnly = false;
	s32 PresetSelected = -1, RenamePreset = -1;
	u8 PresetFrames = 0;
	char PresetBuffer[MaxNameBytes + 1] = {}, PresetFilter[128] = {};
	ChooserKind PopupKind = ChooserKind::Model;

	xr_vector<u32> PinnedIds, InspectActors, InspectCameras, InspectLights, InspectParticles, ChooserOwners, PickColorIds;
	bool IsPinned = false, IsAutoKey = false, IsPickingColor = false, IsPickingFocus = false;
	s32 PickFocusBlock = -1;
	xr_vector<WriteLatch> Latches;
	AttachGraph AttachCheck;
	xr_vector<const Object*> AttachChoices;
	char BoneFilter[64] = {};
	u32 HighlightObject = 0;
	shared_str HighlightBone;
	u32 FocusObject = 0;
	s32 FocusBlock = -1;
	FocusKind Focus = FocusKind::Objects;
	float RowKeyX = 0.0f, RowWidth = 0.0f;
	char NameEdit[MaxNameBytes + 1] = {};

	xr_vector<shared_str> ModelFiles, ItemSections, OmfFiles, EffectNames, TextureFiles;
	shared_str MenuPack;
	xr_vector<u32> Filtered;
	xr_vector<u64> Ranked;
	char Filters[7][128] = {};
	s32 ChooserCursor = 0;
	ChooserKind FilteredKind = ChooserKind::Model;
	u32 FilteredObject = 0;
	size_t FilteredSources = 0;
	bool IsLibraryBuilt = false, IsTexturesBuilt = false, IsFilterDirty = true, IsBrowsingPacks = false, IsChooserOpen = false, IsChooserFresh = false;

	bool IsEditorCamera = false, IsEyeValid = false, IsFlying = false, IsFlyArmed = false, IsPanning = false;
	Fvector Eye = Fvector().set(0.0f, 0.0f, 0.0f);
	float Yaw = 0.0f, Pitch = 0.0f, Roll = 0.0f, EyeFov = 0.0f, FlyFov = 0.0f;


	shared_str EyeLevel;


	GizmoMode Gizmo = GizmoMode::Universal, GizmoShown = GizmoMode::Universal;
	u32 GizmoPart = 0;
	ImDrawList* Overlay = nullptr;
	bool IsGizmoLocalSingle = true, IsGizmoLocalMulti = false, IsGizmoActive = false, IsGizmoCancelled = false, IsGizmoHovered = false, IsGizmoRelative = false;
	bool IsPickArmed = false, ShouldOpenPopup = false, IsPopupAligned = false, HasPopupHit = false;
	Fmatrix GizmoPivot = Fidentity, GizmoInverse = Fidentity;
	xr_vector<GizmoItem> GizmoItems;
	Fvector RangeCentre = Fvector().set(0.0f, 0.0f, 0.0f), RangeAxis = Fvector().set(1.0f, 0.0f, 0.0f);
	float RangeStart = 0.0f;
	s32 TangentBlock = -1, TangentKey = -1, DragTangentKey = -1;
	u32 TangentCount = 0;
	float TangentTime = 0.0f;
	u8 DragTangentSide = 0;
	bool IsTangentActive = false, IsTangentCancelled = false;
	PoseKey TangentBase;
	Fvector TangentIn = Fvector().set(0.0f, 0.0f, 0.0f), TangentOut = Fvector().set(0.0f, 0.0f, 0.0f), TangentStart = Fvector().set(0.0f, 0.0f, 0.0f), TangentPlane = Fvector().set(0.0f, 0.0f, 0.0f);
	Fvector PopupPoint = Fvector().set(0.0f, 0.0f, 0.0f), PopupNormal = Fvector().set(0.0f, 1.0f, 0.0f);
	ImRect CubeRect;
	float ViewRight = FLT_MAX;
};
}
