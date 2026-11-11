#pragma once
#include "CinemaActorRuntime.h"
#include "CinemaCamera.h"
#include "CinemaEffects.h"

namespace Cinema
{
enum class ViewSource : u8
{
	None,
	Editor,
	Global,
	Busy
};

struct ObjectState
{
	Fmatrix World = Fidentity;
	LinkFrames Frames;
	bool IsReady = false;
};

class Player final : public pureFrame
{
public:
	Player();
	~Player();
	void OnFrame() override;
	void Replace(Scene&& Incoming);
	void Play();
	void Pause();
	void Stop();
	void Seek(float Target);
	void SetEditorView(bool IsOpen, bool IsCamera, const Fvector& Position, const Fmatrix& Orientation, float Fov = 0.0f);

	void SetAudition(u32 ObjectId, const shared_str& Source, const shared_str& Name, const shared_str& Visual = shared_str());
	void RequestFit(u32 ObjectId);
	void RequestAttach(u32 ObjectId, s32 Block, const Attachment& NewAttach);
	bool HasRequests() const { return !FitRequests.empty() || !AttachRequests.empty(); }
	const ActorRuntime* FindActor(u32 ObjectId) const;
	void PreviewClip(u32 ObjectId, s32 Clip, float Time);
	bool BakeClip(u32 ObjectId, s32 Clip, xr_vector<Fquaternion>& Turns, xr_vector<Fvector>& Shifts);
	u32 BakeSource(u32 ObjectId, const shared_str& Source, const shared_str& Name, xr_vector<Fquaternion>& Turns, xr_vector<Fvector>& Shifts);
	const LightRuntime* FindLight(u32 ObjectId) const;
	const ParticleRuntime* FindParticle(u32 ObjectId) const;
	const DecalRuntime* FindDecal(u32 ObjectId) const;
	const SoundRuntime* FindSound(u32 ObjectId) const;
	const BlockCache* FindCameraBlock(u32 Block) const;
	const ObjectState* FindState(u32 ObjectId) const;
	ViewSource View() const { return CurrentView; }
	bool IsCutBetween(float From, float To) const;
	bool IsSilent(const Object& Item) const { return Item.IsMuted || (!Soloed.empty() && std::find(Soloed.begin(), Soloed.end(), Item.Id) == Soloed.end()); }
	void EndLevel();
	void RestoreDofSwitch();
	void GhostBones(u32 ObjectId, float At, xr_vector<Fvector>& Lines);
	bool BakeMotion(u32 ObjectId, float From, u32 Frames, bool IsPoseOnly, xr_vector<Fquaternion>& Turns, xr_vector<Fvector>& Shifts);
	bool PathPoint(u32 ObjectId, float At, Fvector& Point) const;
	const xr_vector<shared_str>& Shaders();
	bool LinkFrame(const AttachLink& Link, Fmatrix& Out) const { u8 Problems = 0; return Frame(Link, Out, Problems, 0); }
	Scene Data;
	xr_vector<u32> Soloed;
	float Time = 0.0f, PlayStart = 0.0f, Speed = 1.0f;
	bool IsPlaying = false, IsLooping = false, HasLoopRange = false, IsLookingThrough = true;
	float LoopStart = 0.0f, LoopEnd = 0.0f;
	bool HasUnsavedChanges = false;
	bool IsDofPreview = true;
	bool IsEffectPreview = false;
	u32 Generation = 0;

private:
	struct LateFrame final : public pureFrame
	{
		Player* Owner = nullptr;
		void OnFrame() override { Owner->FlushCamera(); }
	};
	struct LinkEntry
	{
		u32 Id = 0;
		Attachment Links;
	};
	struct AttachRequest
	{
		u32 ObjectId = 0;
		s32 Block = -1;
		Attachment Links;
	};
	void ResetLevel();
	void SaveUnsaved();
	void Advance();
	void Sync();
	void Fit(Object& Target, ActorRuntime& Runtime, float Step);
	void Absolutize(Object& Target, ActorRuntime& Runtime, float Step);
	void RefreshLinks();
	void Convert(const AttachRequest& Request);
	void EvaluateObjects(float At, const u8* Mask);
	void EvaluateLinked(u32 Index, float At);
	void PoseActor(u32 Index, float At, bool IsNeeded);
	void PoseCamera(u32 Index, float At);
	void PoseLight(u32 Index, float At, bool IsDrawn);
	void PoseParticle(u32 Index, float At, bool IsDrawn);
	void PoseDecal(u32 Index, float At, bool IsDrawn);
	void PoseSound(u32 Index, float At, bool IsDrawn);
	void Stamp(DecalRuntime& Runtime, const Fmatrix& World, float Size);
	float PoseTransform(u32 Index, float At, const Fvector& Position, const Fvector& Rotation);
	void DropRuntimes();
	void Resolve(const Attachment& Links, bool IsActor, LinkFrames& Out, float Local) const;
	void Unhook(const Attachment& Links, const xr_vector<Lane>& Lanes, float Start, float Local, LinkFrames& Out);
	bool FrameAt(const AttachLink& Link, float At, Fmatrix& Out);
	bool Frame(const AttachLink& Link, Fmatrix& Out, u8& Problems, u8 BoneProblem) const;
	void OutputCamera();
	void OutputWeather();
	void OutputPost();
	bool IsCut(const CameraPose& From, s32 FromBlock, const CameraPose& To, s32 ToBlock) const;
	void OutputDof(const CameraSample& Lens);
	void FlushCamera();
	void Release();
	ActorRuntime& ActorAt(u32 ObjectId);
	const Audition* Heard(u32 ObjectId) const { return !IsPlaying && Listen.ObjectId == ObjectId ? &Listen : nullptr; }
	Audition Listen;
	IGame_Level* TrackedLevel = nullptr;
	bool WasLevelReady = false;
	bool ShouldDropRuntimes = false;
	xr_vector<xr_unique_ptr<ActorRuntime>> Actors;
	xr_vector<xr_unique_ptr<LightRuntime>> Lights;
	xr_vector<xr_unique_ptr<ParticleRuntime>> Emitters;
	xr_vector<xr_unique_ptr<DecalRuntime>> Decals;
	xr_vector<xr_unique_ptr<SoundRuntime>> Voices;
	xr_vector<u32> RuntimeIds;
	float Played = 0.0f;
	float EvaluationTime = 0.0f;
	xr_vector<u32> FitRequests;
	xr_vector<AttachRequest> AttachRequests;
	xr_vector<BlockCache> CameraCache;
	xr_vector<Curves::AreaTable> ShakeAreas;
	xr_vector<s16> ShakeOwners;
	AttachGraph Graph;
	xr_vector<LinkEntry> LinkCache;
	xr_vector<ObjectState> States;
	xr_vector<u8> Needed;
	xr_vector<float> Times;
	xr_vector<Fvector> Converted;
	xr_vector<ActorBlock> Built;
	xr_vector<float> BuiltStarts;
	CameraSample Shot;
	LateFrame Late;
	xr_shared_ptr<CameraState> Camera;
	CEffectorCam* Effector = nullptr;
	ViewSource CurrentView = ViewSource::None;
	bool IsEditorOpen = false, IsEditorCamera = false, IsDofForced = false;
	CameraPose EditorPose, LastPose;
	s32 LastBlock = -1;
	bool HasLastPose = false;
	u32 RetargetIndex = 0;
	xr_vector<shared_str> ShaderNames;
	bool IsShadersRead = false;
};

Player* FindPlayer();
Player& EnsurePlayer();
}
