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
	const LightRuntime* FindLight(u32 ObjectId) const;
	const ParticleRuntime* FindParticle(u32 ObjectId) const;
	const BlockCache* FindCameraBlock(u32 Block) const;
	const ObjectState* FindState(u32 ObjectId) const;
	ViewSource View() const { return CurrentView; }
	bool IsSilent(const Object& Item) const { return Item.IsMuted || (!Soloed.empty() && std::find(Soloed.begin(), Soloed.end(), Item.Id) == Soloed.end()); }
	void EndLevel();
	void RestoreDofSwitch();
	Scene Data;
	xr_vector<u32> Soloed;
	float Time = 0.0f, PlayStart = 0.0f, Speed = 1.0f;
	bool IsPlaying = false, IsLooping = false, HasLoopRange = false, IsLookingThrough = true;
	float LoopStart = 0.0f, LoopEnd = 0.0f;
	bool HasUnsavedChanges = false;
	bool IsDofPreview = true;
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
	void PoseActor(u32 Index, float At, bool IsNeeded);
	void PoseCamera(u32 Index, float At);
	void PoseLight(u32 Index, float At, bool IsDrawn);
	void PoseParticle(u32 Index, float At, bool IsDrawn);
	float PoseTransform(u32 Index, float At, const Fvector& Position, const Fvector& Rotation);
	void DropRuntimes();
	void Resolve(const Attachment& Links, bool IsActor, LinkFrames& Out) const;
	bool Frame(const AttachLink& Link, Fmatrix& Out, u8& Problems, u8 BoneProblem) const;
	void OutputCamera();
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
	xr_vector<u32> RuntimeIds;
	float Played = 0.0f;
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
	CameraPose EditorPose;
	u32 RetargetIndex = 0;
};

Player* FindPlayer();
Player& EnsurePlayer();
}
