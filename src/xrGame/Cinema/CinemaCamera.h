#pragma once
#include "CinemaCurve.h"
#include "../../xrEngine/Effector.h"

namespace Cinema
{
struct CameraState
{
	bool IsActive = true;
	bool IsAttached = true;
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix Orientation = Fidentity;
	float FovA = 0.0f, FovB = 0.0f, Weight = 1.0f;
};

class CameraEffector final : public CEffectorCam
{
public:
	explicit CameraEffector(xr_shared_ptr<CameraState> Shared);
	~CameraEffector() override;
	bool AbsolutePositioning() override;
	bool ProcessCam(SCamEffectorInfo& Info) override;

private:
	xr_shared_ptr<CameraState> State;
};

class Spline
{
public:
	void Build(const CameraBlock& Block);
	void Evaluate(float Time, Fvector& Position, Fmatrix& Orientation) const;
	bool Empty() const { return Points.empty(); }
	bool Handles(u32 Key, Fvector& In, Fvector& Out) const;

private:
	struct Point
	{
		float Time = 0.0f;
		Interp Mode = Interp::Linear;
		u32 Key = 0;
		double Value[7] = {};
		double Ahead[7] = {};
		double Back[7] = {};
	};
	xr_vector<Point> Points;
};

struct CameraPose
{
	Fvector Position = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix Orientation = Fidentity;
	float Fov = 0.0f;
	Fvector4 Dof = {};
	Fvector4 Lens = {};
	Fvector4 Shape = {};
	Fvector4 Timing = {};
	bool HasDof = false;
	bool IsPhysical = false;
};

struct CameraSample
{
	bool IsActive = false;
	s32 Block = -1;
	CameraPose A;
	CameraPose B;
	float Weight = 1.0f;
	Fvector Shift = Fvector().set(0.0f, 0.0f, 0.0f);
	Fmatrix Turn = Fidentity;
};

inline constexpr u8 MissingParent = 1, MissingParentBone = 2, MissingTarget = 4, MissingTargetBone = 8, MissingFocus = 16, MissingFocusBone = 32;

struct LinkFrames
{
	Fmatrix Parent = Fidentity;
	Fvector Look = Fvector().set(0.0f, 0.0f, 0.0f);
	bool HasParent = false;
	bool HasLook = false;
	bool IsYawOnly = false;
	u8 Problems = 0;
	float Weight = 1.0f;
};

struct BlockCache
{
	xr_vector<PoseKey> Keys;
	Curves::AreaTable TimeScaleArea;
	LinkFrames Frames;
	Fvector FocusPoint = Fvector().set(0.0f, 0.0f, 0.0f);
	bool HasFocus = false;
};
}

namespace Cinema::Spaces
{
void Orthonormalize(Fmatrix& Frame);
void Compose(const LinkFrames& Frames, Fmatrix& Pose);
void Decompose(const LinkFrames& Frames, Fmatrix& Pose);
}

namespace Cinema::Cameras
{
inline constexpr float ShakeAnchor = 1e-3f;

struct ClipSpan
{
	float Start = 0.0f;
	float Length = 0.0f;
};

float BlockStart(const CameraData& Data, u32 Block);
s32 BlockAt(const CameraData& Data, float Local);
s32 ShakeOwner(const CameraData& Data, const ShakeClip& Clip);
bool IsTrackValid(const xr_vector<ClipSpan>& Spans);
float PathTime(const CameraBlock& Block, const BlockCache& Cache, float Local);
float LocalFromPath(const CameraBlock& Block, const BlockCache& Cache, float Path);
float PathTime(const CameraBlock& Block, float Local);
float LocalFromPath(const CameraBlock& Block, float Path);
void EvaluateBlock(const CameraBlock& Block, const BlockCache& Cache, float Local, CameraPose& Pose);
void PathPose(const CameraBlock& Block, float Local, Fvector& Position, Fmatrix& Orientation);
void Rebuild(CameraBlock& Block);
void SetPoseAt(CameraBlock& Block, float Time, const Fvector& Position, const Fvector& Rotation, Interp Mode, float HalfFrame);
void RemovePoseAt(CameraBlock& Block, float Time, float HalfFrame);
void MovePoseAt(CameraBlock& Block, float From, float To, float HalfFrame);
void ModeAt(CameraBlock& Block, float Time, Interp Mode, float HalfFrame);
bool KeysToLanes(CameraBlock& Block);
float FocusDepth(const CameraBlock& Block, const BlockCache& Cache, float Local, const Fvector& Eye, const Fvector& Forward);
CameraPose Mix(const CameraSample& Sample);
bool Sample(const Object& Camera, const xr_vector<BlockCache>& Cache, const xr_vector<Curves::AreaTable>& ShakeAreas, const xr_vector<s16>& ShakeOwners, float Time, CameraSample& Out);
bool IsShakeFree(const CameraData& Data, u32 Skip, u8 Row, float Start, float Length, u32 Until, bool IsStrict = false);
u32 SettleShakes(CameraData& Data, const xr_vector<u8>& Moved = {}, bool IsStrict = false);

}
