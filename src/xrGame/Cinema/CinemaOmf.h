#pragma once

class IKinematics;
class IKinematicsAnimated;

namespace Cinema
{
struct AnimClip;

struct AnimationClip
{
	shared_str Name;
	bool IsFx = false;
};

struct AnimationSource
{
	shared_str Id;
	xr_vector<AnimationClip> Clips;
};
}

namespace Cinema::Omf
{
inline constexpr float MotionRate = 30.0f;
inline constexpr u32 MaxMotionFrames = 18000;
inline constexpr u32 ClipChunk = 0x00C1E001, ClipVersion = 3;
inline constexpr const char* ClipFolder = "cinema\\";
bool IsOmfPath(const shared_str& Path);
bool IsClipName(const char* Name);
void WriteClip(const AnimClip& Clip, CMemoryWriter& Output);
bool ReadClip(const char* Name, AnimClip& Clip);
bool VisualExists(const char* Visual);
bool CheckVisual(const char* Visual, xr_string& Error);
bool IndexAnimationFile(const char* Path, AnimationSource& Source);
void WriteMotion(const char* Name, const xr_vector<shared_str>& Bones, const xr_vector<Fquaternion>& Turns, const xr_vector<Fvector>& Shifts, CMemoryWriter& Output);
bool BuildAdaptedMotion(IKinematics* Skeleton, IKinematicsAnimated* Animated, const char* SourcePath, CMemoryWriter& Output, xr_string& Error);
}
