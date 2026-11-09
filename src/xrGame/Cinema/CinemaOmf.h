#pragma once

class IKinematics;
class IKinematicsAnimated;

namespace Cinema
{
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
bool IsOmfPath(const shared_str& Path);
bool VisualExists(const char* Visual);
bool CheckVisual(const char* Visual, xr_string& Error);
bool IndexAnimationFile(const char* Path, AnimationSource& Source);
bool BuildAdaptedMotion(IKinematics* Skeleton, IKinematicsAnimated* Animated, const char* SourcePath, CMemoryWriter& Output, xr_string& Error);
}
