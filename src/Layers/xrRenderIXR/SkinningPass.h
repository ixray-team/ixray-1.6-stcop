/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#pragma once
#include "RenderAPI.h"
#include "SH_Atomic.h"

class CSkeletonX;
class CKinematics;
class GfxCmdList;

namespace nri
{
struct Buffer;
struct Descriptor;
} // namespace nri

// Calculates the bones of the skeletons on the workers; their update callbacks run after, on this thread, in order. Drops
// from the list those already posed or held this frame
void PoseSkeletons(xr_vector<CKinematics*>& Skeletons);

class RENDER_API SkinningPass
{
public:
	void Destroy();
	void BeginFrame();
	void Bind(GfxCmdList& Cmd);
	void Request(CSkeletonX& Mesh, const Fmatrix& World);
	void Flush(GfxCmdList& Cmd);
	void Snapshot(CKinematics& Kinematics, const Fmatrix& World);

	// With the frames one behind the game (r_render_thread pipelined) the game animates its next frame while this one
	// is drawn: the pose a frame draws is taken with its collect. Mark names a skeleton the collect met, Capture poses
	// those and keeps their bones' render transforms (this frame's and the last one's) and old world matrix, and the
	// draw reads them here. One the collect did not take is posed and read when it is drawn, as with the mode off
	struct Pose
	{
		const Fmatrix* Bones;
		Fmatrix OldWorld;
	};

	void Mark(CKinematics* Kinematics, const Fmatrix& World);
	void Capture();
	bool Find(const CKinematics& Kinematics, Pose& Out) const;
	const Fmatrix& OldWorld(const CKinematics& Kinematics) const;
	// A rigid part's world matrices from its bone
	void Rigid(CKinematics& Kinematics, const Fmatrix& World, u16 Bone, Fmatrix& Now, Fmatrix& Old);
	u32 GetFrame() const { return Frame; }
	// The skinned vertices as the scene's sets read them: another view once the output grows, null before the first flush
	nri::Descriptor* GetView() const { return OutputRead; }

private:
	struct Item
	{
		CSkeletonX* Mesh;
		Fmatrix World;
	};

	void Release();
	void Grow();

	struct Taken
	{
		u32 First;
		Fmatrix OldWorld;
	};

	xr_vector<Item> Pending;
	xr_vector<std::pair<CKinematics*, Fmatrix>> Marked;
	xr_hash_map<const CKinematics*, Taken> Poses;
	xr_vector<Fmatrix> PoseBones;
	xr_vector<CKinematics*> Parents;
	xr_vector<u32> Records;
	ref_cs Shader;
	// Made through NRI; the old backend's draws read it through Adopted, which wraps the same buffer
	nri::Buffer* Output = nullptr;
	nri::Descriptor* OutputRead = nullptr;
	nri::Descriptor* OutputWrite = nullptr;
	IRHIBuffer* Adopted = nullptr;
	IRHIShaderResourceView* AdoptedView = nullptr;
	bool IsNew = false;
	u32 OutputCapacity = 1u << 20;
	u32 Cursor = 0;
	u32 Frame = 1;
};
