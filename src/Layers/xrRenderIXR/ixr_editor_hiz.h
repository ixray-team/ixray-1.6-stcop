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

namespace nri
{
struct Texture;
struct Descriptor;
struct Buffer;
struct Pipeline;
struct PipelineLayout;
struct DescriptorPool;
struct DescriptorSet;
} // namespace nri

// Occlusion culling of the editors (no HOM, sectors or portals): the scene depth of a frame is
// reduced on the GPU to a grid of farthest depths, read back without stalling a few frames
// later, and boxes are tested against it on the CPU with the camera of that frame.

class CEditorHiZ
{
public:
	CEditorHiZ() = default;
	CEditorHiZ(const CEditorHiZ&) = delete;
	CEditorHiZ& operator=(const CEditorHiZ&) = delete;
	~CEditorHiZ();

	void Destroy();
	void Build(GfxCmdList& Cmd);
	void Fetch();
	bool IsVisible(const Fbox& Box) const;

	bool IsUsable() const
	{
		return Usable;
	}

private:
	struct SReadback
	{
		nri::Buffer* Buffer = nullptr;
		nri::DescriptorSet* Set = nullptr;
		u32 Frame = 0;
		Fmatrix ViewProject;
		Fvector CameraPosition;
		Fvector CameraDirection;
		bool Pending = false;
	};

	bool CreateResources(u32 Width, u32 Height);
	void ReleaseResources();
	void BuildMips();

	static constexpr u32 GridWidth = 256;
	// One a frame, read when the ring comes back to it: mapping a copy the GPU has not reached makes D3D11 wait
	static constexpr u32 ReadbackCount = 8;

	nri::Texture* Grid = nullptr;
	nri::Descriptor* GridView = nullptr;
	nri::PipelineLayout* Layout = nullptr;
	nri::Pipeline* Pipeline = nullptr;
	nri::DescriptorPool* Pool = nullptr;
	SReadback Readbacks[ReadbackCount];
	bool IsGridWritten = false;

	u32 ViewWidth = 0;
	u32 ViewHeight = 0;
	u32 GridHeight = 0;

	ref_cs Shader;

	struct SLevel
	{
		u32 Width = 0;
		u32 Height = 0;
		xr_vector<float> Depth;
	};
	xr_vector<SLevel> Levels;

	bool HasData = false;
	bool Usable = false;
	Fmatrix ViewProject;
	Fvector CameraPosition;
	Fvector CameraDirection;
};
