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

class light;
class GfxCmdList;

namespace nri
{
struct Buffer;
struct Descriptor;
}

// Which lamps the frame can do without. A lamp whose whole reach is behind what the main view drew lights nothing the
// eye sees: the GPU tests each lamp's sphere against the depth pyramid of the frame before, and the answer is read
// frames later, when the device has long finished it. A lamp no answer names is taken as seen
class LampSelection
{
public:
	// The frame's lamps, tested where the lamp maps are drawn
	void Mark(GfxCmdList& Cmd, const xr_vector<light*>& Lamps);
	// The oldest answer, for the lamps the frame collects
	void Read(const xr_vector<light*>& Lamps);
	// Of the lamps Read was given, by their place among them
	bool IsHidden(u32 Lamp) const { return Lamp < Hidden.size() && Hidden[Lamp]; }
	// After the scene's unload
	void Destroy();

private:
	static constexpr u32 Frames = 8;
	static constexpr u32 Most = 64;
	// A word a lamp, 1 when it is seen, and the frame that asked after them
	static constexpr u64 Bytes = (Most + 1) * sizeof(u32);

	void Create();

	nri::Buffer* Seen = nullptr;
	nri::Descriptor* SeenView = nullptr;
	nri::Buffer* Copies[Frames] = {};
	bool IsNew = true;
	ref_cs Shader;
	xr_vector<const light*> Lists[Frames];
	u32 Stamps[Frames] = {};
	xr_vector<bool> Hidden;
};
