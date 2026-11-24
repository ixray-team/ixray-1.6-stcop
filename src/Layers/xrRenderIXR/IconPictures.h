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

// The pictures of the 3D icons the interface's icon target holds, each where it was drawn: a picture stays while its
// icon's key does and nothing is drawn over its place
class IconPictures
{
public:
	static constexpr u32 Capacity = 256;

	// Whether the picture at Place is of Key; one that is is marked used in Frame
	bool IsKept(const Irect& Place, u32 Key, u32 Frame);
	// An icon drawn at Place: the pictures it overlaps are lost, its own is kept when IsWhole, the least lately used giving
	// way when all places are taken. Returns the pictures lost, not counting the one at Place
	u32 Drawn(const Irect& Place, u32 Key, u32 Frame, bool IsWhole);
	void Clear() { Count = 0; }
	u32 Size() const { return Count; }

private:
	struct Picture
	{
		Irect Place;
		u32 Key;
		u32 Frame;
	};

	Picture Pictures[Capacity];
	u32 Count = 0;
};
