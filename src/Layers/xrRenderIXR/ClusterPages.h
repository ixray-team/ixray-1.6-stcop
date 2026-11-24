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

#include "PackedVertex.h"

namespace ClusterPages
{
constexpr float GridScale = 1024.0f;

enum class Baked : u32
{
	None,
	Lightmap,
	Colour,
	Tree,
};

// A cluster's header, HeaderWords words of Store::Attributes before the two words of each of its vertices: the anchor of
// an axis in 24 bits with the bits a vertex takes along it above them, x y z (the kind of baked word above those of x);
// where its positions start; the least UV of its vertices, in the steps of PackedVertex, and how far their offsets
// are shifted down, u v; where its baked words start. Section is what the words
// of the three other sections stand at once the store is one array, see Join
constexpr u32 HeaderWords = 9;
// After a cluster's corners, from the next word, the cluster as a meshlet for what needs a vertex's place alone: the
// vertices that stand in one place taken as one. A byte of how many places, a byte a place of the vertex that stands
// for it, then three bytes a triangle, each a place

struct Cluster
{
	u32 Header;
	u32 FirstCorner; // a word of four corners
	u32 VertexCount;
};

struct Store
{
	xr_vector<u32> Positions;
	xr_vector<u32> Attributes; // a cluster's header, then two words a vertex
	xr_vector<u32> BakedWords;
	xr_vector<u8> Corners;
	xr_vector<Cluster> Clusters;
	u64 Vertices = 0;
	u64 Places = 0;
};

// The store as one array of words: attributes, positions, baked words, corners. returns what a cluster's FirstCorner is
// to be moved by
u32 Join(Store& From, xr_vector<u32>& Words);

// KeepsUv: the vertices of one place stay apart where their UVs differ (what is cut out by its texture)
void Append(Store& Out, const PackedVertex* Vertices, const u16* Corners, u32 TriangleCount, Baked Kind, bool KeepsUv);
} // namespace ClusterPages
