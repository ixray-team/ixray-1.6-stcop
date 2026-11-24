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
#include "stdafx.h"
#include "ixr.h"
#include "PackedVertex.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

// The level's own puddles (level.puddles, r_wet_surfaces) in the forward level frame: every box in view is a quad of water
// as high in the box as the wetness has raised it, as the other frame lays it, and all of them are one batch of the
// vertex ring
void CRender::RenderForwardPuddles(GfxCmdList& Cmd)
{
	if (m_levels_puddles.empty() || !ps_r2_ls_flags_ext.test(R4FLAG_PUDDLES) || !g_pGamePersistent || !Target->s_puddles)
	{
		return;
	}

	const float Wetness = RFrame.Wetness;
	constexpr u32 Most = 64;
	const PuddleBase* Shown[Most];
	u32 Count = 0;

	for (const PuddleBase& Puddle : m_levels_puddles)
	{
		if (Count < Most && Wetness > 0.0f && ViewBase.testSphere_dirty(Puddle.m_world.c, Puddle.m_radius))
		{
			Shown[Count++] = &Puddle;
		}
	}

	if (!Count)
	{
		return;
	}

	GPU_EVENT(ForwardPuddles);

	if (!PuddleGeom)
	{
		PuddleGeom.create(EGeomSource::Ring);
	}

	// The corners in the box's own space, in the order the ring's quads take them
	constexpr float Corners[4][2] = { { -1.0f, -1.0f }, { -1.0f, 1.0f }, { 1.0f, -1.0f }, { 1.0f, 1.0f } };
	u32 Offset = 0;
	PackedVertex* Vertices = static_cast<PackedVertex*>(Dynamic.Lock(Count * 4, EVertexFormat::Packed, Offset));

	for (u32 Index = 0; Index < Count; ++Index)
	{
		const Fmatrix& World = Shown[Index]->m_world;
		Fvector Up = World.j;
		Fvector Across = World.i;
		Fvector Along = World.k;
		Up.normalize();
		Across.normalize();
		Along.normalize();

		for (u32 Corner = 0; Corner < 4; ++Corner)
		{
			PackedVertex& Vertex = Vertices[Index * 4 + Corner];
			Fvector Local;
			Local.set(Corners[Corner][0], Wetness * Shown[Index]->m_height, Corners[Corner][1]);
			Vertex = {};
			World.transform_tiny(Vertex.P, Local);
			VertexPacking::EncodeFrame(Vertex, Up, Across, Along);
			// Under the open sky and in the sun, with no baked lamp: the cascades say where the sun reaches
			Vertex.C = color_rgba(255, 255, 0, 0);
		}
	}

	Dynamic.Unlock(Count * 4);
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetPass(Target->s_puddles);
	Cmd.SetGeometry(PuddleGeom);
	Dynamic.DrawQuads(Cmd, Count, Offset);
}
