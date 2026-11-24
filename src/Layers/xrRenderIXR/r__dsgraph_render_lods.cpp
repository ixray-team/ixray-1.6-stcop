#include "stdafx.h"
#include "FLOD.h"
#include "PackedVertex.h"

#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

extern float r_ssaLOD_A;
extern float r_ssaLOD_B;

void R_dsgraph_structure::r_dsgraph_render_lods(bool _setup_zb, bool _clear)
{
	PROF_EVENT("LODS: Render")
	if (!mapLOD.size())
		return;
	GfxCmdList& Cmd = *Backend;
	DynamicGeometry& Stream = RImplementation.Dynamic;
	// A card's record, then the corners of the two views it blends: no geometry page is read
	constexpr u32 CardRecords = 9;

	// *** Fill VB and generate groups
	u32 shid = _setup_zb?ShaderVariant::ModelLighting:ShaderVariant::NormalLQ;
	ref_selement cur_S;
	float ssaRange = r_ssaLOD_A - r_ssaLOD_B;
	if (ssaRange<EPS_S)
		ssaRange = EPS_S;

	for (u32 i = 0; i < mapLOD.size();)
	{
		const u32 iBatchSize = std::min(u32(mapLOD.size()) - i, Stream.GetCapacity() / CardRecords);
		const u32 BatchStart = i;
		cur_S = mapLOD[i].pVisual->shader->E[shid];
		int cur_count = 0;
		u32 vOffset;
		PackedVertex* V = (PackedVertex*)Stream.Lock(iBatchSize * CardRecords, EVertexFormat::Packed, vOffset);
		PackedVertex* Views = V + iBatchSize;
		const u32 ViewBase = vOffset % DynamicGeometry::ChunkRecords + iBatchSize;
		const u32 BatchEnd = vOffset + iBatchSize * CardRecords;

		for ( u32 j=0; j<iBatchSize; ++j, ++i )
		{
			// sort out redundancy
			R_dsgraph::_LodItem	&P = mapLOD[i];
			FLOD* lodV = (FLOD*)P.pVisual;
			if (P.pVisual->shader->E[shid]==cur_S)
				cur_count++;
			else
			{
				lstLODgroups.push_back(cur_count);
				cur_S = P.pVisual->shader->E[shid];
				cur_count = 1;
			}

			// calculate alpha
			float ssaDiff = P.ssa - r_ssaLOD_B;
			float scale = ssaDiff/ssaRange;
			int iA = iFloor((1-scale)*255.f);	
			u32 uA = u32(clampr(iA,0,255));

			// calculate direction and shift
			Fvector Ldir,shift;
			Ldir.sub(lodV->vis.sphere.P,RFrame.vCameraPosition).normalize();
			shift.mul(Ldir,-.5f * lodV->vis.sphere.R);

			// gen geometry
			FLOD::_face* facets = lodV->facets;
			FixedVector<std::pair<float,u32>,8>	selector;
			for (u32 s=0; s<8; s++)
				selector.push_back(std::make_pair(Ldir.dotproduct(facets[s].N),s));
			std::sort(selector.begin(),selector.end(), [](const std::pair<float, u32>& _1, const std::pair<float, u32>& _2) { return _1.first < _2.first; });

			float dot_best = selector[selector.size()-1].first;
			float dot_next = selector[selector.size()-2].first;
			float dot_next_2 = selector[selector.size()-3].first;
			u32 id_best = selector[selector.size()-1].second;
			u32 id_next = selector[selector.size()-2].second;

			// Now we have two "best" planes, calculate factor, and approx normal
			float fA = dot_best, fB = dot_next, fC = dot_next_2;
			float alpha = 0.5f + 0.5f*(1-(fB-fC)/(fA-fC));
			int iF = iFloor(alpha*255.5f);
			u32 uF = u32(clampr(iF,0,255));

			// P = shift, N = the first view's corners, TF = best | next << 3 | alpha << 8 | blend << 16
			lodV->PackFacet(id_best, Views + j * 8);
			lodV->PackFacet(id_next, Views + j * 8 + 4);
			*V++ = { shift, ViewBase + j * 8, 0u | 1u << 3 | uA << 8 | uF << 16, 0, 0, 0 };
		}
		lstLODgroups.push_back(cur_count);
		Stream.Unlock(iBatchSize * CardRecords);

		// *** Render
		Cmd.SetObjectTransform(Fidentity);
		for ( u32 uiPass = 0; uiPass < SHADER_PASSES_MAX; ++uiPass)
		{
			u32 current = BatchStart;
			u32 vCurOffset = vOffset;
			
			for (int p_count : lstLODgroups)
			{
				u32 uiNumPasses = mapLOD[current].pVisual->shader->E[shid]->passes.size();
				if (uiPass<uiNumPasses)
				{
					Cmd.SetPass(mapLOD[current].pVisual->shader->E[shid], uiPass);
					Stream.Draw(Cmd, ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 6 * p_count, vCurOffset, BatchEnd - vCurOffset);
				}
				current	+= p_count;
				vCurOffset += p_count;
			}

		}

		lstLODgroups.clear();
	}

	mapLOD.clear();
}
