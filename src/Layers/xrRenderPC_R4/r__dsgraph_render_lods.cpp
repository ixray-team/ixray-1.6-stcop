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

	// *** Fill VB and generate groups
	u32 shid = _setup_zb?ShaderVariant::ModelLighting:ShaderVariant::NormalLQ;
	ref_selement cur_S;
	float ssaRange = r_ssaLOD_A - r_ssaLOD_B;
	if (ssaRange<EPS_S)
		ssaRange = EPS_S;

	for (u32 i = 0; i < mapLOD.size();)
	{
		const u32 iBatchSize = std::min(u32(mapLOD.size()) - i, Cmd.Vertex.GetCapacity());
		const u32 BatchStart = i;
		cur_S = mapLOD[i].val.pVisual->shader->E[shid];
		SGeometry* cur_G = ((FLOD*)mapLOD[i].val.pVisual)->geom._get();
		int cur_count = 0;
		u32 vOffset;
		PackedVertex* V = (PackedVertex*)Cmd.Vertex.Lock(iBatchSize, EVertexFormat::Packed, vOffset);

		for ( u32 j=0; j<iBatchSize; ++j, ++i )
		{
			// sort out redundancy
			R_dsgraph::_LodItem	&P = mapLOD[i].val;
			FLOD* lodV = (FLOD*)P.pVisual;
			if (P.pVisual->shader->E[shid]==cur_S && lodV->geom._get()==cur_G)
				cur_count++;
			else
			{
				lstLODgroups.push_back(cur_count);
				cur_S = P.pVisual->shader->E[shid];
				cur_G = lodV->geom._get();
				cur_count = 1;
			}

			// calculate alpha
			float ssaDiff = P.ssa - r_ssaLOD_B;
			float scale = ssaDiff/ssaRange;
			int iA = iFloor((1-scale)*255.f);	
			u32 uA = u32(clampr(iA,0,255));

			// calculate direction and shift
			Fvector Ldir,shift;
			Ldir.sub(lodV->vis.sphere.P,Device.vCameraPosition).normalize();
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

			// P = shift, N = facet base, TF = best | next << 3 | alpha << 8 | blend << 16
			*V++ = { shift, lodV->Facets.VertexBase, id_best | id_next << 3 | uA << 8 | uF << 16, 0, 0, 0 };
		}
		lstLODgroups.push_back(cur_count);
		Cmd.Vertex.Unlock(iBatchSize);

		// *** Render
		Cmd.SetObjectTransform(Fidentity);
		for ( u32 uiPass = 0; uiPass < SHADER_PASSES_MAX; ++uiPass)
		{
			u32 current = BatchStart;
			u32 vCurOffset = vOffset;
			
			for (int p_count : lstLODgroups)
			{
				u32 uiNumPasses = mapLOD[current].val.pVisual->shader->E[shid]->passes.size();
				if (uiPass<uiNumPasses)
				{
					Cmd.SetPass(mapLOD[current].val.pVisual->shader->E[shid], uiPass);
					Cmd.SetGeometry(((FLOD*)mapLOD[current].val.pVisual)->geom);
					Cmd.Draw(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 6 * p_count, vCurOffset);
				}
				current	+= p_count;
				vCurOffset += p_count;
			}

		}

		lstLODgroups.clear();
	}

	mapLOD.clear();
}
