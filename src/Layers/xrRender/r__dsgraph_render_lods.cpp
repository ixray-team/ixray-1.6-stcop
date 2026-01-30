#include "stdafx.h"
#include "FLOD.h"

#ifdef _EDITOR
#include "igame_persistent.h"
#include "environment.h"
#else
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#endif

extern float r_ssaLOD_A;
extern float r_ssaLOD_B;

void R_dsgraph_structure::r_dsgraph_render_lods(bool _setup_zb, bool _clear)
{
	PROF_EVENT("LODS: Render")
	if (!mapLOD.size())
		return;
 
	// *** Fill VB and generate groups
	u32 shid = _setup_zb?SE_R1_LMODELS:SE_R1_NORMAL_LQ;
	FLOD* firstV = (FLOD*)mapLOD[0].val.pVisual;
	ref_selement cur_S = firstV->shader->E[shid];
	float ssaRange = r_ssaLOD_A - r_ssaLOD_B;
	if (ssaRange<EPS_S)
		ssaRange = EPS_S;

	const u32 uiVertexPerImposter = 4;
	const u32 uiImpostersFit = RCache.Vertex.GetSize() / (firstV->geom->vb_stride*uiVertexPerImposter);

	//Msg						("dbg_lods: shid[%d],firstV[%X]",shid,u32((void*)firstV));
	//Msg						("dbg_lods: shader[%X]",u32((void*)firstV->shader._get()));
	//Msg						("dbg_lods: shader_E[%X]",u32((void*)cur_S._get()));

	for (u32 i=0; i< mapLOD.size(); i++)
	{
		const u32 iBatchSize = (u32)std::min(mapLOD.size() - i, uiImpostersFit);
		int cur_count = 0;
		u32 vOffset;
		struct _hw4
		{
			FLOD::_hw buff[4];
		};
		_hw4* V = (_hw4*)RCache.Vertex.Lock(iBatchSize*uiVertexPerImposter,firstV->geom->vb_stride, vOffset);

		for ( u32 j=0; j<iBatchSize; ++j, ++i )
		{
			// sort out redundancy
			R_dsgraph::_LodItem	&P = mapLOD[i].val;
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
			FLOD* lodV = (FLOD*)P.pVisual;
			Fvector Ldir,shift;
			Ldir.sub(lodV->vis.sphere.P,Device.vCameraPosition).normalize();
			shift.mul(Ldir,-.5f * lodV->vis.sphere.R);

			// gen geometry
			FLOD::_face* facets = lodV->facets;
			svector<std::pair<float,u32>,8>	selector;
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

			// Fill VB
			FLOD::_face& FA = facets[id_best];
			FLOD::_face& FB = facets[id_next];
			*V =
			{
				FLOD::_hw{
					FB.v[3].v + shift,
					FA.v[3].v + shift,
					FB.N,
					FA.N,
					color_rgba(FB.v[3].c_sun,FA.v[3].c_sun,uA,uF),
					FB.v[3].t,
					FA.v[3].t,
					FB.v[3].c_rgb_hemi,
					FA.v[3].c_rgb_hemi
				},

				FLOD::_hw{
					FB.v[0].v + shift,
					FA.v[0].v + shift,
					FB.N,
					FA.N,
					color_rgba(FB.v[0].c_sun,FA.v[0].c_sun,uA,uF),
					FB.v[0].t,
					FA.v[0].t,
					FB.v[0].c_rgb_hemi,
					FA.v[0].c_rgb_hemi
				},

				FLOD::_hw{
					FB.v[2].v + shift,
					FA.v[2].v + shift,
					FB.N,
					FA.N,
					color_rgba(FB.v[2].c_sun,FA.v[2].c_sun,uA,uF),
					FB.v[2].t,
					FA.v[2].t,
					FB.v[2].c_rgb_hemi,
					FA.v[2].c_rgb_hemi
				},

				FLOD::_hw{
					FB.v[1].v + shift,
					FA.v[1].v + shift,
					FB.N,
					FA.N,
					color_rgba(FB.v[1].c_sun,FA.v[1].c_sun,uA,uF),
					FB.v[1].t,
					FA.v[1].t,
					FB.v[1].c_rgb_hemi,
					FA.v[1].c_rgb_hemi
				}
			};
			V++;
		}
		lstLODgroups.push_back(cur_count);
		RCache.Vertex.Unlock(iBatchSize*uiVertexPerImposter*4, firstV->geom->vb_stride);

		// *** Render
		RCache.set_xform_world(Fidentity);
		for ( u32 uiPass = 0; uiPass < SHADER_PASSES_MAX; ++uiPass)
		{
			int current=0;
			u32 vCurOffset = vOffset;
			
			for (int p_count : lstLODgroups)
			{
				u32 uiNumPasses = mapLOD[current].val.pVisual->shader->E[shid]->passes.size();
				if (uiPass<uiNumPasses)
				{
					RCache.set_Element(mapLOD[current].val.pVisual->shader->E[shid], uiPass);
					RCache.set_Geometry(firstV->geom);
					RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,vCurOffset,0,4*p_count,0,2*p_count);
				}
				RCache.stat.r.s_flora_lods.add(4*p_count);
				current	+= p_count;
				vCurOffset += 4*p_count;
			}

		}

		lstLODgroups.clear();
	}

	mapLOD.clear();
}
