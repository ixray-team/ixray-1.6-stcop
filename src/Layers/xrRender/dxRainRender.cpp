#include "stdafx.h"
#include "dxRainRender.h"

#include "../../xrEngine/Rain.h"
static Fvector2 Rain_l_UV[2][4]
{
	{{0.f,1.f},{0.f,0.f},{1.f,1.f},{1.f,0.f}},
	{{1.f,0.f},{1.f,1.f},{0.f,0.f},{0.f,1.f}}
};


dxRainRender::dxRainRender()
{
	IReader* F = FS.r_open(_game_meshes_,"dm\\rain.dm"); 
	VERIFY3(F,"Can't open file.","dm\\rain.dm");

	DM_Drop	= ::RImplementation.model_CreateDM		(F);

	//
	SH_Rain.create("effects\\rain","fx\\fx_rain");
	hGeom_Rain.create(FVF::F_LIT, RCache.Vertex.Buffer(), RCache.QuadIB);
	hGeom_Drops.create(D3DFVF_XYZ | D3DFVF_DIFFUSE | D3DFVF_TEX1, RCache.Vertex.Buffer(), RCache.Index.Buffer());
	
	FS.r_close(F);
}

dxRainRender::~dxRainRender()
{
	::RImplementation.model_Delete(DM_Drop);
}

void dxRainRender::Copy(IRainRender &_in)
{
	*this = *(dxRainRender*)&_in;
}

#include "../../xrEngine/IGame_Persistent.h"

void dxRainRender::Render(CEffect_Rain &owner)
{
	float factor = g_pGamePersistent->Environment().CurrentEnv->rain_density;
	auto& m_rquads = owner.m_rquads;
	if (factor<EPS_L || m_rquads.empty())
		return;

	// perform update
	RCache.set_xform_world(Fidentity);
	RCache.set_Shader(SH_Rain);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Geometry(hGeom_Rain);

	Fvector& f_rain_color = g_pGamePersistent->Environment().CurrentEnv->rain_color;
	float factor_visual = factor / 2.f + .5f;
	u32 u_rain_color = color_rgba_f(f_rain_color.x, f_rain_color.y, f_rain_color.z, factor_visual);

#if RENDER != R_R1
	f_rain_color.mul(0.9f);
	factor_visual *= 0.8f;
#endif // RENDER != R_R1

	u32 current_line = 0;
	struct LITF { struct { Fvector p; u32 color; Fvector2 uv; } buff[4]; };
	u32 max_quads_per_batch = RCache.Vertex.GetSize() / u32(sizeof(LITF));
	while (current_line < m_rquads.size())
	{
		u32 lines_in_batch = std::min(max_quads_per_batch, (u32)m_rquads.size()-current_line);
		u32 vertices_in_batch = lines_in_batch * 4u;
		u32 vOffset;
		
		LITF* verts = (LITF*)RCache.Vertex.Lock(vertices_in_batch, hGeom_Rain->vb_stride, vOffset);
		for (u32 i = 0; i < lines_in_batch; ++i)
		{
			CEffect_Rain::rain_line& line = m_rquads[current_line + i];
			u32 s = line.uv_set;
			*verts =
			{
				line.quad[0],u_rain_color,Rain_l_UV[s][0].x,Rain_l_UV[s][0].y,
				line.quad[1],u_rain_color,Rain_l_UV[s][1].x,Rain_l_UV[s][1].y,
				line.quad[2],u_rain_color,Rain_l_UV[s][2].x,Rain_l_UV[s][2].y,
				line.quad[3],u_rain_color,Rain_l_UV[s][3].x,Rain_l_UV[s][3].y
			};
			verts++;
		}
		RCache.Vertex.Unlock(vertices_in_batch, hGeom_Rain->vb_stride);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, vOffset, 0, vertices_in_batch, 0, vertices_in_batch / 2);
		current_line += lines_in_batch;
	}

	// Particles
	CEffect_Rain::Particle*	P = owner.particle_active;
	if (0==P)
		return;

	{
		float dt = Device.fTimeDelta;
		float particles_time = g_pGamePersistent->Environment().particles_time;
		int particles_cache = g_pGamePersistent->Environment().particles_cache;
		_IndexStream& _IS = RCache.Index;
		RCache.set_Shader(DM_Drop->shader);
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		Fmatrix mXform,mScale;
		int pcount = 0;
		u32 v_offset,i_offset;
		u32 vCount_Lock = particles_cache*DM_Drop->number_vertices;
		u32 iCount_Lock = particles_cache*DM_Drop->number_indices;
		IRender_DetailModel::fvfVertexOut* v_ptr = (IRender_DetailModel::fvfVertexOut*)RCache.Vertex.Lock(vCount_Lock, hGeom_Drops->vb_stride, v_offset);
		u16* i_ptr = _IS.Lock(iCount_Lock, i_offset);
		while (P)
		{
			CEffect_Rain::Particle*	next = P->next;
			float& time = P->time;
			// Update
			// P can be zero sometimes and it crashes
			time -= dt;
			if (time<=0)
			{
				owner.p_free(P);
				P =	next;
				continue;
			}

			// Render
			if (::Render->ViewBase.testSphere_dirty(P->bounds.P, P->bounds.R))
			{
				// Build matrix
				float scale = time / particles_time;
				mScale.scale(scale,scale,scale);
				mXform.mul_43(P->mXForm,mScale);

				// XForm verts
				DM_Drop->transfer(mXform,v_ptr,u_rain_color,i_ptr,pcount*DM_Drop->number_vertices);
				v_ptr += DM_Drop->number_vertices;
				i_ptr += DM_Drop->number_indices;
				pcount++;

				if (pcount >= particles_cache)
				{
					// flush
					RCache.Vertex.Unlock(vCount_Lock,hGeom_Drops->vb_stride);
					_IS.Unlock(iCount_Lock);
					RCache.set_Geometry(hGeom_Drops);
					RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,v_offset,0,vCount_Lock,i_offset, iCount_Lock/3);

					v_ptr = (IRender_DetailModel::fvfVertexOut*)RCache.Vertex.Lock(vCount_Lock, hGeom_Drops->vb_stride, v_offset);
					i_ptr = _IS.Lock(iCount_Lock, i_offset);

					pcount = 0;
				}
			}

			P = next;
		}

		// Flush if needed
		vCount_Lock = pcount*DM_Drop->number_vertices;
		iCount_Lock = pcount*DM_Drop->number_indices;
		u32	dwNumPrimitives = iCount_Lock/3;
		RCache.Vertex.Unlock(vCount_Lock,hGeom_Drops->vb_stride);
		_IS.Unlock(iCount_Lock);
		if (pcount)
		{
			RCache.set_Geometry(hGeom_Drops);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,v_offset,0,vCount_Lock,i_offset,dwNumPrimitives);
		}
	}
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
}

const Fsphere& dxRainRender::GetDropBounds() const
{
	return DM_Drop->bv_sphere;
}