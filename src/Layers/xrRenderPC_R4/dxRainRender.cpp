#include "stdafx.h"
#include "dxRainRender.h"
#include "../../xrEngine/Rain.h"

dxRainRender::dxRainRender()
{
	IReader* F = FS.r_open(_game_meshes_,"dm\\rain.dm"); 
	VERIFY3(F,"Can't open file.","dm\\rain.dm");

	DM_Drop	= ::RImplementation.model_CreateDM		(F);

	//
	SH_Rain.create("effects\\rain","fx\\fx_rain");
	hGeom_Rain.create(EGeomSource::Ring);
	hGeom_Drops.create(EGeomSource::Ring, 0, RImplementation.Imm().Index.Buffer());
	
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
	GfxCmdList& Cmd = RImplementation.Imm();
	float factor = g_pGamePersistent->Environment().CurrentEnv->rain_density;
	auto& m_sprites = owner.m_sprites;
	u32 total_sprites = (u32)m_sprites.size();
	if (factor<EPS_L || total_sprites == 0)
		return;

	// perform update
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetPass(SH_Rain);
	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetGeometry(hGeom_Rain);

	u32 MAX_SPRITES = Cmd.Vertex.GetCapacity() / 4u;
	for (u32 start_idx = 0u; start_idx < total_sprites; start_idx += MAX_SPRITES)
	{
		u32 batch_size = std::min(MAX_SPRITES, total_sprites - start_idx);
		u32 vertices_in_batch = batch_size * 4u;
		u32 vOffset;
		CEffect_Rain::rain_sprite* buff = (CEffect_Rain::rain_sprite*)Cmd.Vertex.Lock(vertices_in_batch, EVertexFormat::LIT, vOffset);
		for (u32 i = 0u; i < batch_size; ++i)
			buff[i] = m_sprites[start_idx + i];
		Cmd.Vertex.Unlock(vertices_in_batch);
		Cmd.DrawQuads(batch_size, vOffset);
	}

	// Particles
	CEffect_Rain::Particle*	P = owner.particle_active;
	if (nullptr==P)
		return;

	{
		float dt = Device.fTimeDelta;
		float particles_time = g_pGamePersistent->Environment().particles_time;
		int particles_cache = g_pGamePersistent->Environment().particles_cache;
		Fvector& f_rain_color = g_pGamePersistent->Environment().CurrentEnv->rain_color;
		float factor_visual = factor / 2.f + .5f;
		u32 u_rain_color = color_rgba_f(f_rain_color.x, f_rain_color.y, f_rain_color.z, factor_visual);
		f_rain_color.mul(0.9f);
		factor_visual *= 0.8f;
		_IndexStream& _IS = Cmd.Index;
		Cmd.SetPass(DM_Drop->shader);
		Cmd.SetCullMode(ERHI_CULLMODE::NONE);
		Fmatrix mXform,mScale;
		int pcount = 0;
		u32 v_offset,i_offset;
		u32 vCount_Lock = particles_cache*DM_Drop->number_vertices;
		u32 iCount_Lock = particles_cache*DM_Drop->number_indices;
		IRender_DetailModel::fvfVertexOut* v_ptr = (IRender_DetailModel::fvfVertexOut*)Cmd.Vertex.Lock(vCount_Lock, EVertexFormat::LIT, v_offset);
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
					Cmd.Vertex.Unlock(vCount_Lock);
					_IS.Unlock(iCount_Lock);
					Cmd.SetGeometry(hGeom_Drops);
					Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, iCount_Lock, i_offset, v_offset);

					v_ptr = (IRender_DetailModel::fvfVertexOut*)Cmd.Vertex.Lock(vCount_Lock, EVertexFormat::LIT, v_offset);
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
		Cmd.Vertex.Unlock(vCount_Lock);
		_IS.Unlock(iCount_Lock);
		if (pcount)
		{
			Cmd.SetGeometry(hGeom_Drops);
			Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, dwNumPrimitives * 3, i_offset, v_offset);
		}
	}
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
}

const Fsphere& dxRainRender::GetDropBounds() const
{
	return DM_Drop->bv_sphere;
}