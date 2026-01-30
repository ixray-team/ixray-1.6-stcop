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
	if (factor<EPS_L || owner.items.empty())
		return;

  	u32 desired_items = iFloor(0.5f*(1.f+factor)*float(g_pGamePersistent->Environment().max_desired_items));
	// visual
	float factor_visual = factor/2.f+.5f;
	Fvector f_rain_color = g_pGamePersistent->Environment().CurrentEnv->rain_color;

#if RENDER != R_R1
	f_rain_color.mul(0.9f);
	factor_visual *= 0.8f;
#endif // RENDER != R_R1

	u32 u_rain_color = color_rgba_f(f_rain_color.x,f_rain_color.y,f_rain_color.z,factor_visual);

	// perform update
	u32 vOffset;
	struct LITF
	{
		struct
		{
			Fvector p; u32 color; Fvector2 t;
		} buff[4];
	};
	LITF *verts = (LITF*) RCache.Vertex.Lock(desired_items*4,hGeom_Rain->vb_stride,vOffset);
	LITF *start = verts;
	const Fvector& vEye = Device.vCameraPosition;
	float rain_width = g_pGamePersistent->Environment().CurrentEnv->rain_width;
	for (CEffect_Rain::Item& item : owner.items)
	{
		// Build line
		Fvector& pos_head = item.P;
		Fvector pos_trail;
		pos_trail.mad(pos_head, item.D,-g_pGamePersistent->Environment().CurrentEnv->rain_length *factor_visual);

		// Culling
		Fvector sC,lineD;
		float sR; 
		sC.sub(pos_head,pos_trail);
		lineD.normalize(sC);
		sC.mul(.5f);
		sR = sC.magnitude();
		sC.add(pos_trail);

		if (!::Render->ViewBase.testSphere_dirty(sC,sR))
			continue;

		static Fvector2 UV[2][4]
		{
			{{0.f,1.f},{0.f,0.f},{1.f,1.f},{1.f,0.f}},
			{{1.f,0.f},{1.f,1.f},{0.f,0.f},{0.f,1.f}}
		};

		// Everything OK - build vertices
		Fvector	P,lineTop,camDir;
		camDir.sub(sC,vEye);
		camDir.normalize();
		lineTop.crossproduct(camDir,lineD);
		
		u32 s = item.uv_set;
		*verts =
		{
			Fvector().mad(pos_trail,lineTop,-rain_width),u_rain_color,UV[s][0].x,UV[s][0].y,
			Fvector().mad(pos_trail,lineTop,rain_width),u_rain_color,UV[s][1].x,UV[s][1].y,
			Fvector().mad(pos_head,lineTop,-rain_width),u_rain_color,UV[s][2].x,UV[s][2].y,
			Fvector().mad(pos_head,lineTop,rain_width),u_rain_color,UV[s][3].x,UV[s][3].y
		};
		verts++;
	}
	u32 vCount = (u32)(verts-start)*4;
	RCache.Vertex.Unlock(vCount,hGeom_Rain->vb_stride);

	// Render if needed
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_xform_world(Fidentity);
	RCache.set_Shader(SH_Rain);
	RCache.set_Geometry(hGeom_Rain);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,vOffset,0,vCount,0,vCount/2);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);

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
}

const Fsphere& dxRainRender::GetDropBounds() const
{
	return DM_Drop->bv_sphere;
}