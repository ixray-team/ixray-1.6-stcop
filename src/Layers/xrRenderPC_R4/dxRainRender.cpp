#include "stdafx.h"
#include "dxRainRender.h"
#include "../../xrEngine/Rain.h"

namespace RainStreaks
{
	struct Vertex
	{
		Fvector Position;
		u32 Color;
		Fvector2 UV;
		Fvector Motion;
		Fvector Side;
	};

	static D3DVERTEXELEMENT9 Declaration[] =
	{
		{ 0, 0,  D3DDECLTYPE_FLOAT3,	D3DDECLMETHOD_DEFAULT,	D3DDECLUSAGE_POSITION,	0 },
		{ 0, 12, D3DDECLTYPE_D3DCOLOR,	D3DDECLMETHOD_DEFAULT,	D3DDECLUSAGE_COLOR,		0 },
		{ 0, 16, D3DDECLTYPE_FLOAT2,	D3DDECLMETHOD_DEFAULT,	D3DDECLUSAGE_TEXCOORD,	0 },
		{ 0, 24, D3DDECLTYPE_FLOAT3,	D3DDECLMETHOD_DEFAULT,	D3DDECLUSAGE_TEXCOORD,	1 },
		{ 0, 36, D3DDECLTYPE_FLOAT3,	D3DDECLMETHOD_DEFAULT,	D3DDECLUSAGE_TEXCOORD,	2 },
		D3DDECL_END()
	};

	constexpr u32 MaxLights = 8;

	static u32 GatherLights(Fvector4 (&Position)[MaxLights], Fvector4 (&Color)[MaxLights], Fvector4 (&Direction)[MaxLights])
	{
		if (LightingModeIsStaticRender())
		{
			return 0;
		}

		const Fvector& Eye = Device.vCameraPosition;
		const float Reach = g_pGamePersistent->Environment().source_rain_radius_render + 5.0f;

		std::pair<float, light*> Best[MaxLights];
		u32 Count = 0;

		auto Consider = [&](light* L)
		{
			if (!L || L->flags.bHudMode || L->range < EPS_L)
			{
				return;
			}

			const float Distance = Eye.distance_to(L->position) - L->range;
			if (Distance > Reach)
			{
				return;
			}

			if (Count < MaxLights)
			{
				Best[Count++] = { Distance, L };
				return;
			}

			u32 Worst = 0;
			for (u32 It = 1; It < MaxLights; ++It)
			{
				if (Best[It].first > Best[Worst].first)
				{
					Worst = It;
				}
			}

			if (Distance < Best[Worst].first)
			{
				Best[Worst] = { Distance, L };
			}
		};

		const light_Package& Package = RImplementation.Lights.package;
		for (light* L : Package.v_point)
		{
			Consider(L);
		}
		for (light* L : Package.v_spot)
		{
			Consider(L);
		}
		for (light* L : Package.v_shadowed)
		{
			Consider(L);
		}

		for (u32 It = 0; It < Count; ++It)
		{
			const light* L = Best[It].second;
			Position[It].set(L->position.x, L->position.y, L->position.z, L->range);
			Color[It].set(L->color.r, L->color.g, L->color.b, 0.0f);

			if (L->flags.type == IRender_Light::SPOT)
			{
				Direction[It].set(L->direction.x, L->direction.y, L->direction.z, std::cos(L->cone * 0.5f));
			}
			else
			{
				Direction[It].set(0.0f, 0.0f, 0.0f, -2.0f);
			}
		}

		return Count;
	}
}

dxRainRender::dxRainRender()
{
	IReader* F = FS.r_open(_game_meshes_,"dm\\rain.dm"); 
	VERIFY3(F,"Can't open file.","dm\\rain.dm");

	DM_Drop	= ::RImplementation.model_CreateDM		(F);

	//
	SH_Rain.create("effects\\rain_streaks","fx\\rain_streak_drop");
	hGeom_Rain.create(RainStreaks::Declaration, RCache.Vertex.Buffer(), RCache.QuadIB);
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
	auto& m_sprites = owner.m_sprites;
	u32 total_sprites = (u32)m_sprites.size();
	if (factor<EPS_L || total_sprites == 0)
		return;

	RImplementation.Target->PhaseRainStreaksBegin();

	// perform update
	RCache.set_xform_world(Fidentity);
	RCache.set_Shader(SH_Rain);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Geometry(hGeom_Rain);

	const auto& SpriteMotion = owner.SpriteMotion;
	total_sprites = std::min(total_sprites, (u32)SpriteMotion.size());

	Fvector4 LightPosition[RainStreaks::MaxLights];
	Fvector4 LightColor[RainStreaks::MaxLights];
	Fvector4 LightDirection[RainStreaks::MaxLights];
	const u32 LightCount = RainStreaks::GatherLights(LightPosition, LightColor, LightDirection);

	for (u32 It = 0; It < LightCount; ++It)
	{
		RCache.set_ca("rain_lights_position", It, LightPosition[It]);
		RCache.set_ca("rain_lights_color", It, LightColor[It]);
		RCache.set_ca("rain_lights_direction", It, LightDirection[It]);
	}

	RCache.set_c("rain_refraction_params", g_RainRefraction, float(LightCount), 0.0f, 0.0f);

	float WidthScale = g_RainWidthScale;

	constexpr float Exposure = 1.0f / 30.0f;
	Fvector CameraShift = Fvector().sub(Device.vCameraPosition, PrevCameraPosition);
	if (!HasPrevCamera || CameraShift.square_magnitude() > 4.0f || Device.fTimeDelta < EPS_L)
	{
		CameraShift.set(0.0f, 0.0f, 0.0f);
	}
	else
	{
		CameraShift.mul(Exposure / Device.fTimeDelta);
	}

	PrevCameraPosition = Device.vCameraPosition;
	HasPrevCamera = true;

	u32 MAX_SPRITES = RCache.Vertex.GetSize() / u32(4u * sizeof(RainStreaks::Vertex));
	for (u32 start_idx = 0u; start_idx < total_sprites; start_idx += MAX_SPRITES)
	{
		u32 batch_size = std::min(MAX_SPRITES, total_sprites - start_idx);
		u32 vertices_in_batch = batch_size * 4u;
		u32 vOffset;
		RainStreaks::Vertex* buff = (RainStreaks::Vertex*)RCache.Vertex.Lock(vertices_in_batch, hGeom_Rain->vb_stride, vOffset);
		for (u32 i = 0u; i < batch_size; ++i)
		{
			const CEffect_Rain::rain_sprite& Sprite = m_sprites[start_idx + i];
			const Fvector& Motion = SpriteMotion[start_idx + i];

			const Fvector TrailCenter = Fvector().add(Sprite.buff[0].p, Sprite.buff[1].p).mul(0.5f);
			const Fvector HeadCenter = Fvector().add(Sprite.buff[2].p, Sprite.buff[3].p).mul(0.5f);
			const float Length = TrailCenter.distance_to(HeadCenter);

			Fvector Stretch = CameraShift;
			const float StretchLength = Stretch.magnitude();
			if (StretchLength > 0.5f * Length)
			{
				Stretch.mul(0.5f * Length / StretchLength);
			}

			for (u32 Pair = 0u; Pair < 4u; Pair += 2u)
			{
				const auto& Left = Sprite.buff[Pair];
				const auto& Right = Sprite.buff[Pair + 1u];

				Fvector Center = Pair == 0u ? Fvector().add(TrailCenter, Stretch) : HeadCenter;
				Fvector Side = Fvector().sub(Right.p, Left.p).mul(0.5f * WidthScale);

				*buff++ = { Center, Left.color, Left.uv, Motion, Fvector().invert(Side) };
				*buff++ = { Center, Right.color, Right.uv, Motion, Side };
			}
		}
		RCache.Vertex.Unlock(vertices_in_batch, hGeom_Rain->vb_stride);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, vOffset, 0u, vertices_in_batch, 0u, vertices_in_batch / 2u);
	}

	RImplementation.Target->PhaseRainLayers(owner.SkyVisibility());

	RImplementation.Target->PhaseRainStreaksEnd();

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