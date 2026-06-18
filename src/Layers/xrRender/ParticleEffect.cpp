#include "stdafx.h"
#include "ParticleEffect.h"
#include "CHudInitializer.h"

using namespace PAPI;
using namespace PS;

const u32	PS::uDT_STEP 	= 33;
const float	PS::fDT_STEP 	= float(uDT_STEP)/1000.f;

static void ApplyTexgen(const Fmatrix& mVP)
{
	Fmatrix mTexgen;

	const float _w = float(RCache.get_width());
	const float _h = float(RCache.get_height());
#ifdef USE_DX11
	const float o_w = 0.f;
	const float o_h = 0.f;
#else 
	const float o_w = (.5f / _w);
	const float o_h = (.5f / _h);
#endif

	Fmatrix mTexelAdjust =
	{
		0.5f,		0.0f,		0.0f, 0.0f,
		0.0f,		-0.5f,		0.0f, 0.0f,
		0.0f,		0.0f,		1.0f, 0.0f,
		0.5f + o_w,	0.5f + o_h,	0.0f, 1.0f
	};

	mTexgen.mul(mTexelAdjust, mVP);
	RCache.set_c("mVPTexgen", mTexgen);
}

void PS::OnEffectParticleBirth(void* owner, u32 , PAPI::Particles& P, size_t pID, u32 )
{
	CParticleEffect* PE = static_cast<CParticleEffect*>(owner); VERIFY(PE);
    if (CPEDef* PED = PE->GetDefinition())
	{
        if (PED->m_Flags.is(CPEDef::dfRandomFrame))
        {
	        P.frame_arr[pID] = (u16)iFloor(Random.randI(PED->m_Frame.m_iFrameCount)*255.f);
        }

        if (PED->m_Flags.is(CPEDef::dfAnimated)&&PED->m_Flags.is(CPEDef::dfRandomPlayback)&&Random.randI(2))
        {
	        P.flags_arr[pID].set(Particles::ANIMATE_CCW,true);
        }
    }
}
void PS::OnEffectParticleDead(void* , u32 , PAPI::Particles& , size_t , u32 )
{
//	CPEDef* PE = static_cast<CPEDef*>(owner);
}
//------------------------------------------------------------------------------
// class CParticleEffect
//------------------------------------------------------------------------------
CParticleEffect::~CParticleEffect()
{
	xrCriticalSectionGuard guard(&onframe_lock);
	// Log					("--- destroy PE");
	GeomDestroy();
}

void CParticleEffect::Play()
{
	xrCriticalSectionGuard guard(&onframe_lock);
	m_RT_Flags.set(flRT_DefferedStop,false);
	m_RT_Flags.set(flRT_Playing,true);
	Pholder.PlayEffect();
}
void CParticleEffect::Stop(bool bDefferedStop)
{
	xrCriticalSectionGuard guard(&onframe_lock);
	Pholder.StopEffect(bDefferedStop);
	if (bDefferedStop)
		m_RT_Flags.set(flRT_DefferedStop,true);
	else
		m_RT_Flags.set(flRT_Playing,false);
}

void CParticleEffect::UpdateParent(const Fmatrix& m, const Fvector& velocity, bool bXFORM)
{
	xrCriticalSectionGuard guard(&onframe_lock);
	m_RT_Flags.set(flRT_XFORM, bXFORM);
	if (bXFORM)
	{
		m_XFORM.set(m);
	}
	else
	{
		m_InitialPosition = m.c;
		Pholder.Transform(m,velocity);
	}
}

void CParticleEffect::OnFrame(u32 frame_dt)
{
	// PROF_EVENT(__FUNCTION__);
	xrCriticalSectionGuard guard(&onframe_lock);
	if (!m_Def || !m_RT_Flags.is(flRT_Playing))
	{
		vis.box.set(m_InitialPosition, m_InitialPosition);
		vis.box.grow(EPS_L);
		vis.box.getsphere(vis.sphere.P, vis.sphere.R);
		return;
	}

	if (0==m_RT_Flags.is(flRT_LiveUpdate))
	{
		m_MemDT += frame_dt;
		int	StepCount = 0;
		if (m_MemDT >= uDT_STEP)
		{
			StepCount = m_MemDT / uDT_STEP;
			m_MemDT = m_MemDT % uDT_STEP;
			clamp(StepCount, 0, 3);
		}

		for (; StepCount; StepCount--)
		{
			if (m_Def->m_Flags.is(CPEDef::dfTimeLimit))
			{
				if (!m_RT_Flags.is(flRT_DefferedStop))
				{
					m_fElapsedLimit -= fDT_STEP;
					if (m_fElapsedLimit < 0.f)
					{
						m_fElapsedLimit = m_Def->m_fTimeLimit;
						Stop(true);
						break;
					}
				}
			}
			Pholder.Update(fDT_STEP);

			u32 p_cnt = 0u;
			auto& particles = Pholder.GetParticles(p_cnt);

			// our actions
			if (m_Def->m_Flags.is(CPEDef::dfFramed | CPEDef::dfAnimated))
				m_Def->ExecuteAnimate(particles, p_cnt, fDT_STEP);

			bool EditorTest = !Device.IsEditorMode();
#ifdef _EDITOR
			EditorTest = EditorTest || UI->IsPlayInEditor();
#endif
			if (m_Def->m_Flags.is(CPEDef::dfCollision) && EditorTest)
				m_Def->ExecuteCollision(particles, p_cnt, fDT_STEP, this, m_CollisionCallback);

			//-move action
			if (p_cnt)
			{
				vis.box.invalidate();
				float p_size = 0.f;
				if(!RImplementation.ViewBase.testSphere_dirty(vis.sphere.P, vis.sphere.R))
				{
					auto PosArr = particles.pos_arr;
					auto PosIArr = particles.posI_arr;
					for (u32 i = 0; i < p_cnt; i++)
					{
						PosIArr[i].set(PosArr[i]);
					}
					auto RotArr = particles.rot_arr;
					auto RotIArr = particles.rotI_arr;
					for (u32 i = 0; i < p_cnt; i++)
					{
						RotIArr[i].set(RotArr[i]);
					}
					auto velArr = particles.vel_arr;
					auto velIArr = particles.velI_arr;
					for (u32 i = 0; i < p_cnt; i++)
					{
						velIArr[i].set(velArr[i]);
					}
					auto SizeArr = particles.size_arr;
					auto SizeIArr = particles.sizeI_arr;
					for (u32 i = 0; i < p_cnt; i++)
					{
						SizeIArr[i].set(SizeArr[i]);
					}
				}
				for (u32 i = 0; i < p_cnt; i++)
				{
					auto elem = particles.pos_arr[i];
					vis.box.modify(elem);
				}
				for (u32 i = 0; i < p_cnt; i++)
				{
					auto elem = particles.size_arr[i];
					if (elem.x > p_size) p_size = elem.x;
					if (elem.y > p_size) p_size = elem.y;
					if (elem.z > p_size) p_size = elem.z;
					
				}
				vis.box.grow(p_size);
				vis.box.getsphere(vis.sphere.P, vis.sphere.R);
			}
			if (m_RT_Flags.is(flRT_DefferedStop) && (0 == p_cnt))
			{
				m_RT_Flags.set(flRT_Playing | flRT_DefferedStop, false);
				break;
			}
		}
	}
	else
	{
		Pholder.Update(Device.fTimeDelta);
		u32 p_cnt = 0u;
		auto& particles = Pholder.GetParticles(p_cnt);
		// our actions
		if (m_Def->m_Flags.is(CPEDef::dfFramed | CPEDef::dfAnimated))
		{
			m_Def->ExecuteAnimate(particles, p_cnt, Device.fTimeDelta);
		}

		if (m_Def->m_Flags.is(CPEDef::dfCollision))
		{
			m_Def->ExecuteCollision(particles, p_cnt, Device.fTimeDelta, this, m_CollisionCallback);
		}

		//-move action
		if (p_cnt!=0u)
		{
			vis.box.invalidate();
			float p_size = 0.f;
			for (u32 i = 0; i < p_cnt; i++)
			{
				auto elem = particles.pos_arr[i];
				if (!_valid(elem))
				{
					continue;
				}
				vis.box.modify(elem);
			}
			for (u32 i = 0; i < p_cnt; i++)
			{
				auto elem = particles.size_arr[i];
				if (elem.x > p_size) p_size = elem.x;
				if (elem.y > p_size) p_size = elem.y;
				if (elem.z > p_size) p_size = elem.z;
			}
			vis.box.grow(p_size);
			vis.box.getsphere(vis.sphere.P, vis.sphere.R);
		}

		bool deffered_stop = true;
		if (m_Def->m_Flags.is(CPEDef::dfTimeLimit))
		{
			if (!m_RT_Flags.is(flRT_DefferedStop))
			{
				m_fElapsedLimit -= Device.fTimeDelta;
				if (m_fElapsedLimit < 0.f)
				{
					m_fElapsedLimit = m_Def->m_fTimeLimit;
					Stop(true);
					deffered_stop = false;
				}
			}
		}
		if (deffered_stop && m_RT_Flags.is(flRT_DefferedStop) && (0 == p_cnt))
			m_RT_Flags.set(flRT_Playing | flRT_DefferedStop, false);
	}
}

void CParticleEffect::Compile(CPEDef* def)
{
	xrCriticalSectionGuard guard(&onframe_lock);
	m_Def = def;
	if (m_Def)
	{
		// refresh shader
		GeomDestroy();
		GeomCreate();

		// append actions
		IReader F (m_Def->m_Actions.pointer(),m_Def->m_Actions.size());
        Pholder.LoadActions(F);
        Pholder.SetMaxParticles(m_Def->m_MaxParticles);
        Pholder.SetCallback(OnEffectParticleBirth,OnEffectParticleDead,this,0);
		// time limit
		if (m_Def->m_Flags.is(CPEDef::dfTimeLimit))
			m_fElapsedLimit = m_Def->m_fTimeLimit;
	}
}

void CParticleEffect::GeomCreate()
{
	xrCriticalSectionGuard guard(&onframe_lock);
	if (m_Def && m_Def->m_Flags.is(CPEDef::dfSprite))
	{
		geom.create(FVF::F_LIT, RCache.Vertex.Buffer(), RCache.QuadIB);
		shader = m_Def->m_CachedShader;
	}
}

void CParticleEffect::Copy(dxRender_Visual*)
{
	FATAL("Can't duplicate particle system - NOT IMPLEMENTED");
}

void CParticleEffect::GeomDestroy()
{
	xrCriticalSectionGuard guard(&onframe_lock);
	geom.destroy();
	shader.destroy();
}

void CParticleEffect::SetBirthDeadCB(PAPI::OnBirthParticleCB bc, PAPI::OnDeadParticleCB dc, void* owner, u32 p)
{
	xrCriticalSectionGuard guard(&onframe_lock);
	Pholder.SetCallback(bc,dc,owner,p);
}

u32 CParticleEffect::SpriteCount()
{
	return Pholder.GetParticlesCount();
}

PAPI::ParticleAction* CParticleEffect::FindPA(shared_str PEName, PAPI::PActionEnum Action)
{
	xrCriticalSectionGuard guard(&onframe_lock);
	R_ASSERT4(PEName == Name(), "Attempt to find PA in wrong PE", PEName.c_str(), Name().c_str());

	auto it = std::find_if(Pholder.m_actions.begin(), Pholder.m_actions.end(), [Action](PAPI::ParticleAction* action) { return action->type == Action; });
	
	return it != Pholder.m_actions.end() ? *it : nullptr;
}

//------------------------------------------------------------------------------
// Render
//------------------------------------------------------------------------------
#ifndef _EDITOR
//----------------------------------------------------
ICF void FillSprite	(PAPI::Particles::LITBUFF*& pv, xrCriticalSection& cache_lock, const Fvector& pos, const Fvector& T, const Fvector& R, const Fvector4& uv, const Fvector2& vs, u32 clr, float angle)
{
	float sa = std::sin(angle);  
	float ca = std::cos(angle);  

	Fvector Vr = T*vs.x*sa + R*vs.x*ca;
	Fvector Vt = T*vs.y*ca - R*vs.y*sa;
	Fvector a = Vt-Vr, b = Vt+Vr;

	xrCriticalSectionGuard guard(&cache_lock);
	*pv =
	{
		-b+pos, clr, uv.x,uv.w,
		a+pos, clr, uv.x,uv.y,
		-a+pos, clr, uv.z,uv.w,
		b+pos, clr, uv.z,uv.y
	};
	++pv;
}

void CParticleEffect::UpdateCache()
{
	// PROF_EVENT(__FUNCTION__);

	if (!m_Def || !m_Def->m_Flags.test(CPEDef::dfSprite) || !m_RT_Flags.is(flRT_Playing))
		return;

	if (Device.dwFrame == chache_frame.load())	return;
	chache_frame.store(Device.dwFrame);


	// Get a pointer to the particles in gp memory
	u32 p_cnt = 0u;
	auto& particles = Pholder.GetParticles(p_cnt);

	if (p_cnt == 0u)
	{
		return;
	}

	Fvector& cam_dir = RDEVICE.vCameraDirection_saved;
    Fvector& cam_top = RDEVICE.vCameraTop_saved;
    Fvector& cam_right = RDEVICE.vCameraRight_saved;

	if (m_RT_Flags.is(flRT_LiveUpdate))
	{
		auto PosArr = particles.pos_arr;
		auto PosIArr = particles.posI_arr;
		for (u32 i = 0; i < p_cnt; i++)
		{
			PosIArr[i].set(PosArr[i]);
		}
		auto RotArr = particles.rot_arr;
		auto RotIArr = particles.rotI_arr;
		for (u32 i = 0; i < p_cnt; i++)
		{
			RotIArr[i].set(RotArr[i]);
		}
		auto velArr = particles.vel_arr;
		auto velIArr = particles.velI_arr;
		for (u32 i = 0; i < p_cnt; i++)
		{
			velIArr[i].set(velArr[i]);
		}
		auto SizeArr = particles.size_arr;
		auto SizeIArr = particles.sizeI_arr;
		for (u32 i = 0; i < p_cnt; i++)
		{
			SizeIArr[i].set(SizeArr[i]);
		}
	} else
	{
		constexpr float tau = .1f;
		float dt = exp(-Device.fTimeDelta / tau);
				
		auto PosArr = particles.pos_arr;
		auto PosIArr = particles.posI_arr;
		for (u32 i = 0; i < p_cnt; i++)
		{
			PosIArr[i].inertion(PosArr[i], dt);
		}
		auto RotArr = particles.rot_arr;
		auto RotIArr = particles.rotI_arr;
		for (u32 i = 0; i < p_cnt; i++)
		{
			RotIArr[i] = RotArr[i];
		}
		auto velArr = particles.vel_arr;
		auto velIArr = particles.velI_arr;
		for (u32 i = 0; i < p_cnt; i++)
		{
			velIArr[i].inertion(velArr[i], dt);
		}
		auto SizeArr = particles.size_arr;
		auto SizeIArr = particles.sizeI_arr;
		for (u32 i = 0; i < p_cnt; i++)
		{
			SizeIArr[i].inertion(SizeArr[i], dt);
		}
	}

	// float r_x - +1 = 1
	// float r_y - +1 = 2
	// Fvector2 lt - +2 = 4
	// Fvector2 rb - +2 = 6
	// Fcolor FinalColor - +4 = 10
	size_t required_buff_size = p_cnt*10;
	thread_local xr_vector<float> buff(required_buff_size, {});
	if (buff.size() < required_buff_size)
	{
		buff.resize(required_buff_size);
	}
	float* r_x_arr = buff.data();
	float* r_y_arr = buff.data()+p_cnt;
	Fvector2* lt_arr = (Fvector2*)(buff.data()+(p_cnt*2));
	Fvector2* rb_arr = (Fvector2*)(buff.data()+(p_cnt*4));
	Fcolor* FinalColor = (Fcolor*)(buff.data()+(p_cnt*6));
	
	{
		auto SizeIArr = particles.sizeI_arr;
		auto sizeModArr = particles.sizeMod_arr;
		for (u32 i = 0; i < p_cnt; i++)
		{
			r_x_arr[i] = SizeIArr[i].x*sizeModArr[i].mod.x;
			VERIFY(r_x_arr[i] >= 0.f && r_x_arr[i] <= 1000.f);
			r_x_arr[i] *= 0.5f;
			r_y_arr[i] = SizeIArr[i].y*sizeModArr[i].mod.y;
			VERIFY(r_y_arr[i] >= 0.f && r_y_arr[i] <= 1000.f);
			r_y_arr[i] *= 0.5f;
		}
		if (m_Def->m_Flags.is(CPEDef::dfVelocityScale))
		{
			auto VelIArr = particles.velI_arr;
			for (u32 i = 0; i < p_cnt; i++)
			{
				auto Speed = VelIArr[i].magnitude();
				r_x_arr[i] += Speed*m_Def->m_VelocityScale.x;
				r_y_arr[i] += Speed*m_Def->m_VelocityScale.y;
			}
		}
	}

	{
		for (u32 i = 0; i < p_cnt; i++)
		{
			lt_arr[i].set(0.f, 0.f);
			rb_arr[i].set(1.f, 1.f);
		}
				
		if (m_Def->m_Flags.is(CPEDef::dfFramed))
		{
			auto FrameArr = particles.frame_arr;
			for (u32 i = 0; i < p_cnt; i++)
			{
				m_Def->m_Frame.CalculateTC(iFloor(float(FrameArr[i]) / 255.f), lt_arr[i], rb_arr[i]);
			}
		}
	}

	{
		auto ColorArr = particles.color_arr;
		auto ColorModArr = particles.colorMod_arr;
		for (u32 i = 0; i < p_cnt; i++)
		{
			auto Color = ColorArr[i];
			auto ColorMod = ColorModArr[i].mod;
			FinalColor[i].set(
				clampr(Color.r * ColorMod.x, 0.0f, 1.0f),
				clampr(Color.g * ColorMod.y, 0.0f, 1.0f),
				clampr(Color.b * ColorMod.z, 0.0f, 1.0f),
				clampr(Color.a * ColorMod.w, 0.0f, 1.0f)
				);
		}
	}

	auto BuffPtr = particles.buff_arr;
	auto RotIArr = particles.rotI_arr;
	if (m_Def->m_Flags.is(CPEDef::dfAlignToPath))
	{
		auto PosIArr = particles.posI_arr;
		for (u32 i = 0; i < p_cnt; i++)
		{
			float speed = particles.velI_arr[i].magnitude();
			if ((speed < EPS_S) && m_Def->m_Flags.is(CPEDef::dfWorldAlign))
			{
				Fmatrix	M;  	
				M.setXYZ(m_Def->m_APDefaultRotation);
				if (m_RT_Flags.is(flRT_XFORM))
				{
					Fvector p;
					m_XFORM.transform_tiny(p,PosIArr[i]);
					M.mulA_43(m_XFORM);
					FillSprite(BuffPtr, cache_lock,p,M.k,M.i,
						{lt_arr[i].x, lt_arr[i].y,rb_arr[i].x, rb_arr[i].y},
						{r_x_arr[i],r_y_arr[i]},FinalColor[i].get(),RotIArr[i].x);
				}
				else
				{
					FillSprite(BuffPtr, cache_lock, PosIArr[i], M.k, M.i,
						{lt_arr[i].x, lt_arr[i].y,rb_arr[i].x, rb_arr[i].y},
						{r_x_arr[i],r_y_arr[i]},FinalColor[i].get(),RotIArr[i].x);
				}
			}
			else if ((speed >= EPS_S) && m_Def->m_Flags.is(CPEDef::dfFaceAlign))
			{
				Fmatrix	M; M.identity();
				M.k.div(particles.velI_arr[i], speed);
				M.j.set(0.f, 1.f, 0.f);

				if (std::abs(M.j.dotproduct(M.k)) > .99f)
					M.j.set(0.f, 0.f, 1.f);

				M.i.crossproduct(M.j, M.k); M.i.normalize();
				M.j.crossproduct(M.k, M.i); M.j.normalize();
				if (m_RT_Flags.is(flRT_XFORM))
				{
					Fvector p;
					m_XFORM.transform_tiny(p,PosIArr[i]);
					M.mulA_43(m_XFORM);
					FillSprite(BuffPtr, cache_lock,p,M.k,M.i,
						{lt_arr[i].x, lt_arr[i].y,rb_arr[i].x, rb_arr[i].y},
						{r_x_arr[i],r_y_arr[i]},FinalColor[i].get(),RotIArr[i].x);
				}
				else
				{
					FillSprite(BuffPtr, cache_lock, PosIArr[i], M.k, M.i,
						{lt_arr[i].x, lt_arr[i].y,rb_arr[i].x, rb_arr[i].y},
						{r_x_arr[i],r_y_arr[i]},FinalColor[i].get(),RotIArr[i].x);
				}
			}
			else
			{
				Fvector dir;

				if (speed >= EPS_S)
				{
					dir.div(particles.velI_arr[i], speed);
				}
				else
				{
					dir.setHP(-m_Def->m_APDefaultRotation.y, -m_Def->m_APDefaultRotation.x);
				}

				if (m_RT_Flags.is(flRT_XFORM))
				{
					Fvector p,d;
					m_XFORM.transform_tiny(p,PosIArr[i]);
					m_XFORM.transform_dir(d,dir);
					FillSprite(BuffPtr, cache_lock, p, d, Fvector(d^cam_dir).normalize_safe(),
						{lt_arr[i].x, lt_arr[i].y, rb_arr[i].x, rb_arr[i].y},
						{r_x_arr[i],r_y_arr[i]},FinalColor[i].get(),RotIArr[i].x);
				}
				else
				{
					FillSprite(BuffPtr, cache_lock, PosIArr[i], dir, Fvector(dir^cam_dir).normalize_safe(),
						{lt_arr[i].x, lt_arr[i].y, rb_arr[i].x, rb_arr[i].y},
						{r_x_arr[i],r_y_arr[i]},FinalColor[i].get(),RotIArr[i].x);
				}
			}
		}
	}
	else
	{
		if (m_RT_Flags.is(flRT_XFORM))
		{
			for (u32 i = 0; i < p_cnt; i++)
			{
				Fvector p;
				m_XFORM.transform_tiny(p,particles.posI_arr[i]);
				FillSprite(BuffPtr, cache_lock, p, cam_top, cam_right,
					{lt_arr[i].x, lt_arr[i].y,rb_arr[i].x, rb_arr[i].y},
					{r_x_arr[i],r_y_arr[i]}, FinalColor[i].get(),RotIArr[i].x);
			}
		}
		else
		{
			for (u32 i = 0; i < p_cnt; i++)
			{
				FillSprite(BuffPtr, cache_lock, particles.posI_arr[i], cam_top, cam_right,
					{lt_arr[i].x, lt_arr[i].y,rb_arr[i].x, rb_arr[i].y},
					{r_x_arr[i],r_y_arr[i]}, FinalColor[i].get(),RotIArr[i].x);
			}
		}
	}
}

void CParticleEffect::Render(float)
{
	if (!m_Def || !m_Def->m_Flags.test(CPEDef::dfSprite))
	{
		return;
	}

	Fvector c = vis.sphere.P;
	m_XFORM.transform_tiny(c);

	if (Device.vCameraPosition.distance_to_sqr(c) > _sqr(g_pGamePersistent->Environment().CurrentEnv->fog_distance + vis.sphere.R))
	{
		return;
	}

	UpdateCache();

	xrCriticalSectionGuard guard(&cache_lock);
	u32 total_sprites = 0u;
	auto& particles = Pholder.GetParticles(total_sprites);

	if (total_sprites == 0u)
	{
		return;
	}

	RCache.set_xform_world(Fidentity);
	RCache.set_Geometry(geom);

	GRHI->StateManager->SetCullMode(m_Def->m_Flags.test(CPEDef::dfCulling) ? (m_Def->m_Flags.test(CPEDef::dfCullCCW) ? ERHI_CULLMODE::BACK : ERHI_CULLMODE::FRONT) : ERHI_CULLMODE::NONE);

	u32 MAX_SPRITES = RCache.Vertex.GetSize() / u32(sizeof(PAPI::Particles::LITBUFF));
	for (u32 start_idx = 0u; start_idx < total_sprites; start_idx += MAX_SPRITES)
	{
		u32 batch_size = std::min(MAX_SPRITES, total_sprites - start_idx);
		u32 vertices_in_batch = batch_size * 4u;
		u32 vOffset;

		PAPI::Particles::LITBUFF* buff = (PAPI::Particles::LITBUFF*)RCache.Vertex.Lock(vertices_in_batch, geom->vb_stride, vOffset);
		for (u32 i = 0u; i < batch_size; ++i)
		{
			buff[i] = particles.buff_arr[start_idx + i];
		}
		RCache.Vertex.Unlock(vertices_in_batch, geom->vb_stride);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, vOffset, 0u, vertices_in_batch, 0u, vertices_in_batch / 2u);
	}

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
}
#else
ICF void FillSprite(PAPI::Particles::LITBUFF*& pv, const Fvector& T, const Fvector& R, const Fvector& pos, const Fvector2& lt, const Fvector2& rb, float r1, float r2, u32 clr, float angle)
{
	float sa = std::sin(angle);
	float ca = std::cos(angle);
	Fvector Vr, Vt;
	Vr.x = T.x * r1 * sa + R.x * r1 * ca;
	Vr.y = T.y * r1 * sa + R.y * r1 * ca;
	Vr.z = T.z * r1 * sa + R.z * r1 * ca;
	Vt.x = T.x * r2 * ca - R.x * r2 * sa;
	Vt.y = T.y * r2 * ca - R.y * r2 * sa;
	Vt.z = T.z * r2 * ca - R.z * r2 * sa;

	Fvector a, b, c, d;
	a.sub(Vt, Vr);
	b.add(Vt, Vr);
	c.invert(a);
	d.invert(b);
	*pv =
	{
		d + pos, clr, lt.x,rb.y,
		a + pos, clr, lt.x,lt.y,
		c + pos, clr, rb.x,rb.y,
		b + pos, clr, rb.x,lt.y
	};
	pv++;
}

ICF void FillSprite(PAPI::Particles::LITBUFF*& pv, const Fvector& pos, const Fvector& dir, const Fvector2& lt, const Fvector2& rb, float r1, float r2, u32 clr, float angle)
{
	float sa = std::sin(angle);
	float ca = std::cos(angle);
	const Fvector& T = dir;
	Fvector R; 	R.crossproduct(T, RDEVICE.vCameraDirection).normalize_safe();
	Fvector Vr, Vt;
	Vr.x = T.x * r1 * sa + R.x * r1 * ca;
	Vr.y = T.y * r1 * sa + R.y * r1 * ca;
	Vr.z = T.z * r1 * sa + R.z * r1 * ca;
	Vt.x = T.x * r2 * ca - R.x * r2 * sa;
	Vt.y = T.y * r2 * ca - R.y * r2 * sa;
	Vt.z = T.z * r2 * ca - R.z * r2 * sa;

	Fvector a, b, c, d;
	a.sub(Vt, Vr);
	b.add(Vt, Vr);
	c.invert(a);
	d.invert(b);
	*pv =
	{
		d + pos, clr, lt.x,rb.y,
		a + pos, clr, lt.x,lt.y,
		c + pos, clr, rb.x,rb.y,
		b + pos, clr, rb.x,lt.y
	};
	pv++;
}

void CParticleEffect::Render(float)
{
	// PROF_EVENT(__FUNCTION__);
	xrCriticalSectionGuard guard(&onframe_lock);

	u32 dwOffset, dwCount;
	// Get a pointer to the particles in gp memory
	u32 p_cnt;
	auto& particles = Pholder.GetParticles(p_cnt);

	if (p_cnt > 0)
	{
		if (m_Def && m_Def->m_Flags.is(CPEDef::dfSprite))
		{
			PAPI::Particles::LITBUFF* pv_start = (PAPI::Particles::LITBUFF*)RCache.Vertex.Lock(p_cnt * 4 * 4, geom->vb_stride, dwOffset);
			PAPI::Particles::LITBUFF* pv = pv_start;

			if (m_RT_Flags.is(flRT_LiveUpdate))
			{
				auto PosArr = particles.pos_arr;
				auto PosIArr = particles.posI_arr;
				for (u32 i = 0; i < p_cnt; i++)
				{
					PosIArr[i].set(PosArr[i]);
				}
				auto RotArr = particles.rot_arr;
				auto RotIArr = particles.rotI_arr;
				for (u32 i = 0; i < p_cnt; i++)
				{
					RotIArr[i].set(RotArr[i]);
				}
				auto velArr = particles.vel_arr;
				auto velIArr = particles.velI_arr;
				for (u32 i = 0; i < p_cnt; i++)
				{
					velIArr[i].set(velArr[i]);
				}
				auto SizeArr = particles.size_arr;
				auto SizeIArr = particles.sizeI_arr;
				for (u32 i = 0; i < p_cnt; i++)
				{
					SizeIArr[i].set(SizeArr[i]);
				}
			} else
			{
				constexpr float tau = .1f;
				float dt = exp(-Device.fTimeDelta / tau);
				
				auto PosArr = particles.pos_arr;
				auto PosIArr = particles.posI_arr;
				for (u32 i = 0; i < p_cnt; i++)
				{
					PosIArr[i].inertion(PosArr[i], dt);
				}
				auto RotArr = particles.rot_arr;
				auto RotIArr = particles.rotI_arr;
				for (u32 i = 0; i < p_cnt; i++)
				{
					RotIArr[i] = RotArr[i];
				}
				auto velArr = particles.vel_arr;
				auto velIArr = particles.velI_arr;
				for (u32 i = 0; i < p_cnt; i++)
				{
					velIArr[i].inertion(velArr[i], dt);
				}
				auto SizeArr = particles.size_arr;
				auto SizeIArr = particles.sizeI_arr;
				for (u32 i = 0; i < p_cnt; i++)
				{
					SizeIArr[i].inertion(SizeArr[i], dt);
				}
			}

			// float r_x - +1 = 1
			// float r_y - +1 = 2
			// Fvector2 lt - +2 = 4
			// Fvector2 rb - +2 = 6
			// Fcolor FinalColor - +4 = 10
			size_t required_buff_size = p_cnt*10;
			thread_local xr_vector<float> buff(required_buff_size, {});
			if (buff.size() < required_buff_size)
			{
				buff.resize(required_buff_size);
			}
			float* r_x_arr = buff.data();
			float* r_y_arr = buff.data()+p_cnt;
			Fvector2* lt_arr = (Fvector2*)(buff.data()+(p_cnt*2));
			Fvector2* rb_arr = (Fvector2*)(buff.data()+(p_cnt*4));
			Fcolor* FinalColor = (Fcolor*)(buff.data()+(p_cnt*6));
			
			{
				auto SizeIArr = particles.sizeI_arr;
				auto sizeModArr = particles.sizeMod_arr;
				for (u32 i = 0; i < p_cnt; i++)
				{
					r_x_arr[i] = SizeIArr[i].x*sizeModArr[i].mod.x;
					VERIFY(r_x_arr[i] >= 0.f && r_x_arr[i] <= 1000.f);
					r_x_arr[i] *= 0.5f;
					r_y_arr[i] = SizeIArr[i].y*sizeModArr[i].mod.y;
					VERIFY(r_y_arr[i] >= 0.f && r_y_arr[i] <= 1000.f);
					r_y_arr[i] *= 0.5f;
				}
				if (m_Def->m_Flags.is(CPEDef::dfVelocityScale))
				{
					auto VelIArr = particles.velI_arr;
					for (u32 i = 0; i < p_cnt; i++)
					{
						auto Speed = VelIArr[i].magnitude();
						r_x_arr[i] += Speed*m_Def->m_VelocityScale.x;
						r_y_arr[i] += Speed*m_Def->m_VelocityScale.y;
					}
				}
			}

			{
				for (u32 i = 0; i < p_cnt; i++)
				{
					lt_arr[i].set(0.f, 0.f);
					rb_arr[i].set(1.f, 1.f);
				}
				
				if (m_Def->m_Flags.is(CPEDef::dfFramed))
				{
					auto FrameArr = particles.frame_arr;
					for (u32 i = 0; i < p_cnt; i++)
					{
						m_Def->m_Frame.CalculateTC(iFloor(float(FrameArr[i]) / 255.f), lt_arr[i], rb_arr[i]);
					}
				}
			}

			{
				auto ColorArr = particles.color_arr;
				auto ColorModArr = particles.colorMod_arr;
				for (u32 i = 0; i < p_cnt; i++)
				{
					auto Color = ColorArr[i];
					auto ColorMod = ColorModArr[i].mod;
					FinalColor[i].set(
						clampr(Color.r * ColorMod.x, 0.0f, 1.0f),
						clampr(Color.g * ColorMod.y, 0.0f, 1.0f),
						clampr(Color.b * ColorMod.z, 0.0f, 1.0f),
						clampr(Color.a * ColorMod.w, 0.0f, 1.0f)
						);
				}
			}

			
			if (m_Def->m_Flags.is(CPEDef::dfAlignToPath))
			{
				auto RotIArr = particles.rotI_arr;
				auto PosIArr = particles.posI_arr;
				for (u32 i = 0; i < p_cnt; i++)
				{
					float speed = particles.velI_arr[i].magnitude();
					if ((speed < EPS_S) && m_Def->m_Flags.is(CPEDef::dfWorldAlign))
					{
						Fmatrix	M;  	
						M.setXYZ(m_Def->m_APDefaultRotation);
						if (m_RT_Flags.is(flRT_XFORM))
						{
							Fvector p;
							m_XFORM.transform_tiny(p,PosIArr[i]);
							M.mulA_43(m_XFORM);
							FillSprite(pv,M.k,M.i,
								p,lt_arr[i],rb_arr[i],r_x_arr[i],r_y_arr[i],FinalColor[i].get(),RotIArr[i].x);
						}
						else
						{
							FillSprite(pv,M.k,M.i,
								PosIArr[i],lt_arr[i],rb_arr[i],r_x_arr[i],r_y_arr[i],FinalColor[i].get(),RotIArr[i].x);
						}
					}
					else if ((speed >= EPS_S) && m_Def->m_Flags.is(CPEDef::dfFaceAlign))
					{
						Fmatrix	M; M.identity();
						M.k.div(particles.velI_arr[i], speed);
						M.j.set(0.f, 1.f, 0.f);

						if (std::abs(M.j.dotproduct(M.k)) > .99f)
							M.j.set(0.f, 0.f, 1.f);

						M.i.crossproduct(M.j, M.k); M.i.normalize();
						M.j.crossproduct(M.k, M.i); M.j.normalize();
						if (m_RT_Flags.is(flRT_XFORM))
						{
							Fvector p;
							m_XFORM.transform_tiny(p,PosIArr[i]);
							M.mulA_43(m_XFORM);
							FillSprite(pv,M.j,M.i,
								p,lt_arr[i],rb_arr[i],r_x_arr[i],r_y_arr[i],FinalColor[i].get(),RotIArr[i].x);
						}
						else
						{
							FillSprite(pv,M.j,M.i,
								PosIArr[i],lt_arr[i],rb_arr[i],r_x_arr[i],r_y_arr[i],FinalColor[i].get(),RotIArr[i].x);
						}
					}
					else
					{
						Fvector dir;

						if (speed >= EPS_S)
						{
							dir.div(particles.velI_arr[i], speed);
						}
						else
						{
							dir.setHP(-m_Def->m_APDefaultRotation.y, -m_Def->m_APDefaultRotation.x);
						}

						if (m_RT_Flags.is(flRT_XFORM))
						{
							Fvector p,d;
							m_XFORM.transform_tiny(p,PosIArr[i]);
							m_XFORM.transform_dir(d,dir);
							FillSprite(pv,p,d,lt_arr[i],rb_arr[i],r_x_arr[i],r_y_arr[i],FinalColor[i].get(),RotIArr[i].x);
						}
						else
						{
							FillSprite(pv,
								PosIArr[i],dir,lt_arr[i],rb_arr[i],r_x_arr[i],r_y_arr[i],FinalColor[i].get(),RotIArr[i].x);
						}
					}
				}
			}
			else
			{
				if (m_RT_Flags.is(flRT_XFORM))
				{
					for (u32 i = 0; i < p_cnt; i++)
					{
						Fvector p;
						m_XFORM.transform_tiny(p,particles.posI_arr[i]);
						FillSprite(pv,RDEVICE.vCameraTop,RDEVICE.vCameraRight,
							p,lt_arr[i],rb_arr[i],r_x_arr[i],r_y_arr[i],FinalColor[i].get(),particles.rotI_arr[i].x);
					}
				}
				else
				{
					for (u32 i = 0; i < p_cnt; i++)
					{
						FillSprite(pv,RDEVICE.vCameraTop,RDEVICE.vCameraRight,
							particles.posI_arr[i],lt_arr[i],rb_arr[i],r_x_arr[i],r_y_arr[i],FinalColor[i].get(),
							particles.rotI_arr[i].x);
					}
				}
			}
			
			dwCount = u32(pv - pv_start) * 4;
			RCache.Vertex.Unlock(dwCount, geom->vb_stride);

			CHudInitializer initalizer(false, true);

			if (GetHudMode())
			{
				initalizer.SetHudMode();
				RCache.set_xform_view(Device.mView);
				RCache.set_xform_project(Device.mProject);
				ApplyTexgen(Device.mFullTransform);
			}


			RCache.set_xform_world(Fidentity);
			RCache.set_Geometry(geom);

			GRHI->StateManager->SetCullMode(m_Def->m_Flags.test(CPEDef::dfCulling) ? (m_Def->m_Flags.test(CPEDef::dfCullCCW) ? ERHI_CULLMODE::BACK : ERHI_CULLMODE::FRONT) : ERHI_CULLMODE::NONE);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, dwOffset, 0, dwCount, 0, dwCount / 2);
			GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);

			if (GetHudMode())
			{
				initalizer.SetDefaultMode();
				RCache.set_xform_view(Device.mView);
				RCache.set_xform_project(Device.mProject);
				ApplyTexgen(Device.mFullTransform);
			}
		}
	}
}
#endif

