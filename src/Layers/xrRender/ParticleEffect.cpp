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

void PS::OnEffectParticleBirth(void* owner, u32 , PAPI::Particle& m, u32 )
{
	CParticleEffect* PE = static_cast<CParticleEffect*>(owner); VERIFY(PE);
    CPEDef* PED			= PE->GetDefinition(); 
    if (PED){
        if (PED->m_Flags.is(CPEDef::dfRandomFrame))
            m.frame	= (u16)iFloor(Random.randI(PED->m_Frame.m_iFrameCount)*255.f);
        if (PED->m_Flags.is(CPEDef::dfAnimated)&&PED->m_Flags.is(CPEDef::dfRandomPlayback)&&Random.randI(2))
            m.flags.set(Particle::ANIMATE_CCW,TRUE);
    }
}
void PS::OnEffectParticleDead(void* , u32 , PAPI::Particle& , u32 )
{
//	CPEDef* PE = static_cast<CPEDef*>(owner);
}
//------------------------------------------------------------------------------
// class CParticleEffect
//------------------------------------------------------------------------------
CParticleEffect::CParticleEffect()
{
	m_RT_Flags.zero			();
	m_Def					= 0;
	m_fElapsedLimit			= 0.f;
	m_MemDT					= 0;
	m_InitialPosition.set	(0,0,0);
	m_DestroyCallback		= 0;
	m_CollisionCallback		= 0;
	m_XFORM.identity		();
}
CParticleEffect::~CParticleEffect()
{
	xrCriticalSectionGuard guard(&onframe_lock);
	// Log					("--- destroy PE");
	OnDeviceDestroy			();
}

void CParticleEffect::Play()
{
	xrCriticalSectionGuard guard(&onframe_lock);
	m_RT_Flags.set		(flRT_DefferedStop,FALSE);
	m_RT_Flags.set		(flRT_Playing,TRUE);
	Pholder.PlayEffect();
}
void CParticleEffect::Stop(BOOL bDefferedStop)
{
	xrCriticalSectionGuard guard(&onframe_lock);
	Pholder.StopEffect(bDefferedStop);
	if (bDefferedStop){
		m_RT_Flags.set	(flRT_DefferedStop,TRUE);
	}else{
		m_RT_Flags.set	(flRT_Playing,FALSE);
	}
}
void CParticleEffect::RefreshShader()
{
	OnDeviceDestroy();
	OnDeviceCreate();
}

void CParticleEffect::UpdateParent(const Fmatrix& m, const Fvector& velocity, BOOL bXFORM)
{
	xrCriticalSectionGuard guard(&onframe_lock);
	m_RT_Flags.set			(flRT_XFORM, bXFORM);
	if (bXFORM)				m_XFORM.set	(m);
	else{
		m_InitialPosition	= m.c;
		Pholder.Transform(m,velocity);
	}
}

void CParticleEffect::OnFrame(u32 frame_dt)
{
	xrCriticalSectionGuard guard(&onframe_lock);
	if (0==m_RT_Flags.is(flRT_LiveUpdate))
	{
		if (m_Def && m_RT_Flags.is(flRT_Playing)) 
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

				PAPI::Particle* particles;
				u32 p_cnt;
				Pholder.GetParticles(particles, p_cnt);

				// our actions
				if (m_Def->m_Flags.is(CPEDef::dfFramed | CPEDef::dfAnimated))	m_Def->ExecuteAnimate(particles, p_cnt, fDT_STEP);
				if (m_Def->m_Flags.is(CPEDef::dfCollision)) 				m_Def->ExecuteCollision(particles, p_cnt, fDT_STEP, this, m_CollisionCallback);

				//-move action
				if (p_cnt)
				{
					vis.box.invalidate();
					float p_size = 0.f;
					for (u32 i = 0; i < p_cnt; i++) 
					{
						Particle& m = particles[i];
						if(!RImplementation.ViewBase.testSphere_dirty(vis.sphere.P, vis.sphere.R))
						{
							m.posI.set(m.pos);
							m.rotI.set(m.rot);
							m.velI.set(m.vel);
							m.sizeI.set(m.size);
						}
						vis.box.modify((Fvector&)m.pos);
						if (m.size.x > p_size) p_size = m.size.x;
						if (m.size.y > p_size) p_size = m.size.y;
						if (m.size.z > p_size) p_size = m.size.z;
					}
					vis.box.grow(p_size);
					vis.box.getsphere(vis.sphere.P, vis.sphere.R);
				}
				if (m_RT_Flags.is(flRT_DefferedStop) && (0 == p_cnt)) 
				{
					m_RT_Flags.set(flRT_Playing | flRT_DefferedStop, FALSE);
					break;
				}
			}
		}
		else 
		{
			vis.box.set(m_InitialPosition, m_InitialPosition);
			vis.box.grow(EPS_L);
			vis.box.getsphere(vis.sphere.P, vis.sphere.R);
		}
	}
	else
	{
		if (m_Def && m_RT_Flags.is(flRT_Playing))
		{
			Pholder.Update(Device.fTimeDelta);
			PAPI::Particle* particles = nullptr;
			u32 p_cnt = 0;
			Pholder.GetParticles(particles, p_cnt);
			if (!particles) return;
			// our actions
			if (m_Def->m_Flags.is(CPEDef::dfFramed | CPEDef::dfAnimated))	m_Def->ExecuteAnimate(particles, p_cnt, Device.fTimeDelta);
			if (m_Def->m_Flags.is(CPEDef::dfCollision)) 				m_Def->ExecuteCollision(particles, p_cnt, Device.fTimeDelta, this, m_CollisionCallback);

			//-move action
			if (p_cnt)
			{
				vis.box.invalidate();
				float p_size = 0.f;
				for (u32 i = 0; i < p_cnt; i++)
				{
					Particle& m = particles[i];

					if (!_valid(m.pos))continue;
					vis.box.modify(Fvector(m.pos));
					if (m.size.x > p_size) p_size = m.size.x;
					if (m.size.y > p_size) p_size = m.size.y;
					if (m.size.z > p_size) p_size = m.size.z;
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
				m_RT_Flags.set(flRT_Playing | flRT_DefferedStop, FALSE);
		}
		else
		{
			vis.box.set(m_InitialPosition, m_InitialPosition);
			vis.box.grow(EPS_L);
			vis.box.getsphere(vis.sphere.P, vis.sphere.R);
		}
	}
}

BOOL CParticleEffect::Compile(CPEDef* def)
{
	xrCriticalSectionGuard guard(&onframe_lock);
	m_Def 						= def;
	if (m_Def){
		// refresh shader
		RefreshShader			();

		// append actions
		IReader F				(m_Def->m_Actions.pointer(),m_Def->m_Actions.size());
        Pholder.LoadActions		(F);
        Pholder.SetMaxParticles	(m_Def->m_MaxParticles);
        Pholder.SetCallback		(OnEffectParticleBirth,OnEffectParticleDead,this,0);
		// time limit
		if (m_Def->m_Flags.is(CPEDef::dfTimeLimit))
			m_fElapsedLimit 	= m_Def->m_fTimeLimit;
	}
	if (def)	shader			= def->m_CachedShader;
	return TRUE;
}

void CParticleEffect::SetBirthDeadCB(PAPI::OnBirthParticleCB bc, PAPI::OnDeadParticleCB dc, void* owner, u32 p)
{
	xrCriticalSectionGuard guard(&onframe_lock);
	Pholder.SetCallback		(bc,dc,owner,p);
}

u32 CParticleEffect::ParticlesCount()
{
	xrCriticalSectionGuard guard(&onframe_lock);
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
void CParticleEffect::Copy(dxRender_Visual* )
{
	FATAL	("Can't duplicate particle system - NOT IMPLEMENTED");
}

void CParticleEffect::OnDeviceCreate()
{
	xrCriticalSectionGuard guard(&onframe_lock);
	if (m_Def){
		if (m_Def->m_Flags.is(CPEDef::dfSprite)){
			geom.create			(FVF::F_LIT, RCache.Vertex.Buffer(), RCache.QuadIB);
			if (m_Def) shader	= m_Def->m_CachedShader;
		}
	}
}

void CParticleEffect::OnDeviceDestroy()
{
	xrCriticalSectionGuard guard(&onframe_lock);
	if (m_Def){
		if (m_Def->m_Flags.is(CPEDef::dfSprite)){
			geom.destroy		();
			shader.destroy		();
		}    
	}
}
struct LITF
{
	struct
	{
		Fvector p; u32 color; Fvector2 t;
	} buff[4];
};
//----------------------------------------------------
ICF void FillSprite	(LITF*& pv, const Fvector& T, const Fvector& R, const Fvector& pos, const Fvector2& lt, const Fvector2& rb, float r1, float r2, u32 clr, float angle)
{
	float sa = std::sin(angle);  
	float ca = std::cos(angle);  
	Fvector Vr, Vt;
	Vr.x = T.x*r1*sa+R.x*r1*ca;
	Vr.y = T.y*r1*sa+R.y*r1*ca;
	Vr.z = T.z*r1*sa+R.z*r1*ca;
	Vt.x = T.x*r2*ca-R.x*r2*sa;
	Vt.y = T.y*r2*ca-R.y*r2*sa;
	Vt.z = T.z*r2*ca-R.z*r2*sa;

	Fvector a,b,c,d;
	a.sub(Vt,Vr);
	b.add(Vt,Vr);
	c.invert(a);
	d.invert(b);
	*pv =
	{
		d+pos, clr, lt.x,rb.y,
		a+pos, clr, lt.x,lt.y,
		c+pos, clr, rb.x,rb.y,
		b+pos, clr, rb.x,lt.y
	};
	pv++;
}

ICF void FillSprite	(LITF*& pv, const Fvector& pos, const Fvector& dir, const Fvector2& lt, const Fvector2& rb, float r1, float r2, u32 clr, float angle)
{
	float sa = std::sin(angle);  
	float ca = std::cos(angle);  
	const Fvector& T = dir;
	Fvector R; 	R.crossproduct(T,RDEVICE.vCameraDirection).normalize_safe();
	Fvector Vr, Vt;
	Vr.x = T.x*r1*sa+R.x*r1*ca;
	Vr.y = T.y*r1*sa+R.y*r1*ca;
	Vr.z = T.z*r1*sa+R.z*r1*ca;
	Vt.x = T.x*r2*ca-R.x*r2*sa;
	Vt.y = T.y*r2*ca-R.y*r2*sa;
	Vt.z = T.z*r2*ca-R.z*r2*sa;

	Fvector a,b,c,d;
	a.sub(Vt,Vr);
	b.add(Vt,Vr);
	c.invert(a);
	d.invert(b);
	*pv =
	{
		d+pos, clr, lt.x,rb.y,
		a+pos, clr, lt.x,lt.y,
		c+pos, clr, rb.x,rb.y,
		b+pos, clr, rb.x,lt.y
	};
	pv++;
}

void CParticleEffect::Render(float )
{
	PROF_EVENT(__FUNCTION__);
	xrCriticalSectionGuard guard(&onframe_lock);

#ifndef _EDITOR
	Fvector c = vis.sphere.P;
	m_XFORM.transform_tiny(c);

	if (Device.vCameraPosition.distance_to_sqr(c) > _sqr(g_pGamePersistent->Environment().CurrentEnv->fog_distance + vis.sphere.R))
		return;
#endif

	u32 dwOffset,dwCount;
	// Get a pointer to the particles in gp memory
    PAPI::Particle* particles;
    u32 p_cnt;
	Pholder.GetParticles(particles,p_cnt);

	if(p_cnt>0)
	{
		if (m_Def&&m_Def->m_Flags.is(CPEDef::dfSprite))
		{
			LITF* pv_start = (LITF*)RCache.Vertex.Lock(p_cnt*4*4,geom->vb_stride,dwOffset);
			LITF* pv = pv_start;

			for(u32 i = 0; i < p_cnt; i++)
			{
				PAPI::Particle &m = particles[i];

				if (m_RT_Flags.is(flRT_LiveUpdate))
				{
					m.posI.set(m.pos);
					m.rotI.set(m.rot);
					m.velI.set(m.vel);
					m.sizeI.set(m.size);
				}
				else
				{
					float dt = 1.f - 10.f * Device.fTimeDelta;
					clamp(dt, 0.f, 0.99f);
					m.posI.inertion(m.pos, dt);
					m.rotI.inertion(m.rot, dt);
					m.velI.inertion(m.vel, dt);
					m.sizeI.inertion(m.size, dt);
				}

				Fvector2 lt,rb;
				lt.set(0.f,0.f);
				rb.set(1.f,1.f);

				if (m_Def->m_Flags.is(CPEDef::dfFramed))
					m_Def->m_Frame.CalculateTC(iFloor(float(m.frame)/255.f),lt,rb);

				float r_x = m.sizeI.x*0.5f;
				float r_y = m.sizeI.y*0.5f;
				if (m_Def->m_Flags.is(CPEDef::dfVelocityScale))
				{
					float speed	= m.velI.magnitude();
					r_x += speed*m_Def->m_VelocityScale.x;
					r_y += speed*m_Def->m_VelocityScale.y;
				}
				if (m_Def->m_Flags.is(CPEDef::dfAlignToPath))
				{
					float speed	= m.velI.magnitude();
                    if ((speed<EPS_S)&&m_Def->m_Flags.is(CPEDef::dfWorldAlign))
					{
                    	Fmatrix	M;  	
                        M.setXYZ(m_Def->m_APDefaultRotation);
                        if (m_RT_Flags.is(flRT_XFORM))
						{
                            Fvector p;
                            m_XFORM.transform_tiny(p,m.posI);
	                        M.mulA_43(m_XFORM);
                            FillSprite(pv,M.k,M.i,p,lt,rb,r_x,r_y,m.color,m.rotI.x);
                        }
						else
                            FillSprite(pv,M.k,M.i,m.posI,lt,rb,r_x,r_y,m.color,m.rotI.x);

                    }
					else if ((speed>=EPS_S)&&m_Def->m_Flags.is(CPEDef::dfFaceAlign))
					{
                    	Fmatrix	M; M.identity();
                        M.k.div(m.velI,speed);
                        M.j.set(0.f,1.f,0.f);

						if (std::abs(M.j.dotproduct(M.k))>.99f)
							M.j.set(0.f,0.f,1.f);

                        M.i.crossproduct(M.j,M.k); M.i.normalize();
                        M.j.crossproduct(M.k,M.i); M.j.normalize();
                        if (m_RT_Flags.is(flRT_XFORM))
						{
                            Fvector p;
                            m_XFORM.transform_tiny(p,m.posI);
	                        M.mulA_43(m_XFORM);
                            FillSprite(pv,M.j,M.i,p,lt,rb,r_x,r_y,m.color,m.rotI.x);
                        }
						else
                            FillSprite(pv,M.j,M.i,m.posI,lt,rb,r_x,r_y,m.color,m.rotI.x);
                    }
					else
					{
						Fvector dir;

                        if (speed>=EPS_S)
							dir.div(m.velI,speed);
                        else
							dir.setHP(-m_Def->m_APDefaultRotation.y,-m_Def->m_APDefaultRotation.x);

                        if (m_RT_Flags.is(flRT_XFORM))
						{
                            Fvector p,d;
                            m_XFORM.transform_tiny(p,m.posI);
                            m_XFORM.transform_dir(d,dir);
                            FillSprite(pv,p,d,lt,rb,r_x,r_y,m.color,m.rotI.x);
                        }
						else
                            FillSprite(pv,m.posI,dir,lt,rb,r_x,r_y,m.color,m.rotI.x);
                    }
				}
				else
				{
					if (m_RT_Flags.is(flRT_XFORM))
					{
						Fvector p;
						m_XFORM.transform_tiny(p,m.posI);
						FillSprite(pv,RDEVICE.vCameraTop,RDEVICE.vCameraRight,p,lt,rb,r_x,r_y,m.color,m.rotI.x);
					}
					else
						FillSprite(pv,RDEVICE.vCameraTop,RDEVICE.vCameraRight,m.posI,lt,rb,r_x,r_y,m.color,m.rotI.x);
				}
			}
			dwCount = u32(pv-pv_start)*4;
			RCache.Vertex.Unlock(dwCount,geom->vb_stride);

			CHudInitializer initalizer(false);

			if (GetHudMode())
			{
				initalizer.SetHudMode();
				RImplementation.rmNear();
				RCache.set_xform_view(Device.mView);
				RCache.set_xform_project(Device.mProject);
				ApplyTexgen(Device.mFullTransform);
			}


			RCache.set_xform_world(Fidentity);
			RCache.set_Geometry(geom);

			GRHI->StateManager->SetCullMode(m_Def->m_Flags.test(CPEDef::dfCulling) ? (m_Def->m_Flags.test(CPEDef::dfCullCCW) ? ERHI_CULLMODE::BACK : ERHI_CULLMODE::FRONT) : ERHI_CULLMODE::NONE);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,dwOffset,0,dwCount,0,dwCount/2);
			GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);

			if (GetHudMode())
			{
				RImplementation.rmNormal();
				initalizer.SetDefaultMode();
				RCache.set_xform_view(Device.mView);
				RCache.set_xform_project(Device.mProject);
				ApplyTexgen(Device.mFullTransform);
			}
		}
	}
}
