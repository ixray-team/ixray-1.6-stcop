#include "StdAfx.h"
#include "ParticlesObject.h"
#include "../Include/xrRender/RenderVisual.h"
#include "../Include/xrRender/ParticleCustom.h"
#include "GamePersistent.h"

CParticlesObject::CParticlesObject(LPCSTR p_name, BOOL bAutoRemove, bool destroy_on_game_load) :
	m_bAutoRemove(bAutoRemove), m_destroy_on_game_load(destroy_on_game_load)
{
	renderable.pROS_Allowed = FALSE;
	dwLastTime = Device.dwTimeGlobal;

	float time_limit = 1.0f;

	if (!g_dedicated_server)
	{
		// create visual
		renderable.visual = Render->model_CreateParticles(p_name);
		if (renderable.visual != nullptr)
		{
			IParticleCustom* V = renderable.visual->dcast_ParticleCustom(); VERIFY(V);
			time_limit = V->GetTimeLimit();
		}
	}

	if (time_limit > 0.f)
		m_iLifeTime = iFloor(time_limit * 1000.f);
	else
	{
		if (bAutoRemove)
			R_ASSERT3(!m_bAutoRemove, "Can't set auto-remove flag for looped particle system.", p_name);
		else
		{
			m_iLifeTime = 0;
			m_bLooped = true;
		}
	}


	// spatial
	SpatialComponent->spatial.type = ESPATIAL_TYPE::NONE;
	SpatialComponent->spatial.sector = nullptr;
	renderable.pROS_Allowed = FALSE;
}

extern ENGINE_API xr_atomic_bool g_bRendering;
CParticlesObject::~CParticlesObject()
{
	VERIFY(!g_bRendering);
	ISpatialOwner::spatial_unregister();
}

const shared_str CParticlesObject::Name()
{
	if(g_dedicated_server || renderable.visual == nullptr)	return "";

	IParticleCustom* V = renderable.visual->dcast_ParticleCustom(); VERIFY(V);
	return V->Name();
}

PAPI::ParticleAction* CParticlesObject::FindAction(shared_str PEName, PAPI::PActionEnum type)
{
	if(g_dedicated_server)	return nullptr;

	IParticleCustom* V	= smart_cast<IParticleCustom*>(renderable.visual); VERIFY(V);
	return (V) ? V->FindPA(PEName, type) : nullptr;
}

xr_shared_ptr<CParticlesObject> Particles::Details::Create(LPCSTR p_name, BOOL bAutoRemove, bool remove_on_game_load)
{
	auto Particle = xr_make_shared<CParticlesObject>(p_name, bAutoRemove, remove_on_game_load);
	GamePersistent().ps_active_deffer.push_back(Particle);
	return Particle;
}

//----------------------------------------------------
void CParticlesObject::Play(bool bHudMode)
{
	if (g_dedicated_server || renderable.visual == nullptr || m_bPlaying)
		return;

	IParticleCustom* V = renderable.visual->dcast_ParticleCustom(); VERIFY(V);
	V->SetHudMode(bHudMode);
	V->Play();

	if(!IsLooped())
		m_iLifeTime = iFloor(V->GetTimeLimit() * 1000.f);
	V->UpdateCache();
	m_bPlaying = true;
}

void CParticlesObject::play_at_pos(const Fvector& pos, BOOL xform)
{
	if (g_dedicated_server || renderable.visual == nullptr || m_bPlaying)
		return;

	IParticleCustom* V = renderable.visual->dcast_ParticleCustom(); VERIFY(V);
	Fmatrix m; m.translate(pos); 
	V->UpdateParent(m,zero_vel,xform);
	V->Play();

	if (!IsLooped())
		m_iLifeTime = iFloor(V->GetTimeLimit() * 1000.f);
	V->UpdateCache();
	m_bPlaying = true;
}

void CParticlesObject::Stop(BOOL bDefferedStop)
{
	if (g_dedicated_server || renderable.visual == nullptr || !m_bPlaying)
		return;

	IParticleCustom* V = renderable.visual->dcast_ParticleCustom(); VERIFY(V);
	V->Stop(bDefferedStop);

	if(!bDefferedStop)
		m_bPlaying = false;
}

void CParticlesObject::Update(u32 _dt, CFrustum& viewbase)
{
	if (m_NeedDestroy || (!m_bPlaying && !m_bAutoRemove)) return;
	PROF_EVENT(__FUNCTION__);
	if (m_bAutoStop && m_bPlaying && !IsPlaying())
		Stop(FALSE);

	m_iLifeTime -= _dt;
	if (m_bAutoRemove && !IsAlive())
		Destroy();

	if (IParticleCustom* V = renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL)
	{
		V->OnFrame(_dt);

		// UpdateSpatial (+ workaround occasional bug inside particle-system)
		vis_data& vis = renderable.visual->getVisData();
		if (_valid(vis.sphere))
		{
			Fvector	P; float R = vis.sphere.R;
			renderable.xform.transform_tiny(P, vis.sphere.P);
			if (ESPATIAL_TYPE::NONE == SpatialComponent->spatial.type)
			{
				// First 'valid' update - register
				SpatialComponent->spatial.type = ESPATIAL_TYPE::PARTICLE;
				SpatialComponent->spatial.sphere.set(P, R);
				spatial_register();
			}
			else
			{
				if (!P.similar(SpatialComponent->spatial.sphere.P, EPS_L * 10.f) || !fsimilar(R, SpatialComponent->spatial.sphere.R, 0.15f))
				{
					SpatialComponent->spatial.sphere.set(P, R);
					spatial_move();
				}
			}


			if (RDEVICE.vCameraPosition_saved.distance_to_sqr(P) > _sqr(g_pGamePersistent->Environment().CurrentEnv->fog_distance + vis.sphere.R))
				return;
			
			if (!viewbase.testSphere_dirty(P, R))
				return;
		}
		V->UpdateCache();
	}
}

void CParticlesObject::SetXFORM(const Fmatrix& m)
{
	if(g_dedicated_server || renderable.visual == nullptr) return;

	IParticleCustom* V = renderable.visual->dcast_ParticleCustom(); VERIFY(V);
	V->UpdateParent(m,zero_vel,TRUE);
	renderable.xform.set(m);
}

void CParticlesObject::SetLiveUpdate(BOOL b)
{
	if (g_dedicated_server || renderable.visual == nullptr)
		return;

	IParticleCustom* V = renderable.visual->dcast_ParticleCustom(); VERIFY(V);
	V->SetLiveUpdate(b);
}

bool CParticlesObject::GetLiveUpdate()
{
	if(g_dedicated_server || renderable.visual == nullptr)
		return false;

	IParticleCustom* V = renderable.visual->dcast_ParticleCustom(); VERIFY(V);
	return !!V->GetLiveUpdate();
}

void CParticlesObject::UpdateParent(const Fmatrix& m, const Fvector& vel)
{
	if(g_dedicated_server || renderable.visual == nullptr)
		return;

	IParticleCustom* V = renderable.visual->dcast_ParticleCustom(); VERIFY(V);
	V->UpdateParent(m,vel,FALSE);
}

Fvector& CParticlesObject::Position()
{
	if(g_dedicated_server || renderable.visual == nullptr)
	{
		static Fvector _pos = zero_vel;
		return _pos;
	}
	vis_data &vis = renderable.visual->getVisData();
	return vis.sphere.P;
}

void CParticlesObject::renderable_Render()
{
	if (g_dedicated_server || renderable.visual == nullptr || m_NeedDestroy || !m_bPlaying)
		return;

	::Render->set_Transform	(&renderable.xform);
	::Render->add_Visual	(renderable.visual);
}

//играются ли партиклы, отличается от IsAlive, тем что после
//остановки Stop партиклы могут еще доигрывать анимацию IsPlaying = true
bool CParticlesObject::IsPlaying()
{
	if(g_dedicated_server || renderable.visual == nullptr) return false;

	IParticleCustom* V = renderable.visual->dcast_ParticleCustom(); VERIFY(V);
	return !!V->IsPlaying();
}

u32 CParticlesObject::GetSpriteCount()
{
	IParticleCustom* V = renderable.visual->dcast_ParticleCustom(); VERIFY(V);

	return V->SpriteCount();
}