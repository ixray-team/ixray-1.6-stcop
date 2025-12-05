//----------------------------------------------------
// file: PSObject.cpp
//----------------------------------------------------
#include "stdafx.h"
#include "ParticlesObject.h"

#include "../xrEngine/defines.h"
#include "../Include/xrRender/RenderVisual.h"
#include "../Include/xrRender/ParticleCustom.h"
#include "../xrEngine/Render.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrEngine/Environment.h"
#include "GamePersistent.h"
CParticlesObject::CParticlesObject(LPCSTR p_name, BOOL bAutoRemove, bool destroy_on_game_load) :
	m_destroy_on_game_load(destroy_on_game_load), m_bAutoRemove(bAutoRemove)
{
	renderable.pROS_Allowed = FALSE;

	m_iLifeTime = int_max;

	m_bDead = FALSE;

	dwLastTime = Device.dwTimeGlobal;

	m_bLooped = false;
	m_bStopping = false;
	float time_limit = 1.0f;

	if (!g_dedicated_server)
	{
		// create visual
		renderable.visual = Render->model_CreateParticles(p_name);
		if (renderable.visual != nullptr)
		{
			IParticleCustom* V = renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL;  VERIFY(V);
			time_limit = V->GetTimeLimit();
		}
	}

	if (time_limit > 0.f)
	{
		m_iLifeTime = iFloor(time_limit * 1000.f);
	}
	else
	{
		if (bAutoRemove)
		{
			R_ASSERT3(!m_bAutoRemove, "Can't set auto-remove flag for looped particle system.", p_name);
		}
		else
		{
			m_iLifeTime = 0;
			m_bLooped = true;
		}
	}


	// spatial
	SpatialComponent->spatial.type = 0;
	SpatialComponent->spatial.sector = nullptr;
}

extern ENGINE_API xr_atomic_bool g_bRendering;
CParticlesObject::~CParticlesObject()
{
	VERIFY(!g_bRendering);
	ISpatialOwner::spatial_unregister();
}

const shared_str CParticlesObject::Name()
{
	if(g_dedicated_server)	return "";

	IParticleCustom* V	= renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL; VERIFY(V);
	return (V) ? V->Name() : "";
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
	if (g_dedicated_server || renderable.visual == nullptr)
		return;

	IParticleCustom* V = renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL; VERIFY(V);
	if (bHudMode)
		V->SetHudMode(bHudMode);

	V->Play();

	m_bStopping = false;
}

void CParticlesObject::play_at_pos(const Fvector& pos, BOOL xform)
{
	if (g_dedicated_server || renderable.visual == nullptr)
		return;

	IParticleCustom* V			= renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL; VERIFY(V);
	Fmatrix m; m.translate		(pos); 
	V->UpdateParent				(m,zero_vel,xform);
	V->Play						();

	m_bStopping					= false;
}

void CParticlesObject::Stop(BOOL bDefferedStop)
{
	if (g_dedicated_server || renderable.visual == nullptr)
		return;

	IParticleCustom* V			= renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL; VERIFY(V);
	V->Stop						(bDefferedStop);
	m_bStopping					= true;
}

void CParticlesObject::Update(u32 _dt)
{
	if (renderable.pROS)			::Render->ros_destroy(renderable.pROS);	//. particles doesn't need ROS

	m_iLifeTime -= _dt;

	// remove???
	if (m_bDead) return;
	if (m_bAutoRemove && m_iLifeTime <= 0)
		PSI_destroy();

	if (m_bDead)					
		return;

	if (IParticleCustom* V = renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL)
		V->OnFrame(_dt);

	// UpdateSpatial (+ workaround occasional bug inside particle-system)
	vis_data& vis = renderable.visual->getVisData();
	if (_valid(vis.sphere))
	{
		Fvector	P;	float	R;
		renderable.xform.transform_tiny(P, vis.sphere.P);
		R = vis.sphere.R;
		if (0 == SpatialComponent->spatial.type)
		{
			// First 'valid' update - register
			SpatialComponent->spatial.type = STYPE_PARTICLE;
			SpatialComponent->spatial.sphere.set(P, R);
			spatial_register();
		}
		else
		{
			bool bMove = false;
			if (!P.similar(SpatialComponent->spatial.sphere.P, EPS_L * 10.f))		bMove = true;
			if (!fsimilar(R, SpatialComponent->spatial.sphere.R, 0.15f))			bMove = true;

			if (bMove)
			{
				SpatialComponent->spatial.sphere.set(P, R);
				spatial_move();
			}
		}
	}
}

void CParticlesObject::SetXFORM			(const Fmatrix& m)
{
	if(g_dedicated_server)		return;

	IParticleCustom* V	= renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL; VERIFY(V);
	V->UpdateParent		(m,zero_vel,TRUE);
	renderable.xform.set(m);
}

void CParticlesObject::SetLiveUpdate(BOOL b)
{
	if(g_dedicated_server)		return;

	if (renderable.visual)
	{
		IParticleCustom* V = renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL; VERIFY(V);
		V->SetLiveUpdate(b);
	}
}

BOOL CParticlesObject::GetLiveUpdate()
{
	if(g_dedicated_server || renderable.visual == nullptr)
		return 0;

	IParticleCustom* V	= renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL; VERIFY(V);
	return V->GetLiveUpdate();
}

void CParticlesObject::UpdateParent		(const Fmatrix& m, const Fvector& vel)
{
	if(g_dedicated_server || renderable.visual == nullptr)
		return;

	IParticleCustom* V	= renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL; VERIFY(V);
	V->UpdateParent		(m,vel,FALSE);
}

Fvector& CParticlesObject::Position		()
{
	if(g_dedicated_server) 
	{
		static Fvector _pos = zero_vel;
		return _pos;
	}
	vis_data &vis = renderable.visual->getVisData();
	return vis.sphere.P;
}

void CParticlesObject::renderable_Render	()
{
	VERIFY					(renderable.visual);

	::Render->set_Transform	(&renderable.xform);
	::Render->add_Visual	(renderable.visual);
}

bool CParticlesObject::IsAutoRemove			()
{
	if(m_bAutoRemove) return true;
	else return false;
}
void CParticlesObject::SetAutoRemove		(bool auto_remove)
{
	VERIFY(!IsLooped());
	m_bAutoRemove = auto_remove;
}

//играются ли партиклы, отличается от PSI_Alive, тем что после
//остановки Stop партиклы могут еще доигрывать анимацию IsPlaying = true
bool CParticlesObject::IsPlaying()
{
	if(g_dedicated_server)		return false;

	IParticleCustom* V	= renderable.visual ? renderable.visual->dcast_ParticleCustom() : NULL;
	VERIFY(V);
	return !!V->IsPlaying();
} 

void CParticlesObject::PSI_destroy()
{
	m_bDead = TRUE;
	m_iLifeTime = 0;
	m_NeedDestroy = true;
}

void CParticlesObject::PSI_internal_delete()
{
	CParticlesObject* self = this;
	xr_delete(self);
}