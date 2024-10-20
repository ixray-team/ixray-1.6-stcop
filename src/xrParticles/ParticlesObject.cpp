//----------------------------------------------------
// file: PSObject.cpp
//----------------------------------------------------
#include "stdafx.h"
#pragma hdrstop

#include "ParticlesObject.h"
#include "../xrEngine/defines.h"
#include "../Include/xrRender/RenderVisual.h"
#include "../Include/xrRender/ParticleCustom.h"
#include "../xrEngine/render.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrEngine/environment.h"

xr_task_group ParticleObjectTasks;

PARTICLES_API const Fvector zero_vel = {0.f,0.f,0.f};
xr_list<CParticlesObject*> CParticlesObject::AllParticleObjects;

CParticlesObject::CParticlesObject	(LPCSTR p_name, BOOL bAutoRemove, bool destroy_on_game_load) :
	inherited				(destroy_on_game_load)
{
	Init					(p_name,0,bAutoRemove);
}

void CParticlesObject::Init	(LPCSTR p_name, IRender_Sector* S, BOOL bAutoRemove)
{
	m_bLooped				= false;
	m_bStopping				= false;
	m_bAutoRemove			= bAutoRemove;
	float time_limit		= 1.0f;

	if(!g_dedicated_server)
	{
		// create visual
		renderable.visual		= Render->model_CreateParticles(p_name);
		if (renderable.visual != nullptr)
		{
			IParticleCustom* V = smart_cast<IParticleCustom*>(renderable.visual);  VERIFY(V);
			time_limit = V->GetTimeLimit();
		}
	}

	if(time_limit > 0.f)
	{
		m_iLifeTime			= iFloor(time_limit*1000.f);
	}
	else
	{
		if(bAutoRemove)
		{
			R_ASSERT3			(!m_bAutoRemove,"Can't set auto-remove flag for looped particle system.",p_name);
		}
		else
		{
			m_iLifeTime = 0;
			m_bLooped = true;
		}
	}


	// spatial
	spatial.type			= 0;
	spatial.sector			= S;
	
	// sheduled
	shedule.t_min			= 20;
	shedule.t_max			= 50;
	shedule_register		();

	AllParticleObjects.push_back(this);

	dwLastTime = Device.dwTimeGlobal;
}

//----------------------------------------------------
CParticlesObject::~CParticlesObject()
{
	// FX: DX9 выгружает мусор при рестарте девайса
	// а партиклы могли не обновиться...
	// Держите пролаг в лицо 
	WaitForParticles();

	AllParticleObjects.remove(this);
}

void CParticlesObject::UpdateAllAsync()
{
	for (CParticlesObject* particle : AllParticleObjects)
	{
		auto UpdateParticle = [particle]()
		{
			PROF_THREAD("Particles Worker")
			u32 dt = Device.dwTimeGlobal - particle->dwLastTime;
			IParticleCustom* V = smart_cast<IParticleCustom*>(particle->renderable.visual);
			
			if (V == nullptr)
				return;

			PROF_EVENT("Particle OnFrame")
			V->OnFrame(dt);

			particle->dwLastTime = Device.dwTimeGlobal;
		};

		if (particle->m_bDead)
			continue;

		if (psDeviceFlags.test(mtParticles))
		{
			ParticleObjectTasks.run(UpdateParticle);
		}
		else
		{
			UpdateParticle();
		}
	}
}

void CParticlesObject::UpdateSpatial()
{
	if(g_dedicated_server)		return;

	// spatial	(+ workaround occasional bug inside particle-system)
	vis_data &vis = renderable.visual->getVisData();
	if (_valid(vis.sphere))
	{
		Fvector	P;	float	R;
		renderable.xform.transform_tiny	(P,vis.sphere.P);
		R								= vis.sphere.R;
		if (0==spatial.type)	{
			// First 'valid' update - register
			spatial.type			= STYPE_PARTICLE;
			spatial.sphere.set		(P,R);
			spatial_register		();
		} else {
			BOOL	bMove			= FALSE;
			if		(!P.similar(spatial.sphere.P,EPS_L*10.f))		bMove	= TRUE;
			if		(!fsimilar(R,spatial.sphere.R,0.15f))			bMove	= TRUE;
			if		(bMove)			{
				spatial.sphere.set	(P, R);
				spatial_move		();
			}
		}
	}
}

const shared_str CParticlesObject::Name()
{
	if(g_dedicated_server)	return "";

	IParticleCustom* V	= smart_cast<IParticleCustom*>(renderable.visual); VERIFY(V);
	return (V) ? V->Name() : "";
}

//----------------------------------------------------
void CParticlesObject::Play(bool bHudMode)
{
	if (g_dedicated_server || renderable.visual == nullptr)
		return;

	IParticleCustom* V = smart_cast<IParticleCustom*>(renderable.visual); VERIFY(V);
	if (bHudMode)
		V->SetHudMode(bHudMode);

	V->Play();
	dwLastTime = Device.dwTimeGlobal - 33ul;

	PerformAllTheWork();
	m_bStopping = false;
}

void CParticlesObject::play_at_pos(const Fvector& pos, BOOL xform)
{
	if (g_dedicated_server || renderable.visual == nullptr)
		return;

	IParticleCustom* V			= smart_cast<IParticleCustom*>(renderable.visual); VERIFY(V);
	Fmatrix m; m.translate		(pos); 
	V->UpdateParent				(m,zero_vel,xform);
	V->Play						();
	dwLastTime					= Device.dwTimeGlobal-33ul;

	PerformAllTheWork			();
	m_bStopping					= false;
}

void CParticlesObject::Stop(BOOL bDefferedStop)
{
	if (g_dedicated_server || renderable.visual == nullptr)
		return;

	IParticleCustom* V			= smart_cast<IParticleCustom*>(renderable.visual); VERIFY(V);
	V->Stop						(bDefferedStop);
	m_bStopping					= true;
}

void CParticlesObject::shedule_Update(u32 _dt)
{
	inherited::shedule_Update(_dt);

	if (g_dedicated_server)		
		return;

	if (m_bDead)					
		return;

	UpdateSpatial();
}

void CParticlesObject::PerformAllTheWork()
{
	if(g_dedicated_server)		return;

	// Update
	UpdateSpatial					();
}

void CParticlesObject::WaitForParticles()
{
	if (psDeviceFlags.test(mtParticles))
	{
		PROF_EVENT("Particles Wait")
		ParticleObjectTasks.wait();
	}
}

void CParticlesObject::SetXFORM			(const Fmatrix& m)
{
	if(g_dedicated_server)		return;

	IParticleCustom* V	= smart_cast<IParticleCustom*>(renderable.visual); VERIFY(V);
	V->UpdateParent		(m,zero_vel,TRUE);
	renderable.xform.set(m);
	UpdateSpatial		();
}

void CParticlesObject::SetLiveUpdate(BOOL b)
{
	if(g_dedicated_server)		return;

	if (renderable.visual)
	{
		IParticleCustom* V = smart_cast<IParticleCustom*>(renderable.visual); VERIFY(V);
		V->SetLiveUpdate(b);
	}
}

BOOL CParticlesObject::GetLiveUpdate()
{
	if(g_dedicated_server || renderable.visual == nullptr)
		return 0;

	IParticleCustom* V	= smart_cast<IParticleCustom*>(renderable.visual); VERIFY(V);
	return V->GetLiveUpdate();
}

void CParticlesObject::UpdateParent		(const Fmatrix& m, const Fvector& vel)
{
	if(g_dedicated_server || renderable.visual == nullptr)
		return;

	IParticleCustom* V	= smart_cast<IParticleCustom*>(renderable.visual); VERIFY(V);
	V->UpdateParent		(m,vel,FALSE);
	UpdateSpatial		();
}

Fvector& CParticlesObject::Position		()
{
	if(g_dedicated_server) 
	{
		static Fvector _pos = Fvector().set(0,0,0);
		return _pos;
	}
	vis_data &vis = renderable.visual->getVisData();
	return vis.sphere.P;
}

float CParticlesObject::shedule_Scale		()	
{ 
	if(g_dedicated_server)		return 5.0f;

	return Device.vCameraPosition.distance_to(Position())/200.f; 
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
	VERIFY(m_bStopping || !IsLooped());
	m_bAutoRemove = auto_remove;
}

//играются ли партиклы, отличается от PSI_Alive, тем что после
//остановки Stop партиклы могут еще доигрывать анимацию IsPlaying = true
bool CParticlesObject::IsPlaying()
{
	if(g_dedicated_server)		return false;

	IParticleCustom* V	= smart_cast<IParticleCustom*>(renderable.visual); 
	VERIFY(V);
	return !!V->IsPlaying();
}
