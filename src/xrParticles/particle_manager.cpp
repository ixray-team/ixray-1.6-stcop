//---------------------------------------------------------------------------
#include "stdafx.h"
#pragma hdrstop

#include "particle_manager.h"
#include "particle_effect.h"
#include "particle_actions_collection.h"
#include "ParticlesObject.h"

using namespace PAPI;

// system
CParticleManager* PM = nullptr;
PARTICLES_API IParticleManager* PAPI::ParticleManager()
{
	if (!PM)
	{
		PM = new CParticleManager();
	}

	return PM;
}

// 
CParticleManager::CParticleManager	()
{
	Device.seqFrame.Add(this, REG_PRIORITY_HIGH + 10500);
}

CParticleManager::~CParticleManager	()
{
	Device.seqFrame.Remove(this);
}

CParticleManager::SharedParticleEffect CParticleManager::GetEffectPtr(int effect_id)
{
	xrSRWLockGuard guard(m_effect_guard, true);
	auto effectIter = m_effect_map.find(effect_id);
	if (effectIter != m_effect_map.end())
	{
		return effectIter->second;
	}

	return CParticleManager::SharedParticleEffect();
}

CParticleManager::SharedParticleActions CParticleManager::GetActionListPtr(int a_list_num)
{
	xrSRWLockGuard guard(m_action_guard, true);
	auto actionIter = m_alist_map.find(a_list_num);
	if (actionIter != m_alist_map.end())
	{
		return actionIter->second;
	}

	return CParticleManager::SharedParticleActions();
}

// create
int CParticleManager::CreateEffect(u32 max_particles)
{
	xrSRWLockGuard guard(m_effect_guard);
	int effectId = m_effect_counter++;

	auto EffectResultPair = m_effect_map.emplace(std::make_pair(effectId, xr_make_shared<ParticleEffect>(max_particles)));
	R_ASSERT2(EffectResultPair.second, "Can't create particle effect with global counter");

	return effectId;
}

void CParticleManager::DestroyEffect(int effect_id)
{
	xrSRWLockGuard guard(m_effect_guard);
	m_effect_map.erase(effect_id);
}

int	CParticleManager::CreateActionList()
{
	xrSRWLockGuard guard(m_action_guard);
	int actionId = m_action_counter++;

	while (ActionIter != 0)
	{
		std::this_thread::sleep_for(std::chrono::milliseconds(0));
	}

	auto ActionResultPair = m_alist_map.emplace(std::make_pair(actionId, xr_make_shared<ParticleActions>()));
	R_ASSERT2(ActionResultPair.second, "Can't create particle action with global counter");

	return actionId;
}

void CParticleManager::DestroyActionList(int alist_id)
{
	xrSRWLockGuard guard(m_action_guard);

	while (ActionIter != 0)
	{
		std::this_thread::sleep_for(std::chrono::milliseconds(0));
	}

	m_alist_map.erase(alist_id);
}

// control
void CParticleManager::PlayEffect(int effect_id, int alist_id)
{
	// Execute the specified action list.
	SharedParticleActions particleAction = GetActionListPtr(alist_id);

	if (!particleAction)
		return; // ERROR

	ActionIter++;

	// Step through all the actions in the action list.
	for (PAVecIt it = particleAction->begin(); it != particleAction->end(); ++it)
	{
		VERIFY((*it));
		switch ((*it)->type)
		{
		case PASourceID:
			static_cast<PASource*>(*it)->m_Flags.set(PASource::flSilent, FALSE);
			break;
		case PAExplosionID:
			static_cast<PAExplosion*>(*it)->age = 0.f;
			break;
		case PATurbulenceID:
			static_cast<PATurbulence*>(*it)->age = 0.f;
			break;
		}
	}

	ActionIter--;
}

void CParticleManager::StopEffect(int effect_id, int alist_id, BOOL deffered)
{
	// Execute the specified action list.
	SharedParticleActions particleAction = GetActionListPtr(alist_id);

	if (!particleAction)
		return; // ERROR

	// Step through all the actions in the action list.
	ActionIter++;
	for (PAVecIt it = particleAction->begin(); it != particleAction->end(); ++it)
	{
		switch ((*it)->type)
		{
		case PASourceID:
			static_cast<PASource*>(*it)->m_Flags.set(PASource::flSilent, TRUE);
			break;
		}
	}
	ActionIter--;

	if (!deffered)
	{
		// effect
		SharedParticleEffect particleEffect = GetEffectPtr(effect_id);
		if (particleEffect != nullptr)
		{
			xrCriticalSectionGuard cs(ParticleEffect::mParticleRemoveCS);
			particleEffect->p_count = 0;
		}
	}
}

// update&render
void CParticleManager::Update(int effect_id, int alist_id, float dt)
{
	SharedParticleEffect pe = GetEffectPtr(effect_id);
	SharedParticleActions pa = GetActionListPtr(alist_id);

	if (!pe || !pa)
	{
		return;
	}

	// Step through all the actions in the action list.
	float kill_old_time = 3.0f;
	for (PAPI::ParticleAction* pAction : *pa)
	{
		pAction->Execute(&(*pe), dt, kill_old_time);
	}
}

void CParticleManager::Transform(int alist_id, const Fmatrix& full, const Fvector& vel)
{
	// Execute the specified action list.
	SharedParticleActions pa = GetActionListPtr(alist_id);

	if (!pa)
		return;

	Fmatrix mT;
	mT.translate(full.c);

	// Step through all the actions in the action list.
	for (PAVecIt it = pa->begin(); it != pa->end(); ++it)
	{
		BOOL r = (*it)->m_Flags.is(ParticleAction::ALLOW_ROTATE);
		const Fmatrix& m = r ? full : mT;
		(*it)->Transform(m);
		switch ((*it)->type)
		{
		case PASourceID:
			static_cast<PASource*>(*it)->parent_vel = pVector(vel.x, vel.y, vel.z) * static_cast<PASource*>(*it)->parent_motion;
			break;
		}
	}
}

// effect
void CParticleManager::RemoveParticle(int effect_id, u32 p_id)
{
	SharedParticleEffect pe = GetEffectPtr(effect_id);
	if (!pe)
		return;

	pe->Remove(p_id);
}

void CParticleManager::SetMaxParticles(int effect_id, u32 max_particles)
{
	SharedParticleEffect pe = GetEffectPtr(effect_id);
	if (!pe)
		return;

	pe->Resize(max_particles);
}

void CParticleManager::SetCallback(int effect_id, OnBirthParticleCB b, OnDeadParticleCB d, void* owner, u32 param)
{
	SharedParticleEffect pe = GetEffectPtr(effect_id);
	if (!pe)
		return;

	pe->b_cb = b;
	pe->d_cb = d;
	pe->owner = owner;
	pe->param = param;
}

void CParticleManager::GetParticles(int effect_id, Particle*& particles, u32& cnt)
{
	SharedParticleEffect pe = GetEffectPtr(effect_id);
	if (!pe)
	{
		particles = nullptr;
		cnt = 0;
		return;
	}

	particles = pe->particles;
	cnt = pe->p_count;
}

u32	CParticleManager::GetParticlesCount	(int effect_id)
{
	SharedParticleEffect pe = GetEffectPtr(effect_id);
	if (!pe)
		return 0;

	return pe->p_count;
}

// action
ParticleAction* CParticleManager::CreateAction(PActionEnum type)
{
	ParticleAction* pa			= 0;
    switch(type){
    case PAAvoidID:				pa = new PAAvoid();				break;
    case PABounceID:    		pa = new PABounce();			break;
    case PACopyVertexBID:    	pa = new PACopyVertexB();		break;
    case PADampingID:    		pa = new PADamping();			break;
    case PAExplosionID:    		pa = new PAExplosion();			break;
    case PAFollowID:    		pa = new PAFollow();			break;
    case PAGravitateID:    		pa = new PAGravitate();			break;
    case PAGravityID:    		pa = new PAGravity();			break;
    case PAJetID:    			pa = new PAJet();				break;
    case PAKillOldID:    		pa = new PAKillOld();			break;
    case PAMatchVelocityID:    	pa = new PAMatchVelocity();		break;
    case PAMoveID:    			pa = new PAMove();				break;
    case PAOrbitLineID:    		pa = new PAOrbitLine();			break;
    case PAOrbitPointID:    	pa = new PAOrbitPoint();		break;
    case PARandomAccelID:    	pa = new PARandomAccel();		break;
    case PARandomDisplaceID:    pa = new PARandomDisplace();	break;
    case PARandomVelocityID:    pa = new PARandomVelocity();	break;
    case PARestoreID:    		pa = new PARestore();			break;
    case PASinkID:    			pa = new PASink();				break;
    case PASinkVelocityID:    	pa = new PASinkVelocity();		break;
    case PASourceID:    		pa = new PASource();			break;
    case PASpeedLimitID:    	pa = new PASpeedLimit();		break;
    case PATargetColorID:    	pa = new PATargetColor();		break;
    case PATargetSizeID:    	pa = new PATargetSize();		break;
    case PATargetRotateID:    	pa = new PATargetRotate();		break;
    case PATargetRotateDID:    	pa = new PATargetRotate();		break;
    case PATargetVelocityID:    pa = new PATargetVelocity(); 	break;
    case PATargetVelocityDID:   pa = new PATargetVelocity();	break;
    case PAVortexID:    		pa = new PAVortex();			break;
    case PATurbulenceID:		pa = new PATurbulence();		break;
    case PAScatterID:  			pa = new PAScatter();			break;
    default: NODEFAULT;
    }
    pa->type					= type;
    return pa;
}

u32 CParticleManager::LoadActions(int alist_id, IReader& R)
{
	// Execute the specified action list.
	SharedParticleActions pa = GetActionListPtr(alist_id);
	if (!pa)
		return 0;

	pa->clear();
	if (R.length())
	{
		u32 cnt = R.r_u32();
		for (u32 k = 0; k < cnt; ++k)
		{
			ParticleAction* act = CreateAction((PActionEnum)R.r_u32());
			act->Load(R);
			pa->append(act);
		}
	}
	return pa->size();
}

void CParticleManager::SaveActions(int alist_id, IWriter& W)
{
	// Execute the specified action list.
	SharedParticleActions pa = GetActionListPtr(alist_id);
	if (!pa)
		return;

	W.w_u32(pa->size());

	for (PAVecIt it = pa->begin(); it != pa->end(); ++it)
		(*it)->Save(W);
}

void PAPI::CParticleManager::OnFrame()
{
	CParticlesObject::UpdateAllAsync();
}








