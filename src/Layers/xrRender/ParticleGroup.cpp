#include "stdafx.h"
#include "ParticleGroup.h"
#include "PSLibrary.h"
#include "ParticleEffect.h"

using namespace PS;

#ifdef _EDITOR
extern BOOL ps_r2_particle_dt;
#endif

//------------------------------------------------------------------------------
CPGDef::CPGDef()
{                             
	m_Flags.zero	();
	m_fTimeLimit	= 0.f;
}

CPGDef::~CPGDef()
{
	for (EffectIt it=m_Effects.begin(); it!=m_Effects.end(); it++)
		xr_delete	(*it);
	m_Effects.clear	();
}

void CPGDef::SetName(LPCSTR name)
{
	m_Name			= name;
}

#ifdef _EDITOR
void CPGDef::Clone	(CPGDef* source)
{
	m_Name			= "<invalid_name>";
	m_Flags			= source->m_Flags;
	m_fTimeLimit	= source->m_fTimeLimit;

	m_Effects.resize(source->m_Effects.size(),0);		
	for (EffectIt d_it=m_Effects.begin(),s_it=source->m_Effects.begin(); s_it!=source->m_Effects.end(); s_it++,d_it++)
		*d_it		= new SEffect(**s_it);
}
#endif

//------------------------------------------------------------------------------
// I/O part
//------------------------------------------------------------------------------
BOOL CPGDef::LoadOriginal(IReader& F)
{
	bool FoundedChunk = !!F.find_chunk(PS::PG::Chunks::VERSION);
	R_ASSERT2(FoundedChunk, "Not found chunk PGD_CHUNK_VERSION");

	PS::PG::Version version	= F.r_enum<PS::PG::Version>();

	if (version!=PS::PG::Version::Original){
		Log			("!Unsupported PG version. Load failed.");
		return FALSE;
	}

	FoundedChunk = !!F.find_chunk(PS::PG::Chunks::NAME);
	R_ASSERT2(FoundedChunk, "Not found chunk PGD_CHUNK_NAME");

	F.r_stringZ		(m_Name);

	F.r_chunk		(PS::PG::Chunks::FLAGS,&m_Flags);

	if (F.find_chunk(PS::PG::Chunks::TIME_LIMIT))
		m_fTimeLimit= F.r_float();
	else
		m_fTimeLimit	= 0.0f;
	
	bool dont_calc_timelimit = m_fTimeLimit > 0.0f;
	if (F.find_chunk(PS::PG::Chunks::EFFECTS))
	{
		m_Effects.resize(F.r_u32());
		for (EffectIt it=m_Effects.begin(); it!=m_Effects.end(); it++){
			*it				= new SEffect();
#ifdef _EDITOR
			(*it)->parent = this;
#endif
			F.r_stringZ		((*it)->m_EffectName);
			F.r_stringZ		((*it)->m_OnPlayChildName);
			F.r_stringZ		((*it)->m_OnBirthChildName);
			F.r_stringZ		((*it)->m_OnDeadChildName);
			(*it)->m_Time0 	= F.r_float();
			(*it)->m_Time1 	= F.r_float();
			(*it)->m_Flags.assign	(F.r_u32());
			
			if(!dont_calc_timelimit)
				m_fTimeLimit	= std::max(m_fTimeLimit, (*it)->m_Time1);
		}
	}
	return TRUE;
}                   

BOOL CPGDef::Load2(CInifile& ini)
{
//.	u16 version						= ini.r_u16("_group", "version");
	
	auto ver = ini.r_enum<PS::PG::Version>("_group", "version");
	switch (ver)
	{
	case PG::Version::Original:
		{
			return Load2Original(ini);
		}
	case PG::Version::Extended:
		{
			return Load2Extended(ini);
		}
	default:
		{
			return false;
		}
	}
}

BOOL CPGDef::Load2Original(CInifile& ini)
{
	m_Flags.assign					(ini.r_u32("_group", "flags"));
	m_Effects.resize				(ini.r_u32("_group", "effects_count"));

	u32 counter						= 0;
	string256						buff;
	for (EffectIt it=m_Effects.begin(); it!=m_Effects.end(); ++it,++counter)
	{
		*it							= new SEffect();
#ifdef _EDITOR
		(*it)->parent = this;
#endif

		xr_sprintf					(buff, sizeof(buff), "effect_%04d", counter);
		
		(*it)->m_EffectName			= ini.r_string	(buff, "effect_name");
		(*it)->m_OnPlayChildName	= ini.r_string	(buff, "on_play_child");
		(*it)->m_OnBirthChildName	= ini.r_string	(buff, "on_birth_child");
		(*it)->m_OnDeadChildName	= ini.r_string	(buff, "on_death_child");

		(*it)->m_Time0 				= ini.r_float(buff, "time0");
		(*it)->m_Time1 				= ini.r_float(buff, "time1");
		(*it)->m_Flags.assign		(ini.r_u32(buff, "flags"));
	}
	m_fTimeLimit					= ini.r_float		("_group", "timelimit");
	return							TRUE;
}

BOOL CPGDef::Load2Extended(CInifile& ini)
{
	return true;
}

void CPGDef::Save(IWriter& F)
{
	F.open_chunk	(PS::PG::Chunks::VERSION);
	F.w_enum		(PS::PG::Version::Original);
	F.close_chunk	();

	F.open_chunk	(PS::PG::Chunks::NAME);
	F.w_stringZ		(m_Name);
	F.close_chunk	();

	F.w_chunk		(PS::PG::Chunks::FLAGS,&m_Flags,sizeof(m_Flags));

	F.open_chunk	(PS::PG::Chunks::EFFECTS);
	F.w_u32			((u32)m_Effects.size());
	for (EffectIt it=m_Effects.begin(); it!=m_Effects.end(); it++){
		F.w_stringZ	((*it)->m_EffectName);
		F.w_stringZ	((*it)->m_OnPlayChildName);
		F.w_stringZ	((*it)->m_OnBirthChildName);
		F.w_stringZ	((*it)->m_OnDeadChildName);
		F.w_float	((*it)->m_Time0);
		F.w_float	((*it)->m_Time1);
		F.w_u32		((*it)->m_Flags.get());
	}
	F.close_chunk	();

	F.open_chunk	(PS::PG::Chunks::TIME_LIMIT);
	F.w_float		(m_fTimeLimit);
	F.close_chunk	();
}

void CPGDef::Save2(CInifile& ini)
{
	ini.w_enum		("_group", "version", PS::PG::Version::Original);

	ini.w_u32		("_group", "flags", m_Flags.get());

	ini.w_u32		("_group", "effects_count", (u32)m_Effects.size());

	u32 counter		= 0;
	string256		buff;
	for (EffectIt it=m_Effects.begin(); it!=m_Effects.end(); ++it,++counter)
	{
		xr_sprintf		(buff, sizeof(buff), "effect_%04d", counter);
		
		ini.w_string	(buff, "effect_name",	(*it)->m_EffectName.c_str());
		ini.w_string	(buff, "on_play_child", (*it)->m_Flags.test(SEffect::flOnPlayChild)?(*it)->m_OnPlayChildName.c_str():"");
		ini.w_string	(buff, "on_birth_child",(*it)->m_Flags.test(SEffect::flOnBirthChild)?(*it)->m_OnBirthChildName.c_str():"");
		ini.w_string	(buff, "on_death_child",(*it)->m_Flags.test(SEffect::flOnDeadChild)?(*it)->m_OnDeadChildName.c_str():"");
		ini.w_float		(buff, "time0",			(*it)->m_Time0);
		ini.w_float		(buff, "time1",			(*it)->m_Time1);
		ini.w_u32		(buff, "flags",			(*it)->m_Flags.get());
	}

	ini.w_float		("_group", "timelimit", m_fTimeLimit);
}

//------------------------------------------------------------------------------
// Particle Group item
//------------------------------------------------------------------------------
void CParticleGroup::SItem::Clear()
{
	for (CParticleEffect* ChildPart : _children_related)
		RImplementation.Models->DeleteDeffered((dxRender_Visual*&)ChildPart);

	for (CParticleEffect* ChildPart : _children_free)
		RImplementation.Models->DeleteDeffered((dxRender_Visual*&)ChildPart);

	//	Igor: zero all pointers! Previous code didn't zero _source_ pointers,
	//	just temporary ones.
	RImplementation.Models->DeleteDeffered((dxRender_Visual*&)_effect);
	_children_related.clear();
	_children_free.clear();
}

void CParticleGroup::SItem::StartRelatedChild(CParticleEffect* emitter, LPCSTR eff_name, PAPI::Particle& m)
{
	CParticleEffect* C = static_cast<CParticleEffect*>(RImplementation.model_CreatePE(eff_name));
	
	C->SetHudMode(emitter->GetHudMode());

	Fmatrix M; M.identity();
	Fvector vel; vel.sub(m.pos,m.posB); vel.div(C->m_RT_Flags.is(CParticleEffect::flRT_LiveUpdate)?Device.fTimeDelta:fDT_STEP);
	if (emitter->m_RT_Flags.is(CParticleEffect::flRT_XFORM))
	{
		M.set(emitter->m_XFORM);
		M.transform_dir(vel);
	}
	Fvector p;
	M.transform_tiny(p,m.pos);
	M.c.set(p);
	C->Play();
	C->UpdateParent(M,vel,FALSE);
	_children_related.push_back(C);
}
void CParticleGroup::SItem::StopRelatedChild(u32 idx)
{
	VERIFY(idx<_children_related.size());
	CParticleEffect*& V = _children_related[idx];
	V->Stop(TRUE);
	_children_free.push_back(V);
	_children_related[idx] = _children_related.back();
	_children_related.pop_back();
}
void CParticleGroup::SItem::StartFreeChild(CParticleEffect* emitter, LPCSTR nm, PAPI::Particle& m)
{
	CParticleEffect* C = static_cast<CParticleEffect*>(RImplementation.model_CreatePE(nm));
	C->SetHudMode(emitter->GetHudMode());
	if(!C->IsLooped())
	{
		Fmatrix M; M.identity();
		Fvector vel; vel.sub(m.pos,m.posB); vel.div(C->m_RT_Flags.is(CParticleEffect::flRT_LiveUpdate)?Device.fTimeDelta:fDT_STEP);
		if (emitter->m_RT_Flags.is(CParticleEffect::flRT_XFORM))
		{
			M.set(emitter->m_XFORM);
			M.transform_dir(vel);
		}
		Fvector p;
		M.transform_tiny(p,m.pos);
		M.c.set(p);
		C->Play();
		C->UpdateParent(M,vel,FALSE);
		_children_free.push_back(C);
	}
	else
	{
#ifdef _EDITOR        
		Msg			("!Can't use looped effect '%s' as 'On Birth' child for group.",nm);
#else
		Debug.fatal	(DEBUG_INFO,"Can't use looped effect '%s' as 'On Birth' child for group.",nm);
#endif
	}
}
void CParticleGroup::SItem::Play()
{
	if (_effect)
		_effect->Play();
}

void CParticleGroup::SItem::Stop(BOOL def_stop)
{
	// stop all effects
	if (_effect)
		_effect->Stop(def_stop);

	for (CParticleEffect* ChildPart : _children_related)
	{
		ChildPart->Stop(def_stop);
		if (!def_stop) 
			RImplementation.Models->DeleteDeffered((dxRender_Visual*&)ChildPart);
	}

	for (CParticleEffect* ChildPart : _children_free)
	{
		ChildPart->Stop(def_stop);
		if (!def_stop)
			RImplementation.Models->DeleteDeffered((dxRender_Visual*&)ChildPart);
	}

	if (!def_stop)
	{
		_children_related.clear();
		_children_free.clear();
	}
}

BOOL CParticleGroup::SItem::IsPlaying() const
{
	return _effect ? _effect->IsPlaying() : FALSE;
}

void CParticleGroup::SItem::UpdateParent(const Fmatrix& m, const Fvector& velocity, BOOL bXFORM)
{
	if (_effect)
		_effect->UpdateParent(m,velocity,bXFORM);
}
//------------------------------------------------------------------------------
void OnGroupParticleBirth(void* owner, u32 param, PAPI::Particle& m, u32 idx)
{
	CParticleGroup* PG = static_cast<CParticleGroup*>(owner); VERIFY(PG);
	CParticleEffect* PE	= PG->items[param]._effect;
	PS::OnEffectParticleBirth(PE, param, m, idx);
	// if have child
	const CPGDef* PGD = PG->GetDefinition(); VERIFY(PGD);
	const CPGDef::SEffect* eff = PGD->m_Effects[param];

	if (eff->m_Flags.is(CPGDef::SEffect::flOnBirthChild))
		PG->items[param].StartFreeChild(PE,*eff->m_OnBirthChildName,m);

	if (eff->m_Flags.is(CPGDef::SEffect::flOnPlayChild))
		PG->items[param].StartRelatedChild(PE,*eff->m_OnPlayChildName,m);
}

void OnGroupParticleDead(void* owner, u32 param, PAPI::Particle& m, u32 idx)
{
	CParticleGroup* PG = static_cast<CParticleGroup*>(owner); VERIFY(PG);
	CParticleEffect* PE = PG->items[param]._effect;
	PS::OnEffectParticleDead(PE, param, m, idx);
	// if have child
	const CPGDef* PGD = PG->GetDefinition(); VERIFY(PGD);
	const CPGDef::SEffect* eff = PGD->m_Effects[param];

	if (eff->m_Flags.is(CPGDef::SEffect::flOnPlayChild))
		PG->items[param].StopRelatedChild(idx);

	if (eff->m_Flags.is(CPGDef::SEffect::flOnDeadChild))
		PG->items[param].StartFreeChild(PE,*eff->m_OnDeadChildName,m);
}

void CParticleGroup::SItem::OnFrame(u32 u_dt, const CPGDef::SEffect& def, Fbox& box, bool& bPlaying)
{
	PROF_EVENT(__FUNCTION__);
	if (CParticleEffect* E = _effect)
	{
		E->OnFrame(u_dt);
		if (E->IsPlaying())
		{
			bPlaying = true;
			if (E->vis.box.is_valid())
				box.merge(E->vis.box);

			if (def.m_Flags.is(CPGDef::SEffect::flOnPlayChild) && def.m_OnPlayChildName.size())
			{
				PAPI::Particle* particles;
				u32 p_cnt;
				E->Pholder.GetParticles(particles, p_cnt);
				VERIFY(p_cnt == _children_related.size());

				if (p_cnt)
				{
					for (u32 i = 0; i < p_cnt; i++)
					{
						PAPI::Particle& m = particles[i];
						CParticleEffect* C = _children_related[i];
						Fmatrix M; M.translate(m.pos);
						Fvector vel; vel.sub(m.pos, m.posB); vel.div(C->m_RT_Flags.is(CParticleEffect::flRT_LiveUpdate) ? Device.fTimeDelta : fDT_STEP);
						C->UpdateParent(M, vel, FALSE);
					}
				}
			}
		}
	}

	if (!_children_related.empty())
	{
		for (CParticleEffect* E_ : _children_related)
		{
			E_->OnFrame(u_dt);
			if (E_->IsPlaying())
			{
				bPlaying = true;
				if (E_->vis.box.is_valid())
					box.merge(E_->vis.box);
			}
			else
			{
				if (def.m_Flags.is(CPGDef::SEffect::flOnPlayChildRewind))
					E_->Play();
			}
		}
	}

	if (!_children_free.empty())
	{
		_children_free.erase(std::remove_if(_children_free.begin(), _children_free.end(), [u_dt, &bPlaying, &box](CParticleEffect* E_)
			{
				E_->OnFrame(u_dt);
				if (E_->IsPlaying())
				{
					bPlaying = true;
					if (E_->vis.box.is_valid()) box.merge(E_->vis.box);
				}
				else
				{
					RImplementation.Models->DeleteDeffered((dxRender_Visual*&)E_);
					return true;
				}
				return false;
			}), _children_free.end());
	}
}

u32	CParticleGroup::SItem::SpriteCount()
{
	u32 p_count = _effect ? _effect->SpriteCount() : 0u;

	for (CParticleEffect* ChildPart : _children_related)
		p_count += ChildPart->SpriteCount();

	for (CParticleEffect* ChildPart : _children_free)
		p_count += ChildPart->SpriteCount();

	return p_count;
}


//------------------------------------------------------------------------------
// Particle Group part
//------------------------------------------------------------------------------
CParticleGroup::CParticleGroup()
{
	m_RT_Flags.zero			();
	m_InitialPosition.set	(0,0,0);
}

CParticleGroup::~CParticleGroup()
{
	// Msg ("!!! destoy PG");
	xrSRWLockGuard srwguard(&lock, true);
	for (SItem& item : items)
		item.Clear();
}

void CParticleGroup::OnFrame(u32 u_dt)
{
	PROF_EVENT(__FUNCTION__);
	xrCriticalSectionGuard guard(&onframe_lock);
	if (!m_Def || !m_RT_Flags.is(flRT_Playing))
	{
		vis.box.set(m_InitialPosition, m_InitialPosition);
		vis.box.grow(EPS_L);
		vis.box.getsphere(vis.sphere.P, vis.sphere.R);
	}

	float fdeltaTime = float(u_dt) / 1000.f;

	auto& def_effects = m_Def->m_Effects;
	bool bPlaying = false;
	Fbox BBOX; BBOX.invalidate();

	{
		xrSRWLockGuard srwguard(&lock, true);
		for (size_t i = 0; i < def_effects.size(); i++)
		{
			PS::CPGDef::SEffect* pEffect = def_effects[i];
			SItem& particleRenderItem = items[i];
			if (pEffect->m_Flags.is(CPGDef::SEffect::flEnabled))
			{
				VERIFY(items.size() == def_effects.size());


				if (particleRenderItem.IsPlaying())
				{
					if ((m_CurrentTime <= pEffect->m_Time1) && (m_CurrentTime + fdeltaTime >= pEffect->m_Time1))
					{
						particleRenderItem.Stop(pEffect->m_Flags.is(CPGDef::SEffect::flDefferedStop));
					}
				}
				else
				{
					if (!m_RT_Flags.is(flRT_DefferedStop))
					{
						if ((m_CurrentTime <= pEffect->m_Time0) && (m_CurrentTime + fdeltaTime >= pEffect->m_Time0))
						{
							particleRenderItem.Play();
						}
					}
				}
			}

			particleRenderItem.OnFrame(u_dt, *pEffect, BBOX, bPlaying);
		}
	}

	m_CurrentTime += fdeltaTime;
	if ((m_Def->m_fTimeLimit > 0.f) && (m_CurrentTime > m_Def->m_fTimeLimit))
	{
		if (!m_RT_Flags.is(flRT_DefferedStop))
		{
			Stop(true);
		}
	}

	if (m_RT_Flags.is(flRT_DefferedStop) && !bPlaying)
	{
		m_RT_Flags.set(flRT_Playing | flRT_DefferedStop, FALSE);
	}

	if (BBOX.is_valid())
	{
		vis.box.set(BBOX);
		vis.box.getsphere(vis.sphere.P, vis.sphere.R);
	}
}

#ifndef _EDITOR
void CParticleGroup::UpdateCache()
{
	PROF_EVENT(__FUNCTION__);
	xrSRWLockGuard srwguard(&lock, true);
	for (SItem& item : items)
	{
		if(item._effect)
			item._effect->UpdateCache();

		for (CParticleEffect* ChildPart : item._children_related)
			ChildPart->UpdateCache();

		for (CParticleEffect* ChildPart : item._children_free)
			ChildPart->UpdateCache();
	}
}
#endif

void CParticleGroup::UpdateParent(const Fmatrix& m, const Fvector& velocity, BOOL bXFORM)
{
	{
		xrCriticalSectionGuard guard(&onframe_lock);
		m_InitialPosition = m.c;
	}
	xrSRWLockGuard srwguard(&lock, true);
	for (SItem& item : items)
		item.UpdateParent(m,velocity,bXFORM);
}

void CParticleGroup::Compile(CPGDef* def)
{
	xrCriticalSectionGuard guard(&onframe_lock);
	m_Def = def;
	// destroy existing
	xrSRWLockGuard srwguard(&lock, false);
	for (SItem& item : items)
		item.Clear();
	items.clear();
	// create new
	if (m_Def)
	{
		auto& def_effects = m_Def->m_Effects;
		items.resize(def_effects.size());
		for (CPGDef::EffectVec::const_iterator e_it=def_effects.begin(); e_it!=def_effects.end(); e_it++)
		{
			CParticleEffect* eff = (CParticleEffect*)RImplementation.model_CreatePE(*(*e_it)->m_EffectName);
			eff->SetBirthDeadCB	(OnGroupParticleBirth,OnGroupParticleDead,this,u32(e_it-def_effects.begin()));
			items[e_it-def->m_Effects.begin()]._effect = eff;
		}
	}
}

void CParticleGroup::Play()
{
	xrCriticalSectionGuard guard(&onframe_lock);
	m_CurrentTime   = 0;
	m_RT_Flags.set	(flRT_DefferedStop,FALSE);
	m_RT_Flags.set 	(flRT_Playing,TRUE);
}

void CParticleGroup::Stop(BOOL bDefferedStop)
{
	{
		xrCriticalSectionGuard guard(&onframe_lock);
		if (bDefferedStop)
			m_RT_Flags.set(flRT_DefferedStop, TRUE);
		else
			m_RT_Flags.set(flRT_Playing, FALSE);
	}
	xrSRWLockGuard srwguard(&lock, true);
	for (SItem& item : items)
		item.Stop(bDefferedStop);
}

u32 CParticleGroup::SpriteCount()
{
	u32 p_count = 0u;
	{
		xrSRWLockGuard srwguard(&lock, true);
		for (SItem& item : items)
			p_count += item.SpriteCount();
	}
	return p_count;
}

PAPI::ParticleAction* CParticleGroup::FindPA(shared_str PEName, PAPI::PActionEnum Action)
{
	xrSRWLockGuard srwguard(&lock, true);
	auto it = std::find_if(items.begin(), items.end(), [PEName](SItem& elem)
	{
		return elem._effect->dcast_ParticleCustom()->Name() == PEName;
	});
	R_ASSERT4(it != items.end(), "Unable to find PE in PG", PEName.c_str(), Name().c_str());
	return it != items.end() ? it->_effect->dcast_ParticleCustom()->FindPA(PEName, Action) : nullptr;
}

void CParticleGroup::SetHudMode(BOOL b)
{
	xrSRWLockGuard srwguard(&lock, true);
	for (SItem& item : items)
	{
		if(item._effect)
			item._effect->SetHudMode(b);
	}
}

BOOL CParticleGroup::GetHudMode()
{
	xrSRWLockGuard srwguard(&lock, true);
	if(!items.empty() && items[0]._effect)
		return items[0]._effect->GetHudMode();

	return FALSE;
}

void CParticleGroup::SetLiveUpdate(BOOL b)
{
	xrSRWLockGuard srwguard(&lock, true);
	for (SItem& item : items)
	{
		if(item._effect)
			item._effect->SetLiveUpdate(b);
	}
}

BOOL CParticleGroup::GetLiveUpdate()
{
	xrSRWLockGuard srwguard(&lock, true);
	if(!items.empty() && items[0]._effect)
		return items[0]._effect->GetLiveUpdate();

	return FALSE;
}