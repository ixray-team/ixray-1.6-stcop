///////////////////////////////////////////////////////////////
// ParticlesPlayer.cpp
// интерфейс для проигрывания партиклов на объекте
///////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "ParticlesPlayer.h"
#include "GameObject.h"
#include "../Include/xrRender/Kinematics.h"
#include "ParticlesObject.h"
//-------------------------------------------------------------------------------------
static void generate_orthonormal_basis(const Fvector& dir,Fmatrix &result)
{
	result.identity		();
	result.k.normalize	(dir);
	Fvector::generate_orthonormal_basis(result.k, result.j, result.i);
}
TParticlesPlayer::SParticlesInfo* TParticlesPlayer::SBoneInfo::FindParticles(const shared_str& ps_name)
{
	for(ParticlesInfoListIt it = particles.begin(); it != particles.end(); it++) {
		if(it->ps && it->ps->Name() == ps_name) {
			return &(*it);
		}
	}
	return 0;
}
TParticlesPlayer::SParticlesInfo* TParticlesPlayer::SBoneInfo::AppendParticles(CObject* object, const shared_str& ps_name)
{
	SParticlesInfo* pi	= FindParticles(ps_name);
	if (pi)				return pi;
	particles.push_back	(SParticlesInfo());
	pi					= &particles.back();
	pi->ps				= Particles::Details::Create(*ps_name,FALSE);
	return pi;
}
void TParticlesPlayer::SBoneInfo::StopParticles(const shared_str& ps_name, bool bDestroy)
{
	SParticlesInfo* pi	= FindParticles(ps_name);
	if (pi){
		if(!bDestroy)
			pi->ps->Stop();
		else
			Particles::Details::Destroy(pi->ps);
	}
}

void TParticlesPlayer::SBoneInfo::StopParticles(u16 sender_id, bool bDestroy)
{
	for (ParticlesInfoListIt it=particles.begin(); it!=particles.end(); it++)
		if (it->sender_id==sender_id){
			if(!bDestroy)
				it->ps->Stop();
			else
				Particles::Details::Destroy(it->ps);
		}
}
//-------------------------------------------------------------------------------------

TParticlesPlayer::TParticlesPlayer() : m_bActiveBones(false)
{
	AppendBone(0);
	SetParentVel(zero_vel);
}

void TParticlesPlayer::EndComponent()
{
	for (BoneInfoVecIt b_it = m_Bones.begin(); b_it != m_Bones.end(); b_it++)
	{
		SBoneInfo& b_info = *b_it;

		for (ParticlesInfoListIt p_it = b_info.particles.begin(); p_it != b_info.particles.end(); p_it++)
		{
			SParticlesInfo& p_info = *p_it;
			Particles::Details::Destroy(p_info.ps);
		}
		b_info.particles.clear();
	}
}

void TParticlesPlayer::LoadParticles(IKinematics* K)
{
	VERIFY(K);
	m_Bones.clear();

	//считать список косточек и соответствующих
	//офсетов  куда можно вешать партиклы
	CInifile* ini = K->LL_UserData();
	if (ini && ini->section_exist("particle_bones"))
	{
		bone_mask.zero();
		CInifile::Sect& data = ini->r_section("particle_bones");
		for (CInifile::SectCIt I = data.Data.begin(); I != data.Data.end(); I++) {
			const CInifile::Item& item = *I;
			u16 index = K->LL_BoneID(*item.first);
			R_ASSERT3(index != BI_NONE, "Particles bone not found", *item.first);
			Fvector					offs;
			sscanf(*item.second, "%f,%f,%f", &offs.x, &offs.y, &offs.z);
			AppendBone(index, offs);
		}
	}

	if (m_Bones.empty())
		AppendBone(K->LL_GetBoneRoot());
}

void TParticlesPlayer::LoadParticles(const char* section, IKinematics* K)
{
	VERIFY				(K);

	if(pSettings->section_exist(section))
	{
		CInifile::Sect& data		= pSettings->r_section(section);
		for (CInifile::SectCIt I=data.Data.begin(); I!=data.Data.end(); I++){
			const CInifile::Item& item	= *I;
			u16 index				= K->LL_BoneID(*item.first); 
			R_ASSERT3(index != BI_NONE, "Particles bone not found", *item.first);
			Fvector					offs;
			sscanf					(*item.second,"%f,%f,%f",&offs.x,&offs.y,&offs.z);
			AppendBone(index, offs);
		}
	}
}

void TParticlesPlayer::LoadParticles(const char* section, const char* line, IKinematics* K)
{
	VERIFY				(K);

	const char* line_items = pSettings->r_string(section, line);

	int count = _GetItemCount(line_items);
	string64 S1;
	for (int i = 0; i < count; ++i)
	{
		_GetItem(line_items, i, S1);
		u16 bone_id = K->LL_BoneID(S1);
		R_ASSERT3(bone_id != BI_NONE, "Particles bone not found", K->LL_BoneName_dbg(bone_id));
		AppendBone(bone_id);
	}
}

void TParticlesPlayer::AppendBone(u16 bone_id, Fvector offs)
{
	if(get_bone_info(bone_id))
		return;

	bone_mask.set(bone_id, true);
	m_Bones.push_back	(SBoneInfo(bone_id,offs));
}

TParticlesPlayer::SBoneInfo* TParticlesPlayer::get_nearest_bone_info(IKinematics* K, u16 bone_index)
{
	u16 play_bone = bone_index;
	while ((BI_NONE != play_bone) && !bone_mask.is(play_bone))
	{
		play_bone = K->LL_GetData(play_bone).GetParentID();
	}
	return get_bone_info(play_bone);
}

void TParticlesPlayer::StartParticles(const shared_str& particles_name, u16 bone_num, const Fvector& dir, u16 sender_id, int life_time, bool auto_stop)
{
	Fmatrix xform;
	generate_orthonormal_basis(dir,xform);
	StartParticles(particles_name,bone_num,xform,sender_id,life_time,auto_stop);
}

void TParticlesPlayer::StartParticles(const shared_str& particles_name, u16 bone_num, const Fmatrix& xform, u16 sender_id, int life_time, bool auto_stop)
{
	VERIFY(fis_zero(xform.c.magnitude()));
	R_ASSERT(*particles_name);

	SBoneInfo* pBoneInfo = get_nearest_bone_info(PKinematics(GetComponentOwner()->Visual()), bone_num);
	if (!pBoneInfo)
	{
		return;
	}

	SParticlesInfo& particles_info = *pBoneInfo->AppendParticles(GetComponentOwner(), particles_name);

	particles_info.sender_id = sender_id;

	particles_info.life_time = auto_stop ? life_time : u32(-1);
	xform.getHPB(particles_info.angles);

	Fmatrix m; m.setHPB(particles_info.angles.x, particles_info.angles.y, particles_info.angles.z);
	GetBonePos(GetComponentOwner(), pBoneInfo->index, pBoneInfo->offset, m.c);
	particles_info.ps->UpdateParent(m, zero_vel);
	if (!particles_info.ps->IsPlaying())
		particles_info.ps->Play(false);

	m_bActiveBones = true;
}

void TParticlesPlayer::StartParticles(const shared_str& ps_name, const Fmatrix& xform, u16 sender_id, int life_time, bool auto_stop)
{
	for (BoneInfoVecIt it = m_Bones.begin(); it != m_Bones.end(); it++)
	{
		SParticlesInfo& particles_info = *it->AppendParticles(GetComponentOwner(), ps_name);
		particles_info.sender_id = sender_id;

		particles_info.life_time = auto_stop ? life_time : u32(-1);
		xform.getHPB(particles_info.angles);
		//начать играть партиклы

		Fmatrix m; m.set(xform);
		GetBonePos(GetComponentOwner(), it->index, it->offset, m.c);
		particles_info.ps->UpdateParent(m, zero_vel);
		if (!particles_info.ps->IsPlaying())
		{
			particles_info.ps->Play(false);
		}
	}

	m_bActiveBones = true;
}

void TParticlesPlayer::StartParticles(const shared_str& ps_name, const Fvector& dir, u16 sender_id, int life_time, bool auto_stop)
{
	Fmatrix xform;
	generate_orthonormal_basis(dir,xform);
	StartParticles(ps_name,xform,sender_id,life_time,auto_stop);
}


void TParticlesPlayer::StopParticles(u16 sender_id, u16 bone_id, bool bDestroy)
{
	if (BI_NONE==bone_id){
		for(BoneInfoVecIt it=m_Bones.begin(); it!=m_Bones.end(); it++)
			it->StopParticles	(sender_id, bDestroy);
	}else{
		SBoneInfo* bi			= get_bone_info(bone_id); VERIFY(bi);
		bi->StopParticles		(sender_id, bDestroy);
	}
	UpdateParticles();
}

void TParticlesPlayer::StopParticles(const shared_str& ps_name, u16 bone_id, bool bDestroy)
{
	if (BI_NONE==bone_id){
		for(BoneInfoVecIt it=m_Bones.begin(); it!=m_Bones.end(); it++)
			it->StopParticles	(ps_name, bDestroy);
	}else{
		SBoneInfo* bi			= get_bone_info(bone_id); VERIFY(bi);
		bi->StopParticles		(ps_name, bDestroy);
	}
	UpdateParticles();
}

//остановка партиклов, по истечении их времени жизни
void TParticlesPlayer::AutoStopParticles(const shared_str& ps_name, u16 bone_id,u32 life_time)
{
	if (BI_NONE==bone_id){
		for(BoneInfoVecIt it=m_Bones.begin(); it!=m_Bones.end(); it++)
		{
			SParticlesInfo* pInfo = it->FindParticles	(ps_name);
			if(pInfo) pInfo->life_time = life_time;
		}
	}else{
		SBoneInfo* bi			= get_bone_info(bone_id); VERIFY(bi);
		SParticlesInfo* pInfo = bi->FindParticles	(ps_name);
		if(pInfo) pInfo->life_time = life_time;
	}
}
struct SRP
{
	bool operator	() (TParticlesPlayer::SParticlesInfo& pi)
	{
		return ! pi.ps;
	}
};
void TParticlesPlayer::UpdateParticles()
{
	if (!m_bActiveBones)	return;
	m_bActiveBones = false;

	for (SBoneInfo& b_info : m_Bones)
	{
		for (SParticlesInfo& p_info : b_info.particles)
		{
			if (!p_info.ps)
				continue;

			//обновить позицию партиклов
			Fmatrix xform;
			xform.setHPB(p_info.angles.x, p_info.angles.y, p_info.angles.z);
			GetBonePos(GetComponentOwner(), b_info.index, b_info.offset, xform.c);
			p_info.ps->UpdateParent(xform, parent_vel);

			//обновить время существования
			if (p_info.life_time != u32(-1))
			{
				if (p_info.life_time > Device.dwTimeDelta)	p_info.life_time -= Device.dwTimeDelta;
				else
				{
					p_info.ps->Stop();
					p_info.life_time = u32(-1);
				}
			}

			if (!p_info.ps->IsPlaying())
			{
				Particles::Details::Destroy(p_info.ps);
			}
			else
			{
				m_bActiveBones = true;
			}
		}

		const auto RI = std::remove_if(b_info.particles.begin(), b_info.particles.end(), [](const SParticlesInfo& pi)
		{
			return pi.ps == nullptr;
		});

		b_info.particles.erase(RI, b_info.particles.end());
	}
}

void TParticlesPlayer::GetBonePos(CObject* pObject, u16 bone_id, const Fvector& offset, Fvector& result)
{
	VERIFY(pObject);
	IKinematics* pKinematics = PKinematics(pObject->Visual()); VERIFY(pKinematics);
	CBoneInstance& l_tBoneInstance = pKinematics->LL_GetBoneInstance(bone_id);

	result = offset;
	l_tBoneInstance.mTransform.transform_tiny(result);
	pObject->XFORM().transform_tiny(result);
}

void TParticlesPlayer::MakeXFORM	(CObject* pObject, u16 bone_id, const Fvector& dir, const Fvector& offset, Fmatrix& result)
{
	generate_orthonormal_basis(dir,result);
	GetBonePos(pObject, bone_id, offset, result.c);
}

u16 TParticlesPlayer::GetNearestBone	(IKinematics* K, u16 bone_id)
{
	u16 play_bone	= bone_id;

	while((BI_NONE!=play_bone)&&!bone_mask.is(play_bone))
	{
		play_bone	= K->LL_GetData(play_bone).GetParentID();
	}
	return play_bone;
}

CGameObject* TParticlesPlayer::GetComponentOwner() const
{
	if (ComponentOwner == nullptr)
	{
		TParticlesPlayer* This = const_cast<TParticlesPlayer*>(this);
		ComponentOwner = smart_cast<CGameObject*>(GECSManager->GetComponentOwner(This));
		VERIFY(ComponentOwner);
	}

	return ComponentOwner;
}
