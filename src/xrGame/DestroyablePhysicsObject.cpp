#include "StdAfx.h"
#include "pch_script.h"
#include "PHCollisionDamageReceiver.h"
#include "PhysicObject.h"
#include "Hit.h"
#include "PHDestroyable.h"
#include "hit_immunity.h"
#include "damage_manager.h"
#include "DestroyablePhysicsObject.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "../Include/xrRender/Kinematics.h"
#include "xrServer_Objects_ALife.h"
#include "game_object_space.h"
#include "../xrScripts/script_callback_ex.h"
#include "script_game_object.h"
#include "../xrPhysics/PhysicsShell.h"

#ifdef DEBUG
#	include "../xrPhysics/IPHWorld.h"
#endif

CDestroyablePhysicsObject ::CDestroyablePhysicsObject()
{
	m_fHealth=1.f;
	child_part = false;
}

CDestroyablePhysicsObject::~CDestroyablePhysicsObject()
{
}

void CDestroyablePhysicsObject::OnChangeVisual()
{
	if (m_pPhysicsShell){
		m_pPhysicsShell->Deactivate();
		xr_delete		(m_pPhysicsShell);
		VERIFY			(0==Visual());
	}
	inherited::OnChangeVisual();
}
CPhysicsShellHolder*	 CDestroyablePhysicsObject ::	PPhysicsShellHolder			()
{
	return cast_physics_shell_holder();
}

void CDestroyablePhysicsObject::net_Destroy()
{
	inherited::net_Destroy();
	CPHDestroyable::RespawnInit();
	CPHCollisionDamageReceiver::Clear();
}

bool CDestroyablePhysicsObject::net_Spawn(CSE_Abstract* DC)
{
	CSE_PHSkeleton *l_tpPHSkeleton = smart_cast<CSE_PHSkeleton*>(DC);
	child_part = l_tpPHSkeleton && l_tpPHSkeleton->source_id != u16(-1);
	bool res=inherited::net_Spawn(DC);
	IKinematics		*K=PKinematics(Visual());
	CInifile* ini=K->LL_UserData();

	CPHDestroyable::Init();
	if(ini&&ini->section_exist("destroyed"))
		CPHDestroyable::Load(ini,"destroyed");

	TDamageManager* DmgManager = GetComponent<TDamageManager>();
	DmgManager->reload("damage_section",ini);

	if(ini){	
		if(ini->section_exist("immunities"))		CHitImmunity::LoadImmunities("immunities",ini);
		CPHCollisionDamageReceiver::Init();
		if(ini->section_exist("sound"))				m_destroy_sound.create(ini->r_string("sound","break_sound"),st_Effect,sg_SourceType);
		if(ini->section_exist("particles"))			m_destroy_particles=ini->r_string("particles","destroy_particles");

		if (ini->section_exist("hit_from"))
		{
			CInifile::Sect& data = ini->r_section("hit_from");
			if (data.Data.size() > 0)
			{
				for (auto I = data.Data.cbegin(); I != data.Data.cend(); ++I)
					hit_object_name.insert(I->first);
			}
		}
	}

	TParticlesPlayer* PPlayer = GetOrCreateComponent<TParticlesPlayer>();
	PPlayer->LoadParticles(K);
	RunStartupAnim(DC);

	SpatialComponent->spatial.type |= ESPATIAL_TYPE::PHYSIC_OBJECT_DESTR;
	return res;
}

void CDestroyablePhysicsObject::Hit(SHit* pHDS)
{
	SHit HDS = *pHDS;
	callback(GameObject::eHit)(
		lua_game_object(),
		HDS.power,
		HDS.dir,
		HDS.who->cast_game_object()->lua_game_object(),
		HDS.bone()
		);

	if (!hit_object_name.empty() && !hit_object_name.contains(HDS.who->cName()))
		return;

	HDS.power = CHitImmunity::AffectHit(HDS.power, HDS.hit_type);
	float hit_scale = 1.f, wound_scale = 1.f;

	TDamageManager* DmgManager = GetComponent<TDamageManager>();
	DmgManager->HitScale(HDS.bone(), hit_scale, wound_scale);

	HDS.power *= hit_scale;

	inherited::Hit(&HDS);
	m_fHealth -= HDS.power;
	if (m_fHealth <= 0.f)
	{
		CPHDestroyable::SetFatalHit(HDS);
		if (CPHDestroyable::CanDestroy())Destroy();
	}
}

void CDestroyablePhysicsObject::Destroy()
{
	VERIFY(!physics_world()->Processing());

	if (g_pGamePersistent->GameType() & eGameIDFreeMP)
	{
		setVisible(false);
	}

	CObject* cast_initiator = const_cast<CObject*>(FatalHit().initiator());
	const CGameObject *who_object = cast_initiator != nullptr ? cast_initiator->cast_game_object() : nullptr;
	callback(GameObject::eDeath)(lua_game_object(), who_object ? who_object->lua_game_object() : 0);
	CPHDestroyable::Destroy(ID(),"physic_destroyable_object");
	if(m_destroy_sound.handle())
	{
		m_destroy_sound.play_at_pos(this,Position());
	}

	if (*m_destroy_particles)
	{
		Fmatrix m; m.identity();
		/////////////////////////////////////////////////
		m.j.set(0, 1.f, 0);
		///////////////////////////////////////////////

		Fvector hdir; hdir.set(CPHDestroyable::FatalHit().direction());

		if (fsimilar(std::abs(m.j.dotproduct(hdir)), 1.f, EPS_L))
		{
			do {
				hdir.random_dir();
			} while (fsimilar(std::abs(m.j.dotproduct(hdir)), 1.f, EPS_L));
		}
		m.i.crossproduct(m.j, hdir); m.i.normalize();
		m.k.crossproduct(m.i, m.j);

		TParticlesPlayer* PPlayer = GetOrCreateComponent<TParticlesPlayer>();
		PPlayer->StartParticles(m_destroy_particles, m, ID());
	}

	SheduleRegister();
}
void CDestroyablePhysicsObject::InitServerObject(CSE_Abstract* D)
{
	CSE_PHSkeleton* ps = smart_cast<CSE_PHSkeleton*>(D);
	R_ASSERT(ps);
	if (ps->_flags.test(CSE_PHSkeleton::flSpawnCopy))
		inherited::InitServerObject(D);
	else
		CPHDestroyable::InitServerObject(D);

	CSE_ALifeObjectPhysic* PO = smart_cast<CSE_ALifeObjectPhysic*>(D);
	if (PO)PO->type = epotSkeleton;
}

void CDestroyablePhysicsObject::shedule_Update(u32 dt)
{
	PROF_EVENT("CDestroyablePhysicsObject::shedule_Update")
	inherited::shedule_Update(dt);
	CPHDestroyable::SheduleUpdate(dt);
}

bool CDestroyablePhysicsObject::CanRemoveObject()
{
	bool CanRemove = true;

	if (TParticlesPlayer* PPlayer = GetComponent<TParticlesPlayer>())
	{
		CanRemove = !PPlayer->IsPlaying();
	}
	return CanRemove && !m_destroy_sound.is_playing();
}

DLL_Pure *CDestroyablePhysicsObject::_construct()
{
	TDamageManager& DmgManager = CreateComponent<TDamageManager>();
	return inherited::_construct();
}