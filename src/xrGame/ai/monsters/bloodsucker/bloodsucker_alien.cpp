#include "stdafx.h"
#include "../../../level.h"
#include "../../../Actor.h"
#include "../../../ActorEffector.h"
#include "../../../inventory.h"
#include "../../../HudItem.h"
#include "../../../../xrEngine/CustomHUD.h"

#include "bloodsucker_alien_pp.h"
#include "bloodsucker_alien.h"

#include "bloodsucker.h"

#include "bloodsucker_alien_effector.h"

#define EFFECTOR_ID_GEN(type) (type( u32(u64(this) & u32(-1)) ))

CustomBloodsuckerAlien::CustomBloodsuckerAlien()
{
	m_active = false;
	m_crosshair_show = false;
	m_effector = nullptr;
	m_effector_pp = nullptr;
	m_object	= nullptr;
}

CustomBloodsuckerAlien::~CustomBloodsuckerAlien()
{

}

void CustomBloodsuckerAlien::init_external(CustomBloodsucker*obj)
{
	m_object	= obj;
}

void CustomBloodsuckerAlien::reinit()
{
	m_active				= false;	
	m_crosshair_show		= false;
}

void CustomBloodsuckerAlien::activate()
{
	if (m_active) return;

	VERIFY	(Actor());
	m_object->CControlledActor::install			(Actor());
	m_object->CControlledActor::dont_need_turn	();

	if (!m_object->EnemyMan.get_enemy())	m_object->EnemyMan.add_enemy(Actor());

//.	Actor()->inventory().setSlotsBlocked			(true);
	Actor()->SetWeaponHideState(INV_STATE_BLOCK_ALL, true);

	// hide crosshair
	m_crosshair_show			= !!psHUD_Flags.is(HUD_CROSSHAIR_RT);
	if (m_crosshair_show)		psHUD_Flags.set(HUD_CROSSHAIR_RT,FALSE);

	// Start effector
	m_effector_pp				= new CustomBloodsuckerAlienEffectorPP	(m_object->pp_vampire_effector, EFFECTOR_ID_GEN(EEffectorPPType));
	Actor()->Cameras().AddPPEffector	(m_effector_pp);
	
	m_effector					= new CustomBloodsuckerAlienEffector(EFFECTOR_ID_GEN(ECamEffectorType), m_object);
	Actor()->Cameras().AddCamEffector	(m_effector);

	// make invisible
	m_object->state_invisible	= true;
	m_object->setVisible		(false);

	m_active					= true;
}

void CustomBloodsuckerAlien::deactivate()
{
	if (!m_active) return;

	m_object->CControlledActor::release			();

	Actor()->SetWeaponHideState(INV_STATE_BLOCK_ALL, false);
	if (m_crosshair_show)							psHUD_Flags.set(HUD_CROSSHAIR_RT,TRUE);

	// Stop camera effector
	Actor()->Cameras().RemoveCamEffector(EFFECTOR_ID_GEN(ECamEffectorType));
	m_effector						= 0;
	
	// Stop postprocess effector
	Actor()->Cameras().RemovePPEffector(EFFECTOR_ID_GEN(EEffectorPPType));
	m_effector_pp->Destroy			();
	m_effector_pp					= 0;

	m_active						= false;

	// make visible
	m_object->state_invisible	= false;
	m_object->setVisible		(true);
}
