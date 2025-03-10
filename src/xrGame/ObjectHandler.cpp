
#include "StdAfx.h"
#if !USE_OLD_OBJECT_PLANNER
#include "ObjectHandler.h"
#include "ObjectHandlerSpace.h"
#include "ai_monster_space.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "WeaponMagazined.h"
#include "ai/stalker/ai_stalker.h"
#include "Inventory.h"
#include "Torch.h"
#include "../Include/xrRender/Kinematics.h"
#include "enemy_manager.h"
#include "ai_object_location.h"
#include "EffectorShot.h"
#include "RbmkObjectHandlerPlanner.h"

CObjectHandler::CObjectHandler		(CAI_Stalker *object)
{
	m_planner					= new FRbmkObjectHandlerPlanner(object);
	m_inventory_actual			= false;
//	m_last_enemy_for_best_weapon= 0;
}

CObjectHandler::~CObjectHandler		()
{
	xr_delete					(m_planner);
}

void CObjectHandler::Load			(LPCSTR section)
{
	inherited::Load				(section);
}

void CObjectHandler::reinit			(CAI_Stalker *object)
{
	inherited::reinit			();
	m_hammer_is_clutched		= false;
	m_planner->Initialize();
	IKinematics					*kinematics = smart_cast<IKinematics*>(m_planner->GetOwner()->Visual());
	m_r_hand					= kinematics->LL_BoneID(pSettings->r_string(*m_planner->GetOwner()->cNameSect(),"weapon_bone0"));
	m_l_finger1					= kinematics->LL_BoneID(pSettings->r_string(*m_planner->GetOwner()->cNameSect(),"weapon_bone1"));
	m_r_finger2					= kinematics->LL_BoneID(pSettings->r_string(*m_planner->GetOwner()->cNameSect(),"weapon_bone2"));
	m_strap_object_id			= ALife::_OBJECT_ID(-1);
	m_strap_bone0				= -1;
	m_strap_bone1				= -1;
	m_clutched_hammer_enabled	= false;
}

void CObjectHandler::reload			(LPCSTR section)
{
	inherited::reload			(section);
}

BOOL CObjectHandler::net_Spawn		(CSE_Abstract* DC)
{
	if (!inherited::net_Spawn(DC))
		return					(FALSE);

	CSE_Abstract				*abstract = static_cast<CSE_Abstract*>(DC);
	CSE_ALifeTraderAbstract		*trader = smart_cast<CSE_ALifeTraderAbstract*>(abstract);
	VERIFY						(trader);

	m_infinite_ammo				= !!trader->m_trader_flags.test(CSE_ALifeTraderAbstract::eTraderFlagInfiniteAmmo);
	return						(TRUE);
}

void CObjectHandler::OnItemTake		(CInventoryItem *inventory_item)
{
	inherited::OnItemTake		(inventory_item);

	m_inventory_actual			= false;

	m_planner->AddObject			(inventory_item);

	if (m_planner->GetOwner()->g_Alive())
		switch_torch			(inventory_item,true);

	if (inventory_item->useful_for_NPC() && (inventory_item->object().cNameSect() == m_item_to_spawn)) {
		m_item_to_spawn			= shared_str();
		m_ammo_in_box_to_spawn	= 0;
	}

	CWeapon						*weapon = smart_cast<CWeapon*>(inventory_item);
	if (weapon)
	{
		CameraRecoil cam_recoil_copy;
		cam_recoil_copy.Clone( weapon->cam_recoil );
		cam_recoil_copy.RelaxSpeed = cam_recoil_copy.RelaxSpeed_AI;
		m_planner->GetOwner()->weapon_shot_effector().Initialize( cam_recoil_copy );
	}
}

void CObjectHandler::OnItemDrop		(CInventoryItem *inventory_item, bool just_before_destroy)
{
	inherited::OnItemDrop	(inventory_item, just_before_destroy);

	m_inventory_actual		= false;
	
	if (m_infinite_ammo && m_planner->GetOwner()->g_Alive() && !inventory_item->useful_for_NPC()) {
		CWeaponAmmo				*weapon_ammo = smart_cast<CWeaponAmmo*>(inventory_item);
		if (weapon_ammo) {
			Level().spawn_item		(*weapon_ammo->cNameSect(),m_planner->GetOwner()->Position(),m_planner->GetOwner()->ai_location().level_vertex_id(),m_planner->GetOwner()->ID());
			m_item_to_spawn			= weapon_ammo->cNameSect();
			m_ammo_in_box_to_spawn	= weapon_ammo->m_boxSize;
		}
	}

	m_planner->RemoveObject		(inventory_item->cast_game_object());

	switch_torch				(inventory_item,false);
}

CInventoryItem *CObjectHandler::best_weapon() const
{
	if (!m_planner->GetOwner()->g_Alive())
		return									(0);

	m_planner->GetOwner()->update_best_item_info	();
	return										(m_planner->GetOwner()->m_best_item_to_kill);
}

void CObjectHandler::update		()
{
	START_PROFILE("Object Handler")
	m_planner->GoapPlanner.Update		();
	STOP_PROFILE
}

void CObjectHandler::set_goal	(MonsterSpace::EObjectAction object_action, CGameObject *game_object, u32 min_queue_size, u32 max_queue_size, u32 min_queue_interval, u32 max_queue_interval)
{
	m_planner->SetGoap(object_action,game_object,min_queue_size,max_queue_size,min_queue_interval,max_queue_interval);
}

void CObjectHandler::set_goal	(MonsterSpace::EObjectAction object_action, CInventoryItem *inventory_item, u32 min_queue_size, u32 max_queue_size, u32 min_queue_interval, u32 max_queue_interval)
{
	set_goal(object_action,inventory_item ? &inventory_item->object() : 0,min_queue_size,max_queue_size,min_queue_interval,max_queue_interval);
}

bool CObjectHandler::goal_reached	()
{
	return					(m_planner->GoapPlanner.IsReached());
}

void CObjectHandler::weapon_bones	(int &b0, int &b1, int &b2) const
{
	CWeapon						*weapon = smart_cast<CWeapon*>(inventory().ActiveItem());
	if (!weapon || !m_planner->bStrapped) {
		if (weapon)
			weapon->strapped_mode	(false);
		b0						= m_r_hand;
		b1						= m_r_finger2;
		b2						= m_l_finger1;
		return;
	}

	THROW3						(weapon->can_be_strapped(),"Cannot strap weapon",*weapon->cName());

	if (weapon->ID() != m_strap_object_id) {
		IKinematics				*kinematics = smart_cast<IKinematics*>(m_planner->GetOwner()->Visual());
		m_strap_bone0			= kinematics->LL_BoneID(weapon->strap_bone0());
		m_strap_bone1			= kinematics->LL_BoneID(weapon->strap_bone1());
		m_strap_object_id		= weapon->ID();
	}

	weapon->strapped_mode		(true);
	b0							= m_strap_bone0;
	b1							= m_strap_bone1;
	b2							= b1;
}

bool CObjectHandler::weapon_strapped	() const
{
	CWeapon						*weapon = smart_cast<CWeapon*>(inventory().ActiveItem());
	if (!weapon)
		return					(false);

	return						(weapon_strapped(weapon));
}

void CObjectHandler::actualize_strap_mode	(CWeapon *weapon) const
{
	VERIFY						(weapon);

	if (!m_planner->bStrapped) {
//		Msg						( "[%6d][%s] actualizing: weapon_strapped = false", Device.dwTimeGlobal, m_planner->GetOwner()->cName().c_str() );
		weapon->strapped_mode	(false);
		return;
	}

	THROW3						(weapon->can_be_strapped(),"Cannot strap weapon",*weapon->cName());
//	Msg							( "[%6d][%s] actualizing: weapon_strapped = true", Device.dwTimeGlobal, m_planner->GetOwner()->cName().c_str() );
	weapon->strapped_mode		(true);
}

bool CObjectHandler::weapon_strapped	(CWeapon *weapon) const
{
	VERIFY						(weapon);

	if (!weapon->can_be_strapped()) {
//		Msg						( "[%6d][%s] weapon_strapped = false0", Device.dwTimeGlobal, m_planner->GetOwner()->cName().c_str() );
		return					(false);
	}

	static shared_str NAME_StrappingToIdle = "StrappingToIdle";
	static shared_str NAME_Strapping = "Strapping";
	static shared_str NAME_Unstrapping2Idle = "UnstrappingToIdle";
	static shared_str NAME_Unstrapping = "Unstrapping";
	bool const almost_strapped	= m_planner->CurrentActionStateName() == NAME_StrappingToIdle;
	if (
		almost_strapped ||
		(m_planner->CurrentActionStateName() == NAME_Strapping) ||
		(m_planner->CurrentActionStateName() == NAME_Unstrapping2Idle) ||
		(m_planner->CurrentActionStateName() == NAME_Unstrapping)
	) {
//		Msg						( "[%6d][%s] weapon_strapped = %s1", Device->dwTimeGlobal, m_planner->GetOwner()->cName().c_str(), almost_strapped && !m_planner->m_storage.property(ObjectHandlerSpace::eWorldPropertyStrapped2Idle) ? "true" : "false" );

		if ( !almost_strapped || m_planner->bStrapped2Idle )
			return				(false);
	}
	actualize_strap_mode		(weapon);

	return						(weapon->strapped_mode());
}

bool CObjectHandler::weapon_unstrapped	() const
{
	CWeapon						*weapon = smart_cast<CWeapon*>(inventory().ActiveItem());
	if (!weapon) {
//		Msg						( "[%6d][%s] no active item!!(%d)(%s)", Device.dwTimeGlobal, m_planner->GetOwner()->cName().c_str(), inventory().GetActiveSlot(), inventory().ItemFromSlot(3) ? inventory().ItemFromSlot(3)->object().cName().c_str() : "<no_item>" );
		return					(true);
	}

	return						(weapon_unstrapped(weapon));
}

bool CObjectHandler::weapon_unstrapped	(CWeapon *weapon) const
{
	VERIFY						(weapon);

	if (!weapon->can_be_strapped()) {
//		Msg						( "[%6d][%s] weapon_unstrapped = true0", Device.dwTimeGlobal, m_planner->GetOwner()->cName().c_str() );
		return					(true);
	}

	static shared_str NAME_StrappingToIdle = "StrappingToIdle";
	static shared_str NAME_Strapping = "Strapping";
	static shared_str NAME_Unstrapping2Idle = "UnstrappingToIdle";
	static shared_str NAME_Unstrapping = "Unstrapping";
	static shared_str NAME_Strapped = "Strapped";
	bool const almost_unstrapped = m_planner->CurrentActionStateName() == NAME_Unstrapping2Idle;
	if (
		almost_unstrapped ||
		(m_planner->CurrentActionStateName() == NAME_StrappingToIdle) ||
		(m_planner->CurrentActionStateName() == NAME_Strapping) ||
		(m_planner->CurrentActionStateName() == NAME_Unstrapping)
		) {

//		Msg						( "[%6d][%s] weapon_unstrapped = %s1", Device->dwTimeGlobal, m_planner->GetOwner()->cName().c_str(), almost_unstrapped && !m_planner->m_storage.property(ObjectHandlerSpace::eWorldPropertyStrapped2Idle) ? "true" : "false" );

		if ( !almost_unstrapped || m_planner->bStrapped2Idle )
			return				(false);
	}

	actualize_strap_mode		(weapon);

	VERIFY						(
		(m_planner->CurrentActionStateName() != NAME_Strapped) ||
		weapon->strapped_mode()
	);

	return						(!weapon->strapped_mode());
}

IC	void CObjectHandler::switch_torch	(CInventoryItem *inventory_item, bool value)
{
	CTorch						*torch = smart_cast<CTorch*>(inventory_item);
	if (torch && attached(torch) && m_planner->GetOwner()->g_Alive())
		torch->Switch			(value);
}

void CObjectHandler::attach				(CInventoryItem *inventory_item)
{
	inherited::attach			(inventory_item);
	switch_torch				(inventory_item,true);
}

void CObjectHandler::detach				(CInventoryItem *inventory_item)
{
	switch_torch				(inventory_item,false);
	inherited::detach			(inventory_item);
}

extern Flags32 g_uCommonFlags;

bool CObjectHandler::can_use_dynamic_lights	()
{	
	// flAiUseTorchDynamicLights == 1
	return						(!!g_uCommonFlags.test(1));
}
void CObjectHandler::aim_time			(const CWeapon &weapon, const u32 &aim_time) const
{
	m_planner->SetAimTime(&weapon,aim_time);
}

u32 CObjectHandler::aim_time			(const CWeapon &weapon) const
{
	return m_planner->GetAimTime(&weapon);
}

bool CObjectHandler::is_weapon_going_to_be_strapped	(const CGameObject * object ) const
{
	return m_planner->IsWeaponGoingToBeStrapped(object);
}
#endif