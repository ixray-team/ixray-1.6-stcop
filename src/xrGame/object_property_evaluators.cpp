////////////////////////////////////////////////////////////////////////////
//	Module 		: object_property_evaluators.cpp
//	Created 	: 12.03.2004
//  Modified 	: 26.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Object property evaluators
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "object_property_evaluators.h"
#include "Weapon.h"
#include "ai/stalker/ai_stalker.h"
#include "Inventory.h"
#include "Missile.h"
#include "FoodItem.h"
#include "WeaponMagazined.h"

//////////////////////////////////////////////////////////////////////////
// CObjectPropertyEvaluatorState
//////////////////////////////////////////////////////////////////////////

CObjectPropertyEvaluatorState::CObjectPropertyEvaluatorState	(CWeapon *item, CAI_Stalker *owner, u32 state, bool equality) :
	inherited		(item,owner),
	m_state			(state),
	m_equality		(equality)
{
}

bool CObjectPropertyEvaluatorState::evaluate	()
{
	VERIFY			(m_item);
	return			(bool((m_item->GetState() == m_state) == m_equality));
}

CObjectPropertyEvaluatorWeaponHidden::CObjectPropertyEvaluatorWeaponHidden(CWeapon *item, CAI_Stalker *owner):
	inherited		(item,owner)
{
}

bool CObjectPropertyEvaluatorWeaponHidden::evaluate()
{
	VERIFY(m_item);

	return ((m_item !=  m_item->m_pInventory->ActiveItem()) || (m_item->GetState() == CWeapon::eShowing));
}
//////////////////////////////////////////////////////////////////////////
// CObjectPropertyEvaluatorAmmo
//////////////////////////////////////////////////////////////////////////

CObjectPropertyEvaluatorAmmo::CObjectPropertyEvaluatorAmmo	(CWeapon *item, CAI_Stalker *owner, u32 ammo_type) :
	inherited		(item,owner),
	m_ammo_type		(ammo_type)
{
}

bool CObjectPropertyEvaluatorAmmo::evaluate	()
{
	if (!m_ammo_type)
		return !!m_item->GetSuitableAmmoTotal();

	return false;
}

//////////////////////////////////////////////////////////////////////////
// CObjectPropertyEvaluatorEmpty
//////////////////////////////////////////////////////////////////////////

CObjectPropertyEvaluatorEmpty::CObjectPropertyEvaluatorEmpty(CWeapon *item, CAI_Stalker *owner, u32 ammo_type) :
	inherited		(item,owner),
	m_ammo_type		(ammo_type)
{
}

bool CObjectPropertyEvaluatorEmpty::evaluate()
{
	if (!m_ammo_type)
		return !m_item->GetAmmoElapsed();

	return false;
}

//////////////////////////////////////////////////////////////////////////
// CObjectPropertyEvaluatorFull
//////////////////////////////////////////////////////////////////////////

CObjectPropertyEvaluatorFull::CObjectPropertyEvaluatorFull	(CWeapon *item, CAI_Stalker *owner, u32 ammo_type) :
	inherited		(item,owner),
	m_ammo_type		(ammo_type)
{
}

bool CObjectPropertyEvaluatorFull::evaluate	()
{
	if (!m_ammo_type)
		return		(bool(m_item->GetAmmoElapsed() == m_item->GetAmmoMagSize()));
	else
		return false;
}

//////////////////////////////////////////////////////////////////////////
// CObjectPropertyEvaluatorReady
//////////////////////////////////////////////////////////////////////////

CObjectPropertyEvaluatorReady::CObjectPropertyEvaluatorReady(CWeapon *item, CAI_Stalker *owner, u32 ammo_type) :
	inherited		(item,owner),
	m_ammo_type		(ammo_type)
{
}

bool CObjectPropertyEvaluatorReady::evaluate	()
{
	if (!m_ammo_type)
		return (bool(!m_item->IsMisfire() && (m_item->GetAmmoElapsed() && (m_item->GetState() != CWeapon::eReload))));

	return false;
}

//////////////////////////////////////////////////////////////////////////
// CObjectPropertyEvaluatorQueue
//////////////////////////////////////////////////////////////////////////

CObjectPropertyEvaluatorQueue::CObjectPropertyEvaluatorQueue(CWeapon *item, CAI_Stalker *owner, u32 type) :
	inherited		(item,owner),
	m_type			(type)
{
	m_magazined		= item->cast_weapon_magazined();
}

bool CObjectPropertyEvaluatorQueue::evaluate	()
{
	return			(!m_magazined ? true : !m_magazined->StopedAfterQueueFired());
}

//////////////////////////////////////////////////////////////////////////
// CObjectPropertyEvaluatorNoItems
//////////////////////////////////////////////////////////////////////////

CObjectPropertyEvaluatorNoItems::CObjectPropertyEvaluatorNoItems(CAI_Stalker *owner)
{
	m_object		= owner;
}

bool CObjectPropertyEvaluatorNoItems::evaluate	()
{
	PIItem I = object().inventory().ActiveItem();
	if (!I)
		return		(true);
	
	if (!I->cast_hud_item() || I->cast_hud_item()->IsHidden())
		return		(true);

	if (I->cast_hud_item() && I->cast_hud_item()->IsShowing())
		return		(true);

	return			(false);
}

//////////////////////////////////////////////////////////////////////////
// CObjectPropertyEvaluatorMissile
//////////////////////////////////////////////////////////////////////////

CObjectPropertyEvaluatorMissile::CObjectPropertyEvaluatorMissile	(CMissile *item, CAI_Stalker *owner, u32 state, bool equality) :
	inherited		(item,owner),
	m_state			(state),
	m_equality		(equality)
{
}

bool CObjectPropertyEvaluatorMissile::evaluate	()
{
	VERIFY			(m_item);
	return ((m_item->GetState() == m_state) == m_equality);
}

//////////////////////////////////////////////////////////////////////////
// CObjectPropertyEvaluatorMissileStarted
//////////////////////////////////////////////////////////////////////////

CObjectPropertyEvaluatorMissileStarted::CObjectPropertyEvaluatorMissileStarted	(CMissile *item, CAI_Stalker *owner) :
	inherited		(item,owner)
{
}

bool CObjectPropertyEvaluatorMissileStarted::evaluate	()
{
	VERIFY			(m_item);
	if (m_item->GetState() != CMissile::eThrow)
		return		(false);

	return			(true);
}

//////////////////////////////////////////////////////////////////////////
// CObjectPropertyEvaluatorMissileHidden
//////////////////////////////////////////////////////////////////////////

CObjectPropertyEvaluatorMissileHidden::CObjectPropertyEvaluatorMissileHidden	(CMissile *item, CAI_Stalker *owner) :
	inherited		(item,owner)
{
}

bool CObjectPropertyEvaluatorMissileHidden::evaluate	()
{
	VERIFY			(m_item);

	if (!object().inventory().ActiveItem())
		return		(true);

	if (object().inventory().ActiveItem() != m_item)
		return		(true);

	if (m_item->GetState() == CMissile::eHidden)
		return		(true);

	if (m_item->GetState() == CMissile::eShowing)
		return		(true);

	return			(false);
}
