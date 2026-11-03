////////////////////////////////////////////////////////////////////////////
//	Module 		: stalker_combat_actions.h
//	Created 	: 25.03.2004
//  Modified 	: 26.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Stalker combat action classes
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "stalker_combat_action_base.h"
#include "random32.h"

class CCoverPoint;

namespace MonsterSpace {
	enum EBodyState;
	enum EMovementType;
}

//////////////////////////////////////////////////////////////////////////
// CStalkerActionGetItemToKill
//////////////////////////////////////////////////////////////////////////

class CStalkerActionGetItemToKill : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionGetItemToKill	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionMakeItemKilling
//////////////////////////////////////////////////////////////////////////

class CStalkerActionMakeItemKilling : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionMakeItemKilling	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize						() override;
	virtual void		execute							() override;
	virtual void		finalize						() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionRetreatFromEnemy
//////////////////////////////////////////////////////////////////////////

class CStalkerActionRetreatFromEnemy : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionRetreatFromEnemy	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize						() override;
	virtual void		execute							() override;
	virtual void		finalize						() override;
	virtual u16	weight					(const CSConditionState &condition0, const CSConditionState &condition1) const override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionGetReadyToKill
//////////////////////////////////////////////////////////////////////////

class CStalkerActionGetReadyToKill : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

private:
	MonsterSpace::EBodyState		m_body_state;
	MonsterSpace::EMovementType		m_movement_type;
	bool							m_affect_properties;
	bool							m_enable_enemy_change;

public:
						CStalkerActionGetReadyToKill(bool affect_properties, CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionKillEnemy
//////////////////////////////////////////////////////////////////////////

class CStalkerActionKillEnemy : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionKillEnemy		(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionTakeCover
//////////////////////////////////////////////////////////////////////////

class CStalkerActionTakeCover : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

private:
	MonsterSpace::EBodyState		m_body_state;
	MonsterSpace::EMovementType		m_movement_type;

public:
						CStalkerActionTakeCover		(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize						() override;
	virtual void		execute							() override;
	virtual void		finalize						() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionLookOut
//////////////////////////////////////////////////////////////////////////

class CStalkerActionLookOut : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

private:
	u32					m_last_change_time;
	CRandom32			m_crouch_look_out_random;

public:
						CStalkerActionLookOut		(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionHoldPosition
//////////////////////////////////////////////////////////////////////////

class CStalkerActionHoldPosition : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionHoldPosition	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionGetDistance
//////////////////////////////////////////////////////////////////////////

class CStalkerActionGetDistance : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionGetDistance	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDetourEnemy
//////////////////////////////////////////////////////////////////////////

class CStalkerActionDetourEnemy : public CStalkerActionCombatBase {
private:
	const CCoverPoint	*m_enemy_cover;

protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionDetourEnemy	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionSearchEnemy
//////////////////////////////////////////////////////////////////////////

class CStalkerActionSearchEnemy : public CStalkerActionCombatBase {
private:
	const CCoverPoint	*m_enemy_cover;
	u32					m_last_hit_time;

protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionSearchEnemy	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};


//////////////////////////////////////////////////////////////////////////
// CStalkerActionPostCombatWait
//////////////////////////////////////////////////////////////////////////

class CStalkerActionPostCombatWait : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionPostCombatWait(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionHideFromGrenade
//////////////////////////////////////////////////////////////////////////

class CStalkerActionHideFromGrenade : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionHideFromGrenade(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionSuddenAttack
//////////////////////////////////////////////////////////////////////////

class CStalkerActionSuddenAttack : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionSuddenAttack	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionKillEnemyIfPlayerOnThePath
//////////////////////////////////////////////////////////////////////////

class CStalkerActionKillEnemyIfPlayerOnThePath : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionKillEnemyIfPlayerOnThePath(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize								() override;
	virtual void		execute									() override;
	virtual void		finalize								() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionCriticalHit
//////////////////////////////////////////////////////////////////////////

class CStalkerActionCriticalHit : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionCriticalHit	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerCombatActionThrowGrenade
//////////////////////////////////////////////////////////////////////////

class CStalkerCombatActionThrowGrenade : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

	ALife::_OBJECT_ID	m_grenade_id;

public:
						CStalkerCombatActionThrowGrenade(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize						() override;
	virtual void		execute							() override;
	virtual void		finalize						() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerCombatActionSmartCover
//////////////////////////////////////////////////////////////////////////

class CStalkerCombatActionSmartCover : public CStalkerActionCombatBase {
private:
	typedef CStalkerActionCombatBase					inherited;

public:
						CStalkerCombatActionSmartCover	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize						() override;
	virtual void		execute							() override;
	virtual void		finalize						() override;

private:
	bool				m_check_can_kill_enemy;
};