#pragma once
#include "PhysicsShellHolder.h"
#include "damage_manager.h"
#include "EntityCondition.h"

// refs
class ENGINE_API CCameraBase;
class ENGINE_API CMotionDef;
class IKinematics;
class CBoneInstance;
class CWeaponList;
class CPHMovementControl;
class CHudItem;
class CActor;
class CAI_Stalker;
class CEntityAlive;
class CInventoryOwner;

class CEntity :
	public CPhysicsShellHolder
{
	friend class CEntityCondition;
private:
	typedef	CPhysicsShellHolder		inherited;			

protected:
	//время через которое мертвое тело убирется с уровня
	ALife::_TIME_ID					m_dwBodyRemoveTime;	
protected:
	virtual	CEntityConditionSimple	*create_entity_condition	(CEntityConditionSimple* ec);

public:
	CEntityConditionSimple* m_entity_condition = nullptr;

	IC float					GetfHealth			() const			{ return m_entity_condition->GetHealth(); }
	IC float					SetfHealth			(float value)		{ m_entity_condition->SetHealth( value ) ; return value;}
	float						m_fMorale;
	// Team params
	int							id_Team;
	int							id_Squad;
	int							id_Group;
	
	virtual void		ChangeTeam				(int team, int squad, int group);

	struct SEntityState
	{
		u32		bJump	:1;
		u32		bCrouch	:1;
		u32		bFall	:1;
		u32		bSprint	:1;
		float	fVelocity;
		float	fAVelocity;
	};
	
	float					m_fFood;

	// General
	CEntity					();
	virtual ~CEntity		();
	virtual DLL_Pure		*_construct				();
	virtual CEntity*		cast_entity			() override {return this;}
	virtual CActor*			cast_actor			() override {return nullptr;}
	virtual CAI_Stalker* cast_stalker() override { return nullptr; }
	virtual CEntityAlive* cast_entity_alive() override { return nullptr; }
	virtual CInventoryOwner* cast_inventory_owner() override { return nullptr; }
	virtual CGameObject* cast_game_object() override { return this; }

public:

	// Core events
	virtual void Load(const char* section) override;
	virtual void reinit() override;
	virtual void reload(const char* section) override;
	bool net_Spawn(CSE_Abstract* DC) override;
	virtual void net_Destroy() override;
	
	virtual void Serialize(ISaveObject& Object) override { inherited::Serialize(Object); };
	
	virtual void			shedule_Update		(u32 dt);

//	virtual float			g_Health			()const	{ return GetfHealth();}
/*	virtual*/ IC float			GetMaxHealth		()const	{ return m_entity_condition->max_health();	}
/*	virtual*/ IC void			SetMaxHealth		(float v)	{ m_entity_condition->max_health()=v;}

	/*virtual*/ IC bool		g_Alive				()const	{ return GetfHealth()>0; }
	virtual bool			g_State				(SEntityState&) const	{return false;}
	
			bool			AlreadyDie			()			{return  0!=GetLevelDeathTime()?true:false;}
			ALife::_TIME_ID	GetGameDeathTime	()const		{return m_game_death_time;}
			u32				GetLevelDeathTime	()const		{return m_level_death_time;}
	
	virtual float			CalcCondition		(float hit);

	// if false - hits go through and dont hit
	virtual bool            in_solid_state      () { return true; }

	int						g_Team				()const	{ return id_Team;	}
	int						g_Squad				()const	{ return id_Squad;	}
	int						g_Group				()const	{ return id_Group;	}

	// Health calculations
	virtual	void			Hit					(SHit* pHDS);
	virtual void			HitSignal			(float P, Fvector &local_dir,	CObject* who, s16 element)		= 0;
	virtual void			HitImpulse			(float P, Fvector &vWorldDir, 	Fvector& vLocalDir)	= 0;

	virtual void			Die					(CObject* who);
//			void			KillEntity			(CObject* who);
			void			KillEntity			(ALife::_OBJECT_ID whoID, bool bypass_actor_check = false);
		
	// Events
	virtual void			OnEvent				( NET_Packet& P, u16 type		);

	virtual void			g_fireParams		(const CHudItem*, Fvector &, Fvector &){}; 
	virtual bool			g_stateFire			() {return true;}
	//time of entity death
	u32						m_level_death_time;
	ALife::_TIME_ID			m_game_death_time;

			void			set_death_time		();
	virtual	void			set_ready_to_save	();

private:
	ALife::_OBJECT_ID		m_killer_id;

public:
	IC ALife::_OBJECT_ID	killer_id				() const {return m_killer_id;};
	virtual	bool			use_simplified_visual	() const {return false;};

public:
	virtual	void			on_before_change_team	();
	virtual	void			on_after_change_team	();

private:
	bool					m_registered_member;
	bool					m_isSkipKillActor = false;
	const char*				m_onSkipKillActor = {};

};
