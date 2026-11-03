#ifndef PHYSICSSHELL_HOLDER_H
#define PHYSICSSHELL_HOLDER_H

#include "GameObject.h"
#include "ParticlesPlayer.h"
#include "../xrEngine/IObjectPhysicsCollision.h"
#include "../xrPhysics/IPhysicsShellHolder.h"
#include "../xrScripts/script_export_space.h"

class CPHDestroyable;
class CPHCollisionDamageReceiver;
class CPHSoundPlayer;
class IDamageSource;
class CPHSkeleton;
class CCharacterPhysicsSupport;
class ICollisionDamageInfo;
class TIKLimbsController;

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CPhysicsShellHolder:
	public CGameObject,
	public IObjectPhysicsCollision,
	public IPhysicsShellHolder
	
{
	bool				b_sheduled;
public:
	void	SheduleRegister		(){if(!IsSheduled())shedule_register();b_sheduled=true;}
	void	SheduleUnregister	(){if(IsSheduled())shedule_unregister();b_sheduled=false;}
IC	bool	IsSheduled			(){return b_sheduled;}	
public:

	typedef CGameObject inherited;
	

	CPhysicsShell			*m_pPhysicsShell;


			CPhysicsShellHolder							();
	virtual ~CPhysicsShellHolder						() override;

private:
	Fvector					m_overriden_activation_speed;
	bool					m_activation_speed_is_overriden;

public:
	virtual bool ActivationSpeedOverriden(Fvector& dest, bool clear_override);
	virtual void SetActivationSpeedOverride(Fvector const& speed);

	IC CPhysicsShell	*&PPhysicsShell				()		
	{
		return m_pPhysicsShell;
	}

	IC CPhysicsShellHolder*	PhysicsShellHolder	()
	{
		return this;
	}

	virtual const IObjectPhysicsCollision* physics_collision() override;
	virtual const IPhysicsShell* physics_shell() const override;
	virtual IPhysicsShell* physics_shell() override;
	virtual const IPhysicsElement* physics_character() const override;
	virtual CPHDestroyable* ph_destroyable() { return nullptr; }
	virtual ICollisionDamageReceiver* PHCollisionDamageReceiver() { return nullptr; }
	virtual CPHSkeleton* PHSkeleton() { return nullptr; }

	virtual CPhysicsShellHolder* cast_physics_shell_holder() override { return this; }
	virtual CPhysicItem* cast_physics_item() override { return nullptr; }
	virtual CGameObject* cast_game_object() override { return this; }
	virtual IDamageSource* cast_IDamageSource() { return nullptr; }

	virtual	CCharacterPhysicsSupport* character_physics_support() { return nullptr; }
	virtual	const CCharacterPhysicsSupport* character_physics_support() const { return nullptr; }
	virtual ICollisionHitCallback* get_collision_hit_callback() { return nullptr; }

	virtual void set_collision_hit_callback(ICollisionHitCallback* cc) {}
	virtual void _BCL enable_notificate() override {}
public:

	virtual void			PHGetLinearVell		(Fvector& velocity);
	virtual void			PHSetLinearVell		(Fvector& velocity);
	virtual void			PHSetMaterial		(const char* m);
	virtual void			PHSetMaterial		(u16 m);
			void			PHSaveState			(NET_Packet &P);
			void			PHLoadState			(IReader &P);
			void			PHSerializeState(ISaveObject& Object);
	virtual f32				GetMass				();
	virtual	void			PHHit				(SHit &H);
	virtual void			Hit					(SHit* pHDS) override;
///////////////////////////////////////////////////////////////////////
	virtual u16				PHGetSyncItemsNumber();
	virtual CPHSynchronize*	PHGetSyncItem		(u16 item);
	virtual void			PHUnFreeze			();
	virtual void			PHFreeze			();
	virtual float			EffectiveGravity	();
///////////////////////////////////////////////////////////////
	virtual void			create_physic_shell		();
	virtual void			activate_physic_shell	();
	virtual void			setup_physic_shell		();
	virtual void			deactivate_physics_shell ();

	virtual void			net_Destroy			() override;
	virtual bool			net_Spawn			(CSE_Abstract*	DC) override;
	virtual void			save				(NET_Packet &output_packet) override;
	virtual void			load				(IReader &input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;
			void			init				();

	virtual void			OnChangeVisual		() override;
	//для наследования CParticlesPlayer
	virtual void			UpdateCL			() override;
			void			correct_spawn_pos	();
protected:
	virtual	bool			has_shell_collision_place( const CPhysicsShellHolder* obj ) const;
	virtual void			on_child_shell_activate	 ( CPhysicsShellHolder* obj );
public:
	virtual bool			register_schedule	() const override;

private://IPhysicsShellHolder
	virtual Fmatrix&				_BCL					ObjectXFORM							() override;
	virtual Fvector&				_BCL					ObjectPosition						() override;
	virtual const char*					_BCL					ObjectName							()		const override;
	virtual const char*					_BCL					ObjectNameVisual					()		const override;
	virtual const char*					_BCL					ObjectNameSect						()		const override;
	virtual bool					_BCL					ObjectGetDestroy					()		const override;
	virtual ICollisionHitCallback*	_BCL 					ObjectGetCollisionHitCallback		() override;
	virtual ALife::_OBJECT_ID						_BCL	ObjectID							()		const override;
	virtual ICollisionForm*			_BCL					ObjectCollisionModel				() override;
	virtual IKinematics*			_BCL					ObjectKinematics					() override;
	virtual IDamageSource*			_BCL					ObjectCastIDamageSource				() override;
	virtual void					_BCL					ObjectProcessingDeactivate			() override;
	virtual void					_BCL					ObjectProcessingActivate			() override;
	virtual void					_BCL					ObjectSpatialMove					() override;
	virtual CPhysicsShell*&			_BCL					ObjectPPhysicsShell					() override;
	virtual bool					_BCL					has_parent_object					() override;
	virtual IPHCapture*				_BCL					PHCapture							() override;
	virtual bool					_BCL					IsInventoryItem						() override;
	virtual bool					_BCL					IsActor								() override;
	virtual bool					_BCL					IsStalker							() override;
	virtual void					_BCL					HideAllWeapons						( bool v ) override;
	virtual void					_BCL					MovementCollisionEnable				( bool enable ) override;
	virtual CPHSoundPlayer*			_BCL					ObjectPhSoundPlayer					() override {return inherited::ph_sound_player();}
	virtual ICollisionDamageReceiver* _BCL				ObjectPhCollisionDamageReceiver		() override;
	virtual void					_BCL					BonceDamagerCallback				(float &damage_factor) override;
#ifdef	DEBUG
	virtual xr_string				_BCL					dump								(EDumpType type) const override;
#endif
};

#endif