#pragma once
#include "alife_space.h"
#include "../../../xrPhysics/IPhysicsShellHolder.h"

class ECORE_API CPhysicsShellHolderEditorBase:
	public IPhysicsShellHolder
{
public:
	void								CreatePhysicsShell	( Fmatrix*	obj_xform );
    void								DeletePhysicsShell	();
    void								UpdateObjectXform	( Fmatrix &obj_xform );
    void								ApplyDragForce		( const Fvector &force );

protected:
	CPhysicsShellHolderEditorBase(): m_physics_shell(nullptr),m_object_xform(Fidentity){}
	~CPhysicsShellHolderEditorBase()	{ /*DeletePhysicsShell	();*/ }

protected:
	CPhysicsShell*						m_physics_shell;
    Fmatrix								m_object_xform;
private:
  	virtual	const char*						_BCL	ObjectName							()		const	 { return "EditorActor"; }
	virtual	const char*						_BCL	ObjectNameVisual					()		const	 { return "unknown"; }
	virtual	const char*						_BCL	ObjectNameSect						()		const	 { return "unknown"; }
	virtual	bool						_BCL	ObjectGetDestroy					()		const	 { return false; };
	virtual ICollisionHitCallback*		_BCL	ObjectGetCollisionHitCallback		()				 { return nullptr;}
	virtual	ALife::_OBJECT_ID			_BCL	ObjectID							()		const	 { return ALife::INVALID_OBJECT_ID;}
	virtual	ICollisionForm*				_BCL	ObjectCollisionModel				()				 { VERIFY(false);return nullptr; }
	virtual IDamageSource*				_BCL	ObjectCastIDamageSource				()				 { return nullptr; }
	virtual	void						_BCL	ObjectProcessingDeactivate			()				 {;}
	virtual	void						_BCL	ObjectProcessingActivate			()				 {}
	virtual	void						_BCL	ObjectSpatialMove					()				 {}
    virtual	CPhysicsShell*&				_BCL	ObjectPPhysicsShell					()				 { return m_physics_shell; }
	virtual	void						_BCL	enable_notificate					()				 {}
	virtual bool						_BCL	has_parent_object					()				 { return false; }
	virtual	IPHCapture*					_BCL	PHCapture							()				 { return nullptr;}
	virtual	bool						_BCL	IsInventoryItem						()				 { return false; }
	virtual	bool						_BCL	IsActor								()				 { return false; }
 	virtual bool						_BCL	IsStalker							()				 { return false; }
	virtual	void						_BCL	HideAllWeapons						( bool v )		 {}//(SetWeaponHideState(INV_STATE_BLOCK_ALL,true))
	virtual	void						_BCL	MovementCollisionEnable				( bool enable )	 {}
	virtual CPHSoundPlayer*				_BCL	ObjectPhSoundPlayer				()  			 	 { return nullptr; }
	virtual	ICollisionDamageReceiver* 	_BCL	ObjectPhCollisionDamageReceiver	()				 	 { return nullptr; }
	virtual	void					 	_BCL	BonceDamagerCallback			( float &damage_factor ){}
public:
    virtual	Fmatrix&					_BCL	ObjectXFORM				()														{  return m_object_xform;}
private:
    virtual	Fvector&					_BCL	ObjectPosition			()														{  return m_object_xform.c;}

#ifdef	DEBUG
	virtual	xr_string					_BCL	dump							( EDumpType type )const  { VERIFY(false); return xr_string("ActorEditor!");}
#endif
};