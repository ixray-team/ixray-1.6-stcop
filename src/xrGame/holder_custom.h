#pragma once

//#include "gameobject.h"

#include "../xrScripts/script_export_space.h"

class CInventory;
class CGameObject;
class CCameraBase;
class CActor;

class CHolderCustom
{
private:
	CGameObject*			m_owner;
	CActor*					m_ownerActor;
protected:
	CGameObject*			Owner				(){return m_owner;}
	CActor*					OwnerActor			(){return m_ownerActor;}
public:
	bool					m_bEnterLocked;
	bool					m_bExitLocked;
							CHolderCustom()		{ m_owner = nullptr; m_ownerActor = nullptr; m_bEnterLocked = false; m_bExitLocked = false; }
	virtual					~CHolderCustom		()				{;}
	virtual	void			UpdateEx			(float fov){}; //called by owner
	virtual CHolderCustom	*cast_holder_custom	()				{return this;}
			bool			Engaged				()				{return m_owner!= nullptr;}
	virtual void			OnMouseMove			(int x, int y)	= 0;
	virtual void			OnKeyboardPress		(int dik)		= 0;
	virtual void			OnKeyboardRelease	(int dik)		= 0;
	virtual void			OnKeyboardHold		(int dik)		= 0;
	// Inventory for the car
	virtual CInventory*		GetInventory		()				= 0;

	virtual void			cam_Update			(float dt, float fov=90.0f)		= 0;

	virtual bool			EnterLocked			()				{return m_bEnterLocked;}
	virtual bool			ExitLocked			()				{return m_bExitLocked;}
	virtual void			SetEnterLocked		(bool v)		{m_bEnterLocked = v;}
	virtual void			SetExitLocked		(bool v)		{m_bExitLocked = v;}
	virtual bool			Use					(const Fvector& pos,const Fvector& dir,const Fvector& foot_pos)=0;
	virtual bool			attach_Actor		(CGameObject* actor);
	virtual void			detach_Actor		();
	virtual bool			allowWeapon			()	const		= 0;
	virtual bool			HUDView				() const		= 0;
	virtual Fvector			ExitPosition		()				= 0;
	virtual Fvector			ExitVelocity		()				{return Fvector().set(0,0,0);}
	virtual CCameraBase*	Camera				()				= 0;
	virtual void			Action				(u16 id, u32 flags)				{};
	virtual void			SetParam			(int id, Fvector2 val)			{};
	virtual void			SetParam			(int id, Fvector val)			{};

	shared_str m_sUseAction;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};