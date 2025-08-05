//----------------------------------------------------
// file: TempObject.cpp
//----------------------------------------------------
#include "stdafx.h"


#include "PS_instance.h"
#include "IGame_Persistent.h"

CPS_Instance::CPS_Instance(bool destroy_on_game_load) :
	m_destroy_on_game_load(destroy_on_game_load)
{
	renderable.pROS_Allowed = FALSE;

	m_iLifeTime = int_max;
	m_bAutoRemove = TRUE;
	m_bDead = FALSE;
}
extern ENGINE_API xr_atomic_bool			g_bRendering; 

//----------------------------------------------------
CPS_Instance::~CPS_Instance()
{
	VERIFY(!g_bRendering);
	ISpatialOwner::spatial_unregister();
}

//----------------------------------------------------
void CPS_Instance::shedule_Update	(u32 dt)
{
	if (renderable.pROS)			::Render->ros_destroy	(renderable.pROS);	//. particles doesn't need ROS

	//ISheduled::shedule_Update		(dt);
	m_iLifeTime						-= dt;

	// remove???
	if (m_bDead)					return;
	if (m_bAutoRemove && m_iLifeTime<=0)
		PSI_destroy					();
}
//----------------------------------------------------
void CPS_Instance::PSI_destroy()
{
	m_bDead = TRUE;
	m_iLifeTime = 0;
	m_NeedDestroy = true;
	//g_pGamePersistent->ps_destroy.push_back	(this);
}

//----------------------------------------------------
void CPS_Instance::PSI_internal_delete		()
{
	VERIFY(!"Õ≈ «¿’Œƒ»“‹");
	CPS_Instance*	self = this;
	xr_delete		(self);
}
