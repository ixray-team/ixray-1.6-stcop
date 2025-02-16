////////////////////////////////////////////////////////////////////////////
//	Module 		: stalker_animation_manager_update.cpp
//	Created 	: 25.02.2003
//  Modified 	: 13.12.2006
//	Author		: Dmitriy Iassenev
//	Description : Stalker animation manager update cycle
////////////////////////////////////////////////////////////////////////////

#include "pch_script.h"
#include "stalker_animation_manager.h"
#include "ai/stalker/ai_stalker.h"
#include "game_object_space.h"
#include "../xrScripts/script_callback_ex.h"
#include "stalker_movement_manager.h"
#include "../Include/xrRender/animation_blend.h"

void CStalkerAnimationManager::play_delayed_callbacks	()
{
	if (!m_call_script_callback)
		return;

	m_call_script_callback	= false;
	object().callback		(GameObject::eScriptAnimation)	();

}

IC	bool CStalkerAnimationManager::script_callback			() const
{
	if (script_animations().empty())
		return				(false);
	
	return					(object().callback(GameObject::eScriptAnimation));
}

IC	bool CStalkerAnimationManager::need_update				() const
{
	if (script_callback())
		return				(true);

	return					(non_script_need_update());
}

IC	void CStalkerAnimationManager::update_tracks			()
{
	if (!need_update())
		return;

	m_skeleton_animated->UpdateTracks	();
}

#ifdef USE_HEAD_BONE_PART_FAKE
IC	void CStalkerAnimationManager::play_script_impl			()
{
	global().reset			();
	torso().reset			();
	legs().reset			();

	const CStalkerAnimationScript	&selected = assign_script_animation();
	script().animation		(selected.animation());
	script().play			(m_skeleton_animated,script_play_callback,&object(),selected.use_movement_controller(),false,m_script_bone_part_mask);

	head().animation		(assign_head_animation());
	head().play				(m_skeleton_animated,head_play_callback,&object(),false);
}
#else // USE_HEAD_BONE_PART_FAKE
IC	void CStalkerAnimationManager::play_script_impl			()
{
	global().reset			();
	head().reset			();
	torso().reset			();
	legs().reset			();

	script().animation		(assign_script_animation());
	script().play			(m_skeleton_animated,script_play_callback,&object(),false);
}
#endif // USE_HEAD_BONE_PART_FAKE

bool CStalkerAnimationManager::play_script					()
{
	if (script_animations().empty()) {
		script().reset		();
		return				(false);
	}

	play_script_impl		();

	return					(true);
}

#ifdef USE_HEAD_BONE_PART_FAKE
IC	void CStalkerAnimationManager::play_global_impl			(const MotionID &animation)
{
	torso().reset			();
	legs().reset			();

	global().animation		(animation);
	global().play			(m_skeleton_animated,global_play_callback,&object(),false,false,m_script_bone_part_mask);

	head().animation		(assign_head_animation());
	head().play				(m_skeleton_animated,head_play_callback,&object(),false);
}
#else // USE_HEAD_BONE_PART_FAKE
IC	void CStalkerAnimationManager::play_global_impl			(const MotionID &animation)
{
	head().reset			();
	torso().reset			();
	legs().reset			();

	global().animation		(animation);
	global().play			(m_skeleton_animated,global_play_callback,&object(),false);
}
#endif // USE_HEAD_BONE_PART_FAKE

bool CStalkerAnimationManager::play_global					()
{
	const MotionID			&global_animation = assign_global_animation();
	if (!global_animation) {
		global().reset		();
		return				(false);
	}

	play_global_impl		(global_animation);

	return					(true);
}

IC	void CStalkerAnimationManager::play_head				()
{
	head().animation		(assign_head_animation());
	head().play				(m_skeleton_animated,head_play_callback,&object(),false);
}

IC	void CStalkerAnimationManager::play_torso				()
{
	torso().animation		(assign_torso_animation());
	torso().play			(m_skeleton_animated,torso_play_callback,&object(),false);
}

void CStalkerAnimationManager::play_legs					()
{
	float					speed = 0.f;
	bool					first_time = !legs().animation();
	bool					result = legs().animation(assign_legs_animation());
	
	if (!first_time && !result && legs().blend()) {
		float				amount = legs().blend()->blendAmount;
		m_previous_speed	= (m_current_speed - m_previous_speed)*amount + m_previous_speed;
	}

	legs().play				(m_skeleton_animated,legs_play_callback,&object(),!fis_zero(m_current_speed),false);
	
	if (result && legs().blend()) {
		float				amount = legs().blend()->blendAmount;
		speed				= (m_current_speed - m_previous_speed)*amount + m_previous_speed;
	}

	if (fis_zero(speed))
		return;
	
	if (!legs().blend())
		return;

	object().movement().setup_speed_from_animation	(speed);
}

void CStalkerAnimationManager::update_impl					()
{
	if (!object().g_Alive())
		return;

	play_delayed_callbacks	();
	update_tracks			();

	if (play_script())
		return;

	if (play_global())
		return;

//	Msg("* %s %6d play head", *object().cName(), Device.dwTimeGlobal);
	play_head				();
//	Msg("* %s %6d play torso", *object().cName(), Device.dwTimeGlobal);
	play_torso				();
//	Msg("* %s %6d play legs", *object().cName(), Device.dwTimeGlobal);
	play_legs				();
//	Msg("* %s %6d sync", *object().cName(), Device.dwTimeGlobal);
	torso().synchronize		(m_skeleton_animated,m_legs);
}
#ifdef DEBUG
int anim_manager_exception_filter(CAI_Stalker* stk, u32 code, _EXCEPTION_POINTERS* ep)
{
	Msg("! error in stalker [%s]", stk->Name());
	Msg("anim_manager_exception_filter: code=[0x%x][%s]", code, Debug.exception_name(code));
	Debug.exception_stacktrace(ep);
	return EXCEPTION_EXECUTE_HANDLER;
}
#endif
void CStalkerAnimationManager::update						()
{
#ifndef DEBUG
/*	try */ {
		START_PROFILE		("stalker/client_update/animations")
		update_impl			();
		STOP_PROFILE
	}
/*	catch(...) {
		Msg					("! error in stalker [%s] with visual [%s]",*object().cName(),*object().cNameVisual());
		Msg					("! lua stacktrace:");
		ai().script_engine().last_called();
		Msg					("! Engine stacktrace:");
		Debug.log_stack_trace();
		throw;
	}*/
#else
	
	__try
	{
		update_impl			();
	}
	__except(anim_manager_exception_filter(m_object, GetExceptionCode(), GetExceptionInformation()))
	{
	}
	
#endif
}