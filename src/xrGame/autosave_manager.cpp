////////////////////////////////////////////////////////////////////////////
//	Module 		: autosave_manager.cpp
//	Created 	: 04.11.2004
//  Modified 	: 04.11.2004
//	Author		: Dmitriy Iassenev
//	Description : Autosave manager
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "autosave_manager.h"
#include "../xrEngine/date_time.h"
#include "ai_space.h"
#include "Level.h"
#include "xrMessages.h"
#include "UIGameCustom.h"
#include "Actor.h"
#include "MainMenu.h"
#include "../xrEngine/string_table.h"

#include "../xrEngine/XR_IOConsole.h"
extern void execute_console_command_deferred(CConsole* c, const char* string_to_execute);

extern const char* alife_section;

CAutosaveManager::CAutosaveManager			()
{
	save_str.printf("save %s - %s", Core.UserName, g_pStringTable->translate("autosave").c_str());

	u32							hours,minutes,seconds;
	const char*						section = alife_section;

	m_last_autosave_time		= Device.dwTimeGlobal;

	sscanf						(pSettings->r_string(section,"delay_autosave_interval"),"%d:%d:%d",&hours,&minutes,&seconds);
	m_delay_autosave_interval	= (u32)generate_time(1,1,1,hours,minutes,seconds);

	m_not_ready_count			= 0;

	shedule.t_min				= 5000;
	shedule.t_max				= 5000;
	shedule_register			();
}

CAutosaveManager::~CAutosaveManager			()
{
	shedule_unregister			();
}

float CAutosaveManager::shedule_Scale		()
{
	return						(.5f);
}

bool g_autosaves_enabled = true;
int g_autosaves_interval = 65;

void CAutosaveManager::shedule_Update		(u32 dt)
{
	PROF_EVENT("CAutosaveManager::shedule_Update");
	inherited::shedule_Update	(dt);

	if (!g_autosaves_enabled)
	{
		return;
	}

	if (!ai().get_alife())
	{
		return;
	}

	if (last_autosave_time() + (g_autosaves_interval * 60 * 1000) >= Device.dwTimeGlobal)
	{
		return;
	}

	if (Device.dwPrecacheFrame || !g_actor || !ready_for_autosave() || !Actor()->g_Alive()) {
		delay_autosave			();
		return;
	}
		
	update_autosave_time		();
	execute_console_command_deferred(Console, save_str.c_str());
}

void CAutosaveManager::on_game_loaded	()
{
	m_last_autosave_time		= Device.dwTimeGlobal;
}