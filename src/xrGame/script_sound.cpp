////////////////////////////////////////////////////////////////////////////
//	Module 		: script_sound.cpp
//	Created 	: 06.02.2004
//  Modified 	: 06.02.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script sound class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "script_sound.h"
#include "script_game_object.h"
#include "GameObject.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"

CScriptSound::CScriptSound				(LPCSTR caSoundName, ESoundTypes sound_type)
{
	m_caSoundToPlay			= caSoundName;
	string_path				l_caFileName;
	VERIFY(::Sound)	;
	if (FS.exist(l_caFileName, "$game_sounds$", caSoundName, ".ogg")) {
		m_sound.create(caSoundName, st_Effect, sound_type);
	} else {
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeMessage, "File not found \"%s\"!", l_caFileName);
		FS.update_path(l_caFileName, "$game_sounds$", "$no_sound.ogg");
		m_sound.create("$no_sound.ogg", st_Effect, sound_type);
	}
}

CScriptSound::~CScriptSound		() noexcept(false)
{
	m_sound.destroy			();
}

Fvector CScriptSound::GetPosition() const
{
	VERIFY(m_sound._handle());
	CSound_params l_tpSoundParams = m_sound.get_params();
	return			(l_tpSoundParams.position);

}

void CScriptSound::Play			(CScriptGameObject *object, float delay, int flags)
{
//	Msg							("%6d : CScriptSound::Play (%s), delay %f, flags %d",Device.dwTimeGlobal,m_sound._handle()->file_name(),delay,flags);
	m_sound.play				((object) ? &object->object() : nullptr, flags, delay);
}

void CScriptSound::PlayAtPos		(CScriptGameObject *object, const Fvector &position, float delay, int flags)
{
//	Msg							("%6d : CScriptSound::Play (%s), delay %f, flags %d",m_sound._handle()->file_name(),delay,flags);
	m_sound.play_at_pos			((object) ? &object->object() : nullptr, position,flags,delay);
}

void CScriptSound::PlayNoFeedback	(CScriptGameObject *object,	u32 flags/*!< Looping */, float delay/*!< Delay */, Fvector pos, float vol)
{
	m_sound.play_no_feedback	((object) ? &object->object() : nullptr, flags,delay,&pos,&vol);
}
