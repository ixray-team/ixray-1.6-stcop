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

CScriptSound::CScriptSound				(const char* caSoundName, ESoundTypes sound_type)
{
	m_caSoundToPlay			= caSoundName;
	string_path				l_caFileName;
	VERIFY(::Sound)	;
	if (FS.exist(l_caFileName, _game_sounds_, caSoundName, ".ogg")) {
		m_sound.create(caSoundName, st_Effect, sound_type);
	} else {
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeMessage, "File not found \"%s\"!", l_caFileName);
		FS.update_path(l_caFileName, _game_sounds_, "$no_sound.ogg");
		m_sound.create("$no_sound.ogg", st_Effect, sound_type);
	}
}

CScriptSound::~CScriptSound		() noexcept(false)
{
	m_sound.destroy			();
}

Fvector CScriptSound::GetPosition() const
{
	CSound_params	l_tpSoundParams = m_sound.get_params();
	return			(l_tpSoundParams.position);
}

void CScriptSound::Play(CScriptGameObject* object, float delay, int flags)
{
	m_sound.play((object) ? &object->object() : nullptr, flags, delay);
}

void CScriptSound::PlayAtPos(CScriptGameObject* object, const Fvector& position, float delay, int flags)
{
	m_sound.play_at_pos((object) ? &object->object() : nullptr, position, flags, delay);
}

void CScriptSound::PlayNoFeedback(CScriptGameObject* object, u32 flags /*!< Looping */, float delay /*!< Delay */, Fvector pos, float vol)
{
	m_sound.play_no_feedback((object) ? &object->object() : nullptr, flags, delay, &pos, &vol);
}

static SoundShaderType FindScriptShader(const char* Name)
{
	const SoundShaderType Type = XRay::Sound::Shader::Find(Name);
	if (Type == SoundShaderType::None)
	{
		Msg("! [Sound] Unknown sound shader '%s'", Name ? Name : "");
	}
	return Type;
}

void CScriptSound::AddShader(const char* Shader)
{
	m_sound.add_shader(FindScriptShader(Shader));
}

void CScriptSound::RemoveShader(const char* Shader)
{
	m_sound.remove_shader(FindScriptShader(Shader));
}

void CScriptSound::ClearShaders()
{
	m_sound.clear_shaders();
}

void CScriptSound::SetShaderParam(const char* Shader, const char* Param, float Value)
{
	m_sound.set_shader_param(FindScriptShader(Shader), Param, Value);
}

void CScriptSound::RampShaderParam(const char* Shader, const char* Param, float Value, float Seconds, const char* Curve)
{
	m_sound.ramp_shader_param(FindScriptShader(Shader), Param, Value, Seconds, XRay::Sound::Shader::FindCurve(Curve));
}

void CScriptSound::SetShaderLfo(const char* Shader, const char* Param, float Rate, float Depth, const char* Shape)
{
	m_sound.set_shader_lfo(FindScriptShader(Shader), Param, Rate, Depth, XRay::Sound::Shader::FindLfoShape(Shape));
}

void CScriptSound::StopShaderAutomation(const char* Shader, const char* Param)
{
	m_sound.stop_shader_automation(FindScriptShader(Shader), Param);
}
