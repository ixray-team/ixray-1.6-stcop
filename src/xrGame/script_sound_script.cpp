////////////////////////////////////////////////////////////////////////////
//	Module 		: script_sound_script.cpp
//	Created 	: 06.02.2004
//  Modified 	: 06.02.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script sound class script export
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "script_sound.h"
#include "script_game_object.h"

using namespace luabind;

static u32 FindScriptBus(const char* Bus)
{
	const u32 BusIdx = XRay::Sound::Mixer::FindBus(Bus);
	if (BusIdx == 0)
	{
		Msg("! [Sound] Unknown sound bus '%s'", Bus ? Bus : "");
	}
	return BusIdx;
}

static SoundShaderType FindMixerShader(const char* Shader)
{
	const SoundShaderType Type = XRay::Sound::Shader::Find(Shader);
	if (Type == SoundShaderType::None)
	{
		Msg("! [Sound] Unknown sound shader '%s'", Shader ? Shader : "");
	}
	return Type;
}

static void AddBusShader(const char* Bus, const char* Shader)
{
	XRay::Sound::Mixer::AddBusShader(FindScriptBus(Bus), FindMixerShader(Shader));
}

// SpatialOnly: 2D sounds (UI, HUD, ...) skip the shaders of this bus
static void AddBusShaderSpatial(const char* Bus, const char* Shader, bool SpatialOnly)
{
	XRay::Sound::Mixer::AddBusShader(FindScriptBus(Bus), FindMixerShader(Shader), SpatialOnly);
}

static void RemoveBusShaderSpatial(const char* Bus, const char* Shader, bool SpatialOnly)
{
	XRay::Sound::Mixer::RemoveBusShader(FindScriptBus(Bus), FindMixerShader(Shader), SpatialOnly);
}

static void RemoveBusShader(const char* Bus, const char* Shader)
{
	XRay::Sound::Mixer::RemoveBusShader(FindScriptBus(Bus), FindMixerShader(Shader));
}

static void ClearBusShaders(const char* Bus)
{
	XRay::Sound::Mixer::ClearBusShaders(FindScriptBus(Bus));
}

static void SetBusShaderParam(const char* Bus, const char* Shader, const char* Param, float Value)
{
	XRay::Sound::Mixer::SetBusShaderParam(FindScriptBus(Bus), FindMixerShader(Shader), Param, Value);
}

#pragma optimize("s",on)
void CScriptSound::script_register(lua_State *L)
{
	module(L)
	[
		class_<CSound_params>("sound_params")
			.def_readwrite("position",			&CSound_params::position)
			.def_readwrite("volume",			&CSound_params::volume)
			.def_readwrite("frequency",			&CSound_params::freq)
			.def_readwrite("min_distance",		&CSound_params::min_distance)
			.def_readwrite("max_distance",		&CSound_params::max_distance),
			
		class_<CScriptSound>("sound_object")
			.enum_("sound_play_type")
			[
				value("looped",					sm_Looped),
				value("s2d",					sm_2D),
				value("s3d",					0)
			]
			.property("frequency",				&CScriptSound::GetFrequency,	&CScriptSound::SetFrequency)
			.property("min_distance",			&CScriptSound::GetMinDistance,	&CScriptSound::SetMinDistance)
			.property("max_distance",			&CScriptSound::GetMaxDistance,	&CScriptSound::SetMaxDistance)
			.property("volume",					&CScriptSound::GetVolume,		&CScriptSound::SetVolume)
			.property("ignore_bus_shaders",		&CScriptSound::GetIgnoreBusShaders,	&CScriptSound::SetIgnoreBusShaders)
			.def(								constructor<const char*>())
			.def(								constructor<const char*,ESoundTypes>())
			.def("get_position",				&CScriptSound::GetPosition)
			.def("set_position",				&CScriptSound::SetPosition)
			.def("play",						(void (CScriptSound::*)(CScriptGameObject*))(&CScriptSound::Play))
			.def("play",						(void (CScriptSound::*)(CScriptGameObject*,float))(&CScriptSound::Play))
			.def("play",						(void (CScriptSound::*)(CScriptGameObject*,float,int))(&CScriptSound::Play))
			.def("play_at_pos",					(void (CScriptSound::*)(CScriptGameObject*,const Fvector &))(&CScriptSound::PlayAtPos))
			.def("play_at_pos",					(void (CScriptSound::*)(CScriptGameObject*,const Fvector &,float))(&CScriptSound::PlayAtPos))
			.def("play_at_pos",					(void (CScriptSound::*)(CScriptGameObject*,const Fvector &,float, int))(&CScriptSound::PlayAtPos))
			.def("set_panning",					&CScriptSound::SetPanning)
			.def("play_no_feedback",			&CScriptSound::PlayNoFeedback)
			.def("stop",						&CScriptSound::Stop)
			.def("stop_deffered",				&CScriptSound::StopDeffered)
			.def("playing",						&CScriptSound::IsPlaying)
			.def("length",						&CScriptSound::Length)
			.def("attach_tail",					&CScriptSound::AttachTail)
			.def("add_shader",					&CScriptSound::AddShader)
			.def("remove_shader",				&CScriptSound::RemoveShader)
			.def("clear_shaders",				&CScriptSound::ClearShaders)
			.def("set_shader_param",			&CScriptSound::SetShaderParam)
	];

	module(L, "sound_mixer")
	[
		def("add_bus_shader",					&AddBusShader),
		def("add_bus_shader",					&AddBusShaderSpatial),
		def("remove_bus_shader",				&RemoveBusShader),
		def("remove_bus_shader",				&RemoveBusShaderSpatial),
		def("clear_bus_shaders",				&ClearBusShaders),
		def("set_bus_shader_param",				&SetBusShaderParam)
	];
}
