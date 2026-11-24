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

static void RampBusShaderParamCurve(const char* Bus, const char* Shader, const char* Param, float Value, float Seconds, const char* Curve)
{
	XRay::Sound::Mixer::RampBusShaderParam(FindScriptBus(Bus), FindMixerShader(Shader), Param, Value, Seconds, XRay::Sound::Shader::FindCurve(Curve));
}

static void RampBusShaderParam(const char* Bus, const char* Shader, const char* Param, float Value, float Seconds)
{
	RampBusShaderParamCurve(Bus, Shader, Param, Value, Seconds, nullptr);
}

static void SetBusShaderLfoShape(const char* Bus, const char* Shader, const char* Param, float Rate, float Depth, const char* Shape)
{
	XRay::Sound::Mixer::SetBusShaderLfo(FindScriptBus(Bus), FindMixerShader(Shader), Param, Rate, Depth, XRay::Sound::Shader::FindLfoShape(Shape));
}

static void SetBusShaderLfo(const char* Bus, const char* Shader, const char* Param, float Rate, float Depth)
{
	SetBusShaderLfoShape(Bus, Shader, Param, Rate, Depth, nullptr);
}

static void StopBusShaderAutomationParam(const char* Bus, const char* Shader, const char* Param)
{
	XRay::Sound::Mixer::StopBusShaderAutomation(FindScriptBus(Bus), FindMixerShader(Shader), Param);
}

static void StopBusShaderAutomation(const char* Bus, const char* Shader)
{
	StopBusShaderAutomationParam(Bus, Shader, nullptr);
}

static u32 FindScriptOutputBus(const char* Bus)
{
	return (Bus != nullptr && Bus[0] != 0) ? FindScriptBus(Bus) : 0;
}

// Returns the bus name, "" on failure
static const char* AcquireBus(const char* Name)
{
	return XRay::Sound::Mixer::GetBusName(XRay::Sound::Mixer::AcquireDynamicBus(Name));
}

static const char* AcquireBusOutput(const char* Name, const char* Output)
{
	return XRay::Sound::Mixer::GetBusName(XRay::Sound::Mixer::AcquireDynamicBus(Name, FindScriptOutputBus(Output)));
}

static const char* AcquireShaderBus(const char* Shader)
{
	return XRay::Sound::Mixer::GetBusName(XRay::Sound::Mixer::AcquireShaderBus(FindMixerShader(Shader)));
}

static const char* AcquireShaderBusOutput(const char* Shader, const char* Output)
{
	return XRay::Sound::Mixer::GetBusName(XRay::Sound::Mixer::AcquireShaderBus(FindMixerShader(Shader), FindScriptOutputBus(Output)));
}

static void ReleaseBus(const char* Bus)
{
	XRay::Sound::Mixer::ReleaseDynamicBus(FindScriptBus(Bus));
}

// Target "" restores the config output
static void RouteBus(const char* Bus, const char* Target)
{
	XRay::Sound::Mixer::RouteBus(FindScriptBus(Bus), FindScriptOutputBus(Target));
}

static void UnrouteBus(const char* Bus)
{
	XRay::Sound::Mixer::RouteBus(FindScriptBus(Bus), 0);
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
			.property("route_bus",				&CScriptSound::GetRouteBus,			&CScriptSound::SetRouteBus)
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
			.def("ramp_shader_param",			&CScriptSound::RampShaderParamLinear)
			.def("ramp_shader_param",			&CScriptSound::RampShaderParam)
			.def("set_shader_lfo",				&CScriptSound::SetShaderLfoSine)
			.def("set_shader_lfo",				&CScriptSound::SetShaderLfo)
			.def("stop_shader_automation",		&CScriptSound::StopShaderAutomationAll)
			.def("stop_shader_automation",		&CScriptSound::StopShaderAutomation)
	];

	module(L, "sound_mixer")
	[
		def("add_bus_shader",					&AddBusShader),
		def("add_bus_shader",					&AddBusShaderSpatial),
		def("remove_bus_shader",				&RemoveBusShader),
		def("remove_bus_shader",				&RemoveBusShaderSpatial),
		def("clear_bus_shaders",				&ClearBusShaders),
		def("set_bus_shader_param",				&SetBusShaderParam),
		def("ramp_bus_shader_param",			&RampBusShaderParam),
		def("ramp_bus_shader_param",			&RampBusShaderParamCurve),
		def("set_bus_shader_lfo",				&SetBusShaderLfo),
		def("set_bus_shader_lfo",				&SetBusShaderLfoShape),
		def("stop_bus_shader_automation",		&StopBusShaderAutomation),
		def("stop_bus_shader_automation",		&StopBusShaderAutomationParam),
		def("acquire_bus",						&AcquireBus),
		def("acquire_bus",						&AcquireBusOutput),
		def("acquire_shader_bus",				&AcquireShaderBus),
		def("acquire_shader_bus",				&AcquireShaderBusOutput),
		def("release_bus",						&ReleaseBus),
		def("route_bus",						&RouteBus),
		def("unroute_bus",						&UnrouteBus)
	];
}
