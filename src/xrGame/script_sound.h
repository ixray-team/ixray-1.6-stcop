////////////////////////////////////////////////////////////////////////////
//	Module 		: script_sound.h
//	Created 	: 06.02.2004
//  Modified 	: 06.02.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script sound class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrScripts/script_export_space.h"
#include "../xrSound/ai_sounds.h"

class CScriptGameObject;

class CScriptSound {
	mutable ref_sound			m_sound;
	shared_str					m_caSoundToPlay;

	friend class CScriptSoundAction;
public:

								CScriptSound		(const char* caSoundName, ESoundTypes sound_type = SOUND_TYPE_NO_SOUND);
	virtual						~CScriptSound		() noexcept(false);
	IC		u32					Length				();
	IC		void				Play				(CScriptGameObject *object);
	IC		void				Play				(CScriptGameObject *object, float delay);
			void				Play				(CScriptGameObject *object, float delay, int flags);
	IC		void				PlayAtPos			(CScriptGameObject *object, const Fvector &position);
	IC		void				PlayAtPos			(CScriptGameObject *object, const Fvector &position, float delay);
			void				PlayAtPos			(CScriptGameObject *object, const Fvector &position, float delay, int flags);
			void				PlayNoFeedback		(CScriptGameObject *object,	u32 flags/*!< Looping */, float delay/*!< Delay */, Fvector pos, float vol);
	IC		void				AttachTail			(const char* caSoundName);
	IC		void				Stop				();
	IC		void				StopDeffered		();
	IC		void				SetPosition			(const Fvector &position);
	IC		void				SetPanning			(double left, double right);
	IC		void				SetFrequency		(float frequency);
	IC		void				SetVolume			(float volume);
	IC		CSound_params	    GetParams			();
	IC		void				SetParams			(CSound_params *sound_params);
			void				SetMinDistance		(const float fMinDistance);
	IC		void				SetMaxDistance		(const float fMaxDistance);
			Fvector				GetPosition			() const;
	IC		const float			GetFrequency		() const;
	IC		const float			GetMinDistance		() const;
	IC		const float			GetMaxDistance		() const;
	IC		const float			GetVolume			() const;
	IC		bool				IsPlaying			() const;

			// Sound shaders by name ("radio", "low_pass", ...)
			void				AddShader			(const char* shader);
			void				RemoveShader		(const char* shader);
			void				ClearShaders		();
			void				SetShaderParam		(const char* shader, const char* param, float value);
			// Automation: curve "linear" / "exp" / "smooth", shape "sine" / "triangle" / "square" / "saw_up" / "saw_down" / "random"
			void				RampShaderParam		(const char* shader, const char* param, float value, float seconds, const char* curve);
	IC		void				RampShaderParamLinear(const char* shader, const char* param, float value, float seconds)	{ RampShaderParam(shader, param, value, seconds, nullptr); }
			void				SetShaderLfo		(const char* shader, const char* param, float rate, float depth, const char* shape);
	IC		void				SetShaderLfoSine	(const char* shader, const char* param, float rate, float depth)	{ SetShaderLfo(shader, param, rate, depth, nullptr); }
			void				StopShaderAutomation(const char* shader, const char* param);
	IC		void				StopShaderAutomationAll(const char* shader)						{ StopShaderAutomation(shader, nullptr); }
	IC		bool				GetIgnoreBusShaders	() const						{ return m_sound.ignores_bus_shaders(); }
	IC		void				SetIgnoreBusShaders	(bool value)					{ m_sound.set_ignore_bus_shaders(value); }
	// Bus the sound is mixed into instead of its own ("" restores), kept across plays
	IC		const char*			GetRouteBus			() const						{ return m_sound.route_bus(); }
	IC		void				SetRouteBus			(const char* bus)				{ m_sound.set_route_bus(bus); }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};

#include "script_sound_inline.h"
