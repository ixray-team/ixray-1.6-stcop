#include "StdAfx.h"
#ifdef DEBUG
#include "ode_include.h"
#include "../xrEngine/StatGraph.h"
#include "PHDebug.h"
#endif
#include "alife_space.h"
#include "Hit.h"
#include "PHDestroyable.h"
#include "Car.h"
#include "../Include/xrRender/Kinematics.h"
#include "PHWorld.h"
#include "../xrEngine/CameraBase.h"
#include "../xrEngine/GameMtlLib.h"
#include "../xrSound/New/SoundMixerInternal.h"

constexpr float CarDeferredStopFade = 0.1f;
constexpr float MaxEngineStartDelayMs = 10000.f;

CCar::SCarSound::SCarSound(CCar* car)
{
	volume = 1.f;
	engine_start_delay = 0;
	time_state_start = 0;
	eCarSound = sndOff;
	pcar = car;
	relative_pos.set(0.f, 0.5f, -1.f);
	relative_engine_pos.set(0.f, 0.5f, -1.f);
	//relative_engine_exhaust_pos.set(0.f, 0.5f, -1.f);
}

CCar::SCarSound::~SCarSound()
{
	
}
void CCar::SCarSound::Init()
{
	IKinematics* Kinematics = pcar->Visual() ? smart_cast<IKinematics*>(pcar->Visual()) : nullptr;
	if (Kinematics == nullptr)
	{
		eCarSound = sndOff;
		return;
	}

	CInifile* ini = Kinematics->LL_UserData();
	if (ini && ini->section_exist("car_sound"))
	{
		if (ini->line_exist("car_sound", "snd_volume"))
		{
			volume = ini->r_float("car_sound", "snd_volume");
			if (!std::isfinite(volume))
				volume = 1.f;
			clamp(volume, 0.f, 1.f);
		}

		if (!snd_engine._p && ini->line_exist("car_sound", "snd_name"))
		{
			snd_engine.create(ini->r_string("car_sound", "snd_name"), st_Effect, sg_SourceType);
			pcar->m_sound = true;
		}

		if (!snd_engine_start._p && ini->line_exist("car_sound", "engine_start"))
			snd_engine_start.create(ini->r_string("car_sound", "engine_start"), st_Effect, sg_SourceType);

		if (!snd_engine_stop._p && ini->line_exist("car_sound", "engine_stop"))
			snd_engine_stop.create(ini->r_string("car_sound", "engine_stop"), st_Effect, sg_SourceType);

		const float fengine_start_delay = ini->read_if_exists<float>("car_sound", "engine_sound_start_dellay", 0.25f);
		float engine_start_length = 0.f;
		if (snd_engine_start._p)
		{
			engine_start_length = snd_engine_start.get_length_sec();
			if (!(engine_start_length > 0.f) && snd_engine_start._p->fn_attached[0].size())
			{
				engine_start_length = XRay::Sound::Mixer::GetDuration(snd_engine_start._p->fn_attached[0].c_str());
			}
		}

		float StartDelay = (snd_engine_start._p ? engine_start_length * 1000.f : 1.f) * fengine_start_delay;
		if (!std::isfinite(StartDelay) || StartDelay < 0.f)
			StartDelay = 0.f;

		// A stale or corrupted sound duration must not keep the engine in the
		// starter state forever.
		engine_start_delay = iFloor(std::min(StartDelay, MaxEngineStartDelayMs));

		if (ini->line_exist("car_sound", "relative_pos"))
			relative_pos.set(ini->r_fvector3("car_sound", "relative_pos"));

		if (ini->line_exist("car_sound", "relative_engine_pos"))
			relative_engine_pos.set(ini->r_fvector3("car_sound", "relative_engine_pos"));
		else
			relative_engine_pos.set(ini->r_fvector3("car_sound", "relative_pos"));

		//if (ini->line_exist("car_sound", "relative_engine_exhaust_pos"))
		//	relative_engine_exhaust_pos.set(ini->r_fvector3("car_sound", "relative_engine_exhaust_pos"));
		//else
		//	relative_engine_exhaust_pos.set(ini->r_fvector3("car_sound", "relative_pos"));

		if (!snd_transmission._p && ini->line_exist("car_sound", "transmission_switch"))
		{
			snd_transmission.create(ini->r_string("car_sound", "transmission_switch"), st_Effect, sg_SourceType);
			// Not muffled in the cabin
			snd_transmission.set_ignore_bus_shaders(true);
		}

		if (!snd_gas._p && ini->line_exist("car_sound", "gas_sound"))
		{
			snd_gas.create(ini->r_string("car_sound", "gas_sound"), st_Effect, sg_SourceType);
			pcar->m_sound = true;
		}

		if (!snd_brake_release._p && ini->line_exist("car_sound", "brake_release_sound"))
			snd_brake_release.create(ini->r_string("car_sound", "brake_release_sound"), st_Effect, sg_SourceType);

		if (!snd_valve._p && ini->line_exist("car_sound", "valve_sound"))
			snd_valve.create(ini->r_string("car_sound", "valve_sound"), st_Effect, sg_SourceType);

		if (!snd_turbo._p && ini->line_exist("car_sound", "turbo_sound"))
		{
			snd_turbo.create(ini->r_string("car_sound", "turbo_sound"), st_Effect, sg_SourceType);
			pcar->m_sound = true;
		}

		if (!snd_limiter._p && ini->line_exist("car_sound", "limiter_sound"))
		{
			snd_limiter.create(ini->r_string("car_sound", "limiter_sound"), st_Effect, sg_SourceType);
			pcar->m_sound = true;
		}

		if (!snd_idle._p && ini->line_exist("car_sound", "idle_sound"))
		{
			snd_idle.create(ini->r_string("car_sound", "idle_sound"), st_Effect, sg_SourceType);
			pcar->m_sound = true;
		}

		if (!snd_engine_decel._p && ini->line_exist("car_sound", "decel_sound"))
		{
			snd_engine_decel.create(ini->r_string("car_sound", "decel_sound"), st_Effect, sg_SourceType);
			pcar->m_sound = true;
		}

		if (!snd_engine_heavy._p && ini->line_exist("car_sound", "heavy_sound"))
		{
			snd_engine_heavy.create(ini->r_string("car_sound", "heavy_sound"), st_Effect, sg_SourceType);
			pcar->m_sound = true;
		}

		if (!snd_blower._p && ini->line_exist("car_sound", "blower_sound"))
		{
			snd_blower.create(ini->r_string("car_sound", "blower_sound"), st_Effect, sg_SourceType);
		pcar->m_sound = true;
		}

		if (pcar->m_nitro)
		{
			if (!snd_nitro._p && ini->line_exist("car_sound", "nitro_sound"))
				snd_nitro.create(ini->r_string("car_sound", "nitro_sound"), st_Effect, sg_SourceType);

			if (!snd_nitro_stop._p && ini->line_exist("car_sound", "nitro_stop"))
				snd_nitro_stop.create(ini->r_string("car_sound", "nitro_stop"), st_Effect, sg_SourceType);

			if (!snd_nitro_start._p && ini->line_exist("car_sound", "nitro_start"))
				snd_nitro_start.create(ini->r_string("car_sound", "nitro_start"), st_Effect, sg_SourceType);

			pcar->m_sound = true;
		}

		if (!snd_whine._p && ini->line_exist("car_sound", "whine_sound"))
		{
			snd_whine.create(ini->r_string("car_sound", "whine_sound"), st_Effect, sg_SourceType);
			snd_whine.set_ignore_bus_shaders(true);
		}

		if (!snd_horn._p && ini->line_exist("car_sound", "horn_sound"))
			snd_horn.create(ini->r_string("car_sound", "horn_sound"), st_Effect, sg_SourceType);

		if (!snd_pop._p && ini->line_exist("car_sound", "pop_sound"))
			snd_pop.create(ini->r_string("car_sound", "pop_sound"), st_Effect, sg_SourceType);

		if (!snd_fire._p && ini->line_exist("car_sound", "fire_sound"))
			snd_fire.create(ini->r_string("car_sound", "fire_sound"), st_Effect, sg_SourceType);

		//if (!snd_engine_exhaust._p && ini->line_exist("car_sound", "snd_name_exhaust"))
		//{
		//	snd_engine_exhaust.create(ini->r_string("car_sound", "snd_name_exhaust"), st_Effect, sg_SourceType);
		//	pcar->m_exhaust_sound = true;
		//}
		//
		//if (!snd_limiter_exhaust._p && ini->line_exist("car_sound", "limiter_sound_exhaust"))
		//{
		//	snd_limiter_exhaust.create(ini->r_string("car_sound", "limiter_sound_exhaust"), st_Effect, sg_SourceType);
		//	pcar->m_exhaust_sound = true;
		//}
		//
		//if (!snd_idle_exhaust._p && ini->line_exist("car_sound", "idle_sound_exhaust"))
		//{
		//	snd_idle_exhaust.create(ini->r_string("car_sound", "idle_sound_exhaust"), st_Effect, sg_SourceType);
		//	pcar->m_exhaust_sound = true;
		//}
		//
		//if (!snd_engine_decel_exhaust._p && ini->line_exist("car_sound", "decel_sound_exhaust"))
		//{
		//	snd_engine_decel_exhaust.create(ini->r_string("car_sound", "decel_sound_exhaust"), st_Effect, sg_SourceType);
		//	pcar->m_exhaust_sound = true;
		//}

	}
	//else
	//{
	//	Msg("! Car doesn't contain sound params");
	//}
	eCarSound = sndOff;
	if (pcar->b_engine_on)
		Drive();
}
void CCar::SCarSound::SetSoundPosition(ref_sound &snd)
{
	VERIFY(!physics_world()->Processing());
	if (snd._p && snd.is_playing())
	{
		Fvector pos;
		pcar->XFORM().transform_tiny(pos,relative_pos);
		snd.set_position		(pos);
	}
}

void CCar::SCarSound::SetEngineSoundPosition(ref_sound& snd)
{
	VERIFY(!physics_world()->Processing());
	if (snd._p && snd.is_playing())
	{
		Fvector pos;
		pcar->XFORM().transform_tiny(pos, relative_engine_pos);
		snd.set_position(pos);
	}
}

//void CCar::SCarSound::SetEngineExhaustSoundPosition(ref_sound& snd)
//{
//	VERIFY(!physics_world()->Processing());
//	if (snd._p && snd.is_playing())
//	{
//		Fvector pos;
//		pcar->XFORM().transform_tiny(pos, relative_engine_exhaust_pos);
//		snd.set_position(pos);
//	}
//}

void CCar::SCarSound::UpdateStarting()
{	
	VERIFY(!physics_world()->Processing());
	SetEngineSoundPosition(snd_engine_start);
	if (snd_engine_stop._p && snd_engine_stop.is_playing())
		snd_engine_stop.stop_deffered(CarDeferredStopFade);

	if (!pcar->b_engine_on)
	{
		// A failed start (including an attempt without fuel) plays the stop
		// sound right after engine_sound_start_dellay, without waiting for the
		// rest of the starter clip.
		const bool AttemptTimeElapsed = time_state_start + engine_start_delay < Device.dwTimeGlobal;
		if (AttemptTimeElapsed)
		{
			pcar->b_starting = false;
			pcar->StopExhausts();
			snd_engine_start.stop_deffered(CarDeferredStopFade);
			Stop();
		}
		return;
	}

	if (time_state_start + engine_start_delay < Device.dwTimeGlobal)
	{
		if (snd_engine._p)
		{
			if (!snd_engine.is_playing())
				snd_engine.play(pcar, sm_Looped | sm_NoOCC);
		}
		//turbo
		if (snd_turbo._p)
		{
			if (!snd_turbo.is_playing())
				snd_turbo.play(pcar, sm_Looped | sm_NoOCC);
		}
		//idle
		if (snd_idle._p)
		{
			if (!snd_idle.is_playing())
				snd_idle.play(pcar, sm_Looped | sm_NoOCC);
		}
		//decel
		if (snd_engine_decel._p)
		{
			if (!snd_engine_decel.is_playing())
				snd_engine_decel.play(pcar, sm_Looped | sm_NoOCC);
		}
		//heavy
		if (snd_engine_heavy._p)
		{
			if (!snd_engine_heavy.is_playing())
				snd_engine_heavy.play(pcar, sm_Looped | sm_NoOCC);
		}
		//low
		if (snd_blower._p)
		{
			if (!snd_blower.is_playing())
				snd_blower.play(pcar, sm_Looped | sm_NoOCC);
		}
		//engine exhaust
		//if (snd_engine_exhaust._p)
		//{
		//	if (!snd_engine_exhaust.is_playing())
		//		snd_engine_exhaust.play(pcar, sm_Looped);
		//}
		////idle exhaust
		//if (snd_idle_exhaust._p)
		//{
		//	if (!snd_idle_exhaust.is_playing())
		//		snd_idle_exhaust.play(pcar, sm_Looped);
		//}
		////decel exhaust
		//if (snd_engine_decel_exhaust._p)
		//{
		//	if (!snd_engine_decel_exhaust.is_playing())
		//		snd_engine_decel_exhaust.play(pcar, sm_Looped);
		//}
	}
	UpdateDrive();
	if(snd_engine_start._p && !snd_engine_start.is_playing())Drive();
}

void CCar::SCarSound::UpdateStalling()
{
	VERIFY(!physics_world()->Processing());
	SetEngineSoundPosition(snd_engine_stop);
	if (snd_engine_start._p && snd_engine_start.is_playing())
		snd_engine_start.stop_deffered(CarDeferredStopFade);
	if(!snd_engine_stop._p || !snd_engine_stop.is_playing())SwitchOff();
}
void CCar::SCarSound::UpdateDrive()
{
	VERIFY(!physics_world()->Processing());

	// UpdateStarting() is the usual place that starts the looped sounds, but a
	// running engine restored from a save skips the starter. Starting them here
	// also recovers if the first play happened while the sound was suspended
	// during level loading. During a normal start the starter sequence owns the
	// delay, so do not start the loops before engine_start_delay has elapsed.
	if (pcar->b_engine_on &&
		(eCarSound != sndStarting || time_state_start + engine_start_delay < Device.dwTimeGlobal))
	{
		auto PlayLoop = [this](ref_sound& sound)
		{
			if (sound._p && !sound.is_playing())
				sound.play(pcar, sm_Looped | sm_NoOCC);
		};

		PlayLoop(snd_engine);
		PlayLoop(snd_turbo);
		PlayLoop(snd_idle);
		PlayLoop(snd_engine_decel);
		PlayLoop(snd_engine_heavy);
		PlayLoop(snd_blower);
	}

	if (pcar->m_sound)
	{
		float sound_torque = 0.f;
		if (pcar->m_sound_rpm)
			sound_torque = pcar->m_sound_rpm;
		else
			sound_torque = pcar->m_torque_rpm;
		if (!std::isfinite(sound_torque) || sound_torque <= EPS)
			sound_torque = 1.f;

		float state_change_up = pcar->m_sound_state_change_up;//3.2f;
		float state_change_down = pcar->m_sound_state_change_down;// 2.5f;
		float idle_state_change_up = pcar->m_sound_idle_change_up;// 12.f;
		float idle_state_change_down = pcar->m_sound_idle_change_down;// 0.5f;
		if (pcar->IsTransmissionSwitching())
			state_change_down *= pcar->m_sound_state_switch_factor;
		else
		{
			if (pcar->brp)
			{
				state_change_up *= 3.f;
				state_change_down *= 1.1f;
			}
		}

		float idle_rpm_change = 5.f;
		float lim_threshold = 0.005f;

		float boost = 1.f;
		if (pcar->m_nitro_engage)
		{
			boost = 1.05f;
			state_change_up *= 1.5f;
		}
		float vol = volume;
		if (pcar->IsTransmissionSwitching())
			vol *= pcar->m_transmission_switch_volume;

		//Msg("pcar->m_engine_rpm_scale %f", pcar->m_engine_rpm_scale);

		if (!pcar->m_engine_drive)
		{
			pcar->m_heavy_sound_boost += 10.f * Device.fTimeDelta;
			clamp(pcar->m_heavy_sound_boost, 0.f, 1.f);
		}
		//Msg("decel_sound_boost %f", pcar->m_heavy_sound_boost);


		pcar->m_engine_state_scale = pcar->m_engine_drive;
		if (pcar->m_engine_state_scale > pcar->m_engine_state_scale_last)
			pcar->m_engine_state_scale = pcar->m_engine_state_scale_last + state_change_up * Device.fTimeDelta;
		if (pcar->m_engine_state_scale < pcar->m_engine_state_scale_last)
			pcar->m_engine_state_scale = pcar->m_engine_state_scale_last - state_change_down * Device.fTimeDelta;

		pcar->m_idle_state_scale = pcar->m_engine_drive;
		if (pcar->m_idle_state_scale > pcar->m_idle_state_scale_last)
			pcar->m_idle_state_scale = pcar->m_idle_state_scale_last + idle_state_change_up *  Device.fTimeDelta;
		if (pcar->m_idle_state_scale < pcar->m_idle_state_scale_last)
			pcar->m_idle_state_scale = pcar->m_idle_state_scale_last - idle_state_change_down * Device.fTimeDelta;

		clamp(pcar->m_engine_state_scale, 0.f, 1.f);
		clamp(pcar->m_idle_state_scale, 0.f, 1.f);
		pcar->m_engine_state_scale_last = pcar->m_engine_state_scale;
		pcar->m_idle_state_scale_last = pcar->m_idle_state_scale;

		//Msg("engine state scale %f", sqrtf(pcar->m_engine_state_scale));

		pcar->m_engine_idle_volume_scale = 1.f - ((pcar->m_engine_rpm_scale - EPS) * idle_rpm_change);
		clamp(pcar->m_engine_idle_volume_scale, 0.f, 1.f);
		pcar->m_engine_idle_volume_scale -= pcar->m_idle_state_scale;
		clamp(pcar->m_engine_idle_volume_scale, 0.f, 1.f);

		//Msg("m_engine_idle_volume_scale %f", sqrtf(pcar->m_engine_idle_volume_scale));

		float state_vol = 1.f;
		float decel_vol = 1.f;
		float state_freq = 1.f;
		if (snd_engine_decel._p)
		{
			state_vol = sqrtf(pcar->m_engine_state_scale);
			decel_vol = sqrtf(1.f - pcar->m_engine_state_scale);
		}
		else
		{
			state_vol = pcar->m_engine_state_scale_min + (1.f - pcar->m_engine_state_scale_min) * sqrtf(pcar->m_engine_state_scale);
			state_freq = 0.7f + 0.3f * state_vol;
		}
		
		float idle_vol = 1.f;
		float idle_freq = 1.f;
		if (snd_idle._p)
		{
			idle_vol = sqrtf(1.f - pcar->m_engine_idle_volume_scale);
			snd_idle.set_frequency(1.5f - 0.5f * sqrtf(pcar->m_engine_idle_volume_scale));
			snd_idle.set_volume(sqrtf(pcar->m_engine_idle_volume_scale) * volume);
			SetEngineSoundPosition(snd_idle);
		}
		else
		{
			idle_vol = pcar->m_engine_idle_volume + (1.f - pcar->m_engine_idle_volume) * sqrtf(1.f - pcar->m_engine_idle_volume_scale);
			idle_freq = 0.5f + 0.5f * idle_vol;
		}

		float engine_lim_vol = 1.f;
		if (snd_limiter._p)
		{
			if (pcar->m_engine_drive && lim_threshold > 0.f)
			{
				pcar->m_limiter_volume = (pcar->m_engine_rpm_scale - (1.f - lim_threshold * 1.5f)) / lim_threshold;
				clamp(pcar->m_limiter_volume, 0.f, 1.f);
			}
			else
				pcar->m_limiter_volume = 0.f;

			if (pcar->m_limiter_volume > 0.f)
			{
				if (!snd_limiter.is_playing())
					snd_limiter.play(pcar, sm_Looped | sm_NoOCC);
			}
			else
				if (snd_limiter.is_playing())
					snd_limiter.stop_deffered(CarDeferredStopFade);

			if (snd_limiter.is_playing())
			{
				//Msg("m_limiter_volume %f", sqrtf(pcar->m_limiter_volume));
				snd_limiter.set_frequency(boost);
				snd_limiter.set_volume(sqrtf(pcar->m_limiter_volume) * volume);
				SetEngineSoundPosition(snd_limiter);
			}
			engine_lim_vol = 0.3f + (0.7f * sqrt(1.f - pcar->m_limiter_volume));
		}

		float delay = pcar->m_transmission_switch_delay;
		if (pcar->m_nitro_engage)
			delay *= pcar->m_nitro_switch_boost;

		float sound_rpm = pcar->m_current_rpm;
		if (pcar->IsTransmissionSwitching() && delay > 0.f)
		{
			float x = (Device.fTimeGlobal - pcar->m_transmission_switch_sound_time) * (1.f / delay);
			x = 0.5f * sinf(PI * x - PI * 0.5f) + 0.5f;
			clamp(x, 0.f, 1.f);
			//Msg("XXXXX %f", x);
			sound_rpm = pcar->m_transmission_switch_rpm - (pcar->m_transmission_switch_rpm - pcar->m_current_rpm) * x;
		}
		//Msg("sound rpm %f", sound_rpm);

		if (pcar->m_sound_switch_effect_type == 1 && pcar->m_sound_effect < pcar->m_sound_switch_effect_duration)
		{
			float trans = float(pcar->CurrentTransmission()) - 1.f;
			clamp(trans, 1.f, trans);
			trans = 1.f / trans;
			//("trans %f", trans);
			float y = 1.f + trans * pcar->m_sound_switch_effect_power * 0.01f * Random.randF(0.8f, 1.2f) * (pcar->m_sound_effect - pcar->m_sound_switch_effect_duration) * sinf(pcar->m_sound_effect);
			sound_rpm *= y;
			pcar->m_sound_effect += pcar->m_sound_switch_effect_speed * boost * 25.f * Device.fTimeDelta;
			clamp(pcar->m_sound_effect, 0.f, pcar->m_sound_switch_effect_duration);
			//Msg("sound_effect %f", y);
		}

		float heavy = 1.f;
		if (snd_engine_heavy._p)
		{
			//if (pcar->m_engine_state_scale > 0.f || pcar->m_idle_state_scale > 0.f)
			//{
				pcar->m_heavy_sound_boost = (pcar->m_heavy_sound_boost - 1.5f * Device.fTimeDelta);
				clamp(pcar->m_heavy_sound_boost, 0.f, 1.f);
				float freq = (pcar->m_engine_heavy_constant + pcar->m_engine_heavy_rpm * (1.f - pcar->m_engine_rpm_scale) + pcar->m_engine_heavy_switch * pcar->m_heavy_sound_boost) * boost;
				clamp(freq, 0.f, 1.f);
				pcar->m_engine_heavy = sqrtf(freq) * state_vol * idle_vol * engine_lim_vol;
				heavy = sqrtf(1.f - freq);
				//Msg("heavy %f", heavy);
			//}
			//else
			//	pcar->m_engine_heavy = 0.f;
		}

		pcar->m_engine_sound_scale_absolute = (pcar->m_engine_sound_scale_min + (0.5f * sound_rpm) / sound_torque) * pcar->m_engine_sound_scale_factor;
		if (!std::isfinite(pcar->m_engine_sound_scale_absolute))
			pcar->m_engine_sound_scale_absolute = pcar->m_engine_sound_scale_min;
		clamp(pcar->m_engine_sound_scale_absolute, pcar->m_engine_sound_scale_min, pcar->m_engine_sound_scale_max);
		pcar->m_engine_sound_scale = pcar->m_engine_sound_scale_absolute * state_freq * idle_freq;
		if (!std::isfinite(pcar->m_engine_sound_scale))
			pcar->m_engine_sound_scale = pcar->m_engine_sound_scale_min;
		pcar->m_engine_volume_scale = state_vol * idle_vol * engine_lim_vol * heavy;
		if (!std::isfinite(pcar->m_engine_volume_scale))
			pcar->m_engine_volume_scale = 0.f;
		clamp(pcar->m_engine_volume_scale, 0.f, 1.f);

		if (snd_engine._p)
		{
			const bool engine_can_play = pcar->m_engine_volume_scale > 0.f && (time_state_start + engine_start_delay < Device.dwTimeGlobal || eCarSound != sndStarting);

			if (engine_can_play && !snd_engine.is_playing())
				snd_engine.play(pcar, sm_Looped | sm_NoOCC);

			// Keep the loop alive at idle; stopping it makes the mixer fade the
			// slot, so pressing gas again restores volume only after the fade.
			if (snd_engine.is_playing())
			{
				snd_engine.set_frequency(pcar->m_engine_sound_scale * boost);
				snd_engine.set_volume(engine_can_play ? pcar->m_engine_volume_scale * vol : 0.f);
				SetEngineSoundPosition(snd_engine);
			}
		}

		if (snd_engine_heavy._p)
		{
			if (pcar->m_engine_heavy > 0.f && (time_state_start + engine_start_delay < Device.dwTimeGlobal || eCarSound != sndStarting))
			{
				if (!snd_engine_heavy.is_playing())
					snd_engine_heavy.play(pcar, sm_Looped | sm_NoOCC);
			}
			else
			{
				if (snd_engine_heavy.is_playing())
					snd_engine_heavy.stop_deffered(CarDeferredStopFade);
			}

			if (snd_engine_heavy.is_playing())
			{
				snd_engine_heavy.set_frequency(pcar->m_engine_sound_scale * boost);
				snd_engine_heavy.set_volume(pcar->m_engine_heavy * vol);
				SetEngineSoundPosition(snd_engine_heavy);
				//Msg("heavy %f", pcar->m_engine_heavy);
			}
		}

		if (snd_engine_decel._p)
		{
			pcar->m_engine_decel_volume_scale = decel_vol * idle_vol * engine_lim_vol;

			if (pcar->m_engine_decel_volume_scale > 0.f && (time_state_start + engine_start_delay < Device.dwTimeGlobal || eCarSound != sndStarting))
			{
				if (!snd_engine_decel.is_playing())
					snd_engine_decel.play(pcar, sm_Looped | sm_NoOCC);
			}
			else
			{
				pcar->m_decel_sound_boost += 0.3f * Device.fTimeDelta;
				clamp(pcar->m_decel_sound_boost, 0.9f, 1.15f);
				if (snd_engine_decel.is_playing())
					snd_engine_decel.stop_deffered(CarDeferredStopFade);
			}

			if (snd_engine_decel.is_playing())
			{
				snd_engine_decel.set_frequency(pcar->m_engine_sound_scale * boost);
				snd_engine_decel.set_volume(pcar->m_engine_decel_volume_scale * pcar->m_decel_sound_boost * vol);
				//Msg("decel ON %f", pcar->m_engine_decel_volume_scale * pcar->m_decel_sound_boost);
				//Msg("decel freq %f", pcar->m_engine_sound_scale);
				SetEngineSoundPosition(snd_engine_decel);
				pcar->m_decel_sound_boost -= 0.2f * Device.fTimeDelta;
				clamp(pcar->m_decel_sound_boost, 0.9f, 1.15f);
			}
		}

		//turbo
		if (snd_turbo._p)
		{
			if (pcar->m_engine_drive)
				pcar->m_turbo_scale = pcar->m_turbo_scale + (pcar->m_turbo_scale_max - pcar->m_turbo_scale) * Device.fTimeDelta * pcar->m_turbo_scale_speed;
			else
				pcar->m_turbo_scale = pcar->m_turbo_scale + (pcar->m_turbo_scale_min - pcar->m_turbo_scale) * Device.fTimeDelta * pcar->m_turbo_scale_speed * 0.3f;

			clamp(pcar->m_turbo_scale, pcar->m_turbo_scale_min, pcar->m_turbo_scale_max);
			//Msg("turbo scale %f", pcar->m_turbo_scale);

			pcar->m_turbo_volume_scale = 0.4f + 0.5f * pcar->m_engine_state_scale + 0.3f * pcar->m_turbo_scale;

			snd_turbo.set_frequency((0.5f * pcar->m_turbo_scale + 0.5f * pcar->m_turbo_scale * pcar->m_engine_rpm_scale) * boost);
			snd_turbo.set_volume(pcar->m_turbo_volume_scale * volume);
			SetEngineSoundPosition(snd_turbo);
		}
		//turbo
		//blower
		if (snd_blower._p)
		{
			float blower_scale = pcar->m_engine_state_scale;
			if (pcar->e_state_drive == drive && pcar->IsTransmissionSwitching())
				blower_scale = 1.f;
			float blower_vol = (pcar->m_blower_volume_min + (1.f - pcar->m_blower_volume_min) * blower_scale);

			float scale = 1.f;
			if (pcar->m_blower_sound_forward)
			{
				Fvector	dir;
				dir.sub(Device.vCameraPosition, pcar->Position());
				const float dist_sq = dir.square_magnitude();
				if (dist_sq > EPS && pcar->Direction().square_magnitude() > EPS)
				{
					dir.div(_sqrt(dist_sq));
					scale = dir.dotproduct(pcar->Direction());
				}
				clamp(scale, 0.f, 1.f);
				blower_vol *= (1.f - pcar->m_blower_sound_forward) + (scale * pcar->m_blower_sound_forward);
			}
			if (!std::isfinite(blower_vol))
				blower_vol = 0.f;
			clamp(blower_vol, 0.f, 1.f);
			if (pcar->m_blower_rpm_current < pcar->m_engine_rpm_scale)
				pcar->m_blower_rpm_current = pcar->m_engine_rpm_scale;
			else
				pcar->m_blower_rpm_current = pcar->m_blower_rpm_current - pcar->m_blower_inertia * (pcar->m_blower_rpm_current - pcar->m_engine_rpm_scale);

			float blower_freq = (pcar->m_blower_scale_min + pcar->m_blower_rpm_scale * pcar->m_blower_rpm_current + 0.1f * pcar->m_engine_state_scale + 0.05f * scale) * boost;
			if (!std::isfinite(blower_freq) || blower_freq <= 0.f)
				blower_freq = 0.01f;
			clamp(blower_freq, 0.01f, 8.f);
			snd_blower.set_frequency(blower_freq);
			snd_blower.set_volume(blower_vol * vol);
			SetEngineSoundPosition(snd_blower);
			//Msg("blower freq %f", blower_freq);
		}
		//blower
		//nitro
		if (snd_nitro._p)
		{
			if (pcar->m_nitro_engage)
			{
				if (!snd_nitro.is_playing())
					snd_nitro.play(pcar, sm_Looped | sm_NoOCC);
			}
			else
			{
				if (snd_nitro.is_playing())
					snd_nitro.stop_deffered(CarDeferredStopFade);
			}
			if (snd_nitro.is_playing())
			{
				snd_nitro.set_frequency(0.7f + 0.3f * (pcar->m_engine_sound_scale_absolute));
				if (!pcar->IsTransmissionSwitching())
					snd_nitro.set_volume(volume);
				else
					snd_nitro.set_volume(volume * 0.6f);
				//Msg("nitro %f", 0.7f + 0.3f * (pcar->m_engine_sound_scale / boost));
				SetSoundPosition(snd_nitro);
			}
		}
		//nitro

		SetEngineSoundPosition(snd_valve);
		SetSoundPosition(snd_transmission);
		SetSoundPosition(snd_nitro_start);
		SetSoundPosition(snd_nitro_stop);
		SetSoundPosition(snd_pop);
		SetSoundPosition(snd_brake_release);
		SetSoundPosition(snd_gas);
	}
}

void CCar::SCarSound::SwitchState(ESoundState new_state)
{
	eCarSound=new_state;
	time_state_start=Device.dwTimeGlobal;
}
void CCar::SCarSound::Update()
{
	VERIFY(!physics_world()->Processing());

	SetSoundPosition(snd_fire);

	// A running engine must not stay in a stop/stall sound state. This can
	// happen after loading when the restored engine state arrives after an
	// older Stop()/Stall(), leaving the stop sound fading over the idle loop.
	if (pcar->b_engine_on && (eCarSound == sndStoping || eCarSound == sndStalling))
	{
		Drive();
	}
	else if (eCarSound == sndOff)
	{
		// Init() resets the sound state when the car is spawned. A restored
		// running engine must not stay silent if that reset happened after
		// RestoreEngineAndLights() or if the first play was lost while the
		// sound subsystem was still loading.
		if (!pcar->b_engine_on)
		{
			pcar->m_engine_state_scale = pcar->m_engine_state_scale_min;
			return;
		}

		Drive();
	}
	switch (eCarSound)
	{
	case sndStarting	:UpdateStarting	()	;	break;
	case sndDrive		:UpdateDrive	()	;	break;
	case sndStalling	:UpdateStalling	()	;	break;
	case sndStoping		:UpdateStalling	()	;	break;
	}
	

}

void CCar::SCarSound::SwitchOn()
{
	pcar->processing_activate();
}
void CCar::SCarSound::Destroy()
{
	SwitchOff();
	snd_engine.destroy	();
	//snd_engine_exhaust.destroy();
	snd_transmission.destroy();
	snd_engine_stop.destroy();
	snd_engine_start.destroy();
	snd_gas.destroy();
	snd_brake_release.destroy();
	snd_turbo.destroy();
	snd_valve.destroy();
	snd_idle.destroy();
	//snd_idle_exhaust.destroy();
	snd_limiter.destroy();
	//snd_limiter_exhaust.destroy();
	snd_engine_decel.destroy();
	//snd_engine_decel_exhaust.destroy();
	snd_engine_heavy.destroy();
	snd_blower.destroy();
	snd_nitro.destroy();
	snd_nitro_start.destroy();
	snd_nitro_stop.destroy();
	snd_pop.destroy();
	snd_whine.destroy();
	snd_horn.destroy();
	snd_fire.destroy();
}

void CCar::SCarSound::SwitchOff()
{
	eCarSound=sndOff;
	pcar->processing_deactivate();
}

void CCar::SCarSound::Start()
{
	VERIFY(!physics_world()->Processing());
	if(eCarSound==sndOff) SwitchOn();
	SwitchState(sndStarting);

	// A new start must not be masked by a stale stop sound from a previous
	// failed start or from a state restored before the engine chunk arrived.
	if (snd_engine_stop._p && snd_engine_stop.is_playing())
		snd_engine_stop.stop_deffered(CarDeferredStopFade);

	if (snd_engine_start._p)
	{
		snd_engine_start.play(pcar, sm_NoOCC);
		snd_engine_start.set_volume(volume);
	}
	SetEngineSoundPosition(snd_engine_start);
}

void CCar::SCarSound::StopSounds(ESoundState new_state)
{
	VERIFY(!physics_world()->Processing());
	if (eCarSound == sndOff)
		return;

	SwitchState(new_state);
	if (snd_engine._p)
		snd_engine.stop_deffered(CarDeferredStopFade);
	if (snd_turbo._p)
		snd_turbo.stop_deffered(CarDeferredStopFade);
	if (snd_nitro._p)
		snd_nitro.stop_deffered(CarDeferredStopFade);
	if (snd_limiter._p)
		snd_limiter.stop_deffered(CarDeferredStopFade);
	if (snd_idle._p)
		snd_idle.stop_deffered(CarDeferredStopFade);
	if (snd_engine_decel._p)
		snd_engine_decel.stop_deffered(CarDeferredStopFade);
	if (snd_engine_heavy._p)
		snd_engine_heavy.stop_deffered(CarDeferredStopFade);
	if (snd_blower._p)
		snd_blower.stop_deffered(CarDeferredStopFade);

	if (snd_engine_stop._p)
	{
		snd_engine_stop.play(pcar, sm_NoOCC);
		snd_engine_stop.set_volume(volume);
	}
	SetEngineSoundPosition(snd_engine_stop);
}

void CCar::SCarSound::Stop()
{
	StopSounds(sndStoping);
}

void CCar::SCarSound::Stall()
{
	StopSounds(sndStalling);
}

void CCar::SCarSound::Drive()
{
	VERIFY(!physics_world()->Processing());
	if(eCarSound==sndOff) SwitchOn();
	SwitchState(sndDrive);

	// A resumed engine skips the starter sequence. Do not leave a stale
	// starter/stop one-shot playing over the looped engine sounds.
	if (snd_engine_start._p && snd_engine_start.is_playing())
		snd_engine_start.stop_deffered(CarDeferredStopFade);
	if (snd_engine_stop._p && snd_engine_stop.is_playing())
		snd_engine_stop.stop_deffered(CarDeferredStopFade);
}

void CCar::SCarSound::TransmissionSwitch()
{
	VERIFY(!physics_world()->Processing());
	if(snd_transmission._p && pcar->Owner())
	{
		//snd_transmission.set_volume(volume);
		if (!snd_transmission.is_playing())
		{
			// Gearbox sounds are not attenuated by the car interior volume.
			snd_transmission.play(pcar, sm_NoCarFactor | sm_NoOCC);
			snd_transmission.set_frequency(Random.randF(0.93f, 1.07f));
		}
		SetSoundPosition(snd_transmission);
	}
}

void CCar::SCarSound::Valve()
{
	VERIFY(!physics_world()->Processing());
	if (snd_valve._p && snd_turbo._p && eCarSound != sndOff)
	{
		//clamp(pcar->m_valve_time, 0.f, 1.f);
		float rpm_scale = (pcar->m_engine_rpm_scale - 0.15f) / 0.79f;
		clamp(rpm_scale, 0.f, 1.f);

		const float TurboSpan = pcar->m_turbo_scale_max - pcar->m_turbo_scale_min;
		if (TurboSpan > EPS)
		{
			const float TurboMid = 0.5f * (pcar->m_turbo_scale - pcar->m_turbo_scale_min);
			pcar->m_valve_volume_scale = rpm_scale *
				((pcar->m_turbo_scale - TurboMid - pcar->m_turbo_scale_min) /
					(pcar->m_turbo_scale_max - TurboMid - pcar->m_turbo_scale_min));
		}
		else
		{
			pcar->m_valve_volume_scale = 0.f;
		}

		if (!std::isfinite(pcar->m_valve_volume_scale))
			pcar->m_valve_volume_scale = 0.f;
		clamp(pcar->m_valve_volume_scale, 0.f, 1.f);

		snd_valve.play(pcar, sm_NoOCC);
		pcar->m_engine_valve = 0;
		float switch_vol = 1.f;
		if (pcar->IsTransmissionSwitching())
			switch_vol *= pcar->m_valve_switch_volume;
		float time = (Device.fTimeGlobal - pcar->m_valve_time) * 4.f;
		clamp(time, 0.f, 1.f);
		//time = powf(time, 2.f);
		pcar->m_valve_time = Device.fTimeGlobal;

		float Volume = pcar->m_valve_volume_scale * switch_vol * time;
		if (!std::isfinite(Volume))
			Volume = 0.f;
		clamp(Volume, 0.f, 1.f);

		snd_valve.set_volume(Volume);
		snd_valve.set_frequency((0.6f + 0.4f * pcar->m_valve_volume_scale) * Random.randF(0.95f, 1.05f));
		SetEngineSoundPosition(snd_valve);
	}
}

void CCar::SCarSound::Gas()
{
	VERIFY(!physics_world()->Processing());
	if (snd_gas._p && eCarSound != sndOff)
	{
		//float state_time_gas = 1.f;
		//Msg("gas time %f", pcar->m_engine_state_time);
		//pcar->m_gas_volume_scale = state_time_gas;
		snd_gas.play(pcar, sm_NoOCC);
		pcar->m_gas_volume_scale = volume;//(pcar->m_gas_volume_scale * 0.75f) / (0.5f + 0.5f * pcar->m_engine_sound_scale_absolute);
		snd_gas.set_volume(pcar->m_gas_volume_scale);
		snd_gas.set_frequency(1.f);
		SetEngineSoundPosition(snd_gas);
	}
}

void CCar::SCarSound::BrakeRelease()
{
	VERIFY(!physics_world()->Processing());
	if (snd_brake_release._p)
	{
		float brake_vol = 1.f;
		if (pcar->m_break_time > EPS)
			brake_vol = (Device.fTimeGlobal - pcar->m_break_start) / pcar->m_break_time;
		if (!std::isfinite(brake_vol))
			brake_vol = 0.f;
		clamp(brake_vol, 0.f, 1.f);
		brake_vol = powf(brake_vol, 1.4f);
		snd_brake_release.play(pcar, sm_NoOCC);
		snd_brake_release.set_volume(volume * brake_vol);
		snd_brake_release.set_frequency((0.8f + 0.2f * brake_vol) * Random.randF(0.91f, 1.09f));
	}
	SetSoundPosition(snd_brake_release);
}

void CCar::SCarSound::NitroStart()
{
	VERIFY(!physics_world()->Processing());
	if (snd_nitro_start._p)
	{
		snd_nitro_start.play(pcar, sm_NoOCC);
		snd_nitro_start.set_volume(volume);
	}
	SetSoundPosition(snd_nitro_start);
}

void CCar::SCarSound::NitroStop()
{
	VERIFY(!physics_world()->Processing());
	if (snd_nitro_stop._p)
	{
		snd_nitro_stop.play(pcar, sm_NoOCC);
		snd_nitro_stop.set_volume(volume);
	}
	pcar->m_nitro_engage_time = Device.fTimeGlobal;
	SetSoundPosition(snd_nitro_stop);
}

void CCar::SCarSound::PopSound()
{
	VERIFY(!physics_world()->Processing());
	if (snd_pop._p)
	{
		snd_pop.play(pcar, sm_NoOCC);
		snd_pop.set_volume(volume);
		snd_pop.set_frequency(Random.randF(0.9f, 1.1f));
	}
	SetSoundPosition(snd_pop);
}

void CCar::SCarSound::Whine()
{
	VERIFY(!physics_world()->Processing());
	if (!snd_whine._p)
		return;

	if (pcar->m_driving_wheels.empty() || pcar->m_whine_speed <= EPS)
	{
		if (snd_whine.is_playing())
			snd_whine.stop_deffered(CarDeferredStopFade);
		return;
	}

	float avg_speed = 0.f;
	for (SWheelDrive& Drive : pcar->m_driving_wheels)
		avg_speed += _abs(Drive.ASpeed()) * Drive.pwheel->radius;

	if (!std::isfinite(avg_speed))
	{
		pcar->m_whine_sound_scale = 0.f;
		if (snd_whine.is_playing())
			snd_whine.stop_deffered(CarDeferredStopFade);
		return;
	}

	avg_speed = std::max(avg_speed - 1.f, 0.f);
	avg_speed = 2.f * sqrtf(((avg_speed * 3.6f) / float(pcar->m_driving_wheels.size())) / pcar->m_whine_speed);
	if (!std::isfinite(avg_speed))
		avg_speed = 0.f;
	clamp(avg_speed, 0.f, 2.f);

	const float Dt = std::clamp(Device.fTimeDelta, 0.f, 0.1f);
	float boost = 1.f;
	if (pcar->m_nitro_engage)
		boost = 1.04f;

	pcar->m_whine_sound_scale = (1.f - 2.5f * Dt) * pcar->m_whine_sound_scale + 2.5f * avg_speed * Dt;
	if (!std::isfinite(pcar->m_whine_sound_scale))
		pcar->m_whine_sound_scale = 0.f;
	clamp(pcar->m_whine_sound_scale, 0.f, 4.f);

	float vol = avg_speed * 0.45f + 0.55f * pcar->m_engine_state_scale;
	if (pcar->IsTransmissionSwitching() && pcar->e_state_drive == drive)
		vol *= pcar->m_transmission_switch_volume;

	if ((avg_speed > 0.2f || pcar->GetSpeedKmH() > 1.f) && ((pcar->CurrentTransmission() == 0 && pcar->m_whine_reverse) || !pcar->m_whine_reverse))
	{
		if (!snd_whine.is_playing())
			snd_whine.play(pcar, sm_Looped | sm_NoCarFactor | sm_NoOCC);

		if (snd_whine.is_playing())
		{
			if (pcar->active_camera == nullptr || pcar->active_camera->tag != EctFirstCam || !pcar->Owner())
				vol *= pcar->m_whine_exterior_volume;

			if (!std::isfinite(vol))
				vol = 0.f;
			clamp(vol, 0.f, 1.f);

			if (vol <= EPS)
			{
				snd_whine.stop_deffered(CarDeferredStopFade);
				return;
			}

			float Frequency = pcar->m_whine_sound_scale * boost;
			if (!std::isfinite(Frequency) || Frequency <= 0.f)
				Frequency = 0.01f;
			clamp(Frequency, 0.01f, 8.f);

			snd_whine.set_volume(vol * volume);
			snd_whine.set_frequency(Frequency);
			SetSoundPosition(snd_whine);
		}
	}
	else if (snd_whine.is_playing())
	{
		snd_whine.stop_deffered(CarDeferredStopFade);
	}
}

void CCar::SCarSound::Horn(bool start)
{
	VERIFY(!physics_world()->Processing());
	if (snd_horn._p)
	{
		if (start)
		{
			if (!snd_horn.is_playing())
				snd_horn.play(pcar, sm_Looped | sm_NoOCC);
		}
		else
		{
			if (snd_horn.is_playing())
				snd_horn.stop_deffered(CarDeferredStopFade);
		}
			SetSoundPosition(snd_horn);
	}
}

void CCar::SCarSound::StartFire()
{
	if (!snd_fire._p || snd_fire.is_playing())
		return;

	snd_fire.play(pcar, sm_Looped | sm_NoOCC);
	snd_fire.set_volume(volume);
}

void CCar::SCarSound::StopFire()
{
	if (snd_fire._p && snd_fire.is_playing())
		snd_fire.stop_deffered(CarDeferredStopFade);
}

namespace CarCabinSound
{
	// Cutoff of the cabin low_pass, 0 when off. Only the car with the actor sets it
	float g_CabinCutoffHz = 0.0f;

	// World sounds, their reverb sends and the zone reverbs; not music
	template <typename TCallback>
	void ForEachCabinBus(TCallback&& Callback)
	{
		for (const char* Name : {"effects", "shooting", "shooting_far", "shooting_indoor"})
		{
			if (const u32 Bus = XRay::Sound::Mixer::FindBus(Name))
			{
				Callback(Bus);
			}
		}

		const SoundBus* Buses = XRay::Sound::Mixer::GetBuses();
		for (u32 BusIdx = 0; BusIdx < SND_BUS_COUNT; BusIdx++)
		{
			if (Buses[BusIdx].IsUsed && Buses[BusIdx].IsGenerated)
			{
				Callback(BusIdx + 1);
			}
		}
	}

	// CutoffHz <= 0 removes the muffling
	void SetCabinMuffling(float CutoffHz)
	{
		using namespace XRay::Sound;
		if (CutoffHz <= 0.0f)
		{
			if (g_CabinCutoffHz > 0.0f)
			{
				ForEachCabinBus([](u32 Bus) { Mixer::RemoveBusShader(Bus, SoundShaderType::LowPass, true); });
			}
			g_CabinCutoffHz = 0.0f;
			return;
		}

		if (g_CabinCutoffHz <= 0.0f)
		{
			// Spatial only: UI and HUD sounds are not in the world
			ForEachCabinBus([](u32 Bus) { Mixer::AddBusShader(Bus, SoundShaderType::LowPass, true); });
		}

		// Every call queues commands, skip changes under 1 Hz
		if (std::abs(CutoffHz - g_CabinCutoffHz) >= 1.0f)
		{
			ForEachCabinBus([CutoffHz](u32 Bus) { Mixer::SetBusShaderParam(Bus, SoundShaderType::LowPass, "cutoff", CutoffHz); });
			g_CabinCutoffHz = CutoffHz;
		}
	}
}


void CCar::ResetCabinSound()
{
	psSoundCarFactor = 1.f;
	CarCabinSound::SetCabinMuffling(0.0f);
}

void CCar::UpdateInteriorSoundFactor(bool FirstPersonView)
{
	if (!Owner())
		return;

	float InteriorVolume = m_interior_volume;
	clamp(InteriorVolume, 0.f, 1.f);

	// An open normal door connects the cabin with the outside, so the
	// interior volume moves halfway towards the exterior volume.
	if (FirstPersonView && HasOpenNormalDoor())
	{
		InteriorVolume = 0.5f * (InteriorVolume + 1.f);
	}

	psSoundCarFactor = FirstPersonView ? InteriorVolume : 1.f;

	constexpr float InteriorMinCutoffHz = 500.f;
	if (FirstPersonView && psSoundCarFactor < 0.999f)
	{
		CarCabinSound::SetCabinMuffling(XRay::Sound::LowPass::CutoffFromFactor(psSoundCarFactor * psSoundCarFactor, InteriorMinCutoffHz));
	}
	else
	{
		CarCabinSound::SetCabinMuffling(0.0f);
	}
}

void CCar::LoadCollisionSounds()
{
	if (g_dedicated_server || !pSettings || !pSettings->line_exist(cNameSect_str(), "collision_sounds"))
		return;

	const char* SectionName = pSettings->r_string(cNameSect_str(), "collision_sounds");
	if (SectionName == nullptr || SectionName[0] == '\0')
		return;

	if (!pSettings->section_exist(SectionName))
	{
		Msg("! [%s] collision_sounds section '%s' does not exist", cNameSect_str(), SectionName);
		return;
	}

	// The material pairs are global, so load each section only once.
	static xr_vector<xr_string> LoadedSections;
	const xr_string Section(SectionName);
	if (std::find(LoadedSections.begin(), LoadedSections.end(), Section) != LoadedSections.end())
		return;

	const CInifile::Sect& Config = pSettings->r_section(SectionName);
	u32 Overridden = 0;
	for (const CInifile::Item& Item : Config.Data)
	{
		const char* Key = Item.first.c_str();
		const char* Separator = strchr(Key, '|');
		if (Separator == nullptr || Separator == Key || Separator[1] == '\0')
		{
			Msg("! [%s] collision_sounds: invalid pair '%s', expected 'material0|material1'", cNameSect_str(), Key);
			continue;
		}

		const char* SoundList = Item.second.c_str();
		if (SoundList == nullptr || SoundList[0] == '\0')
		{
			// An empty value keeps the sounds loaded from gamemtl.xr.
			continue;
		}

		const xr_string Material0(Key, Separator - Key);
		const char* Material1 = Separator + 1;

		// Wheels use their own tire sound system.
		if (0 == _stricmp(Material0.c_str(), "objects\\car_wheel") || 0 == _stricmp(Material1, "objects\\car_wheel"))
			continue;

		auto It0 = GMLib.GetMaterialIt(Material0.c_str());
		auto It1 = GMLib.GetMaterialIt(Material1);
		if (It0 == GMLib.LastMaterial() || It1 == GMLib.LastMaterial())
		{
#ifndef MASTER_GOLD
			Msg("~ [%s] collision_sounds: skipping pair '%s' (material not present in this game)", cNameSect_str(), Key);
#endif
			continue;
		}

		const u16 Index0 = u16(It0 - GMLib.FirstMaterial());
		const u16 Index1 = u16(It1 - GMLib.FirstMaterial());
		SGameMtlPair* Pair = GMLib.GetMaterialPair(Index0, Index1);
		if (Pair == nullptr)
		{
#ifndef MASTER_GOLD
			Msg("~ [%s] collision_sounds: skipping pair '%s' (missing in gamemtl.xr)", cNameSect_str(), Key);
#endif
			continue;
		}

		for (ref_sound& Sound : Pair->CollideSounds)
			Sound.destroy();
		Pair->CollideSounds.clear();

		const int Count = _GetItemCount(SoundList);
		Pair->CollideSounds.resize(Count);
		string128 SoundName;
		for (int i = 0; i < Count; ++i)
		{
			Pair->CollideSounds[i].create(_GetItem(SoundList, i, SoundName), st_Effect, sg_SourceType);
		}
		++Overridden;
	}

	Msg("* [%s] collision_sounds: overridden %u material pairs from '%s'", cNameSect().c_str(), Overridden, SectionName);
	LoadedSections.push_back(Section);
}

void CCar::PlayDynamicCollisionSound(SGameMtl* CarMaterial, SGameMtl* OtherMaterial, const Fvector& Position, float RelativeSpeed)
{
	if (g_dedicated_server || CarMaterial == nullptr || OtherMaterial == nullptr)
		return;

	// A resting or slowly pushed prop must not play the impact sound every
	// physics step. The static collision path uses its own velocity thresholds;
	// dynamic contacts need a small car-specific one.
	constexpr float MinRelativeSpeed = 1.f;
	constexpr u32 SoundCooldownMs = 150;

	if (RelativeSpeed < MinRelativeSpeed || Device.dwTimeGlobal < m_collision_sound_time)
		return;

	const u16 CarIndex = GMLib.GetMaterialIdx(CarMaterial->ID);
	const u16 OtherIndex = GMLib.GetMaterialIdx(OtherMaterial->ID);
	SGameMtlPair* Pair = GMLib.GetMaterialPair(CarIndex, OtherIndex);
	if (Pair == nullptr || Pair->CollideSounds.empty())
		return;

	ref_sound* Sound = &Pair->CollideSounds[::Random.randI(Pair->CollideSounds.size())];
	if (Sound == nullptr || Sound->_p == nullptr)
		return;

	m_collision_sound_time = Device.dwTimeGlobal + SoundCooldownMs;

	float Volume = RelativeSpeed / 8.f;
	clamp(Volume, 0.15f, 1.f);
	float Frequency = ::Random.randF(0.9f, 1.1f);
	Fvector SoundPosition = Position;
	Sound->play_no_feedback(nullptr, 0, 0, &SoundPosition, &Volume, &Frequency);
}

void CCar::LoadSoundDefinitions(CInifile* ini)
{
	m_engine_sound_scale_max = ini->read_if_exists<float>("car_sound", "engine_sound_scale_max", m_engine_sound_scale_max);
	m_engine_sound_scale_min = ini->read_if_exists<float>("car_sound", "engine_sound_scale_min", m_engine_sound_scale_min);
	m_engine_sound_scale_factor = ini->read_if_exists<float>("car_sound", "engine_sound_scale_factor", m_engine_sound_scale_factor);
	m_engine_idle_volume = ini->read_if_exists<float>("car_sound", "engine_idle_volume", m_engine_idle_volume);
	m_sound_state_change_up = ini->read_if_exists<float>("car_sound", "sound_state_change_up", m_sound_state_change_up);
	m_sound_state_change_down = ini->read_if_exists<float>("car_sound", "sound_state_change_down", m_sound_state_change_down);
	m_sound_idle_change_up = ini->read_if_exists<float>("car_sound", "sound_idle_change_up", m_sound_idle_change_up);
	m_sound_idle_change_down = ini->read_if_exists<float>("car_sound", "sound_idle_change_down", m_sound_idle_change_down);
	m_sound_state_switch_factor = ini->read_if_exists<float>("car_sound", "sound_state_switch_factor", m_sound_state_switch_factor);
	m_sound_state_after_switch = ini->read_if_exists<float>("car_sound", "sound_state_after_switch", m_sound_state_after_switch);
	m_sound_switch_effect_type = ini->read_if_exists<u32>("car_sound", "sound_switch_effect_type", m_sound_switch_effect_type);
	m_sound_switch_effect_power = ini->read_if_exists<float>("car_sound", "sound_switch_effect_power", m_sound_switch_effect_power);
	m_sound_switch_effect_speed = ini->read_if_exists<float>("car_sound", "sound_switch_effect_speed", m_sound_switch_effect_speed);
	m_sound_switch_effect_duration = ini->read_if_exists<float>("car_sound", "sound_switch_effect_duration", m_sound_switch_effect_duration);
	m_turbo_scale_max = ini->read_if_exists<float>("car_sound", "turbo_scale_max", m_turbo_scale_max);
	m_turbo_scale_min = ini->read_if_exists<float>("car_sound", "turbo_scale_min", m_turbo_scale_min);
	m_turbo_scale_speed = ini->read_if_exists<float>("car_sound", "turbo_scale_speed", m_turbo_scale_speed);
	m_valve_switch_volume = ini->read_if_exists<float>("car_sound", "valve_switch_volume", m_valve_switch_volume);
	m_blower_scale_min = ini->read_if_exists<float>("car_sound", "blower_scale_min", m_blower_scale_min);
	m_blower_rpm_scale = ini->read_if_exists<float>("car_sound", "blower_rpm_scale", m_blower_rpm_scale);
	m_blower_volume_min = ini->read_if_exists<float>("car_sound", "blower_volume_min", m_blower_volume_min);
	m_blower_inertia = ini->read_if_exists<float>("car_sound", "blower_inertia", m_blower_inertia);
	m_blower_sound_forward = ini->read_if_exists<float>("car_sound", "blower_sound_forward", m_blower_sound_forward);
	m_engine_state_scale_min = ini->read_if_exists<float>("car_sound", "engine_state_scale_min", m_engine_state_scale_min);
	m_engine_heavy_constant = ini->read_if_exists<float>("car_sound", "engine_heavy_constant", m_engine_heavy_constant);
	m_engine_heavy_rpm = ini->read_if_exists<float>("car_sound", "engine_heavy_rpm", m_engine_heavy_rpm);
	m_engine_heavy_switch = ini->read_if_exists<float>("car_sound", "engine_heavy_switch", m_engine_heavy_switch);
	m_whine_speed = ini->read_if_exists<float>("car_sound", "whine_speed", m_whine_speed);
	m_whine_exterior_volume = ini->read_if_exists<float>("car_sound", "whine_exterior_volume", m_whine_exterior_volume);
	m_whine_reverse = ini->read_if_exists<bool>("car_sound", "whine_reverse", m_whine_reverse);

	auto ClampFinite = [](float& Value, float Low, float High, float Fallback)
	{
		if (!std::isfinite(Value))
			Value = Fallback;
		clamp(Value, Low, High);
	};

	ClampFinite(m_engine_sound_scale_min, 0.01f, 10.f, 0.5f);
	ClampFinite(m_engine_sound_scale_max, m_engine_sound_scale_min, 20.f, 1.5f);
	ClampFinite(m_engine_sound_scale_factor, 0.01f, 10.f, 1.f);
	ClampFinite(m_engine_idle_volume, 0.f, 1.f, 0.8f);
	ClampFinite(m_sound_state_change_up, 0.f, 100.f, 3.f);
	ClampFinite(m_sound_state_change_down, 0.f, 100.f, 2.5f);
	ClampFinite(m_sound_idle_change_up, 0.f, 100.f, 25.f);
	ClampFinite(m_sound_idle_change_down, 0.f, 100.f, 0.6f);
	ClampFinite(m_sound_state_switch_factor, 0.f, 10.f, 3.f);
	ClampFinite(m_sound_state_after_switch, 0.f, 1.f, 0.f);
	ClampFinite(m_sound_switch_effect_power, -100.f, 100.f, 1.f);
	ClampFinite(m_sound_switch_effect_speed, 0.f, 100.f, 1.f);
	ClampFinite(m_sound_switch_effect_duration, 0.f, 100.f, 20.f);
	ClampFinite(m_turbo_scale_min, 0.01f, 10.f, 0.2f);
	ClampFinite(m_turbo_scale_max, m_turbo_scale_min, 20.f, 1.f);
	ClampFinite(m_turbo_scale_speed, 0.f, 10.f, 1.5f);
	ClampFinite(m_valve_switch_volume, 0.f, 1.f, 0.8f);
	ClampFinite(m_blower_scale_min, 0.f, 10.f, 0.3f);
	ClampFinite(m_blower_rpm_scale, 0.f, 10.f, 0.9f);
	ClampFinite(m_blower_volume_min, 0.f, 1.f, 0.3f);
	ClampFinite(m_blower_inertia, 0.f, 1.f, 0.1f);
	ClampFinite(m_blower_sound_forward, 0.f, 1.f, 0.5f);
	ClampFinite(m_engine_state_scale_min, 0.f, 1.f, 0.8f);
	ClampFinite(m_engine_heavy_constant, -100.f, 100.f, 0.2f);
	ClampFinite(m_engine_heavy_rpm, -100.f, 100.f, 0.2f);
	ClampFinite(m_engine_heavy_switch, -100.f, 100.f, 0.f);
	ClampFinite(m_whine_speed, 0.01f, 1000.f, 50.f);
	ClampFinite(m_whine_exterior_volume, 0.f, 1.f, 1.f);
}

ref_sound* CCar::GetTireSound(u16 m1_id, u16 m2_id)
{
	switch (GetTireSurface(m1_id, m2_id))
	{
		case TireSurfaceDirt:	return &m_tire_sound_dirt;
		case TireSurfaceGrass:	return &m_tire_sound_grass;
		case TireSurfaceWater:	return &m_tire_sound_water;
		default:				return &m_tire_sound_road;
	}
}

ref_sound* CCar::GetTireSkidSound(u16 m1_id, u16 m2_id)
{
	switch (GetTireSurface(m1_id, m2_id))
	{
		case TireSurfaceDirt:	return &m_tire_skid_sound_dirt;
		case TireSurfaceGrass:	return &m_tire_skid_sound_grass;
		case TireSurfaceWater:	return &m_tire_skid_sound_water;
		default:				return &m_tire_skid_sound_road;
	}
}

void CCar::UpdateWheelTireSound(SWheel& Wheel)
{
	const u32 ContactAge = Device.dwTimeGlobal - Wheel.collision_params.last_contact_time;
	const bool WheelAirborne = ContactAge > 100;
	const bool WheelStopped = GetSpeedKmH() < 1.f && fabsf(Wheel.GetDriveAngleRate()) * Wheel.radius < 0.5f;
	if ((WheelAirborne || WheelStopped) && Wheel.tire_drive._p)
	{
		Wheel.UpdateSounds();
		Wheel.tire_drive.stop();
	}
}
