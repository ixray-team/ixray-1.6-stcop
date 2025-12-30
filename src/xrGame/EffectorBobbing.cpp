#include "StdAfx.h"
#include "EffectorBobbing.h"

#include "Actor.h"
#include "actor_defs.h"

constexpr const char* BOBBING_SECT = "bobbing_effector";
constexpr float CROUCH_FACTOR = 0.75f;
constexpr float SPEED_REMINDER = 5.0f;

float g_bobbing_factor = 1.0f;

CEffectorBobbing::CEffectorBobbing() : CEffectorCam(eCEBobbing, 10000.0f)
{
    Sprint.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "sprint_amplitude", 0.0f);
    Sprint.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "sprint_speed", 0.0f);

    Limp.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "limp_amplitude", 0.0f);
    Limp.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "limp_speed", 0.0f);

    Limp.Amplitude.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_limp_amplitude", 0.0f);
    Limp.Speed.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_limp_speed", 0.0f);

    SlowCrouch.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "slow_crouch_amplitude", 0.0f);
    SlowCrouch.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "slow_crouch_speed", 0.0f);

    SlowCrouch.Amplitude.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_slow_crouch_amplitude", 0.0f);
    SlowCrouch.Speed.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_slow_crouch_speed", 0.0f);

    Crouch.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "crouch_amplitude", 0.0f);
    Crouch.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "crouch_speed", 0.0f);

    Crouch.Amplitude.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_crouch_amplitude", 0.0f);
    Crouch.Speed.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_crouch_speed", 0.0f);

    Walk.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "walk_amplitude", 0.0f);
    Walk.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "walk_speed", 0.0f);

    Walk.Amplitude.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_walk_amplitude", 0.0f);
    Walk.Speed.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_walk_speed", 0.0f);

    Run.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "run_amplitude", 0.0f);
    Run.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "run_speed", 0.0f);

    Run.Amplitude.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_run_amplitude", 0.0f);
    Run.Speed.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_run_speed", 0.0f);

    m_fAmplitudeDelta = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "amplitude_delta", 1.0f);
}

void CEffectorBobbing::SetState(u32 mstate, bool limping, bool ZoomMode)
{
    dwMState = mstate;
    is_limping = limping;
    m_bZoomMode = ZoomMode;
}

BOOL CEffectorBobbing::ProcessCam(SCamEffectorInfo& info)
{
    fTime += Device.fTimeDelta;

    if (dwMState & ACTOR_DEFS::mcAnyMove)
    {
        if (fReminderFactor < 1.0f)
        {
            fReminderFactor += SPEED_REMINDER * Device.fTimeDelta;
        }
        else
        {
            fReminderFactor = 1.0f;
        }
    }
    else
    {
        if (fReminderFactor > 0.0f)
        {
            fReminderFactor -= SPEED_REMINDER * Device.fTimeDelta;
        }
        else
        {
            fReminderFactor = 0.0f;
        }
    }

    if (!fsimilar(fReminderFactor, 0.0f))
    {
        Fmatrix M;
        M.identity();
        M.j.set(info.n);
        M.k.set(info.d);
        M.i.crossproduct(info.n, info.d);
        M.c.set(info.p);

        float A = 0.0f, ST = 0.0f;

        SelectBobbingParams(m_bZoomMode, is_limping, fOldPhase, fOldFreq, fOldAmp, dwMState, fTime, A, ST);

        float _sinA = std::abs(std::sin(ST) * A) * fReminderFactor * g_bobbing_factor;
        float _cosA = std::cos(ST) * A * fReminderFactor * g_bobbing_factor;

        // apply footstep bobbing effect
        Fvector dangle;

        info.p.y += _sinA;
        dangle.x = _cosA;
        dangle.z = _cosA;
        dangle.y = _sinA;

        Fmatrix	R;
        R.setHPB(dangle.x, dangle.y, dangle.z);

        Fmatrix	mR;
        mR.mul(M, R);

        info.d.set(mR.k);
        info.n.set(mR.j);
    }

    return TRUE;
}

void CEffectorBobbing::SelectBobbingParams(bool zoom_mode, bool is_limping, float& old_phase, float& old_freq, float& old_amp, u32 mstate, float time, float& amp, float& st)
{
    float amp_tmp = 0.0f, freq_tmp = 0.0f;

    if ((mstate & mcSprint) > 0)
    {
        amp_tmp = Sprint.Amplitude.Default;
        freq_tmp = Sprint.Speed.Default;
    }
    else if (is_limping)
    {
        if (zoom_mode)
        {
            amp_tmp = Limp.Amplitude.Zoom;
            freq_tmp = Limp.Speed.Zoom;
        }
        else
        {
            amp_tmp = Limp.Amplitude.Default;
            freq_tmp = Limp.Speed.Default;
        }
    }
    else if (((mstate & mcCrouch) > 0) && ((mstate & mcAccel) > 0))
    {
        if (zoom_mode)
        {
            amp_tmp = SlowCrouch.Amplitude.Zoom;
            freq_tmp = SlowCrouch.Speed.Zoom;
        }
        else
        {
            amp_tmp = SlowCrouch.Amplitude.Default;
            freq_tmp = SlowCrouch.Speed.Default;
        }
    }
    else if ((mstate & mcCrouch) > 0)
    {
        if (zoom_mode)
        {
            amp_tmp = Crouch.Amplitude.Zoom;
            freq_tmp = Crouch.Speed.Zoom;
        }
        else
        {
            amp_tmp = Crouch.Amplitude.Default;
            freq_tmp = Crouch.Speed.Default;
        }
    }
    else if ((mstate & mcAccel) > 0)
    {
        if (zoom_mode)
        {
            amp_tmp = Walk.Amplitude.Zoom;
            freq_tmp = Walk.Speed.Zoom;
        }
        else
        {
            amp_tmp = Walk.Amplitude.Default;
            freq_tmp = Walk.Speed.Default;
        }
    }
    else
    {
        if (zoom_mode)
        {
            amp_tmp = Run.Amplitude.Zoom;
            freq_tmp = Run.Speed.Zoom;
        }
        else
        {
            amp_tmp = Run.Amplitude.Default;
            freq_tmp = Run.Speed.Default;
        }
    }

    float phase_tmp = old_phase;

    if (freq_tmp != old_freq)
    {
        phase_tmp = (old_freq - freq_tmp) * time + old_phase;
        if (phase_tmp > 2.0f * PI)
        {
            phase_tmp -= floor(phase_tmp / (2.0f * PI)) * 2.0f * PI;
        }
        else if (phase_tmp < 0.0f)
        {
            phase_tmp += ceil(abs(phase_tmp) / (2.0f * PI)) * 2.0f * PI;
        }

        old_freq = freq_tmp;
        old_phase = phase_tmp;
    }

    if (amp_tmp != old_amp)
    {
        float dt = m_fAmplitudeDelta * Device.fTimeDelta;
        if (amp_tmp > old_amp)
        {
            if (amp_tmp - old_amp > dt)
            {
                amp_tmp = old_amp + dt;
            }
        }
        else if (old_amp - amp_tmp > dt)
        {
            amp_tmp = old_amp - dt;
        }

        old_amp = amp_tmp;
    }

    amp = amp_tmp;
    st = time * freq_tmp + phase_tmp;
}