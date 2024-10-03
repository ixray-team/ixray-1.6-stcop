#include "stdafx.h"
#include "EffectorBobbing.h"


#include "Actor.h"
#include "actor_defs.h"


#define BOBBING_SECT "bobbing_effector"

#define CROUCH_FACTOR	0.75f
#define SPEED_REMINDER	5.f 



//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CEffectorBobbing::CEffectorBobbing() : CEffectorCam(eCEBobbing,10000.f)
{
	fTime			= 0.f;
	fReminderFactor	= 0.f;
	is_limping		= false;

    fOldPhase = 0.f;
    fOldFreq = 0.f;
    fOldAmp = 0.f;

    Sprint.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "sprint_amplitude", 0.f);
    Sprint.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "sprint_speed", 0.f);

    Limp.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "limp_amplitude", 0.f);
    Limp.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "limp_speed", 0.f);

    Limp.Amplitude.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_limp_amplitude", 0.f);
    Limp.Speed.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_limp_speed", 0.f);

    SlowCrouch.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "slow_crouch_amplitude", 0.f);
    SlowCrouch.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "slow_crouch_speed", 0.f);

    SlowCrouch.Amplitude.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_slow_crouch_amplitude", 0.f);
    SlowCrouch.Speed.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_slow_crouch_speed", 0.f);

    Crouch.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "crouch_amplitude", 0.f);
    Crouch.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "crouch_speed", 0.f);

    Crouch.Amplitude.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_crouch_amplitude", 0.f);
    Crouch.Speed.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_crouch_speed", 0.f);

    Walk.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "walk_amplitude", 0.f);
    Walk.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "walk_speed", 0.f);

    Walk.Amplitude.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_walk_amplitude", 0.f);
    Walk.Speed.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_walk_speed", 0.f);

    Run.Amplitude.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "run_amplitude", 0.f);
    Run.Speed.Default = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "run_speed", 0.f);

    Run.Amplitude.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_run_amplitude", 0.f);
    Run.Speed.Zoom = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "zoom_run_speed", 0.f);

    m_fAmplitudeDelta = READ_IF_EXISTS(pSettings, r_float, BOBBING_SECT, "amplitude_delta", 1.f);
}

CEffectorBobbing::~CEffectorBobbing	()
{
}

void CEffectorBobbing::SetState(u32 mstate, bool limping, bool ZoomMode){
	dwMState		= mstate;
	is_limping		= limping;
	m_bZoomMode		= ZoomMode;
}


BOOL CEffectorBobbing::ProcessCam(SCamEffectorInfo& info)
{
	fTime += Device.fTimeDelta;
	if (dwMState&ACTOR_DEFS::mcAnyMove)
	{
		if (fReminderFactor<1.f)
			fReminderFactor += SPEED_REMINDER*Device.fTimeDelta;
		else
			fReminderFactor = 1.f;
	}
	else
	{
		if (fReminderFactor>0.f)
			fReminderFactor -= SPEED_REMINDER*Device.fTimeDelta;
		else
			fReminderFactor = 0.f;
	}

	if (!fsimilar(fReminderFactor,0))
	{
		Fmatrix		M;
		M.identity	();
		M.j.set		(info.n);
		M.k.set		(info.d);
		M.i.crossproduct(info.n, info.d);
		M.c.set		(info.p);

		float A = 0.f, ST = 0.f;

        bool isGuns = EngineExternal().isModificationGunslinger();

        if (!isGuns)
        {
			float k = ((dwMState& ACTOR_DEFS::mcCrouch) ? CROUCH_FACTOR : 1.f);
			
            if (isActorAccelerated(dwMState, m_bZoomMode))
            {
                A = Run.Amplitude.Default * k;
                ST = Run.Speed.Default * fTime * k;
            }
            else if (is_limping)
            {
                A = Limp.Amplitude.Default * k;
                ST = Limp.Speed.Default * fTime * k;
            }
            else
            {
                A = Walk.Amplitude.Default * k;
                ST = Walk.Speed.Default * fTime * k;
            }
        }
        else
            SelectBobbingParams(m_bZoomMode, is_limping, fOldPhase, fOldFreq, fOldAmp, dwMState, fTime, A, ST);

		float _sinA	= _abs(_sin(ST)*A)*fReminderFactor;
		float _cosA	= _cos(ST)*A*fReminderFactor;

		// apply footstep bobbing effect
		Fvector dangle;

		info.p.y	+=	_sinA;
		dangle.x	=	_cosA;
		dangle.z	=	_cosA;
		dangle.y	=	_sinA;

		Fmatrix		R;
		R.setHPB	(dangle.x,dangle.y,dangle.z);

		Fmatrix		mR;
		mR.mul		(M,R);
		
		info.d.set	(mR.k);
		info.n.set	(mR.j);
	}

	return TRUE;
}

void CEffectorBobbing::SelectBobbingParams(bool zoom_mode, bool is_limping, float& old_phase, float& old_freq, float& old_amp, u32 mstate, float time, float& amp, float& st)
{
    float amp_tmp = 0.f, freq_tmp = 0.f;

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
        if (phase_tmp > 2.f * PI)
            phase_tmp -= floor(phase_tmp / (2.f * PI)) * 2.f * PI;
        else if (phase_tmp < 0.f)
            phase_tmp += ceil(abs(phase_tmp) / (2.f * PI)) * 2.f * PI;

        old_freq = freq_tmp;
        old_phase = phase_tmp;
    }

    if (amp_tmp != old_amp)
    {
        float dt = m_fAmplitudeDelta * Device.fTimeDelta;
        if (amp_tmp > old_amp)
        {
            if (amp_tmp - old_amp > dt)
                amp_tmp = old_amp + dt;
        }
        else
        {
            if (old_amp - amp_tmp > dt)
                amp_tmp = old_amp - dt;
        }

        old_amp = amp_tmp;
    }

    amp = amp_tmp;
    st = time * freq_tmp + phase_tmp;
}