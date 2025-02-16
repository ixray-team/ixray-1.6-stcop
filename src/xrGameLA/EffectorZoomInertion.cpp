// EffectorZoomInertion.cpp: инерция(покачивания) оружия в режиме
//							 приближения
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "EffectorZoomInertion.h"


#define EFFECTOR_ZOOM_SECTION "zoom_inertion_effector"
#define CAMERA_MOVE_DELTA_TIME (0.150f)   // delay to react when camera stopped (to compensate for inconsistent camera angular distance between frames on high FPS)

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CEffectorZoomInertion::CEffectorZoomInertion	() : CEffectorCam(eCEZoom,100000.f)
{
	Load();
	SetRndSeed		(Device.dwTimeContinual);
	m_dwTimePassed	= (u32)-1;
	m_fTimeCameraMove = 0.f;
	m_bCameraMoving = false;
	m_enabled = false;
}

CEffectorZoomInertion::~CEffectorZoomInertion	()
{

}

template <class T>
IC T LoadParamInternal(LPCSTR section, LPCSTR prefix, LPCSTR param, T (CInifile::*method)(LPCSTR, LPCSTR)const, T defVal)
{
	static string256 full_name;
	xr_strconcat(full_name, prefix, param);
	if (pSettings->line_exist(section, full_name))
	{
		return (pSettings->*method)(section, full_name);
	}
	else if (pSettings->line_exist(EFFECTOR_ZOOM_SECTION, param))
	{
		return (pSettings->*method)(EFFECTOR_ZOOM_SECTION, param);
	}
	return defVal;
}

void CEffectorZoomInertion::LoadParams(LPCSTR section, LPCSTR prefix)
{
	m_fCameraSpeedThreshold	= LoadParam<float>(section, prefix, "camera_speed_threshold",	1.f);
	m_fDispEpsilon			= LoadParam<float>(section, prefix, "disp_epsilon",				0.1f);
	m_fDispMin				= LoadParam<float>(section, prefix, "disp_min",					0.0f);
	m_fZoomAimingDispK		= LoadParam<float>(section, prefix, "zoom_aim_disp_k",			1.f);
	m_fDispHorzCoef			= LoadParam<float>(section, prefix, "disp_horz_koef",			1.f);
	m_dwDeltaTime			= LoadParam<u32>  (section, prefix, "delta_time",				1000);
}

template <>
float CEffectorZoomInertion::LoadParam(LPCSTR section, LPCSTR prefix, LPCSTR param, float defVal)
{
	return LoadParamInternal(section, prefix, param, &CInifile::r_float, defVal);
}

template <>
u32 CEffectorZoomInertion::LoadParam(LPCSTR section, LPCSTR prefix, LPCSTR param, u32 defVal)
{
	return LoadParamInternal(section, prefix, param, &CInifile::r_u32, defVal);
}

void CEffectorZoomInertion::Load()
{
	LoadParams(EFFECTOR_ZOOM_SECTION, "");

	m_fDispRadius		= m_fOldDispRadius = m_fDispMin;

	m_vCurrentPoint.set(0.f,0.f,0.f);
	m_vTargetPoint.set(0.f,0.f,0.f);
	m_vLastPoint.set(0.f,0.f,0.f);
	m_vOldCameraDir.set(0.f,0.f,0.f);
}

void	CEffectorZoomInertion::Init(CWeaponMagazined*	pWeapon)
{
	if (!pWeapon) return;

	LoadParams(*pWeapon->cNameSect(), "ezi_");
}

void CEffectorZoomInertion::Enable(bool flag, float rotateTime)
{
	if (flag && !m_enabled)
	{
		m_enabled = true;
		if (!IsFirstUpdate())
		{
			CalcNextPoint();
		}
	}
	else if (!flag && m_enabled)
	{
		m_dwTimePassed = 0;
		m_vLastPoint = m_vCurrentPoint;
		m_vTargetPoint.set(0.f, 0.f, 0.f);
		m_dwCenterDeltaTime = !fis_zero(rotateTime) ? (u32)(rotateTime * 1000) : m_dwDeltaTime;
		m_enabled = false;
	}
}

void CEffectorZoomInertion::SetParams	(float disp)
{
	m_fDispRadius = disp * m_fZoomAimingDispK;
	if (m_fDispRadius < m_fDispMin) 
		m_fDispRadius = m_fDispMin;

	if (IsFirstUpdate())
	{
		m_fOldDispRadius = m_fDispRadius;
	}
	else if (!fsimilar(m_fOldDispRadius, m_fDispRadius, m_fDispEpsilon) && m_enabled && !IsCameraMoving())
	{
		// Msg("EZI: Disp changed. Old: %.2f, new: %.2f, eps: %.5f", m_fOldDispRadius, m_fDispRadius, m_fDispEpsilon);
		CalcNextPoint();
		m_fOldDispRadius = m_fDispRadius;
	}
}


void CEffectorZoomInertion::CalcNextPoint		()
{
	m_dwTimePassed = 0;
	m_vLastPoint = m_vCurrentPoint;

	float vertMax = m_fDispRadius/2.f;
	float horzMax = vertMax * m_fDispHorzCoef;
	m_vTargetPoint.x = m_Random.randF(-horzMax, horzMax);
	m_vTargetPoint.y = m_Random.randF(-vertMax, vertMax);

	//Msg("EZI: Next point (%.4f, %.4f)", m_vTargetPoint.x, m_vTargetPoint.y);
}

void CEffectorZoomInertion::ApplyPoint(Fvector& dest, const Fvector& pt)
{
	float h, p;
	dest.getHP(h, p);
	dest.setHP(h + pt.x, p + pt.y);
}

static float SmoothStep(float from, float to, float t)
{
	clamp(t, 0.f, 1.f);
	t = -2.f * t * t * t + 3.f * t * t;
	return to * t + from * (1.f - t);
}

//определяем двигал ли прицелом актер
bool CEffectorZoomInertion::UpdateCameraMoved(const Fvector& camDir, bool& justStopped)
{
	justStopped = false;

	// check camera speed only once in a while
	if (m_fTimeCameraMove < CAMERA_MOVE_DELTA_TIME)
	{
		m_fTimeCameraMove += Device.fTimeDelta;
	}
	else
	{
		Fvector lastDir = m_vOldCameraDir, curDir = camDir;
		float dist = camDir.distance_to(m_vOldCameraDir);
		float cameraSpeed = camDir.distance_to(m_vOldCameraDir) / m_fTimeCameraMove;

		if (cameraSpeed > m_fCameraSpeedThreshold)
		{
			// camera moved
			if (!m_bCameraMoving)
			{
				// Msg("EZI: Camera started moving with speed %.3f (max %.3f), dist: %.4f, time: %.4f, dir: (%.4f,%.4f,%.4f)->(%.4f,%.4f,%.4f)",
				//	cameraSpeed, m_fCameraSpeedThreshold, dist, m_fTimeCameraMove, lastDir.x, lastDir.y, lastDir.z, curDir.x, curDir.y, curDir.z);

				m_bCameraMoving = true;
			}
		}
		else if (m_bCameraMoving)
		{
			//Msg("EZI: Camera stopped moving: speed %.3f, dist: %.4f, fDelta: %.4f, dir: (%.4f,%.4f,%.4f)->(%.4f,%.4f,%.4f)",
			//	cameraSpeed, dist, m_fTimeCameraMove, lastDir.x, lastDir.y, lastDir.z, curDir.x, curDir.y, curDir.z);

			m_bCameraMoving = false;
			justStopped = true;
		}

		m_fTimeCameraMove = 0.f;
		m_vOldCameraDir = camDir;
	}

	return m_bCameraMoving;
}

BOOL CEffectorZoomInertion::ProcessCam(SCamEffectorInfo& info)
{
	if (!m_enabled)
	{
		// transition to center
		if (m_dwTimePassed < m_dwCenterDeltaTime)
		{
			m_vCurrentPoint.lerp(m_vLastPoint, m_vTargetPoint, SmoothStep(0.f, 1.f, float(m_dwTimePassed)/m_dwCenterDeltaTime));
			ApplyPoint(info.d, m_vCurrentPoint);
			m_dwTimePassed += Device.dwTimeDelta;
		}
		return TRUE;
	}

	// check if camera is moving
	bool justStopped;
	if (UpdateCameraMoved(info.d, justStopped))
	{
		ApplyPoint(info.d, m_vCurrentPoint);
		return TRUE;
	}
	else if (justStopped)
	{
		// force next point after camera stopped moving
		m_dwTimePassed = m_dwDeltaTime;
	}

	if (m_dwTimePassed >= m_dwDeltaTime)
	{
		CalcNextPoint();
	}

	m_vCurrentPoint.lerp(m_vLastPoint, m_vTargetPoint, SmoothStep(0.f, 1.f, float(m_dwTimePassed)/m_dwDeltaTime));

	ApplyPoint(info.d, m_vCurrentPoint);

	m_dwTimePassed += Device.dwTimeDelta;

	return TRUE;
}
