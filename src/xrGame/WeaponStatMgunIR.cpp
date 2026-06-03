#include "stdafx.h"
#include "WeaponStatMgun.h"
#include "../xrEngine/xr_level_controller.h"
#include "../xrEngine/xr_input.h"

void CWeaponStatMgun::OnMouseMove(int dx, int dy)
{
	if (Remote())	
		return;

	float scale = psMouseSens * psMouseSensScale / 50.f;
	float h, p;
	m_destEnemyDir.getHP(h, p);

	if (dx) 
	{
		float d = float(dx) * scale;
		h -= d;
		SetDesiredDir(h, p);
	}

	if (dy) 
	{
		float d = (psMouseInvert ? -1 : 1) * float(dy) * scale * 3.f / 4.f;
		p -= d;
		SetDesiredDir(h, p);
	}
}

void CWeaponStatMgun::OnKeyboardPress(int dik)
{
	if (Remote())
		return;

	switch (get_binded_action(dik))
	{
	case kWPN_FIRE:
		FireStart();
		break;
	};
}

void CWeaponStatMgun::OnKeyboardRelease(int dik)
{
	if (Remote())
		return;

	switch (get_binded_action(dik))
	{
	case kWPN_FIRE:
		FireEnd();
		break;
	};
}

void CWeaponStatMgun::OnKeyboardHold(int dik)
{

}

void CWeaponStatMgun::OnGamepadAxisMove(int id, Fvector2 value)
{
	if (Remote())
		return;

	// right stick
	if (id == 1)
	{
		float scale = psGamepadSens * Device.fTimeDelta * psMouseSensScale;
		float h, p;
		m_destEnemyDir.getHP(h, p);

		if (value.x)
		{
			float d = value.x * scale * 8;
			h -= d;
			SetDesiredDir(h, p);
		}

		if (value.y)
		{
			float d = (psGamepadInvert ? -1 : 1) * value.y * scale * 3.f / 4.f;
			d *= 8;
			p -= d;
			SetDesiredDir(h, p);
		}
	}
	// triggers
	else if (id == 2)
	{
		if (!fis_zero(value.y))
		{
			FireStart();
		}
		else if (pInput->GetControllerMode())
		{
			FireEnd();
		}
	}
}

void CWeaponStatMgun::OnGyroscopeMove(Fvector3 value)
{
	if (Remote())
		return;

	float scale = Device.fTimeDelta * psMouseSensScale;
	float h, p;
	m_destEnemyDir.getHP(h, p);

	if (value.y)
	{
		float d = (psGyroscopeInvertX ? -1 : 1) * -value.y * scale;
		h -= d;
		SetDesiredDir(h, p);
	}

	if (value.x)
	{
		float d = (psGyroscopeInvertY ? -1 : 1) * -value.x * scale;
		p -= d;
		SetDesiredDir(h, p);
	}
}
