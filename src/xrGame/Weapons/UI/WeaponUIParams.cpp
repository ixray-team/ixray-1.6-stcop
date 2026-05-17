#include "StdAfx.h"
#include "Weapon.h"

static float normalize(float val, float min, float max)
{
	float d = 100.0f * (val - min) / (max - min);
	return d < 0.0f ? 0.0f : d;
}

float CWeapon::GetRPM() const
{
	float rpm = fOneShotTimeSaved;
	return normalize(rpm, 0.0f, 1500.0f);
}

float CWeapon::GetDamage() const
{
	float hit_power = fvHitPower[g_SingleGameDifficulty];

	int cnt = 0;
	int cnt_ap = 0;
	float k = 0.0f;
	float ap = 0.0f;

	for (const auto& ammo : m_ammoTypes)
	{
		if (pSettings->line_exist(ammo, "k_hit"))
		{
			cnt++;
			k += pSettings->r_float(ammo, "k_hit");
		}

		if (pSettings->line_exist(ammo, "k_ap"))
		{
			cnt_ap++;
			ap += pSettings->r_float(ammo, "k_ap");
		}
	}

	if (cnt > 0)
	{
		k /= cnt;
	}
	else
	{
		k = 0.0f;
	}

	if (cnt_ap > 0)
	{
		ap /= cnt;
		ap = normalize(ap, -0.3f, 0.75f) / 100.0f;
	}
	else
	{
		ap = 1.0f;
	}

	hit_power = hit_power * k * ap;

	return normalize(hit_power, 0.1f, 1.5f);
}

float CWeapon::GetDamageMP() const
{
	float hit_power = fvHitPower[g_SingleGameDifficulty];
	hit_power *= 100.0f;

	return clampr(hit_power, 1.0f, hit_power);
}

float CWeapon::GetHandling() const
{
	float crosshair_inertion = m_crosshair_inertion;

	crosshair_inertion = 11.9f - crosshair_inertion;
	crosshair_inertion = normalize(crosshair_inertion, 0.0f, 10.5f);

	float pdm = m_pdm.m_fPDM_disp_base;

	pdm = 3.5f - pdm;
	pdm = normalize(pdm, 0.5f, 3.2f);

	float cam_disp = rad2deg(cam_recoil.Dispersion);

	cam_disp = std::sqrt(cam_disp);
	cam_disp = normalize(2.25f - cam_disp, 0.2f, 1.8f);

	return (pdm * 2.0f + crosshair_inertion + cam_disp * 3.0f) / 6.0f;
}

float CWeapon::GetAccuracy() const
{
	float fire_dispersion_base = rad2deg(fireDispersionBase);

	fire_dispersion_base = 0.85f - fire_dispersion_base;
	fire_dispersion_base = normalize(fire_dispersion_base, 0.3f, 0.8f);

	float pdm = m_pdm.m_fPDM_disp_base;

	pdm = 3.5f - pdm;
	pdm = normalize(pdm, 0.5f, 3.2f);

	return (pdm + fire_dispersion_base * 2.0f) / 3.0f;
}