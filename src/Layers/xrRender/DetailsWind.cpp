#include "stdafx.h"
#include "DetailsWind.h"
#include <cmath>

extern int   ps_wind_enabled;
extern int   ps_wind_vanilla;
extern int   ps_wind_mode;
extern float ps_wind_blend;
extern float ps_wind_blend_current;
extern float ps_wind_noise_scale;
extern float ps_wind_noise_speed;
extern float ps_wind_noise_angle;
extern int   ps_wind_xz_enabled;
extern int   ps_wind_swirl_enabled;
extern int   ps_wind_xz1_on;
extern float ps_wind_xz1_scale_min;
extern float ps_wind_xz1_scale_max;
extern float ps_wind_xz1_int_min;
extern float ps_wind_xz1_int_max;
extern float ps_wind_xz1_con_min;
extern float ps_wind_xz1_con_max;
extern float ps_wind_xz1_spd_min;
extern float ps_wind_xz1_spd_max;
extern float ps_wind_xz1_ang_min;
extern float ps_wind_xz1_ang_max;
extern int   ps_wind_xz2_on;
extern float ps_wind_xz2_scale_min;
extern float ps_wind_xz2_scale_max;
extern float ps_wind_xz2_int_min;
extern float ps_wind_xz2_int_max;
extern float ps_wind_xz2_con_min;
extern float ps_wind_xz2_con_max;
extern float ps_wind_xz2_spd_min;
extern float ps_wind_xz2_spd_max;
extern float ps_wind_xz2_ang_min;
extern float ps_wind_xz2_ang_max;
extern int   ps_wind_xz3_on;
extern float ps_wind_xz3_scale_min;
extern float ps_wind_xz3_scale_max;
extern float ps_wind_xz3_int_min;
extern float ps_wind_xz3_int_max;
extern float ps_wind_xz3_con_min;
extern float ps_wind_xz3_con_max;
extern float ps_wind_xz3_spd_min;
extern float ps_wind_xz3_spd_max;
extern float ps_wind_xz3_ang_min;
extern float ps_wind_xz3_ang_max;
extern float ps_wind_sw_scale_min;
extern float ps_wind_sw_scale_max;
extern float ps_wind_sw_int_min;
extern float ps_wind_sw_int_max;
extern float ps_wind_sw_con_min;
extern float ps_wind_sw_con_max;
extern float ps_wind_sw_spd_min;
extern float ps_wind_sw_spd_max;
extern float ps_wind_sw_ang_min;
extern float ps_wind_sw_ang_max;

static constexpr float DEG2RAD = 3.14159265f / 180.0f;

float CDetailWind::Hash(float x, float y)
{
	float px = x * 123.34f + y * 456.21f;
	px = px - std::floor(px);
	float d = px * (px + 45.32f);
	float r = d * d + x * y;
	r = r - std::floor(r);
	return r;
}

float CDetailWind::Noise(float x, float y)
{
	float ix = std::floor(x), iy = std::floor(y);
	float fx = x - ix, fy = y - iy;
	float ux = fx * fx * fx * (fx * (fx * 6.0f - 15.0f) + 10.0f);
	float uy = fy * fy * fy * (fy * (fy * 6.0f - 15.0f) + 10.0f);

	float gx0, gy0, gx1, gy1, gx2, gy2, gx3, gy3;
	float n;
	n = Hash(ix, iy) * 6.2831853f; gx0 = std::cos(n); gy0 = std::sin(n);
	n = Hash(ix + 1, iy) * 6.2831853f; gx1 = std::cos(n); gy1 = std::sin(n);
	n = Hash(ix, iy + 1) * 6.2831853f; gx2 = std::cos(n); gy2 = std::sin(n);
	n = Hash(ix + 1, iy + 1) * 6.2831853f; gx3 = std::cos(n); gy3 = std::sin(n);

	float v00 = gx0 * fx + gy0 * fy;
	float v10 = gx1 * (fx - 1) + gy1 * fy;
	float v01 = gx2 * fx + gy2 * (fy - 1);
	float v11 = gx3 * (fx - 1) + gy3 * (fy - 1);

	float a = v00 + ux * (v10 - v00);
	float b = v01 + ux * (v11 - v01);
	return (a + uy * (b - a)) * 0.5f + 0.5f;
}

static float fbm2(float x, float y, float seed)
{
	float v = CDetailWind::Noise(x + seed, y + seed * 1.7f);
	v += CDetailWind::Noise(x * 2.17f + seed * 3.7f, y * 2.17f + seed * 2.3f) * 0.5f;
	return v * 0.6667f;
}

static void packLayer(Fvector4& out, Fvector4& dir, float scale, float intensity, float contrast, float speed, float angleDeg, float enabled)
{
	float rad = angleDeg * DEG2RAD;
	out.set(scale, intensity * enabled, contrast, speed);
	dir.set(std::cos(rad), std::sin(rad), enabled, 0.0f);
}

void CDetailWind::ComputeConstants(Constants& out)
{
	if (!ps_wind_enabled)
	{
		out.global.set(0, 0, 0, ps_wind_vanilla ? 1.0f : 0.0f);
		out.xz1.set(0, 0, 0, 0); out.xz1_dir.set(0, 0, 0, 0);
		out.xz2.set(0, 0, 0, 0); out.xz2_dir.set(0, 0, 0, 0);
		out.xz3.set(0, 0, 0, 0); out.xz3_dir.set(0, 0, 0, 0);
		out.swirl.set(0, 0, 0, 0); out.swirl_dir.set(0, 0, 0, 0);
		return;
	}

	const float on = 1.0f;
	const float xzOn = ps_wind_xz_enabled ? 1.0f : 0.0f;
	const float swOn = ps_wind_swirl_enabled ? 1.0f : 0.0f;

	float blend = ps_wind_blend;
	if (ps_wind_mode == 1)
	{
		const float vt = (float)RDEVICE.dwTimeGlobal * 0.001f * ps_wind_noise_speed;
		const float nv = Noise(vt * std::cos(ps_wind_noise_angle * DEG2RAD) * 3.7f,
		                       vt * std::sin(ps_wind_noise_angle * DEG2RAD) * 3.7f + 7.0f);
		blend = ps_wind_blend * nv;
	}
	blend = std::clamp(blend, 0.0f, 1.0f);
	ps_wind_blend_current = blend;

	auto L = [blend](float mn, float mx) { return mn + (mx - mn) * blend; };

	out.global.set(on, xzOn, swOn, ps_wind_vanilla ? 1.0f : 0.0f);

	packLayer(out.xz1, out.xz1_dir,
		L(ps_wind_xz1_scale_min, ps_wind_xz1_scale_max),
		L(ps_wind_xz1_int_min, ps_wind_xz1_int_max) * (ps_wind_xz1_on ? 1.0f : 0.0f),
		L(ps_wind_xz1_con_min, ps_wind_xz1_con_max),
		L(ps_wind_xz1_spd_min, ps_wind_xz1_spd_max),
		L(ps_wind_xz1_ang_min, ps_wind_xz1_ang_max),
		xzOn * (ps_wind_xz1_on ? 1.0f : 0.0f));
	packLayer(out.xz2, out.xz2_dir,
		L(ps_wind_xz2_scale_min, ps_wind_xz2_scale_max),
		L(ps_wind_xz2_int_min, ps_wind_xz2_int_max) * (ps_wind_xz2_on ? 1.0f : 0.0f),
		L(ps_wind_xz2_con_min, ps_wind_xz2_con_max),
		L(ps_wind_xz2_spd_min, ps_wind_xz2_spd_max),
		L(ps_wind_xz2_ang_min, ps_wind_xz2_ang_max),
		xzOn * (ps_wind_xz2_on ? 1.0f : 0.0f));
	packLayer(out.xz3, out.xz3_dir,
		L(ps_wind_xz3_scale_min, ps_wind_xz3_scale_max),
		L(ps_wind_xz3_int_min, ps_wind_xz3_int_max) * (ps_wind_xz3_on ? 1.0f : 0.0f),
		L(ps_wind_xz3_con_min, ps_wind_xz3_con_max),
		L(ps_wind_xz3_spd_min, ps_wind_xz3_spd_max),
		L(ps_wind_xz3_ang_min, ps_wind_xz3_ang_max),
		xzOn * (ps_wind_xz3_on ? 1.0f : 0.0f));
	packLayer(out.swirl, out.swirl_dir,
		L(ps_wind_sw_scale_min, ps_wind_sw_scale_max),
		L(ps_wind_sw_int_min, ps_wind_sw_int_max),
		L(ps_wind_sw_con_min, ps_wind_sw_con_max),
		L(ps_wind_sw_spd_min, ps_wind_sw_spd_max),
		L(ps_wind_sw_ang_min, ps_wind_sw_ang_max),
		swOn);
}

void CDetailWind::FillPreview(u8* rgba, u32 size, float zoom, u32 timeMs)
{
	if (!ps_wind_enabled)
	{
		memset(rgba, 0, (size_t)size * size * 4);
		return;
	}

	const float t = (float)timeMs * 0.001f;
	const float z = std::max(zoom, 0.1f);
	const float xzOn = ps_wind_xz_enabled ? 1.0f : 0.0f;
	const float swOn = ps_wind_swirl_enabled ? 1.0f : 0.0f;

	float blend = ps_wind_blend;
	if (ps_wind_mode == 1)
	{
		const float vt = t * ps_wind_noise_speed;
		const float nv = Noise(vt * std::cos(ps_wind_noise_angle * DEG2RAD) * 3.7f,
		                       vt * std::sin(ps_wind_noise_angle * DEG2RAD) * 3.7f + 7.0f);
		blend = ps_wind_blend * nv;
	}
	blend = std::clamp(blend, 0.0f, 1.0f);
	ps_wind_blend_current = blend;
	auto L = [blend](float mn, float mx) { return mn + (mx - mn) * blend; };

	const float s1 = L(ps_wind_xz1_scale_min, ps_wind_xz1_scale_max);
	const float i1 = L(ps_wind_xz1_int_min, ps_wind_xz1_int_max);
	const float c1 = L(ps_wind_xz1_con_min, ps_wind_xz1_con_max);
	const float sp1 = L(ps_wind_xz1_spd_min, ps_wind_xz1_spd_max);
	const float a1 = L(ps_wind_xz1_ang_min, ps_wind_xz1_ang_max) * DEG2RAD;
	const float s2 = L(ps_wind_xz2_scale_min, ps_wind_xz2_scale_max);
	const float i2 = L(ps_wind_xz2_int_min, ps_wind_xz2_int_max);
	const float c2 = L(ps_wind_xz2_con_min, ps_wind_xz2_con_max);
	const float sp2 = L(ps_wind_xz2_spd_min, ps_wind_xz2_spd_max);
	const float a2 = L(ps_wind_xz2_ang_min, ps_wind_xz2_ang_max) * DEG2RAD;
	const float s3 = L(ps_wind_xz3_scale_min, ps_wind_xz3_scale_max);
	const float i3 = L(ps_wind_xz3_int_min, ps_wind_xz3_int_max);
	const float c3 = L(ps_wind_xz3_con_min, ps_wind_xz3_con_max);
	const float sp3 = L(ps_wind_xz3_spd_min, ps_wind_xz3_spd_max);
	const float a3 = L(ps_wind_xz3_ang_min, ps_wind_xz3_ang_max) * DEG2RAD;
	const float ss = L(ps_wind_sw_scale_min, ps_wind_sw_scale_max);
	const float si = L(ps_wind_sw_int_min, ps_wind_sw_int_max);
	const float sc = L(ps_wind_sw_con_min, ps_wind_sw_con_max);
	const float ssp = L(ps_wind_sw_spd_min, ps_wind_sw_spd_max);
	const float sa = L(ps_wind_sw_ang_min, ps_wind_sw_ang_max) * DEG2RAD;

	for (u32 py = 0; py < size; ++py)
	{
		for (u32 px = 0; px < size; ++px)
		{
			float u = (float)px / size * z;
			float v = (float)py / size * z;

			float n1 = 0.5f, n2 = 0.5f, n3 = 0.5f, ns = 0.5f;

			if (xzOn && i1 > 0.001f)
			{
				float uu = u * s1 * 40.0f + std::cos(a1) * t * sp1;
				float vv = v * s1 * 40.0f + std::sin(a1) * t * sp1;
				n1 = fbm2(uu, vv, 0.0f);
				n1 = std::clamp((n1 - 0.5f) * (1.0f + c1 * 2.0f) + 0.5f, 0.0f, 1.0f);
			}
			if (xzOn && i2 > 0.001f)
			{
				float uu = u * s2 * 40.0f + std::cos(a2) * t * sp2;
				float vv = v * s2 * 40.0f + std::sin(a2) * t * sp2;
				n2 = fbm2(uu, vv, 7.3f);
				n2 = std::clamp((n2 - 0.5f) * (1.0f + c2 * 2.0f) + 0.5f, 0.0f, 1.0f);
			}
			if (xzOn && i3 > 0.001f)
			{
				float uu = u * s3 * 40.0f + std::cos(a3) * t * sp3;
				float vv = v * s3 * 40.0f + std::sin(a3) * t * sp3;
				n3 = fbm2(uu, vv, 13.7f);
				n3 = std::clamp((n3 - 0.5f) * (1.0f + c3 * 2.0f) + 0.5f, 0.0f, 1.0f);
			}
			if (swOn && si > 0.001f)
			{
				float uu = u * ss * 40.0f + std::cos(sa) * t * ssp;
				float vv = v * ss * 40.0f + std::sin(sa) * t * ssp;
				ns = fbm2(uu, vv, 21.0f);
				ns = std::clamp((ns - 0.5f) * (1.0f + sc * 2.0f) + 0.5f, 0.0f, 1.0f);
			}

			float val = n1 * i1 + n2 * i2 + n3 * i3;
			val /= std::max(i1 + i2 + i3, 0.01f);
			val = std::clamp(val, 0.0f, 1.0f);

			u8 c = (u8)(val * 255.0f);
			u8 sw = (u8)(ns * si * 255.0f);
			u32 idx = (py * size + px) * 4;
			rgba[idx] = c;
			rgba[idx + 1] = c;
			rgba[idx + 2] = sw;
			rgba[idx + 3] = 255;
		}
	}
}
