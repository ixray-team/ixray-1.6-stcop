#include "stdafx.h"

#include "xrDeflectorLight_Packed.h"
#include "CUDA/Vector3HW.h"
#include "embree_raytracing/EmbreeRayTrace.h"

#include "light_point.h"
#include "xrLC_GlobalData.h"
#include "xrFace.h"

void copy_color(hardware_color& Chw, base_color_c& C)
{
	C.hemi = Chw.hemi;
	C.sun = Chw.sun;

	float3 temp_rgb = Chw.get_rgb_f32();
	C.rgb.set(temp_rgb.x, temp_rgb.y, temp_rgb.z);
};

auto LightHW = [&](hardware_lighting& L)
	{
		R_Light cuL;
		cuL.type = L.type;
		cuL.diffuse = { L.diffuse.x, L.diffuse.y, L.diffuse.z };
		cuL.position = { L.position.x, L.position.y, L.position.z };
		cuL.direction = { L.direction.x, L.direction.y, L.direction.z };
		cuL.range = L.range;
		cuL.range2 = L.range2;
		cuL.falloff = L.falloff;
		cuL.attenuation0 = L.attenuation0;
		cuL.attenuation1 = L.attenuation1;
		cuL.attenuation2 = L.attenuation2;
		cuL.energy = L.energy;
		return cuL;
	};

auto Light = [&](R_Light& L, int type)
	{
		hardware_lighting cuL;
		cuL.type = L.type;
		cuL.light_type = type;
		cuL.diffuse = { L.diffuse.x, L.diffuse.y, L.diffuse.z };
		cuL.position = { L.position.x, L.position.y, L.position.z };
		cuL.direction = { L.direction.x, L.direction.y, L.direction.z };
		cuL.range = L.range;
		cuL.range2 = L.range2;
		cuL.falloff = L.falloff;
		cuL.attenuation0 = L.attenuation0;
		cuL.attenuation1 = L.attenuation1;
		cuL.attenuation2 = L.attenuation2;
		cuL.energy = L.energy;
		return cuL;
	};

// Embree

float RaytraceEmbreeNew(hardware_lighting& Lnew, HardwareVector& Pnew, HardwareVector& Dnew, float R, Face* Skip)
{
	auto V = LightHW(Lnew);
	auto P = Fvector().set(Pnew.x, Pnew.y, Pnew.z);
	auto D = Fvector().set(Dnew.x, Dnew.y, Dnew.z);
	return EmbreeMain.RaytraceEmbreeProcess(V, P, D, R, Skip);

}

void CalculatePoint(hardware_lighting& L, HardwareVector& P, HardwareVector& N, hardware_color& C, Face* Skip)
{
	HardwareVector Ldir;
	HardwareVector Pnew = P;
	Pnew.Mad_Self(N, 0.01f);

	HardwareVector LightPosition(L.position);
	HardwareVector LightDirection(L.direction);
	HardwareVector LightDiffuse(L.diffuse);

	bool isSunOrHemi = L.light_type != LGroup::eRGB;
	float att = 0;
	switch (L.type)
	{
	case LT_DIRECT:
	{
		Ldir.Inverted(LightDirection);
		float D = Ldir.DotProduct(N);
		if (D <= 0)
			return;

		float trace = RaytraceEmbreeNew(L, Pnew, Ldir, 1000.f, Skip);
		att = isSunOrHemi ? L.energy * trace : D * L.energy * trace;
	}
	break;

	case LT_POINT:
	{
		float sqD = P.DistanceSquared(LightPosition);
		if (sqD > L.range2)
			return;

		Ldir.Subtract(LightPosition, P).Normalize_Safe();
		float D = Ldir.DotProduct(N);
		if (D <= 0)
			return;



		float R = sqrt(sqD); // from api
		float trace = RaytraceEmbreeNew(L, Pnew, Ldir, R, Skip);
		float scale = D * L.energy * trace;

		if (isSunOrHemi)
		{
			att = scale / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD);
		}
		else
		{
			att = scale * (1 / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD) - R * L.falloff);
		}

	}break;

	case LT_SECONDARY:
	{
		float sqD = P.DistanceSquared(LightPosition);
		if (sqD > L.range2)
			return;

		Ldir.Subtract(LightPosition, P).Normalize_Safe();
		float D = Ldir.DotProduct(N);
		if (D <= 0)
			return;

		D *= -Ldir.DotProduct(LightDirection);
		if (D <= 0)
			return;

		float R = sqrt(sqD);
		float trace = RaytraceEmbreeNew(L, Pnew, Ldir, R, Skip);
		att = powf(D, 0.125f) * L.energy * trace * (1 - R / L.range);

	}break;
	}

	switch (L.light_type)
	{
	case eSun:
		C.sun += att;
		break;
	case eHemi:
		C.hemi += att;
		break;
	case eRGB:
		float3 rgb = C.get_rgb_f32();
		rgb.x += att * L.diffuse.x;
		rgb.y += att * L.diffuse.y;
		rgb.z += att * L.diffuse.z;
		C.set_rgb_f32(rgb.x, rgb.y, rgb.z);
		break;
	}
};

void ProcessRays(Fvector& P, Fvector& D, base_lighting& LS, hardware_color& Cnew, u8 flags, Face* Skip)
{
	HardwareVector Pos(P.x, P.y, P.z);
	HardwareVector Dir(D.x, D.y, D.z);
	if (!(flags & LP_dont_sun))
		for (auto& L : LS.sun)
		{
			hardware_lighting Lnew = Light(L, LGroup::eSun);
			CalculatePoint(Lnew, Pos, Dir, Cnew, Skip);
		}

	if (!(flags & LP_dont_hemi))
		for (auto& L : LS.hemi)
		{
			hardware_lighting Lnew = Light(L, LGroup::eHemi);
			CalculatePoint(Lnew, Pos, Dir, Cnew, Skip);
		}

	if (!(flags & LP_dont_rgb))
		for (auto& L : LS.rgb)
		{
			hardware_lighting Lnew = Light(L, LGroup::eRGB);
			CalculatePoint(Lnew, Pos, Dir, Cnew, Skip);
		}
}

// Cannot Now use in MT