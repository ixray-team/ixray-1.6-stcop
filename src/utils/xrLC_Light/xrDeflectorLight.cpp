#include "stdafx.h"
#include "../../xrCore/Collision/cl_intersect.h"

#include "xrDeflector.h"
#include "xrLC_GlobalData.h"
#include "light_point.h"
#include "xrFace.h"

#include "../xrForms/CompilersUI.h"

void Jitter_Select(Fvector2* &Jitter, u32& Jcount)
{
	static Fvector2 Jitter1[1] = {
		{0,0}
	};
	static Fvector2 Jitter4[4] = {
		{-1,-1}, {1,-1}, {1,1}, {-1,1}
	};
	static Fvector2 Jitter9[9] = {
		{-1,-1},	{0,-1},		{1,-1}, 
		{-1,0},		{0,0},		{1,0},
		{-1,1},		{0,1},		{1,1}
	};

	switch (gCompilerMode.LC_JSample)
	{
	case 1:
		Jcount	= 1;
		Jitter	= Jitter1;
		break;
	case 9:
		Jcount	= 9;
		Jitter	= Jitter9;
		break;
	case 4:
	default:
		Jcount	= 4;
		Jitter	= Jitter4;
		break;
	}
}
 
BOOL ApplyBorders( lm_layer &lm, u32 ref ) 
{
	extern BOOL NEW_ApplyBorders(lm_layer & lm, u32 ref);
 	return NEW_ApplyBorders( lm, ref );
}
 
float getLastRP_Scale(CDB::COLLIDER* DB, CDB::MODEL* MDL, R_Light& L, Face* skip)
{
	u32		tris_count = DB->r_count();
	float	scale = 1.f;
	Fvector B;

	X_TRY
	{
		for (u32 I = 0; I < tris_count; I++)
		{
			CDB::RESULT& rpinf = DB->r_begin()[I];

			// Access to texture
			CDB::TRI& clT = MDL->get_tris()[rpinf.id];

			base_Face* F = convert_nax(clT.dummy);

			if (0 == F)											continue;
			if (skip == F)										continue;

			const Shader_xrLC& SH = F->Shader();
			if (!SH.flags.bLIGHT_CastShadow)					continue;

			if (F->flags.bOpaque) {
				// Opaque poly - cache it
				L.tri[0].set(rpinf.verts[0]);
				L.tri[1].set(rpinf.verts[1]);
				L.tri[2].set(rpinf.verts[2]);
				return 0;
			}

			b_material& M = inlc_global_data()->materials()[F->dwMaterial];
			b_texture& T = inlc_global_data()->textures()[M.surfidx];
#ifdef		DEBUG
			const b_BuildTexture& build_texture = inlc_global_data()->textures()[M.surfidx];
 			VERIFY(!!(build_texture.HasSurface()) == !!(!T.pSurface.Empty()));
#endif
			if (T.pSurface.Empty())
			{
				F->flags.bOpaque = true;
				clMsg("* ERROR: RAY-TRACE: Strange face detected... Has alpha without texture...");
				return 0;
			}

			// barycentric coords
			// note: W,U,V order
			B.set(1.0f - rpinf.u - rpinf.v,rpinf.u,rpinf.v);

			// calc UV
			Fvector2* cuv = F->getTC0();
			Fvector2	uv;
			uv.x = cuv[0].x * B.x + cuv[1].x * B.y + cuv[2].x * B.z;
			uv.y = cuv[0].y * B.x + cuv[1].y * B.y + cuv[2].y * B.z;

			int U = iFloor(uv.x * float(T.dwWidth) + .5f);
			int V = iFloor(uv.y * float(T.dwHeight) + .5f);
			U %= T.dwWidth;		if (U < 0) U += T.dwWidth;
			V %= T.dwHeight;	if (V < 0) V += T.dwHeight;

			u32* raw = static_cast<u32*>(*T.pSurface);
			u32 pixel = raw[V * T.dwWidth + U];
			u32 pixel_a = color_get_A(pixel);
			float opac = 1.f - _sqr(float(pixel_a) / 255.f);
			scale *= opac;
		}
	}
	X_CATCH
	{
		clMsg("* ERROR: getLastRP_Scale");
	}

	return scale;
}
 
float rayTraceOriginal(CDB::COLLIDER* DB, CDB::MODEL* MDL, R_Light& L, Fvector& P, Fvector& D, float R, Face* skip)
{
	R_ASSERT(DB);

	// 1. Check cached polygon	 
	float _u, _v, range;
	bool res = CDB::TestRayTri(P, D, L.tri, _u, _v, range, false);
	if (res && range > 0 && range < R)
		return 0;

	// 2. Polygon doesn't pick - real database query
	DB->ray_options(0);
 	DB->ray_query(MDL, P, D, R);

	if (DB->r_count() == 0)
		return 1;
  
	return getLastRP_Scale(DB, MDL, L, skip);
}

// Embree 
float rayTrace	(CDB::COLLIDER* DB, CDB::MODEL* MDL, R_Light& L, Fvector& P, Fvector& D, float R, Face* skip)
{
	if (MDL)
	{
		return rayTraceOriginal(DB, MDL, L, P, D, R, skip);
	}
	else
	{
		return EmbreeMain.RaytraceEmbreeProcess(P, D, R, skip);
	}
}

void LightPoint(CDB::COLLIDER* DB, CDB::MODEL* MDL, base_color_c& C, Fvector& P, Fvector& N, base_lighting& lights, u32 flags, Face* skip)
{
	auto processLight = [&]<typename T>(R_Light& L, T& accumulator, bool isSunOrHemi)
	{
		Fvector Ldir;
		Fvector Pnew = P;
		Pnew.mad(N, 0.01f);
		float att = 0.0f;

		switch (L.type)
		{
			case LT_DIRECT:
			{
				Ldir.invert(L.direction);
				float D = Ldir.dotproduct(N);
				if (D <= 0)
					return;

				float trace = rayTrace(DB, MDL, L, Pnew, Ldir, 1000.f, skip);
				att = isSunOrHemi ? L.energy * trace : D * L.energy * trace;
				break;
			}
			case LT_POINT:
			{
				float sqD = P.distance_to_sqr(L.position);
				if (sqD > L.range2)
					return;

				Ldir.sub(L.position, P).normalize_safe();
				float D = Ldir.dotproduct(N);
				if (D <= 0)
					return;

				float R = _sqrt(sqD);
				float trace = rayTrace(DB, MDL, L, Pnew, Ldir, R, skip);
				float scale = D * L.energy * trace;

				if (isSunOrHemi)
				{
					att = scale / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD);
				}
				else
				{
					att = (inlc_global_data()->gl_linear())
						? scale * (1 - R / L.range)
						: scale * (1 / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD) - R * L.falloff);
				}
				break;
			}
			case LT_SECONDARY:
			{
				float sqD = P.distance_to_sqr(L.position);
				if (sqD > L.range2)
					return;

				Ldir.sub(L.position, P).normalize_safe();
				float D = Ldir.dotproduct(N);
				if (D <= 0)
					return;

				D *= -Ldir.dotproduct(L.direction);
				if (D <= 0)
					return;

				float R = _sqrt(sqD);
				float trace = rayTrace(DB, MDL, L, Pnew, Ldir, R, skip);
				att = powf(D, 0.125f) * L.energy * trace * (1 - R / L.range);
				break;
			}
		}

		if (isSunOrHemi)
		{
			if constexpr (std::is_arithmetic_v<T>)
			{
				accumulator += att;
			}
			else
			{
				accumulator.add(att);
			}
		}
		else
		{
			C.rgb.x += att * L.diffuse.x;
			C.rgb.y += att * L.diffuse.y;
			C.rgb.z += att * L.diffuse.z;
		}
	};

	// RGB Lights
	if (!(flags & LP_dont_rgb))
	{
		if (DB != nullptr)
			 DB->ray_options(0);
		for (R_Light& L : lights.rgb)
		{
			processLight(L, C.rgb, false);
		}
	}

	// Sun Lights
	if (!(flags & LP_dont_sun))
	{
		if (DB != nullptr)
			DB->ray_options(0);
		for (R_Light& L : lights.sun)
		{
			processLight(L, C.sun, true);
		}
	}

	// Hemi Lights
	if (!(flags & LP_dont_hemi))
	{
		if (DB != nullptr)
			DB->ray_options(0);
		for (R_Light& L : lights.hemi)
		{
			processLight(L, C.hemi, true);
		}
	}
}


void LightPointNew(EmbreeRayTraceModel* MDL, base_color_c& C, Fvector& P, Fvector& N, base_lighting& lights, u32 flags, Face* skip)
{
	auto processLight = [&]<typename T>(R_Light & L, T & accumulator, bool isSunOrHemi)
	{
		Fvector Ldir;
		Fvector Pnew = P;
		Pnew.mad(N, 0.01f);
		float att = 0.0f;

		switch (L.type)
		{
		case LT_DIRECT:
		{
			Ldir.invert(L.direction);
			float D = Ldir.dotproduct(N);
			if (D <= 0)
				return;

			float trace = MDL->RaytraceEmbreeProcess( Pnew, Ldir, 1000.f, skip );
			att = isSunOrHemi ? L.energy * trace : D * L.energy * trace;
			break;
		}
		case LT_POINT:
		{
			float sqD = P.distance_to_sqr(L.position);
			if (sqD > L.range2)
				return;

			Ldir.sub(L.position, P).normalize_safe();
			float D = Ldir.dotproduct(N);
			if (D <= 0)
				return;

			float R = _sqrt(sqD);
			float trace = MDL->RaytraceEmbreeProcess( Pnew, Ldir, R, skip);
			float scale = D * L.energy * trace;

			if (isSunOrHemi)
			{
				att = scale / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD);
			}
			else
			{
				att = (inlc_global_data()->gl_linear())
					? scale * (1 - R / L.range)
					: scale * (1 / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD) - R * L.falloff);
			}
			break;
		}
		case LT_SECONDARY:
		{
			float sqD = P.distance_to_sqr(L.position);
			if (sqD > L.range2)
				return;

			Ldir.sub(L.position, P).normalize_safe();
			float D = Ldir.dotproduct(N);
			if (D <= 0)
				return;

			D *= -Ldir.dotproduct(L.direction);
			if (D <= 0)
				return;

			float R = _sqrt(sqD);
			float trace = MDL->RaytraceEmbreeProcess( Pnew, Ldir, R, skip );
			att = powf(D, 0.125f) * L.energy * trace * (1 - R / L.range);
			break;
		}
		}

		if (isSunOrHemi)
		{
			if constexpr (std::is_arithmetic_v<T>)
			{
				accumulator += att;
			}
			else
			{
				accumulator.add(att);
			}
		}
		else
		{
			C.rgb.x += att * L.diffuse.x;
			C.rgb.y += att * L.diffuse.y;
			C.rgb.z += att * L.diffuse.z;
		}
	};

	// RGB Lights
	if (!(flags & LP_dont_rgb))
	{
		for (R_Light& L : lights.rgb)
		{
			processLight(L, C.rgb, false);
		}
	}

	// Sun Lights
	if (!(flags & LP_dont_sun))
	{
		for (R_Light& L : lights.sun)
		{
			processLight(L, C.sun, true);
		}
	}

	// Hemi Lights
	if (!(flags & LP_dont_hemi))
	{
		for (R_Light& L : lights.hemi)
		{
			processLight(L, C.hemi, true);
		}
	}
}