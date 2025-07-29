#include "stdafx.h"
#include "xrDeflector.h"
#include "xrLC_GlobalData.h"

#include "light_point.h"
#include "xrFace.h"

void CDeflector::LightGPU( HASH& H )
{
	// Geometrical bounds
	Fbox bb;	
	bb.invalidate();
	try 
	{
		for (u32 fid = 0; fid < UVpolys.size(); fid++)
		{
			Face* F = UVpolys[fid].owner;
			for (int i = 0; i < 3; i++)	bb.modify(F->v[i]->P);
		}
		bb.getsphere(Sphere.P, Sphere.R);
	}
	catch (...)
	{
		clMsg("* ERROR: CDeflector::Light - sphere calc");
	}


	// se7kills todo: Аналог на GPU
	// Convert lights to local form
	// LightsSelected->select(inlc_global_data()->L_static(), Sphere.P, Sphere.R);

	// Calculate and fill borders
	try
	{
		lm_layer& lm = layer;

		// UV & HASH
		RemapUV(0, 0, lm.width, lm.height, lm.width, lm.height, FALSE);
		
		Fbox2			bounds;
		Bounds_Summary(bounds);
		H.initialize(bounds, (u32)UVpolys.size());
		for (u32 fid = 0; fid < UVpolys.size(); fid++) {
			UVtri* T = &(UVpolys[fid]);
			Bounds(fid, bounds);
			H.add(bounds, T);
		}

		// Calculate
		lm.create(lm.width, lm.height);
		L_DirectGPU(H);
	}
	catch (...)
	{
		clMsg("* ERROR: CDeflector::L_Calculate");
	}

}



extern void Jitter_Select(Fvector2*& Jitter, u32& Jcount);
 
void CDeflector::L_DirectGPU(   HASH& H)
{
	auto FromBarry = [&](Face* F, Fvector& wP, Fvector& wN, Fvector& B)
		{
			Vertex* V1 = F->v[0];
			Vertex* V2 = F->v[1];
			Vertex* V3 = F->v[2];
			wP.from_bary(V1->P, V2->P, V3->P, B);
			wN.from_bary(V1->N, V2->N, V3->N, B);
			exact_normalize(wN);
			wN.add(F->N);
			exact_normalize(wN);
		};

	lm_layer& lm = layer;
 	// Setup variables
	Fvector2	dim, half;
	dim.set(float(lm.width), float(lm.height));
	half.set(.5f / dim.x, .5f / dim.y);

	// Jitter data
	u32		 Jcount; Fvector2 JS; Fvector2* Jitter;
	JS.set(.4999f / dim.x, .4999f / dim.y);
	Jitter_Select(Jitter, Jcount);
	u32 flags = (inlc_global_data()->b_nosun() ? LP_dont_sun : 0) | LP_UseFaceDisable;
 
	for (u32 V = 0; V < lm.height; V++)
	{
 		for (u32 U = 0; U < lm.width; U++)
		{
 			u32				Fcount = 0;
 
			for (u32 J = 0; J < Jcount; J++)
			{
 				Fvector2 P;
				P.x = float(U) / dim.x + half.x + Jitter[J].x * JS.x;
				P.y = float(V) / dim.y + half.y + Jitter[J].y * JS.y;

				xr_vector<UVtri*>& space = H.query(P.x, P.y);
 				// World space
				Fvector		wP, wN, B;
				for (UVtri** it = &*space.begin(); it != &*space.end(); it++)
				{
					if ((*it)->isInside(P, B))
					{
						Face* F = (*it)->owner;
						FromBarry(F, wP, wN, B);
 						GPUTaskinSystem.LightPointPackedDeflector(this, U, V, wP, wN, flags, F);
  						Fcount += 1;
 						break;
					}
				}
			}

			FacesCount[{U, V}] = Fcount;
		}
	}

	NeedGarbageRays = true;
}

void CDeflector::ApplyGPU(HASH& H, bool isFirst)
{
	auto EdgeProcessing = [&](Fvector2& p1, Fvector2& p2, Fvector& v1, Fvector& v2, Fvector& N, float texel_size, Face* skip)
		{
			Fvector		vdir;
			vdir.sub(v2, v1);

			lm_layer& lm = layer;

			Fvector2		size;
			size.x = p2.x - p1.x;
			size.y = p2.y - p1.y;
			int	du = iCeil(_abs(size.x) / texel_size);
			int	dv = iCeil(_abs(size.y) / texel_size);
			int steps = _max(du, dv);
			if (steps <= 0)	return;

			for (int I = 0; I <= steps; I++)
			{
				float	time = float(I) / float(steps);
				Fvector2	uv;
				uv.x = size.x * time + p1.x;
				uv.y = size.y * time + p1.y;
				int	_x = iFloor(uv.x * float(lm.width));
				int _y = iFloor(uv.y * float(lm.height));

				if ((_x < 0) || (_x >= (int)lm.width))	continue;
				if ((_y < 0) || (_y >= (int)lm.height))	continue;

				if (lm.marker[_y * lm.width + _x])		continue;

				// ok - perform lighting
				base_color_c	C;
				Fvector			P;	P.mad(v1, vdir, time);

				LightPoint(nullptr, nullptr, C, P, N, lc_global_data()->L_static(), (inlc_global_data()->b_nosun() ? LP_dont_sun : 0) | LP_DEFAULT, skip); //.

				C.mul(.5f);
				lm.surface[_y * lm.width + _x]._set(C);
				lm.marker[_y * lm.width + _x] = 255;
			}
		};

	if (NeedGarbageRays)
	{
		NeedGarbageRays = false;

		lm_layer& lm = layer;
		auto UVColors = GPUTaskinSystem.DEF_Colors[this];

		for (auto& [key, count] : FacesCount)
		{
			u32 U = key.first;
			u32 V = key.second;

			if (count)
			{
				base_color_c& C = UVColors[key];
				C.scale(count);
				C.mul(.5f);
				lm.surface[V * lm.width + U]._set(C);
				lm.marker[V * lm.width + U] = 255;
			}
			else
			{
				base_color_c C;
				lm.surface[V * lm.width + U]._set(C);
				lm.marker[V * lm.width + U] = 0;
			}
		}

		FacesCount.clear();


		Fbox2			bounds;
		Bounds_Summary(bounds);
		H.initialize(bounds, (u32)UVpolys.size());
		for (u32 fid = 0; fid < UVpolys.size(); fid++) {
			UVtri* T = &(UVpolys[fid]);
			Bounds(fid, bounds);
			H.add(bounds, T);
		}

		// *** Render Edges (Embree Process)
		float texel_size = (1.f / float(_max(lm.width, lm.height))) / 8.f;
		for (u32 t = 0; t < UVpolys.size(); t++)
		{
			UVtri& T = UVpolys[t];
			Face* F = T.owner;
			EdgeProcessing(T.uv[0], T.uv[1], F->v[0]->P, F->v[1]->P, F->N, texel_size, F);
			EdgeProcessing(T.uv[1], T.uv[2], F->v[1]->P, F->v[2]->P, F->N, texel_size, F);
			EdgeProcessing(T.uv[2], T.uv[0], F->v[2]->P, F->v[0]->P, F->N, texel_size, F);
		}
 
		if (isFirst)
		{
			for (u32 ref = 254; ref > 0; ref--)
				if (!ApplyBorders(layer, ref))
					break;
		}
	}	
}

// Перерасчет в более сжатый формат

BOOL	compress_RMS(lm_layer& lm, u32 rms, u32& w, u32& h);
BOOL	compress_Zero(lm_layer& lm, u32 rms);

void CDeflector::LowerResolutionGPU(HASH& H)
{
	ApplyResolution = false;

	try
	{
		u32	w, h;
		if (compress_Zero(layer, rms_zero))
		{
			ApplyResolution = true;
			return;		// already with borders
		}
		else if (compress_RMS(layer, rms_shrink, w, h))
		{
			
			Fbox2			bounds;
			Bounds_Summary(bounds);
			H.initialize(bounds, (u32)UVpolys.size());
			for (u32 fid = 0; fid < UVpolys.size(); fid++) {
				UVtri* T = &(UVpolys[fid]);
				Bounds(fid, bounds);
				H.add(bounds, T);
			}

			// Reacalculate lightmap at lower resolution
			layer.create(w, h);
			L_DirectGPU(H);
		}
	}
	catch (...)
	{
		clMsg("* ERROR: CDeflector::Light - Compression");
	}
}
 
void CDeflector::ApplyExpadBordersGPU()
{
	if (ApplyResolution) return;

	// Expand with borders
	try
	{
		if (layer.width == 1)
		{
			// Horizontal ZERO - vertical line
			lm_layer		T;
			T.create(2 * BORDER, layer.height + 2 * BORDER);

			// Transfer
			for (u32 y = 0; y < T.height; y++)
			{
				int			py = int(y) - BORDER;
				clamp(py, 0, int(layer.height - 1));
				base_color	C = layer.surface[py];
				T.surface[y * 2 + 0] = C;
				T.marker[y * 2 + 0] = 255;
				T.surface[y * 2 + 1] = C;
				T.marker[y * 2 + 1] = 255;
			}

			// Exchange
			T.width = 0;
			T.height = layer.height;
			layer = T;
		}
		else if (layer.height == 1)
		{
			// Vertical ZERO - horizontal line
			lm_layer		T;
			T.create(layer.width + 2 * BORDER, 2 * BORDER);

			// Transfer
			for (u32 x = 0; x < T.width; x++)
			{
				int			px = int(x) - BORDER;
				clamp(px, 0, int(layer.width - 1));
				base_color	C = layer.surface[px];
				T.surface[0 * T.width + x] = C;
				T.marker[0 * T.width + x] = 255;
				T.surface[1 * T.width + x] = C;
				T.marker[1 * T.width + x] = 255;
			}

			// Exchange
			T.width = layer.width;
			T.height = 0;
			layer = T;
		}
		else
		{
			// Generic blit
			lm_layer		lm_old = layer;
			lm_layer		lm_new;
			lm_new.create(lm_old.width + 2 * BORDER, lm_old.height + 2 * BORDER);
			lblit(lm_new, lm_old, BORDER, BORDER, 255 - BORDER);
			layer = lm_new;

			ApplyBorders(layer, 254);
			ApplyBorders(layer, 253);
			ApplyBorders(layer, 252);
			ApplyBorders(layer, 251);
			for (u32 ref = 250; ref > 0; ref--)
				if (!ApplyBorders(layer, ref))
					break;

			layer.width = lm_old.width;
			layer.height = lm_old.height;
		}
	}
	catch (...)
	{
		clMsg("* ERROR: CDeflector::Light - BorderExpansion");
	}
}