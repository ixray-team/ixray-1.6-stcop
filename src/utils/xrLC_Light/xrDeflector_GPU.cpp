#include "stdafx.h"
#include "xrDeflector.h"
#include "xrLC_GlobalData.h"
#include "light_point.h"
#include "xrFace.h"

// 08.12.2025 (Убрал 2Hash Нужно было для ускорения поиска по треугольникам) (для GPU кода будет сложно сделать)
// 14.12.2025 (Повыпиливал лишние действие с Edge) там можно было и так посчитать ;
 
extern void Jitter_Select(Fvector2*& Jitter, u32& Jcount);

/// Запрашивает лучи у ГПУ
void CDeflector::LightGPU()
{
	Fbox bb;		bb.invalidate();
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

	// Calculate and fill borders
	try
	{ 		
		// UV  
 		RemapUV(0, 0, layer.width, layer.height, layer.width, layer.height, FALSE);
 		layer.create(layer.width, layer.height);

		// Calculate
  		L_DirectGPU();
 	}
	catch (...)
	{
		clMsg("* ERROR: CDeflector::L_Calculate");
	}

}

thread_local HASH hash_2d;

void CDeflector::L_DirectGPU()
{
	auto FromBarry = [](Face* F, Fvector& wP, Fvector& wN, Fvector& B)
	{
		wP.from_bary(F->v[0]->P, F->v[1]->P, F->v[2]->P, B);
		wN.from_bary(F->v[0]->N, F->v[1]->N, F->v[2]->N, B);
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
	u32 Jcount; Fvector2 JS; Fvector2* Jitter;
	JS.set(.4999f / dim.x, .4999f / dim.y);
	Jitter_Select(Jitter, Jcount);
 
	// Создание тоже может занимать время
  	Fbox2			bounds;
	Bounds_Summary(bounds);
	hash_2d.initialize(bounds, (u32)UVpolys.size());
	for (u32 fid = 0; fid < UVpolys.size(); fid++)
	{
		UVtri* T = &(UVpolys[fid]);
		Bounds(fid, bounds);
		hash_2d.add(bounds, T);
	}
 
	for (u32 V = 0; V < lm.height; V++)
	{
 		for (u32 U = 0; U < lm.width; U++)
		{
  			u32 Fcount = 0;
			size_t TaskID = GPUTaskinSystem.MakeKey(U, V); 

			for (u32 J = 0; J < Jcount; J++)
			{
 				Fvector2 P;
				P.x = float(U) / dim.x + half.x + Jitter[J].x * JS.x;
				P.y = float(V) / dim.y + half.y + Jitter[J].y * JS.y;

 				// World space
				Fvector wP, wN, B;
				auto& Hash = hash_2d.query(P.x, P.y);
				for (auto& TRIANGLE : Hash)
				{
					if (TRIANGLE->isInside(P, B))
					{
						Face* F = TRIANGLE->owner;
						FromBarry(F, wP, wN, B);
						GPUTaskinSystem.LightPointPacked_add_task(TaskID, this, wP, wN, F);
 						Fcount += 1;
						break;
					}
				}
			}

			if (Fcount > 0)
				lm.marker[V * lm.width + U] = 255;
			else
				lm.marker[V * lm.width + U] = 0;
		}
	}
	 
	auto EdgeProcessing = [](CDeflector* Deflector, Fvector2& p1, Fvector2& p2, Fvector& v1, Fvector& v2, Fvector& N, float texel_size, Face* skip)
	{
		Fvector vdir;
		vdir.sub(v2, v1);

		lm_layer& lm = Deflector->layer;

		Fvector2 size;
		size.x = p2.x - p1.x;
		size.y = p2.y - p1.y;
		int	du = iCeil(std::abs(size.x) / texel_size);
		int	dv = iCeil(std::abs(size.y) / texel_size);
		int steps = std::max(du, dv);
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
			Fvector			P;
			P.mad(v1, vdir, time);
			  
			size_t TaskID = GPUTaskinSystem.MakeKey(_x, _y);
			GPUTaskinSystem.LightPointPacked_add_task(TaskID, Deflector, P, N, skip);
			lm.marker[_y * lm.width + _x] = 255;
		}
	};

	// *** Render Edges (Embree Process)
	float texel_size = (1.f / float(std::max(lm.width, lm.height))) / 8.f;
	for (u32 t = 0; t < UVpolys.size(); t++)
	{
		UVtri& T = UVpolys[t];
		Face* F = T.owner;
		EdgeProcessing(this, T.uv[0], T.uv[1], F->v[0]->P, F->v[1]->P, F->N, texel_size, F);
		EdgeProcessing(this, T.uv[1], T.uv[2], F->v[1]->P, F->v[2]->P, F->N, texel_size, F);
		EdgeProcessing(this, T.uv[2], T.uv[0], F->v[2]->P, F->v[0]->P, F->N, texel_size, F);
	}

	
	// Для теста выключаем
	ApplyLmap = true; //  (Включает ApplyColors)
}
  

/// Залетают лучи после расчета в ГПУ

// se7kills:
// Убрал hash_map Дорого ее чистить и память кушает 
// Сделал вектор Samples Для хранения результатов в lm_layer
bool CDeflector::ApplyColors()
{
	lm_layer& lm = layer;
 
    // Faces Только будет при простом проходе
 	bool AnyValue = ApplyLmap;
	if ( ApplyLmap)
	{
		ApplyLmap = false;
 
	
		base_color_c C_Zero, Cnew;
 		for (u32 V = 0; V < lm.height; V++)
		{
			for (u32 U = 0; U < lm.width; U++)
			{
				u32		Key   = V * lm.width + U;
				u8   Samples  = lm.samples[Key];
				auto& CResult = lm.surface[Key];

				if (Samples > 0)
				{
 					CResult._get(Cnew);
					Cnew.scale(Samples);
					Cnew.mul(0.5f);
					CResult._set(Cnew);
				}
				else
				{
					CResult._set(C_Zero);
				}
			}
		}
   	}	
 
	return AnyValue;
} 

void CDeflector::ApplyColor(size_t IKey, base_color_c& C)
{	
	auto& lm = layer;

	u32 U				= GPUTaskinSystem.GetU(IKey);
	u32 V				= GPUTaskinSystem.GetV(IKey);

	u32 Key				= V * lm.width + U;
 	auto& CResult		= lm.surface[Key];
	auto& Keys			= lm.samples[Key];
	Keys				+= 1;

	base_color_c cNew;
	CResult._get(cNew);
	cNew.add(C);
	CResult._set(cNew);
}

/// Перерасчет в более сжатый формат


BOOL	compress_RMS(lm_layer& lm, u32 rms, u32& w, u32& h);
BOOL	compress_Zero(lm_layer& lm, u32 rms);

void CDeflector::LowerResolutionGPU()
{
 	for (u32 ref = 254; ref > 0; ref--)
	if (!ApplyBorders(layer, ref))		break;

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
			// Reacalculate lightmap at lower resolution
			layer.create(w, h);
			L_DirectGPU(); 

			GPUTaskinSystem.Recalculated++;
		}	
	}
	catch (...)
	{
		clMsg("* ERROR: CDeflector::Light - Compression");
	}	
}

/// После сжатия пересчитываем
void CDeflector::ApplyExpandBordersGPU()
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
				clamp(py, 0, int(layer.height - 1));				base_color	C = layer.surface[py];
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
 