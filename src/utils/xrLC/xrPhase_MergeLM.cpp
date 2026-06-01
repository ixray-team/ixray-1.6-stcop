
#include "StdAfx.h"
#include "Build.h"

#include "xrPhase_MergeLM_Surface.h"
#include "xrPhase_MergeLM_Rect.h"
#include "../xrLC_Light/xrDeflector.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/Lightmap.h"
#include "../xrForms/CompilersUI.h"
#include <ppl.h>

extern CompilersMode gCompilerMode;

// Other Stuff
IC int	compare_defl(CDeflector* D1, CDeflector* D2)
{
	// First  - by material
	u16 M1 = D1->GetBaseMaterial();
	u16 M2 = D2->GetBaseMaterial();
	if (M1 < M2)	return	1;  // less
	if (M1 > M2)	return	0;	// more
	return				2;	// equal
}

// should define LESS(D1<D2) behaviour
// sorting - in increasing order
IC int	sort_defl_analyze(CDeflector* D1, CDeflector* D2)
{
	// first  - get material index
	u16 M1 = D1->GetBaseMaterial();
	u16 M2 = D2->GetBaseMaterial();

	// 1. material area
	u32	 A1 = pBuild->materials()[M1].internal_max_area;
	u32	 A2 = pBuild->materials()[M2].internal_max_area;
	if (A1 < A2)	return	2;	// A2 better
	if (A1 > A2)	return	1;	// A1 better

	// 2. material sector (geom - locality)
	u32	 s1 = pBuild->materials()[M1].sector;
	u32	 s2 = pBuild->materials()[M2].sector;
	if (s1 < s2)	return	2;	// s2 better
	if (s1 > s2)	return	1;	// s1 better

	// 3. just material index
	if (M1 < M2)	return	2;	// s2 better
	if (M1 > M2)	return	1;	// s1 better

	// 4. deflector area
	u32 da1 = D1->layer.Area();
	u32 da2 = D2->layer.Area();
	if (da1 < da2)return	2;	// s2 better
	if (da1 > da2)return	1;	// s1 better

	// 5. they are EQUAL
	return				0;	// equal
}

// should define LESS(D1<D2) behaviour
// sorting - in increasing order
IC bool	sort_defl_complex(CDeflector* D1, CDeflector* D2)
{
	switch (sort_defl_analyze(D1, D2))
	{
	case 1:		return true;	// 1st is better 
	case 2:		return false;	// 2nd is better
	case 0:		return false;	// none is better
	default:	return false;
	}
}


// Оригенальный метод но с улучшениями для скорости и большей разверткой в 2к - 16к !
// Доработал метод SizeX плюсуется что очень ускоряет сборку DDS ! теперь он быстрее обычного !
u32 MergeLmap_Compact(xr_vector<CDeflector*>& Layer, CLightmap* lmap)
{
	static u32 MergedCount = 0;
	static xr_atomic_u32 CurrentIndex = 0;
	static xr_atomic_u32 ErrorsPlace  = 0;

	CurrentIndex = 0;
	ErrorsPlace = 0;
	MergedCount = 0;
 
	concurrency::parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [Layer, lmap](size_t thread_id)
	{
		static xrCriticalSection IndexLock;
		while (true)
		{
			u32 IndexTask = CurrentIndex.fetch_add(1);
			if (IndexTask >= Layer.size()) break;
			CDeflector* D = Layer[IndexTask];

			if (ErrorsPlace > 256 && D->layer.Area() > 4) continue;
			if (ErrorsPlace > 1024) break;

			lm_layer& L = D->layer;

			if (IndexTask % 16 == 0)
			{
				float Total = float(IndexTask - 1) / float(Layer.size());
				Progress(Total);

				AditionalData("IT: %u/%u | Fill: %u/%u | Error: %u", 
					IndexTask, Layer.size(),
					placer_perpixel.FullFilled,placer_perpixel.SurfaceGrid,
					ErrorsPlace.load());

				placer_perpixel.UpdateFill();
			}

			L_rect		rT;
 
			bool rotated = false;
			if (placer_perpixel.rect_place_full(rT, &L))
			{
				IndexLock.Enter();
				if (D->bMerged == false)
				{
					lmap->Capture(D, rT.a.x, rT.a.y, rT.SizeX(), rT.SizeY(), rotated);
					D->bMerged = true;
 					D->clear_memory();
					MergedCount++;
				}
				IndexLock.Leave();
			}
			else
				if (L.Area() < 128)
					ErrorsPlace.fetch_add(1);
		}
	});

	Layer.erase(
		std::remove_if(Layer.begin(), Layer.end(),
			[](CDeflector* D)
			{
				if (D == nullptr)return true;
				if (D->bMerged) return true;
				return false;
			}), Layer.end()
	);
  	return MergedCount;
}
 
u32 MergeLmapFast(xr_vector<CDeflector*>& Layer, CLightmap* lmap)
{
	u32 BORDER = gCompilerMode.LC_BORDER;

	u32 OFFSET_SHIFT = 2;
	u32 OFFSET_START = (2 * BORDER) + OFFSET_SHIFT;

	int _X = OFFSET_START, _Y = OFFSET_START;
	int _Max_y = 0;
	u32 MERGED = 0;

	u32 LMSIZE = gCompilerMode.LC_sizeLmaps;
	for (int it = 0; it < Layer.size(); it++)
	{
		if (Layer[it]->bMerged) continue;

		if (_Y > LMSIZE - 32) break;

		lm_layer& L = Layer[it]->layer;

		u32 WIDTH = L.width + (2 * BORDER);
		u32 HEIGHT = L.height + (2 * BORDER);

		if (_Max_y < HEIGHT)
			_Max_y = HEIGHT;

		if (_X + WIDTH > LMSIZE - 32)
		{
			_X = OFFSET_START;				// Ставим как стартовый
			_Y += _Max_y + OFFSET_SHIFT;	// Офсетаем не как стартовый
			_Max_y = 0;
		}

		L_rect		rT, rS;
		rS.a.set(_X, _Y);
		rS.b.set(_X + WIDTH, _Y + HEIGHT);
		rS.iArea = L.Area();
		rT = rS;

		// Нужен только в оригенальной LMerge
		bool		bRotated = false;
		if (_Y < LMSIZE - HEIGHT)
		{
			lmap->Capture(Layer[it], rT.a.x, rT.a.y, rT.SizeX(), rT.SizeY(), bRotated);
			Layer[it]->bMerged = true;
			Layer[it]->layer.clear_memory();
			MERGED++;
		}

		_X += WIDTH + OFFSET_SHIFT; // Офсетаем как стартовый
	}
 
	// Удаляем то что сделали !
	Layer.erase(
		std::remove_if(Layer.begin(), Layer.end(),
			[](CDeflector* D)
			{
				if (D == nullptr)return true;
				if (D->bMerged) return true;
				return false;
			}), Layer.end()
	);

	return MERGED;
}


void CBuild::xrPhase_MergeLM()
{
	auto& Layer = lc_global_data()->g_deflectors();

	Phase("Building Lmaps...");
	// **** Select all deflectors, which contain this light-layer
 	for (u32 it = 0; it < materials().size(); it++)
		materials()[it].internal_max_area = 0;
 	for (auto D : Layer)
 		materials()[D->GetBaseMaterial()].internal_max_area = std::max(D->layer.Area(), materials()[D->GetBaseMaterial()].internal_max_area);
	

	// Merge this layer (which left unmerged)
	u32 StartSize   = Layer.size();
	u32 TotalMerged = 0;
	CTimer tStats; 
 
	while (Layer.size())
	{
 		tStats.Start();
 
		CLightmap* lmap = new CLightmap();
		lc_global_data()->lightmaps().push_back(lmap);
    	
		if (gCompilerMode.LC_fast_way)
		{
			std::sort(Layer.begin(), Layer.end(), [](CDeflector* d1, CDeflector* d2) {return d1->layer.height < d1->layer.height; });
 			TotalMerged += MergeLmapFast(Layer, lmap);
		}
		else
		{
			std::stable_sort(Layer.begin(), Layer.end(), sort_defl_complex);

			placer_perpixel._InitSurface();
 			TotalMerged += MergeLmap_Compact(Layer, lmap);
		}
		 
 		lmap->Save(pBuild->path);
		Progress(1);
 
		clMsg("* [Lightmap: %u] : Merging:[%u/%u]  Time(%u ms)",
			lc_global_data()->lightmaps().size(), 
			TotalMerged, StartSize, 
			tStats.GetElapsed_ms()
 		);
	}
}