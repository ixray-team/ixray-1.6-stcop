
#include "StdAfx.h"
#include "Build.h"

#include "xrPhase_MergeLM_Surface.h"
#include "xrPhase_MergeLM_Rect.h"
#include "../xrLC_Light/xrDeflector.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/Lightmap.h"



#define OFFSET_POS 1

// #define USE_NEW_WAY

#ifdef USE_NEW_WAY
void MergeLmap(vecDefl& Layer, CLightmap* lmap, int& MERGED)
{
	// Process 	
	// Немного отступ делаем
	u32 BorderUpdate = ((2 * BORDER) + OFFSET_POS);

	int _X = BorderUpdate, _Y = BorderUpdate;
 	int _Max_y = 0;

 	for (int it = 0; it < Layer.size(); it++)
	{
 		if (0 == (it % 1024))
			AditionalData("Process Y[%u] [%d]...Merged{%d}", _Y, it, MERGED);

		if (_Y > getLMSIZE() - 32)
  			break;

		lm_layer& L = Layer[it]->layer;
 
		u32 WIDTH   = L.width  + (2 * BORDER);
		u32 HEIGHT  = L.height + (2 * BORDER);

		if (_Max_y < HEIGHT)
			_Max_y = HEIGHT;
		 
		if (_X + WIDTH > getLMSIZE() - 32 )
		{
			_X  = BorderUpdate;
			_Y += _Max_y + BorderUpdate;
			_Max_y = 0;
		}

 		L_rect		rT, rS;
 		rS.a.set(_X, _Y);
		rS.b.set(_X + WIDTH, _Y + HEIGHT);
		rS.iArea = L.Area();  
		rT = rS;

		// Нужен только в оригенальной LMerge
		BOOL		bRotated = false;  
 		if (_Y < getLMSIZE() - HEIGHT)
		{
			lmap->Capture(Layer[it], rT.a.x, rT.a.y, rT.SizeX(), rT.SizeY(), bRotated);
			Layer[it]->bMerged = TRUE;
			MERGED++;
		}
 		
		_X += WIDTH + BorderUpdate;
		Progress(float(it) / float(g_XSplit.size()));
	}
}

void CBuild::xrPhase_MergeLM()
{
	vecDefl			Layer;

	// **** Select all deflectors, which contain this light-layer
	Layer.clear	();
	for (u32 it=0; it<lc_global_data()->g_deflectors().size(); it++)
	{
		CDeflector*	D		= lc_global_data()->g_deflectors()[it];
		if (D->bMerged)		continue;
		Layer.push_back		(D);
	}
  
	// Merge this layer (which left unmerged)
	u32 StartSize = Layer.size();
	u32 TotalMerged = 0;
	
	string512	phase_name;
	xr_sprintf(phase_name, "Building lightmaps...");
	Phase(phase_name);

	CTimer t;
	while (Layer.size())
	{
		VERIFY( lc_global_data() );
 
		// Sort layer by similarity (state changes)
		// + calc material area
		Status		("Selection...");
		for (u32 it=0; it<materials().size(); it++) materials()[it].internal_max_area	= 0;
		for (u32 it=0; it<Layer.size(); it++)	
		{
			CDeflector*	D		= Layer[it];
			materials()[D->GetBaseMaterial()].internal_max_area	= _max(D->layer.Area(),materials()[D->GetBaseMaterial()].internal_max_area);
		}

		std::sort(Layer.begin(), Layer.end(),
			[](CDeflector* D1, CDeflector* D2) {
				if (D1->layer.height < D2->layer.height) {
					return true;
				} else {
					return false;
				}
			}
		);

		// Startup
		Status		("Processing...");
 
		CLightmap*	lmap		= new CLightmap ();
		VERIFY( lc_global_data() );
		lc_global_data()->lightmaps().push_back	(lmap);

  		int MERGED = 0;
		MergeLmap(Layer, lmap, MERGED);
 		TotalMerged += MERGED;
  
		
		// Remove merged lightmaps
		vecDeflIt last = std::remove_if(Layer.begin(), Layer.end(), [&](CDeflector* D) 
			{
				if (D->bMerged)
					return true;
				else
					return false;
			});

		Layer.erase(last, Layer.end());
		clMsg("Erase Layer(Deflects) Time: %u ms", t.GetElapsed_ms()); t.Start();
		
		// Save
  		AditionalData("Lmaps: %u, Merging:[%u/%u]|%u", lc_global_data()->lightmaps().size(), MERGED, TotalMerged, Layer.size());
 		Progress(float(float(MERGED) / float(StartSize)));
	}

	VERIFY(lc_global_data());
	clMsg("%d lightmaps builded", lc_global_data()->lightmaps().size());
	Progress(1.f);
}
 
#else 
 
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

class	pred_remove { public: IC bool	operator() (CDeflector* D) { { if (0 == D) return TRUE; }; if (D->bMerged) { D->bMerged = FALSE; return TRUE; } else return FALSE; }; };
 
void CBuild::xrPhase_MergeLM()
{
	vecDefl			Layer;

	// **** Select all deflectors, which contain this light-layer
	Layer.clear();
	for (u32 it = 0; it < lc_global_data()->g_deflectors().size(); it++)
	{
		CDeflector* D = lc_global_data()->g_deflectors()[it];
		if (D->bMerged)		continue;
		Layer.push_back(D);
	}

	// Merge this layer (which left unmerged)
	while (Layer.size())
	{
		VERIFY(lc_global_data());
		string512	phase_name;
		sprintf(phase_name, "Building lightmap %d...", lc_global_data()->lightmaps().size());
		Phase(phase_name);

		// Sort layer by similarity (state changes)
		// + calc material area
		Status("Selection...");
		for (u32 it = 0; it < materials().size(); it++) 
			materials()[it].internal_max_area = 0;

		for (u32 it = 0; it < Layer.size(); it++) 
		{
			CDeflector* D = Layer[it];
			materials()[D->GetBaseMaterial()].internal_max_area = _max(D->layer.Area(), materials()[D->GetBaseMaterial()].internal_max_area);
		}
		std::stable_sort(Layer.begin(), Layer.end(), sort_defl_complex);

  		// Слишком много возьмет для помещения 
		#define MAXIMAL_MAPS_PLACED 4
 		u32 maxarea = getLMSIZE() * getLMSIZE() * MAXIMAL_MAPS_PLACED;	// Max up to 8 lm selected
		u32 curarea = 0;
		u32 merge_count = 0;

		CTimer tMerge;
		tMerge.Start();
		Status("Select Area for merging");
		for (u32 it = 0; it < (int)Layer.size(); it++) 
		{
			int		defl_area = Layer[it]->layer.Area();
			if (curarea + defl_area > maxarea) break;
			curarea += defl_area;
			merge_count++;
		}

		Msg("Select Area Time : %u", tMerge.GetElapsed_ms() );

		// Startup
		Status("Processing...");
		placer_perpixel._InitSurface_tbb();
		CLightmap* lmap = new CLightmap();
		VERIFY(lc_global_data());
		lc_global_data()->lightmaps().push_back(lmap);

		// Process 
		u32 MergedSize = 0;
		tMerge.Start();

		u32 ErrorsPlace = 0;
		for (u32 it = 0; it < merge_count; it++)
		{
			lm_layer& L = Layer[it]->layer;
			
 			if (it % 512 == 0)
			{
				placer_perpixel.RecalcY();
				AditionalData("IT: %u/%u/Merged:%u | Filled: %u | NoPlaced: %u",
					it,
					merge_count,
					MergedSize,
					// placer_perpixel.StartYPos,
					// placer_perpixel.FilledSize,
					placer_perpixel.FilledPercent,
					ErrorsPlace
					);
			}

			L_rect		rT, rS;
			rS.a.set(0, 0);
			rS.b.set(L.width + 2 * BORDER - 1, L.height + 2 * BORDER - 1);
			rS.iArea = L.Area();
			rT = rS;
			bool rotated = false;
		
			if (placer_perpixel.rect_place_full(rT, &L))
			{
				MergedSize++;

				lmap->Capture(Layer[it], rT.a.x, rT.a.y, rT.SizeX(), rT.SizeY(), rotated);
				Layer[it]->bMerged = TRUE;
			}
			else
				if (L.Area() < 128)
					ErrorsPlace++;

			// Раний выход
			if (ErrorsPlace > 4096) // && placer_perpixel.FilledPercent > 400
			 	break;

			Progress( float(it) / float(merge_count) );
		}

		Msg("Process Area Time : %u", tMerge.GetElapsed_ms());

		Progress(1.f);

		// Remove merged lightmaps
		Status("Cleanup...");
		vecDeflIt last = std::remove_if(Layer.begin(), Layer.end(), pred_remove());
		Layer.erase(last, Layer.end());
	}
	VERIFY(lc_global_data());
	clMsg("%d lightmaps builded", lc_global_data()->lightmaps().size());
}
#endif
 
void CBuild::xrPhase_SaveLmaps()
{
	Status("Destroying deflectors...");
	clMsg("Start Destroy Deflectors: Memory: %llu mb used", u32(GetMemoryUsed() / 1024 / 1024));
	for (u32 it = 0; it < lc_global_data()->g_deflectors().size(); it++)
		xr_delete(lc_global_data()->g_deflectors()[it]);
	lc_global_data()->g_deflectors().clear();
	clMsg("End Destroy Deflectors: Memory: %llu mb used", u32(GetMemoryUsed() / 1024 / 1024));

	Status("Start Saving Lmaps: ");
	size_t USED_MEMORY = 0;

	CTimer t;
	int IDX = 0;
	for (auto lmap : lc_global_data()->lightmaps())
	{
		t.Start();
		lmap->Save(pBuild->path);
		clMsg("Saving Map [%u/%u] %u ms", IDX, lc_global_data()->lightmaps().size(), t.GetElapsed_ms());

		IDX++;
		USED_MEMORY += lmap->lm.memory_lmap();
	}

	u32 USED_LMAPS = USED_MEMORY / 1024 / 1024;
	clMsg("Allocated FOR Lmaps Memory: %u mb", u32(USED_LMAPS));

	AditionalData("Lmaps allocated: %u mb", USED_LMAPS);
}
