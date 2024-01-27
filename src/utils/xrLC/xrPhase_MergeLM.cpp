
#include "stdafx.h"
#include "build.h"

#include "xrPhase_MergeLM_Rect.h"
#include "../xrlc_light/xrdeflector.h"
#include "../xrlc_light/xrlc_globaldata.h"
#include "../xrlc_light/lightmap.h"
// Surface access
extern void _InitSurface	();
extern BOOL _rect_place		(L_rect &r, lm_layer*		D);

IC int	compare_defl		(CDeflector* D1, CDeflector* D2)
{
	// First  - by material
	u16 M1		= D1->GetBaseMaterial();
	u16 M2		= D2->GetBaseMaterial();
	if (M1<M2)	return	1;  // less
	if (M1>M2)	return	0;	// more
	return				2;	// equal
}

// should define LESS(D1<D2) behaviour
// sorting - in increasing order
IC int	sort_defl_analyze	(CDeflector* D1, CDeflector* D2)
{
	// first  - get material index
	u16 M1		= D1->GetBaseMaterial();
	u16 M2		= D2->GetBaseMaterial();

	// 1. material area
	u32	 A1		= pBuild->materials()[M1].internal_max_area;
	u32	 A2		= pBuild->materials()[M2].internal_max_area;
	if (A1<A2)	return	2;	// A2 better
	if (A1>A2)	return	1;	// A1 better

	// 2. material sector (geom - locality)
	u32	 s1		= pBuild->materials()[M1].sector;
	u32	 s2		= pBuild->materials()[M2].sector;
	if (s1<s2)	return	2;	// s2 better
	if (s1>s2)	return	1;	// s1 better

	// 3. just material index
	if (M1<M2)	return	2;	// s2 better
	if (M1>M2)	return	1;	// s1 better

	// 4. deflector area
	u32 da1		= D1->layer.Area();
	u32 da2		= D2->layer.Area();
	if (da1<da2)return	2;	// s2 better
	if (da1>da2)return	1;	// s1 better

	// 5. they are EQUAL
	return				0;	// equal
}

// should define LESS(D1<D2) behaviour
// sorting - in increasing order
IC bool	sort_defl_complex	(CDeflector* D1, CDeflector* D2)
{
	switch (sort_defl_analyze(D1,D2))	
	{
	case 1:		return true;	// 1st is better 
	case 2:		return false;	// 2nd is better
	case 0:		return false;	// none is better
	default:	return false;
	}
}

class	pred_remove { public: IC bool	operator() (CDeflector* D) { { if (0==D) return TRUE;}; if (D->bMerged) {D->bMerged=FALSE; return TRUE; } else return FALSE;  }; };

u32 LMTextureSize(const vecDefl& Layer) {
	u64 area = 0;
	int lm_1024 = 1024 * 1024;
	int lm_2048 = 2048 * 2048;
	int lm_4096 = 4096 * 4096;
	int lm_8192 = 8192 * 8192;

	for (int it = 0; it < Layer.size(); it++) {
		if (lm_8192 < area) {
			break;
		}

		lm_layer& L = Layer[it]->layer;
		area += L.Area();
	}

	int use_size = 8192;

	if (area < lm_1024) {
		use_size = 1024;
	} else if (area < lm_2048) {
		use_size = 2048;
	} else if (area < lm_4096) {
		use_size = 4096;
	} else if (area < lm_8192) {
		use_size = 8192;
	}

	c_LMAP_size = use_size;
	Msg("Select LM_SIZE: %d", use_size);
	return use_size;
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

	u32 LightMapTexSize = LMTextureSize(Layer);

	// Merge this layer (which left unmerged)
	while (Layer.size()) 
	{
		VERIFY( lc_global_data() );
		string512	phase_name;
		xr_sprintf		(phase_name,"Building lightmap %d...", lc_global_data()->lightmaps().size());
		Phase		(phase_name);

		// Sort layer by similarity (state changes)
		// + calc material area
		Status		("Selection...");
		for (u32 it=0; it<materials().size(); it++) materials()[it].internal_max_area	= 0;
		for (u32 it=0; it<Layer.size(); it++)	{
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
		_InitSurface			();
		CLightmap*	lmap		= xr_new<CLightmap> ();
		VERIFY( lc_global_data() );
		lc_global_data()->lightmaps().push_back	(lmap);

		int MERGED = 0;
		{
			// Process 	
			int x = 0, y = 0;
			u16 prev_resize_height = 0;
			u16 prev_resize_width = 0;
			u16 max_y = 0;


			for (int it = 0; it < Layer.size(); it++) {
				lm_layer& L = Layer[it]->layer;
				if (max_y < L.height + 2) {
					max_y = L.height + 2;
				}

				if (x + L.width + 2 > LightMapTexSize - 16 - L.width) {
					x = 0;  y += max_y; max_y = 0;
				}

				{
					L_rect		rT, rS;
					rS.a.set(x, y);
					rS.b.set(x + L.width + 2 * BORDER - 1, y + L.height + 2 * BORDER - 1);
					rS.iArea = L.Area();
					rT = rS;

					x += L.width + 2;

					BOOL		bRotated = rT.SizeX() != rS.SizeX();

					if (y < LightMapTexSize - 16 - L.height) {
						lmap->Capture(Layer[it], rT.a.x, rT.a.y, rT.SizeX(), rT.SizeY(), bRotated);
						Layer[it]->bMerged = TRUE;
						MERGED++;
					}
				}

				Progress(float(it) / float(g_XSplit.size()));

				if (0 == (it % 1024)) {
					Status("Process [%d/%d]...", it, g_XSplit.size());
				}
			}
		}
		Progress	(1.f);

		// Remove merged lightmaps
		int recvest = Layer.size() - MERGED;
		Status			("Cleanup...");
		vecDeflIt last	= std::remove_if	(Layer.begin(),Layer.end(),pred_remove());
		Layer.erase		(last,Layer.end());
		clMsg("LM After Clean %d==%d", Layer.size(), recvest);

		// Save
		Status			("Saving...");
		VERIFY			( pBuild );
		lmap->Save		(pBuild->path);
	}
	VERIFY( lc_global_data() );
	clMsg		( "%d lightmaps builded", lc_global_data()->lightmaps().size() );

	// Cleanup deflectors
	Progress	(1.f);
	Status		("Destroying deflectors...");
	for (u32 it=0; it<lc_global_data()->g_deflectors().size(); it++)
		xr_delete(lc_global_data()->g_deflectors()[it]);
	lc_global_data()->g_deflectors().clear();
}
