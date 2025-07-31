#include "StdAfx.h"
#include "Build.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"

extern void		Detach		(vecFace* S);

struct _counter
{
	u16	dwMaterial;
	u32	dwCount;
};

xrCriticalSection csResolveMat;
void	CBuild::xrPhase_ResolveMaterials()
{
	// Count number of materials
	CTimer t;
	t.Start();
 	// Calculating materials
	concurrency::concurrent_vector<_counter> counts_mt_safe;
	{
  		counts_mt_safe.reserve(256);
		xr_parallel_foreach(lc_global_data()->g_faces().begin(), lc_global_data()->g_faces().end(), [&](Face* F)
		{
			BOOL	bCreate = TRUE;
 			for (u32 I = 0; I < counts_mt_safe.size(); I++)
			{
				if (F->dwMaterial == counts_mt_safe[I].dwMaterial)
				{
					counts_mt_safe[I].dwCount += 1;
					bCreate = FALSE;
					return;
				}
			}

			if (bCreate)
			{
				_counter	C;
				C.dwMaterial = F->dwMaterial;
				C.dwCount = 1;
				counts_mt_safe.push_back(C);
			}
		});
	}
	clMsg("Calculating materials/subdivs (MT)... Memory: [%umb] [%ums]", GetHeapMemory() / 1024 / 1024, t.GetElapsed_ms());
	
	
	// Performing Subdivs
	t.Start();
	u32 msCalc = 0;
	{		
		//x6 Áûסענוו םא Ryzen 7 3700x קול SC
		xr_vector<_counter> count(counts_mt_safe.begin(), counts_mt_safe.end());

		concurrency::concurrent_vector<concurrency::concurrent_vector<Face*>> g_Xsplits_def;
		g_Xsplits_def.reserve(64*1024);
		g_Xsplits_def.resize(count.size());

 		xr_parallel_foreach ( lc_global_data()->g_faces().begin(), lc_global_data()->g_faces().end(), [&](Face* F)
		{
			if (!F->Shader().flags.bRendering) return;					
			
			for (u32 I=0; I< count.size(); I++)
			{
				if (F->dwMaterial == count[I].dwMaterial)
				{
					g_Xsplits_def[I].push_back(F);
				}
			}
   		});
		msCalc = t.GetElapsed_ms();

		  
		g_XSplit.reserve(64 * 1024);
		g_XSplit.resize(counts_mt_safe.size());
		for (auto i = 0; i < g_XSplit.size(); i++)
		{
  			g_XSplit[i] = new vecFace( g_Xsplits_def[i].begin(), g_Xsplits_def[i].end() );
 		}
	}	
	clMsg("Perfroming subdivisions (MT)... Memory: [%umb] [%ums] copy[%ums]", GetHeapMemory() / 1024 / 1024, msCalc,  t.GetElapsed_ms() - msCalc);

	t.Start();
	{
		for (int SP = 0; SP<int(g_XSplit.size()); SP++)
		{
			if (g_XSplit[SP]->empty())
				xr_delete(g_XSplit[SP]);
		}
		g_XSplit.erase(std::remove(g_XSplit.begin(),g_XSplit.end(),(vecFace*) NULL),g_XSplit.end());
	}
	clMsg("Removing empty subdivs (SC) ... Memory: [%umb] [%ums]", GetHeapMemory() / 1024 / 1024, t.GetElapsed_ms());
  
	t.Start();
	for (auto F : g_XSplit)
 		Detach(F);
   	clMsg("Detaching subdivs (MT)... Memory: [%umb] [%ums]", GetHeapMemory() / 1024 / 1024, t.GetElapsed_ms());

	clMsg				("%d subdivisions.",g_XSplit.size());
}
