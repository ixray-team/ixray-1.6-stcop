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
	xr_vector<_counter>	counts;
	{
		counts.reserve		(256);
		
		xr_parallel_foreach( lc_global_data()->g_faces().begin(), lc_global_data()->g_faces().end(), [&](Face* F)
		{
			// Face* F = *F_it;
			BOOL	bCreate = TRUE;

			for (u32 I = 0; I < counts.size(); I++)
			{
				if (F->dwMaterial == counts[I].dwMaterial)
				{
					csResolveMat.Enter();
 					counts[I].dwCount += 1;
					csResolveMat.Leave();
					bCreate = FALSE;
					return;
				}
			}

			if (bCreate)
			{
				_counter	C;
				C.dwMaterial = F->dwMaterial;
				C.dwCount = 1;
				csResolveMat.Enter();
 				counts.push_back(C);
				csResolveMat.Leave();
			}
 		});
	}
	clMsg("Calculating materials/subdivs (MT)... Memory: [%umb] [%ums]", GetMemoryUsed() / 1024 / 1024, t.GetElapsed_ms());
	
	
	// Performing Subdivs
	t.Start();
	u32 msCalc = 0;
	{		
		//x6 Áûסענוו םא Ryzen 7 3700x קול SC
		concurrency::concurrent_vector<concurrency::concurrent_vector<Face*>> g_Xsplits_def;
		g_Xsplits_def.reserve(64*1024);
		g_Xsplits_def.resize(counts.size());

 		xr_parallel_foreach ( lc_global_data()->g_faces().begin(), lc_global_data()->g_faces().end(), [&](Face* F)
		{
			if (!F->Shader().flags.bRendering) return;					
			
			for (u32 I=0; I<counts.size(); I++)
			{
				if (F->dwMaterial == counts[I].dwMaterial)
				{
					g_Xsplits_def[I].push_back(F);
				}
			}
   		});
		msCalc = t.GetElapsed_ms();

		  
		g_XSplit.reserve(64 * 1024);
		g_XSplit.resize(counts.size());
		for (auto i = 0; i < g_XSplit.size(); i++)
		{
  			g_XSplit[i] = new vecFace( g_Xsplits_def[i].begin(), g_Xsplits_def[i].end() );
 		}
	}	
	clMsg("Perfroming subdivisions (MT)... Memory: [%umb] [%ums] copy[%ums]", GetMemoryUsed() / 1024 / 1024, msCalc,  t.GetElapsed_ms() - msCalc);

	t.Start();
	{
		for (int SP = 0; SP<int(g_XSplit.size()); SP++)
		{
			if (g_XSplit[SP]->empty())
				xr_delete(g_XSplit[SP]);
		}
		g_XSplit.erase(std::remove(g_XSplit.begin(),g_XSplit.end(),(vecFace*) NULL),g_XSplit.end());
	}
	clMsg("Removing empty subdivs (SC) ... Memory: [%umb] [%ums]", GetMemoryUsed() / 1024 / 1024, t.GetElapsed_ms());
  
	t.Start();
	for (auto F : g_XSplit)
 		Detach(F);
   	clMsg("Detaching subdivs (MT)... Memory: [%umb] [%ums]", GetMemoryUsed() / 1024 / 1024, t.GetElapsed_ms());

	clMsg				("%d subdivisions.",g_XSplit.size());
}
