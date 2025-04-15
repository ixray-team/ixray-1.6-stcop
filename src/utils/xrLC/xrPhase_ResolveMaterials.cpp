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
	t.Start();
	{
		g_XSplit.reserve(64*1024);
		g_XSplit.resize	(counts.size());
		for (u32 I=0; I<counts.size(); I++) 
		{
			g_XSplit[I] = new vecFace ();
			g_XSplit[I]->reserve	(counts[I].dwCount);
		}
		

		xr_parallel_foreach ( lc_global_data()->g_faces().begin(), lc_global_data()->g_faces().end(),
			[&](Face* F)
			{
				if (!F->Shader().flags.bRendering)
					return;		// continue;

				for (u32 I = 0; I < counts.size(); I++)
				{
					if (F->dwMaterial == counts[I].dwMaterial)
					{
						csResolveMat.Enter();
 						g_XSplit[I]->push_back(F);
						csResolveMat.Leave();
					}
				};
			}
		);

		// Single Core
		//for (vecFaceIt F_it=lc_global_data()->g_faces().begin(); F_it!=lc_global_data()->g_faces().end(); F_it++)
		//{
		//	Face*	F							= *F_it;
		//	if (!F->Shader().flags.bRendering)	continue;
		//
		//	for (u32 I=0; I<counts.size(); I++)
		//	{
		//		if (F->dwMaterial == counts[I].dwMaterial)
		//		{
		//			g_XSplit[I]->push_back	(F);
		//		}
		//	}
		//	Progress(float(F_it-lc_global_data()->g_faces().begin())/float(lc_global_data()->g_faces().size()));
		//}
	}	
	clMsg("Perfroming subdivisions (SC)... Memory: [%umb] [%ums]", GetMemoryUsed() / 1024 / 1024, t.GetElapsed_ms());

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
 	for (u32 it=0; it<g_XSplit.size(); it++)
 		Detach(g_XSplit[it]);
 	clMsg("Detaching subdivs (SC)... Memory: [%umb] [%ums]", GetMemoryUsed() / 1024 / 1024, t.GetElapsed_ms());;

	clMsg				("%d subdivisions.",g_XSplit.size());
}
