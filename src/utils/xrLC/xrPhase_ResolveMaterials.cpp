#include "StdAfx.h"
#include "Build.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"
#include <mutex>
#include <execution>
std::mutex g_XSplit_mutex;

extern void		Detach		(vecFace* S);

struct _counter
{
	u16	dwMaterial;
	u32	dwCount;
};

void	CBuild::xrPhase_ResolveMaterials()
{
	// Count number of materials
	Status		("Calculating materials/subdivs...");
	xr_vector<_counter>	counts;
	{
		counts.reserve		(256);
		std::for_each(std::execution::par, lc_global_data()->g_faces().begin(), lc_global_data()->g_faces().end(), [&](Face* F)
			{
				// Face* F = *F_it;
				BOOL	bCreate = TRUE;

				for (u32 I = 0; I < counts.size(); I++)
				{
					if (F->dwMaterial == counts[I].dwMaterial)
					{
						std::lock_guard<std::mutex> lock(g_XSplit_mutex);
						counts[I].dwCount += 1;
						bCreate = FALSE;
						return;
					}
				}

				if (bCreate)
				{
					_counter	C;
					C.dwMaterial = F->dwMaterial;
					C.dwCount = 1;

					std::lock_guard<std::mutex> lock(g_XSplit_mutex);
					counts.push_back(C);
				}
				//Progress(float(F_it-lc_global_data()->g_faces().begin())/float(lc_global_data()->g_faces().size()));
			}
		);
	}
	
	Status				("Perfroming subdivisions...");
	{
		g_XSplit.reserve(64*1024);
		g_XSplit.resize	(counts.size());
		for (u32 I=0; I<counts.size(); I++) 
		{
			g_XSplit[I] = new vecFace ();
			g_XSplit[I]->reserve	(counts[I].dwCount);
		}

		std::for_each(std::execution::par, lc_global_data()->g_faces().begin(), lc_global_data()->g_faces().end(),
		[&](Face* F)
		{
			// Face*	F							= *F_it;
			if (!F->Shader().flags.bRendering)
				return;		// continue;

			for (u32 I = 0; I < counts.size(); I++)
			{
				if (F->dwMaterial == counts[I].dwMaterial)
				{
					std::lock_guard<std::mutex> lock(g_XSplit_mutex);
					g_XSplit[I]->push_back(F);
				}
			}
		});
	}

	Status				("Removing empty subdivs...");
	{
		for (int SP = 0; SP<int(g_XSplit.size()); SP++) 
			if (g_XSplit[SP]->empty())	xr_delete(g_XSplit[SP]);
		g_XSplit.erase(std::remove(g_XSplit.begin(),g_XSplit.end(),(vecFace*) NULL),g_XSplit.end());
	}
	
	Status				("Detaching subdivs...");
	{
		for (u32 it=0; it<g_XSplit.size(); it++)
		{
			Detach(g_XSplit[it]);
		}
	}
	clMsg				("%d subdivisions.",g_XSplit.size());
}
