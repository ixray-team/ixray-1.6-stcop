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
	clMsg		("Calculating materials/subdivs... Memory: [%umb]", GetMemoryUsed() / 1024 / 1024);
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
			//Progress(float(F_it-lc_global_data()->g_faces().begin())/float(lc_global_data()->g_faces().size()));
		});
	}


	size_t VSize = lc_global_data()->g_vertices().size() * sizeof(Vertex);
	size_t FSize = lc_global_data()->g_faces().size() * sizeof(Face);
  	AditionalData("[1] V(%umb)T(%umb)",
 		VSize / 1024 / 1024,
		FSize / 1024 / 1024
	);
	Msg("[1] V(%umb)T(%umb)",
		VSize / 1024 / 1024,
		FSize / 1024 / 1024);
	
	clMsg				("Perfroming subdivisions... Memory: [%umb]", GetMemoryUsed() / 1024 / 1024);
	{
		g_XSplit.reserve(64*1024);
		g_XSplit.resize	(counts.size());
		for (u32 I=0; I<counts.size(); I++) 
		{
			g_XSplit[I] = new vecFace ();
			g_XSplit[I]->reserve	(counts[I].dwCount);
		}
		
		for (vecFaceIt F_it=lc_global_data()->g_faces().begin(); F_it!=lc_global_data()->g_faces().end(); F_it++)
		{
			Face*	F							= *F_it;
			if (!F->Shader().flags.bRendering)	continue;

			for (u32 I=0; I<counts.size(); I++)
			{
				if (F->dwMaterial == counts[I].dwMaterial)
				{
					g_XSplit[I]->push_back	(F);
				}
			}
			Progress(float(F_it-lc_global_data()->g_faces().begin())/float(lc_global_data()->g_faces().size()));
		}
	}

	VSize = lc_global_data()->g_vertices().size() * sizeof(Vertex);
	FSize = lc_global_data()->g_faces().size() * sizeof(Face);
	AditionalData("[2] V(%umb)T(%umb)",
		VSize / 1024 / 1024,
		FSize / 1024 / 1024
	);
	Msg("[2] V(%umb)T(%umb)",
		VSize / 1024 / 1024,
		FSize / 1024 / 1024);

	clMsg				("Removing empty subdivs... Memory: [%umb]", GetMemoryUsed() / 1024 / 1024);
	{
		for (int SP = 0; SP<int(g_XSplit.size()); SP++)
		{
			if (g_XSplit[SP]->empty())
			xr_delete(g_XSplit[SP]);
		}
		g_XSplit.erase(std::remove(g_XSplit.begin(),g_XSplit.end(),(vecFace*) NULL),g_XSplit.end());
	}


	VSize = lc_global_data()->g_vertices().size() * sizeof(Vertex);
	FSize = lc_global_data()->g_faces().size() * sizeof(Face);
	AditionalData("[3] V(%umb)T(%umb)",
		VSize / 1024 / 1024,
		FSize / 1024 / 1024
	);
	Msg("[3] V(%umb)T(%umb)",
		VSize / 1024 / 1024,
		FSize / 1024 / 1024);
	
	clMsg	("Detaching subdivs... Memory: [%umb]", GetMemoryUsed() / 1024 / 1024);
	
 	for (u32 it=0; it<g_XSplit.size(); it++)
	{
		Detach(g_XSplit[it]);
	}
 

	VSize = lc_global_data()->g_vertices().size() * sizeof(Vertex);
	FSize = lc_global_data()->g_faces().size() * sizeof(Face);
	AditionalData("[4] V(%umb)T(%umb)",
		VSize / 1024 / 1024,
		FSize / 1024 / 1024
	);
	Msg("[4] V(%umb)T(%umb)",
		VSize / 1024 / 1024,
		FSize / 1024 / 1024);


	clMsg				("%d subdivisions.",g_XSplit.size());
}
