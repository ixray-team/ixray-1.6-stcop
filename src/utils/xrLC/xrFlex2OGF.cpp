#include "StdAfx.h"
#include "Build.h"
#include "OGF_Face.h"
#include "vbm.h"
//#include "std_classes.h"
#include "../xrLC_Light/Lightmap.h"
#include "../xrLC_Light/xrFace.h"

#define	TRY(a) try { a; } catch (...) { clMsg("* E: %s", #a); }

void CBuild::validate_splits			()
{
	for (splitIt it=g_XSplit.begin(); it!=g_XSplit.end(); it++)
	{
		u32 MODEL_ID		= u32(it-g_XSplit.begin())	;
		if ((*it)->size() > c_SS_HighVertLimit*2)		{
			clMsg	("! ERROR: subdiv #%d has more than %d faces (%d)",MODEL_ID,2*c_SS_HighVertLimit,(*it)->size());
		}
	};
}

void Face2OGF_Vertices( const Face &FF, OGF_Vertex	V[3] ) 
{
	for (u32 fv=0; fv<3; fv++)
	{
		V[fv].P.set	(FF.v[fv]->P);
		V[fv].N.set	(FF.v[fv]->N); 
		V[fv].T		= FF.basis_tangent[fv];
		V[fv].B		= FF.basis_binormal[fv];
		V[fv].Color	= FF.v[fv]->C;
	}
	
	// Normal order
	svector<_TCF,2>::const_iterator TC=FF.tc.begin(); 
	for (;TC!=FF.tc.end(); TC++)
	{
		V[0].UV.push_back(TC->uv[0]);
		V[1].UV.push_back(TC->uv[1]);
		V[2].UV.push_back(TC->uv[2]);
	}
}

void OGF_AddFace( OGF &ogf, const Face& FF, bool _tc_ )
{
	OGF_Vertex	V[3];
	// Geometry
	Face2OGF_Vertices( FF, V );
	// build face
	TRY				(ogf._BuildFace(V[0],V[1],V[2],_tc_));
	V[0].UV.clear();V[1].UV.clear();V[2].UV.clear();
}

void BuildOGFGeom( OGF &ogf, const vecFace& faces, bool _tc_ )
{
	for (vecFaceCit Fit=faces.begin(); Fit!=faces.end(); Fit++)
	{
		Face*	FF = *Fit;
		R_ASSERT(FF);
		OGF_AddFace( ogf, *FF, _tc_ );
	}
}


void CBuild::Flex2OGF()
{
	float p_total	= 0;
	float p_cost	= 1/float(g_XSplit.size());

	validate_splits	();

	g_tree.clear	();
	g_tree.reserve	(4096);

	clMsg("Splits to convert: %u", g_XSplit.size() );
	 
	// for (auto SV  = 0 ; SV< g_XSplit.size(); SV++)
	xrCriticalSection cs;

	int ProgressID = 0;

	xr_parallel_for(size_t(0), size_t(g_XSplit.size()), [&] ( size_t SV )
	{
		auto& faces = g_XSplit[SV];

		Progress( float (SV) / float(g_XSplit.size()) );

		OGF*		pOGF	= new OGF ();
		Face*		F		= (* faces->begin() );			// first face
		b_material*	M		= &(materials()[F->dwMaterial]);	// and it's material
		R_ASSERT	(F && M);
 
		try 
		{
			// Common data
			pOGF->Sector		= M->sector;
			pOGF->material		= F->dwMaterial;
			
			// Collect textures
			OGF_Texture			T;
			//pOGF->shader		= M->shader;
			//pOGF->shader_xrlc	= &F->Shader();
			
			TRY(T.name			= textures()[M->surfidx].name);
			TRY(T.pBuildSurface	= &(textures()[M->surfidx]));
			TRY(pOGF->textures.push_back(T));
			
			try {
				if (F->hasImplicitLighting())
				{
					// specific lmap
					string_path		tn;
					xr_strconcat(tn,*T.name,"_lm.dds");
					T.name			= tn;
					T.pBuildSurface		= T.pBuildSurface;	// Leave surface intact
					R_ASSERT		(pOGF);
					pOGF->textures.push_back(T);
				} else {
					// If lightmaps persist
					CLightmap*	LM	= F->lmap_layer;
					if (LM)	
					{
						string_path	fn;
						xr_sprintf		(fn,"%s_1",LM->lm_texture.name); 
						T.name		= fn;
						T.pBuildSurface	= &(LM->lm_texture);
						R_ASSERT	(T.pBuildSurface);
						R_ASSERT	(pOGF);
						pOGF->textures.push_back(T);					 
						xr_sprintf		(fn,"%s_2",LM->lm_texture.name); 
						T.name		= fn;
						pOGF->textures.push_back(T);
					}
				}
			} 
			catch (...)
			{ 
				Msg("* ERROR: Flex2OGF, model# %d, *textures*", SV);
			}
			
		
			// Collect faces & vertices
			F->CacheOpacity	();
 			bool	_tc_	= !(F->flags.bOpaque);
		
			try 
			{
				BuildOGFGeom( *pOGF, *faces, _tc_ );
			} 
			catch (...)
			{  
				Msg("* ERROR: Flex2OGF, model# %d, *faces*",SV);
			}
		} 
		catch (...)
		{
			Msg("* ERROR: Flex2OGF, 1st part, model# %d",SV);
		}
 	
		try
		{
  			pOGF->Optimize						();
  			pOGF->CalcBounds					();
  			// pOGF->MakeProgressive	(c_PM_MetricLimit_static);
 			// pOGF->Stripify						();
		}
		catch (...)
		{
			Msg("* ERROR: Flex2OGF, 2nd part, model# %d", SV);
		}
 
		cs.Enter();
		g_tree.push_back(pOGF);
		ProgressID++;
		Progress(float(ProgressID) / float(g_XSplit.size()));

		if (ProgressID % 256 == 0)
			clMsg("Progress: %u/%u", ProgressID, g_XSplit.size());
		cs.Leave();
	}
	);

	for (auto it : g_XSplit)
	{
		if (it != nullptr)
			xr_delete(it);
	}
	g_XSplit.clear	();
}

void CBuild::SaveOGF()
{
	return; // ме дндекюмн

 	u32 BaseID = 0;

	u32 start = 0;
 	size_t GBs		= 2 * 1024 * 1024 * 1024;
	 
	int INDEX_FILE	= 0;
	while (true)
	{
		if (start >= g_tree.size() || INDEX_FILE > 4)
			break;

		string_path p_ref;
 		sprintf_s(p_ref, "%s\\build.geom_%u", path, INDEX_FILE);


		IWriter* write_ogf_ref = FS.w_open(p_ref);
		u32 ID = start;

		write_ogf_ref->open_chunk(9999);

		for (; ID < g_tree.size(); ID++)
		{
			OGF_Reference* ORef = smart_cast<OGF_Reference*> (g_tree[ID]);
			if (ORef)
			{
				ORef->SaveForCompile(write_ogf_ref);
				clMsg("BaseINDEX: %u/%u, SizeMU: %llu", ID, g_tree.size(), write_ogf_ref->tell());
 			}
			

			if (write_ogf_ref->tell() > GBs)
 				break;
 		}

 		write_ogf_ref->close_chunk();
		FS.w_close(write_ogf_ref);
		 
		INDEX_FILE++;
		start = ID;
	}
}

size_t CBuild::GetTreeSize()
{
	size_t treeOgf = 0;
	for (auto& tree : g_tree)
	{
		auto P = smart_cast<OGF*> ( tree );
		
		if (P != nullptr)
		treeOgf += P->Sizeof();
	}
	return treeOgf;
}
