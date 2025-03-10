#include "StdAfx.h"
//#include "cl_collector.h"
#include "Build.h"
#include "../xrLC_Light/xrMU_Model.h"
#include "../xrLC_Light/xrMU_Model_Reference.h"

#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../../xrCDB/xrCDB.h"
#include "../xrLC_Light/xrFace.h"

//.#include "communicate.h"

CDB::MODEL*	RCAST_Model	= 0;

IC bool				FaceEqual(Face& F1, Face& F2)
{
	// Test for 6 variations
	if ((F1.v[0]==F2.v[0]) && (F1.v[1]==F2.v[1]) && (F1.v[2]==F2.v[2])) return true;
	if ((F1.v[0]==F2.v[0]) && (F1.v[2]==F2.v[1]) && (F1.v[1]==F2.v[2])) return true;
	if ((F1.v[2]==F2.v[0]) && (F1.v[0]==F2.v[1]) && (F1.v[1]==F2.v[2])) return true;
	if ((F1.v[2]==F2.v[0]) && (F1.v[1]==F2.v[1]) && (F1.v[0]==F2.v[2])) return true;
	if ((F1.v[1]==F2.v[0]) && (F1.v[0]==F2.v[1]) && (F1.v[2]==F2.v[2])) return true;
	if ((F1.v[1]==F2.v[0]) && (F1.v[2]==F2.v[1]) && (F1.v[0]==F2.v[2])) return true;
	return false;
}

void SaveUVM			(LPCSTR fname, xr_vector<b_rc_face>& vm)
{
	IWriter* W			= FS.w_open(fname);
	string256 tmp;
	// vertices
	for (u32 v_idx=0; v_idx<vm.size(); v_idx++){
		b_rc_face& rcf	= vm[v_idx];
		xr_sprintf			(tmp,"f %d %d [%3.2f,%3.2f]-[%3.2f,%3.2f]-[%3.2f,%3.2f]",rcf.dwMaterial,rcf.dwMaterialGame,
						rcf.t[0].x,rcf.t[0].y, rcf.t[1].x,rcf.t[1].y, rcf.t[2].x,rcf.t[2].y);
		W->w_string		(tmp);
	}
	FS.w_close	(W);
}

#include <../xrLC_Light/xrDeflector.h>

void CBuild::BuildRapid		(BOOL bSaveForOtherCompilers)
{
	if (lc_global_data()->GetIsIntelUse())
	{
		IntelEmbereLOAD(bSaveForOtherCompilers);
		return;
	};

	float	p_total			= 0;
	float	p_cost			= 1.f/(lc_global_data()->g_faces().size());

	
	lc_global_data()->destroy_rcmodel();

	Status			("Converting faces...");
	for				(u32 fit=0; fit<lc_global_data()->g_faces().size(); fit++)	lc_global_data()->g_faces()[fit]->flags.bProcessed = false;


	CDB::CollectorPacked	CL(scene_bb, (int)lc_global_data()->g_vertices().size(), (int)lc_global_data()->g_faces().size());

	xr_vector<Face*>			adjacent_vec;
	adjacent_vec.reserve		(6*2*3);
	 
	std::for_each(lc_global_data()->g_faces().begin(), lc_global_data()->g_faces().end(), [&](Face* F)
		{
			const Shader_xrLC& SH = F->Shader();

			if (!SH.flags.bLIGHT_CastShadow)
				return;

			b_material& M = lc_global_data()->materials()[F->dwMaterial];

			// Collect
			adjacent_vec.clear();
			for (int vit = 0; vit < 3; ++vit)
			{
				Vertex* V = F->v[vit];
				for (u32 adj = 0; adj < V->m_adjacents.size(); adj++)
				{
					adjacent_vec.push_back(V->m_adjacents[adj]);
				}
			}

			std::sort(adjacent_vec.begin(), adjacent_vec.end());
			adjacent_vec.erase(std::unique(adjacent_vec.begin(), adjacent_vec.end()), adjacent_vec.end());

			// Unique
			BOOL			bAlready = FALSE;

			for (u32 ait = 0; ait < adjacent_vec.size(); ++ait)
			{
				Face* Test = adjacent_vec[ait];
				if (Test == F)
					continue;
				if (!Test->flags.bProcessed)
					continue;
				if (FaceEqual(*F, *Test))
				{
					bAlready = TRUE;
					break;
				}
			}

			//
			if (!bAlready)
			{
				F->flags.bProcessed = true;
				CL.add_face_D(F->v[0]->P, F->v[1]->P, F->v[2]->P, convert_nax(F), F->sm_group); //ThreadID
			}

		});

	Status("Models...");
	std::for_each(mu_refs().begin(), mu_refs().end(), [&](xrMU_Reference* ref) 
		{
			ref->export_cform_rcast(CL);
		});
 


	// "Building tree..
	Status					("Building search tree...");
	lc_global_data()->create_rcmodel(CL);

	extern void SaveAsSMF			(LPCSTR fname, CDB::CollectorPacked& CL);
	
	// save source SMF
	string_path				fn;

	bool					keep_temp_files = !!strstr(Core.Params,"-keep_temp_files");
	if (g_params().m_quality!=ebqDraft)
	{
		if (keep_temp_files)
			SaveAsSMF		(xr_strconcat(fn,pBuild->path,"build_cform_source.smf"),CL);
	}

	// Saving for AI/DO usage
	if (bSaveForOtherCompilers)
	{
		Status					("Saving...");
		string_path				fn;

		IWriter*		MFS		= FS.w_open	(xr_strconcat(fn,pBuild->path,"build.cform"));
		xr_vector<b_rc_face>	rc_faces;
		rc_faces.resize			(CL.getTS());
		// Prepare faces
		for (u32 k=0; k<CL.getTS(); k++){
			CDB::TRI& T			= CL.getT( k );
			base_Face* F		= convert_nax(T.dummy);
			b_rc_face& cf		= rc_faces[k];
			cf.dwMaterial		= F->dwMaterial;
			cf.dwMaterialGame	= F->dwMaterialGame;
			Fvector2*	cuv		= F->getTC0	();
			cf.t[0].set			(cuv[0]);
			cf.t[1].set			(cuv[1]);
			cf.t[2].set			(cuv[2]);
		}
		if (g_params().m_quality!=ebqDraft) {
			if (keep_temp_files)
				SaveUVM			(xr_strconcat(fn,pBuild->path,"build_cform_source.uvm"),rc_faces);
		}

		MFS->open_chunk			(0);

		// Header
		hdrCFORM hdr;
		hdr.version				= CFORM_CURRENT_VERSION;
		hdr.vertcount			= (u32)CL.getVS();
		hdr.facecount			= (u32)CL.getTS();
		hdr.aabb				= scene_bb;
		MFS->w					(&hdr,sizeof(hdr));

		// Data
		MFS->w					(CL.getV(),(u32)CL.getVS()*sizeof(Fvector));
		MFS->w					(CL.getT(),(u32)CL.getTS()*sizeof(CDB::TRI));
		MFS->close_chunk		();

		MFS->open_chunk			(1);
		MFS->w					(&*rc_faces.begin(),(u32)rc_faces.size()*sizeof(b_rc_face));
		MFS->close_chunk		();

		FS.w_close(MFS);
	}
}
