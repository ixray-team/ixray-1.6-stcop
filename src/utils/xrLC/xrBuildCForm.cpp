#include "StdAfx.h"
#include "Build.h"
#include "../xrLC_Light/xrMU_Model.h"
#include "../xrLC_Light/xrMU_Model_Reference.h"

#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"

#include "../../xrCore/Collision/xrCDB.h"
#include "../xrLC_Light/embree_raytracing/EmbreeGeometryBuilder.h"


extern void SimplifyCFORM		(CDB::CollectorPacked& CL);
void CBuild::BuildCForm	()
{
	// Collecting data
	Status		("CFORM: creating...");
  
 	// Заполняем faces*
	TriangleContainer container;
  	for (auto TRI : lc_global_data()->g_faces())
	{
		if (TRI->Shader().flags.bCollision)
 			container.AddFace(TRI, TRI->v[0]->P, TRI->v[1]->P, TRI->v[2]->P);
	}
 	for (auto& ref : mu_refs())
	{
		xr_vector<FaceDataIntel> temp_buffer;
		ref->export_cform_game_new(temp_buffer);
		for (auto& FaceIntel : temp_buffer)
		{
			Face* F = (Face*)FaceIntel.ptr;
			container.AddFace(F, FaceIntel.v1, FaceIntel.v2, FaceIntel.v3);
		}
	}

	// Чистим дубликаты
	container.RemoveDublicates();
	container.RemoveDublicatesFaces();


	// Расщитуем BBox Уровня
	Fbox BB;
	BB.invalidate	();
	for (auto V : container.verts_v)
		BB.modify( V );

	// Saving
	string_path		fn;
	IWriter*		MFS	= FS.w_open	(xr_strconcat(fn,pBuild->path,"level.cform"));
	Status			("Saving...");

	// Header
	hdrCFORM hdr;
	hdr.version		= CFORM_CURRENT_VERSION;
	hdr.vertcount	= (u32)container.vertex_cnt();
	hdr.facecount	= (u32)container.faces_cnt();
	hdr.aabb		= BB;
	MFS->w			(&hdr,sizeof(hdr));
 
	// Data
	MFS->w			(container.vertex().data(), (u32)container.vertex_cnt() * sizeof(Fvector));
	for (auto TRI : container.faces())
	{
		CDB::TRI T = TRI.Get();
		MFS->w(&T, sizeof(CDB::TRI));
	}
	FS.w_close(MFS);



	// Clear pDeflector (it is stored in the same memory space with dwMaterialGame)
	for (vecFaceIt I=lc_global_data()->g_faces().begin(); I != lc_global_data()->g_faces().end(); I++)
	{
		Face* F			= *I;
		F->pDeflector	= NULL;
	}

}

void CBuild::BuildPortals(IWriter& fs)
{
	fs.w_chunk(fsL_PORTALS, &*portals.begin(), (u32)portals.size() * sizeof(b_portal));
}
