#include "stdafx.h"
#include "xrMU_Model.h"
#include "xrMU_Model_Reference.h"

#include "../../xrCore/Collision/xrCDB.h"
#include "../Shader_xrLC.h"

u32 convert_nax(base_Face* F);

void xrMU_Model::export_cform_rcast	(CDB::CollectorPacked& CL, Fmatrix& xform)
{
	for		(u32 fit=0; fit<m_faces.size(); fit++)	m_faces[fit]->flags.bProcessed = false;

	for (auto F : m_faces)
	{
 		const Shader_xrLC&	SH		= F->Shader();
		if (!SH.flags.bLIGHT_CastShadow)		continue;

		// Unique
		F->flags.bProcessed		= true;
		Fvector					P[3];
		xform.transform_tiny	(P[0],F->v[0]->P);
		xform.transform_tiny	(P[1],F->v[1]->P);
		xform.transform_tiny	(P[2],F->v[2]->P);
		CL.add_face_D			(P[0],P[1],P[2], convert_nax(F), F->sm_group);//
	}
}

void xrMU_Model::export_cform_rcast_new(xr_vector<FaceDataEmbree>& faces, Fmatrix& xform)
{
	for (v_faces_it it = m_faces.begin(); it != m_faces.end(); it++)
	{
		_face* F = (*it);
		const Shader_xrLC& SH = F->Shader();
		if (!SH.flags.bLIGHT_CastShadow) continue;

 		Fvector					P[3];
		xform.transform_tiny(P[0], F->v[0]->P);
		xform.transform_tiny(P[1], F->v[1]->P);
		xform.transform_tiny(P[2], F->v[2]->P);

		FaceDataEmbree data;
		data.v1 = P[0];
		data.v2 = P[1];
		data.v3 = P[2];
		data.ptr = F;
		faces.push_back(data);
	}
}

xr_vector<FaceDataEmbree>& xrMU_Model::EmbreeInstanceCopy()
{
	thread_local xr_vector<FaceDataEmbree> faces;
	faces.clear();
	for (auto& F : m_faces)
	{
 		const Shader_xrLC& SH = F->Shader();
		if (!SH.flags.bLIGHT_CastShadow) continue;

		FaceDataEmbree data;
		data.v1 = F->v[0]->P;
		data.v2 = F->v[1]->P;
		data.v3 = F->v[2]->P;
		data.ptr = F;
		faces.push_back(data);
	}
	return faces;
}