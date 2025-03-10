#include "stdafx.h"

#include "xrMU_Model_Reference.h"
#include "xrLC_GlobalData.h"
#include "xrMU_Model.h"

#include "../../xrCDB/xrCDB.h"
#include "../Shader_xrLC.h"
#include "xrFace.h"
 

void xrMU_Reference::Load( IReader& F, xr_vector<xrMU_Model*>& mu_models )
{
	b_mu_reference		R;
	F.r					(&R,sizeof(R));
	model				= mu_models[R.model_index];
	xform				= R.transform;
	flags				= R.flags;
	sector				= R.sector;

	c_scale.rgb.set		(1,1,1);
	c_scale.hemi		= 1;
	c_scale.sun			= 1;
	c_bias.rgb.set		(0,0,0);
	c_bias.hemi			= 0;
	c_bias.sun			= 0;
}

  
void xrMU_Reference::export_cform_game(CDB::CollectorPacked& CL)
{
	// Collecting data
	xrMU_Model::v_faces*	cfFaces		= new xrMU_Model::v_faces();
	xrMU_Model::v_vertices*	cfVertices	= new xrMU_Model::v_vertices();
	{
		xr_vector<bool>	cfVertexMarks;
		cfVertexMarks.assign(model->m_vertices.size(),false);

		std::sort			(model->m_vertices.begin(),model->m_vertices.end());

		// faces and mark vertices
		cfFaces->reserve	(model->m_faces.size());
		for (xrMU_Model::v_faces_it I=model->m_faces.begin(); I!=model->m_faces.end(); I++)
		{
			_face* F = *I;
			if (F->Shader().flags.bCollision) 
			{
				cfFaces->push_back	(F);

				for (u32 vit=0; vit<3; vit++)
				{
					u32 g_id		=  u32(std::lower_bound
						(
							model->m_vertices.begin(),model->m_vertices.end(),F->v[vit]
						) 
						- model->m_vertices.begin	());
					cfVertexMarks	[g_id] = true;
				}
			}
		}

		// verts
		cfVertices->reserve	(model->m_vertices.size());
		std::sort			(cfFaces->begin(),cfFaces->end());
		for (u32 V=0; V<model->m_vertices.size(); V++)
			if (cfVertexMarks[V]) cfVertices->push_back(model->m_vertices[V]);
	}

	// Collect faces
	u32	Offset			= (u32)CL.getTS();
	for (xrMU_Model::v_faces_it F = cfFaces->begin(); F!=cfFaces->end(); F++)
	{
		_face*	T = *F;
		
		// xform
		Fvector					P[3];
		xform.transform_tiny	(P[0],T->v[0]->P);
		xform.transform_tiny	(P[1],T->v[1]->P);
		xform.transform_tiny	(P[2],T->v[2]->P);

		CL.add_face				( P[0], P[1], P[2], T->dwMaterialGame, sector, T->sm_group);
	}

	xr_delete		(cfFaces);
	xr_delete		(cfVertices);
}

void xrMU_Reference::export_cform_rcast(CDB::CollectorPacked& CL)
{
	model->export_cform_rcast(CL,xform);
}
 
void xrMU_Reference::export_cform_rcast_new(xr_vector<FaceDataIntel>& faces)
{
	model->export_cform_rcast_new(faces, xform);
}
