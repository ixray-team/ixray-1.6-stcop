#include "stdafx.h"
#include "xrMU_InstancedGroup.h"

#include "xrMU_Model.h"
#include "xrEngine/Shader_xrLC.h"

void xrMU_InstancedGroup::export_cform_game(CDB::CollectorPacked& CL)
{
	// Collecting data
	xrMU_Model::v_faces*	cfFaces		= new xrMU_Model::v_faces();
	xrMU_Model::v_vertices*	cfVertices	= new xrMU_Model::v_vertices();
	for (auto& Slot : Slots)
	{
		{
			xr_vector<bool>	cfVertexMarks;
			cfVertexMarks.assign(Slot.Model->m_vertices.size(),false);

			std::sort(Slot.Model->m_vertices.begin(),Slot.Model->m_vertices.end());

			// faces and mark vertices
			cfFaces->reserve	(Slot.Model->m_faces.size());
			for (auto F : Slot.Model->m_faces)
			{
				if (F->Shader().flags.bCollision) 
				{
					cfFaces->push_back	(F);

					for (u32 vit=0; vit<3; vit++)
					{
						u32 g_id = u32(std::lower_bound
							(
								Slot.Model->m_vertices.begin(),Slot.Model->m_vertices.end(),F->v[vit]
							) 
							- Slot.Model->m_vertices.begin	());
						cfVertexMarks[g_id] = true;
					}
				}
			}

			// verts
			cfVertices->reserve	(Slot.Model->m_vertices.size());
			std::sort			(cfFaces->begin(),cfFaces->end());
			for (u32 V=0; V<Slot.Model->m_vertices.size(); V++)
			{
				if (cfVertexMarks[V])
				{
					cfVertices->push_back(Slot.Model->m_vertices[V]);
				}
			}
		}

		// Collect faces
		for (auto& xform : Slot.Instances)
		{
			for (xrMU_Model::v_faces_it F = cfFaces->begin(); F!=cfFaces->end(); F++)
			{
				auto T = *F;
		
				// xform
				Fvector P[3];
				xform.transform_tiny(P[0],T->v[0]->P);
				xform.transform_tiny(P[1],T->v[1]->P);
				xform.transform_tiny(P[2],T->v[2]->P);

				CL.add_face( P[0], P[1], P[2], T->dwMaterialGame, Sector, T->flags.bSharedMaterial, T->sm_group);
			}
		}
	}

	xr_delete(cfFaces);
	xr_delete(cfVertices);
}