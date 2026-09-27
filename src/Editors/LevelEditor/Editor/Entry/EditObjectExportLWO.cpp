#include "stdafx.h"
#include "../Utils/FS2.h"

int FindLPCSTR(LPCSTRVec& vec, const char* key){
	for (LPCSTRIt it=vec.begin(); it!=vec.end(); it++)
		if (0==strcmp(*it,key)) return it-vec.begin();
	return -1;
}

bool CEditableObject::ExportLWO(const char* fname)
{
	CLWMemoryStream* F = new CLWMemoryStream();

	LPCSTRVec images;

	F->begin_save();
	// tags
	F->open_chunk(ID_TAGS);
	for (SurfaceIt s_it = m_Surfaces.begin(); s_it != m_Surfaces.end(); s_it++)
	{
		CSurface* S = *s_it;
		F->w_stringZ(S->m_Name.c_str());
		S->tag = s_it - m_Surfaces.begin();
		if (FindLPCSTR(images, S->m_Texture.c_str()) < 0)
		{
			images.push_back(S->m_Texture.c_str());
		}
	}
	F->close_chunk();
	// images
	for (LPCSTRIt im_it = images.begin(); im_it != images.end(); im_it++)
	{
		F->open_chunk(ID_CLIP);
		F->w_u32(im_it - images.begin());
		F->open_subchunk(ID_STIL);
		F->w_stringZ(*im_it);
		F->close_subchunk();
		F->close_chunk();
	}
	// surfaces
	for (auto s_it = m_Surfaces.begin(); s_it != m_Surfaces.end(); s_it++)
	{
		CSurface* S = *s_it;
		int im_idx = FindLPCSTR(images, S->m_Texture.c_str());
		R_ASSERT(im_idx >= 0);
		const char* vm_name = S->m_VMap.c_str();
		F->Wsurface(S->m_Name.c_str(), S->m_Flags.is(CSurface::sf2Sided), (u16)im_idx, (vm_name && vm_name[0]) ? vm_name : "Texture", S->m_ShaderName.c_str(), S->m_ShaderXRLCName.c_str());
	}
	// meshes/layers
	for (EditMeshIt mesh_it = m_Meshes.begin(); mesh_it != m_Meshes.end(); mesh_it++)
	{
		CEditableMesh* MESH = *mesh_it;
		F->w_layer(u16(mesh_it - m_Meshes.begin()), MESH->m_Name.c_str());
		// bounding box
		F->open_chunk(ID_BBOX);
		F->w_vector(MESH->m_Box.min);
		F->w_vector(MESH->m_Box.max);
		F->close_chunk();
		// points
		F->open_chunk(ID_PNTS);
		for (u32 point_id = 0; point_id < MESH->m_Vertices.size(); point_id++)
		{
			F->w_vector(MESH->m_Vertices.data()[point_id]);
		}
		F->close_chunk();
		// polygons
		F->open_chunk(ID_POLS);
		F->w_u32(ID_FACE);
		for (u32 f_id = 0; f_id < MESH->m_Faces.size(); f_id++)
		{
			F->w_face3(MESH->m_Faces.data()[f_id].pv[0].pindex, MESH->m_Faces.data()[f_id].pv[1].pindex, MESH->m_Faces.data()[f_id].pv[2].pindex);
		}
		F->close_chunk();
		// surf<->face
		F->open_chunk(ID_PTAG);
		F->w_u32(ID_SURF);
		for (SurfFacesPairIt sf_it = MESH->m_SurfFaces.begin(); sf_it != MESH->m_SurfFaces.end(); sf_it++)
		{
			IntVec& lst = sf_it->second;
			for (IntIt i_it = lst.begin(); i_it != lst.end(); i_it++)
			{
				F->w_vx(*i_it);
				F->w_u16(WORD(sf_it->first->tag));
			}
		}
		F->close_chunk();

		// VMap&vmad
		for (xr_unique_ptr<st_VMap>& VMap : MESH->m_VMaps)
		{
			F->begin_vmap(VMap->polymap, (VMap->type == vmtUV) ? ID_TXUV : ID_WGHT, VMap->dim, VMap->name.c_str());
			if (VMap->polymap)
			{
				for (int k = 0; k < VMap->size(); k++)
				{
					F->w_vmad(VMap->vindices[k], VMap->pindices[k], VMap->dim, VMap->getVMdata(k));
				}
			}
			else
			{
				for (int k = 0; k < VMap->size(); k++)
				{
					F->w_vmap(VMap->vindices[k], VMap->dim, VMap->getVMdata(k));
				}
			}
			F->end_vmap();
		}
	}
	F->end_save(fname);

	xr_delete(F);

	return true;
}
