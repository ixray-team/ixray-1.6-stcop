//----------------------------------------------------
// file: StaticMesh.cpp
//----------------------------------------------------
                
#include "stdafx.h"


#include "EditMesh.h"
#include "EditObject.h"

#define EMESH_CURRENT_VERSION	      	0x0011
//----------------------------------------------------
#define EMESH_CHUNK_VERSION	        	0x1000
#define EMESH_CHUNK_MESHNAME        	0x1001
#define EMESH_CHUNK_FLAGS	        	0x1002
#define EMESH_CHUNK_NOT_USED_0        	0x1003
#define EMESH_CHUNK_BBOX	        	0x1004
#define EMESH_CHUNK_VERTS	        	0x1005
#define EMESH_CHUNK_FACES	        	0x1006
#define EMESH_CHUNK_VMAPS_0	        	0x1007
#define EMESH_CHUNK_VMREFS	        	0x1008
#define EMESH_CHUNK_SFACE				0x1009
#define EMESH_CHUNK_BOP					0x1010
#define EMESH_CHUNK_VMAPS_1		       	0x1011
#define EMESH_CHUNK_VMAPS_2		       	0x1012
#define EMESH_CHUNK_SG			       	0x1013
#define EMESH_CHUNK_NORMALS             0x1014

void CEditableMesh::SaveMesh(IWriter& F)
{
	F.open_chunk	(EMESH_CHUNK_VERSION);
	F.w_u16       	(EMESH_CURRENT_VERSION);
	F.close_chunk  	();

	F.open_chunk	(EMESH_CHUNK_MESHNAME);
    F.w_stringZ		(m_Name);
	F.close_chunk   ();

	F.w_chunk		(EMESH_CHUNK_BBOX,&m_Box, sizeof(m_Box));
	F.w_chunk		(EMESH_CHUNK_FLAGS,&m_Flags,1);
	F.w_chunk		(EMESH_CHUNK_BOP,&m_Ops, sizeof(m_Ops));

	F.open_chunk	(EMESH_CHUNK_VERTS);
	F.w_u32			((u32)m_Vertices.size());
    F.w				(m_Vertices.data(), m_Vertices.size()*sizeof(Fvector));

	F.close_chunk     ();

	F.open_chunk	(EMESH_CHUNK_FACES);
	F.w_u32			((u32)m_Faces.size());
    F.w				(m_Faces.data(), m_Faces.size()*sizeof(st_Face));
	F.close_chunk  	();

    if (GetSmoothGroups())
    {
        F.open_chunk	(EMESH_CHUNK_SG);
        F.w				(GetSmoothGroups(), m_Faces.size()*sizeof(u32));
        F.close_chunk  	();
    }
	
	if (!m_Normals.empty())
	{
		F.open_chunk(EMESH_CHUNK_NORMALS);
		for (Fvector N : m_Normals)
		{
			N.x = -N.x;
			N.z = -N.z;
			F.w_fvector3(N);
		}
		F.close_chunk();
	}

	F.open_chunk	(EMESH_CHUNK_VMREFS);
	F.w_u32			(m_VMRefs.size());

	for (st_VMapPtLst& PTs : m_VMRefs)
	{
		int sz = (int)PTs.size();
		VERIFY(sz <= 255);
		F.w_u8((u8)sz);
		F.w(PTs.data(), sizeof(st_VMapPt) * sz);
	}

	F.close_chunk	();

	F.open_chunk	(EMESH_CHUNK_SFACE);
	F.w_u16			((u16)m_SurfFaces.size()); 	/* surface polygon count*/
	for (SurfFacesPairIt plp_it=m_SurfFaces.begin(); plp_it!=m_SurfFaces.end(); plp_it++)
    {
    	F.w_stringZ	(plp_it->first->m_Name.c_str()); 	/* surface name*/
    	IntVec& 	pol_lst = plp_it->second;
        F.w_u32		(pol_lst.size());		/* surface-polygon indices*/
        F.w			(&*pol_lst.begin(), sizeof(int)*pol_lst.size());
    }
	F.close_chunk     ();

	F.open_chunk	(EMESH_CHUNK_VMAPS_2);
	F.w_u32		(m_VMaps.size());
    for (VMapIt vm_it=m_VMaps.begin(); vm_it!=m_VMaps.end(); vm_it++)
    {
        F.w_stringZ	((*vm_it)->name);
        F.w_u8		((*vm_it)->dim);
		F.w_u8		((u8)(*vm_it)->polymap);
        F.w_u8		((*vm_it)->type);
        F.w_u32		((*vm_it)->size());
        F.w			((*vm_it)->getVMdata(), (*vm_it)->VMdatasize());
        F.w			((*vm_it)->getVIdata(), (*vm_it)->VIdatasize());
		if ((*vm_it)->polymap)
	        F.w		((*vm_it)->getPIdata(), (*vm_it)->PIdatasize());
    }
	F.close_chunk  	();
}

bool CEditableMesh::LoadMesh(IReader& F){
    u32 version=0;

    R_ASSERT(F.r_chunk(EMESH_CHUNK_VERSION,&version));
    if (version!=EMESH_CURRENT_VERSION){
        ELog.DlgMsg( mtError, "CEditableMesh: unsuported file version. Mesh can't load.");
        return false;
    }

    R_ASSERT(F.find_chunk(EMESH_CHUNK_MESHNAME));
	F.r_stringZ		(m_Name);

    R_ASSERT(F.r_chunk(EMESH_CHUNK_BBOX,&m_Box));
    R_ASSERT(F.r_chunk(EMESH_CHUNK_FLAGS,&m_Flags));
    F.r_chunk(EMESH_CHUNK_BOP,&m_Ops);

    R_ASSERT(F.find_chunk(EMESH_CHUNK_VERTS));
	{
	u32 vc			= F.r_u32();
    if (vc<3){
        Log				("!CEditableMesh: Vertices<3.");
     	return false;
    }
    m_Vertices.resize(vc);
	F.r					(m_Vertices.data(), vc*sizeof(Fvector));
	}

    R_ASSERT(F.find_chunk(EMESH_CHUNK_FACES));
	{
		u32 fc = F.r_u32();
		if (fc == 0)
		{
			Log("!CEditableMesh: Faces==0.");
			return false;
		}
		m_Faces.resize(fc);
		F.r(m_Faces.data(), fc * sizeof(st_Face));

		m_SmoothGroups.assign(fc, m_Flags.is(flSGMask) ? 0 : u32(-1));
		u32 sg_chunk_size = F.find_chunk(EMESH_CHUNK_SG);
		if (sg_chunk_size)
		{
			VERIFY(fc * sizeof(u32) == sg_chunk_size);
			F.r(m_SmoothGroups.data(), fc * sizeof(u32));
		}

		u32 normal_chunk_size = F.find_chunk(EMESH_CHUNK_NORMALS);
		if (normal_chunk_size)
		{
			VERIFY(fc * 3 * sizeof(Fvector) == normal_chunk_size);
			m_Normals.resize(fc * 3);
			F.r(m_Normals.data(), fc * 3 * sizeof(Fvector));

			for (size_t i = 0; i < fc * 3; i++)
			{
				m_Normals[i].x = -m_Normals[i].x;
				m_Normals[i].z = -m_Normals[i].z;
			}
		}
	}

    R_ASSERT(F.find_chunk(EMESH_CHUNK_VMREFS));
    m_VMRefs.resize		(F.r_u32());
    constexpr int sz_vmpt = sizeof(st_VMapPt);

	for (VMRefsIt r_it = m_VMRefs.begin(); r_it != m_VMRefs.end(); r_it++)
	{
		u8 cnt = F.r_u8();
		r_it->resize(cnt);
		F.r(r_it->data(), sz_vmpt * cnt);
	}

    R_ASSERT(F.find_chunk(EMESH_CHUNK_SFACE));
    string128 surf_name;
    u32 sface_cnt		= F.r_u16(); // surface-face count
    for (u32 sp_i=0; sp_i<sface_cnt; sp_i++)
    {
        F.r_stringZ		(surf_name,sizeof(surf_name));
        int surf_id;
        CSurface* surf	= m_Parent->FindSurfaceByName(surf_name, &surf_id); VERIFY(surf);
        IntVec&			face_lst = m_SurfFaces[surf];
        face_lst.resize	(F.r_u32());
        if (face_lst.empty())
        {
	        Log			("!Empty surface found: %s",surf->m_Name.c_str());
    	 	return false;
        }
        F.r				(&*face_lst.begin(), face_lst.size()*sizeof(int));
        std::sort		(face_lst.begin(),face_lst.end());
    }

    if(F.find_chunk(EMESH_CHUNK_VMAPS_2))
	{
		m_VMaps.resize	(F.r_u32());
		for (VMapIt vm_it=m_VMaps.begin(); vm_it!=m_VMaps.end(); vm_it++)
        {
			*vm_it		= xr_make_unique<st_VMap>();
			F.r_stringZ	((*vm_it)->name);
			(*vm_it)->dim 	= F.r_u8();
			(*vm_it)->polymap=F.r_u8();
			(*vm_it)->type	= F.r_u8();
			(*vm_it)->resize(F.r_u32());
			F.r			((*vm_it)->getVMdata(), (*vm_it)->VMdatasize());
			F.r			((*vm_it)->getVIdata(), (*vm_it)->VIdatasize());
			if ((*vm_it)->polymap)
				F.r		((*vm_it)->getPIdata(), (*vm_it)->PIdatasize());
		}
	}else
	{
		if(F.find_chunk(EMESH_CHUNK_VMAPS_1))
		{
			m_VMaps.resize	(F.r_u32());
			for (VMapIt vm_it=m_VMaps.begin(); vm_it!=m_VMaps.end(); vm_it++)
			{
				*vm_it		= xr_make_unique<st_VMap>();
				F.r_stringZ	((*vm_it)->name);
				(*vm_it)->dim 	= F.r_u8();
				(*vm_it)->type	= F.r_u8();
				(*vm_it)->resize(F.r_u32());
				F.r			((*vm_it)->getVMdata(), (*vm_it)->VMdatasize() );
			}
		}else
		{
			R_ASSERT(F.find_chunk(EMESH_CHUNK_VMAPS_0));
			m_VMaps.resize	(F.r_u32());
			for (VMapIt vm_it=m_VMaps.begin(); vm_it!=m_VMaps.end(); vm_it++)
			{
				*vm_it		= xr_make_unique<st_VMap>();
				F.r_stringZ	((*vm_it)->name);
				(*vm_it)->dim 	= 2;
				(*vm_it)->type	= vmtUV;
				(*vm_it)->resize(F.r_u32());
				F.r			((*vm_it)->getVMdata(), (*vm_it)->VMdatasize() );
			}
		}
		// update vmaps
		RebuildVMaps();
	}

	if (!m_Flags.is(flOptimized))
	{
		OptimizeMesh(false);
		RebuildVMaps();
		m_Flags.set(flOptimized, TRUE);
		m_Parent->bNeedResave = true;
	}

#if 1
    if (!EPrefs->object_flags.is(epoDeffLoadRB))
    {
        GenerateFNormals	();
        GenerateAdjacency	();
	    GenerateVNormals	(nullptr);


		extern ECORE_API xrCriticalSection temp_render_lock;
		xrCriticalSectionGuard guard_lock(temp_render_lock);

		GenerateRenderBuffers();
        UnloadFNormals		();
        UnloadAdjacency		();
	    UnloadVNormals		();
    }
    
    GenerateCFModel();     

#endif

	return 			true;
}
//----------------------------------------------------

