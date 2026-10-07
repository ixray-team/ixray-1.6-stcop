//----------------------------------------------------
// file: StaticMesh.cpp
//----------------------------------------------------

#include "stdafx.h"


#include "EditMesh.h"
#include "EditObject.h"
#include "../xrEngine/bone.h"

#include "face_smoth_flags.h"
#include "../Public/itterate_adjacents.h"
#include "itterate_adjacents_dynamic.h"
#include "UI_ToolsCustom.h"

CEditableMesh::~CEditableMesh()
{
	Clear();
    R_ASSERT2(!m_RenderBuffers,"Render buffer still referenced.");
}

void CEditableMesh::Construct()
{
	m_Box.set		(0,0,0,0,0,0);
	m_Flags.assign	(flVisible);
    m_Name			= "";
    m_SVertInfl		= 0;
	m_FNormalsRefs	= 0;
	m_VNormalsRefs	= 0;
	m_AdjsRefs		= 0;
	m_SVertRefs		= 0;
}

void CEditableMesh::Clear()           
{
	UnloadRenderBuffers	();
    UnloadAdjacency		();
    UnloadCForm			();
    UnloadFNormals		();
    UnloadVNormals		();
    UnloadSVertices		();
	m_SmoothGroups.clear();
	VERIFY				(m_FNormalsRefs==0 && m_VNormalsRefs==0 && m_AdjsRefs==0 && m_SVertRefs==0);

    m_Vertices.clear();
    m_Faces.clear();
	m_Normals.clear();
    m_VMaps.clear();
    m_SurfFaces.clear();
    m_VMRefs.clear();
}

void CEditableMesh::UnloadCForm     ()
{
	m_CFModel.reset();
}

void CEditableMesh::UnloadFNormals  (bool force)
{
	m_FNormalsRefs--;
	if (force||m_FNormalsRefs<=0) 	{ m_FaceNormals.clear(); m_FNormalsRefs=0; }
}
void CEditableMesh::UnloadVNormals  (bool force)
{
	m_VNormalsRefs--;
    if (force||m_VNormalsRefs<=0) 		{ m_VertexNormals.clear(); m_VNormalsRefs=0; }
}
void CEditableMesh::UnloadSVertices	(bool force)
{
	m_SVertRefs--;
    if (force||m_SVertRefs<=0) 		{ m_SVertices.clear(); m_SVertRefs=0; }
}
void CEditableMesh::UnloadAdjacency	(bool force)
{
	m_AdjsRefs--;
    if (force||m_AdjsRefs<=0) 		{ m_Adjs.reset(); m_AdjsRefs=0; }
}

void CEditableMesh::RecomputeBBox()
{
	if( m_Vertices.empty() ){
		m_Box.set(0,0,0, 0,0,0);
		return;
    }
	m_Box.set( m_Vertices[0], m_Vertices[0] );
	for(u32 k=1; k<m_Vertices.size(); k++)
		m_Box.modify(m_Vertices[k]);
}

void CEditableMesh::GenerateFNormals()
{
	m_FNormalsRefs++;
    if (!m_FaceNormals.empty())		return;
	m_FaceNormals.resize(m_Faces.size());

    // face normals
	for (u32 k=0; k<m_Faces.size(); k++)
        m_FaceNormals[k].mknormal(	m_Vertices[m_Faces[k].pv[0].pindex], 
									m_Vertices[m_Faces[k].pv[1].pindex], 
									m_Vertices[m_Faces[k].pv[2].pindex]);
}
bool CEditableMesh::m_bDraftMeshMode = false;

void CEditableMesh::GenerateVNormals(const Fmatrix* parent_xform, bool force)
{
	m_VNormalsRefs++;
	bool IsUsingNormals = !m_Normals.empty() && EPrefs->SmoothGroup == ESmoothGroup::Normals;
	if ((!m_VertexNormals.empty() || IsUsingNormals) && !force)
		return;

	m_VertexNormals.resize(m_Faces.size() * 3);

	// gen req    
	GenerateFNormals();
	GenerateAdjacency();

	if (EPrefs->SmoothGroup == ESmoothGroup::Edges)
	{
		for (u32 f_i = 0; f_i < m_Faces.size(); f_i++)
		{
			for (int k = 0; k < 3; k++)
			{
				Fvector& N = m_VertexNormals[f_i * 3 + k];
				IntVec& a_lst = (*m_Adjs)[m_Faces[f_i].pv[k].pindex];

				IntIt face_adj_it = std::find(a_lst.begin(), a_lst.end(), f_i);
				VERIFY(face_adj_it != a_lst.end());
				//
				N.set(m_FaceNormals[a_lst.front()]);
				if (m_bDraftMeshMode)
					continue;

				using  iterate_adj = itterate_adjacents< itterate_adjacents_params_dynamic<st_FaceVert> >;
				iterate_adj::recurse_tri_params p(N, m_SmoothGroups.data(), m_FaceNormals.data(), a_lst, m_Faces.data(), (u32)m_Faces.size());
				iterate_adj::RecurseTri(face_adj_it - a_lst.begin(), p);
				float len = N.magnitude();
				if (len > EPS_S)
				{
					N.div(len);
				}
				else
				{
					N.set(m_FaceNormals[a_lst.front()]);
				}
			}
		}
	}
	else
	{
		if (m_Flags.is(flSGMask))
		{
			for (u32 f_i = 0; f_i < m_Faces.size(); f_i++)
			{
				u32 sg = m_SmoothGroups[f_i];
				Fvector& FN = m_FaceNormals[f_i];
				for (int k = 0; k < 3; k++) {
					Fvector& N = m_VertexNormals[f_i * 3 + k];
					if (sg) {
						N.set(0, 0, 0);
						IntVec& a_lst = (*m_Adjs)[m_Faces[f_i].pv[k].pindex];
						VERIFY(a_lst.size());
						for (IntIt i_it = a_lst.begin(); i_it != a_lst.end(); i_it++)
							if (sg & m_SmoothGroups[*i_it]) N.add(m_FaceNormals[*i_it]);
						float len = N.magnitude();
						if (len > EPS_S)
						{
							N.div(len);
						}
						else
						{
							Msg("!Invalid smooth group found (MAX type). Object: '%s'. Vertex: [%3.2f, %3.2f, %3.2f]", m_Parent->m_LibName.c_str(), VPUSH(m_Vertices[m_Faces[f_i].pv[k].pindex]));
							N.set(m_FaceNormals[a_lst.front()]);
						}
					}
					else
					{
						N.set(FN);
					}
				}
			}
		}
		else
		{
			for (u32 f_i = 0; f_i < m_Faces.size(); f_i++)
			{
				u32 sg = m_SmoothGroups[f_i];
				Fvector& FN = m_FaceNormals[f_i];
				for (int k = 0; k < 3; k++)
				{
					Fvector& N = m_VertexNormals[f_i * 3 + k];
					if (sg != -1)
					{
						N.set(0, 0, 0);
						IntVec& a_lst = (*m_Adjs)[m_Faces[f_i].pv[k].pindex];
						VERIFY(a_lst.size());
						for (IntIt i_it = a_lst.begin(); i_it != a_lst.end(); i_it++)
						{
							if (sg != m_SmoothGroups[*i_it]) continue;
							N.add(m_FaceNormals[*i_it]);
						}
						float len = N.magnitude();
						if (len > EPS_S) {
							N.div(len);
						}
						else
						{
							Msg("!Invalid smooth group found (Maya type). Object: '%s'. Vertex: [%3.2f, %3.2f, %3.2f]", m_Parent->m_LibName.c_str(), VPUSH(m_Vertices[m_Faces[f_i].pv[k].pindex]));
							N.set(m_FaceNormals[a_lst.front()]);
						}
					}
					else
					{
						N.set(FN);
					}
				}
			}
		}
	}

	UnloadFNormals();
	UnloadAdjacency();
}

void CEditableMesh::AssignMesh(shared_str to_bone)
{
	xr_vector<int> Remap(m_VMaps.size(), -1);
	VMapVec NewVMaps;
	for (size_t i = 0; i < m_VMaps.size(); i++)
	{
		if (m_VMaps[i]->type == vmtWeight)
			continue;

		Remap[i] = (int)NewVMaps.size();
		NewVMaps.push_back(std::move(m_VMaps[i]));
	}

	const int WeightMapID = (int)NewVMaps.size();
	NewVMaps.push_back(xr_make_unique<st_VMap>(to_bone.c_str(), vmtWeight, false));
	st_VMap* WMap = NewVMaps.back().get();
	WMap->resize((int)m_Vertices.size());
	for (int i = 0; i < (int)m_Vertices.size(); i++)
	{
		WMap->getW(i) = 1.0f;
		WMap->vindices[i] = i;
	}

	for (st_VMapPtLst& RefList : m_VMRefs)
	{
		RefList.erase(std::remove_if(RefList.begin(), RefList.end(), [&Remap](const st_VMapPt& Pt) { return Remap[Pt.vmap_index] == -1; }), RefList.end());

		for (st_VMapPt& Pt : RefList)
			Pt.vmap_index = Remap[Pt.vmap_index];
	}

	for (const st_Face& Face : m_Faces)
	{
		for (int k = 0; k < 3; k++)
		{
			st_VMapPtLst& RefList = m_VMRefs[Face.pv[k].vmref];
			auto HasWeight = [WeightMapID](const st_VMapPt& Pt) { return Pt.vmap_index == WeightMapID; };
			if (std::find_if(RefList.begin(), RefList.end(), HasWeight) != RefList.end())
				continue;

			st_VMapPt& Pt = RefList.emplace_back();
			Pt.vmap_index = WeightMapID;
			Pt.index = Face.pv[k].pindex;
		}
	}

	m_VMaps = std::move(NewVMaps);

	u16 Bone = m_Parent->BoneIDByName(to_bone);
	R_ASSERT(Bone != BI_NONE);
	m_Parent->GetBone(Bone)->SetWMap(to_bone.c_str());

	UnloadSVertices(true);
	UnloadRenderBuffers();
}

void CEditableMesh::GenerateSVertices(u32 influence)
{
	if (!m_Parent->IsSkeleton())return;

    m_SVertRefs++;
    if (m_SVertInfl!=influence) UnloadSVertices(true);
    if (!m_SVertices.empty()) 	return;
	m_SVertices.resize(m_Faces.size()*3);
    m_SVertInfl			= influence;

    m_Parent->CalculateAnimation(nullptr);

    // generate normals
	GenerateFNormals	();
	GenerateVNormals	(nullptr);

    for (u32 f_id=0; f_id<m_Faces.size(); f_id++)
	{
        st_Face& F 		= m_Faces[f_id];

        for (int k=0; k<3; ++k)
		{
	    	st_SVert& SV = 	m_SVertices[f_id*3+k];
			const Fvector& N = !m_Normals.empty() && EPrefs->SmoothGroup == ESmoothGroup::Normals ? m_Normals[f_id * 3 + k] : m_VertexNormals[f_id * 3 + k];
            const st_FaceVert& fv = F.pv[k];
	    	const Fvector&  P = m_Vertices[fv.pindex];

			const st_VMapPtLst& VmPtLst = m_VMRefs[fv.vmref];

            st_VertexWB wb;
			for (u8 VmPtID = 0; VmPtID != VmPtLst.size(); ++VmPtID)
			{
				const st_VMap& VM = *m_VMaps[VmPtLst[VmPtID].vmap_index];
				if (VM.type == vmtWeight)
				{
					wb.push_back(st_WB(m_Parent->GetBoneIndexByWMap(VM.name.c_str()), VM.getW(VmPtLst[VmPtID].index)));

					if (wb.back().bone == BI_NONE)
					{
						ELog.DlgMsg(mtError, "Can't find bone assigned to weight map %s", *VM.name);
						FATAL("Editor crashed.");
						return;
					}
				}
				else if (VM.type == vmtUV)
				{
					SV.uv.set(VM.getUV(VmPtLst[VmPtID].index));
				}
			}

            VERIFY(m_SVertInfl<=4);
            
            wb.prepare_weights(m_SVertInfl);

            SV.offs	= P;
            SV.norm	= N;
            SV.bones.resize(wb.size());
            for (u8 k=0; k<(u8)SV.bones.size(); k++)
            	{
                	SV.bones[k].id	=	wb[k].bone;
                    SV.bones[k].w	=	wb[k].weight;
                }
        }
	}

    // restore active motion
	UnloadFNormals	();
	UnloadVNormals	();
}

void CEditableMesh::GenerateAdjacency()
{
	m_AdjsRefs++;
	if (m_Adjs)
	{
		return;
	}

	m_Adjs = xr_make_unique<AdjVec>();
	VERIFY(!m_Faces.empty());
	m_Adjs->resize(m_Vertices.size());

	for (u32 f_id = 0; f_id < m_Faces.size(); f_id++)
	{
		for (int k = 0; k < 3; k++)
		{
			(*m_Adjs)[m_Faces[f_id].pv[k].pindex].push_back(f_id);
		}
	}
}

CSurface* CEditableMesh::GetSurfaceByFaceID(u32 fid)
{
	R_ASSERT(fid < m_Faces.size());
	for (SurfFacesPairIt sp_it = m_SurfFaces.begin(); sp_it != m_SurfFaces.end(); sp_it++)
	{
		IntVec& face_lst = sp_it->second;
		IntIt f_it = std::lower_bound(face_lst.begin(), face_lst.end(), (int)fid);

		if ((f_it != face_lst.end()) && (*f_it == (int)fid))
		{
			return sp_it->first;
		}
	}
	return nullptr;
}

void CEditableMesh::GetFaceTC(u32 fid, const Fvector2* tc[3])
{
	R_ASSERT(fid < m_Faces.size());
	st_Face& F = m_Faces[fid];
	for (int k = 0; k < 3; k++)
	{
		st_VMapPt& vmr = m_VMRefs[F.pv[k].vmref][0];
		tc[k] = &(m_VMaps[vmr.vmap_index]->getUV(vmr.index));
	}
}

void CEditableMesh::GetFacePT(u32 fid, const Fvector* pt[3])
{
	R_ASSERT(fid<m_Faces.size());
	st_Face& F		= m_Faces[fid];

    for (int k=0; k<3; ++k)
    	pt[k] = &m_Vertices[F.pv[k].pindex];
}

int CEditableMesh::GetFaceCount(bool bMatch2Sided, bool bIgnoreOCC)
{
	static shared_str occ_name = "materials\\occ";
	int f_cnt = 0;
    for (SurfFacesPairIt sp_it=m_SurfFaces.begin(); sp_it!=m_SurfFaces.end(); sp_it++)
    {
    	CSurface* S = sp_it->first;
        if(S->m_GameMtlName== occ_name && bIgnoreOCC)
        	continue;
            
    	if (bMatch2Sided){
	    	if (S->m_Flags.is(CSurface::sf2Sided))	
            	f_cnt+=sp_it->second.size()*2;
    	    else												
            	f_cnt+=sp_it->second.size();
        }else{
        	f_cnt+=sp_it->second.size();
        }
	}
    return f_cnt;
}

float CEditableMesh::CalculateSurfaceArea(CSurface* surf, bool bMatch2Sided)
{
	SurfFacesPairIt sp_it 	= m_SurfFaces.find(surf);
    if (sp_it==m_SurfFaces.end()) return 0;
    float area				= 0;
    IntVec& 	pol_lst = sp_it->second;
    for (int k=0; k<int(pol_lst.size()); k++){
        st_Face& F		= m_Faces[pol_lst[k]];
        Fvector 		c,e01,e02;
        e01.sub			(m_Vertices[F.pv[1].pindex],m_Vertices[F.pv[0].pindex]);
        e02.sub			(m_Vertices[F.pv[2].pindex],m_Vertices[F.pv[0].pindex]);
        area			+= c.crossproduct(e01,e02).magnitude()/2.f;
    }
    if (bMatch2Sided&&sp_it->first->m_Flags.is(CSurface::sf2Sided)) area*=2;
    return area;
}

float CEditableMesh::CalculateSurfacePixelArea(CSurface* surf, bool bMatch2Sided)
{
	SurfFacesPairIt sp_it = m_SurfFaces.find(surf);
	if (sp_it == m_SurfFaces.end())
	{
		return 0;
	}

	float area = 0;
	IntVec& pol_lst = sp_it->second;

	for (int k = 0; k < int(pol_lst.size()); k++)
	{
		Fvector c, e01, e02;
		const Fvector2* tc[3];
		GetFaceTC(pol_lst[k], tc);
		e01.sub(Fvector().set(tc[1]->x, tc[1]->y, 0), Fvector().set(tc[0]->x, tc[0]->y, 0));
		e02.sub(Fvector().set(tc[2]->x, tc[2]->y, 0), Fvector().set(tc[0]->x, tc[0]->y, 0));
		area += c.crossproduct(e01, e02).magnitude() / 2.f;
	}
	if (bMatch2Sided && sp_it->first->m_Flags.is(CSurface::sf2Sided))
	{
		area *= 2;
	}
	return area;
}

int CEditableMesh::GetSurfFaceCount(CSurface* surf, bool bMatch2Sided)
{
	SurfFacesPairIt sp_it = m_SurfFaces.find(surf);
    if (sp_it==m_SurfFaces.end()) return 0;
	int f_cnt = sp_it->second.size();
    if (bMatch2Sided&&sp_it->first->m_Flags.is(CSurface::sf2Sided)) f_cnt*=2;
    return f_cnt;
}

int CEditableMesh::FindVMapByName(VMapVec& vmaps, const char* name, u8 t, bool polymap)
{
	for (VMapIt vm_it=vmaps.begin(); vm_it!=vmaps.end(); vm_it++){               
		if (((*vm_it)->type==t)&&(stricmp((*vm_it)->name.c_str(),name)==0)&&(polymap==(*vm_it)->polymap)) return vm_it-vmaps.begin();
	}
	return -1;
}
//----------------------------------------------------------------------------

bool CEditableMesh::Validate()
{
	return true;
}
//----------------------------------------------------------------------------

void CEditableMesh::Create(st_Face* faces, u32 face_count, Fvector* vertices, u32 vertex_count, Fvector* normals, u32 normal_count)
{
	// Clear existing data
	Clear();

	// Allocate and copy vertices
	m_Vertices.assign(vertices, vertices + vertex_count);
	m_Faces.assign(faces, faces + face_count);

	m_SmoothGroups.assign(face_count, 0);

	if (normals && normal_count)
	{
		m_Normals.assign(normals, normals + normal_count);
	}
	else
	{
		GenerateFNormals();
		GenerateVNormals(nullptr, true);
	}

	// Generate adjacency information
	GenerateAdjacency();

	// Update bounding box
	RecomputeBBox();

	// Create default surface if none exists
	if (m_SurfFaces.empty())
	{
		CSurface* surf = new CSurface();
		surf->m_Name = ("default");
		surf->SetShader("default");
		m_Parent->m_Surfaces.push_back(surf);

		IntVec face_indices;
		face_indices.resize(m_Faces.size());
		for (u32 i = 0; i < m_Faces.size(); ++i)
			face_indices[i] = i;

		m_SurfFaces[surf] = face_indices;
	}
}