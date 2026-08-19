//----------------------------------------------------
// file: EditObject.cpp
//----------------------------------------------------

#include "stdafx.h"

#include "../xrEngine/Fmesh.h"
#include "../xrEngine/motion.h"
#include "../xrEngine/bone.h"

#include "EditObject.h"
#include "EditMesh.h"
#include "ImageManager.h"

// mimimal bounding box size
float g_MinBoxSize = 0.05f;

void CSurface::CreateImageData()
{
	VERIFY(0 == ImageData);
	ImageData = new SSimpleImage();
	ImageData->name = m_Texture;
	ImageData->layers.push_back(U32Vec());
	ImageLib.LoadTextureData(*ImageData->name, ImageData->layers.back(), ImageData->w, ImageData->h);
}

void CSurface::RemoveImageData()
{
	xr_delete(ImageData);
}

CEditableObject::CEditableObject(const char* name) :
	m_physics_shell(nullptr), 
	m_object_xform(nullptr)
{
	m_FaceCount = -1;
	m_VertexCount = -1;
	m_LibName = name;

	m_objectFlags.zero();
	m_ObjectVersion = 0;

	vs_SkeletonGeom = nullptr;
	m_BBox.invalidate();

	m_LoadState.zero();

	m_ActiveSMotion = nullptr;

	t_vPosition.set(0.f, 0.f, 0.f);
	t_vScale.set(1.f, 1.f, 1.f);
	t_vRotate.set(0.f, 0.f, 0.f);

	a_vPosition.set(0.f, 0.f, 0.f);
	a_vRotate.set(0.f, 0.f, 0.f);

	bOnModified = false;

	m_RefCount = 0;

	m_LODShader = nullptr;

	m_CreateName = "unknown";
	m_CreateTime = 0;
	m_ModifName = "unknown";
	m_ModifTime = 0;
}

CEditableObject::~CEditableObject()
{
	ClearGeometry();
}
//----------------------------------------------------

void CEditableObject::VerifyMeshNames()
{
	int idx = 0;
	string1024 nm, pref;
	for (EditMeshIt m_def = m_Meshes.begin(); m_def != m_Meshes.end(); m_def++)
	{
		strcpy(pref, (*m_def)->m_Name.size() ? (*m_def)->m_Name.c_str() : "mesh");
		_Trim(pref);
		strcpy(nm, pref);
		while (FindMeshByName(nm, *m_def))
		{
			sprintf(nm, "%s%2d", pref, idx++);
		}
		(*m_def)->SetName(nm);
	}
}

bool CEditableObject::ContainsMesh(const CEditableMesh* m)
{
	VERIFY(m);
	for (EditMeshIt m_def = m_Meshes.begin(); m_def != m_Meshes.end(); m_def++)
	{
		if (m == (*m_def))
		{
			return true;
		}
	}
	return false;
}

CEditableMesh* CEditableObject::FindMeshByName(const char* name, CEditableMesh* Ignore)
{
	for (EditMeshIt m = m_Meshes.begin(); m != m_Meshes.end(); m++)
	{
		if ((Ignore != (*m)) && (stricmp((*m)->Name().c_str(), name) == 0))
		{
			return (*m);
		}
	}
	return nullptr;
}

void CEditableObject::ClearGeometry()
{
	OnDeviceDestroy();

	for (EditMeshIt m = m_Meshes.begin(); m != m_Meshes.end(); m++)
	{
		xr_delete(*m);
	}

	for (SurfaceIt s_it = m_Surfaces.begin(); s_it != m_Surfaces.end(); s_it++)
	{
		xr_delete(*s_it);
	}

	m_Meshes.clear();
	m_Surfaces.clear();

	// bones
	for (BoneIt b_it = m_Bones.begin(); b_it != m_Bones.end(); b_it++)
	{
		xr_delete(*b_it);
	}
	m_Bones.clear();

	// skeletal motions
	for (SMotionIt s_it = m_SMotions.begin(); s_it != m_SMotions.end(); s_it++)
	{
		xr_delete(*s_it);
	}
	m_SMotions.clear();

	m_ActiveSMotion = nullptr;
}

int CEditableObject::GetFaceCount(bool bMatch2Sided, bool bIgnoreOCC)
{
	if (m_FaceCount != -1)
	{
		return m_FaceCount;
	}
	m_FaceCount = 0;
	for (EditMeshIt m = m_Meshes.begin(); m != m_Meshes.end(); m++)
	{
		m_FaceCount += (*m)->GetFaceCount(bMatch2Sided, bIgnoreOCC);
	}
	return m_FaceCount;
}

int CEditableObject::GetSurfFaceCount(const char* surf_name)
{
	int cnt = 0;
	CSurface* surf = FindSurfaceByName(surf_name);
	for (EditMeshIt m = m_Meshes.begin(); m != m_Meshes.end(); m++)
	{
		cnt += (*m)->GetSurfFaceCount(surf);
	}
	return cnt;
}

int CEditableObject::GetVertexCount()
{
	if (m_VertexCount != -1)
	{
		return m_VertexCount;
	}
	m_VertexCount = 0;
	for (EditMeshIt m = m_Meshes.begin(); m != m_Meshes.end(); m++)
	{
		m_VertexCount += (*m)->GetVertexCount();
	}
	return m_VertexCount;
}

void CEditableObject::UpdateBox()
{
	VERIFY(!m_Meshes.empty());
	EditMeshIt m = m_Meshes.begin();
	m_BBox.invalidate();

	for (; m != m_Meshes.end(); ++m)
	{
		Fbox meshbox;
		(*m)->GetBox(meshbox);
		for (int i = 0; i < 8; ++i)
		{
			Fvector pt;
			meshbox.getpoint(i, pt);
			m_BBox.modify(pt);
		}
	}
}
//----------------------------------------------------
void CEditableObject::RemoveMesh(CEditableMesh* mesh)
{
	EditMeshIt m_it = std::find(m_Meshes.begin(), m_Meshes.end(), mesh);
	VERIFY(m_it != m_Meshes.end());
	m_Meshes.erase(m_it);
	xr_delete(mesh);
}

void CEditableObject::TranslateToWorld(const Fmatrix& parent)
{
	EditMeshIt m = m_Meshes.begin();
	for (; m != m_Meshes.end(); m++)
	{
		(*m)->Transform(parent);
	}

	OnDeviceDestroy();
	UpdateBox();
}

CSurface* CEditableObject::FindSurfaceByName(const char* surf_name, int* s_id)
{
	for (SurfaceIt s_it = m_Surfaces.begin(); s_it != m_Surfaces.end(); s_it++)
	{
		if (stricmp((*s_it)->_Name(), surf_name) == 0)
		{
			if (s_id)
			{
				*s_id = s_it - m_Surfaces.begin();
			}
			return *s_it;
		}
	}
	return nullptr;
}

const char* CEditableObject::GenerateSurfaceName(const char* base_name)
{
	static string1024 nm;
	strcpy(nm, base_name);
	if (FindSurfaceByName(nm))
	{
		DWORD idx = 0;
		do
		{
			sprintf(nm, "%s_%d", base_name, idx);
			idx++;
		} while (FindSurfaceByName(nm));
	}
	return nm;
}

bool CEditableObject::VerifyBoneParts()
{
	U8Vec b_use(BoneCount(), 0);
	for (BPIt bp_it = m_BoneParts.begin(); bp_it != m_BoneParts.end(); bp_it++)
	{
		for (int i = 0; i < int(bp_it->bones.size()); i++)
		{
			int idx = FindBoneByNameIdx(bp_it->bones[i].c_str());
			if (idx == -1)
			{
				bp_it->bones.erase(bp_it->bones.begin() + i);
				i--;
			}
			else
			{
				b_use[idx]++;
			}
		}
	}

	for (U8It u_it = b_use.begin(); u_it != b_use.end(); u_it++)
	{
		if (*u_it != 1)
		{
			return false;
		}
	}
	return true;
}

void CEditableObject::PrepareOGFDesc(ogf_desc& desc)
{
	string512 tmp;
	desc.source_file = m_LibName.c_str();
	desc.create_name = m_CreateName.c_str();
	desc.create_time = m_CreateTime;
	desc.modif_name = m_ModifName.c_str();
	desc.modif_time = m_ModifTime;
	desc.build_name = xr_strconcat(tmp, "\\\\", Core.CompName, "\\", Core.UserName);
	ctime(&desc.build_time);
}

void CEditableObject::SetVersionToCurrent(bool bCreate, bool bModif)
{
	string512 tmp;
	if (bCreate)
	{
		m_CreateName = xr_strconcat(tmp, "\\\\", Core.CompName, "\\", Core.UserName);
		m_CreateTime = time(nullptr);
	}
	if (bModif)
	{
		m_ModifName = xr_strconcat(tmp, "\\\\", Core.CompName, "\\", Core.UserName);
		m_ModifTime = time(nullptr);
	}
}

void CEditableObject::GetFaceWorld(const Fmatrix& parent, CEditableMesh* M, int idx, Fvector* verts)
{
	const Fvector* PT[3];
	M->GetFacePT(idx, PT);
	parent.transform_tiny(verts[0], *PT[0]);
	parent.transform_tiny(verts[1], *PT[1]);
	parent.transform_tiny(verts[2], *PT[2]);
}

void CEditableObject::Optimize()
{
	for (EditMeshIt m_def = m_Meshes.begin(); m_def != m_Meshes.end(); m_def++)
	{
		(*m_def)->OptimizeMesh(false);
		(*m_def)->RebuildVMaps();
	}
}

bool CEditableObject::Validate()
{
	bool bRes = true;
	for (SurfaceIt s_it = m_Surfaces.begin(); s_it != m_Surfaces.end(); s_it++)
	{
		if (false == (*s_it)->Validate())
		{
			Msg("!Invalid surface found: Object [%s], Surface [%s].", GetName(), (*s_it)->_Name());
			bRes = false;
		}
	}
	for (EditMeshIt m_def = m_Meshes.begin(); m_def != m_Meshes.end(); m_def++)
	{
		if (false == (*m_def)->Validate())
		{
			Msg("!Invalid mesh found: Object [%s], Mesh [%s].", m_LibName.c_str(), (*m_def)->Name().c_str());
			bRes = false;
		}
	}
	return bRes;
}

const char* CEditableObject::LL_BoneName_dbg(u16 ID)
{
	return GetBone(ID)->Name().c_str();
}

CBoneInstance& CEditableObject::LL_GetBoneInstance(u16 bone_id)
{
	return *GetBone(bone_id);
}

CBoneData& CEditableObject::LL_GetData(u16 bone_id)
{
	VERIFY(false);
	static CBoneData dummy_bone_data(0);
	return dummy_bone_data;
}

Fmatrix& CEditableObject::LL_GetTransform_R(u16 bone_id)
{
	return GetBone(bone_id)->_RenderTransform();
}

Fobb& CEditableObject::LL_GetBox(u16 bone_id)
{
	VERIFY(false);
	static Fobb dummy_box;
	return dummy_box;
}
