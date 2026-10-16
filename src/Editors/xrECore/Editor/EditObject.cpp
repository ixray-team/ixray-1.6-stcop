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
	VERIFY(!ImageData);
	ImageData = xr_make_unique<SSimpleImage>();
	ImageData->name = _Texture();
	ImageData->layers.emplace_back();
	ImageLib.LoadTextureData(*ImageData->name, ImageData->layers.back(), ImageData->w, ImageData->h);
}

void CSurface::RemoveImageData()
{
	ImageData.reset();
}

void CSurface::Reload()
{
	xr_vector<xr_string> Textures;

	if (m_RTFlags.is(rtValidShader) && m_Shader)
	{
		for (const ref_selement& Element : m_Shader->E)
		{
			if (!Element)
			{
				continue;
			}

			for (const ref_pass& Pass : Element->passes)
			{
				if (!Pass || !Pass->T)
				{
					continue;
				}

				for (const auto& [Stage, Texture] : *Pass->T)
				{
					if (Texture && Texture->cName.size())
					{
						Textures.emplace_back(Texture->cName.c_str());
					}
				}
			}
		}
	}

	if (!Textures.empty())
	{
		DEV->ED_UpdateTextures(&Textures);
	}

	OnDeviceDestroy();
	RemoveImageData();
}

CSurface::~CSurface()
{
	R_ASSERT(!m_Shader);
}

void CSurface::CopyFrom(CSurface* surf)
{
	m_Name = surf->m_Name;
	m_Texture = surf->m_Texture;
	m_VMap = surf->m_VMap;
	m_ShaderName = surf->m_ShaderName;
	m_ShaderXRLCName = surf->m_ShaderXRLCName;
	m_GameMtlName = surf->m_GameMtlName;
	m_Flags = surf->m_Flags;
	m_dwFVF = surf->m_dwFVF;
	m_id = surf->m_id;
	m_bEditorVisible = surf->m_bEditorVisible;
	tag = surf->tag;
	m_Shader = nullptr;
	m_ShaderInstanced = nullptr;
	m_RTFlags.set(rtValidShader | rtInstancedShaderTried, false);
	ImageData.reset();
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
	for (auto& m_Meshe : m_Meshes)
	{
		strcpy(pref, m_Meshe->m_Name.size() ? m_Meshe->m_Name.c_str() : "mesh");
		_Trim(pref);
		strcpy(nm, pref);
		while (FindMeshByName(nm, m_Meshe))
		{
			sprintf(nm, "%s%2d", pref, idx++);
		}
		m_Meshe->m_Name = (nm);
	}
}

bool CEditableObject::ContainsMesh(const CEditableMesh* m)
{
	VERIFY(m);
	for (auto& m_Meshe : m_Meshes)
	{
		if (m == m_Meshe)
		{
			return true;
		}
	}
	return false;
}

CEditableMesh* CEditableObject::FindMeshByName(const char* name, CEditableMesh* Ignore)
{
	for (auto& m_Meshe : m_Meshes)
	{
		if ((Ignore != m_Meshe) && (stricmp(m_Meshe->m_Name.c_str(), name) == 0))
		{
			return m_Meshe;
		}
	}
	return nullptr;
}

void CEditableObject::ClearGeometry()
{
	OnDeviceDestroy();

	for (auto& m_Meshe : m_Meshes)
	{
		xr_delete(m_Meshe);
	}

	for (auto& m_Surface : m_Surfaces)
	{
		xr_delete(m_Surface);
	}

	m_Meshes.clear();
	m_Surfaces.clear();

	// bones
	for (auto& m_Bone : m_Bones)
	{
		xr_delete(m_Bone);
	}
	m_Bones.clear();

	// skeletal motions
	for (auto& m_SMotion : m_SMotions)
	{
		xr_delete(m_SMotion);
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
	for (auto& m_Meshe : m_Meshes)
	{
		m_FaceCount += m_Meshe->GetFaceCount(bMatch2Sided, bIgnoreOCC);
	}
	return m_FaceCount;
}

int CEditableObject::GetSurfFaceCount(const char* surf_name)
{
	int cnt = 0;
	CSurface* surf = FindSurfaceByName(surf_name);
	for (auto& m_Meshe : m_Meshes)
	{
		cnt += m_Meshe->GetSurfFaceCount(surf);
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
	for (auto& m_Meshe : m_Meshes)
	{
		m_VertexCount += (int)m_Meshe->m_Vertices.size();
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
	EditMeshIt m_it = std::ranges::find(m_Meshes, mesh);
	VERIFY(m_it != m_Meshes.end());
	m_Meshes.erase(m_it);
	xr_delete(mesh);
}

void CEditableObject::TranslateToWorld(const Fmatrix& parent)
{
	for (auto m : m_Meshes)
	{
		m->Transform(parent);
	}

	OnDeviceDestroy();
	UpdateBox();
}

CSurface* CEditableObject::FindSurfaceByName(const char* surf_name, int* s_id)
{
	for (SurfaceIt s_it = m_Surfaces.begin(); s_it != m_Surfaces.end(); s_it++)
	{
		if (stricmp((*s_it)->m_Name.c_str(), surf_name) == 0)
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
	U8Vec b_use(m_Bones.size(), 0);
	for (auto& m_BonePart : m_BoneParts)
	{
		for (int i = 0; i < int(m_BonePart.bones.size()); i++)
		{
			int idx = FindBoneByNameIdx(m_BonePart.bones[i].c_str());
			if (idx == -1)
			{
				m_BonePart.bones.erase(m_BonePart.bones.begin() + i);
				i--;
			}
			else
			{
				b_use[idx]++;
			}
		}
	}

	for (unsigned char& u_it : b_use)
	{
		if (u_it != 1)
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
	for (auto& m_Meshe : m_Meshes)
	{
		m_Meshe->OptimizeMesh(false);
		m_Meshe->RebuildVMaps();
	}
}

bool CEditableObject::Validate()
{
	bool bRes = true;
	for (auto& m_Surface : m_Surfaces)
	{
		if (false == m_Surface->Validate())
		{
			Msg("!Invalid surface found: Object [%s], Surface [%s].", m_LibName.c_str(), m_Surface->m_Name.c_str());
			bRes = false;
		}
	}
	for (auto& m_Meshe : m_Meshes)
	{
		if (false == m_Meshe->Validate())
		{
			Msg("!Invalid mesh found: Object [%s], Mesh [%s].", m_LibName.c_str(), m_Meshe->m_Name.c_str());
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
