//----------------------------------------------------
// file: StaticMesh.cpp
//----------------------------------------------------

#include "stdafx.h"

#include "EditMesh.h"
#include "EditObject.h"
#include "ui_main.h"
#include "D3DUtils.h"
#include "render.h"

#include <FlexibleVertexFormat.h>
//
//struct VEditorVertex
//{
//	Fvector3 P;
//	Fvector2 tc;
//	Fvector  N;
//};
//
//RHIInputElementDesc VEditorVertexDecl[] =
//{
//	{ "POSITION", 0, ERHI_FORMAT::R32G32B32_FLOAT, 0, 0,							ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
//	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32_FLOAT,    0, D3D11_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
//	{ "NORMAL", 0,   ERHI_FORMAT::R32G32B32_FLOAT, 0, D3D11_APPEND_ALIGNED_ELEMENT, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
//};

struct svertRender
{
	Fvector3 P{}; float weight0 = 1.0f;
	Fvector3 N{}; float weight1 = 0.0f;
	Fvector3 T{}; float weight2 = 0.0f;
	Fvector3 B{}; float weight3 = 0.0f;
	Fvector2 uv { };
	uint32_t ind = 0;
};

static RHIInputElementDesc dwDecl_4W[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TANGENT", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 32, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "BINORMAL", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 48, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32_FLOAT, 0, 64, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 1, ERHI_FORMAT::B8G8R8A8_UNORM, 0, 72, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
};

//----------------------------------------------------
#define F_LIM (10000)
#define V_LIM (F_LIM*3)
//----------------------------------------------------
void CEditableMesh::GenerateRenderBuffers()
{
	if (m_RenderBuffers)
	{
		return;
	}

	m_RenderBuffers = new RBMap();

	GenerateVNormals(nullptr);
	VERIFY(m_VertexNormals || m_Normals);

	for (auto sp_it = m_SurfFaces.begin(); sp_it != m_SurfFaces.end(); ++sp_it)
	{
		IntVec& face_lst = sp_it->second;
		CSurface* S = sp_it->first;

		const int face_count = face_lst.size();
		VERIFY3(face_count, "Empty surface arrive.", S->_Name());

		int vertex_count = face_count * 3;
		if (S->m_Flags.is(CSurface::sf2Sided))
		{
			vertex_count *= 2;
		}

		RBVector rb_vec;
		rb_vec.emplace_back(0, vertex_count);
		st_RenderBuffer& rb = rb_vec.back();

		const u32 vertex_size = sizeof(svertRender);
		const u32 buffer_size = vertex_size * vertex_count;

		VERIFY2(buffer_size, "Empty buffer size");

		IRHIBuffer* pVB = nullptr;
		R_ASSERT(RHIUtils::CreateVertexBuffer(&pVB, nullptr, buffer_size, false));

		rb.pGeom.create(dwDecl_4W, std::size(dwDecl_4W), pVB, 0);

		RHIMappedSubresource mapped{};
		if (pVB->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &mapped))
		{
			u8* bytes = static_cast<u8*>(mapped.pData);
			FillRenderBuffer(face_lst, 0, face_count, S, bytes);
			pVB->Unmap();
		}

		m_RenderBuffers->insert(std::make_pair(S, rb_vec));
	}

	UnloadVNormals();
}

//----------------------------------------------------

void CEditableMesh::UnloadRenderBuffers()
{
	if (m_RenderBuffers){
		for (RBMapPairIt rbmp_it=m_RenderBuffers->begin(); rbmp_it!=m_RenderBuffers->end(); rbmp_it++){
			for(RBVecIt rb_it=rbmp_it->second.begin(); rb_it!=rbmp_it->second.end(); rb_it++)
				if (rb_it->pGeom){
					_RELEASE		(rb_it->pGeom->vb);
					_RELEASE		(rb_it->pGeom->ib);
					rb_it->pGeom.destroy();
				}
		}
		xr_delete					(m_RenderBuffers);
	}
}

void CEditableMesh::FillRenderBuffer(IntVec& face_lst, int start_face, int num_face, const CSurface* surf, u8*& src_data)
{
	VERIFY(surf);

	const u32 dwFVF = surf->_FVF();
	const u32 dwTexCnt = ((dwFVF & D3DFVF_TEXCOUNT_MASK) >> D3DFVF_TEXCOUNT_SHIFT);

	auto* vtx = reinterpret_cast<svertRender*>(src_data);

	auto ProcessVertex = [&](const st_FaceVert& fv, u32 norm_id, bool invert_normal)
	{
		VERIFY2(fv.pindex < (int)m_VertCount, "- Face index out of range");
		vtx->P.x = m_Vertices[fv.pindex].x;
		vtx->P.y = m_Vertices[fv.pindex].y;
		vtx->P.z = m_Vertices[fv.pindex].z;

		vtx->weight0 = 1.0f;
		vtx->weight1 = 0.0f;
		vtx->weight2 = 0.0f;
		vtx->weight3 = 0.0f;

		if (dwFVF & D3DFVF_NORMAL)
		{
			if (EPrefs->SmoothGroup == ESmoothGroup::Normals && m_Normals != nullptr)
			{
				vtx->N = m_Normals[norm_id];
			}
			else
			{
				if (m_VertexNormals == nullptr)
				{
					GenerateVNormals(nullptr, true);
				}

				vtx->N = m_VertexNormals ? m_VertexNormals[norm_id] : m_Normals[norm_id];
			}

			if (invert_normal)
			{
				vtx->N.invert();
			}
		}
		else
		{
			vtx->N.set(0, 1, 0);
		}

		vtx->uv.set(0, 0);
		if (dwTexCnt > 0 && fv.vmref >= 0)
		{
			const auto& vmref = m_VMRefs[fv.vmref];
			int offs = 0;
			for (int t = 0; t < (int)dwTexCnt; ++t)
			{
				int idx = t + offs;
				VERIFY2(idx < (int)vmref.count, "- VMap layer index out of range");

				const st_VMapPt& vm_pt = vmref.pts[idx];
				VERIFY2(vm_pt.vmap_index < (int)m_VMaps.size(), "- VMap index out of range");

				st_VMap* vmap = m_VMaps[vm_pt.vmap_index];
				VERIFY2(vm_pt.index < vmap->size(), "- VMap point index out of range");

				if (vmap->type != vmtUV)
				{
					continue;
				}

				vtx->uv = vmap->getUV(vm_pt.index);
			}
		}

		// Tangent/Binormal are consumed by model shaders (normal mapping) but
		// were never written, leaving them zero. normalize(zero) -> NaN and the
		// geometry collapses for any such shader. Build a stable orthonormal
		// frame from the normal so they are always valid.
		Fvector tref(0, 1, 0);
		if (fabsf(vtx->N.y) > 0.99f) tref.set(0, 0, 1);
		vtx->T.crossproduct(vtx->N, tref);
		if (vtx->T.square_magnitude() < EPS_S) vtx->T.set(1, 0, 0);
		else vtx->T.normalize_safe();
		vtx->B.crossproduct(vtx->N, vtx->T);

		++vtx;
	};

	for (int fl_i = start_face; fl_i < start_face + num_face; ++fl_i)
	{
		u32 f_index = face_lst[fl_i];
		if (f_index >= m_FaceCount)
		{
			Msg("!Incorrect UV reference in mesh %s", m_Name.c_str());
			continue;
		}

		VERIFY(f_index < m_FaceCount);
		const st_Face& face = m_Faces[f_index];

		// Front
		for (int k = 0; k < 3; ++k)
		{
			ProcessVertex(face.pv[k], f_index * 3 + k, false);
		}

		// Back (2-sided)
		if (surf->m_Flags.is(CSurface::sf2Sided))
		{
			for (int k = 2; k >= 0; --k)
			{
				ProcessVertex(face.pv[k], f_index * 3 + k, true);
			}
		}
	}
}

struct SelectionColorRaii
{
	SelectionColorRaii(CCustomObject* Parent, CEditableMesh::EditColorMesh& m_color_map)
	{
		if (!Parent)
		{
			RCache.hemi.set_selection(0);
			return;
		}

		Fvector4 sum_color; sum_color.set(0, 0, 0, 0);
		Fvector4 color = sum_color;

		for (auto& [ID, pColor] : m_color_map[Parent])
		{
			if (pColor.first < EDevice->dwRenderFrame)
			{
				continue;
			}

			color.set(pColor.second.r, pColor.second.g, pColor.second.b, 1.0f);
			sum_color.add(color.mul(pColor.second.a));
		}

		RCache.hemi.set_selection(sum_color);
	};

	~SelectionColorRaii()
	{
		RCache.hemi.set_selection(0);
	};
};

void CEditableMesh::RenderSelection(CCustomObject* parent, u32 Color)
{
	SetColor(parent, 0, Color);
}

void CEditableMesh::RenderEdge(CCustomObject* parent, u32 Color)
{
	SetColor(parent, 1, Color);
}

void CEditableMesh::RemoveColor(CCustomObject* Parent)
{
	m_color_map.erase(Parent);
}

void CEditableMesh::SetColor(CCustomObject* Parent, u8 ID, u32 Color)
{
	if (!Parent)
	{
		return;
	}

	m_color_map[Parent][ID] = xr_pair(EDevice->dwRenderFrame + 2, Color);
}

void CEditableMesh::Render(CCustomObject* pParent, const Fmatrix& parent, CSurface* S)
{
	SelectionColorRaii pRenderColor(pParent, m_color_map);

	if (0==m_RenderBuffers) 
	{
		GenerateRenderBuffers();
	}

	if (!m_Flags.is(flVisible)) 
	{
		return;
	}

	Fbox bb; bb.set(m_Box);
	bb.xform(parent);

	if (!::Render->occ_visible(bb))
	{
		return;
	}

	if (auto rb_pair = m_RenderBuffers->find(S); rb_pair != m_RenderBuffers->end())
	{
		for (auto& rb_it : rb_pair->second)
		{
			EDevice->DP(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, rb_it.pGeom, 0, rb_it.dwNumVertex / 3);
		}
	}
}

void CEditableMesh::RenderSkeleton(CCustomObject* pParent, const Fmatrix&, CSurface* S)
{
	SelectionColorRaii pRenderColor(pParent, m_color_map);

	if (!IsGeneratedSVertices(RENDER_SKELETON_LINKS))
	{
		GenerateSVertices(RENDER_SKELETON_LINKS);
	}

	R_ASSERT2(m_SVertices, "SVertices empty!");
	SurfFacesPairIt sp_it = m_SurfFaces.find(S);

	if (sp_it == m_SurfFaces.end())
	{
		return;
	}

	// set model shader from surface (active shader in editor device)
	ref_shader shader = EDevice->GetShader();
	EDevice->SetShader(shader);

	IntVec& face_lst = sp_it->second;
	size_t FaceCount = face_lst.size();

	_VertexStream* Stream = &RCache.Vertex;

	u32 vBase = 0;

	svertRender* pv = (svertRender*)Stream->Lock(FaceCount * 3, m_Parent->vs_SkeletonGeom->vb_stride, vBase);

	for (auto& i_it : face_lst)
	{
		for (int k = 0; k < 3; k++, pv++)
		{
			st_SVert& SV = m_SVertices[i_it * 3 + k];
			pv->uv = SV.uv;
			pv->P = SV.offs;
			pv->N = SV.norm;

			u8 bone_count = (u8)SV.bones.size();
			float total = SV.bones[0].w;
			float max_weight = SV.bones[0].w + SV.bones[1 % bone_count].w + SV.bones[2 % bone_count].w;
			
			pv->weight3 = SV.bones[0].w / max_weight;
			pv->weight2 = SV.bones[1 % bone_count].w / max_weight;
			pv->weight1 = SV.bones[2 % bone_count].w / max_weight;
			pv->weight0 = SV.bones[3 % bone_count].w / max_weight;

			pv->ind = color_rgba(
				SV.bones[0].id, 
				SV.bones[1 % bone_count].id,
				SV.bones[2 % bone_count].id,
				SV.bones[3 % bone_count].id);
		}
	}

	ERHI_CULLMODE OldCullMode = GRHI->StateManager->GetCullMode();

	Stream->Unlock(FaceCount * 3, m_Parent->vs_SkeletonGeom->vb_stride);

	if (FaceCount)
	{
		u32 dwRequired = shader->E[0]->passes.size();

		for (u32 dwPass = 0; dwPass < dwRequired; dwPass++)
		{
			RCache.set_Shader(shader, dwPass);

			// transfer matrices
			ref_constant array = RCache.get_c("sbones_array");

			const BoneVec& boneVec = m_Parent->m_Bones;
			for (u16 mid = 0, count = (u16)boneVec.size(); mid < count; mid++)
			{
				u32 id = u32(mid * 3);
				const Fmatrix& M = boneVec[mid]->_RenderTransform();
				RCache.set_ca(&*array, id + 0, M._11, M._21, M._31, M._41);
				RCache.set_ca(&*array, id + 1, M._12, M._22, M._32, M._42);
				RCache.set_ca(&*array, id + 2, M._13, M._23, M._33, M._43);
			}

			if (S->m_Flags.is(CSurface::sf2Sided))
			{
				GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
			}
			else
			{
				GRHI->StateManager->SetCullMode(OldCullMode);
			}
			EDevice->SetRS(D3DRS_FILLMODE, EDevice->dwFillMode);
			RCache.set_Geometry(m_Parent->vs_SkeletonGeom);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, vBase, FaceCount);
		}
	}
}