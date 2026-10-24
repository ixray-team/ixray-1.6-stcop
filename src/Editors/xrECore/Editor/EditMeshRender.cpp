//----------------------------------------------------
// file: StaticMesh.cpp
//----------------------------------------------------

#include "stdafx.h"
#include "EditorSceneSubmit.h"

#include "EditMesh.h"
#include "EditObject.h"
#include "ui_main.h"
#include "D3DUtils.h"
#include "../../../Layers/xrRenderPC_R4/RenderEditorApi.h"
#include "EditorInstancing.h"

#include "../../../xrCore/FVFLegacy.h"
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

	m_RenderBuffers = xr_make_unique<RBMap>();

	GenerateVNormals(nullptr);
	VERIFY(!m_VertexNormals.empty() || !m_Normals.empty());

	// Big meshes (terrain) are split into XZ chunks so editor regions can hide parts of them
	m_Chunked = (m_Box.max.x - m_Box.min.x) > RegionChunkSize * 2.0f || (m_Box.max.z - m_Box.min.z) > RegionChunkSize * 2.0f;

	for (auto sp_it = m_SurfFaces.begin(); sp_it != m_SurfFaces.end(); ++sp_it)
	{
		IntVec& face_lst = sp_it->second;
		CSurface* S = sp_it->first;

		VERIFY3(!face_lst.empty(), "Empty surface arrive.", S->m_Name.c_str());

		const u32 verts_per_face = S->m_Flags.is(CSurface::sf2Sided) ? 6 : 3;

		// Faces sorted by chunk; each chunk becomes a vertex range of the same buffer
		IntVec sorted_faces;
		RBVector rb_vec;
		if (m_Chunked)
		{
			xr_vector<std::pair<u64, int>> keyed;
			keyed.reserve(face_lst.size());
			for (int f_index : face_lst)
			{
				if (f_index >= (int)m_Faces.size())
				{
					continue;
				}

				const st_Face& face = m_Faces[f_index];
				Fvector center;
				center.add(m_Vertices[face.pv[0].pindex], m_Vertices[face.pv[1].pindex]);
				center.add(m_Vertices[face.pv[2].pindex]);
				center.div(3.0f);

				const s32 cx = iFloor(center.x / RegionChunkSize);
				const s32 cz = iFloor(center.z / RegionChunkSize);
				keyed.emplace_back((u64(u32(cx)) << 32) | u64(u32(cz)), f_index);
			}

			std::sort(keyed.begin(), keyed.end());

			sorted_faces.reserve(keyed.size());
			for (size_t i = 0; i < keyed.size(); ++i)
			{
				if (i == 0 || keyed[i].first != keyed[i - 1].first)
				{
					rb_vec.emplace_back(u32(sorted_faces.size()) * verts_per_face, 0);
				}

				st_RenderBuffer& chunk = rb_vec.back();
				const st_Face& face = m_Faces[keyed[i].second];
				for (int k = 0; k < 3; ++k)
				{
					chunk.ChunkBox.modify(m_Vertices[face.pv[k].pindex]);
				}

				chunk.dwNumVertex += verts_per_face;
				sorted_faces.push_back(keyed[i].second);
			}
		}
		else
		{
			sorted_faces = face_lst;
			rb_vec.emplace_back(0, u32(face_lst.size()) * verts_per_face);
		}

		if (rb_vec.empty())
		{
			continue;
		}

		const u32 vertex_count = u32(sorted_faces.size()) * verts_per_face;
		const u32 buffer_size = sizeof(svertRender) * vertex_count;

		VERIFY2(buffer_size, "Empty buffer size");

		IRHIBuffer* pVB = nullptr;
		R_ASSERT(RHIUtils::CreateVertexBuffer(&pVB, nullptr, buffer_size, false));

		// All chunks share one geometry, so the buffer is released once
		ref_geom geom;
		geom.create(dwDecl_4W, std::size(dwDecl_4W), pVB, 0);
		for (st_RenderBuffer& rb : rb_vec)
		{
			rb.pGeom = geom;
		}

		RHIMappedSubresource mapped{};
		if (pVB->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &mapped))
		{
			u8* bytes = static_cast<u8*>(mapped.pData);
			FillRenderBuffer(sorted_faces, 0, int(sorted_faces.size()), S, bytes);
			pVB->Unmap();
		}

		m_RenderBuffers->insert(std::make_pair(S, rb_vec));
	}

	UnloadVNormals();
}

//----------------------------------------------------

CEditableMesh::RegionHiddenFn CEditableMesh::RegionHidden = nullptr;
bool CEditableMesh::CullChunksByView = false;

bool CEditableMesh::IsPointHidden(const Fmatrix& Parent, const Fvector& LocalPoint)
{
	if (!RegionHidden)
	{
		return false;
	}

	Fvector World;
	Parent.transform_tiny(World, LocalPoint);
	return RegionHidden(World);
}

bool CEditableMesh::IsRenderBufferHidden(const st_RenderBuffer& RB, const Fmatrix* World) const
{
	if (!World || !m_Chunked || RB.ChunkBox.min.x > RB.ChunkBox.max.x)
	{
		return false;
	}

	if (CullChunksByView)
	{
		Fbox WorldBox = RB.ChunkBox;
		WorldBox.xform(*World);

		u32 Mask = 0xff;
		if (::Render->ViewBase.testAABB(&WorldBox.min.x, Mask) == fcvNone)
		{
			return true;
		}
	}

	Fvector Center;
	RB.ChunkBox.getcenter(Center);
	return IsPointHidden(*World, SnapToChunkCenter(Center));
}

// Center of the chunk cell holding the point, so rendering and picking agree on a face
Fvector CEditableMesh::SnapToChunkCenter(const Fvector& LocalPoint)
{
	Fvector Result = LocalPoint;
	Result.x = (floorf(LocalPoint.x / RegionChunkSize) + 0.5f) * RegionChunkSize;
	Result.z = (floorf(LocalPoint.z / RegionChunkSize) + 0.5f) * RegionChunkSize;
	return Result;
}

bool CEditableMesh::IsResultHidden(const Fmatrix& Parent, const CDB::RESULT& R) const
{
	if (!m_Chunked || !RegionHidden)
	{
		return false;
	}

	Fvector Center;
	Center.add(R.verts[0], R.verts[1]);
	Center.add(R.verts[2]);
	Center.div(3.0f);
	return IsPointHidden(Parent, SnapToChunkCenter(Center));
}

void CEditableMesh::UnloadRenderBuffers()
{
	if (m_RenderBuffers){
		for (RBMapPairIt rbmp_it=m_RenderBuffers->begin(); rbmp_it!=m_RenderBuffers->end(); rbmp_it++){
			RBVector& rb_vec = rbmp_it->second;

			// Chunks of a surface share one geometry
			if (!rb_vec.empty() && rb_vec.front().pGeom){
				_RELEASE		(rb_vec.front().pGeom->vb);
				_RELEASE		(rb_vec.front().pGeom->ib);
			}

			for(RBVecIt rb_it=rb_vec.begin(); rb_it!=rb_vec.end(); rb_it++)
				rb_it->pGeom.destroy();
		}
		m_Chunked = false;
		m_RenderBuffers.reset();
	}
}

void CEditableMesh::FillRenderBuffer(IntVec& face_lst, int start_face, int num_face, const CSurface* surf, u8*& src_data)
{
	VERIFY(surf);

	const u32 dwFVF = surf->m_dwFVF;
	const u32 dwTexCnt = ((dwFVF & D3DFVF_TEXCOUNT_MASK) >> D3DFVF_TEXCOUNT_SHIFT);

	auto* vtx = reinterpret_cast<svertRender*>(src_data);

	auto ProcessVertex = [&](const st_FaceVert& fv, u32 norm_id, bool invert_normal)
	{
		VERIFY2(fv.pindex < (int)m_Vertices.size(), "- Face index out of range");
		vtx->P.x = m_Vertices[fv.pindex].x;
		vtx->P.y = m_Vertices[fv.pindex].y;
		vtx->P.z = m_Vertices[fv.pindex].z;

		vtx->weight0 = 1.0f;
		vtx->weight1 = 0.0f;
		vtx->weight2 = 0.0f;
		vtx->weight3 = 0.0f;

		// HACK: white vertex color for COLOR-reading shaders (Shader Editor preview)
		vtx->ind = 0xFFFFFFFF;

		if (dwFVF & D3DFVF_NORMAL)
		{
			if (EPrefs->SmoothGroup == ESmoothGroup::Normals && !m_Normals.empty())
			{
				vtx->N = m_Normals[norm_id];
			}
			else
			{
				if (m_VertexNormals.empty())
				{
					GenerateVNormals(nullptr, true);
				}

				vtx->N = !m_VertexNormals.empty() ? m_VertexNormals[norm_id] : m_Normals[norm_id];
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
				VERIFY2(idx < (int)vmref.size(), "- VMap layer index out of range");

				const st_VMapPt& vm_pt = vmref[idx];
				VERIFY2(vm_pt.vmap_index < (int)m_VMaps.size(), "- VMap index out of range");

				st_VMap* vmap = m_VMaps[vm_pt.vmap_index].get();
				VERIFY2(vm_pt.index < vmap->size(), "- VMap point index out of range");

				if (vmap->type != vmtUV)
				{
					continue;
				}

				vtx->uv = vmap->getUV(vm_pt.index);
			}
		}

		if (m_TexGen)
		{
			m_TexGen(vtx->uv, fv.pindex, vtx->N);
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
		if (f_index >= m_Faces.size())
		{
			Msg("!Incorrect UV reference in mesh %s", m_Name.c_str());
			continue;
		}

		VERIFY(f_index < m_Faces.size());
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

		auto It = m_color_map.find(Parent);
		if (It == m_color_map.end())
		{
			RCache.hemi.set_selection(sum_color);
			return;
		}

		for (auto& [ID, pColor] : It->second)
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
			if (!IsRenderBufferHidden(rb_it, &parent))
				EDevice->DP(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, rb_it.pGeom, rb_it.dwStartVertex, rb_it.dwNumVertex / 3);
		}
	}
}

void CEditableMesh::Submit(CCustomObject* pParent, const Fmatrix& parent, CSurface* S, const ref_shader& Shader, const ref_shader& InstancedShader, bool Skeleton)
{
	if (!Skeleton && !m_RenderBuffers)
	{
		PROF_EVENT("CEditableMesh::GenerateRenderBuffers")
		GenerateRenderBuffers();
	}

	if (!m_Flags.is(flVisible))
	{
		return;
	}

	// The object submits every surface for every mesh: most meshes of a large object have no
	// faces of the surface, skip them before the bounds and occlusion tests
	if (!Skeleton && m_RenderBuffers && m_RenderBuffers->find(S) == m_RenderBuffers->end())
	{
		return;
	}

	PROF_EVENT("CEditableMesh::Submit")
	Fbox bb;
	bb.set(m_Box);
	bb.xform(parent);

	if (!::Render->occ_visible(bb))
	{
		return;
	}

	GEditorSceneSubmit.AddSurface(pParent, this, S, Shader, InstancedShader, parent, Skeleton);
}

void CEditableMesh::RenderGeometry(CCustomObject* pParent, CSurface* S, const Fmatrix* World)
{
	PROF_EVENT("CEditableMesh::RenderGeometry")
	SelectionColorRaii pRenderColor(pParent, m_color_map);

	if (!m_RenderBuffers)
	{
		return;
	}

	if (auto rb_pair = m_RenderBuffers->find(S); rb_pair != m_RenderBuffers->end())
	{
		for (auto& rb_it : rb_pair->second)
		{
			if (IsRenderBufferHidden(rb_it, World))
			{
				continue;
			}

			RCache.set_Geometry(rb_it.pGeom);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, rb_it.dwStartVertex, rb_it.dwNumVertex / 3);
		}
	}
}

void CEditableMesh::RenderGeometryInstanced(CSurface* S, u32 InstanceCount)
{
	if (!m_RenderBuffers)
	{
		return;
	}

	if (auto rb_pair = m_RenderBuffers->find(S); rb_pair != m_RenderBuffers->end())
	{
		for (auto& rb_it : rb_pair->second)
		{
			RCache.set_Geometry(rb_it.pGeom);
			RCache.RenderInstanced(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, rb_it.dwStartVertex, rb_it.dwNumVertex / 3, InstanceCount);
		}
	}
}

bool CEditableMesh::HasSelectionColor(CCustomObject* Parent) const
{
	auto It = m_color_map.find(Parent);
	if (It == m_color_map.end())
	{
		return false;
	}

	for (auto& [ID, Color] : It->second)
	{
		if (Color.first >= EDevice->dwRenderFrame)
		{
			return true;
		}
	}

	return false;
}

bool CEditableMesh::SubmitInstance(CCustomObject* pParent, const Fmatrix& parent, CSurface* S, ref_shader& InstancedShader)
{
	if (pParent && HasSelectionColor(pParent))
	{
		return false;
	}

	if (0 == m_RenderBuffers)
	{
		GenerateRenderBuffers();
	}

	// Chunks are culled per copy, which instancing cannot do
	if (m_Chunked)
	{
		return false;
	}

	if (!m_Flags.is(flVisible))
	{
		return true;
	}

	Fbox bb; bb.set(m_Box);
	bb.xform(parent);

	if (!::Render->occ_visible(bb))
	{
		return true;
	}

	if (auto rb_pair = m_RenderBuffers->find(S); rb_pair != m_RenderBuffers->end())
	{
		for (auto& rb_it : rb_pair->second)
		{
			GEditorInstancing.Add(InstancedShader, rb_it.pGeom, rb_it.dwNumVertex, parent);
		}
	}

	return true;
}

void CEditableMesh::RenderSkeleton(CCustomObject* pParent, const Fmatrix&, CSurface* S)
{
	// set model shader from surface (active shader in editor device)
	ref_shader shader = EDevice->GetShader();
	EDevice->SetShader(shader);
	DrawSkeleton(pParent, S, &shader);
}

void CEditableMesh::RenderSkeletonGeometry(CCustomObject* pParent, CSurface* S)
{
	DrawSkeleton(pParent, S, nullptr);
}

void CEditableMesh::DrawSkeleton(CCustomObject* pParent, CSurface* S, ref_shader* PassShader)
{
	SelectionColorRaii pRenderColor(pParent, m_color_map);

	if (!IsGeneratedSVertices(RENDER_SKELETON_LINKS))
	{
		GenerateSVertices(RENDER_SKELETON_LINKS);
	}

	SurfFacesPairIt sp_it = m_SurfFaces.find(S);

	if (sp_it == m_SurfFaces.end())
	{
		return;
	}

	IntVec& face_lst = sp_it->second;
	size_t FaceCount = face_lst.size();

	_VertexStream* Stream = &RCache.Vertex;
	const u32 stride = m_Parent->vs_SkeletonGeom->vb_stride;
	VERIFY(stride == sizeof(svertRender));

	ERHI_CULLMODE OldCullMode = GRHI->StateManager->GetCullMode();
	// Without a shader the caller (the scene graph) has set the pass already
	const u32 dwRequired = PassShader ? (u32)(*PassShader)->E[0]->passes.size() : 1;

	// Draw in batches: a single lock of the whole surface can exceed the dynamic VB size
	for (size_t f_start = 0; f_start < FaceCount; f_start += F_LIM)
	{
		const u32 batchFaces = (u32)std::min<size_t>(F_LIM, FaceCount - f_start);

		u32 vBase = 0;
		svertRender* pv = (svertRender*)Stream->Lock(batchFaces * 3, stride, vBase);

		for (u32 f = 0; f < batchFaces; ++f)
		{
			const int i_it = face_lst[f_start + f];
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

		Stream->Unlock(batchFaces * 3, stride);

		for (u32 dwPass = 0; dwPass < dwRequired; dwPass++)
		{
			if (PassShader)
			{
				RCache.set_Shader(*PassShader, dwPass);
			}

			// transfer matrices
			ref_constant array = RCache.get_c("sbones_array");

			if (array)
			{
				const BoneVec& boneVec = m_Parent->m_Bones;
				for (u16 mid = 0, count = (u16)boneVec.size(); mid < count; mid++)
				{
					u32 id = u32(mid * 3);
					const Fmatrix& M = boneVec[mid]->_RenderTransform();
					RCache.set_ca(&*array, id + 0, M._11, M._21, M._31, M._41);
					RCache.set_ca(&*array, id + 1, M._12, M._22, M._32, M._42);
					RCache.set_ca(&*array, id + 2, M._13, M._23, M._33, M._43);
				}
			}

			if (S->m_Flags.is(CSurface::sf2Sided))
			{
				GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
			}
			else
			{
				GRHI->StateManager->SetCullMode(OldCullMode);
			}
			EDevice->SetRS(D3DRS_FILLMODE, GEditorDraw.FillMode);
			RCache.set_Geometry(m_Parent->vs_SkeletonGeom);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, vBase, batchFaces);
		}
	}

	GRHI->StateManager->SetCullMode(OldCullMode);
}