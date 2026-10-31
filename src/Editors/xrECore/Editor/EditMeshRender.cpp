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

static void NormalBasis(const Fvector& N, Fvector& T, Fvector& B)
{
	Fvector tref(0, 1, 0);
	if (fabsf(N.y) > 0.99f) tref.set(0, 0, 1);
	T.crossproduct(N, tref);
	if (T.square_magnitude() < EPS_S) T.set(1, 0, 0);
	else T.normalize_safe();
	B.crossproduct(N, T);
}

//----------------------------------------------------
void CEditableMesh::GenerateRenderBuffers(bool Quiet)
{
	if (m_RenderBuffers)
	{
		return;
	}

	m_RenderBuffers = xr_make_unique<RBMap>();

	GenerateVNormals(nullptr);
	VERIFY(!m_VertexNormals.empty() || !m_Normals.empty());

	xr_vector<PackedVertex> Vertices;
	xr_vector<Fvector2> UV;
	for (auto& [S, face_lst] : m_SurfFaces)
	{
		const u32 face_count = (u32)face_lst.size();
		VERIFY3(face_count, "Empty surface arrive.", S->m_Name.c_str());

		const u32 FaceVertices = S->m_Flags.is(CSurface::sf2Sided) ? 6 : 3;
		const u32 ChunkFaces = (1u << 20) / FaceVertices;
		RBVector& Buffers = (*m_RenderBuffers)[S];
		for (u32 First = 0; First < face_count; First += ChunkFaces)
		{
			const u32 Faces = std::min(ChunkFaces, face_count - First);
			Vertices.resize(Faces * FaceVertices);
			UV.resize(Faces * FaceVertices);
			const u32 Count = FillRenderBuffer(face_lst, First, Faces, S, Vertices.data(), UV.data(), Quiet);
			if (!Count)
			{
				continue;
			}

			st_RenderBuffer& RB = Buffers.emplace_back();
			RB.Alloc = RImplementation.Geometry.Alloc(Count, 0);
			RImplementation.Geometry.Upload(RB.Alloc, 0, Vertices.data(), Count);
			RB.pGeom.create(EGeomSource::Static, RB.Alloc.Page);
		}
	}

	UnloadVNormals();
}

//----------------------------------------------------

void CEditableMesh::UnloadRenderBuffers()
{
	if (!m_RenderBuffers)
	{
		return;
	}

	for (auto& [S, Buffers] : *m_RenderBuffers)
	{
		for (st_RenderBuffer& RB : Buffers)
		{
			RImplementation.Geometry.Free(RB.Alloc);
			RB.pGeom.destroy();
		}
	}
	m_RenderBuffers.reset();
}

u32 CEditableMesh::FillRenderBuffer(IntVec& face_lst, int start_face, int num_face, const CSurface* surf, PackedVertex* Vertices, Fvector2* UV, bool Quiet)
{
	VERIFY(surf);

	const u32 dwFVF = surf->m_dwFVF;
	const u32 dwTexCnt = ((dwFVF & D3DFVF_TEXCOUNT_MASK) >> D3DFVF_TEXCOUNT_SHIFT);

	u32 Count = 0;
	auto ProcessVertex = [&](const st_FaceVert& fv, u32 norm_id, bool invert_normal)
	{
		VERIFY2(fv.pindex < (int)m_Vertices.size(), "- Face index out of range");

		// HACK: white C for COLOR-reading shaders (wire, selection, Shader Editor preview); X = 1.0f is the stub_notransform w
		PackedVertex& V = Vertices[Count];
		V = { m_Vertices[fv.pindex], 0, 0, 0, 0xFFFFFFFF, std::bit_cast<u32>(1.0f) };

		Fvector N;
		if (dwFVF & D3DFVF_NORMAL)
		{
			if (EPrefs->SmoothGroup == ESmoothGroup::Normals && !m_Normals.empty())
			{
				N = m_Normals[norm_id];
			}
			else
			{
				if (m_VertexNormals.empty())
				{
					GenerateVNormals(nullptr, true);
				}

				N = !m_VertexNormals.empty() ? m_VertexNormals[norm_id] : m_Normals[norm_id];
			}

			if (invert_normal)
			{
				N.invert();
			}
		}
		else
		{
			N.set(0, 1, 0);
		}

		Fvector2& uv = UV[Count];
		uv.set(0, 0);
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

				uv = vmap->getUV(vm_pt.index);
			}
		}

		if (m_TexGen)
		{
			m_TexGen(uv, fv.pindex, N);
		}

		Fvector T, B;
		NormalBasis(N, T, B);
		VertexPacking::EncodeFrame(V, N, T, B);

		++Count;
	};

	for (int fl_i = start_face; fl_i < start_face + num_face; ++fl_i)
	{
		u32 f_index = face_lst[fl_i];
		if (f_index >= m_Faces.size())
		{
			Msg("!Incorrect UV reference in mesh %s", m_Name.c_str());
			continue;
		}

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

	const Fvector2 Offset = VertexPacking::FitUV(UV, Count, Quiet ? nullptr : m_Name.c_str());
	for (u32 Index = 0; Index < Count; ++Index)
	{
		VertexPacking::EncodeFitUV(Vertices[Index], UV[Index], Offset);
	}
	return Count;
}

struct SelectionColorRaii
{
	GfxCmdList& Cmd;

	SelectionColorRaii(GfxCmdList& Cmd, CCustomObject* Parent, CEditableMesh::EditColorMesh& m_color_map) : Cmd(Cmd)
	{
		if (!Parent)
		{
			Cmd.Constants.Write<CBEditor>().editor_selection.set(0, 0, 0, 0);
			return;
		}

		Fvector4 sum_color; sum_color.set(0, 0, 0, 0);
		Fvector4 color = sum_color;

		auto It = m_color_map.find(Parent);
		if (It == m_color_map.end())
		{
			Cmd.Constants.Write<CBEditor>().editor_selection = sum_color;
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

		Cmd.Constants.Write<CBEditor>().editor_selection = sum_color;
	};

	~SelectionColorRaii()
	{
		Cmd.Constants.Write<CBEditor>().editor_selection.set(0, 0, 0, 0);
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
	SelectionColorRaii pRenderColor(RImplementation.Imm(), pParent, m_color_map);

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
			EDevice->DP(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, rb_it.pGeom, rb_it.Alloc.VertexBase, rb_it.Alloc.VertexCount / 3);
		}
	}
}

void CEditableMesh::Submit(CCustomObject* pParent, const Fmatrix& parent, CSurface* S, const ref_shader& Shader, const ref_shader& InstancedShader, bool Skeleton)
{
	if (!Skeleton && !m_RenderBuffers)
	{
		GenerateRenderBuffers();
	}

	if (!m_Flags.is(flVisible))
	{
		return;
	}

	Fbox bb;
	bb.set(m_Box);
	bb.xform(parent);

	if (!::Render->occ_visible(bb))
	{
		return;
	}

	GEditorSceneSubmit.AddSurface(pParent, this, S, Shader, InstancedShader, parent, Skeleton);
}

void CEditableMesh::RenderGeometry(GfxCmdList& Cmd, CCustomObject* pParent, CSurface* S)
{
	SelectionColorRaii pRenderColor(Cmd, pParent, m_color_map);

	if (!m_RenderBuffers)
	{
		return;
	}

	if (auto rb_pair = m_RenderBuffers->find(S); rb_pair != m_RenderBuffers->end())
	{
		for (auto& rb_it : rb_pair->second)
		{
			Cmd.SetGeometry(rb_it.pGeom);
			Cmd.Draw(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, rb_it.Alloc.VertexCount, rb_it.Alloc.VertexBase);
		}
	}
}

void CEditableMesh::RenderGeometryInstanced(GfxCmdList& Cmd, CSurface* S, u32 InstanceCount)
{
	if (!m_RenderBuffers)
	{
		return;
	}

	if (auto rb_pair = m_RenderBuffers->find(S); rb_pair != m_RenderBuffers->end())
	{
		for (auto& rb_it : rb_pair->second)
		{
			Cmd.SetGeometry(rb_it.pGeom);
			Cmd.DrawInstanced(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, rb_it.Alloc.VertexCount, InstanceCount, rb_it.Alloc.VertexBase);
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
			GEditorInstancing.Add(InstancedShader, rb_it.pGeom, rb_it.Alloc.VertexBase, rb_it.Alloc.VertexCount, parent);
		}
	}

	return true;
}

void CEditableMesh::RenderSkeleton(CCustomObject* pParent, const Fmatrix&, CSurface* S)
{
	// set model shader from surface (active shader in editor device)
	ref_shader shader = EDevice->GetShader();
	EDevice->SetShader(shader);
	DrawSkeleton(RImplementation.Imm(), pParent, S, &shader);
}

void CEditableMesh::RenderSkeletonGeometry(GfxCmdList& Cmd, CCustomObject* pParent, CSurface* S)
{
	DrawSkeleton(Cmd, pParent, S, nullptr);
}

void CEditableMesh::DrawSkeleton(GfxCmdList& Cmd, CCustomObject* pParent, CSurface* S, ref_shader* PassShader)
{
	SelectionColorRaii pRenderColor(Cmd, pParent, m_color_map);

	if (!IsGeneratedSVertices(RENDER_SKELETON_LINKS))
	{
		GenerateSVertices(RENDER_SKELETON_LINKS);
	}

	SurfFacesPairIt sp_it = m_SurfFaces.find(S);

	if (sp_it == m_SurfFaces.end())
	{
		return;
	}

	const IntVec& face_lst = sp_it->second;
	const u32 FaceCount = (u32)face_lst.size();
	const u32 ChunkFaces = Cmd.Vertex.GetCapacity() / 3;
	const BoneVec& Bones = m_Parent->m_Bones;

	ERHI_CULLMODE OldCullMode = Cmd.GetCullMode();
	// Without a shader the caller (the scene graph) has set the pass already
	const u32 dwRequired = PassShader ? (u32)(*PassShader)->E[0]->passes.size() : 1;

	for (u32 First = 0; First < FaceCount; First += ChunkFaces)
	{
		const u32 Last = std::min(First + ChunkFaces, FaceCount);
		const u32 Count = (Last - First) * 3;
		u32 vBase = 0;
		PackedVertex* V = (PackedVertex*)Cmd.Vertex.Lock(Count, EVertexFormat::Packed, vBase);
		for (u32 Face = First; Face < Last; ++Face)
		{
			const st_SVert* SV = &m_SVertices[face_lst[Face] * 3];
			const Fvector2 UV[3] = { SV[0].uv, SV[1].uv, SV[2].uv };
			const Fvector2 Offset = VertexPacking::FitUV(UV, 3, nullptr);
			for (u32 k = 0; k < 3; ++k, ++V)
			{
				Fvector P{}, N{}, T{}, B{}, BindT, BindB, X;
				NormalBasis(SV[k].norm, BindT, BindB);
				for (const st_SVert::bone& Bone : SV[k].bones)
				{
					const Fmatrix& M = Bones[Bone.id]->_RenderTransform();
					M.transform_tiny(X, SV[k].offs);
					P.mad(X, Bone.w);
					M.transform_dir(X, SV[k].norm);
					N.mad(X, Bone.w);
					M.transform_dir(X, BindT);
					T.mad(X, Bone.w);
					M.transform_dir(X, BindB);
					B.mad(X, Bone.w);
				}

				PackedVertex Out = { P, 0, 0, 0, 0xFFFFFFFF, std::bit_cast<u32>(1.0f) };
				VertexPacking::EncodeFrame(Out, N, T, B);
				VertexPacking::EncodeFitUV(Out, SV[k].uv, Offset);
				*V = Out;
			}
		}
		Cmd.Vertex.Unlock(Count);

		for (u32 dwPass = 0; dwPass < dwRequired; dwPass++)
		{
			if (PassShader)
			{
				Cmd.SetPass(*PassShader, dwPass);
			}

			if (S->m_Flags.is(CSurface::sf2Sided))
			{
				Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			}
			else
			{
				Cmd.SetCullMode(OldCullMode);
			}
			EDevice->SetRS(D3DRS_FILLMODE, GEditorDraw.FillMode);
			Cmd.SetGeometry(DU_impl.RingGeom);
			Cmd.Draw(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Count, vBase);
		}
	}

	Cmd.SetCullMode(OldCullMode);
}