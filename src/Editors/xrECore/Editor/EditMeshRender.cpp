//----------------------------------------------------
// file: StaticMesh.cpp
//----------------------------------------------------

#include "stdafx.h"


//#include "EditMeshVLight.h"
#include "EditMesh.h"
#include "EditObject.h"
#include "ui_main.h"
#include "D3DUtils.h"
#include "render.h"

#include <FlexibleVertexFormat.h>

struct VEditorVertex
{
	Fvector3 P;
	Fvector2 tc;
	Fvector  N;
};

D3DVERTEXELEMENT9 VEditorVertexDecl[] =
{
	{ 0, 0,  D3DDECLTYPE_FLOAT3,  D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0 },
	{ 0, sizeof(Fvector3), D3DDECLTYPE_FLOAT2,  D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0 },
	{ 0, sizeof(Fvector3) + sizeof(Fvector2), D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0 },
	D3DDECL_END()
};

struct FVFDesc
{
	bool HasPosition = false;
	bool HasNormal = false;
	bool HasColor = false;
	u32  TexCount = 0;
};

static FVFDesc DecodeFVF(u32 fvf)
{
	FVFDesc d{};
	d.HasPosition = (fvf & D3DFVF_XYZ) != 0;
	d.HasNormal = (fvf & D3DFVF_NORMAL) != 0;
	d.HasColor = (fvf & D3DFVF_DIFFUSE) != 0;
	d.TexCount = (fvf & D3DFVF_TEXCOUNT_MASK) >> D3DFVF_TEXCOUNT_SHIFT;
	return d;
}

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

	GenerateVNormals(0);
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

		const u32 vertex_size = sizeof(VEditorVertex);
		const u32 buffer_size = vertex_size * vertex_count;

		VERIFY2(buffer_size, "Empty buffer size");

		IRHIBuffer* pVB = nullptr;
		R_ASSERT(RHIUtils::CreateVertexBuffer(&pVB, nullptr, buffer_size));

		rb.pGeom.create(VEditorVertexDecl, pVB, 0);

		RHIMappedSubresource mapped{};
		if (pVB->Map(ERHI_BUFFER_MAP::WRITE, 0, &mapped))
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
//----------------------------------------------------
void CEditableMesh::FillRenderBuffer(IntVec& face_lst, int start_face, int num_face, const CSurface* surf, u8*& src_data)
{
	VERIFY(surf);

	const u32 dwFVF = surf->_FVF();
	const u32 dwTexCnt = ((dwFVF & D3DFVF_TEXCOUNT_MASK) >> D3DFVF_TEXCOUNT_SHIFT);

	auto* vtx = reinterpret_cast<VEditorVertex*>(src_data);

	auto ProcessVertex = [&](const st_FaceVert& fv, u32 norm_id, bool invert_normal)
	{
		VERIFY2(fv.pindex < (int)m_VertCount, "- Face index out of range");
		vtx->P.x = m_Vertices[fv.pindex].x;
		vtx->P.y = m_Vertices[fv.pindex].y;
		vtx->P.z = m_Vertices[fv.pindex].z;

		if (dwFVF & D3DFVF_NORMAL)
		{
			vtx->N = m_VertexNormals ? m_VertexNormals[norm_id] : m_Normals[norm_id];
			if (invert_normal)
			{
				vtx->N.invert();
			}
		}
		else
		{
			vtx->N.set(0, 1, 0);
		}

		vtx->tc.set(0, 0);
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

				vtx->tc = vmap->getUV(vm_pt.index);
			}
		}

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

//----------------------------------------------------
void CEditableMesh::Render(const Fmatrix& parent, CSurface* S)
{
	if (0==m_RenderBuffers) GenerateRenderBuffers();
	// visibility test
	if (!m_Flags.is(flVisible)) return;
	// frustum test
	Fbox bb; bb.set(m_Box);
	bb.xform(parent);
	if (!::Render->occ_visible(bb)) return;
	// render
	RBMapPairIt rb_pair = m_RenderBuffers->find(S);
	if (rb_pair!=m_RenderBuffers->end()){
		RBVector& rb_vec = rb_pair->second;
		for (RBVecIt rb_it=rb_vec.begin(); rb_it!=rb_vec.end(); rb_it++)
			EDevice->DP(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,rb_it->pGeom,0,rb_it->dwNumVertex/3);
	}
}
//----------------------------------------------------
#define MAX_VERT_COUNT 0xFFFF
static Fvector RB[MAX_VERT_COUNT];
static int RB_cnt=0;

void CEditableMesh::RenderList(const Fmatrix& parent, u32 color, bool bEdge, IntVec& fl)
{
//	if (!m_Visible) return;
//	if (!m_LoadState.is(LS_RBUFFERS)) CreateRenderBuffers();

	if (fl.size()==0) return;
	RCache.set_xform_world(parent);
	EDevice->RenderNearer(0.0006);
	RB_cnt = 0;
	if (bEdge){
		EDevice->SetShader(EDevice->m_WireShader);
		EDevice->SetRS(D3DRS_FILLMODE,D3DFILL_WIREFRAME);
	}else
		EDevice->SetShader(EDevice->m_SelectionShader);
	for (IntIt dw_it=fl.begin(); dw_it!=fl.end(); ++dw_it)
	{
		st_Face& face 		= m_Faces[*dw_it];
		for (int k=0; k<3; ++k)
			RB[RB_cnt++].set(m_Vertices[face.pv[k].pindex]);

		if (RB_cnt==MAX_VERT_COUNT)
		{
			DU_impl.DrawPrimitiveL(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,RB_cnt/3,RB,RB_cnt,color,true,false);
			RB_cnt = 0;
		}
	}

	if (RB_cnt)
		DU_impl.DrawPrimitiveL(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,RB_cnt/3,RB,RB_cnt,color,true,false);

	if (bEdge)
		EDevice->SetRS(D3DRS_FILLMODE,EDevice->dwFillMode);

	EDevice->ResetNearer();
}

void CEditableMesh::RenderSelection(const Fmatrix& parent, CSurface* s, u32 color)
{
	if (0==m_RenderBuffers) GenerateRenderBuffers();
//	if (!m_Visible) return;
	Fbox bb; bb.set(m_Box);
	bb.xform(parent);
	if (!::Render->occ_visible(bb)) return;
	// render
	RCache.set_xform_world(parent);
	float bias = -0.00005f;
	float slopeBias = -1.0f;

	EDevice->SetRS(D3DRS_SLOPESCALEDEPTHBIAS, *(DWORD*)&slopeBias);
	EDevice->SetRS(D3DRS_DEPTHBIAS, *(DWORD*)&bias);
	if (s){
		SurfFacesPairIt sp_it = m_SurfFaces.find(s);
		if (sp_it!=m_SurfFaces.end()) RenderList(parent,color,false,sp_it->second);
	}else{
		EDevice->SetRS(D3DRS_TEXTUREFACTOR,	color);
		for (RBMapPairIt p_it=m_RenderBuffers->begin(); p_it!=m_RenderBuffers->end(); p_it++){
			RBVector& rb_vec = p_it->second;
			for (RBVecIt rb_it=rb_vec.begin(); rb_it!=rb_vec.end(); rb_it++)
				EDevice->DP(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,rb_it->pGeom,0,rb_it->dwNumVertex/3);
		}
		EDevice->SetRS(D3DRS_TEXTUREFACTOR,	0xffffffff);
	}
	float zero = 0.0f;
	EDevice->SetRS(D3DRS_SLOPESCALEDEPTHBIAS, *(DWORD*)&zero);
	EDevice->SetRS(D3DRS_DEPTHBIAS, *(DWORD*)&zero);
}
//----------------------------------------------------

void CEditableMesh::RenderEdge(const Fmatrix& parent, CSurface* s, u32 color)
{
	if (0==m_RenderBuffers) GenerateRenderBuffers();
//	if (!m_Visible) return;
	RCache.set_xform_world(parent);
	EDevice->SetShader(EDevice->m_WireShader);
	EDevice->RenderNearer(0.001);
	float bias = -0.00005f;
	float slopeBias = -1.0f;
	
	EDevice->SetRS(D3DRS_SLOPESCALEDEPTHBIAS, *(DWORD*)&slopeBias);
	EDevice->SetRS(D3DRS_DEPTHBIAS, *(DWORD*)&bias);

	// render
	EDevice->SetRS(D3DRS_FILLMODE,D3DFILL_WIREFRAME);
	if (s){
		SurfFacesPairIt sp_it = m_SurfFaces.find(s);
		if (sp_it!=m_SurfFaces.end()) RenderList(parent,color,true,sp_it->second);
	}else{
		EDevice->SetRS(D3DRS_TEXTUREFACTOR,	color);
		for (RBMapPairIt p_it=m_RenderBuffers->begin(); p_it!=m_RenderBuffers->end(); p_it++){
			RBVector& rb_vec = p_it->second;
			for (RBVecIt rb_it=rb_vec.begin(); rb_it!=rb_vec.end(); rb_it++)
				EDevice->DP(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,rb_it->pGeom,0,rb_it->dwNumVertex/3);
		}
		EDevice->SetRS(D3DRS_TEXTUREFACTOR,	0xffffffff);
	}
	EDevice->SetRS(D3DRS_FILLMODE,EDevice->dwFillMode);
	float zero = 0.0f;
	EDevice->SetRS(D3DRS_SLOPESCALEDEPTHBIAS, *(DWORD*)&zero);
	EDevice->SetRS(D3DRS_DEPTHBIAS, *(DWORD*)&zero);
	EDevice->ResetNearer();
}
//----------------------------------------------------
struct svertRender
{
	Fvector3 P; float pad0;
	Fvector3 N; float weight0;
	Fvector3 T; float weight1;
	Fvector3 B; float weight2;
	Fvector2 uv;
	uint32_t ind;
};

void CEditableMesh::RenderSkeleton(const Fmatrix&, CSurface* S)
{
	if (!IsGeneratedSVertices(RENDER_SKELETON_LINKS))
		GenerateSVertices(RENDER_SKELETON_LINKS);

	R_ASSERT2(m_SVertices, "SVertices empty!");
	SurfFacesPairIt sp_it = m_SurfFaces.find(S); 

	if (sp_it == m_SurfFaces.end())
		return;

	// set model shader from surface (active shader in editor device)
	ref_shader shader = EDevice->GetShader();
	RCache.set_Shader(shader);

	// transfer matrices
	ref_constant array = RCache.get_c("sbones_array");

	const BoneVec& boneVec = m_Parent->m_Bones;
	u16 count = (u16)std::min(75ull, boneVec.size());
	for (u16 mid = 0; mid < count; mid++)
	{
		u32 id = u32(mid * 3);
		const Fmatrix& M = boneVec[mid]->_RenderTransform();
		RCache.set_ca(&*array, id + 0, M._11, M._21, M._31, M._41);
		RCache.set_ca(&*array, id + 1, M._12, M._22, M._32, M._42);
		RCache.set_ca(&*array, id + 2, M._13, M._23, M._33, M._43);
	}

	RCache.set_ca(&*array, 225, Fidentity._11, Fidentity._21, Fidentity._31, Fidentity._41);
	RCache.set_ca(&*array, 226, Fidentity._12, Fidentity._22, Fidentity._32, Fidentity._42);
	RCache.set_ca(&*array, 227, Fidentity._13, Fidentity._23, Fidentity._33, Fidentity._43);

	IntVec& face_lst = sp_it->second;
	_VertexStream* Stream = &RCache.Vertex;
	u32 vBase;

	size_t FaceCount = face_lst.size();
	//if (S->m_Flags.is(CSurface::sf2Sided))
	//{
	//	FaceCount *= 2;
	//}

	svertRender* pv = (svertRender*)Stream->Lock(FaceCount * 3, m_Parent->vs_SkeletonGeom->vb_stride, vBase);

	for (IntIt i_it = face_lst.begin(); i_it != face_lst.end(); i_it++)
	{
		for (int k = 0; k < 3; k++, pv++)
		{
			st_SVert& SV = m_SVertices[*i_it * 3 + k];
			pv->uv = SV.uv;
			pv->P = SV.offs;
			pv->N = SV.norm;
		
			u8 bone_count = (u8)SV.bones.size();
			float total = SV.bones[0].w;
			float max_weight = SV.bones[0].w + SV.bones[1 % bone_count].w + SV.bones[2 % bone_count].w;
			u16 max_bone_id = std::max(SV.bones[0].id, std::max(SV.bones[1 % bone_count].id,
				std::max(SV.bones[2 % bone_count].id, SV.bones[3 % bone_count].id)));
		
			if (max_bone_id >= 75)
			{
				const Fmatrix& M = m_Parent->m_Bones[SV.bones[0].id]->_RenderTransform();
				M.transform_tiny(pv->P, SV.offs);
				M.transform_dir(pv->N, SV.norm);

				Fvector P, N;

				for (u8 cnt = 1; cnt < bone_count; cnt++)
				{
					total += SV.bones[cnt].w;

					const Fmatrix& M = m_Parent->m_Bones[SV.bones[cnt].id]->_RenderTransform();
					M.transform_tiny(P, SV.offs);
					M.transform_dir(N, SV.norm);
					pv->P.lerp(pv->P, P, SV.bones[cnt].w / total);
					pv->N.lerp(pv->N, N, SV.bones[cnt].w / total);
				}

				pv->weight0 = pv->weight1 = pv->weight2 = 0.25f;
				pv->ind = color_rgba(75 * 3, 75 * 3, 75 * 3, 75 * 3);
			}
			else
			{
				pv->weight0 = SV.bones[0].w / max_weight;
				pv->weight1 = SV.bones[1 % bone_count].w / max_weight;
				pv->weight2 = SV.bones[2 % bone_count].w / max_weight;
				pv->ind = color_rgba(
					SV.bones[0].id * 3, 
					SV.bones[1 % bone_count].id * 3,
					SV.bones[2 % bone_count].id * 3,
					SV.bones[3 % bone_count].id * 3);
			}
		}
	}

	ERHI_CULLMODE OldCullMode = GRHI->StateManager->GetCullMode();
	if (S->m_Flags.is(CSurface::sf2Sided))
	{
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	}

	Stream->Unlock(FaceCount * 3, m_Parent->vs_SkeletonGeom->vb_stride);

	if (FaceCount)
	{
		EDevice->DP(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, m_Parent->vs_SkeletonGeom, vBase, FaceCount);
	}

	if (S->m_Flags.is(CSurface::sf2Sided))
	{
		GRHI->StateManager->SetCullMode(OldCullMode);
	}
}