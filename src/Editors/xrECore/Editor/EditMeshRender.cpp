//----------------------------------------------------
// file: StaticMesh.cpp
//----------------------------------------------------

#include "stdafx.h"
#pragma hdrstop

//#include "EditMeshVLight.h"
#include "EditMesh.h"
#include "EditObject.h"
#include "ui_main.h"
#include "D3DUtils.h"
#include "render.h"

#include <FlexibleVertexFormat.h>

//----------------------------------------------------
#define F_LIM (10000)
#define V_LIM (F_LIM*3)
//----------------------------------------------------
void CEditableMesh::GenerateRenderBuffers()
{
	if (m_RenderBuffers) return;
	m_RenderBuffers		= new RBMap();

	GenerateVNormals	(0);

	VERIFY(m_VertexNormals || m_Normals);

	for (SurfFacesPairIt sp_it = m_SurfFaces.begin(); sp_it != m_SurfFaces.end(); sp_it++) {
		IntVec& face_lst = sp_it->second;
		CSurface* _S = sp_it->first;
		int num_verts = face_lst.size() * 3;
		RBVector rb_vec;
		int v_cnt = num_verts;
		int start_face = 0;
		int num_face;
		VERIFY3(v_cnt, "Empty surface arrive.", _S->_Name());

		rb_vec.push_back(st_RenderBuffer(0, v_cnt));
		st_RenderBuffer& rb = rb_vec.back();
		if (_S->m_Flags.is(CSurface::sf2Sided))
			rb.dwNumVertex *= 2;
		num_face = v_cnt / 3;

		int buf_size = FVF::ComputeVertexSize(_S->_FVF()) * rb.dwNumVertex;
		R_ASSERT2(buf_size, "Empty buffer size or bad FVF.");
		u8* bytes = 0;
		IDirect3DVertexBuffer9* pVB = 0;
		R_CHK(REDevice->CreateVertexBuffer(buf_size, D3DUSAGE_WRITEONLY, 0, D3DPOOL_MANAGED, &pVB, 0));
		rb.pGeom.create(_S->_FVF(), pVB, 0);
		R_CHK(pVB->Lock(0, 0, (LPVOID*)&bytes, 0));
		FillRenderBuffer(face_lst, start_face, num_face, _S, bytes);
		pVB->Unlock();

		start_face += (_S->m_Flags.is(CSurface::sf2Sided)) ? rb.dwNumVertex / 6 : rb.dwNumVertex / 3;

		if (num_verts > 0) m_RenderBuffers->insert(std::make_pair(_S, rb_vec));
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
void CEditableMesh::FillRenderBuffer(IntVec& face_lst, int start_face, int num_face, const CSurface* surf, LPBYTE& src_data)
{
	LPBYTE data = src_data;
	u32 dwFVF = surf->_FVF();
	u32 dwTexCnt = ((dwFVF & D3DFVF_TEXCOUNT_MASK) >> D3DFVF_TEXCOUNT_SHIFT);

	auto ProcessVertex = [&](st_FaceVert& fv, u32 norm_id, bool invert_normal = false)
	{
		// Position
		if (dwFVF & D3DFVF_XYZ)
		{
			VERIFY2(fv.pindex < (int)m_VertCount, "- Face index out of range.");
			CopyMemory(data, &m_Vertices[fv.pindex], sizeof(Fvector));
			data += sizeof(Fvector);
		}

		// Normal
		if (dwFVF & D3DFVF_NORMAL)
		{
			Fvector PN = (m_VertexNormals == nullptr) ? m_Normals[norm_id] : m_VertexNormals[norm_id];
			if (invert_normal) PN.invert();
			CopyMemory(data, &PN, sizeof(Fvector));
			data += sizeof(Fvector);
		}

		// UVs
		int offs = 0;
		for (int t = 0; t < (int)dwTexCnt; t++)
		{
			VERIFY2((t + offs) < (int)m_VMRefs[fv.vmref].count, "- VMap layer index out of range");
			st_VMapPt& vm_pt = m_VMRefs[fv.vmref].pts[t + offs];

			if (m_VMaps[vm_pt.vmap_index]->type != vmtUV)
			{
				offs++;
				t--;
				continue;
			}

			VERIFY2(vm_pt.vmap_index < (int)m_VMaps.size(), "- VMap index out of range");
			st_VMap* vmap = m_VMaps[vm_pt.vmap_index];
			VERIFY2(vm_pt.index < vmap->size(), "- VMap point index out of range");

			CopyMemory(data, &vmap->getUV(vm_pt.index), sizeof(Fvector2));
			data += sizeof(Fvector2);
		}
	};

	for (int fl_i = start_face; fl_i < start_face + num_face; fl_i++)
	{
		u32 f_index = face_lst[fl_i];
		VERIFY(f_index < m_FaceCount);
		st_Face& face = m_Faces[f_index];

		// Front side
		for (int k = 0; k < 3; k++)
		{
			ProcessVertex(face.pv[k], f_index * 3 + k);
		}

		// Back side (if 2-sided)
		if (surf->m_Flags.is(CSurface::sf2Sided))
		{
			for (int k = 2; k >= 0; k--) 
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
			EDevice->DP(D3DPT_TRIANGLELIST,rb_it->pGeom,0,rb_it->dwNumVertex/3);
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
			DU_impl.DrawPrimitiveL(D3DPT_TRIANGLELIST,RB_cnt/3,RB,RB_cnt,color,true,false);
			RB_cnt = 0;
		}
	}

	if (RB_cnt)
		DU_impl.DrawPrimitiveL(D3DPT_TRIANGLELIST,RB_cnt/3,RB,RB_cnt,color,true,false);

	if (bEdge)
		EDevice->SetRS(D3DRS_FILLMODE,EDevice->dwFillMode);

	EDevice->ResetNearer();
}
//----------------------------------------------------

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
				EDevice->DP(D3DPT_TRIANGLELIST,rb_it->pGeom,0,rb_it->dwNumVertex/3);
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
				EDevice->DP(D3DPT_TRIANGLELIST,rb_it->pGeom,0,rb_it->dwNumVertex/3);
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

		//if (S->m_Flags.is(CSurface::sf2Sided))
		//{
		//	pv->P.set((pv - 1)->P);	pv->N.invert((pv - 1)->N);	pv->uv.set((pv - 1)->uv); pv++;
		//	pv->P.set((pv - 3)->P);	pv->N.invert((pv - 3)->N);	pv->uv.set((pv - 3)->uv); pv++;
		//	pv->P.set((pv - 5)->P);	pv->N.invert((pv - 5)->N);	pv->uv.set((pv - 5)->uv); pv++;
		//}
	}

	u32 OldCullMode = RCache.get_CullMode();
	if (S->m_Flags.is(CSurface::sf2Sided))
	{
		RCache.set_CullMode(CULL_NONE);
	}

	Stream->Unlock(FaceCount * 3, m_Parent->vs_SkeletonGeom->vb_stride);

	if (FaceCount)
	{
		EDevice->DP(D3DPT_TRIANGLELIST, m_Parent->vs_SkeletonGeom, vBase, FaceCount);
	}

	if (S->m_Flags.is(CSurface::sf2Sided))
	{
		RCache.set_CullMode(OldCullMode);
	}
}