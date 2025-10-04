// SkeletonX.cpp: implementation of the CSkeletonX class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "../../xrEngine/Fmesh.h"
#include "FSkinned.h"
#include "SkeletonX.h"

#include "../../xrEngine/EnnumerateVertices.h"
#include <FlexibleVertexFormat.h>
using namespace FVF;
using namespace std;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
static	shared_str	sbones_array;

#pragma pack(push,1)
inline u8 q_N(float v)
{
	int		_v	= clampr(iFloor((v+1.f)*127.5f), 0, 255);
	return	u8	(_v);
}

#ifdef _DEBUG
float errN	(Fvector3 v, u8* qv)
{
	Fvector3	uv;	
	uv.set		(float(qv[0]),float(qv[1]),float(qv[2])).div(255.f).mul(2.f).sub(1.f);
	uv.normalize();
	return		v.dotproduct(uv);
}
#else
float errN	(Fvector3 v, u8* qv)	{ return 0; }
#endif

static RHIInputElementDesc dwDecl_01W[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TANGENT", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 20, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "BINORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32_FLOAT, 0, 28, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
};

static RHIInputElementDesc dwDecl_2W[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TANGENT", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 20, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "BINORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 28, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
};

static RHIInputElementDesc dwDecl_3W[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TANGENT", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 20, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "BINORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 28, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
};

static RHIInputElementDesc dwDecl_4W[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TANGENT", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 20, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "BINORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32_FLOAT, 0, 28, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 1, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 36, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
};

struct	vertHW_1W
{
	float		_P		[4];
	u32			_N_I	;
	u32			_T		;
	u32			_B		;
	float		_tc		[2];
	void set	(Fvector3& P, Fvector3 N, Fvector3 T, Fvector3 B, Fvector2& tc, int index)
	{
		N.normalize_safe();
		T.normalize_safe();
		B.normalize_safe();
		_P[0]		= P.x;
		_P[1]		= P.y;
		_P[2]		= P.z;
		_P[3]		= 1;
		_N_I		= color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(index));
		_T			= color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), 0);
		_B			= color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), 0);
		_tc[0]		= tc.x;
		_tc[1]		= tc.y;
	}
	u16 get_bone() const
	{
		u8 delimeter = 1;

#ifndef USE_DX11
		delimeter = 3;
#endif

		return	(u16)color_get_A(_N_I) / delimeter;
	}
	void get_pos_bones( Fvector& p, CKinematics* Parent ) const
	{
			const Fmatrix& xform	= Parent->LL_GetBoneInstance( get_bone( ) ).mRenderTransform; 
			get_pos	( p );	xform.transform_tiny( p );
	}
	void get_pos(Fvector& p) const
	{
		p.x			= _P[0];
		p.y			= _P[1];
		p.z			= _P[2];
	}
};

struct	vertHW_2W
{
	float		_P		[4];
	u32			_N_w	;
	u32			_T		;
	u32			_B		;
	float		_tc_i	[4];
	void set	(Fvector3& P, Fvector3 N, Fvector3 T, Fvector3 B, Fvector2& tc, int index0, int index1, float w)
	{
		N.normalize_safe	();
		T.normalize_safe	();
		B.normalize_safe	();
		_P[0]		= P.x;
		_P[1]		= P.y;
		_P[2]		= P.z;
		_P[3]		= 1;
		_N_w		= color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(clampr(iFloor(w*255.f+.5f),0,255)));
		_T			= color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), 0);
		_B			= color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), 0);
		_tc_i[0]	= tc.x;
		_tc_i[1]	= tc.y;
		_tc_i[2]	= (float)index0;
		_tc_i[3]	= (float)index1;
	}
	float get_weight() const
	{
		return	float(color_get_A(_N_w))/255.f;
	}
	u16 get_bone(u16 w) const
	{
		u8 delimeter = 1;
#ifndef USE_DX11
		delimeter = 3;
#endif

		return	(u16)_tc_i[w+2] / delimeter;
	}
	void get_pos(Fvector& p) const
	{
		p.x = _P[0];
		p.y = _P[1];
		p.z = _P[2];
	}
	void get_pos_bones( Fvector& p, CKinematics* Parent ) const
	{
			Fvector		P0,P1;
			Fmatrix& xform0			= Parent->LL_GetBoneInstance(get_bone(0)).mRenderTransform; 
			Fmatrix& xform1			= Parent->LL_GetBoneInstance(get_bone(1)).mRenderTransform; 
			get_pos	(P0);	xform0.transform_tiny(P0);
			get_pos	(P1);	xform1.transform_tiny(P1);
			p.lerp			(P0,P1,get_weight());
	}
};

struct	vertHW_3W
{
	float		_P		[4];
	u32			_N_w	;
	u32			_T_w	;
	u32			_B_i	;
	float		_tc_i	[4];
	void set	(Fvector3& P, Fvector3 N, Fvector3 T, Fvector3 B, Fvector2& tc, int index0, int index1, int index2, float w0, float w1)
	{
		N.normalize_safe	();
		T.normalize_safe	();
		B.normalize_safe	();
		_P[0]		= P.x;
		_P[1]		= P.y;
		_P[2]		= P.z;
		_P[3]		= 1;
		_N_w		= color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(clampr(iFloor(w0*255.f+.5f),0,255)));
		_T_w		= color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), u8(clampr(iFloor(w1*255.f+.5f),0,255)));
		_B_i		= color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), u8(index2));
		_tc_i[0]	= tc.x;
		_tc_i[1]	= tc.y;
		_tc_i[2]	= (float)index0;
		_tc_i[3]	= (float)index1;
	}
	float get_weight0() const
	{
		return	float(color_get_A(_N_w))/255.f;
	}
	float get_weight1() const
	{
		return	float(color_get_A(_T_w))/255.f;
	}
	u16 get_bone(u16 w) const
	{
		u8 delimeter = 1;
#ifndef USE_DX11
		delimeter = 3;
#endif

		switch(w)
		{
		case 0:
		case 1:
			return	(u16)_tc_i[w+2] / delimeter;
		case 2:
			return	(u16)color_get_A(_B_i) / delimeter;
		}
		R_ASSERT(0);
		return 0;
	}
	void get_pos(Fvector& p) const
	{
		p.x = _P[0];
		p.y = _P[1];
		p.z = _P[2];
	}
	void get_pos_bones( Fvector& p, CKinematics* Parent ) const
	{
			Fvector		P0,P1,P2;
			Fmatrix& xform0			= Parent->LL_GetBoneInstance(get_bone(0)).mRenderTransform; 
			Fmatrix& xform1			= Parent->LL_GetBoneInstance(get_bone(1)).mRenderTransform;
			Fmatrix& xform2			= Parent->LL_GetBoneInstance(get_bone(2)).mRenderTransform; 
			get_pos	(P0);	xform0.transform_tiny(P0);
			get_pos	(P1);	xform1.transform_tiny(P1);
			get_pos	(P2);	xform2.transform_tiny(P2);
			float w0 = get_weight0();
			float w1 = get_weight1();
			P0.mul(w0);
			P1.mul(w1);
			P2.mul(1-w0-w1);
			p = P0;
			p.add(P1);
			p.add(P2);
	}
};

struct	vertHW_4W
{
	float		_P		[4];
	u32			_N_w	;
	u32			_T_w	;
	u32			_B_w	;
	float		_tc		[2];
	u32			_i		;
	void set	(Fvector3& P, Fvector3 N, Fvector3 T, Fvector3 B, Fvector2& tc, int index0, int index1, int index2, int index3, float w0, float w1, float w2)
	{
		N.normalize_safe	();
		T.normalize_safe	();
		B.normalize_safe	();
		_P[0]		= P.x;
		_P[1]		= P.y;
		_P[2]		= P.z;
		_P[3]		= 1;
		_N_w		= color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(clampr(iFloor(w0*255.f+.5f),0,255)));
		_T_w		= color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), u8(clampr(iFloor(w1*255.f+.5f),0,255)));
		_B_w		= color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), u8(clampr(iFloor(w2*255.f+.5f),0,255)));
		_tc[0]		= tc.x;
		_tc[1]		= tc.y;
		_i		= color_rgba( u8(index0), u8(index1), u8(index2), u8(index3));
	}
	float get_weight0() const
	{
		return	float(color_get_A(_N_w))/255.f;
	}
	float get_weight1() const
	{
		return	float(color_get_A(_T_w))/255.f;
	}
	float get_weight2() const
	{
		return	float(color_get_A(_B_w))/255.f;
	}
	u16 get_bone(u16 w) const
	{
		u8 delimeter = 1;
#ifndef USE_DX11
		delimeter = 3;
#endif
		switch(w)
		{
		case 0:
			return	(u16)color_get_R(_i) / delimeter;
		case 1:
			return	(u16)color_get_G(_i) / delimeter;
		case 2:
			return	(u16)color_get_B(_i) / delimeter;
		case 3:
			return	(u16)color_get_A(_i) / delimeter;
		}
		R_ASSERT(0);
		return 0;
	}
	void get_pos(Fvector& p) const
	{
		p.x	= _P[0];
		p.y	= _P[1];
		p.z	= _P[2];
	}
	void get_pos_bones( Fvector& p, CKinematics* Parent ) const
	{
			Fvector		P[4];
			for (u16 i=0; i<4; ++i)
			{
				Fmatrix& xform	= Parent->LL_GetBoneInstance(get_bone(i)).mRenderTransform;
				get_pos	(P[i]);
				xform.transform_tiny(P[i]);
			}

			float w[3];
			w[0] = get_weight0();
			w[1] = get_weight1();
			w[2] = get_weight2();

			for (int j=0; j<3; ++j)
				P[j].mul(w[j]);
			P[3].mul(1-w[0]-w[1]-w[2]);

			p = P[0];
			for (int k=1; k<4; ++k)
				p.add(P[k]);
	}
};

#pragma pack(pop)

//////////////////////////////////////////////////////////////////////
// Body Part
//////////////////////////////////////////////////////////////////////
void CSkeletonX_PM::Copy	(dxRender_Visual *V) 
{
	inherited1::Copy		(V);
	CSkeletonX_PM *X		= (CSkeletonX_PM*)(V);
	_Copy					((CSkeletonX*)X);
}
void CSkeletonX_ST::Copy	(dxRender_Visual *P) 
{
	inherited1::Copy		(P);
	CSkeletonX_ST *X		= (CSkeletonX_ST*)P;
	_Copy					((CSkeletonX*)X);
}
//////////////////////////////////////////////////////////////////////
void CSkeletonX_PM::Render	(float LOD) 
{
	//PROF_EVENT("CSkeletonX_PM::Render");
	int lod_id				= inherited1::last_lod;
	if (LOD>=0.f){
		clamp				(LOD,0.f,1.f);
		lod_id				= iFloor((1.f-LOD)*float(nSWI.count-1)+0.5f);
		inherited1::last_lod= lod_id;
	}
	VERIFY					(lod_id>=0 && lod_id<int(nSWI.count));
	FSlideWindow& SW		= nSWI.sw[lod_id];
	_Render					(rm_geom,SW.num_verts,SW.offset,SW.num_tris);
}
void CSkeletonX_ST::Render	(float LOD) 
{
	//PROF_EVENT("CSkeletonX_ST::Render");
	_Render		(rm_geom,vCount,0,dwPrimitives);
}

//////////////////////////////////////////////////////////////////////
void CSkeletonX_PM::Release()
{
	inherited1::Release();
}
void CSkeletonX_ST::Release()
{
	inherited1::Release();
}
//////////////////////////////////////////////////////////////////////
void CSkeletonX_PM::Load(const char* N, IReader *data, u32 dwFlags) 
{
	_Load							(N,data,vCount);
	void*	_verts_					= data->pointer	();
	inherited1::Load				(N,data,dwFlags|VLOAD_NOVERTICES);
	Engine.External.SetSkinningMode();
#ifdef USE_DX11
	_DuplicateIndices(N, data);
#endif //USE_DX11
	vBase							= 0;
	_Load_hw						(*this,_verts_);
}

void CSkeletonX_ST::Load(const char* N, IReader *data, u32 dwFlags) 
{
	_Load							(N,data,vCount);
	void*	_verts_					= data->pointer	();
	inherited1::Load				(N,data,dwFlags|VLOAD_NOVERTICES);
	Engine.External.SetSkinningMode();
#ifdef USE_DX11
	_DuplicateIndices(N, data);
#endif //USE_DX11
	vBase							= 0;
	_Load_hw						(*this,_verts_);
}

//This is required rn
template<typename DeclT, size_t Size>
u32 ComputeStride(const DeclT(&decl)[Size])
{
    u32 stride = 0;
    for (size_t i = 0; i < Size; ++i)
    {
        switch (decl[i].Format)
        {
        case ERHI_FORMAT::R32G32B32A32_FLOAT: stride += 16; break;
        case ERHI_FORMAT::R32G32B32_FLOAT:    stride += 12; break;
        case ERHI_FORMAT::R32G32_FLOAT:       stride += 8;  break;
        case ERHI_FORMAT::R32_FLOAT:          stride += 4;  break;
        case ERHI_FORMAT::R8G8B8A8_UNORM:     stride += 4;  break;
        case ERHI_FORMAT::R8G8B8A8_UINT:      stride += 4;  break;
        default:
            VERIFY2(false, "Unknown DX11 format!");
        }
    }
    return stride;
}

template<typename VertSrc, typename VertHW, typename DeclT, size_t Size, typename VerticesContainer, typename Setter>
void _Load_hw_generic(Fvisual& V, void* _verts_, DeclT(&decl)[Size], VerticesContainer& container, Setter setter)
{
    // Back up vertex data
    u32 size = V.vCount * sizeof(VertSrc);
    u32 crc = crc32(_verts_, size);
    container.create(crc, V.vCount, (VertSrc*)_verts_);

    u32 vStride = (u32)ComputeStride(decl);

    VERIFY(vStride == sizeof(VertHW));
    VERIFY(nullptr == V.p_rm_Vertices);

    VertHW* dstOriginal = xr_alloc<VertHW>(V.vCount);
    VertHW* dst = dstOriginal;
    VertSrc* src = (VertSrc*)_verts_;
    for (u32 it = 0; it < V.vCount; it++, dst++, src++)
    {
        setter(dst, src);
    }

    R_ASSERT(RHIUtils::CreateVertexBuffer(&V.p_rm_Vertices, dstOriginal, V.vCount * vStride));
    xr_free(dstOriginal);
	V.rm_geom.create(decl, Size, V.p_rm_Vertices, V.p_rm_Indices);
}

void CSkeletonX_ext::_Load_hw(Fvisual& V, void* _verts_)
{
#ifdef USE_DX11
	static int Multipler = 1;
#else
	static int Multipler = 3;
#endif

	switch (RenderMode)
	{
		case RM_SKINNING_SOFT:
		{
			V.rm_geom.create(vertRenderFVF, RCache.Vertex.Buffer(), V.p_rm_Indices);
			break;
		}
		case RM_SINGLE:
		case RM_SKINNING_1B:
		{
			_Load_hw_generic<vertBoned1W, vertHW_1W>
			(
				V, _verts_, dwDecl_01W, Vertices1W,
				[](vertHW_1W* dst, vertBoned1W* src)
				{
					Fvector2 uv; uv.set(src->u, src->v);
					dst->set(src->P, src->N, src->T, src->B, uv, src->matrix * Multipler);
				}
			);
			break;
		}
		case RM_SKINNING_2B:
		{
			_Load_hw_generic<vertBoned2W, vertHW_2W>
			(
				V, _verts_, dwDecl_2W, Vertices2W,
				[](vertHW_2W* dst, vertBoned2W* src)
				{
					Fvector2 uv; uv.set(src->u, src->v);
					dst->set(src->P, src->N, src->T, src->B, uv, int(src->matrix0) * Multipler, int(src->matrix1) * Multipler, src->w);
				}
			);
			break;
		}
		case RM_SKINNING_3B:
		{
			_Load_hw_generic<vertBoned3W, vertHW_3W>
			(
				V, _verts_, dwDecl_3W, Vertices3W,
				[](vertHW_3W* dst, vertBoned3W* src)
				{
					Fvector2 uv; uv.set(src->u, src->v);
					dst->set(src->P, src->N, src->T, src->B, uv, int(src->m[0]) * Multipler, int(src->m[1]) * Multipler, int(src->m[2]) * Multipler, src->w[0], src->w[1]);
				}
			);
			break;
		}
		case RM_SKINNING_4B:
		{
			_Load_hw_generic<vertBoned4W, vertHW_4W>
			(
				V, _verts_, dwDecl_4W, Vertices4W,
				[](vertHW_4W* dst, vertBoned4W* src)
				{
					Fvector2 uv; uv.set(src->u, src->v);
					dst->set(src->P, src->N, src->T, src->B, uv, int(src->m[0]) * Multipler, int(src->m[1]) * Multipler, int(src->m[2]) * Multipler, int(src->m[3]) * Multipler, src->w[0], src->w[1], src->w[2]);
				}
			);
			break;
		}
	}
}


//-----------------------------------------------------------------------------------------------------
// Wallmarks
//-----------------------------------------------------------------------------------------------------
#include "cl_intersect.h"

#ifdef	DEBUG

template	< typename vertex_type >
static void verify_vertex( const vertex_type& v, const Fvisual* V, const CKinematics *Parent, u32 iBase, u32 iCount, const u16 *indices, u32 vertex_idx, u32 idx )
{
	VERIFY(Parent);
#ifndef _EDITOR
	for( u8 i =0; i<vertex_type::bones_count; ++i )
		if( v.get_bone_id(i) >= Parent->LL_BoneCount() )
		{
			Msg( "v.get_bone_id(i): %d, Parent->LL_BoneCount() %d ", v.get_bone_id(i), Parent->LL_BoneCount() );
			Msg( "&v: %p, &V: %p, indices: %p", &v, V, indices );
			Msg( " iBase: %d, iCount: %d, V->iBase %d, V->iCount %d, V->vBase: %d,  V->vCount  %d, vertex_idx: %d, idx: %d", iBase, iCount, V->iBase, V->iCount, V->vBase, V->vCount, vertex_idx, idx  );
			Msg( " v.P: %s , v.N: %s, v.T: %s, v.B: %s", get_string( v.P ).c_str(),get_string(  v.N ).c_str(),get_string(  v.T ).c_str(),get_string(  v.B  ).c_str());
			Msg( "Parent->dbg_name: %s ", Parent->dbg_name.c_str() );
			xrLogger::FlushLog();
			FATAL( "v.get_bone_id(i) >= Parent->LL_BoneCount()" );
		}
#endif        
}
#endif

void CSkeletonX_ext::_CollectBoneFaces(Fvisual* V, u32 iBase, u32 iCount)
{
#ifdef USE_DX11
	u16* indices = *m_Indices;
#else
	RHIMappedSubresource mappedIdx = {};
	if (!V->p_rm_Indices->Map(ERHI_BUFFER_MAP::READ, 0, &mappedIdx))
		return;

	u16* indices = reinterpret_cast<u16*>(mappedIdx.pData) + iBase;
#endif

#ifndef USE_DX11
	if (RenderMode == RM_SKINNING_SOFT)
#endif
	{
		if (*Vertices1W)
		{
			vertBoned1W* vertices = *Vertices1W;
			for (u32 idx = 0; idx < iCount; idx++)
			{
				vertBoned1W& v = vertices[V->vBase + indices[idx]];
#ifdef DEBUG
				verify_vertex(v, V, Parent, iBase, iCount, indices, V->vBase + indices[idx], idx);
#endif
				CBoneData& BD = Parent->LL_GetData((u16)v.matrix);
				BD.AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
		else if (*Vertices2W)
		{
			vertBoned2W* vertices = *Vertices2W;
			for (u32 idx = 0; idx < iCount; idx++)
			{
				vertBoned2W& v = vertices[V->vBase + indices[idx]];
#ifdef DEBUG
				verify_vertex(v, V, Parent, iBase, iCount, indices, V->vBase + indices[idx], idx);
#endif
				Parent->LL_GetData((u16)v.matrix0).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData((u16)v.matrix1).AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
		else if (*Vertices3W)
		{
			vertBoned3W* vertices = *Vertices3W;
			for (u32 idx = 0; idx < iCount; idx++)
			{
				vertBoned3W& v = vertices[V->vBase + indices[idx]];
#ifdef DEBUG
				verify_vertex(v, V, Parent, iBase, iCount, indices, V->vBase + indices[idx], idx);
#endif
				Parent->LL_GetData((u16)v.m[0]).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData((u16)v.m[1]).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData((u16)v.m[2]).AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
		else if (*Vertices4W)
		{
			vertBoned4W* vertices = *Vertices4W;
			for (u32 idx = 0; idx < iCount; idx++)
			{
				vertBoned4W& v = vertices[V->vBase + indices[idx]];
#ifdef DEBUG
				verify_vertex(v, V, Parent, iBase, iCount, indices, V->vBase + indices[idx], idx);
#endif
				Parent->LL_GetData((u16)v.m[0]).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData((u16)v.m[1]).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData((u16)v.m[2]).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData((u16)v.m[3]).AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
	}
#ifndef USE_DX11
	else
	{
		RHIMappedSubresource mappedV = {};
		if (!V->p_rm_Vertices->Map(ERHI_BUFFER_MAP::READ, 0, &mappedV))
		{
			V->p_rm_Indices->Unmap();
			return;
		}

		switch (RenderMode)
		{
		case RM_SINGLE:
		case RM_SKINNING_1B:
		{
			vertHW_1W* vertices = reinterpret_cast<vertHW_1W*>(mappedV.pData);

			for (u32 idx = 0; idx < iCount; idx++)
			{
				vertHW_1W& v = vertices[V->vBase + indices[idx]];
				Parent->LL_GetData(v.get_bone()).AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
		break;
		case RM_SKINNING_2B:
		{
			vertHW_2W* vertices = reinterpret_cast<vertHW_2W*>(mappedV.pData);
			for (u32 idx = 0; idx < iCount; idx++)
			{
				vertHW_2W& v = vertices[V->vBase + indices[idx]];
				Parent->LL_GetData(v.get_bone(0)).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData(v.get_bone(1)).AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
		break;
		case RM_SKINNING_3B:
		{
			vertHW_3W* vertices = reinterpret_cast<vertHW_3W*>(mappedV.pData);
			for (u32 idx = 0; idx < iCount; idx++)
			{
				vertHW_3W& v = vertices[V->vBase + indices[idx]];
				Parent->LL_GetData(v.get_bone(0)).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData(v.get_bone(1)).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData(v.get_bone(2)).AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
		break;
		case RM_SKINNING_4B:
		{
			vertHW_4W* vertices = reinterpret_cast<vertHW_4W*>(mappedV.pData);
			for (u32 idx = 0; idx < iCount; idx++)
			{
				vertHW_4W& v = vertices[V->vBase + indices[idx]];
				Parent->LL_GetData(v.get_bone(0)).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData(v.get_bone(1)).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData(v.get_bone(2)).AppendFace(ChildIDX, (u16)(idx / 3));
				Parent->LL_GetData(v.get_bone(3)).AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
		break;
		default:
			R_ASSERT2(0, "Unsupported RenderMode");
		}

		V->p_rm_Vertices->Unmap();
	}
	V->p_rm_Indices->Unmap();
#endif
}


void CSkeletonX_ST::AfterLoad(CKinematics* parent, u16 child_idx)
{
	inherited2::AfterLoad			(parent,child_idx);
	inherited2::_CollectBoneFaces	(this,iBase,iCount);
}

void CSkeletonX_PM::AfterLoad(CKinematics* parent, u16 child_idx)
{
	inherited2::AfterLoad			(parent,child_idx);
	FSlideWindow& SW				= nSWI.sw[0]; // max LOD
	inherited2::_CollectBoneFaces	(this,iBase+SW.offset,SW.num_tris*3);
}

template<typename T>
IC void get_pos_bones(const T& v, Fvector& p, CKinematics* Parent )
{
	v.get_pos_bones( p, Parent );
}

BOOL CSkeletonX_ext::_PickBoneHW1W		(IKinematics::pick_result &r, float dist, const Fvector& S, const Fvector& D, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertHW_1W>(Parent,r, dist, S, D, V, indices, faces);
}
BOOL CSkeletonX_ext::_PickBoneHW2W		(IKinematics::pick_result &r, float dist, const Fvector& S, const Fvector& D, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertHW_2W>(Parent,r, dist, S, D, V, indices, faces);
}

BOOL CSkeletonX_ext::_PickBoneHW3W(IKinematics::pick_result &r, float dist, const Fvector& S, const Fvector& D, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertHW_3W>(Parent,r, dist, S, D, V, indices, faces);
}
BOOL CSkeletonX_ext::_PickBoneHW4W(IKinematics::pick_result &r, float dist, const Fvector& S, const Fvector& D, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertHW_4W>(Parent,r, dist, S, D, V, indices, faces);
}

BOOL CSkeletonX_ext::_PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, Fvisual* V, u16 bone_id, u32 iBase, u32 iCount)
{
	VERIFY(Parent && (ChildIDX != u16(-1)));
	CBoneData& BD = Parent->LL_GetData(bone_id);
	CBoneData::FacesVec* faces = &BD.child_faces[ChildIDX];
	BOOL result = FALSE;

#ifdef USE_DX11
	u16* indices = *m_Indices;
#else //USE_DX11

	RHIMappedSubresource mappedIdx = {};
	if (!V->p_rm_Indices->Map(ERHI_BUFFER_MAP::READ, 0, &mappedIdx))
	{
		return false;
	}

	u16* indices = reinterpret_cast<u16*>(mappedIdx.pData) + iBase;

	// fill vertices
	switch (RenderMode)
	{
	case RM_SKINNING_SOFT:
#endif

		if (*Vertices1W)
			result = _PickBoneSoft1W(r, dist, start, dir, indices, *faces);
		else if (*Vertices2W)
			result = _PickBoneSoft2W(r, dist, start, dir, indices, *faces);
		else if (*Vertices3W)
			result = _PickBoneSoft3W(r, dist, start, dir, indices, *faces);
		else {
			VERIFY(!!(*Vertices4W));
			result = _PickBoneSoft4W(r, dist, start, dir, indices, *faces);
		}

#ifndef USE_DX11
		break;
	case RM_SINGLE:
	case RM_SKINNING_1B:	result = _PickBoneHW1W(r, dist, start, dir, V, indices, *faces); break;
	case RM_SKINNING_2B:	result = _PickBoneHW2W(r, dist, start, dir, V, indices, *faces);	break;
	case RM_SKINNING_3B:	result = _PickBoneHW3W(r, dist, start, dir, V, indices, *faces);	break;
	case RM_SKINNING_4B:	result = _PickBoneHW4W(r, dist, start, dir, V, indices, *faces);	break;
	default: NODEFAULT;
	}
	V->p_rm_Indices->Unmap();
#endif //USE_DX11

	return result;
}

BOOL CSkeletonX_ST::PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id)
{
	return inherited2::_PickBone(r, dist, start, dir, this, bone_id, iBase, iCount);
}

BOOL CSkeletonX_PM::PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id)
{
	FSlideWindow& SW = nSWI.sw[0];
	return inherited2::_PickBone(r, dist, start, dir, this, bone_id, iBase + SW.offset, SW.num_tris * 3);
}

void	CSkeletonX_ST::		EnumBoneVertices( SEnumVerticesCallback &C, u16 bone_id )
{
	inherited2::_EnumBoneVertices( C, this, bone_id, iBase, iCount );
}

void	CSkeletonX_PM::		EnumBoneVertices( SEnumVerticesCallback &C, u16 bone_id )
{
	FSlideWindow& SW				= nSWI.sw[0];
	inherited2::_EnumBoneVertices( C, this, bone_id, iBase+SW.offset, SW.num_tris*3 );
}

#ifdef USE_DX11

void CSkeletonX_ext::_FillVerticesHW1W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	R_ASSERT2(0,"CSkeletonX_ext::_FillVerticesHW1W not implemented");
}
void CSkeletonX_ext::_FillVerticesHW2W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	R_ASSERT2(0,"CSkeletonX_ext::_FillVerticesHW2W not implemented");
}

void CSkeletonX_ext::_FillVerticesHW3W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	R_ASSERT2(0,"CSkeletonX_ext::_FillVerticesHW3W not implemented");
}

void CSkeletonX_ext::_FillVerticesHW4W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	R_ASSERT2(0,"CSkeletonX_ext::_FillVerticesHW4W not implemented");
}

#else //USE_DX11

void CSkeletonX_ext::_FillVerticesHW1W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	RHIMappedSubresource mapped = {};
	if (!V->p_rm_Vertices->Map(ERHI_BUFFER_MAP::READ, 0, &mapped))
	{
		Msg("! _FillVerticesHW1W: failed to map vertex buffer");
		return;
	}

	vertHW_1W* vertices = reinterpret_cast<vertHW_1W*>(mapped.pData);

	for (CBoneData::FacesVecIt it = faces.begin(); it != faces.end(); ++it)
	{
		Fvector p[3];
		u32 idx = (*it) * 3;
		CSkeletonWallmark::WMFace F;

		for (u32 k = 0; k < 3; ++k)
		{
			vertHW_1W& vert = vertices[V->vBase + indices[idx + k]];
			F.bone_id[k][0] = vert.get_bone();
			F.bone_id[k][1] = F.bone_id[k][0];
			F.weight[k] = 0.f;

			const Fmatrix& xform = Parent->LL_GetBoneInstance(F.bone_id[k][0]).mRenderTransform;
			vert.get_pos(F.vert[k]);
			xform.transform_tiny(p[k], F.vert[k]);
		}

		Fvector test_normal;
		test_normal.mknormal(p[0], p[1], p[2]);
		float cosa = test_normal.dotproduct(normal);
		if (cosa < EPS) continue;

		if (CDB::TestSphereTri(wm.ContactPoint(), size, p))
		{
			Fvector UV;
			for (u32 k = 0; k < 3; ++k)
			{
				Fvector2& uv = F.uv[k];
				view.transform_tiny(UV, p[k]);
				uv.x = (1 + UV.x) * 0.5f;
				uv.y = (1 - UV.y) * 0.5f;
			}
			wm.m_Faces.push_back(F);
		}
	}

	V->p_rm_Vertices->Unmap();
}

void CSkeletonX_ext::_FillVerticesHW2W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	RHIMappedSubresource mapped = {};
	if (!V->p_rm_Vertices->Map(ERHI_BUFFER_MAP::READ, 0, &mapped))
	{
		Msg("! _FillVerticesHW2W: failed to map vertex buffer");
		return;
	}

	vertHW_2W* vertices = reinterpret_cast<vertHW_2W*>(mapped.pData);

	for (CBoneData::FacesVecIt it = faces.begin(); it != faces.end(); ++it)
	{
		Fvector p[3];
		u32 idx = (*it) * 3;
		CSkeletonWallmark::WMFace F;

		for (u32 k = 0; k < 3; ++k)
		{
			Fvector P0, P1;
			vertHW_2W& vert = vertices[V->vBase + indices[idx + k]];
			F.bone_id[k][0] = vert.get_bone(0);
			F.bone_id[k][1] = vert.get_bone(1);
			F.weight[k] = vert.get_weight();

			Fmatrix& xform0 = Parent->LL_GetBoneInstance(F.bone_id[k][0]).mRenderTransform;
			Fmatrix& xform1 = Parent->LL_GetBoneInstance(F.bone_id[k][1]).mRenderTransform;

			vert.get_pos(F.vert[k]);
			xform0.transform_tiny(P0, F.vert[k]);
			xform1.transform_tiny(P1, F.vert[k]);
			p[k].lerp(P0, P1, F.weight[k]);
		}

		Fvector test_normal;
		test_normal.mknormal(p[0], p[1], p[2]);
		float cosa = test_normal.dotproduct(normal);
		if (cosa < EPS) continue;

		if (CDB::TestSphereTri(wm.ContactPoint(), size, p))
		{
			Fvector UV;
			for (u32 k = 0; k < 3; ++k)
			{
				Fvector2& uv = F.uv[k];
				view.transform_tiny(UV, p[k]);
				uv.x = (1 + UV.x) * 0.5f;
				uv.y = (1 - UV.y) * 0.5f;
			}
			wm.m_Faces.push_back(F);
		}
	}

	V->p_rm_Vertices->Unmap();
}


void CSkeletonX_ext::_FillVerticesHW3W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	R_ASSERT2(0,"CSkeletonX_ext::_FillVerticesHW3W not implemented");
}

void CSkeletonX_ext::_FillVerticesHW4W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, Fvisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	R_ASSERT2(0,"CSkeletonX_ext::_FillVerticesHW4W not implemented");
}
#endif


void CSkeletonX_ext::_FillVertices(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, Fvisual* V, u16 bone_id, u32 iBase, u32 iCount)
{
	R_ASSERT2(0,"CSkeletonX_ext::_FillVertices not implemented");
}

void CSkeletonX_ST::FillVertices	(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, u16 bone_id)
{
	inherited2::_FillVertices		(view,wm,normal,size,this,bone_id,iBase,iCount);
}

void CSkeletonX_PM::FillVertices	(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, u16 bone_id)
{
	FSlideWindow& SW				= nSWI.sw[0];
	inherited2::_FillVertices		(view,wm,normal,size,this,bone_id,iBase+SW.offset,SW.num_tris*3);
}

template <typename vertex_buffer_type>
IC void TEnumBoneVertices	(vertex_buffer_type vertices, u16* indices, CBoneData::FacesVec& faces, SEnumVerticesCallback &C ) 
{
		for (CBoneData::FacesVecIt it=faces.begin(); it!=faces.end(); it++){
			u32 idx			= (*it)*3;
			for (u32 k=0; k<3; k++){
				Fvector		P;
				vertices[indices[idx+k]].get_pos( P );
				C( P );
			}
		}
}

void CSkeletonX_ext::_EnumBoneVertices(SEnumVerticesCallback& C, Fvisual* V, u16 bone_id, u32 iBase, u32 iCount) const
{
	VERIFY(Parent && (ChildIDX != u16(-1)));
	CBoneData& BD = Parent->LL_GetData(bone_id);
	CBoneData::FacesVec* faces = &BD.child_faces[ChildIDX];

	// Map index buffer
#ifdef USE_DX11
	u16* indices = *m_Indices;
#else
	RHIMappedSubresource mappedIndices = {};
	if (!V->p_rm_Indices->Map(ERHI_BUFFER_MAP::READ, 0, &mappedIndices))
	{
		Msg("! _EnumBoneVertices: failed to map index buffer");
		return;
	}

	u16* indices = reinterpret_cast<u16*>(mappedIndices.pData) + iBase;

	if (RenderMode == RM_SKINNING_SOFT)
#endif
	{
		if (*Vertices1W)
			TEnumBoneVertices(Vertices1W, indices, *faces, C);
		else if (*Vertices2W)
			TEnumBoneVertices(Vertices2W, indices, *faces, C);
		else if (*Vertices3W)
			TEnumBoneVertices(Vertices3W, indices, *faces, C);
		else
		{
			VERIFY(*Vertices4W);
			TEnumBoneVertices(Vertices4W, indices, *faces, C);
		}
	}
#ifndef USE_DX11
	else
	{
		// Map vertex buffer
		RHIMappedSubresource mappedVertices = {};
		if (!V->p_rm_Vertices->Map(ERHI_BUFFER_MAP::READ, 0, &mappedVertices))
		{
			Msg("! _EnumBoneVertices: failed to map vertex buffer");
			V->p_rm_Indices->Unmap();
			return;
		}

		void* vertices = mappedVertices.pData;

		switch (RenderMode)
		{
		case RM_SINGLE:
		case RM_SKINNING_1B: TEnumBoneVertices((vertHW_1W*)vertices, indices, *faces, C); break;
		case RM_SKINNING_2B: TEnumBoneVertices((vertHW_2W*)vertices, indices, *faces, C); break;
		case RM_SKINNING_3B: TEnumBoneVertices((vertHW_3W*)vertices, indices, *faces, C); break;
		case RM_SKINNING_4B: TEnumBoneVertices((vertHW_4W*)vertices, indices, *faces, C); break;
		default: NODEFAULT;
		}

		V->p_rm_Vertices->Unmap();
	}

	V->p_rm_Indices->Unmap();
#endif
}
