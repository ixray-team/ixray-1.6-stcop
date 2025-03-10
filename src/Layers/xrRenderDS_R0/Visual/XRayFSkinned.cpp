#include "pch.h"
static shared_str sbones_array;

#pragma pack(push, 1)

float u_P(s16 v)
{
	return float(v) / (32767.f / 12.f);
}

s16 q_P(float v)
{
	int _v = clampr(iFloor(v * (32767.f / 12.f)), -32768, 32767);
	return s16(_v);
}

u8 q_N(float v)
{
	int _v =  clampr(iFloor((v + 1.f) * 127.5f), 0, 255);
	return u8(_v);
}

s16 q_tc(float v)
{
	int _v =  clampr(iFloor(v * (32767.f / 16.f)), -32768, 32767);
	return s16(_v);
}

#ifdef _DEBUG
float errN(Fvector3 v, u8* qv)
{
	Fvector3 uv;
	uv.set(float(qv[0]), float(qv[1]), float(qv[2])).div(255.f).mul(2.f).sub(1.f);
	uv.normalize();
	return v.dotproduct(uv);
}
#else
float errN(Fvector3 v, u8* qv)
{
	return 0;
}
#endif


struct vertHW_1W
{
	float _P[4];
	u32 _N_I;
	u32 _T;
	u32 _B;
	float _tc[2];

	void set(Fvector3& P, Fvector3 N, Fvector3 T, Fvector3 B, Fvector2& tc, int index)
	{
		N.normalize_safe();
		T.normalize_safe();
		B.normalize_safe();

		_P[0] = P.x;
		_P[1] = P.y;
		_P[2] = P.z;
		_P[3] = 1.f;

		_N_I = color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(index));
		_T = color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), 0);
		_B = color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), 0);

		_tc[0] = tc.x;
		_tc[1] = tc.y;
	}

	u16 get_bone() const
	{
		return (u16)color_get_A(_N_I) / 3;
	}

	void get_pos_bones(Fvector& p, XRayKinematics* Parent) const
	{
		const Fmatrix& xform = Parent->LL_GetBoneInstance(get_bone()).mRenderTransform;
		get_pos(p);
		xform.transform_tiny(p);
	}

	void get_pos(Fvector& p) const
	{
		p.x = _P[0];
		p.y = _P[1];
		p.z = _P[2];
	}
};


struct vertHW_2W
{
	float _P[4];
	u32 _N_w;
	u32 _T;
	u32 _B;
	float _tc_i[4];

	void set(Fvector3& P, Fvector3 N, Fvector3 T, Fvector3 B, Fvector2& tc, int index0, int index1, float w)
	{
		N.normalize_safe();
		T.normalize_safe();
		B.normalize_safe();

		_P[0] = P.x;
		_P[1] = P.y;
		_P[2] = P.z;
		_P[3] = 1.f;

		_N_w = color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(clampr(iFloor(w * 255.f + .5f), 0, 255)));
		_T = color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), 0);
		_B = color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), 0);

		_tc_i[0] = tc.x;
		_tc_i[1] = tc.y;
		_tc_i[2] = s16(index0);
		_tc_i[3] = s16(index1);
	}

	float get_weight() const
	{
		return float(color_get_A(_N_w)) / 255.f;
	}

	u16 get_bone(u16 w) const
	{
		return (u16)_tc_i[w + 2] / 3;
	}

	void get_pos(Fvector& p) const
	{
		p.x = _P[0];
		p.y = _P[1];
		p.z = _P[2];
	}

	void get_pos_bones(Fvector& p, XRayKinematics* Parent) const
	{
		Fvector P0, P1;

		Fmatrix& xform0 = Parent->LL_GetBoneInstance(get_bone(0)).mRenderTransform;
		Fmatrix& xform1 = Parent->LL_GetBoneInstance(get_bone(1)).mRenderTransform;

		get_pos(P0);
		xform0.transform_tiny(P0);
		get_pos(P1);
		xform1.transform_tiny(P1);

		p.lerp(P0, P1, get_weight());
	}
};


struct vertHW_3W
{
	float _P[4];
	u32 _N_w;
	u32 _T_w;
	u32 _B_i;
	float _tc_i[4];

	void set(Fvector3& P, Fvector3 N, Fvector3 T, Fvector3 B, Fvector2& tc, int index0, int index1, int index2, float w0, float w1)
	{
		N.normalize_safe();
		T.normalize_safe();
		B.normalize_safe();

		_P[0] = P.x;
		_P[1] = P.y;
		_P[2] = P.z;
		_P[3] = 1.f;

		_N_w = color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(clampr(iFloor(w0 * 255.f + .5f), 0, 255)));
		_T_w = color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), u8(clampr(iFloor(w1 * 255.f + .5f), 0, 255)));
		_B_i = color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), u8(index2));

		_tc_i[0] = tc.x;
		_tc_i[1] = tc.y;
		_tc_i[2] = s16(index0);
		_tc_i[3] = s16(index1);
	}

	float get_weight0() const
	{
		return float(color_get_A(_N_w)) / 255.f;
	}

	float get_weight1() const
	{
		return float(color_get_A(_T_w)) / 255.f;
	}

	u16 get_bone(u16 w) const
	{
		switch (w)
		{
		case 0:
		case 1:
			return (u16)_tc_i[w + 2] / 3;
		case 2:
			return (u16)color_get_A(_B_i) / 3;
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

	void get_pos_bones(Fvector& p, XRayKinematics* Parent) const
	{
		Fvector P0, P1, P2;

		Fmatrix& xform0 = Parent->LL_GetBoneInstance(get_bone(0)).mRenderTransform;
		Fmatrix& xform1 = Parent->LL_GetBoneInstance(get_bone(1)).mRenderTransform;
		Fmatrix& xform2 = Parent->LL_GetBoneInstance(get_bone(2)).mRenderTransform;

		get_pos(P0);
		xform0.transform_tiny(P0);
		get_pos(P1);
		xform1.transform_tiny(P1);
		get_pos(P2);
		xform2.transform_tiny(P2);

		float w0 = get_weight0();
		float w1 = get_weight1();

		P0.mul(w0);
		P1.mul(w1);
		P2.mul(1 - w0 - w1);

		p = P0;
		p.add(P1);
		p.add(P2);
	}
};



struct vertHW_4W
{
	float _P[4];
	u32 _N_w;
	u32 _T_w;
	u32 _B_w;
	float _tc[2];
	u32 _i;

	void set(Fvector3& P, Fvector3 N, Fvector3 T, Fvector3 B, Fvector2& tc, int index0, int index1, int index2, int index3, float w0, float w1, float w2)
	{
		N.normalize_safe();
		T.normalize_safe();
		B.normalize_safe();

		_P[0] = P.x;
		_P[1] = P.y;
		_P[2] = P.z;
		_P[3] = 1.f;

		_N_w = color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(clampr(iFloor(w0 * 255.f + .5f), 0, 255)));
		_T_w = color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), u8(clampr(iFloor(w1 * 255.f + .5f), 0, 255)));
		_B_w = color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), u8(clampr(iFloor(w2 * 255.f + .5f), 0, 255)));

		_tc[0] = tc.x;
		_tc[1] = tc.y;

		_i = color_rgba(u8(index0), u8(index1), u8(index2), u8(index3));
	}

	float get_weight0() const
	{
		return float(color_get_A(_N_w)) / 255.f;
	}

	float get_weight1() const
	{
		return float(color_get_A(_T_w)) / 255.f;
	}

	float get_weight2() const
	{
		return float(color_get_A(_B_w)) / 255.f;
	}

	u16 get_bone(u16 w) const
	{
		switch (w)
		{
		case 0:
			return (u16)color_get_R(_i) / 3;
		case 1:
			return (u16)color_get_G(_i) / 3;
		case 2:
			return (u16)color_get_B(_i) / 3;
		case 3:
			return (u16)color_get_A(_i) / 3;
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

	void get_pos_bones(Fvector& p, XRayKinematics* Parent) const
	{
		Fvector P[4];

		for (u16 i = 0; i < 4; ++i)
		{
			Fmatrix& xform = Parent->LL_GetBoneInstance(get_bone(i)).mRenderTransform;
			get_pos(P[i]);

			xform.transform_tiny(P[i]);
		}

		float w[3];

		w[0] = get_weight0();
		w[1] = get_weight1();
		w[2] = get_weight2();

		for (int j = 0; j < 3; ++j)
			P[j].mul(w[j]);

		P[3].mul(1 - w[0] - w[1] - w[2]);

		p = P[0];

		for (int k = 1; k < 4; ++k)
			p.add(P[k]);
	}
};

#pragma pack(pop)

//////////////////////////////////////////////////////////////////////
// Body Part

void XRaySkeletonX_PM::Copy(XRayRenderVisual* V)
{
	inherited1::Copy(V);

	XRaySkeletonX_PM* X = (XRaySkeletonX_PM*)(V);
	_Copy((XRaySkeletonX*)X);
}

void XRaySkeletonX_ST::Copy(XRayRenderVisual* P)
{
	inherited1::Copy(P);

	XRaySkeletonX_ST* X = (XRaySkeletonX_ST*)P;
	_Copy((XRaySkeletonX*)X);
}


bool XRaySkeletonX_PM::Render(float LOD, EShaderElement SEType, XRayObjectRender& Item)
{
	if (RenderMode == RM_SINGLE)
	{
		GRenderInterface.UpdateDescriptorHeap(Shader.E[1][SEType]);
		if (!Shader.E[1][SEType].Set(Item, FVF)) { return false; }
	}
	else
	{
		GRenderInterface.UpdateDescriptorHeap(Shader.E[0][SEType]);
		if (!Shader.E[0][SEType].Set(Item, FVF)) { return false; }

	}	Item.UseUniform[XRayUniformAllocator::UT_Transformation] = true;

	

	Item.GraphicsPipelineResource.VertexBuffer = VertexBuffer;
	Item.GraphicsPipelineResource.IndexBuffer = IndexBuffer;
	switch (RenderMode)
	{
	case RM_SKINNING_SOFT:
		R_ASSERT(false);
		return false;
		break;
	case RM_SKINNING_1B:
	case RM_SKINNING_2B:
	case RM_SKINNING_3B:
	case RM_SKINNING_4B:
		Item.UseUniform[XRayUniformAllocator::UT_Skinned] = true;
		break;
	}
	int lod_id = inherited1::last_lod;
	if (LOD >= 0.f)
	{
		clamp(LOD, 0.f, 1.f);
		lod_id = static_cast<int>((1.f - LOD) * float(nSWI.count - 1) + 0.5f);

		inherited1::last_lod = lod_id;
	}
	VERIFY(lod_id >= 0 && lod_id < int(nSWI.count));
	FSlideWindow& SW = nSWI.sw[lod_id];

	Item.GraphicsPipelineResource.CountIndex = SW.num_tris * 3;
	Item.GraphicsPipelineResource.OffsetIndex = OffsetIndex + SW.offset;
	Item.GraphicsPipelineResource.OffsetVertex = OffsetVertex;
	Item.Visual = this;
	return Item.GraphicsPipelineResource.CountIndex;
}

bool XRaySkeletonX_ST::Render(float LOD, EShaderElement SEType, XRayObjectRender& Item)
{
	if (RenderMode == RM_SINGLE )
	{
		GRenderInterface.UpdateDescriptorHeap(Shader.E[1][SEType]);
		if (!Shader.E[1][SEType].Set(Item, FVF)) { return false; }
	}
	else
	{
		GRenderInterface.UpdateDescriptorHeap(Shader.E[0][SEType]);
		if (!Shader.E[0][SEType].Set(Item, FVF)) { return false; }

	}

	Item.UseUniform[XRayUniformAllocator::UT_Transformation] = true;
	Item.GraphicsPipelineResource.VertexBuffer = VertexBuffer;
	Item.GraphicsPipelineResource.IndexBuffer = IndexBuffer;
	Item.GraphicsPipelineResource.CountIndex = CountIndex;
	Item.GraphicsPipelineResource.OffsetIndex = OffsetIndex;
	Item.GraphicsPipelineResource.OffsetVertex = OffsetVertex;
	switch (RenderMode)
	{
	case RM_SKINNING_SOFT:
		R_ASSERT(false);
		return false;
		break;
	break;
	case RM_SKINNING_1B:
	case RM_SKINNING_2B:
	case RM_SKINNING_3B:
	case RM_SKINNING_4B:
		Item.UseUniform[XRayUniformAllocator::UT_Skinned] = true;
		break;
	}

	
	Item.Visual = this;
	return Item.GraphicsPipelineResource.CountIndex;
}


#define VLOAD_NOVERTICES 1<<0
void XRaySkeletonX_PM::Load(const char* N, IReader* data, u32 dwFlags)
{
	_Load(N, data, CountVertex);
	void* _verts_ = data->pointer();

	inherited1::Load(N, data, dwFlags | VLOAD_NOVERTICES);

	::Render->shader_option_skinning(-1);

	_DuplicateIndices(N, data);
	_Load_hw(*this, _verts_);
	//R_ASSERT(VertexBuffer.empty());
}

void XRaySkeletonX_ST::Load(const char* N, IReader* data, u32 dwFlags)
{
	_Load(N, data, CountVertex);
	void* _verts_ = data->pointer();

	inherited1::Load(N, data, dwFlags | VLOAD_NOVERTICES);

	::Render->shader_option_skinning(-1);

	_DuplicateIndices(N, data);

	_Load_hw(*this, _verts_);
}

void XRaySkeletonXExt::_Load_hw(XRayFVisual& V, void* _verts_)
{
	V.FVF = 0;
	switch (RenderMode)
	{
	case RM_SKINNING_SOFT:
		R_ASSERT(0);
		//Msg					("skinning: software");
		//V.rm_geom.create(vertRenderFVF, RCache.Vertex.Buffer(), V.p_rm_Indices);
		break;
	case RM_SINGLE:
		V.FVF = FVF::F_0W;
	case RM_SKINNING_1B:
	{
		{ //	Back up vertex data since we can't read vertex buffer in DX10
			u32 size = V.CountVertex * sizeof(vertBoned1W);
			u32 crc = crc32(_verts_, size);
			Vertices1W.create(crc, V.CountVertex, (vertBoned1W*)_verts_);
		}
		if(!V.FVF)
		V.FVF = FVF::F_1W;
		u32 vStride = sizeof(vertHW_1W);

		VERIFY(vStride == sizeof(vertHW_1W));
		VERIFY(V.VertexBuffer.empty());

		//	TODO: DX10: Check for memory fragmentation
		vertHW_1W* dstOriginal = xr_alloc<vertHW_1W>(V.CountVertex);
		vertHW_1W* dst = dstOriginal;
		vertBoned1W* src = (vertBoned1W*)_verts_;

		for (u32 it = 0; it < V.CountVertex; it++)
		{
			Fvector2 uv;
			uv.set(src->u, src->v);
			dst->set(src->P, src->N, src->T, src->B, uv, src->matrix * 3);
			dst++;
			src++;
		}
		V.VertexBuffer = BearRenderInterface::CreateVertexBuffer();
		V.VertexBuffer->Create( vStride, V.CountVertex,false, dstOriginal);

		//HW.stats_manager.increment_stats_vb(V.p_rm_Vertices);

		xr_free(dstOriginal);

	}
	break;

	case RM_SKINNING_2B:
	{
		{ //	Back up vertex data since we can't read vertex buffer in DX10
			u32 size = V.CountVertex * sizeof(vertBoned2W);
			u32 crc = crc32(_verts_, size);
			Vertices2W.create(crc, V.CountVertex, (vertBoned2W*)_verts_);
		}

		V.FVF = FVF::F_2W;
		u32 vStride = sizeof(vertHW_2W);

		VERIFY(vStride == sizeof(vertHW_2W));
		VERIFY(V.VertexBuffer.empty());


		//	TODO: DX10: Check for memory fragmentation
		vertHW_2W* dstOriginal = xr_alloc<vertHW_2W>(V.CountVertex);
		vertHW_2W* dst = dstOriginal;
		vertBoned2W* src = (vertBoned2W*)_verts_;

		for (u32 it = 0; it < V.CountVertex; it++)
		{
			Fvector2 uv;
			uv.set(src->u, src->v);
			dst->set(src->P, src->N, src->T, src->B, uv, int(src->matrix0) * 3, int(src->matrix1) * 3, src->w);
			dst++;
			src++;
		}
		V.VertexBuffer = BearRenderInterface::CreateVertexBuffer();
		V.VertexBuffer->Create(vStride, V.CountVertex, false, dstOriginal);


		xr_free(dstOriginal);

	}
	break;

	case RM_SKINNING_3B:
	{
		{ //	Back up vertex data since we can't read vertex buffer in DX10
			u32 size = V.CountVertex * sizeof(vertBoned3W);
			u32 crc = crc32(_verts_, size);
			Vertices3W.create(crc, V.CountVertex, (vertBoned3W*)_verts_);
		}


		V.FVF = FVF::F_3W;
		u32 vStride = sizeof(vertHW_3W);

		VERIFY(vStride == sizeof(vertHW_3W));
		VERIFY(V.VertexBuffer.empty());

		//	TODO: DX10: Check for memory fragmentation
		vertHW_3W* dstOriginal = xr_alloc<vertHW_3W>(V.CountVertex);
		vertHW_3W* dst = dstOriginal;
		vertBoned3W* src = (vertBoned3W*)_verts_;

		for (u32 it = 0; it < V.CountVertex; it++)
		{
			Fvector2 uv;
			uv.set(src->u, src->v);
			dst->set(src->P, src->N, src->T, src->B, uv, int(src->m[0]) * 3, int(src->m[1]) * 3, int(src->m[2]) * 3, src->w[0], src->w[1]);
			dst++;
			src++;
		}
		V.VertexBuffer = BearRenderInterface::CreateVertexBuffer();
		V.VertexBuffer->Create(vStride, V.CountVertex, false, dstOriginal);

		//HW.stats_manager.increment_stats_vb(V.p_rm_Vertices);

		xr_free(dstOriginal);

	}
	break;

	case RM_SKINNING_4B:
	{
		{ //	Back up vertex data since we can't read vertex buffer in DX10
			u32 size = V.CountVertex * sizeof(vertBoned4W);
			u32 crc = crc32(_verts_, size);
			Vertices4W.create(crc, V.CountVertex, (vertBoned4W*)_verts_);
		}


		V.FVF = FVF::F_4W;
		u32 vStride = sizeof(vertHW_4W);

		VERIFY(vStride == sizeof(vertHW_4W));
		VERIFY(V.VertexBuffer.empty());

		//	TODO: DX10: Check for memory fragmentation
		vertHW_4W* dstOriginal = xr_alloc<vertHW_4W>(V.CountVertex);
		vertHW_4W* dst = dstOriginal;
		vertBoned4W* src = (vertBoned4W*)_verts_;

		for (u32 it = 0; it < V.CountVertex; it++)
		{
			Fvector2 uv;
			uv.set(src->u, src->v);
			dst->set(src->P, src->N, src->T, src->B, uv, int(src->m[0]) * 3, int(src->m[1]) * 3, int(src->m[2]) * 3, int(src->m[3]) * 3, src->w[0], src->w[1], src->w[2]);
			dst++;
			src++;
		}
		V.VertexBuffer = BearRenderInterface::CreateVertexBuffer();
		V.VertexBuffer->Create(vStride, V.CountVertex, false, dstOriginal);

		//HW.stats_manager.increment_stats_vb(V.p_rm_Vertices);

		xr_free(dstOriginal);

	}
	break;
	}
}

//-----------------------------------------------------------------------------------------------------
// Wallmarks
//-----------------------------------------------------------------------------------------------------
#include "..\cl_intersect.h"

#ifdef DEBUG

template <typename vertex_type>
static void verify_vertex(const vertex_type& v, const XRayFVisual* V, const XRayKinematics* Parent, u32 OffsetIndex, u32 CountIndex, const u16* indices, u32 vertex_idx, u32 idx)
{
	VERIFY(Parent);

	for (u8 i = 0; i < vertex_type::bones_count; ++i)
		if (v.get_bone_id(i) >= Parent->LL_BoneCount())
		{
			/*Msg("v.get_bone_id(i): %d, Parent->LL_BoneCount() %d ", v.get_bone_id(i), Parent->LL_BoneCount());
			Msg("&v: %p, &V: %p, indices: %p", &v, V, indices);
			Msg(" OffsetIndex: %d, CountIndex: %d, V->OffsetIndex %d, V->CountIndex %d, V->OffsetVertex: %d,  V->CountVertex  %d, vertex_idx: %d, idx: %d", OffsetIndex, CountIndex, V->OffsetIndex, V->CountIndex, V->OffsetVertex, V->CountVertex, vertex_idx, idx);
			Msg(" v.P: %s , v.N: %s, v.T: %s, v.B: %s", get_string(v.P).c_str(), get_string(v.N).c_str(), get_string(v.T).c_str(), get_string(v.B).c_str());
			Msg("Parent->dbg_name: %s ", Parent->dbg_name.c_str());*/

			FATAL("v.get_bone_id(i) >= Parent->LL_BoneCount()");
		}
}
#endif

void XRaySkeletonXExt::_CollectBoneFaces(XRayFVisual* V, size_t OffsetIndex, size_t CountIndex)
{
	u16* indices = 0;

	indices = *m_Indices;

	indices += OffsetIndex;

		if (*Vertices1W)
		{
			vertBoned1W* vertices = *Vertices1W;

			for (u32 idx = 0; idx < CountIndex; idx++)
			{
				vertBoned1W& v = vertices[V->OffsetVertex + indices[idx]];
#ifdef DEBUG
				verify_vertex(v, V, Parent, OffsetIndex, CountIndex, indices, V->OffsetVertex + indices[idx], idx);
#endif
				CBoneData& BD = Parent->LL_GetData((u16)v.matrix);

				BD.AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
		else if (*Vertices2W)
		{
			vertBoned2W* vertices = *Vertices2W;

			for (u32 idx = 0; idx < CountIndex; ++idx)
			{
				vertBoned2W& v = vertices[V->OffsetVertex + indices[idx]];
#ifdef DEBUG
				verify_vertex(v, V, Parent, OffsetIndex, CountIndex, indices, V->OffsetVertex + indices[idx], idx);
#endif
				CBoneData& BD0 = Parent->LL_GetData((u16)v.matrix0);
				BD0.AppendFace(ChildIDX, (u16)(idx / 3));
				CBoneData& BD1 = Parent->LL_GetData((u16)v.matrix1);
				BD1.AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
		else if (*Vertices3W)
		{
			vertBoned3W* vertices = *Vertices3W;

			for (u32 idx = 0; idx < CountIndex; ++idx)
			{
				vertBoned3W& v = vertices[V->OffsetVertex + indices[idx]];
#ifdef DEBUG
				verify_vertex(v, V, Parent, OffsetIndex, CountIndex, indices, V->OffsetVertex + indices[idx], idx);
#endif
				CBoneData& BD0 = Parent->LL_GetData((u16)v.m[0]);
				BD0.AppendFace(ChildIDX, (u16)(idx / 3));
				CBoneData& BD1 = Parent->LL_GetData((u16)v.m[1]);
				BD1.AppendFace(ChildIDX, (u16)(idx / 3));
				CBoneData& BD2 = Parent->LL_GetData((u16)v.m[2]);
				BD2.AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
		else if (*Vertices4W)
		{
			vertBoned4W* vertices = *Vertices4W;

			for (u32 idx = 0; idx < CountIndex; ++idx)
			{
				vertBoned4W& v = vertices[V->OffsetVertex + indices[idx]];
#ifdef DEBUG
				verify_vertex(v, V, Parent, OffsetIndex, CountIndex, indices, V->OffsetVertex + indices[idx], idx);
#endif
				CBoneData& BD0 = Parent->LL_GetData((u16)v.m[0]);
				BD0.AppendFace(ChildIDX, (u16)(idx / 3));
				CBoneData& BD1 = Parent->LL_GetData((u16)v.m[1]);
				BD1.AppendFace(ChildIDX, (u16)(idx / 3));

				CBoneData& BD2 = Parent->LL_GetData((u16)v.m[2]);
				BD2.AppendFace(ChildIDX, (u16)(idx / 3));
				CBoneData& BD3 = Parent->LL_GetData((u16)v.m[3]);
				BD3.AppendFace(ChildIDX, (u16)(idx / 3));
			}
		}
		else
			R_ASSERT2(0, "not implemented yet");
	
}

void XRaySkeletonX_ST::AfterLoad(XRayKinematics* parent, u16 child_idx)
{
	inherited2::AfterLoad(parent, child_idx);
	inherited2::_CollectBoneFaces(this, OffsetIndex, CountIndex);
}

void XRaySkeletonX_PM::AfterLoad(XRayKinematics* parent, u16 child_idx)
{
	inherited2::AfterLoad(parent, child_idx);
	FSlideWindow& SW = nSWI.sw[0]; // max LOD

	inherited2::_CollectBoneFaces(this, OffsetIndex + SW.offset, SW.num_tris * 3);
}

template <typename T>
IC void get_pos_bones(const T& v, Fvector& p, XRayKinematics* Parent)
{
	v.get_pos_bones(p, Parent);
}
template<typename T>
BOOL pick_bone(XRayKinematics* Parent, IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, XRayFVisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	VERIFY(!"Not implemented");
	return FALSE;
}
BOOL XRaySkeletonXExt::_PickBoneHW1W(IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, XRayFVisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertHW_1W>(Parent, r, dist, S, D, V, indices, faces);
}

BOOL XRaySkeletonXExt::_PickBoneHW2W(IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, XRayFVisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertHW_2W>(Parent, r, dist, S, D, V, indices, faces);
}

BOOL XRaySkeletonXExt::_PickBoneHW3W(IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, XRayFVisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertHW_3W>(Parent, r, dist, S, D, V, indices, faces);
}

BOOL XRaySkeletonXExt::_PickBoneHW4W(IKinematics::pick_result& r, float dist, const Fvector& S, const Fvector& D, XRayFVisual* V, u16* indices, CBoneData::FacesVec& faces)
{
	return pick_bone<vertHW_4W>(Parent, r, dist, S, D, V, indices, faces);
}

BOOL XRaySkeletonXExt::_PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, XRayFVisual* V, u16 bone_id, size_t OffsetIndex, size_t CountIndex)
{
	VERIFY(Parent && (ChildIDX != u16(-1)));

	CBoneData& BD = Parent->LL_GetData(bone_id);
	CBoneData::FacesVec* faces = &BD.child_faces[ChildIDX];

	BOOL result = FALSE;
	u16* indices = 0;

	indices = *m_Indices;

	if (*Vertices1W)
		result = _PickBoneSoft1W(r, dist, start, dir, indices + OffsetIndex, *faces);
	else if (*Vertices2W)
		result = _PickBoneSoft2W(r, dist, start, dir, indices + OffsetIndex, *faces);
	else if (*Vertices3W)
		result = _PickBoneSoft3W(r, dist, start, dir, indices + OffsetIndex, *faces);
	else
	{
		VERIFY(!!(*Vertices4W));

		result = _PickBoneSoft4W(r, dist, start, dir, indices + OffsetIndex, *faces);
	}

	return result;
}

void XRaySkeletonXExt::UpdateUniform(void* ptr)
{
	switch (RenderMode)
	{
	case RM_SKINNING_1B:
	case RM_SKINNING_2B:
	case RM_SKINNING_3B:
	case RM_SKINNING_4B:
	{
		// transfer matrices
		Fvector4* array = (Fvector4*)ptr;
		u32						count = RMS_bonecount;
		for (u32 mid = 0; mid < count; mid++)
		{
			Fmatrix& M = Parent->LL_GetTransform_R(u16(mid));
			u32			id = mid * 3;
			array[id + 0].set(M._11, M._21, M._31, M._41);
			array[id + 1].set(M._12, M._22, M._32, M._42);
			array[id + 2].set(M._13, M._23, M._33, M._43);
		}
	}
	break;
	}
}

BOOL XRaySkeletonX_ST::PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id)
{
	return inherited2::_PickBone(r, dist, start, dir, this, bone_id, OffsetIndex, CountIndex);
}

void XRaySkeletonX_ST::UpdateUniform(XRayUniformAllocator::EUniformType Type, void* ptr)
{
	if (Type == XRayUniformAllocator::EUniformType::UT_Skinned)
	{
		XRaySkeletonXExt::UpdateUniform(ptr);
	}
}

BOOL XRaySkeletonX_PM::PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id)
{
	FSlideWindow& SW = nSWI.sw[0];
	return inherited2::_PickBone(r, dist, start, dir, this, bone_id, OffsetIndex + SW.offset, SW.num_tris * 3);
}

void XRaySkeletonX_PM::UpdateUniform(XRayUniformAllocator::EUniformType Type, void* ptr)
{
	if (Type == XRayUniformAllocator::EUniformType::UT_Skinned)
	{
		XRaySkeletonXExt::UpdateUniform(ptr);
	}
}

void XRaySkeletonX_ST::EnumBoneVertices(SEnumVerticesCallback& C, u16 bone_id)
{
	inherited2::_EnumBoneVertices(C, this, bone_id, OffsetIndex, CountIndex);
}

void XRaySkeletonX_PM::EnumBoneVertices(SEnumVerticesCallback& C, u16 bone_id)
{
	FSlideWindow& SW = nSWI.sw[0];
	inherited2::_EnumBoneVertices(C, this, bone_id, OffsetIndex + SW.offset, SW.num_tris * 3);
}


template <typename vertex_buffer_type>
IC void TEnumBoneVertices(vertex_buffer_type vertices, u16* indices, CBoneData::FacesVec& faces, SEnumVerticesCallback& C)
{
	for (auto it = faces.begin(); it != faces.end(); it++)
	{
		u32 idx = (*it) * 3;

		for (u32 k = 0; k < 3; k++)
		{
			Fvector P;
			vertices[indices[idx + k]].get_pos(P);

			C(P);
		}
	}
}

void XRaySkeletonXExt::_EnumBoneVertices(SEnumVerticesCallback& C, XRayFVisual* V, u16 bone_id, size_t OffsetIndex, size_t CountIndex) const
{

	VERIFY(Parent && (ChildIDX != u16(-1)));

	CBoneData& BD = Parent->LL_GetData(bone_id);
	CBoneData::FacesVec* faces = &BD.child_faces[ChildIDX];

	u16* indices = 0;

	VERIFY(*m_Indices);

	indices = *m_Indices;

	if (*Vertices1W)
		TEnumBoneVertices(Vertices1W, indices + OffsetIndex, *faces, C);
	else if (*Vertices2W)
		TEnumBoneVertices(Vertices2W, indices + OffsetIndex, *faces, C);
	else if (*Vertices3W)
		TEnumBoneVertices(Vertices3W, indices + OffsetIndex, *faces, C);
	else
	{
		VERIFY(!!(*Vertices4W));

		TEnumBoneVertices(Vertices4W, indices + OffsetIndex, *faces, C);
	}
}
