#include "stdafx.h"
#include "DetailManager.h"

struct vertHW
{
	Fvector4 pos_frac;
	Fvector2 uv;
	u32 normal = 0;
};

extern constexpr u8 q_N(float v);

static RHIInputElementDesc dwDecl[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32_FLOAT, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
};

CDetail::~CDetail()
{
}

void CDetail::Unload	()
{
	if (vertices) 
	{ 
		xr_free(vertices);
		vertices = nullptr;
	}

	if (indices) 
	{ 
		xr_free(indices);
		indices = nullptr;
	}

	shader.destroy();

	//LVutner: Release (per-object) IB/VB
#ifdef USE_DX11
	_RELEASE(hw_VB);
	_RELEASE(hw_IB);
	hw_Geom.destroy();

	for (u32 i = 0; i < 2; i++)
	{
		for (u32 j = 0; j < 3; j++)
		{
			_RELEASE(DetailGPUBoundBuffers[i][j].first);
			_RELEASE(DetailGPUBoundBuffers[i][j].second);
		}
	}
#endif
}

// Transfer vertices
void CDetail::transfer	(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset)
{
	CDetail::fvfVertexIn* srcIt = vertices, *srcEnd = vertices + number_vertices;
	CDetail::fvfVertexOut* dstIt = vDest;

	for (; srcIt != srcEnd; srcIt++, dstIt++)
	{
		mXform.transform_tiny(dstIt->P, srcIt->P);
		dstIt->C = C;
		dstIt->u = srcIt->u;
		dstIt->v = srcIt->v;
	}
	
	// Transfer indices (in 32bit lines)
	VERIFY(iOffset < 65535);

	u32	item = (iOffset << 16) | iOffset;
	u32	count = number_indices / 2;
	LPDWORD	sit = LPDWORD(indices);
	LPDWORD	send = sit + count;
	LPDWORD	dit = LPDWORD(iDest);

	for (; sit != send; dit++, sit++)
	{
		*dit = *sit + item;
	}

	if (number_indices & 1)
	{
		iDest[number_indices - 1] = u16(indices[number_indices - 1] + u16(iOffset));
	}
}

void CDetail::transfer	(Fmatrix& mXform, fvfVertexOut* vDest, u32 C, u16* iDest, u32 iOffset, float du, float dv)
{
	CDetail::fvfVertexIn* srcIt = vertices, * srcEnd = vertices + number_vertices;
	CDetail::fvfVertexOut* dstIt = vDest;

	for (; srcIt != srcEnd; srcIt++, dstIt++)
	{
		mXform.transform_tiny(dstIt->P, srcIt->P);
		dstIt->C = C;
		dstIt->u = srcIt->u + du;
		dstIt->v = srcIt->v + dv;
	}
	
	// Transfer indices (in 32bit lines)
	VERIFY(iOffset < 65535);

	u32	item = (iOffset << 16) | iOffset;
	u32	count = number_indices / 2;
	LPDWORD	sit = LPDWORD(indices);
	LPDWORD	send = sit + count;
	LPDWORD	dit = LPDWORD(iDest);

	for (; sit != send; dit++, sit++)
	{
		*dit = *sit + item;
	}

	if (number_indices & 1)
	{
		iDest[number_indices - 1] = u16(indices[number_indices - 1] + u16(iOffset));
	}
}

//LVutner: Create vertex and index buffers
#ifdef USE_DX11
void CDetail::LoadGeom()
{
	xr_vector<Fvector> vNormals(number_vertices, Fidentity.c);
	Fvector normal;

	for (u32 i = 0; i < number_indices; i += 3)
	{
		const auto& idx_0 = indices[i + 0];
		const auto& idx_1 = indices[i + 1];
		const auto& idx_2 = indices[i + 2];

		const auto& v_0 = vertices[idx_0].P;
		const auto& v_1 = vertices[idx_1].P;
		const auto& v_2 = vertices[idx_2].P;

		normal.mknormal(v_0, v_1, v_2);

		vNormals[idx_0].add(normal);
		vNormals[idx_1].add(normal);
		vNormals[idx_2].add(normal);
	}

	xr_vector<vertHW> pV;
	vertHW V;

	for (u32 v = 0; v < number_vertices; v++)
	{
		V.pos_frac.x = vertices[v].P.x;
		V.pos_frac.y = vertices[v].P.y;
		V.pos_frac.z = vertices[v].P.z;
		V.pos_frac.w = vertices[v].P.y / (bv_bb.max.y - bv_bb.min.y);

		auto& vNormal = vNormals[v].normalize_safe();
		V.normal = color_rgba(q_N(vNormal.x), q_N(vNormal.y), q_N(vNormal.z), 0);

		V.uv.x = vertices[v].u;
		V.uv.y = vertices[v].v;

		pV.push_back(V);
	}

	u32 size_indices = number_indices * sizeof(u16);
	R_ASSERT(RHIUtils::CreateVertexBuffer(&hw_VB, pV.data(), number_vertices * sizeof(vertHW)));
	R_ASSERT(RHIUtils::CreateIndexBuffer(&hw_IB, indices, size_indices));
	hw_Geom.create(dwDecl, std::size(dwDecl), hw_VB, hw_IB);
}
#endif

void CDetail::Load(IReader* S)
{
	// Shader
	string256 fnT, fnS;

	S->r_stringZ(fnS, sizeof(fnS));
	S->r_stringZ(fnT, sizeof(fnT));

	shader.create(fnS, fnT);

	// Params
	m_Flags.assign(S->r_u32());
	m_fMinScale = S->r_float();
	m_fMaxScale = S->r_float();
	number_vertices = S->r_u32();
	number_indices = S->r_u32();

	R_ASSERT(0 == (number_indices % 3));

	// Vertices                             
	u32 size_vertices = number_vertices * sizeof(fvfVertexIn);
	vertices = xr_alloc<CDetail::fvfVertexIn>(number_vertices);
	S->r(vertices, size_vertices);

	// Indices
	u32 size_indices = number_indices * sizeof(u16);
	indices = xr_alloc<u16>(number_indices);
	S->r(indices, size_indices);
	
	// Validate indices
#ifdef DEBUG
	for (u32 idx = 0; idx<number_indices; idx++)
	{
		R_ASSERT(indices[idx] < (u16)number_vertices);
	}
#endif

	// Calc BB & SphereRadius
	bv_bb.invalidate();

	for (u32 i = 0; i < number_vertices; i++)
	{
		bv_bb.modify(vertices[i].P);
	}

	bv_bb.getsphere(bv_sphere.P, bv_sphere.R);

	//LVutner: Create vertex and index buffers
#ifdef USE_DX11
	{
		xr_vector<Fvector> vNormals(number_vertices, Fidentity.c);
		Fvector normal;

		for (u32 i = 0; i < number_indices; i += 3)
		{
			const auto& idx_0 = indices[i + 0];
			const auto& idx_1 = indices[i + 1];
			const auto& idx_2 = indices[i + 2];

			const auto& v_0 = vertices[idx_0].P;
			const auto& v_1 = vertices[idx_1].P;
			const auto& v_2 = vertices[idx_2].P;

			normal.mknormal(v_0, v_1, v_2);

			vNormals[idx_0].add(normal);
			vNormals[idx_1].add(normal);
			vNormals[idx_2].add(normal);
		}
		
#if 0
		xr_vector<Fvector> vNormals2 = vNormals;

		for (u32 idx = 0; idx < number_vertices; ++idx)
		{
			const auto& v_0 = vertices[idx].P;

			for (u32 idx_1 = 0; idx_1 < number_vertices; ++idx_1)
			{
				const auto& v_1 = vertices[idx_1].P;

				if (idx_1 == idx)
				{
					continue;
				}

				if (v_0.distance_to_sqr(v_1) <= EPS)
				{
					vNormals2[idx].add(vNormals[idx_1]);
				}
			}
		}

		vNormals = vNormals2;
#endif

		xr_vector<vertHW> pV;
		vertHW V;

		for (u32 v = 0; v < number_vertices; v++)
		{
			V.pos_frac.x = vertices[v].P.x;
			V.pos_frac.y = vertices[v].P.y;
			V.pos_frac.z = vertices[v].P.z;
			V.pos_frac.w = vertices[v].P.y / (bv_bb.max.y - bv_bb.min.y);

			auto& vNormal = vNormals[v].normalize_safe();
			V.normal = color_rgba(q_N(vNormal.x), q_N(vNormal.y), q_N(vNormal.z), 0);

			V.uv.x = vertices[v].u;
			V.uv.y = vertices[v].v;

			pV.push_back(V);
		}

		R_ASSERT(RHIUtils::CreateVertexBuffer(&hw_VB, pV.data(), number_vertices * sizeof(vertHW)));
		R_ASSERT(RHIUtils::CreateIndexBuffer(&hw_IB, indices, size_indices));
		hw_Geom.create(dwDecl, std::size(dwDecl), hw_VB, hw_IB);
	}
#endif

#ifndef _EDITOR
	Optimize	();
#endif

#ifdef USE_DX11
	LoadGeom();
#endif
}

#ifndef _EDITOR
#include "xrStripify.h"

void CDetail::Optimize	()
{
	xr_vector<u16>		vec_indices,	vec_permute;
	const int			cache			= 24;

	// Stripify
	vec_indices.assign	(indices,indices+number_indices);
	vec_permute.resize	(number_vertices);
	int vt_old			= xrSimulate(vec_indices,cache);
	xrStripify			(vec_indices,vec_permute,cache,0);
	int vt_new			= xrSimulate(vec_indices,cache);
	if (vt_new<vt_old)	
	{
		// Msg					("* DM: %d verts, %d indices, VT: %d/%d",number_vertices,number_indices,vt_old,vt_new);

		// Copy faces
		CopyMemory			(indices,&*vec_indices.begin(),vec_indices.size()*sizeof(u16));

		// Permute vertices
		xr_vector<fvfVertexIn>	verts;
		verts.assign			(vertices,vertices+number_vertices);
		for(u32 i=0; i<verts.size(); i++)
			vertices[i]=verts[vec_permute[i]];
	}
}
#endif
