#include "stdafx.h"
#include "DetailManager.h"

CDetail::CDetail()
{
	vertices = nullptr;
	number_vertices = 0;
	indices = nullptr;
	number_indices = 0;
	m_fMinScale = 0.5f;
	m_fMaxScale = 2.f;
	for (u32 i = 0; i < 2; i++)
		for (u32 j = 0; j < 3; j++)
			DetailGPUBoundBuffers[i][j] = {nullptr, nullptr};
}

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

	RImplementation.Geometry.Free(Alloc);
	hw_Geom.destroy();

	for (u32 i = 0; i < 2; i++)
	{
		for (u32 j = 0; j < 3; j++)
		{
			_RELEASE(DetailGPUBoundBuffers[i][j].first);
			_RELEASE(DetailGPUBoundBuffers[i][j].second);
		}
	}
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

void CDetail::LoadGeom()
{
	RImplementation.Geometry.Free(Alloc);
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

	xr_vector<Fvector2> UV(number_vertices);
	for (u32 v = 0; v < number_vertices; v++)
	{
		UV[v].set(vertices[v].u, vertices[v].v);
	}
	const Fvector2 Offset = VertexPacking::FitUV(UV.data(), number_vertices, "detail model");
	const float Height = bv_bb.max.y - bv_bb.min.y;
	const float Scale = Height > EPS_L ? 2048.0f / Height : 0.0f;

	xr_vector<PackedVertex> Records(number_vertices);
	for (u32 v = 0; v < number_vertices; v++)
	{
		const s16 Frac = s16(std::lrint(std::clamp(vertices[v].P.y * Scale, -32768.0f, 32767.0f)));
		Records[v] = { vertices[v].P, VertexPacking::EncodeOct(vNormals[v].normalize_safe()), 0, 0, 0, u16(Frac) };
		VertexPacking::EncodeFitUV(Records[v], UV[v], Offset);
	}

	Alloc = RImplementation.Geometry.Alloc(number_vertices, number_indices);
	RImplementation.Geometry.Upload(Alloc, 0, Records.data(), number_vertices);
	RImplementation.Geometry.UploadIndices(Alloc, 0, indices, number_indices);
	hw_Geom.create(EGeomSource::Static, Alloc.Page);
}

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

	if (!Device.IsEditorMode())
	{
		Optimize	();
	}
}

bool CDetail::LoadFromDM(const char* dm_path)
{
	string_path fn;
	if (!FS.exist(fn, "$game_meshes$", dm_path))
	{
		if (!FS.exist(dm_path))
		{
			Msg("! CDetail::LoadFromDM: cannot find '%s'", dm_path);
			return false;
		}
		xr_strcpy(fn, sizeof(fn), dm_path);
	}

	IReader* F = FS.r_open(fn);
	if (!F)
	{
		Msg("! CDetail::LoadFromDM: failed to open '%s'", fn);
		return false;
	}

	Load(F);
	FS.r_close(F);
	LoadGeom();

	Msg("* CDetail::LoadFromDM: loaded '%s' (%d verts, %d indices)", fn, number_vertices, number_indices);
	return true;
}

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
