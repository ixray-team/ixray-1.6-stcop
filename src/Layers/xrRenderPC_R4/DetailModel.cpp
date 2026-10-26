#include "stdafx.h"
#include "DetailManager.h"
#include <array>

struct vertHW
{
	Fvector4 pos_frac;
	Fvector2 uv;
	u32 normal = 0;
	Fvector2 WindShape;
};

// Local copy of the quantizer from XRayFSkinned.cpp (dedicated server renderer,
// not linked here): constexpr bodies emit no symbol in Release builds.
static constexpr u8 q_N(float v)
{
	int _v = clampr(iFloor((v + 1.f) * 127.5f), 0, 255);
	return u8(_v);
}

static RHIInputElementDesc dwDecl[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32_FLOAT, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 1, ERHI_FORMAT::R32G32_FLOAT, 0, 28, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
};

CDetail::CDetail()
{
	vertices = nullptr;
	number_vertices = 0;
	indices = nullptr;
	number_indices = 0;
	m_fMinScale = 0.5f;
	m_fMaxScale = 2.f;
	hw_VB = nullptr;
	hw_IB = nullptr;
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

	//LVutner: Release (per-object) IB/VB
	_RELEASE(hw_VB);
	_RELEASE(hw_IB);
	hw_Geom.destroy();
	WindLowGeometry.destroy();
	_RELEASE(WindLowVertexBuffer);
	_RELEASE(WindLowIndexBuffer);
	WindLowVertexCount = WindLowIndexCount = 0;
	ResetInstanceBuffers();
}

void CDetail::ResetInstanceBuffers()
{
	for (InstancePacket* Packet : {&RenderInstances, &PreparedInstances})
	{
		for (u32 Group = 0; Group < 4; ++Group)
		{
			_RELEASE(Packet->Buffers[Group].second);
			_RELEASE(Packet->Buffers[Group].first);
			Packet->Counts[Group] = 0;
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

//LVutner: Create vertex and index buffers
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
		V.pos_frac.w = (vertices[v].P.y - bv_bb.min.y) / std::max(bv_bb.max.y - bv_bb.min.y, EPS);
		V.WindShape.set(bv_bb.min.y, std::max(bv_bb.max.y - bv_bb.min.y, EPS));

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

// Split card triangles into horizontal bands while preserving rest positions and UVs.
// The 20% boundary is explicit; shared card vertices are retained for the GPU cache.
static void PrepareWindGeometry(CDetail& Model)
{
	if (Model.m_Flags.is(DO_NO_WAVING) || Model.number_vertices < 8 || Model.number_vertices > 2048 || !FS.exist("$game_config$", "mod_system_wind_presets.ltx"))
	{
		return;
	}
	using Vertex = CDetail::fvfVertexIn;
	const float Height = Model.bv_bb.max.y - Model.bv_bb.min.y;
	if (Height < EPS)
	{
		return;
	}
	xr_vector<Vertex> Output;
	xr_vector<u16> Indices;
	xr_map<std::array<s32, 8>, u16> Welded;
	auto Clip = [](const xr_vector<Vertex>& Input, float Y, bool Above)
	{
		xr_vector<Vertex> Result;
		if (Input.empty())
		{
			return Result;
		}
		Vertex Previous = Input.back();
		bool PreviousInside = Above ? Previous.P.y >= Y : Previous.P.y <= Y;
		for (const Vertex& Current : Input)
		{
			const bool Inside = Above ? Current.P.y >= Y : Current.P.y <= Y;
			if (Inside != PreviousInside)
			{
				const float T = (Y - Previous.P.y) / (Current.P.y - Previous.P.y);
				Vertex Intersection;
				Intersection.P.lerp(Previous.P, Current.P, T);
				Intersection.P.y = Y;
				Intersection.u = Previous.u + (Current.u - Previous.u) * T;
				Intersection.v = Previous.v + (Current.v - Previous.v) * T;
				Result.push_back(Intersection);
			}
			if (Inside)
			{
				Result.push_back(Current);
			}
			Previous = Current;
			PreviousInside = Inside;
		}
		return Result;
	};
	constexpr float Bands[] = {0, .2f, .2f + .8f / 3, .2f + .8f * 2 / 3, 1};
	const float* Cuts = Bands;
	const int BandCount = int(std::size(Bands));
	for (u32 Index = 0; Index < Model.number_indices; Index += 3)
	{
		const xr_vector<Vertex> Triangle = {Model.vertices[Model.indices[Index]], Model.vertices[Model.indices[Index + 1]], Model.vertices[Model.indices[Index + 2]]};
		Fvector OriginalA, OriginalB, PlaneNormal;
		OriginalA.sub(Triangle[1].P, Triangle[0].P);
		OriginalB.sub(Triangle[2].P, Triangle[0].P);
		PlaneNormal.crossproduct(OriginalA, OriginalB).normalize_safe();
		for (int Band = 0; Band < BandCount - 1; ++Band)
		{
			auto Polygon = Clip(Triangle, Model.bv_bb.min.y + Height * Cuts[Band], true);
			Polygon = Clip(Polygon, Model.bv_bb.min.y + Height * Cuts[Band + 1], false);
			for (size_t Corner = 1; Corner + 1 < Polygon.size(); ++Corner)
			{
				Fvector EdgeA, EdgeB, Cross;
				EdgeA.sub(Polygon[Corner].P, Polygon[0].P);
				EdgeB.sub(Polygon[Corner + 1].P, Polygon[0].P);
				Cross.crossproduct(EdgeA, EdgeB);
				if (Cross.square_magnitude() < 1e-14f)
				{
					continue;
				}
				u16 Face[3];
				u32 FaceCorner = 0;
				for (size_t C : {size_t(0), Corner, Corner + 1})
				{
					const auto& V = Polygon[C];
					// Quantization only joins numerical duplicates; plane normals retain card seams.
					const std::array<s32, 8> Key = {
						s32(std::lround(V.P.x * 100000)), s32(std::lround(V.P.y * 100000)), s32(std::lround(V.P.z * 100000)),
						s32(std::lround(V.u * 100000)), s32(std::lround(V.v * 100000)),
						s32(std::lround(PlaneNormal.x * 10000)), s32(std::lround(PlaneNormal.y * 10000)), s32(std::lround(PlaneNormal.z * 10000))};
					auto Found = Welded.find(Key);
					if (Found == Welded.end())
					{
						if (Output.size() >= 65535)
						{
							return;
						}
						Found = Welded.emplace(Key, u16(Output.size())).first;
						Output.push_back(V);
					}
					Face[FaceCorner++] = Found->second;
				}
				if (Face[0] != Face[1] && Face[1] != Face[2] && Face[0] != Face[2])
				{
					Indices.insert(Indices.end(), std::begin(Face), std::end(Face));
				}
			}
		}
	}
	if (Output.empty())
	{
		return;
	}
	xr_free(Model.vertices);
	xr_free(Model.indices);
	Model.number_vertices = u32(Output.size());
	Model.number_indices = u32(Indices.size());
	Model.vertices = xr_alloc<Vertex>(Output.size());
	Model.indices = xr_alloc<u16>(Indices.size());
	std::copy(Output.begin(), Output.end(), Model.vertices);
	std::copy(Indices.begin(), Indices.end(), Model.indices);
}

void CDetail::Load(IReader* S)
{
	// Shader
	string256 fnT, fnS;

	S->r_stringZ(fnS, sizeof(fnS));
	S->r_stringZ(fnT, sizeof(fnT));
	TextureName = fnT;

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

	if (!m_Flags.is(DO_NO_WAVING) && number_vertices >= 8 && number_vertices <= 2048 && FS.exist("$game_config$", "mod_system_wind_presets.ltx"))
	{
		// Beyond 50 m retain the original mesh, without adding wind cuts.
		CDetail Coarse;
		Coarse.m_Flags = m_Flags;
		Coarse.bv_bb = bv_bb;
		Coarse.number_vertices = number_vertices;
		Coarse.number_indices = number_indices;
		Coarse.vertices = xr_alloc<fvfVertexIn>(number_vertices);
		Coarse.indices = xr_alloc<u16>(number_indices);
		std::copy(vertices, vertices + number_vertices, Coarse.vertices);
		std::copy(indices, indices + number_indices, Coarse.indices);
		if (!Device.IsEditorMode())
		{
			Coarse.Optimize();
		}
		Coarse.LoadGeom();
		WindLowGeometry = Coarse.hw_Geom;
		WindLowVertexBuffer = Coarse.hw_VB;
		WindLowIndexBuffer = Coarse.hw_IB;
		WindLowVertexCount = Coarse.number_vertices;
		WindLowIndexCount = Coarse.number_indices;
		Coarse.hw_VB = Coarse.hw_IB = nullptr;
		xr_free(Coarse.vertices);
		xr_free(Coarse.indices);
	}
	PrepareWindGeometry(*this);

	if (!Device.IsEditorMode())
	{
		Optimize	();
	}

	LoadGeom();
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

	Msg("* CDetail::LoadFromDM: loaded '%s' (%d verts, %d indices, distant %u verts, %u indices)", fn, number_vertices, number_indices, WindLowVertexCount, WindLowIndexCount);
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
