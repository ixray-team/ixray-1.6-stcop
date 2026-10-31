#include "stdafx.h"
#include "../../xrEngine/Fmesh.h"
#include "FVisual.h"
#include "../../xrEngine/EngineAPI.h"

Fvisual::Fvisual() :
	dxRender_Visual()
{
	m_fast = nullptr;
}

Fvisual::~Fvisual()
{
	xr_delete(m_fast);
}

void Fvisual::Release()
{
	dxRender_Visual::Release();
}

void Fvisual::Load		(const char* N, IReader *data, u32 dwFlags)
{
	PROF_EVENT(__FUNCTION__);
	dxRender_Visual::Load		(N,data,dwFlags);

	dwPrimitives				= 0;

	if (data->find_chunk(OGF_GCONTAINER))
	{
		const u32 VertexBuffer = data->r_u32();
		const u32 VertexBase = data->r_u32();
		vCount = data->r_u32();
		const u32 IndexBuffer = data->r_u32();
		const u32 IndexBase = data->r_u32();
		iCount = data->r_u32();
		dwPrimitives = iCount / 3;
		SetPulled(RImplementation.UploadLevelMesh(false, VertexBuffer, VertexBase, vCount, IndexBuffer, IndexBase, iCount));

		if (LightingModeIsDynamicRender() && data->find_chunk(OGF_FASTPATH))
		{
			destructor<IReader>	geomdef(data->open_chunk(OGF_FASTPATH));
			destructor<IReader>	def(geomdef().open_chunk(OGF_GCONTAINER));

			m_fast = new IRender_Mesh();
			const u32 FastVertexBuffer = def().r_u32();
			const u32 FastVertexBase = def().r_u32();
			m_fast->vCount = def().r_u32();
			const u32 FastIndexBuffer = def().r_u32();
			const u32 FastIndexBase = def().r_u32();
			m_fast->iCount = def().r_u32();
			m_fast->dwPrimitives = m_fast->iCount / 3;
			m_fast->SetPulled(RImplementation.UploadLevelMesh(true, FastVertexBuffer, FastVertexBase, m_fast->vCount, FastIndexBuffer, FastIndexBase, m_fast->iCount));
		}
		return;
	}

	const auto Fail = [N](const char* Reason)
	{
		Msg("! Model '%s': %s", N, Reason);
		Debug.do_exit(std::string("Model '") + N + "': " + Reason);
	};

	const u64 IndexBytes = u64(data->find_chunk(OGF_INDICES));
	if (IndexBytes < 4)
	{
		Fail("OGF_INDICES chunk is missing");
	}
	iBase = 0;
	iCount = data->r_u32();
	dwPrimitives = iCount / 3;
	if (4 + u64(iCount) * sizeof(u16) > IndexBytes)
	{
		Fail("OGF_INDICES chunk is shorter than its index count");
	}
	if (dwFlags & VLOAD_NOVERTICES)
	{
		return;
	}
	const u16* Indices = (const u16*)data->pointer();

	const u64 VertexBytes = u64(data->find_chunk(OGF_VERTICES));
	if (VertexBytes < 8)
	{
		Fail("OGF_VERTICES chunk is missing");
	}
	const u32 Fvf = data->r_u32();
	vCount = data->r_u32();
	constexpr u32 Required = D3DFVF_XYZ | D3DFVF_TEX1;
	if ((Fvf & ~(Required | D3DFVF_NORMAL | D3DFVF_DIFFUSE | D3DFVF_SPECULAR)) != 0 || (Fvf & Required) != Required)
	{
		Fail("unsupported vertex format, expected XYZ, optional NORMAL/DIFFUSE/SPECULAR and one float2 TEXCOORD");
	}
	const u32 Stride = 20 + (Fvf & D3DFVF_NORMAL ? 12 : 0) + (Fvf & D3DFVF_DIFFUSE ? 4 : 0) + (Fvf & D3DFVF_SPECULAR ? 4 : 0);
	if (8 + u64(vCount) * Stride > VertexBytes)
	{
		Fail("OGF_VERTICES chunk is shorter than its vertex count");
	}

	const u8* Source = (const u8*)data->pointer();
	xr_vector<Fvector> Frames(size_t(vCount) * 4);
	xr_vector<Fvector2> UV(vCount);
	Fvector* Positions = Frames.data();
	Fvector* Normals = Positions + vCount;
	Fvector* Tangents = Normals + vCount;
	Fvector* Binormals = Tangents + vCount;
	for (u32 Index = 0; Index < vCount; ++Index)
	{
		const u8* Vertex = Source + size_t(Index) * Stride;
		Positions[Index] = *(const Fvector*)Vertex;
		Normals[Index] = Fvf & D3DFVF_NORMAL ? *(const Fvector*)(Vertex + 12) : Fvector{ 0.0f, 0.0f, 0.0f };
		UV[Index] = *(const Fvector2*)(Vertex + Stride - 8);
	}
	VertexPacking::GenerateTangents(Positions, UV.data(), vCount, Indices, iCount, Tangents, Binormals);
	const Fvector2 Offset = VertexPacking::FitUV(UV.data(), vCount, N);

	xr_vector<PackedVertex> Records(vCount);
	for (u32 Index = 0; Index < vCount; ++Index)
	{
		PackedVertex& Vertex = Records[Index];
		Vertex = { Positions[Index], 0, 0, 0, 0, 0 };
		VertexPacking::EncodeFrame(Vertex, Normals[Index], Tangents[Index], Binormals[Index]);
		VertexPacking::EncodeFitUV(Vertex, UV[Index], Offset);
	}
	const GeometryAlloc Block = RImplementation.Geometry.Alloc(vCount, iCount);
	RImplementation.Geometry.Upload(Block, 0, Records.data(), vCount);
	RImplementation.Geometry.UploadIndices(Block, 0, Indices, iCount);
	SetPulled(Block);
}

static SVS* incompatible_vertex_shader(const ref_shader& sh, u32 element, const ref_geom& geom)
{
	ShaderElement* E = sh ? &*sh->E[element] : nullptr;
	if (!E || E->passes.empty() || !E->passes[0]->vs || !E->passes[0]->vs->signature || !geom)
		return nullptr;

	SVS* vs = &*E->passes[0]->vs;
	RHIBlob* signature = vs->signature->signature;
	const xr_vector<RHIInputElementDesc>& dcl = geom->dcl->dx10_dcl_code;
	RHIObject* layout = nullptr;
	if (FAILED(GRHI->CreateInputLayout(dcl.data(), dcl.size(), signature->GetBufferPointer(), signature->GetBufferSize(), &layout)))
		return vs;

	_RELEASE(layout);
	return nullptr;
}

bool Fvisual::fast_usable()
{
	if (m_fast_usable < 0)
		m_fast_usable = incompatible_vertex_shader(shader, ShaderVariant::Shadow, m_fast->rm_geom) ? 0 : 1;

	return m_fast_usable != 0;
}

void Fvisual::check_layout()
{
	if (m_layout_checked)
		return;

	m_layout_checked = true;
	for (u32 element : { u32(ShaderVariant::NormalHQ), u32(ShaderVariant::NormalLQ), u32(ShaderVariant::Shadow) })
	{
		if (SVS* vs = incompatible_vertex_shader(shader, element, rm_geom))
		{
			Msg("! visual [%s] shader [%s] textures [%s] element [%u]: geometry is incompatible with VS [%s]", dbg_name.c_str(), shader->src_shader.c_str(), shader->src_textures.c_str(), element, vs->cName.c_str());
			for (const RHIInputElementDesc& e : rm_geom->dcl->dx10_dcl_code)
				Msg("!   decl %s%u fmt=%u off=%u", e.SemanticName, e.SemanticIndex, (u32)e.Format, e.AlignedByteOffset);
		}
	}
}

void Fvisual::Render(GfxCmdList& Cmd, float)
{
	if (m_fast && RImplementation.phase == CRender::PHASE_SMAP && fast_usable())
	{
		Cmd.SetGeometry(m_fast->rm_geom);
		Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, m_fast->dwPrimitives * 3, m_fast->iBase, m_fast->vBase);
		return;
	}
	Cmd.SetGeometry(rm_geom);
	Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, dwPrimitives * 3, iBase, vBase);
}

#define PCOPY(a)	a = pFrom->a
void Fvisual::Copy(dxRender_Visual* pSrc)
{
	dxRender_Visual::Copy(pSrc);

	Fvisual* pFrom = dynamic_cast<Fvisual*> (pSrc);

	PCOPY(rm_geom);

	PCOPY(p_rm_Vertices); if (p_rm_Vertices) p_rm_Vertices->AddRef();
	PCOPY(vBase);
	PCOPY(vCount);

	PCOPY(iBase);
	PCOPY(iCount);
	PCOPY(dwPrimitives);
	PCOPY(Alloc);
	RImplementation.Geometry.Retain(Alloc);

	PCOPY(m_fast);
}
