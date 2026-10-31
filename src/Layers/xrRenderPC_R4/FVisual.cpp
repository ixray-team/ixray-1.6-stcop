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
	dxRender_Visual::Load		(N,data,dwFlags);

	xr_vector<RHIInputElementDesc> dcl;
	RHIInputElementDesc*	vFormat	= nullptr;
	size_t FormatSize = 0;
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

	if ((dwFlags & VLOAD_NOVERTICES) == 0)
	{
		bool FoundedChunk = data->find_chunk(OGF_VERTICES);
		R_ASSERT2(FoundedChunk, "Not found chunk OGF_VERTICES");

		vBase = 0;
		u32 fvf = data->r_u32();
		R_ASSERT(RHIUtils::Shader::CreateInputLayoutFromFVF(fvf, dcl));
		vFormat = dcl.data();
		FormatSize = dcl.size();

		vCount = data->r_u32();
		u32 vStride = (u32)RHIUtils::Shader::ComputeVertexStride(dcl);

		VERIFY(nullptr == p_rm_Vertices);
		R_ASSERT(RHIUtils::CreateVertexBuffer(&p_rm_Vertices, data->pointer(), vCount * vStride));
	}

	{
		bool FoundedChunk = data->find_chunk(OGF_INDICES);
		R_ASSERT2(FoundedChunk, "Not found chunk OGF_INDICES");

		iBase				= 0;
		iCount				= data->r_u32();
		dwPrimitives		= iCount/3;

		VERIFY(nullptr == p_rm_Indices);
		R_ASSERT(RHIUtils::CreateIndexBuffer(&p_rm_Indices, data->pointer(), iCount * 2, false, false));
	}

	if (dwFlags&VLOAD_NOVERTICES)	
		return;

	rm_geom.create(vFormat, FormatSize, p_rm_Vertices,p_rm_Indices);
}

void Fvisual::Render(GfxCmdList& Cmd, float)
{
	if (m_fast && RImplementation.phase == CRender::PHASE_SMAP)
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

	PCOPY(p_rm_Indices); if (p_rm_Indices) p_rm_Indices->AddRef();
	PCOPY(iBase);
	PCOPY(iCount);
	PCOPY(dwPrimitives);

	PCOPY(m_fast);
}