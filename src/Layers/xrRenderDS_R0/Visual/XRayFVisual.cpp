#include "stdafx.h"
#define VLOAD_NOVERTICES 1<<0
#include "XRayFVisual.h"

XRayFVisual::XRayFVisual()
{
	CountIndex = 0;
	OffsetIndex = 0;
	CountVertex = 0;
	OffsetVertex = 0;
	FVF = 0;
}

XRayFVisual::~XRayFVisual()
{
}

void XRayFVisual::Load(const char* N, IReader* data, u32 dwFlags)
{
	XRayRenderVisual::Load(N, data, dwFlags);
	bool				Loaded = false;
	if (dwFlags & VLOAD_SWI)
	{
		destructor<IReader> lods(data->open_chunk(OGF_SWIDATA));
		nSWI.reserved[0] = lods().r_u32();	// reserved 16 bytes
		nSWI.reserved[1] = lods().r_u32();
		nSWI.reserved[2] = lods().r_u32();
		nSWI.reserved[3] = lods().r_u32();
		nSWI.count = lods().r_u32();
		VERIFY(NULL == nSWI.sw);
		nSWI.sw = xr_alloc<FSlideWindow>(nSWI.count);
		lods().r(nSWI.sw, nSWI.count * sizeof(FSlideWindow));
	}
	if (data->find_chunk(OGF_GCONTAINER))
	{
		
		{
			// R_ASSERT(MeshBuffer.empty());
			// R_ASSERT(VertexBufferAsStructured.empty());
			Loaded = true;
			u32 ID = data->r_u32();
			OffsetVertex = data->r_u32();
			CountVertex = data->r_u32();

			// R_ASSERT(VertexBuffer.empty());
			//
			// VertexBuffer = GRenderInterface.GetVertexBuffer(ID);
			// FVF = GRenderInterface.GetVertexState(ID);


			ID = data->r_u32();
			OffsetIndex = data->r_u32();
			CountIndex = data->r_u32();

			// R_ASSERT(IndexBuffer.empty());
			//
			// IndexBuffer = GRenderInterface.GetIndexBuffer(ID);
		}
		
	}

	if (!Loaded && (dwFlags & VLOAD_NOVERTICES) == 0)
	{
		if (data->find_chunk(OGF_VCONTAINER))
		{
			R_ASSERT2(0, "pls notify andy about this.");
		}
		else
		{
			R_ASSERT(data->find_chunk(OGF_VERTICES));
			FVF = data->r_u32();

			//FIXME:������� ���������!!!
			CountVertex = data->r_u32();
			// R_ASSERT(VertexBuffer.empty());
			// VertexBuffer = BearRenderInterface::CreateVertexBuffer();
			// VertexBuffer->Create(GResourcesManager->GetStride(FVF), CountVertex, true);
			// memcpy(VertexBuffer->Lock(), data->pointer(), CountVertex * GResourcesManager->GetStride(FVF));
			// VertexBuffer->Unlock();
			//R_ASSERT(!MeshPipeline);
		}
	}

	// indices
	if (!Loaded)
	{
		if (data->find_chunk(OGF_ICONTAINER))
		{
			R_ASSERT2(0, "pls notify andy about this.");
		}
		else
		{
			R_ASSERT(data->find_chunk(OGF_INDICES));
			CountIndex = data->r_u32();
			//FIXME:������� ���������!!!
		//	R_ASSERT(IndexBuffer.empty());
			// IndexBuffer = BearRenderInterface::CreateIndexBuffer();
			// IndexBuffer->Create(CountIndex, true);
			// u32* dst = IndexBuffer->Lock();
			// u16* src = (u16*)data->pointer();
			// for (size_t i = 0; i < CountIndex; i++)
			// {
			// 	dst[i] = src[i];
			// }
			// IndexBuffer->Unlock();
			//R_ASSERT(!MeshPipeline);
		}

	}
}
#define PCOPY(a)	a = pFrom->a
void XRayFVisual::Copy(XRayRenderVisual* from)
{
	XRayRenderVisual::Copy(from);

	XRayFVisual* pFrom = dynamic_cast<XRayFVisual*> (from);

	PCOPY(FVF);
	//if (Shader.MeshPipeline)
	//{
	//	// PCOPY(VertexBufferAsStructured);
	//	// PCOPY(MeshBuffer);
	//	// PCOPY(MeshletSubsets);
	//	PCOPY(CountMeshlet);
	//	PCOPY(OffsetUniqueVertexIndices);
	//	PCOPY(OffsetPrimitiveIndices);
	//}
	//else
	//{
	////	PCOPY(VertexBuffer);
	//	PCOPY(OffsetVertex);
	//	PCOPY(CountVertex);
	//
	////	PCOPY(IndexBuffer);
	//	PCOPY(OffsetIndex);
	//	PCOPY(CountIndex);
	//}

}

//
// bool XRayFVisual::Render(float LOD, EShaderElement SEType, XRayObjectRender& Item)
// {
// 	GRenderInterface.UpdateDescriptorHeap(Shader.E[0][SEType]);
// 	if (!Shader.E[0][SEType].Set(Item, FVF)) { return false; }
// 	Item.UseUniform[XRayUniformAllocator::UT_Transformation] = true;
// 	Item.UseMeshPipeline = Shader.MeshPipeline;
//
// 	Item.Visual = this;
// 	if (Shader.MeshPipeline)
// 	{
// 		Item.MeshPipelineResource.VertexBufferAsStructured = VertexBufferAsStructured;
// 		Item.MeshPipelineResource.MeshBuffer = MeshBuffer;
// 		Item.MeshPipelineResource.CountMeshlet = CountMeshlet;
// 		Item.MeshPipelineResource.OffsetMeshlet = 0;
// 		Item.MeshPipelineResource.OffsetUniqueVertexIndices = OffsetUniqueVertexIndices;
// 		Item.MeshPipelineResource.OffsetPrimitiveIndices = OffsetPrimitiveIndices;
//
// 		return Item.MeshPipelineResource.CountMeshlet;
// 	}
// 	else
// 	{
// 		Item.GraphicsPipelineResource.VertexBuffer = VertexBuffer;
// 		Item.GraphicsPipelineResource.IndexBuffer = IndexBuffer;
// 		Item.GraphicsPipelineResource.CountIndex = CountIndex;
// 		Item.GraphicsPipelineResource.OffsetIndex = OffsetIndex;
// 		Item.GraphicsPipelineResource.OffsetVertex = OffsetVertex;
//
// 		return Item.GraphicsPipelineResource.CountIndex;
// 	}
// }
