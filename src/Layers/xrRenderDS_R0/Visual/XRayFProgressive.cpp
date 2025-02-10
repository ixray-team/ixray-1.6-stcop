#include "stdafx.h"
#include "XRayFProgressive.h"


XRayFProgressive::XRayFProgressive()
{
    xSWI = 0;
}

XRayFProgressive::~XRayFProgressive()
{

}
void 		XRayFProgressive::Release()
{
    xr_free(nSWI.sw);
    if (xSWI) {
        xr_free(xSWI->sw);
        xr_delete(xSWI);
        xSWI = 0;

    }
}
// bool XRayFProgressive::Render(float LOD, EShaderElement SEType, XRayObjectRender& Item)
// {
//     GRenderInterface.UpdateDescriptorHeap(Shader.E[0][SEType]);
//     if (!Shader.E[0][SEType].Set(Item, FVF)) { return false; }
//
//     Item.UseUniform[XRayUniformAllocator::UT_Transformation] = true;
//     Item.Visual = this;
//
//
//     int lod_id = last_lod;
//     if (LOD >= 0.f) 
//     {
//         clamp(LOD, 0.f, 1.f);
//         lod_id = iFloor((1.f - LOD) * float(nSWI.count - 1) + 0.5f);
//         last_lod = lod_id;
//     }
//     VERIFY(lod_id >= 0 && lod_id<int(nSWI.count));
//
//
//     Item.UseMeshPipeline = Shader.MeshPipeline;
//
//     Item.Visual = this;
//     if (Shader.MeshPipeline)
//     {
//         Item.MeshPipelineResource.VertexBufferAsStructured = VertexBufferAsStructured;
//         Item.MeshPipelineResource.MeshBuffer = MeshBuffer;
//         Item.MeshPipelineResource.CountMeshlet = MeshletSubsets[lod_id].Count;
//         Item.MeshPipelineResource.OffsetMeshlet = MeshletSubsets[lod_id].Offset *sizeof(XRayMeshlet);
//         Item.MeshPipelineResource.OffsetUniqueVertexIndices = OffsetUniqueVertexIndices;
//         Item.MeshPipelineResource.OffsetPrimitiveIndices = OffsetPrimitiveIndices;
//         return Item.MeshPipelineResource.CountMeshlet;
//     }
//     else
//     {
//         FSlideWindow& SW = nSWI.sw[lod_id];
//         Item.GraphicsPipelineResource.VertexBuffer = VertexBuffer;
//         Item.GraphicsPipelineResource.IndexBuffer = IndexBuffer;
//         Item.GraphicsPipelineResource.CountIndex = SW.num_tris * 3;
//         Item.GraphicsPipelineResource.OffsetIndex = OffsetIndex + SW.offset;
//         Item.GraphicsPipelineResource.OffsetVertex = OffsetVertex;
//         return Item.GraphicsPipelineResource.CountIndex;
//     }
// }
void XRayFProgressive::Load(const char* N, IReader* data, u32 dwFlags)
{
    XRayFVisual::Load(N, data, dwFlags| VLOAD_SWI);

    // normal SWI
   
}
#define PCOPY(a)	a = pFrom->a
void XRayFProgressive::Copy(XRayRenderVisual* from)
{
    XRayFVisual::Copy(from);
    XRayFProgressive* pFrom = (XRayFProgressive*)from;
    PCOPY(nSWI);
    PCOPY(xSWI);
}
