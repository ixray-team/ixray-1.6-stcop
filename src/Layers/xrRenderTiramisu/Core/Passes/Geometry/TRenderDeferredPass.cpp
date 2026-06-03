#include "TRenderDeferredPass.h"

#include "Legacy/Scene/TLegacyRenderGraph.h"
#include "Legacy/Scene/TLegacyScene.h"
#include "Legacy/Visual/XRayRenderVisual.h"
#include "Resources/Materials/Proxy/TMaterialRenderProxy.h"
#include "Scene/TRenderScene.h"


TRenderDeferredPass::TRenderDeferredPass()
{
	
}

TRenderDeferredPass::~TRenderDeferredPass()
{
}

void TRenderDeferredPass::Render(nri::CommandBuffer& CurrentCommandBuffer)
{
    GRenderDevice.CoreInterface.CmdBeginAnnotation(CurrentCommandBuffer,"DeferredPass",nri::BGRA_UNUSED);
	
   xr_vector<TPrimitiveSceneProxy*>& RenderSceneProxies = GRenderResourcesManager->RenderScene->RenderSceneProxies;
    for (TPrimitiveSceneProxy*& SceneProxy :RenderSceneProxies)
    {
    	for (uint32_t  i = 0; i < SceneProxy->GetNumMeshBatches();i++)
    	{
    		FRenderMeshBath MeshBath;
    		SceneProxy->GetMeshBath(i,MeshBath);

		    auto Pipeline = MeshBath.Material->GetPipelines().find(MeshBath.VertexType);
    		GRenderDevice.CoreInterface.CmdSetPipeline(CurrentCommandBuffer,*Pipeline->second);
			u32 BaseInstance = MeshBath.Material->GetTexture()->GetOrCreateHeapID();
    		
    		GRenderDevice.CoreInterface.CmdSetIndexBuffer(CurrentCommandBuffer,*MeshBath.IndexBuffer.IndexBuffer, MeshBath.IndexBuffer.Offset, MeshBath.IndexBuffer.IndexType);
    		
    		nri::VertexBufferDesc VertexBufferDescription = {};
    		VertexBufferDescription.buffer = MeshBath.VertexBuffer.VertexBuffer;
    		VertexBufferDescription.offset = MeshBath.VertexBuffer.Offset;
    		VertexBufferDescription.stride = MeshBath.VertexBuffer.Stride;
    		GRenderDevice.CoreInterface.CmdSetVertexBuffers(CurrentCommandBuffer, 0, &VertexBufferDescription, 1);

    		for (FRenderMeshBathElement& Element :MeshBath.Elements)
    		{
    			GRenderDevice.CoreInterface.CmdDrawIndexed(CurrentCommandBuffer, {Element.CountIndex , 1, Element.OffsetIndex, static_cast<int32_t>(Element.OffsetVertex),BaseInstance});
    		}
    	}
    
	   
    }
    GRenderDevice.CoreInterface.CmdEndAnnotation(CurrentCommandBuffer);
}
