#pragma once
#include "XRayRenderDescriptorHeapAllocator.h"

class TRenderResourcesFlusher;
class TRenderLegacyScene;
class XRayGlobalShadersManager;
class XRayShaderDefinesManager;

class XRayRenderResourcesManager
{
public:
                                            XRayRenderResourcesManager  ();
                                            ~XRayRenderResourcesManager ();
        void                                Initialize                  ();
        bool                                IsCookedMode                ();
        void                                FlushNextFrame              (); 
        
    
    nri::DescriptorPool*                    GlobalDescriptorPool    = nullptr;
    nri::PipelineLayout*                    GlobalPipelineLayout    = nullptr;
    nri::DescriptorSet*                     SamplerDescriptorSet    = nullptr;
    nri::DescriptorSet*                     ResourcesDescriptorSet  = nullptr;
    
    XRayShaderDefinesManager*               ShaderDefinesManager = nullptr;
    XRayGlobalShadersManager*               GlobalShadersManager = nullptr;
    XRayRenderDescriptorHeapAllocator*      DescriptorHeapAllocator = nullptr;
    XRayTexturesManager*                    TexturesManager = nullptr;
    TRenderLegacyScene*                     LegacyScene = nullptr;
    TRenderResourcesFlusher*                ResourcesFlusher = nullptr;
    
    nri::Buffer*                            QuadGeometryBuffer = nullptr;
    uint64_t                                QuadGeometryOffset = 0; 
    XRayTexture2D*                          BlackTexture = nullptr;
    XRayTexture2D*                          WhiteTexture = nullptr;
    
private:
        void                                CreateSamplers          ();
        void                                CreateQuadBuffer        ();
    
    nri::Descriptor*                        LinearSampler = nullptr;
    
};
extern XRayRenderResourcesManager* GRenderResourcesManager;