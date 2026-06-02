#pragma once
#include "TRenderDescriptorHeapAllocator.h"

class TRenderResourcesFlusher;
class TLegacyScene;
class TGlobalShadersManager;
class TShaderDefinesManager;

class TRenderResourcesManager
{
public:
                                            TRenderResourcesManager  ();
                                            ~TRenderResourcesManager ();
        void                                Initialize                  ();
        bool                                IsCookedMode                ();
        void                                FlushNextFrame              (); 
        
    
    nri::DescriptorPool*                    GlobalDescriptorPool    = nullptr;
    nri::PipelineLayout*                    GlobalPipelineLayout    = nullptr;
    nri::DescriptorSet*                     SamplerDescriptorSet    = nullptr;
    nri::DescriptorSet*                     ResourcesDescriptorSet  = nullptr;
    
    TShaderDefinesManager*               ShaderDefinesManager = nullptr;
    TGlobalShadersManager*               GlobalShadersManager = nullptr;
    TRenderDescriptorHeapAllocator*      DescriptorHeapAllocator = nullptr;
    TRenderTexturesManager*                    TexturesManager = nullptr;
    TLegacyScene*                     LegacyScene = nullptr;
    TRenderResourcesFlusher*                ResourcesFlusher = nullptr;
    
    nri::Buffer*                            QuadGeometryBuffer = nullptr;
    uint64_t                                QuadGeometryOffset = 0; 
    TRenderTexture2D*                          BlackTexture = nullptr;
    TRenderTexture2D*                          WhiteTexture = nullptr;
    
private:
        void                                CreateSamplers          ();
        void                                CreateQuadBuffer        ();
    
    nri::Descriptor*                        LinearSampler = nullptr;
    
};
extern TRenderResourcesManager* GRenderResourcesManager;