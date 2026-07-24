#pragma once

#include "TiramisuRenderTypes.h"
#include "TiramisuRenderDescriptorHeapAllocator.h"
#include "Materials/TiramisuRenderMaterialInterface.h"

class TiramisuRenderMaterialsManager;
class TiramisuRenderScene;
class TiramisuRenderResourcesFlusher;
class TiramisuLegacyScene;
class TiramisuGlobalShadersManager;
class TiramisuShaderDefinesManager;
class TiramisuRenderMaterialGpuStorage;
class TiramisuRenderMaterialPipelineRegistry;
class TiramisuRenderMaterialShaderLibrary;

// Корневой владелец shader, material, texture и scene ресурсов renderer.
class TiramisuRenderResourcesManager
{
public:
                                            TiramisuRenderResourcesManager  ();
                                            ~TiramisuRenderResourcesManager ();
        // Создаёт общие descriptor heaps и менеджеры в порядке их зависимостей.
        void                                Initialize                  ();
        bool                                IsCookedMode                ();
        // Разделяет game-thread запрос flush и его безопасное render-thread исполнение.
        void                                FlushNextFrame              (); 
        // Разделяет game-thread запрос flush и его безопасное render-thread исполнение.
        void                                FlushNextFrame_RenderThread (); 
        
    
    nri::DescriptorPool*                    GlobalDescriptorPool    = nullptr;
    nri::PipelineLayout*                    GlobalPipelineLayout    = nullptr;
    nri::DescriptorSet*                     SamplerDescriptorSet    = nullptr;
    nri::DescriptorSet*                     ResourcesDescriptorSet  = nullptr;
    
    TiramisuShaderDefinesManager*                  ShaderDefinesManager = nullptr;
    TiramisuGlobalShadersManager*                  GlobalShadersManager = nullptr;
    TiramisuRenderDescriptorHeapAllocator*         DescriptorHeapAllocator = nullptr;
    TiramisuRenderTexturesManager*                 TexturesManager = nullptr;
    TiramisuRenderMaterialsManager*                MaterialsManager = nullptr;
    TiramisuRenderMaterialGpuStorage*              MaterialGpuStorage = nullptr;
    TiramisuRenderMaterialPipelineRegistry*         MaterialPipelineRegistry = nullptr;
    TiramisuRenderMaterialShaderLibrary*            MaterialShaderLibrary = nullptr;
    TiramisuRenderScene*                           RenderScene = nullptr;
    
    nri::Buffer*                            QuadGeometryBuffer = nullptr;
    u64                                QuadGeometryOffset = 0; 
    TiramisuRenderMaterialInterface*               DefaultMaterial = nullptr;
    
    TiramisuRenderTexture2D*                       BlackTexture = nullptr;
    TiramisuRenderTexture2D*                       WhiteTexture = nullptr;
    
private:
        void                                CreateSamplers          ();
        void                                CreateQuadBuffer        ();
    
    nri::Descriptor*                        LinearSampler = nullptr;
    
};
extern TiramisuRenderResourcesManager* GRenderResourcesManager;
