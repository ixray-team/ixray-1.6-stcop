#pragma once
#include <barrier>

#include "TRenderViewport.h"
#include "Extensions/NRIImgui.h"

class TRenderTargetResourceProxy;
class TRenderDeferredPass;
class TRenderTexture;
class TRenderTarget2D;
class TRenderTexture2D;
class TRenderUIPass;

struct FXRayRenderConstantBuffer
{
    Fvector4    SceneView;
    Fmatrix     ViewProjection;
};

class TRender
{
public:
                                TRender                             ();
                                ~TRender                            ();

            void                Initialize                          ();
            void                Destroy                             ();
            
            void                SetViewport                         (TRenderViewport* ToViewport);
            void                SubmitFrame                         ();
    
            void                Render_RenderThread                 ();
            void                WaitGPU_RenderThread                ();
    
            void                ResizeRenderTarget                  (uint32_t InWidth, uint32_t InHeight);
            TRenderUIPass*      UIPass = nullptr;
    
            void                EnableRenderThread                  ();
            void                DisableRenderThread                 ();
            void                DisableRenderThreadWithWaitStoping  ();
    
protected:
            void                CreateGlobalConstantBuffer          ();
            void                UpdateGlobalConstantBuffer          ();
            void                Submit                              (TRenderViewport* ToViewport);
    
    TRenderViewport*            CurrentViewport = nullptr;
    xr_vector<FQueuedFrame>	    QueuedFrames;
    
    nri::Fence*					FrameFence = nullptr;
    uint32_t			        FrameIndex = 0;
    nri::Fence*                 WaitSemaphore = nullptr;
    nri::Fence*                 SignalSemaphore = nullptr;
    
    nri::DescriptorSet*         GlobalConstantDescriptorSet  = nullptr;
    nri::Buffer*                GlobalConstantBuffer = nullptr; 
    nri::Descriptor*            GlobalConstantDescriptor = nullptr; 
    nri::Memory*                GlobalConstantBufferMemory = nullptr;
    
    nri::Pipeline*              Pipeline = nullptr;
    
    
    TRenderTarget2D*            OutputRenderTarget = nullptr;
    TRenderTarget2D*            DepthRenderTarget = nullptr;

    TRenderTargetResourceProxy* OutputRenderTarget_RenderThread = nullptr;
    TRenderTargetResourceProxy* DepthRenderTarget_RenderThread = nullptr;
    
    TRenderDeferredPass*        GeometryPass = nullptr;

    nri::Imgui*                 ImGuiInstance = nullptr;
private:
    static void                 OnThread            (void* p);
            void                SpawnRenderThread   ();
    
    volatile bool               bRenderThreadEnable = true;
    std::barrier<>              SyncPoint{2};
    ThreadID                    RenderThread = nullptr;
};
extern TRender* GRender;