#pragma once
#include "TRenderViewport.h"
#include "Extensions/NRIImgui.h"

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
                                TRender                  ();
                                ~TRender                 ();

            void                Initialize                  ();
            void                Destroy                     ();
            
            void                Submit                      (TRenderViewport* ToViewport);
            void                Render                      ();
            void                WaitGPU                     ();
            void                ResizeRenderTarget          (uint32_t InWidth, uint32_t InHeight);
            TRenderUIPass*   UIPass = nullptr;
    
    nri::Pipeline*              TestPipeline = nullptr;

                            protected:
            void                CreateGlobalConstantBuffer  ();
            void                UpdateGlobalConstantBuffer  ();
    
    xr_vector<FQueuedFrame>	QueuedFrames;
    
    nri::Fence*					FrameFence = nullptr;
    uint32_t			        FrameIndex = 0;
    nri::Fence*                 WaitSemaphore = nullptr;
    nri::Fence*                 SignalSemaphore = nullptr;
    

    
    nri::DescriptorSet*         GlobalConstantDescriptorSet  = nullptr;
    nri::Buffer*                GlobalConstantBuffer = nullptr; 
    nri::Descriptor*            GlobalConstantDescriptor = nullptr; 
    nri::Memory*                GlobalConstantBufferMemory = nullptr;
    
    nri::Pipeline*              Pipeline = nullptr;
    
    
    TRenderTarget2D*         OutputRenderTarget = nullptr;
    TRenderTarget2D*         DepthRenderTarget = nullptr;

    TRenderDeferredPass*        GeometryPass = nullptr;

    nri::Imgui*                 ImGuiInstance = nullptr;
private:
    bool                        IsWaitSubmit = false;
};
extern TRender* GRender;