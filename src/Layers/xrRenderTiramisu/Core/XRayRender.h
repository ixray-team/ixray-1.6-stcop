#pragma once
#include "XRayRenderViewport.h"

class XRayTexture;
class XRayRenderTarget2D;
class XRayTexture2D;
class XRayRenderUIPass;

struct FXRayRenderConstantBuffer
{
    Fvector4 SceneView;
};

class XRayRender
{
public:
                                XRayRender                  ();
                                ~XRayRender                 ();

            void                Initialize                  ();
            void                Destroy                     ();
            
            void                Submit                      (XRayRenderViewport* ToViewport);
            void                Render                      ();
            void                WaitGPU                     ();
            void                ResizeRenderTarget          (uint32_t InWidth, uint32_t InHeight);
            XRayRenderUIPass*   UIPass = nullptr;
    
    nri::Pipeline*              TestPipeline = nullptr;
protected:
            void                CreateGlobalConstantBuffer  ();
            void                UpdateGlobalConstantBuffer  ();
    
    xr_vector<FXRayQueuedFrame>	QueuedFrames;
    
    nri::Fence*					FrameFence = nullptr;
    uint32_t			        FrameIndex = 0;
    nri::Fence*                 WaitSemaphore = nullptr;
    nri::Fence*                 SignalSemaphore = nullptr;
    

    
    nri::DescriptorSet*         GlobalConstantDescriptorSet  = nullptr;
    nri::Buffer*                GlobalConstantBuffer = nullptr; 
    nri::Descriptor*            GlobalConstantDescriptor = nullptr; 
    nri::Memory*                GlobalConstantBufferMemory = nullptr;
    
    nri::Pipeline*              Pipeline = nullptr;
    
    
    XRayRenderTarget2D*         OutputRenderTarget = nullptr;
    
    
    XRayTexture*                TestTexture = nullptr;
    XRayTexture*                TestTexture2 = nullptr;
private:
    bool                        IsWaitSubmit = false;
};
extern XRayRender* GRender;