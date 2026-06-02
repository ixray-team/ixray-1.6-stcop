#pragma once

struct FSwapChainTexture 
{
    nri::Fence* AcquireSemaphore = nullptr;
    nri::Fence* ReleaseSemaphore = nullptr;
    nri::Texture* Texture = nullptr;
    nri::Descriptor* ColorAttachment = nullptr;
    nri::Format AttachmentFormat = nri::Format::UNKNOWN;
};

struct FQueuedFrame
{
    nri::CommandAllocator* CommandAllocator = nullptr;
    nri::CommandBuffer* CommandBuffer = nullptr;
};

class TRenderViewport
{
public:
                                        TRenderViewport      ();
                                        ~TRenderViewport     ();

            void                        CreateOrReset           (SDL_Window* InWindows, uint32_t InWidth, uint32_t InHeight,bool InVSync = false);
            void                        Destroy                 ();
            void                        WaitGPU                 ();
    
            void                        BeginRender             (nri::DescriptorPool* DescriptionPool = nullptr);
            nri::CommandBuffer&         GetCurrentCommandBuffer ();
            void                        EndRender               (nri::Fence* WaitSemaphore = nullptr, nri::Fence* SignalSemaphore = nullptr);
    
            uint32_t                    GetWidth                () const { return Width; }
            uint32_t                    GetHeight               () const { return Height; }
            nri::Format                 GetSwapChainFormat      () const { return SwapChainFormat; }
protected:
            uint8_t                     GetOptimalTextureNum    () const;
            uint8_t                     GetQueuedFrameNum       () const;
    
    nri::SwapChain*						SwapChain = nullptr;
    
    nri::Format                         SwapChainFormat = nri::Format::UNKNOWN;
    xr_vector<FSwapChainTexture>    SwapChainTextures;
    xr_vector<FQueuedFrame>			QueuedFrames;
    
    uint32_t							FrameIndex = 0;
    nri::Fence*							FrameFence = nullptr;
    
    bool                                bVSync = false;
    uint32_t                            Width = 0;
    uint32_t                            Height = 0;
private:
    bool                                bRenderer = false;
    uint32_t                            CurrentSwapChainTextureIndex = 0;
    
};
