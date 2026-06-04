#pragma once

struct FXRayDescriptorCache
{
    nri::Descriptor* Descriptors[256] = {};
    uint32_t Index = 0;
};

class TRenderDescriptorHeapAllocator
{
public:
                                                    TRenderDescriptorHeapAllocator    ();
                                                    ~TRenderDescriptorHeapAllocator   ();
        uint32_t                                    Alloc                           (nri::Descriptor* InDescriptor);
        void                                        Free                            (uint32_t Index);
    
        void                                        FlushNextFrame_RenderThread                  (); 
        void                                        UpdateDescriptorRanges          (); 
    
private:
    xr_vector<FXRayDescriptorCache*>                DescriptorCaches;
    
    xr_vector<nri::UpdateDescriptorRangeDesc>       UpdateDescriptorRangesDescriptions;
    xr_vector<uint32_t>                             FreeIndexes;
    xr_vector<uint32_t>                             FreeIndexesForNextFrame;
    uint32_t                                        NextIndex = 0;
#ifdef DEBUG
    xr_vector<nri::Descriptor*>                     DebugState;
#endif
};
