#pragma once

struct FXRayDescriptorCache
{
    nri::Descriptor* Descriptors[256] = {};
    uint32_t Index = 0;
};

class XRayRenderDescriptorHeapAllocator
{
public:
                                                    XRayRenderDescriptorHeapAllocator    ();
                                                    ~XRayRenderDescriptorHeapAllocator   ();
        uint32_t                                    Alloc                           (nri::Descriptor* InDescriptor);
        void                                        Free                            (uint32_t Index);
    
        void                                        FlushNextFrame                  (); 
        void                                        UpdateDescriptorRanges          (); 
    
private:
    xr_vector<FXRayDescriptorCache*>                DescriptorCaches;
    
    xr_vector<nri::UpdateDescriptorRangeDesc>       UpdateDescriptorRangesDescriptions;
    xr_vector<uint32_t>                             FreeIndexes;
    xr_vector<uint32_t>                             FreeIndexesForNextFrame;
    uint32_t                                        NextIndex = 0;
};
