#pragma once

class TRenderResourceProxy
{
public:
                    TRenderResourceProxy    ();
    virtual         ~TRenderResourceProxy   ();
            u32     GetOrCreateHeapID       ();
            u32     GetHeapID               () const;
    
    nri::Descriptor*Descriptor = nullptr;
private:
    u32             HeapID = INDEX_NONE;
};
