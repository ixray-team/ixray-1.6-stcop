#pragma once

#include "TiramisuRenderTypes.h"

// Render-thread представление GPU-ресурса с явным жизненным циклом.
class TiramisuRenderResourceProxy
{
public:
                    TiramisuRenderResourceProxy    ();
    virtual         ~TiramisuRenderResourceProxy   ();
            u32     GetOrCreateHeapID       ();
            u32     GetHeapID               () const;
    
    nri::Descriptor*Descriptor = nullptr;
private:
    u32             HeapID = INDEX_NONE;
};
