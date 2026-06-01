#pragma once

namespace nri
{
    struct VertexAttributeDesc;
}

struct FXRayUIVertex 
{
    static nri::VertexAttributeDesc  VertexAttributeDescription[3];
    
    Fvector position;
    u32     color;
    float   uv[2];
};

