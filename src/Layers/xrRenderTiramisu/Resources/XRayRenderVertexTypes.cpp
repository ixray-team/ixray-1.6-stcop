nri::VertexAttributeDesc FXRayUIVertex::VertexAttributeDescription[3] = 
{
    {{"POSITION", 0}, {0},offsetof(FXRayUIVertex, position),nri::Format::RGB32_SFLOAT,0, },
    {{"COLOR", 0}, {1},offsetof(FXRayUIVertex, color),nri::Format::RGBA8_UNORM,0, },
    {{"TEXCOORD", 0}, {2},offsetof(FXRayUIVertex, uv),nri::Format::RG32_SFLOAT,0, }
};