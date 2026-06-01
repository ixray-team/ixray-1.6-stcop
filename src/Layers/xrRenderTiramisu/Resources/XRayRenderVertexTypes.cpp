nri::VertexAttributeDesc FXRayUIVertex::VertexAttributeDescription[3] = 
{
    {{"POSITION", 0}, {0},offsetof(FXRayUIVertex, position),nri::Format::RGB32_SFLOAT,0, },
    {{"COLOR", 0}, {1},offsetof(FXRayUIVertex, color),nri::Format::RGBA8_UNORM,0, },
    {{"TEXCOORD", 0}, {2},offsetof(FXRayUIVertex, uv),nri::Format::RG32_SFLOAT,0, }
};

nri::VertexAttributeDesc FXRayLegacyLevelVertex_BaseWithLightMap::VertexAttributeDescription[6] = 
{
    {{"POSITION", 0}, {0},offsetof(FXRayLegacyLevelVertex_BaseWithLightMap, position),nri::Format::RGB32_SFLOAT,0, },
    {{"TEXCOORD", 0}, {1},offsetof(FXRayLegacyLevelVertex_BaseWithLightMap, normal),nri::Format::RGBA8_SNORM,0, },
    {{"TEXCOORD", 1}, {2},offsetof(FXRayLegacyLevelVertex_BaseWithLightMap, tangent),nri::Format::RGBA8_SNORM,0, },
    {{"TEXCOORD", 2}, {3},offsetof(FXRayLegacyLevelVertex_BaseWithLightMap, binormal),nri::Format::RGBA8_SNORM,0, },
    {{"TEXCOORD", 3}, {4},offsetof(FXRayLegacyLevelVertex_BaseWithLightMap, uv0),nri::Format::RG16_SINT,0, },
    {{"TEXCOORD", 4}, {5},offsetof(FXRayLegacyLevelVertex_BaseWithLightMap, uv1),nri::Format::RG16_SINT,0, },
};

nri::VertexAttributeDesc FXRayLegacyLevelVertex_BaseWithLightColor::VertexAttributeDescription[6] = 
{
    {{"POSITION", 0}, {0},offsetof(FXRayLegacyLevelVertex_BaseWithLightColor, position),nri::Format::RGB32_SFLOAT,0, },
    {{"TEXCOORD", 0}, {1},offsetof(FXRayLegacyLevelVertex_BaseWithLightColor, normal),nri::Format::RGBA8_SNORM,0, },
    {{"TEXCOORD", 1}, {2},offsetof(FXRayLegacyLevelVertex_BaseWithLightColor, tangent),nri::Format::RGBA8_SNORM,0, },
    {{"TEXCOORD", 2}, {3},offsetof(FXRayLegacyLevelVertex_BaseWithLightColor, binormal),nri::Format::RGBA8_SNORM,0, },
    {{"COLOR", 0}, {4},offsetof(FXRayLegacyLevelVertex_BaseWithLightColor, color),nri::Format::RGBA8_UNORM,0, },
    {{"TEXCOORD", 3}, {5},offsetof(FXRayLegacyLevelVertex_BaseWithLightColor, uv0),nri::Format::RG16_SINT,0, },
};

nri::VertexAttributeDesc FXRayLegacyLevelVertex_OnlyVertex::VertexAttributeDescription[1] = 
{
    {{"POSITION", 0}, {0},offsetof(FXRayLegacyLevelVertex_OnlyVertex, position),nri::Format::RGB32_SFLOAT,0, },
};

nri::VertexAttributeDesc FXRayLegacyLevelVertex_MultipleUsageModel::VertexAttributeDescription[6] = 
{
    {{"POSITION", 0}, {0},offsetof(FXRayLegacyLevelVertex_MultipleUsageModel, position),nri::Format::RGB32_SFLOAT,0, },
    {{"TEXCOORD", 0}, {1},offsetof(FXRayLegacyLevelVertex_MultipleUsageModel, normal),nri::Format::RGBA8_SNORM,0, },
    {{"TEXCOORD", 1}, {2},offsetof(FXRayLegacyLevelVertex_MultipleUsageModel, tangent),nri::Format::RGBA8_SNORM,0, },
    {{"TEXCOORD", 2}, {3},offsetof(FXRayLegacyLevelVertex_MultipleUsageModel, binormal),nri::Format::RGBA8_SNORM,0, },
    {{"TEXCOORD", 3}, {4},offsetof(FXRayLegacyLevelVertex_MultipleUsageModel, uv0),nri::Format::RGBA16_UINT,0, },
};
