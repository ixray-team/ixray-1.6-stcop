nri::VertexAttributeDesc FUIVertex::VertexAttributeDescription[3] =
	{
		{
			{"POSITION", 0},
			{0},
			offsetof(FUIVertex, position),
			nri::Format::RGB32_SFLOAT,
			0,
		},
		{
			{"COLOR", 0},
			{1},
			offsetof(FUIVertex, color),
			nri::Format::RGBA8_UNORM,
			0,
		},
		{
			{"TEXCOORD", 0},
			{2},
			offsetof(FUIVertex, uv),
			nri::Format::RG32_SFLOAT,
			0,
		}
};

nri::VertexAttributeDesc FStaticMeshVertex::VertexAttributeDescription[6] =
	{
		{{"POSITION", 0}, {0}, offsetof(FStaticMeshVertex, position), nri::Format::RGB32_SFLOAT, 0},
		{{"NORMAL", 0}, {1}, offsetof(FStaticMeshVertex, normal), nri::Format::RGB32_SFLOAT, 0},
		{{"TANGENT", 0}, {2}, offsetof(FStaticMeshVertex, tangent), nri::Format::RGBA32_SFLOAT, 0},
		{{"TEXCOORD", 0}, {3}, offsetof(FStaticMeshVertex, uv0), nri::Format::RG32_SFLOAT, 0},
		{{"TEXCOORD", 1}, {4}, offsetof(FStaticMeshVertex, uv1), nri::Format::RG32_SFLOAT, 0},
		{{"COLOR", 0}, {5}, offsetof(FStaticMeshVertex, color), nri::Format::RGBA8_UNORM, 0},
};

static_assert(sizeof(FStaticMeshVertex) == 60);
static_assert(offsetof(FStaticMeshVertex, position) == 0);
static_assert(offsetof(FStaticMeshVertex, normal) == 12);
static_assert(offsetof(FStaticMeshVertex, tangent) == 24);
static_assert(offsetof(FStaticMeshVertex, uv0) == 40);
static_assert(offsetof(FStaticMeshVertex, uv1) == 48);
static_assert(offsetof(FStaticMeshVertex, color) == 56);

nri::VertexAttributeDesc FLegacyLevelVertex_BaseWithLightMap::VertexAttributeDescription[6] =
	{
		{
			{"POSITION", 0},
			{0},
			offsetof(FLegacyLevelVertex_BaseWithLightMap, position),
			nri::Format::RGB32_SFLOAT,
			0,
		},
		{
			{"TEXCOORD", 0},
			{1},
			offsetof(FLegacyLevelVertex_BaseWithLightMap, normal),
			nri::Format::RGBA8_SNORM,
			0,
		},
		{
			{"TEXCOORD", 1},
			{2},
			offsetof(FLegacyLevelVertex_BaseWithLightMap, tangent),
			nri::Format::RGBA8_SNORM,
			0,
		},
		{
			{"TEXCOORD", 2},
			{3},
			offsetof(FLegacyLevelVertex_BaseWithLightMap, binormal),
			nri::Format::RGBA8_SNORM,
			0,
		},
		{
			{"TEXCOORD", 3},
			{4},
			offsetof(FLegacyLevelVertex_BaseWithLightMap, uv0),
			nri::Format::RG16_SINT,
			0,
		},
		{
			{"TEXCOORD", 4},
			{5},
			offsetof(FLegacyLevelVertex_BaseWithLightMap, uv1),
			nri::Format::RG16_SINT,
			0,
		},
};

nri::VertexAttributeDesc FLegacyLevelVertex_BaseWithLightColor::VertexAttributeDescription[6] =
	{
		{
			{"POSITION", 0},
			{0},
			offsetof(FLegacyLevelVertex_BaseWithLightColor, position),
			nri::Format::RGB32_SFLOAT,
			0,
		},
		{
			{"TEXCOORD", 0},
			{1},
			offsetof(FLegacyLevelVertex_BaseWithLightColor, normal),
			nri::Format::RGBA8_SNORM,
			0,
		},
		{
			{"TEXCOORD", 1},
			{2},
			offsetof(FLegacyLevelVertex_BaseWithLightColor, tangent),
			nri::Format::RGBA8_SNORM,
			0,
		},
		{
			{"TEXCOORD", 2},
			{3},
			offsetof(FLegacyLevelVertex_BaseWithLightColor, binormal),
			nri::Format::RGBA8_SNORM,
			0,
		},
		{
			{"COLOR", 0},
			{4},
			offsetof(FLegacyLevelVertex_BaseWithLightColor, color),
			nri::Format::RGBA8_UNORM,
			0,
		},
		{
			{"TEXCOORD", 3},
			{5},
			offsetof(FLegacyLevelVertex_BaseWithLightColor, uv0),
			nri::Format::RG16_SINT,
			0,
		},
};

nri::VertexAttributeDesc FLegacyLevelVertex_OnlyVertex::VertexAttributeDescription[1] =
	{
		{
			{"POSITION", 0},
			{0},
			offsetof(FLegacyLevelVertex_OnlyVertex, position),
			nri::Format::RGB32_SFLOAT,
			0,
		},
};

nri::VertexAttributeDesc FLegacyLevelVertex_MultipleUsageModel::VertexAttributeDescription[6] =
	{
		{
			{"POSITION", 0},
			{0},
			offsetof(FLegacyLevelVertex_MultipleUsageModel, position),
			nri::Format::RGB32_SFLOAT,
			0,
		},
		{
			{"TEXCOORD", 0},
			{1},
			offsetof(FLegacyLevelVertex_MultipleUsageModel, normal),
			nri::Format::RGBA8_SNORM,
			0,
		},
		{
			{"TEXCOORD", 1},
			{2},
			offsetof(FLegacyLevelVertex_MultipleUsageModel, tangent),
			nri::Format::RGBA8_SNORM,
			0,
		},
		{
			{"TEXCOORD", 2},
			{3},
			offsetof(FLegacyLevelVertex_MultipleUsageModel, binormal),
			nri::Format::RGBA8_SNORM,
			0,
		},
		{
			{"TEXCOORD", 3},
			{4},
			offsetof(FLegacyLevelVertex_MultipleUsageModel, uv0),
			nri::Format::RGBA16_UINT,
			0,
		},
};
