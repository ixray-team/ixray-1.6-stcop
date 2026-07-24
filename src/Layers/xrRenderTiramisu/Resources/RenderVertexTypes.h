#pragma once

#include "TiramisuRenderTypes.h"

namespace nri
{
struct VertexAttributeDesc;
}

// Вершина UI, совместимая с layout соответствующего global shader.
struct FUIVertex
{
	static nri::VertexAttributeDesc VertexAttributeDescription[3];

	Fvector position;
	u32 color;
	float uv[2];
};

enum class EVertexType : uint8_t
{
	BaseWithLightMap = 0,
	BaseWithLightColor = 1,
	VertexOnly = 2,
	MultipleUsageModel = 3,
	StaticMesh = 4,
};

// Базовая вершина нового static mesh pipeline.
struct FStaticMeshVertex
{
	static nri::VertexAttributeDesc VertexAttributeDescription[6];

	Fvector position;
	Fvector normal;
	float tangent[4];
	float uv0[2];
	float uv1[2];
	u32 color;
};

// Legacy-вершина уровня с базовыми и lightmap UV.
struct FLegacyLevelVertex_BaseWithLightMap
{
	static nri::VertexAttributeDesc VertexAttributeDescription[6];
	Fvector position;
	u32 normal;
	u32 tangent;
	u32 binormal;
	u16 uv0[2];
	u16 uv1[2];
};

// Legacy-вершина уровня с UV и предварительно вычисленным цветом света.
struct FLegacyLevelVertex_BaseWithLightColor
{
	static nri::VertexAttributeDesc VertexAttributeDescription[6];
	Fvector position;
	u32 normal;
	u32 tangent;
	u32 binormal;
	u32 color;
	u16 uv0[2];
};

// Минимальная legacy-вершина, содержащая только позицию.
struct FLegacyLevelVertex_OnlyVertex
{
	static nri::VertexAttributeDesc VertexAttributeDescription[1];
	Fvector position;
};

// Legacy-вершина модели с общим packed layout.
struct FLegacyLevelVertex_MultipleUsageModel
{
	static nri::VertexAttributeDesc VertexAttributeDescription[6];
	Fvector position;
	u32 normal;
	u32 tangent;
	u32 binormal;
	u16 uv0[4];
};
