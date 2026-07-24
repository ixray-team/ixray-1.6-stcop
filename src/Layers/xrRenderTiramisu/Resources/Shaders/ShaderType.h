#pragma once

#include "TiramisuRenderTypes.h"
enum class EShaderType
{
	Vertex = 0,
	Hull,
	Domain,
	Geometry,
	Pixel,
	Compute,
	Count,
	ALL,
	None = 0xFFFF
};

#define GLOBAL_SHADERS_SHIPPING_NAME "global.shaders"