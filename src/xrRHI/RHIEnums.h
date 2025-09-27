#pragma once

enum ERHI_API_LAYER
{
	D3D11,
	D3D9,

	NOT_CREATED = -1
};

enum class ERTColor
{
	Transparent,
	Gray,
	Black
};

enum class ERHI_USAGE
{
	USAGE_DEFAULT = 0,
	USAGE_IMMUTABLE = 1,
	USAGE_DYNAMIC = 2,
	USAGE_STAGING = 3
};