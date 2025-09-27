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

enum class ERHI_DSV_DIMENSION
{
	UNKNOWN = 0,
	TEXTURE1D = 1,
	TEXTURE1DARRAY = 2,
	TEXTURE2D = 3,
	TEXTURE2DARRAY = 4,
	TEXTURE2DMS = 5,
	TEXTURE2DMSARRAY = 6
};

enum class ERHI_VIEW_DIMENSION
{
	Texture2D,
	Texture3D,
	Buffer
};