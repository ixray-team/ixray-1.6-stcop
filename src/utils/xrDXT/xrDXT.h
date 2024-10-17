#pragma once

#ifdef DXT_EXPORTS
#	define DXT_API __declspec(dllexport)
#else
#	define DXT_API __declspec(dllimport)
#endif

struct STextureParams;

namespace DXTUtils
{
	struct ImageInfo
	{
		size_t W;
		size_t H;
		U8Vec P;
	};

	struct DXT_API Converter
	{
		static void MakeTGA(xr_path From, xr_path To);
		static void MakePNG(xr_path From, xr_path To);
	};

	int DXT_API Compress(const char* out_name, u8* raw_data, u8* normal_map, u32 w, u32 h, u32 pitch, STextureParams* fmt, u32 depth);
	U8Vec DXT_API GitPixels(const char* FileName, u32 NewW, u32 NewH);
	ImageInfo DXT_API GitPixels(const char* FileName);
}