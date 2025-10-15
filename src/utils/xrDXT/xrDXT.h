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
		u32 W;
		u32 H;
		U8Vec P;
		shared_str Format;
	};

	struct DXT_API Converter
	{
		static void MakeTGA(xr_path From, xr_path To);
		static void MakeTGA(u32 w, u32 h, U32Vec& From, xr_path To);
		static void MakePNG(xr_path From, xr_path To);
	};

	struct DXT_API Filter
	{
		enum EIMF_Type
		{
			imf_filter = 0,
			imf_box,
			imf_triangle,
			imf_bell,
			imf_b_spline,
			imf_lanczos3,
			imf_mitchell,

			imf_FORCEDWORD = 0xffffffff
		};

		static void Process(u32* dst, u32 dstW, u32 dstH, u32* src, u32 srcW, u32 srcH, EIMF_Type FILTER);
		static void Resize(u32* dst, u32 dstW, u32 dstH, u32* out, u32 outW, u32 outH);
	};

	int DXT_API Compress(const char* out_name, u8* raw_data, u8* normal_map, u32 w, u32 h, u32 pitch, STextureParams* fmt, u32 depth);
	U8Vec DXT_API GitPixels(const char* FileName, u32 NewW, u32 NewH);
	ImageInfo DXT_API GitPixels(const char* FileName);
}