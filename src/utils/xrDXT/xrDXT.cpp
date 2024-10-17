#include "StdAfx.h"
#include "xrDXT.h"

#include <RedImage.hpp>

extern int DXTCompress(LPCSTR out_name, u8* raw_data, u8* normal_map, u32 w, u32 h, u32 pitch, STextureParams* fmt, u32 depth);

void DXTUtils::Converter::MakeTGA(xr_path From, xr_path To)
{
	RedImageTool::RedImage Surface;
	if (Surface.LoadFromFile(From.xstring().data()))
	{
		Surface.Convert(RedImageTool::RedTexturePixelFormat::R8G8B8A8);
		Surface.SaveToTga(To.xstring().data());
	}
}

void DXTUtils::Converter::MakePNG(xr_path From, xr_path To)
{
	RedImageTool::RedImage Surface;
	if (Surface.LoadFromFile(From.xstring().data()))
	{
		Surface.Convert(RedImageTool::RedTexturePixelFormat::R8G8B8A8);
		Surface.SaveToPng(To.xstring().data());
	}
}

int DXT_API DXTUtils::Compress(const char* out_name, u8* raw_data, u8* normal_map, u32 w, u32 h, u32 pitch, STextureParams* fmt, u32 depth)
{
	return DXTCompress(out_name, raw_data, normal_map, w, h, pitch, fmt, depth);
}

U8Vec DXT_API DXTUtils::GitPixels(const char* FileName, u32 NewW, u32 NewH)
{
	U8Vec Pixels;

	RedImageTool::RedImage Img;
	Img.LoadFromFile(FileName);
	Img.Convert(RedImageTool::RedTexturePixelFormat::R8G8B8A8);
	Img.Scale(NewW, NewH, RedImageTool::RedResizeFilter::Cubicbspline);
	Img.SwapRB();

	Pixels.resize(NewH * NewW * 4);
	memcpy(Pixels.data(), *Img, NewH * NewW * 4);

	return Pixels;
}

DXTUtils::ImageInfo DXT_API DXTUtils::GitPixels(const char* FileName)
{
	U8Vec Pixels;

	RedImageTool::RedImage Img;
	Img.LoadFromFile(FileName);
	Img.Convert(RedImageTool::RedTexturePixelFormat::R8G8B8A8);
	Img.SwapRB();

	size_t PixelCount = Img.GetWidth() * Img.GetHeight() * 4;
	Pixels.resize(PixelCount);
	memcpy(Pixels.data(), *Img, PixelCount);

	return { Img.GetWidth(), Img.GetHeight(), Pixels };
}
