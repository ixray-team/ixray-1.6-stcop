// DXT.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#pragma warning(push)
#pragma warning(disable:4244)
#pragma warning(disable:4018)
#include "ddsw.hpp"
#pragma warning(pop)

#include "../../Layers/xrRender/ETextureParams.h"
#include <ddraw.h>
extern u32* Build32MipLevel(u32& _w, u32& _h, u32& _p, u32* pdwPixelSrc, STextureParams* fmt, float blend);

void GenerateAdvancedFilterMipMaps(u32& w, u32& h, nvtt::InputOptions& inOpt, u32 pitch, u8* raw_data, STextureParams* fmt, bool& result, nvtt::CompressionOptions& compOpt, nvtt::OutputOptions& outOpt)
{
	auto GetPowerOf2Plus1Lambda = [](u32 v)
	{
		u32 cnt = 0;
		while (v) { v >>= 1; cnt++; };
		return cnt;
	};

	u8* pImagePixels = 0;
	int numMipmaps = GetPowerOf2Plus1Lambda(std::min(w, h));
	inOpt.setMipmapGeneration(true, numMipmaps);
	u32 dwW = w;
	u32 dwH = h;
	u32 dwP = pitch;
	u32* pLastMip = xr_alloc<u32>(w * h * 4);
	memcpy(pLastMip, raw_data, w * h * 4);
	inOpt.setMipmapData(pLastMip, dwW, dwH, 1, 0, 0);

	float inv_fade = clampr(1.f - float(fmt->fade_amount) / 100.f, 0.f, 1.f);
	float blend = fmt->flags.is_any(STextureParams::flFadeToColor | STextureParams::flFadeToAlpha) ? inv_fade : 1.f;

	for (int i = 1; i < numMipmaps; i++)
	{
		u32* pNewMip = Build32MipLevel(dwW, dwH, dwP, pLastMip, fmt, i < fmt->fade_delay ? 0.f : 1.f - blend);
		xr_free(pLastMip);
		pLastMip = pNewMip;
		pNewMip = 0;
		inOpt.setMipmapData(pLastMip, dwW, dwH, 1, 0, i);
	}

	xr_free(pLastMip);

	result = nvtt::Compressor().process(inOpt, compOpt, outOpt);
	xr_free(pImagePixels);
}

static HFILE gFileOut;
static HFILE gFileIn;

int DXTCompressImageNVTT(LPCSTR out_name, u8* raw_data, u32 w, u32 h, u32 pitch, STextureParams* fmt, u32 depth)
{
	R_ASSERT(0 != w && 0 != h);
	gFileOut = _open(out_name, _O_WRONLY | _O_BINARY | _O_CREAT | _O_TRUNC, _S_IWRITE);
	if (gFileOut == -1)
	{
		fprintf(stderr, "Can't open output file %s\n", out_name);
		return 0;
	}
	bool result = false;
	nvtt::InputOptions inOpt;
	
	
	//auto layout = fmt->type == STextureParams::ttCubeMap ? nvtt::TextureType_Cube : nvtt::TextureType_2D;
	inOpt.setTextureLayout(nvtt::TextureType_2D, w, h);
	inOpt.setMipmapGeneration(fmt->flags.is(STextureParams::flGenerateMipMaps));
	inOpt.setWrapMode(nvtt::WrapMode_Clamp);
	inOpt.setNormalMap(false);
	inOpt.setConvertToNormalMap(false);
	inOpt.setGamma(2.2f, 2.2f);
	inOpt.setNormalizeMipmaps(false);

	nvtt::CompressionOptions compOpt;
	compOpt.setQuality(nvtt::Quality_Highest);
	compOpt.setQuantization(fmt->flags.is(STextureParams::flDitherColor), false, fmt->flags.is(STextureParams::flBinaryAlpha));

	switch (fmt->fmt)
	{
	case STextureParams::tfDXT1: 	compOpt.setFormat(nvtt::Format_DXT1); 	break;
	case STextureParams::tfADXT1: 	compOpt.setFormat(nvtt::Format_DXT1a); 	break;
	case STextureParams::tfDXT3: 	compOpt.setFormat(nvtt::Format_DXT3); 	break;
	case STextureParams::tfDXT5: 	compOpt.setFormat(nvtt::Format_DXT5); 	break;
	case STextureParams::tfBC7: 	compOpt.setFormat(nvtt::Format_BC7); 	break;
	case STextureParams::tfRGB: 	compOpt.setFormat(nvtt::Format_RGB); 	break;
	case STextureParams::tfRGBA: 	compOpt.setFormat(nvtt::Format_RGBA); 	break;
	}

	switch (fmt->mip_filter)
	{
	case STextureParams::kMIPFilterAdvanced:    break;
	case STextureParams::kMIPFilterBox:         inOpt.setMipmapFilter(nvtt::MipmapFilter_Box);      break;
	case STextureParams::kMIPFilterTriangle:    inOpt.setMipmapFilter(nvtt::MipmapFilter_Triangle); break;
	case STextureParams::kMIPFilterKaiser:      inOpt.setMipmapFilter(nvtt::MipmapFilter_Kaiser);   break;
	}

	nvtt::OutputOptions outOpt;
	
	DDSWriter writer(gFileOut);
	DDSErrorHandler handler;
	outOpt.setOutputHandler(&writer);
	outOpt.setErrorHandler(&handler);

	if ((fmt->flags.is(STextureParams::flGenerateMipMaps)) && (STextureParams::kMIPFilterAdvanced == fmt->mip_filter))
	{
		GenerateAdvancedFilterMipMaps(w, h, inOpt, pitch, raw_data, fmt, result, compOpt, outOpt);
	}
	else
	{
		rgba_t* pixels = new rgba_t[w * h * 4];
		u8* pixel = raw_data;
		for (u32 k = 0; k < w * h; k++, pixel += 4)
			pixels[k].set(pixel[0], pixel[1], pixel[2], pixel[3]);

		inOpt.setMipmapData(pixels, w, h);
		result = nvtt::Compressor().process(inOpt, compOpt, outOpt);
	}

	_close(gFileOut);
	if (!result)
	{
		unlink(out_name);
		return 0;
	}
	return 1;
}