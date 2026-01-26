// DXT.cpp : Defines the entry point for the DLL application.
//

#include "StdAfx.h"
#pragma warning(push)
#pragma warning(disable:4244)
#pragma warning(disable:4018)
#include "ddsw.hpp"
#pragma warning(pop)

#include "../../xrEngine/ETextureParams.h"
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

void ExtractCubeFacesFromCrossRGBA(const u8* src, int W, int H, int pitch, std::array<xr_vector<u8>, 6>& faces)
{
	int S = W / 4;
	auto copyFace = [&](int dstIndex, int srcX, int srcY)
	{
		faces[dstIndex].resize(S * S * 4);
		for (int y = 0; y < S; y++)
		{
			const u8* srcLine = src + (srcY + y) * pitch + srcX * 4;
			u8* dstLine = faces[dstIndex].data() + y * S * 4;
			memcpy(dstLine, srcLine, S * 4);
		}
	};

	// порядок: +X, -X, +Y, -Y, +Z, -Z
	copyFace(0, 2 * S, S); // +X
	copyFace(1, 0 * S, S); // -X
	copyFace(2, 1 * S, 0); // +Y
	copyFace(3, 1 * S, 2 * S); // -Y
	copyFace(4, 1 * S, S); // +Z
	copyFace(5, 3 * S, S); // -Z
}


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
	
	
	nvtt::TextureType layout = fmt->type == STextureParams::ttCubeMap ? nvtt::TextureType_Cube : nvtt::TextureType_2D;
	inOpt.setTextureLayout(layout, w, h);
	inOpt.setMipmapGeneration(fmt->flags.is(STextureParams::flGenerateMipMaps));
	inOpt.setWrapMode(nvtt::WrapMode_Clamp);
	inOpt.setNormalMap(false);
	inOpt.setConvertToNormalMap(false);
	inOpt.setGamma(2.2f, 2.2f);
	inOpt.setNormalizeMipmaps(false);

	nvtt::CompressionOptions compOpt;
	compOpt.setQuality(nvtt::Quality_Fastest);
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
		// se7kills: Нахрена это нужно ? 8к текстура утечка 1.1гб
		// rgba_t* pixels = new rgba_t[w * h * 4];
		
		u8* pixel = raw_data;
		// for (u32 k = 0; k < w * h; k++, pixel += 4)
		// 	pixels[k].set(pixel[0], pixel[1], pixel[2], pixel[3]);

		if (fmt->type == STextureParams::ttCubeMap)
		{
			std::array<xr_vector<u8>, 6> faces;
			ExtractCubeFacesFromCrossRGBA(raw_data, w, h, pitch, faces);

			u32 side = w / 4;
			inOpt.setTextureLayout(nvtt::TextureType_Cube, side, side);

			for (int face = 0; face < 6; face++)
				inOpt.setMipmapData(faces[face].data(), side, side, 1, face);
		}
		else
		{
			inOpt.setMipmapData(raw_data, w, h);
		}



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