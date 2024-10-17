#pragma once
#include <nvtt/nvtt.h>

#include "tPixel.h"

// filled in by reading a dds file
struct DDS_PIXELFORMAT
{
    DWORD dwSize;
    DWORD dwFlags;
    DWORD dwFourCC;
    DWORD dwRGBBitCount;
    DWORD dwRBitMask;
    DWORD dwGBitMask;
    DWORD dwBBitMask;
    DWORD dwRGBAlphaBitMask;
};

struct DDS_HEADER
{
    DWORD dwSize;
    DWORD dwHeaderFlags;
    DWORD dwHeight;
    DWORD dwWidth;
    DWORD dwPitchOrLinearSize;
    DWORD dwDepth; // only if DDS_HEADER_FLAGS_VOLUME is set in dwHeaderFlags
    DWORD dwMipMapCount;
    DWORD dwReserved1[11];
    DDS_PIXELFORMAT ddspf;
    DWORD dwSurfaceFlags;
    DWORD dwCubemapFlags;
    DWORD dwReserved2[3];
};

enum DDS_HEADER_FLAGS
{
    DDSD_CAPS = 1 << 0,
    DDSD_HEIGHT = 1 << 1,
    DDSD_WIDTH = 1 << 2,
    DDSD_PITCH = 1 << 3,
    DDSD_PIXELFORMAT = 1 << 12,
    DDSD_MIPMAPCOUNT = 1 << 17,
    DDSD_LINEARSIZE = 1 << 19,
    DDSD_DEPTH = 1 << 23,
};

class DDSWriter : 
    public nvtt::OutputHandler 
{
public:
	HFILE& file;

	DDSWriter(HFILE& file) : file(file) {};
	virtual void beginImage(int size, int width, int height, int depth, int face, int miplevel) override {}
	virtual bool writeData(const void* data, int size) override;
	virtual void endImage() override {};
};

class DDSErrorHandler : 
    public nvtt::ErrorHandler 
{
public:
	virtual void error(nvtt::Error e) override;
};
