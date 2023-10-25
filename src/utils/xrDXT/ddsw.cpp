
#include "StdAfx.h"
#include "ddsw.hpp"

const u32 fcc_DXT1 = MAKEFOURCC('D', 'X', 'T', '1');
const u32 fcc_DXT2 = MAKEFOURCC('D', 'X', 'T', '2');
const u32 fcc_DXT3 = MAKEFOURCC('D', 'X', 'T', '3');
const u32 fcc_DXT4 = MAKEFOURCC('D', 'X', 'T', '4');
const u32 fcc_DXT5 = MAKEFOURCC('D', 'X', 'T', '5');

bool DDSWriter::writeData(const void* data, int size)
{
	if (size == sizeof(DDS_HEADER))
	{
		DDS_HEADER* hdr = (DDS_HEADER*)data;
		if (hdr->dwSize == size)
		{
			switch (hdr->ddspf.dwFourCC)
			{
			case fcc_DXT1:
			case fcc_DXT2:
			case fcc_DXT3:
			case fcc_DXT4:
			case fcc_DXT5:
				hdr->ddspf.dwRGBBitCount = 0;
				break;
			}
		}
	}
	_write(file, data, size);
	return true;
}


void DDSErrorHandler::error(nvtt::Error e)
{
	const char* msg;
	switch (e)
	{
	default:
	case nvtt::Error_Unknown:               msg = "Unknown error"; break;
	case nvtt::Error_InvalidInput:          msg = "Invalid input"; break;
	case nvtt::Error_UnsupportedFeature:    msg = "Unsupported feature"; break;
	case nvtt::Error_CudaError:             msg = "CUDA error"; break;
	case nvtt::Error_FileOpen:              msg = "File open error"; break;
	case nvtt::Error_FileWrite:             msg = "File write error"; break;
	}
	MessageBoxA(0, msg, "DXT compress error", MB_ICONERROR | MB_OK);
}