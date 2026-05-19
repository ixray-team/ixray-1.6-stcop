#include "ScreenshotManager.h"
#include "stdafx.h"

#ifdef IXR_WINDOWS
#	include <wincodec.h>
#	include <memory>
#endif

#ifdef IXR_LINUX
#	define STB_IMAGE_WRITE_IMPLEMENTATION
#	define NULL 0
#	include <stb/stb_image_write.h>
#endif

extern int SM_FOR_SEND_WIDTH;
extern int SM_FOR_SEND_HEIGHT;
using namespace DirectX;


bool ScreenshotManager::SaveScreenshot(IRender_interface::ScreenshotMode Mode, const char* Name, CMemoryWriter* MemoryWriter)
{
	if (!GRHI || !GRHI->DevicePtr)
	{
		return false;
	}

	IRHIRenderTargetView* Rtv = GRHI->GetRenderTargetView(0);
	if (Rtv == nullptr)
	{
		Rtv = RTarget;

		if (Rtv == nullptr)
		{
			return false;
		}
	}

	u32 Width = 0;
	u32 Height = 0;
	u32 RowPitch = 0;

	// estimate buffer size: will be updated by ReadRenderTargetPixels
	u32 MaxSize = 4096 * 4096 * 4;
	xr_unique_ptr<u8[]> Buffer(new u8[MaxSize]);

	bool Ok = GRHI->DevicePtr->ReadRenderTargetPixels(Rtv, Buffer.get(), MaxSize, Width, Height, RowPitch);
	if (!Ok)
	{
		return false;
	}

	// Create ScratchImage in BGRA8 format and copy rows
	ScratchImage Img;
	HRESULT Hr = Img.Initialize2D((DXGI_FORMAT)Rtv->GetSurface()->GetFormat(), Width, Height, 1, 1);
	if (FAILED(Hr))
	{
		return false;
	}

	u8* DstPixels = reinterpret_cast<u8*>(Img.GetPixels());
	u32 DstRow = Width * 4;

	for (u32 Y = 0; Y < Height; ++Y)
	{
		memcpy(DstPixels + (size_t)Y * DstRow, Buffer.get() + (size_t)Y * RowPitch, DstRow);
	}

#ifdef IXR_LINUX
	// Convert BGRA to RGBA for stb_image_write
	for (u32 i = 0; i < Width * Height; ++i)
	{
		u8* pixel = DstPixels + i * 4;
		std::swap(pixel[0], pixel[2]);
	}
#endif

	Blob Saved = {};

	switch (Mode)
	{
	case IRender_interface::SM_FOR_GAMESAVE:
	{
		ScratchImage Small;
		Hr = Resize(*Img.GetImage(0,0,0), EngineExternal().gamesaveSize.x, EngineExternal().gamesaveSize.y, TEX_FILTER_FLAGS::TEX_FILTER_DEFAULT, Small);
		if (FAILED(Hr))
		{
			return false;
		}

#ifdef IXR_WINDOWS
		Hr = SaveToDDSMemory(*Small.GetImage(0,0,0), DirectX::DDS_FLAGS_NONE, Saved);
		if (FAILED(Hr))
		{
			return false;
		}
		
		auto Fs = FS.w_open(Name);
		R_ASSERT(Fs);
		Fs->w(Saved.GetBufferPointer(), (u32)Saved.GetBufferSize());
		FS.w_close(Fs);
#else
		// Linux: save directly to file as PNG
		const Image* SrcImg = Small.GetImage(0,0,0);
		
		auto Fs = FS.w_open(Name);
		if (Fs)
		{
			struct CallbackData
			{
				IWriter* fs;
			} cbData = {Fs};
			
			stbi_write_png_to_func(
				[](void* context, void* data, int size) {
					CallbackData* cb = (CallbackData*)context;
					cb->fs->w(data, size);
				},
				&cbData, SrcImg->width, SrcImg->height, 4, SrcImg->pixels, SrcImg->width * 4
			);
			FS.w_close(Fs);
			Hr = S_OK;
		}
		else
		{
			Hr = E_FAIL;
		}
		
		if (FAILED(Hr))
		{
			return false;
		}
#endif
	}
	break;

	case IRender_interface::SM_FOR_MPSENDING:
	{
		ScratchImage Small;
		Hr = Resize(*Img.GetImage(0,0,0), SM_FOR_SEND_WIDTH, SM_FOR_SEND_HEIGHT, TEX_FILTER_FLAGS::TEX_FILTER_DEFAULT, Small);
		if (FAILED(Hr))
		{
			return false;
		}

#ifdef IXR_WINDOWS
		Hr = SaveToDDSMemory(*Small.GetImage(0,0,0), DDS_FLAGS::DDS_FLAGS_NONE, Saved);
		if (FAILED(Hr))
		{
			return false;
		}
		
		if (!MemoryWriter)
		{
			auto Fs = FS.w_open(Name);
			R_ASSERT(Fs);
			Fs->w(Saved.GetBufferPointer(), (u32)Saved.GetBufferSize());
			FS.w_close(Fs);
		}
		else
		{
			MemoryWriter->w(Saved.GetBufferPointer(), (u32)Saved.GetBufferSize());
		}
#else
		// Linux: save directly to file or memory writer as PNG
		const Image* SrcImg = Small.GetImage(0,0,0);
		
		if (!MemoryWriter)
		{
			auto Fs = FS.w_open(Name);
			if (Fs)
			{
				struct CallbackData
				{
					IWriter* fs;
				} cbData = {Fs};
				
				stbi_write_png_to_func(
					[](void* context, void* data, int size) {
						CallbackData* cb = (CallbackData*)context;
						cb->fs->w(data, size);
					},
					&cbData, SrcImg->width, SrcImg->height, 4, SrcImg->pixels, SrcImg->width * 4
				);
				FS.w_close(Fs);
				Hr = S_OK;
			}
			else
			{
				Hr = E_FAIL;
			}
		}
		else
		{
			// Write to memory writer via custom callback
			struct CallbackData
			{
				CMemoryWriter* writer;
			} cbData = {MemoryWriter};
			
			stbi_write_png_to_func(
				[](void* context, void* data, int size) {
					CallbackData* cb = (CallbackData*)context;
					cb->writer->w(data, size);
				},
				&cbData, SrcImg->width, SrcImg->height, 4, SrcImg->pixels, SrcImg->width * 4
			);
			Hr = S_OK;
		}
		
		if (FAILED(Hr))
		{
			return false;
		}
#endif
	}
	break;

	case IRender_interface::SM_NORMAL:
	{
		string64 TStamp = {};
		string_path Buf = {};
		xr_string LvlName = "mainmenu";
		if (g_pGameLevel)
		{
			LvlName = g_pStringTable->translate(g_pGameLevel->name().c_str()).c_str();
		}

		const Image* SrcImg = Img.GetImage(0,0,0);
		int W = SrcImg->width;
		int H = SrcImg->height;

#ifdef IXR_WINDOWS
		if (ps_screenshot_format == 0)
		{
			xr_sprintf(Buf, sizeof(Buf), "ss_%s_%s_(%s).jpg", Core.UserName, timestamp(TStamp), LvlName.c_str());
			Hr = SaveToWICMemory(*SrcImg, WIC_FLAGS::WIC_FLAGS_FORCE_SRGB, GUID_ContainerFormatJpeg, Saved);
		}
		else if (ps_screenshot_format == 1)
		{
			xr_sprintf(Buf, sizeof(Buf), "ss_%s_%s_(%s).tga", Core.UserName, timestamp(TStamp), LvlName.c_str());
			Hr = SaveToTGAMemory(*SrcImg, TGA_FLAGS::TGA_FLAGS_FORCE_SRGB, Saved);
		}
		else // ps_screenshot_format == 2
		{
			xr_sprintf(Buf, sizeof(Buf), "ss_%s_%s_(%s).png", Core.UserName, timestamp(TStamp), LvlName.c_str());
			Hr = SaveToWICMemory(*SrcImg, WIC_FLAGS::WIC_FLAGS_FORCE_SRGB, GUID_ContainerFormatPng, Saved);
		}
		
		if (FAILED(Hr))
		{
			return false;
		}
		
		auto Fs = FS.w_open("$screenshots$", Buf);
		R_ASSERT(Fs);
		Fs->w(Saved.GetBufferPointer(), (u32)Saved.GetBufferSize());
		FS.w_close(Fs);
#else
		// Linux implementation using stb_image_write
		if (ps_screenshot_format == 0) // JPEG
		{
			xr_sprintf(Buf, sizeof(Buf), "ss_%s_%s_(%s).jpg", Core.UserName, timestamp(TStamp), LvlName.c_str());
			auto Fs = FS.w_open("$screenshots$", Buf);
			if (Fs)
			{
				struct CallbackData
				{
					IWriter* fs;
				} cbData = {Fs};
				
				stbi_write_jpg_to_func
				(
					[](void* context, void* data, int size)
					{
						CallbackData* cb = (CallbackData*)context;
						cb->fs->w(data, size);
					},
					&cbData, W, H, 4, SrcImg->pixels, 90
				);
				FS.w_close(Fs);
				Hr = S_OK;
			}
			else
			{
				Hr = E_FAIL;
			}
		}
		else if (ps_screenshot_format == 1) // TGA
		{
			xr_sprintf(Buf, sizeof(Buf), "ss_%s_%s_(%s).tga", Core.UserName, timestamp(TStamp), LvlName.c_str());
			auto Fs = FS.w_open("$screenshots$", Buf);
			if (Fs)
			{
				struct CallbackData
				{
					IWriter* fs;
				} cbData = {Fs};
				
				stbi_write_tga_to_func
				(
					[](void* context, void* data, int size)
					{
						CallbackData* cb = (CallbackData*)context;
						cb->fs->w(data, size);
					},
					&cbData, W, H, 4, SrcImg->pixels
				);
				FS.w_close(Fs);
				Hr = S_OK;
			}
			else
			{
				Hr = E_FAIL;
			}
		}
		else // ps_screenshot_format == 2, PNG
		{
			xr_sprintf(Buf, sizeof(Buf), "ss_%s_%s_(%s).png", Core.UserName, timestamp(TStamp), LvlName.c_str());
			auto Fs = FS.w_open("$screenshots$", Buf);
			if (Fs)
			{
				struct CallbackData
				{
					IWriter* fs;
				} cbData = {Fs};
				
				stbi_write_png_to_func
				(
					[](void* context, void* data, int size) 
					{
						CallbackData* cb = (CallbackData*)context;
						cb->fs->w(data, size);
					},
					&cbData, W, H, 4, SrcImg->pixels, W * 4
				);
				FS.w_close(Fs);
				Hr = S_OK;
			}
			else
			{
				Hr = E_FAIL;
			}
		}
		
		if (FAILED(Hr))
		{
			return false;
		}
#endif
	}
	break;

	case IRender_interface::SM_FOR_LEVELMAP:
	case IRender_interface::SM_FOR_CUBEMAP:
	{
		ScratchImage Small;
		Hr = Resize(*Img.GetImage(0,0,0), Device.TargetHeight, Device.TargetHeight, TEX_FILTER_FLAGS::TEX_FILTER_LINEAR, Small);
		if (FAILED(Hr))
		{
			return false;
		}

#ifdef IXR_WINDOWS
		Hr = SaveToTGAMemory(*Small.GetImage(0,0,0), TGA_FLAGS::TGA_FLAGS_NONE, Saved);
		if (FAILED(Hr))
		{
			return false;
		}
		
		string_path Buf;
		VERIFY(Name);
		xr_strconcat(Buf, Name, ".tga");
		auto Fs = FS.w_open("$screenshots$", Buf);
		R_ASSERT(Fs);
		Fs->w(Saved.GetBufferPointer(), (u32)Saved.GetBufferSize());
		FS.w_close(Fs);
#else
		// Linux: save as TGA directly to file
		const Image* SrcImg = Small.GetImage(0,0,0);
		string_path TgaBuf;
		VERIFY(Name);
		xr_strconcat(TgaBuf, Name, ".tga");
		
		auto Fs = FS.w_open("$screenshots$", TgaBuf);
		if (Fs)
		{
			struct CallbackData
			{
				IWriter* fs;
			} cbData = {Fs};
			
			stbi_write_tga_to_func(
				[](void* context, void* data, int size) {
					CallbackData* cb = (CallbackData*)context;
					cb->fs->w(data, size);
				},
				&cbData, SrcImg->width, SrcImg->height, 4, SrcImg->pixels
			);
			FS.w_close(Fs);
			Hr = S_OK;
		}
		else
		{
			Hr = E_FAIL;
		}
		
		if (FAILED(Hr))
		{
			return false;
		}
#endif
	}
	break;
	}

	return true;
}