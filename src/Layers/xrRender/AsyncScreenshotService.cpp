#include "stdafx.h"
#include "AsyncScreenshotService.h"

#include "../../xrEngine/string_table.h"
#include "../../xrRHI/RHI.h"
#include <DirectXTex.h>

#ifdef IXR_WINDOWS
#include <wincodec.h>
#include "../../xrCore/Platform/Windows/ComInitialization.h"
#else
#include <stb/stb_image_write.h>
#endif

using namespace DirectX;

class AsyncScreenshotService::State
{
	struct Job
	{
		xr_unique_ptr<IRHIReadback> Readback;
		xr_vector<u8> Pixels;
		xr_string Path;
		ERHI_FORMAT Format = {};
		u32 Width = 0;
		u32 Height = 0;
		u32 OutputWidth = 0;
		u32 OutputHeight = 0;
		u32 ImageFormat = 0;
		bool IsThumbnail = false;
		bool IsSuccessful = false;
	};

	xr_vector<xr_unique_ptr<Job>> Pending;
	xr_unique_ptr<Job> Writing;
	xr_task_group Tasks;
	xr_atomic_bool IsComplete = false;
	u32 Sequence = 0;

	static bool Encode(Job& Capture)
	{
#ifdef IXR_WINDOWS
		Platform::ComInitialization Com;
		if (!Com.IsReady())
		{
			return false;
		}
#endif
		ScratchImage Image;
		if (FAILED(Image.Initialize2D(static_cast<DXGI_FORMAT>(Capture.Format), Capture.Width, Capture.Height, 1, 1)))
		{
			return false;
		}
		for (u32 Y = 0; Y < Capture.Height; ++Y)
		{
			memcpy(Image.GetPixels() + size_t(Y) * Image.GetImage(0, 0, 0)->rowPitch, Capture.Pixels.data() + size_t(Y) * Capture.Width * 4, size_t(Capture.Width) * 4);
		}
		ScratchImage Small;
		const auto* Pixels = Image.GetImage(0, 0, 0);
		if (Capture.IsThumbnail)
		{
			if (FAILED(Resize(*Pixels, Capture.OutputWidth, Capture.OutputHeight, TEX_FILTER_DEFAULT, Small)))
			{
				return false;
			}
			Pixels = Small.GetImage(0, 0, 0);
		}
#ifdef IXR_WINDOWS
		Blob Encoded;
		HRESULT Result;
		if (Capture.IsThumbnail)
		{
			Result = SaveToDDSMemory(*Pixels, DDS_FLAGS_NONE, Encoded);
		}
		else if (Capture.ImageFormat == 1)
		{
			Result = SaveToTGAMemory(*Pixels, TGA_FLAGS_FORCE_SRGB, Encoded);
		}
		else
		{
			Result = SaveToWICMemory(*Pixels, WIC_FLAGS_FORCE_SRGB, Capture.ImageFormat == 0 ? GUID_ContainerFormatJpeg : GUID_ContainerFormatPng, Encoded);
		}
		if (FAILED(Result))
		{
			return false;
		}
		auto File = FS.wg_open(Capture.Path.c_str());
		if (!File || !File->valid())
		{
			return false;
		}
		File->w(Encoded.GetBufferPointer(), static_cast<u32>(Encoded.GetBufferSize()));
		return true;
#else
		// stb expects RGBA, while the game targets are BGRA.
		for (size_t Y = 0; Y < Pixels->height; ++Y)
		{
			for (size_t X = 0; X < Pixels->width; ++X)
			{
				auto* Pixel = Pixels->pixels + Y * Pixels->rowPitch + X * 4;
				std::swap(Pixel[0], Pixel[2]);
			}
		}
		auto File = FS.wg_open(Capture.Path.c_str());
		if (!File || !File->valid())
		{
			return false;
		}
		auto Write = [](void* Context, void* Data, int Size)
		{ static_cast<IWriter*>(Context)->w(Data, Size); };
		if (!Capture.IsThumbnail && Capture.ImageFormat == 0)
		{
			return stbi_write_jpg_to_func(Write, &*File, Pixels->width, Pixels->height, 4, Pixels->pixels, 90) != 0;
		}
		if (!Capture.IsThumbnail && Capture.ImageFormat == 1)
		{
			return stbi_write_tga_to_func(Write, &*File, Pixels->width, Pixels->height, 4, Pixels->pixels) != 0;
		}
		return stbi_write_png_to_func(Write, &*File, Pixels->width, Pixels->height, 4, Pixels->pixels, Pixels->rowPitch) != 0;
#endif
	}

public:
	bool Queue(IRender_interface::ScreenshotMode Mode, const char* Name)
	{
		if (Pending.size() + (Writing ? 1 : 0) >= 8)
		{
			Msg("! Screenshot queue is full");
			return false;
		}
		auto Capture = xr_make_unique<Job>();
		Capture->IsThumbnail = Mode == IRender_interface::SM_FOR_GAMESAVE;
		Capture->ImageFormat = ps_screenshot_format;
		Capture->OutputWidth = EngineExternal().gamesaveSize.x;
		Capture->OutputHeight = EngineExternal().gamesaveSize.y;
		if (Capture->IsThumbnail)
		{
			if (!Name || !Name[0])
			{
				return false;
			}
			Capture->Path = Name;
		}
		else
		{
			string64 Time;
			string_path Filename, Path;
			const char* Level = g_pGameLevel ? g_pStringTable->translate(g_pGameLevel->name().c_str()).c_str() : "mainmenu";
			const char* Extension = Capture->ImageFormat == 0 ? "jpg" : Capture->ImageFormat == 1 ? "tga"
																								  : "png";
			xr_sprintf(Filename, "ss_%s_%s_(%s)_%u.%s", Core.UserName, timestamp(Time), Level, ++Sequence, Extension);
			FS.update_path(Path, "$screenshots$", Filename);
			Capture->Path = Path;
		}
		Capture->Readback = GRHI->DevicePtr->BeginRenderTargetReadback(RTarget);
		if (Capture->Readback)
		{
			Capture->Width = Capture->Readback->Width;
			Capture->Height = Capture->Readback->Height;
			Capture->Format = Capture->Readback->Format;
		}
		else
		{
			// D3D9 has no nonblocking staging readback; encoding and disk I/O still run off-thread.
			Capture->Width = RTarget->GetSurface()->GetWidth();
			Capture->Height = RTarget->GetSurface()->GetHeight();
			Capture->Format = RTarget->GetSurface()->GetFormat();
			const size_t BufferSize = size_t(Capture->Width) * Capture->Height * 4;
			if (BufferSize > u32(-1))
			{
				return false;
			}
			Capture->Pixels.resize(BufferSize);
			u32 Pitch = 0;
			if (!GRHI->DevicePtr->ReadRenderTargetPixels(RTarget, Capture->Pixels.data(), static_cast<u32>(BufferSize), Capture->Width, Capture->Height, Pitch) || Pitch != Capture->Width * 4)
			{
				return false;
			}
		}
		Pending.emplace_back(std::move(Capture));
		return true;
	}

	void Process(bool Wait = false)
	{
		do
		{
			if (Writing)
			{
				if (!Wait && !IsComplete.load())
				{
					return;
				}
				Tasks.wait();
				if (!Writing->IsSuccessful)
				{
					Msg("! Failed to save screenshot: %s", Writing->Path.c_str());
				}
				else if (Writing->IsThumbnail)
				{
					xr_string TextureName = Writing->Path.Split('\\').back();
					ref_texture Texture;
					Texture.create(TextureName.c_str());
					Texture->Unload();
					Texture->Load();
				}
				Writing.reset();
			}
			if (Pending.empty())
			{
				return;
			}
			auto& Capture = Pending.front();
			if (Capture->Readback)
			{
				const auto Status = Capture->Readback->ReadPixels(Capture->Pixels, Wait);
				if (Status == ERHIReadbackStatus::Pending)
				{
					return;
				}
				if (Status == ERHIReadbackStatus::Failed)
				{
					Msg("! Failed to read screenshot: %s", Capture->Path.c_str());
					Pending.erase(Pending.begin());
					continue;
				}
				Capture->Readback.reset();
			}
			Writing = std::move(Capture);
			Pending.erase(Pending.begin());
			IsComplete.store(false);
			const auto EncodeLambda = [this]()
			{
				try
				{
					Writing->IsSuccessful = Encode(*Writing);
				}
				catch (...)
				{
					Writing->IsSuccessful = false;
				}
				IsComplete.store(true);
			};
			Tasks.run(EncodeLambda);
		} while (Wait);
	}
};

AsyncScreenshotService::AsyncScreenshotService() : Data(xr_make_shared<State>())
{
}

AsyncScreenshotService::~AsyncScreenshotService()
{
	Data->Process(true);
}

bool AsyncScreenshotService::Queue(IRender_interface::ScreenshotMode Mode, const char* Name)
{
	return Data->Queue(Mode, Name);
}

void AsyncScreenshotService::Process(bool Wait)
{
	Data->Process(Wait);
}
