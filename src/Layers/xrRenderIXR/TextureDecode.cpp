/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "TextureDecode.h"
#include "GpuResource.h"
#include "GpuTextures.h"

// The engine defines Device as a macro; NRI and NriDevice use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

#include <NRI.h>
#include <Extensions/NRIHelper.h>
#include <Extensions/NRIStreamer.h>

namespace
{
constexpr u32 UploadGap = 4; // frames without an upload step after which nothing is queued
constexpr u32 UploadBudget = 16 << 20; // bytes of textures a frame makes

struct Waiting
{
	TextureDecode::Decoded* Item;
	xr_atomic_bool* Done;
};

nri::CoreInterface Api;
nri::StreamerInterface StreamApi;
bool HasApi = false;
xrCriticalSection Guard;
xr_vector<Waiting> Pending;
u32 UploadFrame = u32(0) - UploadGap;

void LoadApi()
{
	if (!HasApi)
	{
		nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::CoreInterface), &Api);
		nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::StreamerInterface), &StreamApi);
		HasApi = true;
	}
}
} // namespace

bool TextureDecode::Read(const char* Name, Decoded& Out)
{
	PROF_EVENT("TextureWholeRead");
	Out.Bytes = 0;

	if (!GpuTextures::ReadWhole(Name, Out.Levels, Out.Format, Out.Width, Out.Height, Out.BlockBytes))
	{
		Out.Levels.clear();
		return false;
	}

	for (const xr_vector<u8>& Level : Out.Levels)
	{
		Out.Bytes += u32(Level.size());
	}

	Out.Mips = u32(Out.Levels.size());
	return true;
}

void TextureDecode::Queue(Decoded& Item, xr_atomic_bool& Done)
{
	xrCriticalSectionGuard Lock(Guard);
	Pending.push_back({ &Item, &Done });
}

bool TextureDecode::IsUploading()
{
	return DevicePtr->dwFrame - UploadFrame < UploadGap;
}

void TextureDecode::Upload(nri::CommandBuffer& Commands, nri::Streamer& Streamer)
{
	LoadApi();
	UploadFrame = DevicePtr->dwFrame;

	// What a frame takes: as many of the queued textures as fit a budget of bytes, and always one. One a frame
	// left a level drawn with placeholders for seconds when hundreds were asked for at once (r_msaa turned on)
	for (u32 Sent = 0;;)
	{
		Waiting Next = {};
		{
			xrCriticalSectionGuard Lock(Guard);

			if (!Pending.empty() && (!Sent || Sent + Pending.front().Item->Bytes <= UploadBudget))
			{
				Next = Pending.front();
				Pending.erase(Pending.begin());
			}
		}

		if (!Next.Item)
		{
			return;
		}

		PROF_EVENT("TextureWholeUpload");
		Decoded& Item = *Next.Item;
		Sent += Item.Bytes;
		nri::TextureDesc Desc = {};
		Desc.type = nri::TextureType::TEXTURE_2D;
		Desc.usage = nri::TextureUsageBits::SHADER_RESOURCE;
		Desc.format = nri::nriConvertDXGIFormatToNRI(Item.Format);
		Desc.width = nri::Dim_t(Item.Width);
		Desc.height = nri::Dim_t(Item.Height);
		Desc.depth = 1;
		Desc.mipNum = nri::Dim_t(Item.Levels.size());
		Desc.layerNum = 1;
		Desc.sampleNum = 1;

		if (Api.CreateCommittedTexture(*GRHI->Nri->Device, nri::MemoryLocation::DEVICE, 0.0f, Desc, Item.Texture) != nri::Result::SUCCESS)
		{
			Item.Texture = nullptr;
			Next.Done->store(true, std::memory_order_release);
			continue;
		}

		Api.SetDebugName(Item.Texture, "Texture whole copy");

		nri::TextureBarrierDesc Barrier = {};
		Barrier.texture = Item.Texture;
		Barrier.after = { nri::AccessBits::COPY_DESTINATION, nri::Layout::COPY_DESTINATION, nri::StageBits::COPY };
		Barrier.mipNum = nri::REMAINING;
		Barrier.layerNum = nri::REMAINING;
		nri::BarrierDesc Barriers = {};
		Barriers.textures = &Barrier;
		Barriers.textureNum = 1;
		Api.CmdBarrier(Commands, Barriers);

		const nri::StreamerCopyBatch Batch = StreamApi.BeginStreamerCopyBatch(Streamer);

		for (u32 Mip = 0; Mip < Item.Levels.size(); ++Mip)
		{
			const u32 Wide = std::max(Item.Width >> Mip, 4u);
			const u32 High = std::max(Item.Height >> Mip, 4u);
			nri::StreamTextureDataDesc Data = {};
			Data.data = Item.Levels[Mip].data();
			Data.dataRowPitch = Wide / 4 * Item.BlockBytes;
			Data.dataSlicePitch = u32(Item.Levels[Mip].size());
			Data.copyBatch = Batch;
			Data.dstTexture = Item.Texture;
			Data.dstRegion = { 0, 0, 0, nri::Dim_t(Wide), nri::Dim_t(High), 1, nri::Dim_t(Mip), 0, nri::PlaneBits::ALL };
			StreamApi.StreamTextureData(Streamer, Data);
		}

		StreamApi.CmdCopyStreamedData(Commands, Streamer, Batch);

		Barrier.before = Barrier.after;
		Barrier.after = GpuResource::ReadState();
		Api.CmdBarrier(Commands, Barriers);

		Item.Levels.clear();
		Next.Done->store(true, std::memory_order_release);
	}
}

void TextureDecode::Create(Decoded& In, GpuResource& Out, u32& OutSize)
{
	Out.Take(*std::exchange(In.Texture, nullptr));
	OutSize = In.Bytes;
}

void TextureDecode::Drop(Decoded& In)
{
	{
		xrCriticalSectionGuard Lock(Guard);
		std::erase_if(Pending, [&](const Waiting& Item) { return Item.Item == &In; });
	}

	GRHI->Nri->Retire(std::exchange(In.Texture, nullptr));
	In.Levels.clear();
}
