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
#include "GpuResource.h"
#include "NriPass.h"
#include "TextureDecode.h"

// The engine defines Device as a macro; NRI, NriDevice and DirectXTex use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

#include <DirectXTex.h>
#include <NRI.h>
#include <Extensions/NRIHelper.h>

void ResolveTextureFile(const char* name, string_path& filename, string_path& path);
int get_texture_load_lod(const char* fn, size_t& w, size_t& h, int Floor);

struct GpuResource::Waiting
{
	xr_vector<u8> Bytes;
	xr_vector<nri::TextureSubresourceUploadDesc> Levels;
};

namespace
{
const nri::CoreInterface& Api()
{
	return GRHI->Nri->CoreApi();
}

u32 Align(u32 Value, u32 Alignment)
{
	return (Value + Alignment - 1) / Alignment * Alignment;
}

u32 LevelCount(const nri::TextureDesc& Desc)
{
	return u32(Desc.mipNum) * Desc.layerNum;
}
} // namespace

GpuResource::GpuResource(GpuResource&& Other) noexcept
	: Texture(std::exchange(Other.Texture, nullptr)), View(std::exchange(Other.View, nullptr)), State(std::exchange(Other.State, EState::Undefined)), IsCube(Other.IsCube),
	Late(Other.Late.exchange(nullptr))
{
}

GpuResource& GpuResource::operator=(GpuResource&& Other) noexcept
{
	if (this != &Other)
	{
		Reset();
		Texture = std::exchange(Other.Texture, nullptr);
		View = std::exchange(Other.View, nullptr);
		State = std::exchange(Other.State, EState::Undefined);
		IsCube = Other.IsCube;
		Late.store(Other.Late.exchange(nullptr));
	}

	return *this;
}

GpuResource::~GpuResource()
{
	Reset();
}

nri::AccessLayoutStage GpuResource::ReadState()
{
	return { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::ALL };
}

void GpuResource::Reset()
{
	Waiting* Item = Late.exchange(nullptr);
	xr_delete(Item);

	if (Texture && GRHI->Nri)
	{
		GRHI->Nri->Retire(View);
		GRHI->Nri->Retire(Texture);
	}

	Texture = nullptr;
	View = nullptr;
	State = EState::Undefined;
	IsCube = false;
}

bool GpuResource::Create(const nri::TextureDesc& Desc, const nri::TextureSubresourceUploadDesc* Levels, bool IsCubeMap, const char* Name)
{
	Reset();

	if (Desc.format == nri::Format::UNKNOWN || Api().CreateCommittedTexture(*GRHI->Nri->Device, nri::MemoryLocation::DEVICE, 0.0f, Desc, Texture) != nri::Result::SUCCESS)
	{
		Texture = nullptr;
		Msg("! GPU texture '%s' could not be made: %ux%ux%u, %u layers, %u mips, format %u", Name, u32(Desc.width), u32(Desc.height), u32(Desc.depth), u32(Desc.layerNum), u32(Desc.mipNum), u32(Desc.format));
		return false;
	}

	Api().SetDebugName(Texture, Name);
	IsCube = IsCubeMap;

	if (!MakeView() || !Fill(Levels))
	{
		Msg("! GPU texture '%s' could not be filled", Name);
		Reset();
		return false;
	}

	return true;
}

bool GpuResource::Create(const RHITextureDesc& Source, const RHISubResource* Levels, const char* Name)
{
	nri::TextureDesc Desc = {};
	Desc.type = Source.Depth > 1 ? nri::TextureType::TEXTURE_3D : nri::TextureType::TEXTURE_2D;
	Desc.usage = nri::TextureUsageBits::SHADER_RESOURCE;
	Desc.format = nri::nriConvertDXGIFormatToNRI(u32(Source.Format));
	Desc.width = nri::Dim_t(Source.Width);
	Desc.height = nri::Dim_t(Source.Height);
	Desc.depth = nri::Dim_t(Source.Depth);
	Desc.mipNum = nri::Dim_t(Source.MipLevels);
	Desc.layerNum = nri::Dim_t(Source.ArraySize);
	Desc.sampleNum = 1;

	const u32 Texel = Desc.format == nri::Format::UNKNOWN ? 0 : nri::nriGetFormatProps(Desc.format)->stride;
	xr_vector<nri::TextureSubresourceUploadDesc> Parts;

	for (u32 Index = 0; Index < Source.MipLevels * Source.ArraySize; ++Index)
	{
		const RHISubResource& Level = Levels[Index];
		const u32 Mip = Index % Source.MipLevels;
		const u32 RowPitch = Level.RowPitch ? Level.RowPitch : Level.DataSize ? Level.DataSize : std::max(Source.Width >> Mip, 1u) * Texel;
		const u32 SlicePitch = Level.DepthPitch ? Level.DepthPitch : RowPitch * std::max(Source.Height >> Mip, 1u);
		Parts.push_back({ Level.Data, Source.Depth > 1 ? std::max(Source.Depth >> Mip, 1u) : 1u, RowPitch, SlicePitch });
	}

	return Create(Desc, Parts.data(), (Source.MiscFlags & u32(ERHI_RESOURCE_MISC_FLAG::TEXTURECUBE)) != 0, Name);
}

bool GpuResource::Load(const char* Name, u32& Bytes)
{
	Bytes = 0;
	string_path File = {};
	string_path Path = {};
	ResolveTextureFile(Name, File, Path);
	IReader* Reader = FS.r_open(Path);

	if (!Reader)
	{
		Msg("! texture_load: r_open failed for '%s' (resolved '%s')", File, Path);
		return false;
	}

	const u8* Data = static_cast<const u8*>(Reader->pointer());
	const size_t Length = size_t(Reader->length());
	DirectX::TexMetadata Info = {};
	DirectX::ScratchImage Image;
	HRESULT Result = DirectX::GetMetadataFromDDSMemory(Data, Length, DirectX::DDS_FLAGS_NONE, Info);
	const bool IsSampled = SUCCEEDED(Result) && (Api().GetFormatSupport(*GRHI->Nri->Device, nri::nriConvertDXGIFormatToNRI(u32(Info.format))) & nri::FormatSupportBits::TEXTURE);

	// A 16-bit format the device cannot sample is widened as it is read
	if (SUCCEEDED(Result) && !IsSampled)
	{
		Msg("! TEXTURE FORMAT ERROR: %s, format %u", File, u32(Info.format));
	}

	if (SUCCEEDED(Result))
	{
		Result = DirectX::LoadFromDDSMemory(Data, Length, IsSampled ? DirectX::DDS_FLAGS_NONE : DirectX::DDS_FLAGS_NO_16BPP, &Info, Image);
	}

	FS.r_close(Reader);

	// What NRI has no format for among the loader's readings of legacy files (B8G8R8X8, A8) is read as RGBA8
	if (SUCCEEDED(Result) && nri::nriConvertDXGIFormatToNRI(u32(Info.format)) == nri::Format::UNKNOWN && !DirectX::IsCompressed(Info.format))
	{
		DirectX::ScratchImage Wide;
		Result = DirectX::Convert(Image.GetImages(), Image.GetImageCount(), Info, DXGI_FORMAT_R8G8B8A8_UNORM, DirectX::TEX_FILTER_DEFAULT, DirectX::TEX_THRESHOLD_DEFAULT, Wide);

		if (SUCCEEDED(Result))
		{
			Image = std::move(Wide);
			Info = Image.GetMetadata();
		}
	}

	if (FAILED(Result) || std::max({ Info.width, Info.height, Info.depth, Info.arraySize, Info.mipLevels }) > UINT16_MAX)
	{
		Msg("! TEXTURE CREATION ERROR: %s (0x%08X)", File, u32(Result));
		return false;
	}

	size_t First = 0;

	if (!Info.IsCubemap() && !Info.IsVolumemap())
	{
		size_t Width = Info.width;
		size_t Height = Info.height;

		for (int Lod = get_texture_load_lod(Path, Width, Height, psTextureLOD); Lod > 0 && Info.mipLevels - First > 1 && (Info.width >> First) > 4 && (Info.height >> First) > 4; --Lod)
		{
			++First;
		}
	}

	const bool IsVolume = Info.IsVolumemap();
	nri::TextureDesc Desc = {};
	Desc.type = IsVolume ? nri::TextureType::TEXTURE_3D : nri::TextureType::TEXTURE_2D;
	Desc.usage = nri::TextureUsageBits::SHADER_RESOURCE;
	Desc.format = nri::nriConvertDXGIFormatToNRI(u32(Info.format));
	Desc.width = nri::Dim_t(Info.width >> First);
	Desc.height = nri::Dim_t(Info.height >> First);
	Desc.depth = nri::Dim_t(Info.depth);
	Desc.mipNum = nri::Dim_t(Info.mipLevels - First);
	Desc.layerNum = nri::Dim_t(Info.arraySize);
	Desc.sampleNum = 1;

	xr_vector<nri::TextureSubresourceUploadDesc> Levels;
	Levels.reserve(Info.arraySize * Desc.mipNum);

	for (size_t Layer = 0; Layer < Info.arraySize; ++Layer)
	{
		for (size_t Mip = First; Mip < Info.mipLevels; ++Mip)
		{
			const DirectX::Image* Level = Image.GetImage(Mip, Layer, 0);

			if (!Level || Level->rowPitch > UINT32_MAX || Level->slicePitch > UINT32_MAX)
			{
				Msg("! TEXTURE CREATION ERROR: %s (no level %u of layer %u)", File, u32(Mip), u32(Layer));
				return false;
			}

			const u32 Slices = IsVolume ? u32(std::max<size_t>(Info.depth >> Mip, 1)) : 1u;
			Levels.push_back({ Level->pixels, Slices, u32(Level->rowPitch), u32(Level->slicePitch) });
			Bytes += u32(Level->slicePitch) * Slices;
		}
	}

	return Create(Desc, Levels.data(), Info.IsCubemap(), File);
}

void GpuResource::Take(nri::Texture& Made)
{
	Reset();
	Texture = &Made;
	State = EState::Read;

	if (!MakeView())
	{
		Reset();
	}
}

bool GpuResource::MakeView()
{
	const nri::TextureDesc& Desc = Api().GetTextureDesc(*Texture);
	nri::TextureViewDesc ViewDesc = {};
	ViewDesc.texture = Texture;
	ViewDesc.type = IsCube ? (Desc.layerNum > 6 ? nri::TextureView::TEXTURE_CUBE_ARRAY : nri::TextureView::TEXTURE_CUBE) :
		Desc.type == nri::TextureType::TEXTURE_2D && Desc.layerNum > 1 ? nri::TextureView::TEXTURE_ARRAY : nri::TextureView::TEXTURE;
	ViewDesc.mipNum = nri::REMAINING;
	ViewDesc.layerNum = nri::REMAINING;
	ViewDesc.sliceNum = nri::REMAINING;
	return Api().CreateTextureView(ViewDesc, View) == nri::Result::SUCCESS;
}

// A D3D12 level's frame records the copy in its own commands, before every later use of the texture; elsewhere the
// queue's upload helper copies it and waits. D3D11 copies on the context the primary thread draws with: another thread's
// texture waits for the primary's next Ready
bool GpuResource::Fill(const nri::TextureSubresourceUploadDesc* Levels)
{
	const bool IsPrimary = g_AppInfo.IsRenderThread();
	const bool IsD3D12 = GRHI->APILevel == ERHI_API_LAYER::D3D12;

	if (IsPrimary && IsD3D12 && TextureDecode::IsUploading())
	{
		return Record(Levels);
	}

	if (IsPrimary || IsD3D12)
	{
		const nri::TextureUploadDesc Upload = { Levels, Texture, ReadState(), nri::PlaneBits::ALL };
		State = GRHI->Nri->UploadData(&Upload, 1, nullptr, 0) ? EState::Read : EState::Undefined;
		return State == EState::Read;
	}

	Waiting* Item = new Waiting();
	const u32 Count = LevelCount(Api().GetTextureDesc(*Texture));
	size_t Total = 0;

	for (u32 Index = 0; Index < Count; ++Index)
	{
		Total += size_t(Levels[Index].slicePitch) * Levels[Index].sliceNum;
	}

	Item->Bytes.resize(Total);
	Item->Levels.assign(Levels, Levels + Count);
	u8* To = Item->Bytes.data();

	for (nri::TextureSubresourceUploadDesc& Level : Item->Levels)
	{
		const size_t Size = size_t(Level.slicePitch) * Level.sliceNum;
		std::memcpy(To, Level.slices, Size);
		Level.slices = To;
		To += Size;
	}

	Late.store(Item, std::memory_order_release);
	return true;
}

bool GpuResource::Ready()
{
	if (!Late.load(std::memory_order_acquire))
	{
		return true;
	}

	if (!g_AppInfo.IsRenderThread())
	{
		return false;
	}

	Waiting* Item = Late.exchange(nullptr, std::memory_order_acq_rel);
	const bool IsFilled = Item && Fill(Item->Levels.data());
	xr_delete(Item);
	return IsFilled;
}

bool GpuResource::Record(const nri::TextureSubresourceUploadDesc* Levels)
{
	nri::Device& Gpu = *GRHI->Nri->Device;
	const nri::TextureDesc& Desc = Api().GetTextureDesc(*Texture);
	const u32 RowAlignment = Api().GetDeviceDesc(Gpu).memoryAlignment.uploadBufferTextureRow;
	const u32 PlaceAlignment = Api().GetDeviceDesc(Gpu).memoryAlignment.uploadBufferTextureSlice;
	const u32 Count = LevelCount(Desc);
	xr_vector<nri::TextureDataLayoutDesc> Layouts(Count);
	u64 Size = 0;

	// D3D12 takes the slices of a volume level one after another at the row pitch times the rows
	for (u32 Index = 0; Index < Count; ++Index)
	{
		const nri::TextureSubresourceUploadDesc& Level = Levels[Index];
		const u32 RowPitch = Align(Level.rowPitch, RowAlignment);
		const u64 Offset = (Size + PlaceAlignment - 1) / PlaceAlignment * PlaceAlignment;
		Layouts[Index] = { Offset, RowPitch, Level.slicePitch / Level.rowPitch * RowPitch };
		Size = Offset + u64(Layouts[Index].slicePitch) * Level.sliceNum;
	}

	nri::BufferDesc StagingDesc = {};
	StagingDesc.size = Size;
	nri::Buffer* Staging = nullptr;

	if (Api().CreateCommittedBuffer(Gpu, nri::MemoryLocation::HOST_UPLOAD, 0.0f, StagingDesc, Staging) != nri::Result::SUCCESS)
	{
		return false;
	}

	u8* Mapped = static_cast<u8*>(Api().MapBuffer(*Staging, 0, Size));

	for (u32 Index = 0; Index < Count; ++Index)
	{
		const nri::TextureSubresourceUploadDesc& Level = Levels[Index];
		const nri::TextureDataLayoutDesc& Layout = Layouts[Index];
		const u32 Rows = Level.slicePitch / Level.rowPitch;

		for (u32 Slice = 0; Slice < Level.sliceNum; ++Slice)
		{
			for (u32 Row = 0; Row < Rows; ++Row)
			{
				std::memcpy(Mapped + Layout.offset + u64(Slice) * Layout.slicePitch + u64(Row) * Layout.rowPitch, static_cast<const u8*>(Level.slices) + size_t(Slice) * Level.slicePitch + size_t(Row) * Level.rowPitch, Level.rowPitch);
			}
		}
	}

	Api().UnmapBuffer(*Staging);

	nri::TextureBarrierDesc Barrier = {};
	Barrier.texture = Texture;
	Barrier.after = { nri::AccessBits::COPY_DESTINATION, nri::Layout::COPY_DESTINATION, nri::StageBits::COPY };
	Barrier.mipNum = nri::REMAINING;
	Barrier.layerNum = nri::REMAINING;
	nri::BarrierDesc Barriers = {};
	Barriers.textures = &Barrier;
	Barriers.textureNum = 1;

	nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, nullptr);
	Api().CmdBarrier(Commands, Barriers);

	for (u32 Index = 0; Index < Count; ++Index)
	{
		nri::TextureRegionDesc Region = {};
		Region.mipOffset = nri::Dim_t(Index % Desc.mipNum);
		Region.layerOffset = nri::Dim_t(Index / Desc.mipNum);
		Api().CmdUploadBufferToTexture(Commands, *Texture, Region, *Staging, Layouts[Index]);
	}

	Barrier.before = Barrier.after;
	Barrier.after = ReadState();
	Api().CmdBarrier(Commands, Barriers);
	EndNriCommands(&Commands);
	GRHI->Nri->Retire(Staging);
	State = EState::Read;
	return true;
}

IRHISurface* GpuResource::Adopt() const
{
	if (!Texture)
	{
		return nullptr;
	}

	RHITextureDesc Desc;
	Desc.Adopt = reinterpret_cast<void*>(Api().GetTextureNativeObject(Texture));
	Desc.Format = ERHI_FORMAT(nri::nriConvertNRIFormatToDXGI(Api().GetTextureDesc(*Texture).format));
	Desc.MiscFlags = IsCube ? u32(ERHI_RESOURCE_MISC_FLAG::TEXTURECUBE) : 0u;
	return GRHI->CreateTextureFromMemory(nullptr, 0, Desc);
}
