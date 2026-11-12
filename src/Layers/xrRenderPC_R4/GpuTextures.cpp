#include "stdafx.h"
#include "GpuTextures.h"

#include "NriPass.h"

// The engine defines Device as a macro; NRI and NriDevice use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

#include <bit>

#include <NRI.h>
#include <Extensions/NRIHelper.h>
#include <Extensions/NRIWrapperD3D11.h>
#include <Extensions/NRIWrapperD3D12.h>

namespace
{
nri::CoreInterface Api;
nri::HelperInterface Help;

constexpr u32 TailSize = 512;
constexpr u32 TailMips = 8;
constexpr u32 MaxSize = 4096;

constexpr u32 FormatBC1 = 71;
constexpr u32 FormatBC1Srgb = 72;
constexpr u32 FormatBC3 = 77;
constexpr u32 FormatBC3Srgb = 78;
constexpr u32 FormatBC7 = 98;
constexpr u32 FormatBC7Srgb = 99;

constexpr u32 FourCCDxt1 = 0x31545844;
constexpr u32 FourCCDxt5 = 0x35545844;
constexpr u32 FourCCDx10 = 0x30315844;

struct Source
{
	IReader* File = nullptr;
	const u8* Data = nullptr;
	u32 Format = 0;
	u32 Width = 0;
	u32 Height = 0;
	u32 BlockBytes = 0;
	u32 LastLevel = 0;
	u32 Tail = 0;
	u32 Head = 0;
};

u32 Log2(u32 Value)
{
	return u32(std::bit_width(Value)) - 1;
}

u32 LevelBlocks(u32 Side, u32 Level)
{
	return std::max(Side >> Level, 4u) / 4;
}

size_t LevelOffset(const Source& Item, u32 Level)
{
	size_t Offset = 0;

	for (u32 Index = 0; Index < Level; ++Index)
	{
		Offset += size_t(LevelBlocks(Item.Width, Index)) * LevelBlocks(Item.Height, Index) * Item.BlockBytes;
	}

	return Offset;
}

u32 ReadWord(const u8* Data, u32 Offset)
{
	u32 Value;
	std::memcpy(&Value, Data + Offset, sizeof(Value));
	return Value;
}

// A block-compressed 2D texture with mips down to the tail, as a view of its file
bool OpenSource(const char* Name, Source& Item)
{
	string_path Path;

	if (!FS.exist(Path, "$level$", Name, ".dds") && !FS.exist(Path, "$game_saves$", Name, ".dds") && !FS.exist(Path, "$game_textures$", Name, ".dds"))
	{
		return false;
	}

	IReader* File = FS.r_open(Path);
	const u8* Data = File ? static_cast<const u8*>(File->pointer()) : nullptr;
	const size_t Length = File ? size_t(File->length()) : 0;
	bool IsValid = Length > 148 && ReadWord(Data, 0) == 0x20534444;
	size_t Header = 128;

	if (IsValid)
	{
		const u32 FourCC = ReadWord(Data, 84);
		const u32 Caps = ReadWord(Data, 112);
		Item.Height = ReadWord(Data, 12);
		Item.Width = ReadWord(Data, 16);
		Item.Format = FourCC == FourCCDxt1 ? FormatBC1 : FourCC == FourCCDxt5 ? FormatBC3 : 0;
		IsValid = (ReadWord(Data, 80) & 4) != 0 && (Caps & 0x20FE00) == 0;

		if (FourCC == FourCCDx10)
		{
			Header = 148;
			Item.Format = ReadWord(Data, 128);
			IsValid = IsValid && ReadWord(Data, 132) == 3 && (ReadWord(Data, 136) & 4) == 0 && ReadWord(Data, 140) <= 1;
		}
	}

	const u32 Formats[] = { FormatBC1, FormatBC1Srgb, FormatBC3, FormatBC3Srgb, FormatBC7, FormatBC7Srgb };
	const u32 Larger = std::max(Item.Width, Item.Height);
	const u32 Smaller = std::min(Item.Width, Item.Height);
	IsValid = IsValid && std::find(std::begin(Formats), std::end(Formats), Item.Format) != std::end(Formats);
	IsValid = IsValid && std::has_single_bit(Item.Width) && std::has_single_bit(Item.Height) && Smaller >= 4 && Larger <= MaxSize;

	if (IsValid)
	{
		const u32 Mips = std::max(ReadWord(Data, 28), 1u);
		const u32 First = Larger > TailSize ? Log2(Larger / TailSize) : 0;
		Item.BlockBytes = Item.Format == FormatBC1 || Item.Format == FormatBC1Srgb ? 8 : 16;
		Item.LastLevel = std::min(Mips - 1, Log2(Smaller) - 2);
		IsValid = Item.LastLevel >= First && Header + LevelOffset(Item, Item.LastLevel + 1) <= Length;
	}

	if (!IsValid)
	{
		if (File)
		{
			FS.r_close(File);
		}

		return false;
	}

	Item.File = File;
	Item.Data = Data + Header;
	return true;
}

// One mip of a square slice: the blocks of a source mip, repeated until they fill it
nri::TextureSubresourceUploadDesc Tile(const Source& Item, u32 Level, u32 Size, xr_vector<xr_vector<u8>>& Scratch)
{
	Level = std::min(Level, Item.LastLevel);
	const u32 Columns = LevelBlocks(Item.Width, Level);
	const u32 Rows = LevelBlocks(Item.Height, Level);
	const u32 Blocks = Size / 4;
	const u32 RowBytes = Blocks * Item.BlockBytes;
	const u8* From = Item.Data + LevelOffset(Item, Level);
	nri::TextureSubresourceUploadDesc Desc = { From, 1, RowBytes, RowBytes * Blocks };

	if (Columns == Blocks && Rows == Blocks)
	{
		return Desc;
	}

	xr_vector<u8>& To = Scratch.emplace_back(size_t(RowBytes) * Blocks);

	for (u32 Row = 0; Row < Blocks; ++Row)
	{
		const u8* Line = From + size_t(Row % Rows) * Columns * Item.BlockBytes;

		for (u32 Column = 0; Column < Blocks; Column += Columns)
		{
			std::memcpy(&To[(size_t(Row) * Blocks + Column) * Item.BlockBytes], Line, size_t(std::min(Columns, Blocks - Column)) * Item.BlockBytes);
		}
	}

	Desc.slices = To.data();
	return Desc;
}
} // namespace

nri::Texture* WrapNriSurface(IRHISurface* Surface, u32 Format)
{
	nri::Texture* Texture = nullptr;

	if (GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		nri::WrapperD3D12Interface Wrapper;
		nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::WrapperD3D12Interface), &Wrapper);
		nri::TextureD3D12Desc Desc = {};
		Desc.d3d12Resource = static_cast<ID3D12Resource*>(Surface->GetRawTexture());
		Desc.format = s32(Format ? Format : u32(Surface->GetFormat()));
		Wrapper.CreateTextureD3D12(*GRHI->Nri->Device, Desc, Texture);
	}
	else
	{
		nri::WrapperD3D11Interface Wrapper;
		nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::WrapperD3D11Interface), &Wrapper);
		nri::TextureD3D11Desc Desc = {};
		Desc.d3d11Resource = static_cast<ID3D11Resource*>(Surface->GetRawTexture());
		Desc.format = s32(Format ? Format : u32(Surface->GetFormat()));
		Wrapper.CreateTextureD3D11(*GRHI->Nri->Device, Desc, Texture);
	}

	return Texture;
}

u32 GpuTextures::Place(u32 Format, u32 Size, u32 Mips)
{
	u32 Slot = 0;

	while (Slot < BucketCount && (Buckets[Slot].Format != Format || Buckets[Slot].Size != Size))
	{
		++Slot;
	}

	if (Slot == MaxBuckets || Buckets[Slot].Slices == 0xFFFF)
	{
		return Absent;
	}

	if (Slot == BucketCount)
	{
		Buckets[BucketCount++] = { nullptr, nullptr, Format, Size, Mips, 0 };
	}

	return (Slot << 16) | Buckets[Slot].Slices++;
}

void GpuTextures::Build(const xr_vector<CTexture*>& Textures, xr_vector<Ref>& Refs)
{
	nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::CoreInterface), &Api);
	nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::HelperInterface), &Help);
	Refs.assign(Textures.size(), Ref{});

	xr_vector<Source> Sources;
	u32 Heads = 0;

	for (size_t Index = 0; Index < Textures.size(); ++Index)
	{
		Source Item;

		if (!OpenSource(Textures[Index]->cName.c_str(), Item))
		{
			continue;
		}

		const u32 Larger = std::max(Item.Width, Item.Height);
		const u32 HeadMips = Larger > TailSize ? Log2(Larger / TailSize) : 0;
		const u32 Base = std::max(Larger, TailSize);
		Item.Tail = Place(Item.Format, TailSize, TailMips);
		Item.Head = Item.Tail != Absent && HeadMips ? Place(Item.Format, Larger, HeadMips) : Absent;

		if (Item.Tail == Absent || (HeadMips && Item.Head == Absent))
		{
			FS.r_close(Item.File);
			continue;
		}

		Refs[Index] = { Item.Tail, Item.Head, Log2(Base / Item.Width) | (Log2(Base / Item.Height) << 4) | (HeadMips << 8) };
		Heads += HeadMips != 0;
		Sources.push_back(Item);
	}

	u64 Bytes = 0;

	for (u32 Slot = 0; Slot < BucketCount; ++Slot)
	{
		Bucket& Item = Buckets[Slot];
		nri::TextureDesc Desc = {};
		Desc.type = nri::TextureType::TEXTURE_2D;
		Desc.usage = nri::TextureUsageBits::SHADER_RESOURCE;
		Desc.format = nri::nriConvertDXGIFormatToNRI(Item.Format);
		Desc.width = nri::Dim_t(Item.Size);
		Desc.height = nri::Dim_t(Item.Size);
		Desc.depth = 1;
		Desc.mipNum = nri::Dim_t(Item.Mips);
		Desc.layerNum = nri::Dim_t(Item.Slices);
		Desc.sampleNum = 1;
		Api.CreateCommittedTexture(*GRHI->Nri->Device, nri::MemoryLocation::DEVICE, 0.0f, Desc, Item.Texture);
		Api.SetDebugName(Item.Texture, Item.Size == TailSize ? "Scene texture tails" : "Scene texture heads");

		xr_vector<nri::TextureSubresourceUploadDesc> Parts(size_t(Item.Slices) * Item.Mips);
		xr_vector<xr_vector<u8>> Scratch;

		for (const Source& From : Sources)
		{
			const bool IsTail = (From.Tail >> 16) == Slot;

			if (!IsTail && (From.Head == Absent || (From.Head >> 16) != Slot))
			{
				continue;
			}

			const u32 Slice = (IsTail ? From.Tail : From.Head) & 0xFFFF;
			const u32 First = IsTail && From.Head != Absent ? Buckets[From.Head >> 16].Mips : 0;

			for (u32 Mip = 0; Mip < Item.Mips; ++Mip)
			{
				Parts[size_t(Slice) * Item.Mips + Mip] = Tile(From, First + Mip, Item.Size >> Mip, Scratch);
				Bytes += Parts[size_t(Slice) * Item.Mips + Mip].slicePitch;
			}
		}

		nri::TextureUploadDesc Upload = {};
		Upload.subresources = Parts.data();
		Upload.texture = Item.Texture;
		Upload.after = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::FRAGMENT_SHADER };
		Upload.planes = nri::PlaneBits::ALL;
		Help.UploadData(*GRHI->Nri->Queue, &Upload, 1, nullptr, 0);

		nri::TextureViewDesc View = {};
		View.texture = Item.Texture;
		View.type = nri::TextureView::TEXTURE_ARRAY;
		View.mipNum = nri::REMAINING;
		View.layerNum = nri::REMAINING;
		Api.CreateTextureView(View, Item.View);
		Msg("* [scene] texture bucket %u: %u, format %u, %u mips, %u slices", Slot, Item.Size, Item.Format, Item.Mips, Item.Slices);
	}

	for (Source& Item : Sources)
	{
		FS.r_close(Item.File);
	}

	Msg("* [scene] texture buckets: %u textures of %u in %u buckets, %u with heads, %llu MB", u32(Sources.size()), u32(Textures.size()), BucketCount, Heads, Bytes >> 20);
}

void GpuTextures::Destroy()
{
	for (u32 Slot = 0; Slot < BucketCount; ++Slot)
	{
		Api.DestroyDescriptor(Buckets[Slot].View);
		Api.DestroyTexture(Buckets[Slot].Texture);
		Buckets[Slot] = {};
	}

	BucketCount = 0;
}

nri::Descriptor* GpuTextures::View(u32 Bucket) const
{
	return Buckets[Bucket < BucketCount ? Bucket : 0].View;
}
