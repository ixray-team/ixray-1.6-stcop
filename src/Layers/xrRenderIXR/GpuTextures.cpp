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
#include "GpuTextures.h"

#include "NriPass.h"

// The engine defines Device as a macro; NRI and NriDevice use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

#include <bit>

#include <NRI.h>
#include <Extensions/NRIHelper.h>
#include <Extensions/NRIStreamer.h>
#include <Extensions/NRIWrapperD3D11.h>
#include <Extensions/NRIWrapperD3D12.h>

int get_texture_load_lod(const char* fn, size_t& w, size_t& h, int Floor);

namespace
{
// The levels r_streaming_quality left out when the buckets were laid out, -1 while there are none: a head or tail read
// later must have the sizes the slices and the materials' info words were made for, whatever the setting is now
int BuiltLod = -1;

nri::CoreInterface Api;
nri::HelperInterface Help;
nri::StreamerInterface StreamApi;

constexpr u32 TailSize = 512;
constexpr u32 TailMips = 8;
constexpr u32 MaxSize = 4096;
constexpr u32 MaxSlices = 2048;
constexpr u32 MaxStreams = 4095; // SCENE_TEXTURE_STREAMS - 1: an info word holds the index plus one in 12 bits
// Slices a tail bucket of the forward level frame keeps for the textures of the models made after the load (Add):
// 85 MB of BC3 tails, 43 MB of BC1
constexpr u32 SpareTails = 256;
constexpr u32 SeenFrames = 16; // feedback older than this is not a request
constexpr u32 KeepFrames = 120; // a head not asked for this long gives its place to any other
constexpr float HeatKept = 0.995f; // of a head's heat a frame later: half of it is left after 138 frames
constexpr float HeatAsked = 2.0f; // a head is not loaded for less: what flickers into view for a frame waits
constexpr float HeatTaken = 2.0f; // times the heat of a head it takes the place of, asked for or not, while that one is younger than KeepFrames
constexpr u32 StarvedFrames = 120; // a pool whose heads wait this long takes places from another
constexpr u32 ResizeGap = 600; // frames between two changes of the pools' sizes: each copies every slice a pool keeps
constexpr u32 RetiredFrames = 8; // an array is destroyed this long after the pool left it

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
	u32 FileLevels = 0; // the mips the file holds down to four texels, of which a flat one keeps the first
	size_t FileBytes = 0; // of mip data in the file
	u32 First = 0; // the source mip the tail starts at
	bool IsFlat = false; // its mips stop above the tail: the top level is kept whole, as a tail of one mip
	bool IsPlain = false; // one 2D texture, whatever its format and size: no cube, volume or array
	bool IsPlaceholder = false; // the file is missing: the engine's placeholder is read in its place
	const char* Why = ""; // it cannot go into buckets
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

// A sequence or a video: the engine plays it, and no file of its name is read
bool IsPlayed(const char* Name)
{
	string_path Path;
	return FS.exist(Path, "$game_textures$", Name, ".seq") || FS.exist(Path, "$game_textures$", Name, ".ogm") || FS.exist(Path, "$game_textures$", Name, ".avi");
}

// A block-compressed 2D texture with mips down to the tail, as a view of its file. A missing file is the placeholder the
// engine's loader reads in its place (texture_load)
bool OpenSource(const char* Name, Source& Item)
{
	string_path Path;
	const char* Missing = strstr(Name, "_bump#") ? "ed\\ed_dummy_bump#" : strstr(Name, "_bump") ? "ed\\ed_dummy_bump" : "ed\\ed_not_existing_texture";
	const bool IsFound = FS.exist(Path, "$level$", Name, ".dds") || FS.exist(Path, "$game_saves$", Name, ".dds") || FS.exist(Path, "$game_textures$", Name, ".dds");
	const bool IsOwn = !IsFound && (strchr(Name, '$') || IsPlayed(Name));

	if (!IsFound && (IsOwn || !FS.exist(Path, "$game_textures$", Missing, ".dds")))
	{
		Item.Why = IsOwn ? "a target, a sequence or a video" : "no file";
		return false;
	}

	Item.IsPlaceholder = !IsFound;
	IReader* File = FS.r_open(Path);
	const u8* Data = File ? static_cast<const u8*>(File->pointer()) : nullptr;
	const size_t Length = File ? size_t(File->length()) : 0;
	bool IsValid = Length > 148 && ReadWord(Data, 0) == 0x20534444;
	size_t Header = 128;

	if (IsValid)
	{
		const u32 FourCC = ReadWord(Data, 84);
		Item.Height = ReadWord(Data, 12);
		Item.Width = ReadWord(Data, 16);
		Item.Format = FourCC == FourCCDxt1 ? FormatBC1 : FourCC == FourCCDxt5 ? FormatBC3 : 0;
		Item.IsPlain = (ReadWord(Data, 112) & 0x20FE00) == 0;
		IsValid = (ReadWord(Data, 80) & 4) != 0;

		if (FourCC == FourCCDx10)
		{
			Header = 148;
			Item.Format = ReadWord(Data, 128);
			Item.IsPlain = Item.IsPlain && ReadWord(Data, 132) == 3 && (ReadWord(Data, 136) & 4) == 0 && ReadWord(Data, 140) <= 1;
		}

		IsValid = IsValid && Item.IsPlain;
	}

	const u32 Formats[] = { FormatBC1, FormatBC1Srgb, FormatBC3, FormatBC3Srgb, FormatBC7, FormatBC7Srgb };
	const u32 Larger = std::max(Item.Width, Item.Height);
	const u32 Smaller = std::min(Item.Width, Item.Height);
	Item.Why = !Item.IsPlain ? "a cube, a volume, an array or no texture file" : IsValid ? "" : "not block-compressed";
	IsValid = IsValid && std::find(std::begin(Formats), std::end(Formats), Item.Format) != std::end(Formats);
	Item.Why = IsValid || Item.Why[0] ? Item.Why : "format";
	IsValid = IsValid && std::has_single_bit(Item.Width) && std::has_single_bit(Item.Height) && Smaller >= 4 && Larger <= MaxSize;
	Item.Why = IsValid || Item.Why[0] ? Item.Why : "size";

	if (IsValid)
	{
		// The levels the engine's own loader leaves out (texture size cap, r_streaming_quality) are left out here too
		size_t Wide = Item.Width;
		size_t High = Item.Height;
		const u32 Levels = std::max(ReadWord(Data, 28), 1u);
		const u32 Skip = std::min({ u32(std::max(get_texture_load_lod(Path, Wide, High, BuiltLod < 0 ? psTextureLOD : BuiltLod), 0)), Levels - 1, Log2(Smaller) - 2 });
		Item.BlockBytes = Item.Format == FormatBC1 || Item.Format == FormatBC1Srgb ? 8 : 16;
		Header += LevelOffset(Item, Skip);
		Item.Width >>= Skip;
		Item.Height >>= Skip;

		const u32 Mips = Levels - Skip;
		const u32 Larger = std::max(Item.Width, Item.Height);
		const u32 Smaller = std::min(Item.Width, Item.Height);
		const u32 First = Larger > TailSize ? Log2(Larger / TailSize) : 0;
		Item.LastLevel = std::min(Mips - 1, Log2(Smaller) - 2);
		Item.FileLevels = Item.LastLevel + 1;
		Item.FileBytes = Length - Header;
		Item.IsFlat = Item.LastLevel < First;
		Item.LastLevel = Item.IsFlat ? 0 : Item.LastLevel;
		IsValid = Header + LevelOffset(Item, Item.LastLevel + 1) <= Length;
		Item.Why = IsValid ? "" : "data";
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

bool GpuTextures::ReadWhole(const char* Name, xr_vector<xr_vector<u8>>& Levels, u32& Format, u32& Width, u32& Height, u32& BlockBytes)
{
	Source File;
	Levels.clear();

	if (!OpenSource(Name, File))
	{
		return false;
	}

	// The engine's own loader reads a missing file, and says so
	if (File.IsPlaceholder)
	{
		FS.r_close(File.File);
		return false;
	}

	for (u32 Mip = 0; Mip < File.FileLevels; ++Mip)
	{
		const size_t Offset = LevelOffset(File, Mip);
		const size_t Bytes = size_t(LevelBlocks(File.Width, Mip)) * LevelBlocks(File.Height, Mip) * File.BlockBytes;

		if (Offset + Bytes > File.FileBytes)
		{
			break;
		}

		Levels.emplace_back(File.Data + Offset, File.Data + Offset + Bytes);
	}

	Format = File.Format;
	Width = File.Width;
	Height = File.Height;
	BlockBytes = File.BlockBytes;
	FS.r_close(File.File);
	return !Levels.empty();
}

// Reads the levels of a head as a slice of Size takes them. Runs on the loading thread: the file is mapped, and it
// is the copy out of it that reads the disk
bool GpuTextures::ReadHead(const char* Name, u32 Size, u32 Mips, xr_vector<Level>& Levels, u32 First)
{
	PROF_EVENT("TextureHeadRead");
	Source File;
	Levels.clear();

	if (!OpenSource(Name, File))
	{
		return false;
	}

	Levels.resize(Mips);

	for (u32 Mip = 0; Mip < Mips; ++Mip)
	{
		xr_vector<xr_vector<u8>> Scratch;
		const nri::TextureSubresourceUploadDesc Part = Tile(File, First + Mip, Size >> Mip, Scratch);
		Levels[Mip].RowBytes = u32(Part.rowPitch);

		if (!Scratch.empty())
		{
			Levels[Mip].Data = std::move(Scratch.back());
		}
		else
		{
			const u8* From = static_cast<const u8*>(Part.slices);
			Levels[Mip].Data.assign(From, From + Part.slicePitch);
		}
	}

	FS.r_close(File.File);
	return true;
}

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

u32 GpuTextures::Find(u32 Format, u32 Size, u32 Mips) const
{
	u32 Slot = 0;

	while (Slot < BucketCount && (Buckets[Slot].Format != Format || Buckets[Slot].Size != Size || Buckets[Slot].Mips != Mips))
	{
		++Slot;
	}

	return Slot;
}

u32 GpuTextures::Place(u32 Format, u32 Size, u32 Mips)
{
	const u32 Slot = Find(Format, Size, Mips);

	if (Slot == MaxBuckets || Buckets[Slot].Slices == MaxSlices)
	{
		return Absent;
	}

	if (Slot == BucketCount)
	{
		Buckets[BucketCount++] = { nullptr, nullptr, Format, Size, Mips, 0 };
	}

	return (Slot << 16) | Buckets[Slot].Slices++;
}

bool GpuTextures::Probe(const CTexture& Texture, u32& Format, u32& Width, u32& Height, const char*& Why)
{
	Source Item;
	if (!OpenSource(Texture.cName.c_str(), Item))
	{
		Why = Item.Why;
		return false;
	}

	Format = Item.Format;
	Width = Item.Width;
	Height = Item.Height;
	Why = Item.IsPlaceholder ? "the placeholder of a missing file" : Why;
	FS.r_close(Item.File);
	return true;
}

bool GpuTextures::IsPlain(const CTexture& Texture, const char*& Why)
{
	Source Item;

	if (OpenSource(Texture.cName.c_str(), Item))
	{
		FS.r_close(Item.File);
	}

	Why = Item.IsPlain ? "" : Item.Why;
	return Item.IsPlain;
}

void GpuTextures::Build(const xr_vector<CTexture*>& Textures, const xr_vector<u8>& WantsHead, xr_vector<Ref>& Refs)
{
	nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::CoreInterface), &Api);
	nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::HelperInterface), &Help);
	nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::StreamerInterface), &StreamApi);
	Refs.assign(Textures.size(), Ref{});
	BuiltLod = psTextureLOD;
	Loader.Start();
	Streams.assign(Textures.size(), Stream{});
	Heads.assign(Textures.size(), Absent);

	xr_vector<Source> Sources;
	u32 HeadCount = 0;

	for (size_t Index = 0; Index < Textures.size(); ++Index)
	{
		Source Item;

		if (!OpenSource(Textures[Index]->cName.c_str(), Item))
		{
			continue;
		}

		const u32 Larger = std::max(Item.Width, Item.Height);
		Item.First = Larger > TailSize && !Item.IsFlat ? Log2(Larger / TailSize) : 0;
		const u32 HeadMips = WantsHead[Index] ? Item.First : 0;
		const u32 Whole = Item.IsFlat ? Larger : TailSize;
		const u32 Levels = Item.IsFlat ? 1 : TailMips;
		const u32 Base = std::max(Larger, Whole);
		Item.Tail = Place(Item.Format, Whole, Levels);
		// A head has the texture's whole chain, the levels of its tail under its own: a pixel reads one or the other
		Item.Head = Item.Tail != Absent && HeadMips ? Place(Item.Format, Larger, HeadMips + Levels) : Absent;

		if (Item.Tail == Absent || (HeadMips && Item.Head == Absent))
		{
			FS.r_close(Item.File);
			continue;
		}

		Refs[Index] = { Item.Tail, Item.Head, Log2(Base / Item.Width) | (Log2(Base / Item.Height) << 4) | (HeadMips << 8) | (Log2(Whole) << 12) | (Levels << 16) };
		HeadCount += HeadMips != 0;

		// A head that is not loaded now gets a place in the pool of its kind when it is needed
		if (!HeadMips && Item.First && Index < MaxStreams)
		{
			const u32 Kind = Place(Item.Format, Larger, Item.First + Levels);

			if (Kind != Absent)
			{
				Bucket& Pool = Buckets[Kind >> 16];
				--Pool.Slices;
				++Pool.Wanted;
				Streams[Index] = { Textures[Index]->cName, Kind >> 16 };
				Refs[Index].Info |= (Item.First << 8) | (u32(Index + 1) << 20);
			}
		}
		Sources.push_back(Item);
	}

	for (u32 Slot = 0; Slot < BucketCount; ++Slot)
	{
		Bucket& Item = Buckets[Slot];
		Item.Spare = Item.Slices;
		Item.Slices += Item.Size == TailSize && Item.Mips == TailMips ? std::min(SpareTails, MaxSlices - Item.Slices) : 0;
		Item.PoolFirst = Item.Slices;
	}

	u64 Bytes = 0;
	u64 Asked = 0;
	u64 Pooled = 0;
	u32 Streamed = 0;

	for (u32 Slot = 0; Slot < BucketCount; ++Slot)
	{
		Asked += Buckets[Slot].Wanted * SliceBytes(Buckets[Slot]);
	}

	// r_texture_budget for all pools together, and no more than the video memory the system still grants the
	// process; never under the least the variable takes
	nri::VideoMemoryInfo Memory = {};
	Help.QueryVideoMemoryInfo(*GRHI->Nri->Device, nri::MemoryLocation::DEVICE, Memory);
	const u64 Granted = Memory.budgetSize > Memory.usageSize ? Memory.budgetSize - Memory.usageSize : 0;
	const u64 HeadBudget = std::max(std::min(u64(ps_r_texture_budget) << 20, Granted), 256ull << 20);
	PoolBudget = HeadBudget;

	// Every pool gets its share of the budget, by what its textures would take all loaded
	for (u32 Slot = 0; Slot < BucketCount; ++Slot)
	{
		Bucket& Item = Buckets[Slot];

		if (Item.Wanted)
		{
			const u32 Share = Asked <= HeadBudget ? Item.Wanted : std::max(1u, u32(u64(Item.Wanted) * HeadBudget / Asked));
			Item.Owners.assign(std::min(Share, MaxSlices - Item.Slices), Absent);
			Item.Slices += u32(Item.Owners.size());
			Pooled += Item.Owners.size() * SliceBytes(Item);
			Streamed += Item.Wanted;
		}
	}

	for (u32 Slot = 0; Slot < BucketCount; ++Slot)
	{
		Bucket& Item = Buckets[Slot];
		CreateArray(Item);

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
			const u32 First = IsTail ? From.First : 0;

			for (u32 Mip = 0; Mip < Item.Mips; ++Mip)
			{
				Parts[size_t(Slice) * Item.Mips + Mip] = Tile(From, First + Mip, Item.Size >> Mip, Scratch);
				Bytes += Parts[size_t(Slice) * Item.Mips + Mip].slicePitch;
			}
		}

		// The spare tails and the pool start empty: every subresource still has to be given
		const u32 BlockBytes = Item.Format == FormatBC1 || Item.Format == FormatBC1Srgb ? 8 : 16;
		const xr_vector<u8> Empty(Item.Slices == Item.Spare ? 0 : size_t(Item.Size / 4) * (Item.Size / 4) * BlockBytes);

		for (size_t Part = size_t(Item.Spare) * Item.Mips; Part < Parts.size() && !Empty.empty(); ++Part)
		{
			const u32 Blocks = LevelBlocks(Item.Size, u32(Part % Item.Mips));
			Parts[Part] = { Empty.data(), 1, Blocks * BlockBytes, Blocks * BlockBytes * Blocks };
		}

		nri::TextureUploadDesc Upload = {};
		Upload.subresources = Parts.data();
		Upload.texture = Item.Texture;
		Upload.after = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::FRAGMENT_SHADER };
		Upload.planes = nri::PlaneBits::ALL;
		Help.UploadData(*GRHI->Nri->Queue, &Upload, 1, nullptr, 0);
		Msg("* [scene] texture bucket %u: %u, format %u, %u mips, %u slices", Slot, Item.Size, Item.Format, Item.Mips, Item.Slices);
	}

	for (Source& Item : Sources)
	{
		FS.r_close(Item.File);
	}

	Msg("* [scene] texture buckets: %u textures of %u in %u buckets, %u with heads, %llu MB", u32(Sources.size()), u32(Textures.size()), BucketCount, HeadCount, Bytes >> 20);
	Msg("* [scene] texture heads: %u streamed, %llu MB all loaded, pools %llu MB of a budget of %llu MB (r_texture_budget %d, %llu MB of video memory left to the process)", Streamed, Asked >> 20, Pooled >> 20, HeadBudget >> 20, ps_r_texture_budget, Granted >> 20);
}

u64 GpuTextures::SliceBytes(const Bucket& Item)
{
	const u64 Block = Item.Format == FormatBC1 || Item.Format == FormatBC1Srgb ? 8 : 16;
	u64 Sum = 0;

	for (u32 Mip = 0; Mip < Item.Mips; ++Mip)
	{
		Sum += u64(LevelBlocks(Item.Size, Mip)) * LevelBlocks(Item.Size, Mip) * Block;
	}

	return Sum;
}

void GpuTextures::CreateArray(Bucket& Item)
{
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

	nri::TextureViewDesc View = {};
	View.texture = Item.Texture;
	View.type = nri::TextureView::TEXTURE_ARRAY;
	View.mipNum = nri::REMAINING;
	View.layerNum = nri::REMAINING;
	Api.CreateTextureView(View, Item.View);
}

void GpuTextures::Resize(u32 Slot, u32 Places, nri::Texture* Texture, nri::Descriptor* View, u32 Frame, nri::CommandBuffer& Commands)
{
	PROF_EVENT("TexturePoolResize");
	Bucket& Pool = Buckets[Slot];
	const u32 Kept = std::min(Pool.Slices, Pool.PoolFirst + Places);

	for (u32 Place = Places; Place < Pool.Owners.size(); ++Place)
	{
		if (const u32 Owner = Pool.Owners[Place]; Owner != Absent)
		{
			Streams[Owner].Slice = Absent;
			Heads[Owner] = Absent;
		}
	}

	Bucket Old = Pool;
	Pool.Slices = Pool.PoolFirst + Places;
	Pool.Owners.resize(Places, Absent);
	Pool.Texture = Texture;
	Pool.View = View;
	RetiredArrays.push_back({ Old.Texture, Old.View, Frame });

	// The frames in flight read the old array, and so do the draws of this frame: it goes back to what it was
	nri::TextureBarrierDesc Parts[2] = {};
	Parts[0].texture = Old.Texture;
	Parts[0].before = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::FRAGMENT_SHADER };
	Parts[0].after = { nri::AccessBits::COPY_SOURCE, nri::Layout::COPY_SOURCE, nri::StageBits::COPY };
	Parts[0].mipNum = nri::REMAINING;
	Parts[0].layerNum = nri::REMAINING;
	Parts[1].texture = Pool.Texture;
	Parts[1].after = { nri::AccessBits::COPY_DESTINATION, nri::Layout::COPY_DESTINATION, nri::StageBits::COPY };
	Parts[1].mipNum = nri::REMAINING;
	Parts[1].layerNum = nri::REMAINING;
	nri::BarrierDesc Barriers = {};
	Barriers.textures = Parts;
	Barriers.textureNum = 2;
	Api.CmdBarrier(Commands, Barriers);

	for (u32 Slice = 0; Slice < Kept; ++Slice)
	{
		for (u32 Mip = 0; Mip < Pool.Mips; ++Mip)
		{
			const nri::TextureRegionDesc Region = { 0, 0, 0, nri::WHOLE_SIZE, nri::WHOLE_SIZE, nri::WHOLE_SIZE, nri::Dim_t(Mip), nri::Dim_t(Slice), nri::PlaneBits::ALL };
			Api.CmdCopyTexture(Commands, *Pool.Texture, &Region, *Old.Texture, &Region);
		}
	}

	std::swap(Parts[0].before, Parts[0].after);
	Parts[1].before = Parts[1].after;
	Parts[1].after = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::FRAGMENT_SHADER };
	Api.CmdBarrier(Commands, Barriers);
}

bool GpuTextures::Rebalance(u32 Slot, float Heat, u32 Frame)
{
	Bucket& Pool = Buckets[Slot];
	const u64 Wanted = SliceBytes(Pool);
	u64 Pooled = 0;

	for (u32 Other = 0; Other < BucketCount; ++Other)
	{
		Pooled += Buckets[Other].Owners.size() * SliceBytes(Buckets[Other]);
	}

	// The donor: the pool whose end holds the most bytes of heads nothing has asked for in KeepFrames, or several
	// times colder than the head that waits here
	u32 Donor = Absent;
	u32 DonorPlaces = 0;
	u64 DonorBytes = 0;

	for (u32 Other = 0; Other < BucketCount; ++Other)
	{
		const Bucket& Item = Buckets[Other];
		u32 Idle = 0;

		while (Other != Slot && Idle < Item.Owners.size())
		{
			const u32 Owner = Item.Owners[Item.Owners.size() - 1 - Idle];

			if (Owner != Absent && Frame - Streams[Owner].LastSeen <= KeepFrames && Streams[Owner].Heat * HeatTaken >= Heat)
			{
				break;
			}

			++Idle;
		}

		if (Idle * SliceBytes(Item) > DonorBytes)
		{
			Donor = Other;
			DonorPlaces = Idle;
			DonorBytes = Idle * SliceBytes(Item);
		}
	}

	const u64 Spare = PoolBudget > Pooled ? PoolBudget - Pooled : 0;

	if (Spare + DonorBytes < Wanted || Pool.Slices >= MaxSlices)
	{
		return false;
	}

	// No more is taken from the donor than half of what waits here needs
	u32 Waiting = 0;

	for (const Stream& Item : Streams)
	{
		Waiting += Item.Bucket == Slot && Item.Slice == Absent && Frame - Item.LastSeen <= SeenFrames;
	}

	const u32 Grown = std::clamp(u32((Spare + DonorBytes) / Wanted), 1u, std::min(std::max(Waiting, 1u), MaxSlices - Pool.Slices));
	const u64 Needed = Grown * Wanted > Spare ? Grown * Wanted - Spare : 0;
	const u32 Taken = Donor != Absent && Needed ? u32((Needed + SliceBytes(Buckets[Donor]) - 1) / SliceBytes(Buckets[Donor])) : 0;

	MovingPools[0] = { Slot, u32(Pool.Owners.size()) + Grown, Pool };
	MovingPools[1] = Taken ? Moving{ Donor, u32(Buckets[Donor].Owners.size()) - std::min(Taken, DonorPlaces), Buckets[Donor] } : Moving{};

	for (Moving& Item : MovingPools)
	{
		Item.Made.Slices = Item.Slot != Absent ? Item.Made.PoolFirst + Item.Places : 0;
		Item.Made.Owners.clear();
	}

	const bool IsAsked = Loader.Begin([this]()
	{
		for (Moving& Item : MovingPools)
		{
			if (Item.Slot != Absent)
			{
				CreateArray(Item.Made);
			}
		}
	});

	if (!IsAsked)
	{
		MovingPools[0] = MovingPools[1] = Moving{};
		return false;
	}

	Msg("* [scene] head pools: %u (%u) grows by %u to %u places; %u (%u) gives %u", Slot, Pool.Size, Grown, u32(Pool.Owners.size()) + Grown, Taken ? Donor : Slot, Taken ? Buckets[Donor].Size : 0, Taken ? std::min(Taken, DonorPlaces) : 0);
	return true;
}

u32 GpuTextures::FindPlace(const Stream& Item, u32 Frame) const
{
	const Bucket& Pool = Buckets[Item.Bucket];
	u32 Best = Absent;

	for (u32 Slot = 0; Slot < Pool.Owners.size(); ++Slot)
	{
		const u32 Owner = Pool.Owners[Slot];

		if (Owner == Absent)
		{
			return Slot;
		}

		const Stream& Held = Streams[Owner];
		const bool IsFree = Frame - Held.LastSeen > KeepFrames || Held.Heat * HeatTaken < Item.Heat;

		if (IsFree && (Best == Absent || Held.Heat < Streams[Pool.Owners[Best]].Heat))
		{
			Best = Slot;
		}
	}

	return Best;
}

void GpuTextures::DropLoading()
{
	Loader.Wait();
	Loader.Finish();

	for (Moving& Item : MovingPools)
	{
		if (Item.Slot != Absent && Item.Made.Texture)
		{
			Api.DestroyDescriptor(Item.Made.View);
			Api.DestroyTexture(Item.Made.Texture);
		}

		Item = Moving{};
	}

	LoadingHead = Absent;
	LoadingLevels.clear();
}

void GpuTextures::CopySlice(const Bucket& Pool, u32 Slice, const xr_vector<Level>& Levels, nri::CommandBuffer& Commands, nri::Streamer& Streamer)
{
	nri::TextureBarrierDesc Barrier = {};
	Barrier.texture = Pool.Texture;
	Barrier.before = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::FRAGMENT_SHADER };
	Barrier.after = { nri::AccessBits::COPY_DESTINATION, nri::Layout::COPY_DESTINATION, nri::StageBits::COPY };
	Barrier.mipNum = nri::REMAINING;
	Barrier.layerOffset = nri::Dim_t(Slice);
	Barrier.layerNum = 1;
	nri::BarrierDesc Barriers = {};
	Barriers.textures = &Barrier;
	Barriers.textureNum = 1;
	Api.CmdBarrier(Commands, Barriers);

	const nri::StreamerCopyBatch Batch = StreamApi.BeginStreamerCopyBatch(Streamer);

	for (u32 Mip = 0; Mip < Pool.Mips; ++Mip)
	{
		nri::StreamTextureDataDesc Data = {};
		Data.data = Levels[Mip].Data.data();
		Data.dataRowPitch = Levels[Mip].RowBytes;
		Data.dataSlicePitch = u32(Levels[Mip].Data.size());
		Data.copyBatch = Batch;
		Data.dstTexture = Pool.Texture;
		Data.dstRegion = { 0, 0, 0, nri::Dim_t(Pool.Size >> Mip), nri::Dim_t(Pool.Size >> Mip), 1, nri::Dim_t(Mip), nri::Dim_t(Slice), nri::PlaneBits::ALL };
		StreamApi.StreamTextureData(Streamer, Data);
	}

	StreamApi.CmdCopyStreamedData(Commands, Streamer, Batch);
	std::swap(Barrier.before, Barrier.after);
	Api.CmdBarrier(Commands, Barriers);
}

GpuTextures::Ref GpuTextures::Add(const CTexture& Texture, nri::CommandBuffer& Commands, nri::Streamer& Streamer)
{
	Source Item;
	Ref Made;

	if (!OpenSource(Texture.cName.c_str(), Item))
	{
		return Made;
	}

	FS.r_close(Item.File);
	const u32 Larger = std::max(Item.Width, Item.Height);
	const u32 First = Larger > TailSize && !Item.IsFlat ? Log2(Larger / TailSize) : 0;
	const u32 Whole = Item.IsFlat ? Larger : TailSize;
	const u32 Levels = Item.IsFlat ? 1 : TailMips;
	const u32 Base = std::max(Larger, Whole);
	const u32 Slot = Find(Item.Format, Whole, Levels);
	xr_vector<Level> Data;

	if (Slot == BucketCount || Buckets[Slot].Spare == Buckets[Slot].PoolFirst || !ReadHead(Texture.cName.c_str(), Whole, Levels, Data, First))
	{
		return Made;
	}

	Bucket& Tails = Buckets[Slot];
	CopySlice(Tails, Tails.Spare, Data, Commands, Streamer);
	Made.Tail = (Slot << 16) | Tails.Spare++;
	Made.Info = Log2(Base / Item.Width) | (Log2(Base / Item.Height) << 4) | (Log2(Whole) << 12) | (Levels << 16);

	// The pool of a head waits for places the rebalance gives it, as any pool whose heads wait does
	const u32 Pool = First ? Find(Item.Format, Larger, First + Levels) : BucketCount;

	if (Pool != BucketCount && Streams.size() < MaxStreams)
	{
		++Buckets[Pool].Wanted;
		Made.Info |= (First << 8) | (u32(Streams.size() + 1) << 20);
		Streams.push_back({ Texture.cName, Pool });
		Heads.push_back(Absent);
	}

	return Made;
}

void GpuTextures::TakeLoaded(u32 Frame, nri::CommandBuffer& Commands, nri::Streamer& Streamer)
{
	if (!Loader.Finish())
	{
		return;
	}

	PROF_EVENT("TextureLoaded");

	for (Moving& Item : MovingPools)
	{
		if (Item.Slot != Absent)
		{
			Resize(Item.Slot, Item.Places, Item.Made.Texture, Item.Made.View, Frame, Commands);
			ResizeFrame = Frame;
		}

		Item = Moving{};
	}

	const u32 Index = std::exchange(LoadingHead, Absent);
	xr_vector<Level> Levels = std::move(LoadingLevels);
	LoadingLevels.clear();

	if (Index == Absent || Index >= Streams.size())
	{
		return;
	}

	Stream& Item = Streams[Index];

	if (!IsHeadRead)
	{
		Msg("! [scene] the head of texture %s could not be read: it stays on its tail", Item.Name.c_str());
		Item.Bucket = Absent;
		return;
	}

	// The pool may have changed while the head was read: its place is found now
	const u32 Best = Item.Bucket != Absent && Item.Slice == Absent ? FindPlace(Item, Frame) : Absent;

	if (Best == Absent || Levels.size() != Buckets[Item.Bucket].Mips)
	{
		return;
	}

	Bucket& Pool = Buckets[Item.Bucket];
	const u32 Slice = Pool.PoolFirst + Best;
	CopySlice(Pool, Slice, Levels, Commands, Streamer);

	if (const u32 Owner = Pool.Owners[Best]; Owner != Absent)
	{
		Streams[Owner].Slice = Absent;
		Heads[Owner] = Absent;
	}

	Pool.Owners[Best] = Index;
	Item.Slice = Slice;
	Heads[Index] = (Item.Bucket << 16) | Slice;
}

void GpuTextures::Update(const u32* Seen, u32 Frame, nri::CommandBuffer& Commands, nri::Streamer& Streamer)
{
	PROF_EVENT("TextureStreaming");
	for (size_t Index = 0; Index < RetiredArrays.size();)
	{
		if (Frame - RetiredArrays[Index].Frame < RetiredFrames)
		{
			++Index;
			continue;
		}

		Api.DestroyDescriptor(RetiredArrays[Index].View);
		Api.DestroyTexture(RetiredArrays[Index].Texture);
		RetiredArrays.erase(RetiredArrays.begin() + Index);
	}

	const u32 Count = u32(Streams.size());

	if (!Seen)
	{
		return;
	}

	LastFrame = Frame;

	// How often and how lately a head is asked for is one number: every frame it is asked adds one, and every
	// frame takes a share of it away
	xr_vector<u32> Asking;

	for (u32 Index = 0; Index < std::min(Count, MaxStreams); ++Index)
	{
		Stream& Item = Streams[Index];
		Item.Heat *= HeatKept;

		if (Seen[Index] && ((Frame - (Seen[Index] >> 8)) & 0xFFFFFFu) < SeenFrames)
		{
			Item.LastSeen = Frame;
			Item.Heat += float(Seen[Index] & 255u) * (1.0f / 255.0f);

			if (Item.Bucket != Absent && Item.Slice == Absent && Item.Heat >= HeatAsked)
			{
				Asking.push_back(Index);
			}
		}
	}

	// The hottest of those that wait is served first
	std::sort(Asking.begin(), Asking.end(), [this](u32 Left, u32 Right) { return Streams[Left].Heat > Streams[Right].Heat; });

	TakeLoaded(Frame, Commands, Streamer);

	float StarvedHeat[MaxBuckets] = {};

	for (const u32 Index : Asking)
	{
		Stream& Item = Streams[Index];

		if (Item.Bucket == Absent || Item.Slice != Absent)
		{
			continue;
		}

		if (FindPlace(Item, Frame) == Absent)
		{
			StarvedHeat[Item.Bucket] = std::max(StarvedHeat[Item.Bucket], Item.Heat);
			continue;
		}

		// The file is read and laid out as the slice takes it by the loading thread: read here, a head of the
		// largest size stopped the frame for a few of them
		if (Loader.IsIdle() && LoadingHead == Absent)
		{
			const Bucket& Pool = Buckets[Item.Bucket];
			const shared_str Name = Item.Name;
			const u32 Size = Pool.Size;
			const u32 Mips = Pool.Mips;

			if (Loader.Begin([this, Name, Size, Mips]() { IsHeadRead = ReadHead(Name.c_str(), Size, Mips, LoadingLevels); }))
			{
				LoadingHead = Index;
			}
		}
	}

	// A pool whose heads have waited long with every place of it asked for takes places from a pool that holds what
	// nothing asks for: the budget is one for all of them
	for (u32 Slot = 0; Slot < BucketCount; ++Slot)
	{
		Buckets[Slot].Starved = StarvedHeat[Slot] > 0.0f ? Buckets[Slot].Starved + 1 : 0;
	}

	for (u32 Slot = 0; Slot < BucketCount && Frame - ResizeFrame > ResizeGap && Loader.IsIdle() && LoadingHead == Absent; ++Slot)
	{
		if (Buckets[Slot].Starved > StarvedFrames && Rebalance(Slot, StarvedHeat[Slot], Frame))
		{
			Buckets[Slot].Starved = 0;
			ResizeFrame = Frame;
		}
	}
}

void GpuTextures::LogPools() const
{
	// Over its budget the process has its memory paged by the system, and a frame waits for it
	nri::VideoMemoryInfo Memory = {};
	Help.QueryVideoMemoryInfo(*GRHI->Nri->Device, nri::MemoryLocation::DEVICE, Memory);
	Msg("* [scene] video memory of the process: %u MB used of %u MB granted by the system%s", u32(Memory.usageSize >> 20), u32(Memory.budgetSize >> 20), Memory.usageSize > Memory.budgetSize ? " - OVER THE BUDGET" : "");

	for (u32 Slot = 0; Slot < BucketCount; ++Slot)
	{
		const Bucket& Pool = Buckets[Slot];

		if (Pool.Owners.empty())
		{
			continue;
		}

		u32 Held = 0;
		u32 HeldAsked = 0;
		u32 Waiting = 0;
		u32 Weak = 0;
		const Stream* Hottest = nullptr;

		for (const Stream& Item : Streams)
		{
			if (Item.Bucket != Slot)
			{
				continue;
			}

			const bool IsAsked = LastFrame - Item.LastSeen <= SeenFrames;
			Held += Item.Slice != Absent;
			HeldAsked += Item.Slice != Absent && IsAsked;

			if (Item.Slice == Absent && IsAsked)
			{
				++Waiting;
				Weak += Item.Heat < HeatAsked;
				Hottest = !Hottest || Item.Heat > Hottest->Heat ? &Item : Hottest;
			}
		}

		Msg("* [scene] head pool %u: %u, format %u, %u places for %u textures; %u held, %u of them asked for now; %u wait, %u of them asked too weakly to be loaded%s%s (heat %.2f)", Slot, Pool.Size, Pool.Format, u32(Pool.Owners.size()), Pool.Wanted, Held, HeldAsked, Waiting, Weak,
			Hottest ? ", the hottest " : "", Hottest ? Hottest->Name.c_str() : "", Hottest ? Hottest->Heat : 0.0f);
	}
}

void GpuTextures::Destroy()
{
	if (BucketCount)
	{
		DropLoading();
	}

	Loader.Stop();

	for (const Retired& Item : RetiredArrays)
	{
		Api.DestroyDescriptor(Item.View);
		Api.DestroyTexture(Item.Texture);
	}

	RetiredArrays.clear();

	for (u32 Slot = 0; Slot < BucketCount; ++Slot)
	{
		Api.DestroyDescriptor(Buckets[Slot].View);
		Api.DestroyTexture(Buckets[Slot].Texture);
		Buckets[Slot] = {};
	}

	BucketCount = 0;
	BuiltLod = -1;
	Streams.clear();
	Heads.clear();
}

nri::Descriptor* GpuTextures::View(u32 Bucket) const
{
	return Buckets[Bucket < BucketCount ? Bucket : 0].View;
}
