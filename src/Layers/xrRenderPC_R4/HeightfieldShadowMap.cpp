#include "stdafx.h"
#include "HeightfieldShadowMap.h"

#include <charconv>
#include <array>
#include <memory>

namespace HeightfieldAssets
{
// The asset format uses IEEE CRC32, independent of the engine's CPU-selected CRC.
constexpr auto ChecksumTable = []
{
	std::array<u32, 256> Table{};
	for (u32 Index = 0; Index < Table.size(); ++Index)
	{
		u32 Value = Index;
		for (u32 Bit = 0; Bit < 8; ++Bit)
		{
			Value = (Value >> 1) ^ ((Value & 1) ? 0xedb88320u : 0u);
		}
		Table[Index] = Value;
	}
	return Table;
}();

constexpr u32 AssetChecksum(const char* Data, size_t Size)
{
	u32 Value = ~0u;
	for (size_t Index = 0; Index < Size; ++Index)
	{
		Value = (Value >> 8) ^ ChecksumTable[(Value ^ static_cast<u8>(Data[Index])) & 255u];
	}
	return ~Value;
}
static_assert(AssetChecksum("123456789", 9) == 0xcbf43926u);

// This optional generated sidecar has a deliberately small numeric LTX grammar.
// Parse it without the fatal errors/includes/inheritance of a game configuration.
using Values = xr_hash_map<xr_string, xr_string_view>;

xr_string_view Trim(xr_string_view Text)
{
	const size_t First = Text.find_first_not_of(" \t\r");
	if (First == xr_string_view::npos)
	{
		return {};
	}
	return Text.substr(First, Text.find_last_not_of(" \t\r") - First + 1);
}

bool ParseMetadata(xr_string_view Text, Values& Result)
{
	bool HasSection = false;
	while (!Text.empty())
	{
		const size_t End = Text.find('\n');
		xr_string_view Line = Text.substr(0, End);
		Text = End == xr_string_view::npos ? xr_string_view{} : Text.substr(End + 1);
		Line = Trim(Line.substr(0, Line.find(';')));
		if (Line.empty())
		{
			continue;
		}
		if (!HasSection)
		{
			if (Line != "[heightfield]")
			{
				return false;
			}
			HasSection = true;
			continue;
		}
		const size_t Equal = Line.find('=');
		if (Equal == xr_string_view::npos)
		{
			return false;
		}
		const xr_string_view Key = Trim(Line.substr(0, Equal));
		if (Key.empty() || !Result.emplace(xr_string(Key.data(), u32(Key.size())), Trim(Line.substr(Equal + 1))).second)
		{
			return false;
		}
	}
	return HasSection;
}

template <class T>
bool ReadNumber(const Values& Data, const char* Key, T& Number)
{
	const auto Found = Data.find(Key);
	if (Found == Data.end() || Found->second.empty())
	{
		return false;
	}
	const xr_string_view Text = Found->second;
	const auto Parsed = std::from_chars(Text.data(), Text.data() + Text.size(), Number);
	return Parsed.ec == std::errc{} && Parsed.ptr == Text.data() + Text.size();
}

bool LoadTexture(const xr_string& Name, const char* ResourceName, u32 Width, u32 Height, u32 Bits, u32 Checksum, ref_texture& Texture)
{
	CReaderGuarded Reader(FS.r_open("$game_textures$", Name.c_str()));
	if (!Reader || Reader->length() < 148 || AssetChecksum(static_cast<const char*>(Reader->pointer()), Reader->length()) != Checksum)
	{
		return false;
	}
	const ERHI_FORMAT Format = Bits == 16 ? ERHI_FORMAT::R16_UNORM : ERHI_FORMAT::R8_UNORM;
	RHITextureMetadata Metadata;
	if (FAILED(GRHI->GetDDSMetadata(Reader->pointer(), Reader->length(), Metadata)) || Metadata.width != Width || Metadata.height != Height ||
		Metadata.mipmap_count != 1 || Metadata.format != int(Format) || Metadata.cubemap || Metadata.volumemap)
	{
		return false;
	}
	const size_t ExpectedSize = 148 + size_t(Width) * Height * (Bits / 8);
	if (Reader->length() != ExpectedSize)
	{
		return false;
	}
	int Lod = 0;
	IRHISurface* Surface = nullptr;
	const auto Result = GRHI->LoadDDS(Reader->pointer(), Reader->length(), ERHI_USAGE::USAGE_DEFAULT, u32(ERHI_BIND_FLAG::SHADER_RESOURCE), ERHI_CPU_ACCESS_FLAG_NONE, Lod, false, &Surface);
	const auto ReleaseSurface = [](IRHISurface* Value)
	{
		if (Value)
		{
			Value->Release();
		}
	};
	// xr_unique_ptr uses engine deletion; an RHI resource requires Release instead.
	const std::unique_ptr<IRHISurface, decltype(ReleaseSurface)> Owner(Surface, ReleaseSurface);
	if (FAILED(Result) || !Surface)
	{
		return false;
	}
	if (Surface->GetArraySize() != 1 || Surface->GetMipLevels() != 1 || Surface->GetWidth() != Width || Surface->GetHeight() != Height)
	{
		return false;
	}
	Texture.create(ResourceName);
	Texture->surface_set(Surface);
	return Texture->GetView() != nullptr;
}
} // namespace HeightfieldAssets

HeightfieldShadowMap::~HeightfieldShadowMap()
{
	Destroy();
}

void HeightfieldShadowMap::Destroy()
{
	HasData = false;
	if (Heights)
	{
		Heights->surface_set(nullptr);
		Heights.destroy();
	}
	WorldToGrid.set(0, 0, 0, 0);
	Decode.set(0, 0, 0, 0);
	Grid.set(0, 0, 0, 0);
}

void HeightfieldShadowMap::Load(const char* LevelName)
{
	Destroy();
	if (!LevelName || !LevelName[0] || strspn(LevelName, "abcdefghijklmnopqrstuvwxyz0123456789_-") != strlen(LevelName))
	{
		return;
	}
	const xr_string Prefix = xr_string("heightfields\\") + LevelName;
	string_path Path;
	if (!FS.exist(Path, "$game_textures$", (Prefix + ".ltx").c_str()))
	{
		return;
	}
	CReaderGuarded Reader(FS.r_open(Path));
	HeightfieldAssets::Values Data;
	u32 Version = 0, Width = 0, Height = 0, Bits = 0, HeightCrc = 0;
	float OriginX = 0, OriginZ = 0, StepX = 0, StepZ = 0, MinY = 0, RangeY = 0;
	using HeightfieldAssets::ReadNumber;
	bool IsMetadataValid = Reader && Reader->length() <= 4096 &&
						   HeightfieldAssets::ParseMetadata(xr_string_view(static_cast<const char*>(Reader->pointer()), Reader->length()), Data) &&
						   ReadNumber(Data, "version", Version) && ReadNumber(Data, "width", Width) && ReadNumber(Data, "height", Height) &&
						   ReadNumber(Data, "bits", Bits) &&
						   ReadNumber(Data, "origin_x", OriginX) && ReadNumber(Data, "origin_z", OriginZ) &&
						   ReadNumber(Data, "step_x", StepX) && ReadNumber(Data, "step_z", StepZ) &&
						   ReadNumber(Data, "height_min", MinY) && ReadNumber(Data, "height_range", RangeY) &&
						   ReadNumber(Data, "height_crc32", HeightCrc);
	IsMetadataValid = IsMetadataValid && Version == 1 && (Bits == 8 || Bits == 16) &&
					  Width >= 2 && Height >= 2 && Width <= 4096 && Height <= 4096 && // 64 blocks of 64 samples.
					  std::isfinite(OriginX) && std::isfinite(OriginZ) && std::isfinite(StepX) && std::isfinite(StepZ) &&
					  std::isfinite(MinY) && std::isfinite(RangeY) && StepX > 1e-5f && StepZ < -1e-5f && RangeY >= 0.001f &&
					  std::isfinite(OriginX + StepX * Width) && std::isfinite(OriginZ + StepZ * Height) &&
					  std::isfinite(MinY + RangeY) && std::isfinite(RangeY * 2.0f) && std::isfinite(MinY - RangeY);
	if (!IsMetadataValid ||
		!HeightfieldAssets::LoadTexture(Prefix + ".dds", "$user$heightfield_height", Width, Height, Bits, HeightCrc, Heights))
	{
		Msg("! Heightfield shadows: invalid or incomplete assets for '%s'; using existing sun shadows", LevelName);
		Destroy();
		return;
	}
	const float MaxCode = float((1u << Bits) - 1u);
	const float Step = RangeY / (MaxCode - 1.0f);
	WorldToGrid.set(OriginX, OriginZ, 1.0f / StepX, 1.0f / StepZ);
	Decode.set(Step * MaxCode, MinY - Step, Step, MaxCode);
	Grid.set(float(Width), float(Height), 0.0f, 0.0f);
	HasData = true;
	Msg("* Heightfield shadows: '%s', %ux%u R%u, Y=[%.3f, %.3f], prefix scan", LevelName, Width, Height, Bits, MinY, MinY + RangeY);
}
