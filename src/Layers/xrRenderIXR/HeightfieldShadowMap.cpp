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
#include "ixr.h"
#include "xrRender_console.h"
#include "dxRenderDeviceRender.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrCore/CharConv.h"
#include <array>

namespace
{
// HEIGHTFIELD_BLOCK of shaders/common/heightfield_shadow.cs.hlsl. A row has at most 64 blocks: the block scan is one group
constexpr u32 BlockSize = 64;
constexpr u32 MaxSize = 4096;
// The magic, the header and its DX10 part
constexpr u32 DdsHeader = 148;

enum EKey : u32
{
	KeyVersion,
	KeyWidth,
	KeyHeight,
	KeyBits,
	KeyOriginX,
	KeyOriginZ,
	KeyStepX,
	KeyStepZ,
	KeyMinY,
	KeyRangeY,
	KeyCrc,
	KeyCount
};

constexpr xr_string_view KeyNames[KeyCount] = { "version", "width", "height", "bits", "origin_x", "origin_z", "step_x", "step_z", "height_min", "height_range", "height_crc32" };

// The file's checksum is the IEEE CRC-32, not the engine's
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

constexpr u32 Checksum(const u8* Data, size_t Size)
{
	u32 Value = ~0u;

	for (size_t Index = 0; Index < Size; ++Index)
	{
		Value = (Value >> 8) ^ ChecksumTable[(Value ^ Data[Index]) & 255u];
	}

	return ~Value;
}

xr_string_view Trim(xr_string_view Text)
{
	const size_t First = Text.find_first_not_of(" \t\r");
	return First == xr_string_view::npos ? xr_string_view() : Text.substr(First, Text.find_last_not_of(" \t\r") - First + 1);
}

// The sidecar the heightfield tool writes: one [heightfield] section of numbers, each key once. It is read without the
// fatal errors, includes and inheritance of a game config; keys it does not know are left alone
bool ReadMetadata(xr_string_view Text, double (&Values)[KeyCount])
{
	u32 Found = 0;
	bool HasSection = false;

	while (!Text.empty())
	{
		const size_t End = Text.find('\n');
		xr_string_view Line = Text.substr(0, End);
		Text = End == xr_string_view::npos ? xr_string_view() : Text.substr(End + 1);
		Line = Trim(Line.substr(0, Line.find(';')));

		if (Line.empty())
		{
			continue;
		}

		if (!HasSection)
		{
			HasSection = Line == "[heightfield]";

			if (!HasSection)
			{
				return false;
			}

			continue;
		}

		const size_t Equal = Line.find('=');

		if (Equal == xr_string_view::npos)
		{
			return false;
		}

		const xr_string_view Value = Trim(Line.substr(Equal + 1));
		const u32 Index = u32(std::find(std::begin(KeyNames), std::end(KeyNames), Trim(Line.substr(0, Equal))) - std::begin(KeyNames));

		if (Index == KeyCount)
		{
			continue;
		}

		const std::from_chars_result Parsed = CharConv::FromChars(Value.data(), Value.data() + Value.size(), Values[Index]);

		if (((Found >> Index) & 1) || Parsed.ec != std::errc{} || Parsed.ptr != Value.data() + Value.size())
		{
			return false;
		}

		Found |= 1u << Index;
	}

	return Found == (1u << KeyCount) - 1;
}

u32 ReadU32(const u8* Data, u32 Offset)
{
	u32 Value;
	std::memcpy(&Value, Data + Offset, sizeof(Value));
	return Value;
}

// One 2D DX10 DDS of R16 or R8, one mip, no more and no less than its texels
bool IsHeightImage(const u8* Data, size_t Size, u32 Width, u32 Height, ERHI_FORMAT Format)
{
	return Size == DdsHeader + size_t(Width) * Height * (Format == ERHI_FORMAT::R16_UNORM ? 2 : 1) && !std::memcmp(Data, "DDS ", 4) && ReadU32(Data, 4) == 124 &&
		ReadU32(Data, 12) == Height && ReadU32(Data, 16) == Width && ReadU32(Data, 28) <= 1 && !std::memcmp(Data + 84, "DX10", 4) && ReadU32(Data, 128) == u32(Format) &&
		ReadU32(Data, 132) == 3 && !(ReadU32(Data, 136) & 4) && ReadU32(Data, 140) == 1;
}
} // namespace

HeightfieldShadowMap::~HeightfieldShadowMap()
{
	Destroy();
}

void HeightfieldShadowMap::Destroy()
{
	if (Heights)
	{
		Heights->surface_set(nullptr);
		Heights = nullptr;
	}

	Target = nullptr;
	Local = nullptr;
	Blocks = nullptr;
	Prefix = nullptr;

	for (ref_cs& Pass : Passes)
	{
		Pass = nullptr;
	}

	Width = Height = 0;
	Frame = u32(-1);
	IsRead = false;
}

IRHISurface* HeightfieldShadowMap::Shadow() const
{
	return Frame == RFrame.dwFrame && Target ? Target->pSurface : nullptr;
}

bool HeightfieldShadowMap::Load(const char* LevelName)
{
	if (!LevelName || !LevelName[0] || strspn(LevelName, "abcdefghijklmnopqrstuvwxyz0123456789_-") != strlen(LevelName))
	{
		return false;
	}

	const xr_string Name = xr_string("heightfields\\") + LevelName;
	string_path Path;

	if (!FS.exist(Path, "$game_textures$", (Name + ".ltx").c_str()))
	{
		Msg("* Heightfield shadows: no textures\\%s.ltx, the level has the cascades alone", Name.c_str());
		return false;
	}

	CReaderGuarded Sidecar(FS.r_open(Path));
	CReaderGuarded Image(FS.r_open("$game_textures$", (Name + ".dds").c_str()));
	double Values[KeyCount] = {};
	const auto IsWhole = [](double Value, double Low, double High) { return Value >= Low && Value <= High && Value == std::floor(Value); };
	// Bounded so that no sum, product or reciprocal the scan and the lookup make of them leaves a float
	const auto IsReal = [](double Value) { return std::abs(Value) <= 1e30; };

	// A modder's file: everything a target is sized by or the shaders divide by is checked before it is used
	const bool IsValid = Sidecar && Sidecar->length() <= 4096 && ReadMetadata(xr_string_view(static_cast<const char*>(Sidecar->pointer()), size_t(Sidecar->length())), Values) &&
		Values[KeyVersion] == 1.0 && (Values[KeyBits] == 8.0 || Values[KeyBits] == 16.0) && IsWhole(Values[KeyWidth], 2, MaxSize) && IsWhole(Values[KeyHeight], 2, MaxSize) &&
		IsWhole(Values[KeyCrc], 0, double(~0u)) && IsReal(Values[KeyOriginX]) && IsReal(Values[KeyOriginZ]) && IsReal(Values[KeyMinY]) && IsReal(Values[KeyRangeY]) &&
		Values[KeyStepX] > 1e-5 && IsReal(Values[KeyStepX]) && Values[KeyStepZ] < -1e-5 && IsReal(Values[KeyStepZ]) && Values[KeyRangeY] >= 0.001 && Image;
	const u32 Bits = IsValid ? u32(Values[KeyBits]) : 0u;
	const ERHI_FORMAT Format = Bits == 16 ? ERHI_FORMAT::R16_UNORM : ERHI_FORMAT::R8_UNORM;
	const u8* Bytes = Image ? static_cast<const u8*>(Image->pointer()) : nullptr;
	const size_t Size = Image ? size_t(Image->length()) : 0;

	if (!IsValid || !IsHeightImage(Bytes, Size, u32(Values[KeyWidth]), u32(Values[KeyHeight]), Format) || Checksum(Bytes, Size) != u32(Values[KeyCrc]))
	{
		Msg("! Heightfield shadows: invalid or incomplete textures\\%s.ltx / .dds, the level has the cascades alone", Name.c_str());
		return false;
	}

	const float OriginX = float(Values[KeyOriginX]);
	const float OriginZ = float(Values[KeyOriginZ]);
	const float StepX = float(Values[KeyStepX]);
	const float StepZ = float(Values[KeyStepZ]);
	const float MinY = float(Values[KeyMinY]);
	const float RangeY = float(Values[KeyRangeY]);
	RHITextureDesc Desc;
	Desc.Width = u32(Values[KeyWidth]);
	Desc.Height = u32(Values[KeyHeight]);
	Desc.Format = Format;
	Desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;
	RHISubResource Level = {};
	Level.Data = Bytes + DdsHeader;
	Level.RowPitch = Desc.Width * (Bits / 8);
	Heights.create("$user$heightfield_height");
	Heights->Make(Desc, &Level);

	if (!Heights->BoundView())
	{
		Msg("! Heightfield shadows: the heightfield of '%s' could not be made, the level has the cascades alone", LevelName);
		Heights->surface_set(nullptr);
		Heights = nullptr;
		return false;
	}

	// A code of zero is a hole; one to the last code span the heights
	const float MaxCode = float((1u << Bits) - 1u);
	const float Step = RangeY / (MaxCode - 1.0f);
	WorldToGrid.set(OriginX, OriginZ, 1.0f / StepX, 1.0f / StepZ);
	Decode.set(Step * MaxCode, MinY - Step, Step, MaxCode);
	Width = Desc.Width;
	Height = Desc.Height;
	Msg("* Heightfield shadows: '%s', %ux%u R%u, Y=[%.3f, %.3f], prefix scan", LevelName, Width, Height, Bits, MinY, MinY + RangeY);
	return true;
}

// The grid is scanned in rows along the sun's major axis, sheared by its minor one, every frame
void HeightfieldShadowMap::Render(GfxCmdList& Cmd)
{
	Cmd.Constants.Write<CBLight>().HeightfieldParams.set(0.0f, 0.0f, 0.0f, 0.0f);
	const light* Sun = static_cast<const light*>(RImplementation.Lights.sun_adapted._get());

	// Baked light has its own sun past the cascades
	if (!g_HeightfieldShadows || !ForwardDynamicLight() || !Sun || !g_pGameLevel || Device.IsEditorMode())
	{
		return;
	}

	if (!IsRead)
	{
		IsRead = true;
		Load(g_pGameLevel->name().c_str());
	}

	Fvector Direction = Sun->direction;
	Direction.normalize_safe();

	if (!Heights || !std::isfinite(Direction.x) || !std::isfinite(Direction.y) || !std::isfinite(Direction.z) || Direction.y >= 0.0f)
	{
		return;
	}

	const float Dx = Direction.x * WorldToGrid.z;
	const float Dz = Direction.z * WorldToGrid.w;
	const bool IsMajorZ = std::abs(Dz) > std::abs(Dx);
	const float Major = IsMajorZ ? Dz : Dx;
	const float Extent = std::abs(Major);
	const float Shear = Extent > 0.0f ? (IsMajorZ ? Dx : Dz) / Extent : 0.0f;
	const u32 MajorSize = IsMajorZ ? Height : Width;
	const u32 MinorSize = IsMajorZ ? Width : Height;
	const u32 Rows = std::min(Width + Height, MinorSize + u32(std::ceil(std::abs(Shear) * float(MajorSize - 1))) + 1);
	// Past the whole span of heights nothing before a texel shadows the next one: a sun overhead needs no division
	const float MaxDrop = Decode.x + 1.0f;
	const float Drop = Extent > 0.0f ? std::min(MaxDrop, -Direction.y / Extent) : MaxDrop;

	if (!std::isfinite(Drop * float(MajorSize)))
	{
		return;
	}

	if (!Passes[0])
	{
		for (u32 Index = 0; Index < std::size(Passes); ++Index)
		{
			ShaderExternalMap Defines;
			const char Digit[2] = { char('0' + Index), 0 };
			Defines["HEIGHTFIELD_PASS"] = Digit;
			Passes[Index] = DEV->_CreateCS("common\\heightfield_shadow", Defines);
		}
	}

	if (!Target)
	{
		const u32 Span = (std::max(Width, Height) + BlockSize - 1) / BlockSize;
		Target.create(R4_RT_HEIGHTFIELD_SHADOW, Width, Height, ERHI_FORMAT::R32_FLOAT, 1, CRT::USE_UAV_FLAG);
		Local.create("$user$heightfield_local", Span * BlockSize, Width + Height, ERHI_FORMAT::R32_FLOAT, 1, CRT::USE_UAV_FLAG);
		Blocks.create("$user$heightfield_blocks", Span, Width + Height, ERHI_FORMAT::R32_FLOAT, 1, CRT::USE_UAV_FLAG);
		Prefix.create("$user$heightfield_prefix", Span, Width + Height, ERHI_FORMAT::R32_FLOAT, 1, CRT::USE_UAV_FLAG);

		if (!Target->pSurface || !Local->pSurface || !Blocks->pSurface || !Prefix->pSurface)
		{
			Msg("! Heightfield shadows: the scan's targets could not be made, the level has the cascades alone");
			Destroy();
			IsRead = true;
			return;
		}
	}

	CBLight& Light = Cmd.Constants.Write<CBLight>();
	Light.HeightfieldWorldToGrid = WorldToGrid;
	Light.HeightfieldDecode = Decode;
	Light.HeightfieldGrid.set(float(Width), float(Height), float(MajorSize), float(Rows));
	Light.HeightfieldParams.set(1.0f, Drop, g_HeightfieldBias + Decode.z, float(g_HeightfieldFiltering));
	Light.HeightfieldScan.set(IsMajorZ ? 1.0f : 0.0f, Major >= 0.0f ? 1.0f : -1.0f, Shear, std::max(Shear, 0.0f) * float(MajorSize - 1));

	// Three passes, each a dispatch of its own: D3D11 drops what a pass writes from the reads of the same pass
	GPU_EVENT(HeightfieldShadows);
	NriPass& Nri = RImplementation.NriPasses;
	const u32 Input = ShaderBindings::Texture("s_heightfield_input");
	const NriBinding HeightRead = { Input, nullptr, 0, Heights->BoundView() };
	const NriBinding LocalWrites[2] = { { 0, Local->pSurface }, { 1, Blocks->pSurface } };
	Nri.Dispatch(Cmd, Passes[0]._get(), &HeightRead, 1, LocalWrites, 2, (MajorSize + BlockSize - 1) / BlockSize, Rows, 1);

	const NriBinding BlockRead = { Input, Blocks->pSurface };
	const NriBinding PrefixWrite = { 0, Prefix->pSurface };
	Nri.Dispatch(Cmd, Passes[1]._get(), &BlockRead, 1, &PrefixWrite, 1, 1, Rows, 1);

	const NriBinding ResolveReads[2] = { { Input, Local->pSurface }, { ShaderBindings::Texture("s_heightfield_blocks"), Prefix->pSurface } };
	const NriBinding TargetWrite = { 0, Target->pSurface };
	Nri.Dispatch(Cmd, Passes[2]._get(), ResolveReads, 2, &TargetWrite, 1, (Width + 7) / 8, (Height + 7) / 8, 1);
	Frame = RFrame.dwFrame;
}
