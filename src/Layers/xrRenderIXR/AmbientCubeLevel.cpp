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
#include "AmbientCubeLevel.h"

namespace
{
u32 Word(const u8* Data, size_t Offset)
{
	u32 Value;
	std::memcpy(&Value, Data + Offset, sizeof(Value));
	return Value;
}

float Brightness(u32 Colour)
{
	return (float(Colour >> 11) / 31.0f + float((Colour >> 5) & 63) / 63.0f + float(Colour & 31) / 31.0f) / 3.0f;
}
} // namespace

float AmbientCubeLevel(const char* Name)
{
	string_path Path;

	if (!Name || !Name[0] || !FS.exist(Path, "$game_textures$", Name, ".dds"))
	{
		return AmbientCubeUnknown;
	}

	IReader* File = FS.r_open(Path);

	if (!File)
	{
		return AmbientCubeUnknown;
	}

	const u8* Data = static_cast<const u8*>(File->pointer());
	const size_t Length = size_t(File->length());
	float Level = AmbientCubeUnknown;

	// A cube of DXT1 blocks with all six sides, as the weather's small cubes are
	if (Length > 128 && Word(Data, 0) == 0x20534444 && Word(Data, 84) == 0x31545844 && (Word(Data, 112) & 0xFE00) == 0xFE00)
	{
		const u32 Height = Word(Data, 12);
		const u32 Width = Word(Data, 16);
		const u32 Mips = std::max(Word(Data, 28), 1u);
		const size_t TopBlocks = size_t(std::max(Width, 4u) / 4) * (std::max(Height, 4u) / 4);
		size_t SideBytes = 0;

		for (u32 Mip = 0; Mip < Mips && Mip < 16; ++Mip)
		{
			SideBytes += size_t(std::max(Width >> Mip, 4u) / 4) * (std::max(Height >> Mip, 4u) / 4) * 8;
		}

		if (Width >= 4 && Height >= 4 && 128 + SideBytes * 6 <= Length)
		{
			double Sum = 0.0;
			size_t Count = 0;

			for (u32 Side = 0; Side < 6; ++Side)
			{
				// The fourth side faces the ground: it lights nothing that looks up or sideways
				if (Side == 3)
				{
					continue;
				}

				const u8* Block = Data + 128 + SideBytes * Side;

				for (size_t Index = 0; Index < TopBlocks; ++Index, Block += 8)
				{
					const u32 First = Word(Block, 0) & 0xFFFF;
					const u32 Second = Word(Block, 0) >> 16;
					const u32 Codes = Word(Block, 4);
					float Palette[4] = { Brightness(First), Brightness(Second), 0.0f, 0.0f };
					Palette[2] = First > Second ? (Palette[0] * 2.0f + Palette[1]) / 3.0f : (Palette[0] + Palette[1]) * 0.5f;
					Palette[3] = First > Second ? (Palette[0] + Palette[1] * 2.0f) / 3.0f : 0.0f;

					for (u32 Texel = 0; Texel < 16; ++Texel)
					{
						Sum += Palette[(Codes >> (Texel * 2)) & 3];
					}

					Count += 16;
				}
			}

			Level = Count ? float(Sum / double(Count)) : AmbientCubeUnknown;
		}
	}

	FS.r_close(File);
	return Level;
}
