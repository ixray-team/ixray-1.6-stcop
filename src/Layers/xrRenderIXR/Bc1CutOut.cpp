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
#include "Bc1CutOut.h"

namespace
{
constexpr u32 BlockBytes = 8;

// The colour of a texel times how much of it is not a hole, and that share
struct Texel
{
	float Colour[3];
	float Cover;
};

u16 ReadEnd(const u8* Block, u32 End)
{
	u16 Value;
	std::memcpy(&Value, Block + End * 2, sizeof(Value));
	return Value;
}

u32 ReadPicks(const u8* Block)
{
	u32 Value;
	std::memcpy(&Value, Block + 4, sizeof(Value));
	return Value;
}

size_t LevelBytes(u32 Width, u32 Height, u32 Level)
{
	return size_t((Width >> Level) / 4) * ((Height >> Level) / 4) * BlockBytes;
}

// The fourth colour of a block whose first end is not above its second is a hole
bool MakePalette(const u16 Ends[2], float Palette[4][3])
{
	for (u32 End = 0; End < 2; ++End)
	{
		const u32 Red = Ends[End] >> 11;
		const u32 Green = (Ends[End] >> 5) & 63;
		const u32 Blue = Ends[End] & 31;
		Palette[End][0] = float((Red << 3) | (Red >> 2));
		Palette[End][1] = float((Green << 2) | (Green >> 4));
		Palette[End][2] = float((Blue << 3) | (Blue >> 2));
	}

	const bool HasHole = Ends[0] <= Ends[1];

	for (u32 Channel = 0; Channel < 3; ++Channel)
	{
		const float First = Palette[0][Channel];
		const float Second = Palette[1][Channel];
		Palette[2][Channel] = HasHole ? (First + Second) * 0.5f : (First * 2.0f + Second) / 3.0f;
		Palette[3][Channel] = HasHole ? 0.0f : (First + Second * 2.0f) / 3.0f;
	}

	return HasHole;
}

u16 HolesOf(const u8* Block)
{
	if (ReadEnd(Block, 0) > ReadEnd(Block, 1))
	{
		return 0;
	}

	const u32 Picks = ReadPicks(Block);
	u16 Holes = 0;

	for (u32 Index = 0; Index < 16; ++Index)
	{
		Holes |= u16((((Picks >> (Index * 2)) & 3) == 3 ? 1u : 0u) << Index);
	}

	return Holes;
}

u16 PackEnd(const float Colour[3])
{
	return u16((std::lround(Colour[0] * 31.0f / 255.0f) << 11) | (std::lround(Colour[1] * 63.0f / 255.0f) << 5) | std::lround(Colour[2] * 31.0f / 255.0f));
}

// The ends are the corners of the box around the colours, each texel takes the nearest colour between them
void Encode(u8* Block, const float Colours[16][3], u16 Holes)
{
	float Low[3] = { 255.0f, 255.0f, 255.0f };
	float High[3] = { 0.0f, 0.0f, 0.0f };

	for (u32 Index = 0; Index < 16; ++Index)
	{
		for (u32 Channel = 0; Channel < 3 && !((Holes >> Index) & 1); ++Channel)
		{
			Low[Channel] = std::min(Low[Channel], Colours[Index][Channel]);
			High[Channel] = std::max(High[Channel], Colours[Index][Channel]);
		}
	}

	u16 Ends[2] = { PackEnd(High), PackEnd(Low) };

	if (Holes ? Ends[0] > Ends[1] : Ends[0] < Ends[1])
	{
		std::swap(Ends[0], Ends[1]);
	}

	float Palette[4][3];
	const u32 Count = MakePalette(Ends, Palette) ? 3 : 4;
	u32 Picks = 0;

	for (u32 Index = 0; Index < 16; ++Index)
	{
		u32 Pick = 3;
		float Nearest = FLT_MAX;

		for (u32 Entry = 0; Entry < Count && !((Holes >> Index) & 1); ++Entry)
		{
			const float Red = Colours[Index][0] - Palette[Entry][0];
			const float Green = Colours[Index][1] - Palette[Entry][1];
			const float Blue = Colours[Index][2] - Palette[Entry][2];
			const float Distance = Red * Red + Green * Green + Blue * Blue;

			if (Distance < Nearest)
			{
				Nearest = Distance;
				Pick = Entry;
			}
		}

		Picks |= Pick << (Index * 2);
	}

	std::memcpy(Block, Ends, sizeof(Ends));
	std::memcpy(Block + 4, &Picks, sizeof(Picks));
}
} // namespace

bool Bc1CutOut::FixMips(const u8* Blocks, u32 Width, u32 Height, u32 Levels, u32 ScanLevel, xr_vector<u8>& Fixed)
{
	if (Levels < 2 || ScanLevel >= Levels || (Width >> (Levels - 1)) < 4 || (Height >> (Levels - 1)) < 4)
	{
		return false;
	}

	size_t ScanOffset = 0;
	size_t Total = 0;

	for (u32 Level = 0; Level < Levels; ++Level)
	{
		ScanOffset = Level == ScanLevel ? Total : ScanOffset;
		Total += LevelBytes(Width, Height, Level);
	}

	bool HasHoles = false;

	for (size_t Offset = 0; Offset < LevelBytes(Width, Height, ScanLevel) && !HasHoles; Offset += BlockBytes)
	{
		HasHoles = HolesOf(Blocks + ScanOffset + Offset) != 0;
	}

	if (!HasHoles)
	{
		return false;
	}

	PROF_EVENT("Bc1CutOutFixMips");
	Fixed.assign(Blocks, Blocks + Total);

	// The second level's texels, gathered from the first's blocks: each block covers two by two of them
	u32 Wide = Width / 2;
	u32 High = Height / 2;
	xr_vector<Texel> Mean(size_t(Wide) * High, Texel{});
	xr_vector<Texel> Next;

	for (u32 Row = 0; Row < Height / 4; ++Row)
	{
		for (u32 Column = 0; Column < Width / 4; ++Column)
		{
			const u8* Block = Blocks + (size_t(Row) * (Width / 4) + Column) * BlockBytes;
			const u16 Ends[2] = { ReadEnd(Block, 0), ReadEnd(Block, 1) };
			const u32 Picks = ReadPicks(Block);
			float Palette[4][3];
			const bool HasHole = MakePalette(Ends, Palette);

			for (u32 Index = 0; Index < 16; ++Index)
			{
				const u32 Pick = (Picks >> (Index * 2)) & 3;

				if (HasHole && Pick == 3)
				{
					continue;
				}

				Texel& To = Mean[(size_t(Row) * 2 + (Index >> 3)) * Wide + Column * 2 + ((Index >> 1) & 1)];
				To.Colour[0] += Palette[Pick][0] * 0.25f;
				To.Colour[1] += Palette[Pick][1] * 0.25f;
				To.Colour[2] += Palette[Pick][2] * 0.25f;
				To.Cover += 0.25f;
			}
		}
	}

	size_t Offset = LevelBytes(Width, Height, 0);

	for (u32 Level = 1; Level < Levels; ++Level)
	{
		for (u32 Row = 0; Row < High / 4; ++Row)
		{
			for (u32 Column = 0; Column < Wide / 4; ++Column)
			{
				u8* Block = &Fixed[Offset + (size_t(Row) * (Wide / 4) + Column) * BlockBytes];
				const u16 Ends[2] = { ReadEnd(Block, 0), ReadEnd(Block, 1) };
				const u32 Picks = ReadPicks(Block);
				const u16 Holes = HolesOf(Block);
				float Palette[4][3];
				float Colours[16][3];
				bool IsDarkened = false;
				MakePalette(Ends, Palette);

				for (u32 Index = 0; Index < 16; ++Index)
				{
					const Texel& From = Mean[(size_t(Row) * 4 + (Index >> 2)) * Wide + Column * 4 + (Index & 3)];
					const bool IsKnown = From.Cover > 0.0f && !((Holes >> Index) & 1);
					IsDarkened = IsDarkened || (IsKnown && From.Cover < 0.996f);

					for (u32 Channel = 0; Channel < 3; ++Channel)
					{
						Colours[Index][Channel] = IsKnown ? From.Colour[Channel] / From.Cover : Palette[(Picks >> (Index * 2)) & 3][Channel];
					}
				}

				if (IsDarkened)
				{
					Encode(Block, Colours, Holes);
				}
			}
		}

		Offset += LevelBytes(Width, Height, Level);

		if (Level + 1 == Levels)
		{
			break;
		}

		Next.assign(size_t(Wide / 2) * (High / 2), Texel{});

		for (u32 Row = 0; Row < High; ++Row)
		{
			for (u32 Column = 0; Column < Wide; ++Column)
			{
				const Texel& From = Mean[size_t(Row) * Wide + Column];
				Texel& To = Next[size_t(Row / 2) * (Wide / 2) + Column / 2];
				To.Colour[0] += From.Colour[0] * 0.25f;
				To.Colour[1] += From.Colour[1] * 0.25f;
				To.Colour[2] += From.Colour[2] * 0.25f;
				To.Cover += From.Cover * 0.25f;
			}
		}

		Mean.swap(Next);
		Wide /= 2;
		High /= 2;
	}

	return true;
}
