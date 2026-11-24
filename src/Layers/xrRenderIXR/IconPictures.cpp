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
#include "IconPictures.h"

namespace
{
bool IsSame(const Irect& Left, const Irect& Right)
{
	return Left.x1 == Right.x1 && Left.y1 == Right.y1 && Left.x2 == Right.x2 && Left.y2 == Right.y2;
}

// Scissor rectangles end before x2 and y2: neighbours that touch keep each other
bool Overlaps(const Irect& Left, const Irect& Right)
{
	return Left.x1 < Right.x2 && Right.x1 < Left.x2 && Left.y1 < Right.y2 && Right.y1 < Left.y2;
}
} // namespace

bool IconPictures::IsKept(const Irect& Place, u32 Key, u32 Frame)
{
	for (u32 Index = 0; Index < Count; ++Index)
	{
		Picture& Item = Pictures[Index];

		if (Item.Key == Key && IsSame(Item.Place, Place))
		{
			Item.Frame = Frame;
			return true;
		}
	}

	return false;
}

u32 IconPictures::Drawn(const Irect& Place, u32 Key, u32 Frame, bool IsWhole)
{
	u32 Lost = 0;

	for (u32 Index = 0; Index < Count;)
	{
		const bool IsOwn = IsSame(Pictures[Index].Place, Place);

		if (!IsOwn && !Overlaps(Pictures[Index].Place, Place))
		{
			++Index;
			continue;
		}

		Lost += !IsOwn;
		Pictures[Index] = Pictures[--Count];
	}

	if (!IsWhole)
	{
		return Lost;
	}

	if (Count == Capacity)
	{
		u32 Oldest = 0;

		for (u32 Index = 1; Index < Count; ++Index)
		{
			Oldest = Frame - Pictures[Index].Frame > Frame - Pictures[Oldest].Frame ? Index : Oldest;
		}

		Pictures[Oldest] = Pictures[--Count];
		++Lost;
	}

	Pictures[Count++] = { Place, Key, Frame };
	return Lost;
}
