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
#pragma once

// A BC1 texture with cut-out texels holds black in its holes, and its file's mips averaged that black into the
// colours beside them while their alpha was rounded back to all or nothing: the darkening cannot be divided out
namespace Bc1CutOut
{
// Blocks: the levels of a file one after another, the first Width x Height, each a multiple of four texels a side.
// Fixed takes a copy whose levels below the first have the mean colour of the texels of the first that are not
// holes, in the blocks where that differs; the holes of every level stay where the file has them. False, and Fixed
// untouched, where the level ScanLevel has no hole
bool FixMips(const u8* Blocks, u32 Width, u32 Height, u32 Levels, u32 ScanLevel, xr_vector<u8>& Fixed);
} // namespace Bc1CutOut
