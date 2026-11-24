/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Core
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

// Where the primary thread is in a frame that takes too long: a watching thread samples its call stack every
// millisecond of every frame, and writes the stacks of a frame that ended late, most frequent first, to a report.
// The threads the primary one waits for are sampled with it: a frame is often late because one of them is
namespace FrameStackSampler
{
// Folder: where the reports go. LimitMs: the least a frame has to take to be reported; three times the usual frame
// when that is more
XRCORE_API void Start(const char* Folder, float LimitMs);
XRCORE_API void Stop();
// Called by the primary thread where its frame begins
XRCORE_API void BeginFrame();
// Called once by a thread that is to be sampled beside the primary one; a few are followed
XRCORE_API void Follow(const char* Name);
} // namespace FrameStackSampler
