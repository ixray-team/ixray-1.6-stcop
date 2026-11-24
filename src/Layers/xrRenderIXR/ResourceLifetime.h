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

#include <atomic>

namespace ResourceLifetime
{
template <typename T>
inline bool IsAlive(const T* Resource)
{
	return Resource->dwReference.load(std::memory_order_acquire) != 0;
}

template <typename T, typename RefType>
bool TryAcquire(T* Resource, RefType& Out)
{
	auto Count = Resource->dwReference.load(std::memory_order_acquire);
	do
	{
		if (Count == 0)
		{
			return false;
		}
	} while (!Resource->dwReference.compare_exchange_weak(Count, Count + 1, std::memory_order_acq_rel, std::memory_order_acquire));

	RefType(Resource, false).swap(Out);
	return true;
}
}  
