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
#include "DetailModel.h"

// Where the blades of the detail slots stand in the GPU's buffer of them: a slot's blades are sent once, when the
// slot is unpacked or trampled, and a frame names runs of them. The details worker owns the places; the render
// thread only takes what waits to be sent
class GrassSlotStore
{
public:
	static constexpr u32 Absent = u32(-1);
	static constexpr u32 Bits = 19; // SCENE_GRASS_STORE_BITS
	static constexpr u32 Capacity = 1u << Bits; // blades

	void Reset()
	{
		xrCriticalSectionGuard Guard(Lock);
		Holes.clear();
		Freed.clear();
		Ranges.clear();
		Items.clear();
		End = 0;
	}

	// A place freed by the task before may still be named by the frame that task made: it is free from this one on
	void BeginTask()
	{
		for (const Range& Block : Freed)
		{
			Release(Block);
		}

		Freed.clear();
	}

	u32 Alloc(u32 Count)
	{
		for (size_t Index = 0; Index < Holes.size(); ++Index)
		{
			Range& Hole = Holes[Index];
			if (Hole.Count < Count)
			{
				continue;
			}

			const u32 First = Hole.First;
			Hole.First += Count;
			Hole.Count -= Count;

			if (!Hole.Count)
			{
				Holes.erase(Holes.begin() + Index);
			}

			return First;
		}

		if (End + Count > Capacity)
		{
			return Absent;
		}

		End += Count;
		return End - Count;
	}

	void Free(u32 First, u32 Count)
	{
		Freed.push_back({ First, Count });
	}

	void Queue(u32 First, const CDetail::SlotItem* Blades, u32 Count)
	{
		xrCriticalSectionGuard Guard(Lock);
		Ranges.push_back({ First, Count });
		Items.insert(Items.end(), Blades, Blades + Count);
	}

	// Upload(First, Blades, Count) for everything queued, in the order it was
	template <class Fn>
	void Drain(Fn&& Upload)
	{
		{
			xrCriticalSectionGuard Guard(Lock);
			Ranges.swap(SentRanges);
			Items.swap(SentItems);
		}

		size_t At = 0;
		for (const Range& Block : SentRanges)
		{
			Upload(Block.First, SentItems.data() + At, Block.Count);
			At += Block.Count;
		}

		SentRanges.clear();
		SentItems.clear();
	}

private:
	struct Range
	{
		u32 First;
		u32 Count;
	};

	void Release(Range Block)
	{
		auto After = std::lower_bound(Holes.begin(), Holes.end(), Block.First, [](const Range& Hole, u32 First) { return Hole.First < First; });

		if (After != Holes.end() && Block.First + Block.Count == After->First)
		{
			After->First = Block.First;
			After->Count += Block.Count;
		}
		else
		{
			After = Holes.insert(After, Block);
		}

		if (After != Holes.begin() && (After - 1)->First + (After - 1)->Count == After->First)
		{
			(After - 1)->Count += After->Count;
			After = Holes.erase(After) - 1;
		}

		if (After->First + After->Count == End)
		{
			End = After->First;
			Holes.erase(After);
		}
	}

	xr_vector<Range> Holes; // by place
	xr_vector<Range> Freed;
	u32 End = 0;
	xrCriticalSection Lock;
	xr_vector<Range> Ranges;
	xr_vector<CDetail::SlotItem> Items;
	xr_vector<Range> SentRanges;
	xr_vector<CDetail::SlotItem> SentItems;
};
