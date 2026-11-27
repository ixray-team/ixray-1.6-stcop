#include "stdafx.h"
#include "DetailManager.h"
#include "GrassThin.h"

//--------------------------------------------------- Decompression
static int magic4x4[4][4] =
{
 	{ 0, 14,  3, 13},
	{11,  5,  8,  6},
	{12,  2, 15,  1},
	{ 7,  9,  4, 10}
};

void bwdithermap	(int levels, int magic[16][16])
{
	/* Get size of each step */
    float N = 255.0f / (levels - 1);

	/*
	* Expand 4x4 dither pattern to 16x16.  4x4 leaves obvious patterning,
	* and doesn't give us full intensity range (only 17 sublevels).
	*
	* magicfact is (N - 1)/16 so that we get numbers in the matrix from 0 to
	* N - 1: mod N gives numbers in 0 to N - 1, don't ever want all
	* pixels incremented to the next level (this is reserved for the
	* pixel value with mod N == 0 at the next level).
	*/

    float	magicfact = (N - 1) / 16;
    for ( int i = 0; i < 4; i++ )
		for ( int j = 0; j < 4; j++ )
			for ( int k = 0; k < 4; k++ )
				for ( int l = 0; l < 4; l++ )
					magic[4*k+i][4*l+j] =
					(int)(0.5 + magic4x4[i][j] * magicfact +
					(magic4x4[k][l] / 16.) * magicfact);
}
//--------------------------------------------------- Decompression

void CDetailManager::cache_ReInitialize()
{
	// Centroid
	cache_Free();
	cache_Alloc();

	bwdithermap(2, dither);
	cache_cx = 0;
	cache_cz = 0;
	if (Device.IsEditorMode())
	{
		BuildClusterField();
		BuildFMBField();
	}
	else
	{
	// Reuse the baked fields when the level + settings still match; otherwise regenerate
	// and, during an actual level Load(), persist the fresh bake for the next session.
	// Runtime-only rebuilds (console changes) stay in memory so a momentary tweak cannot
	// corrupt the on-disk bake; it is overwritten on the next level load instead.
	if (DetailLayers_LoadFromBake())
	{
		// fields already populated from disk
	}
	else
	{
		BuildClusterField();
		BuildFMBField();
		if (m_detail_layers_baking)
			DetailLayers_SaveToBake();
	}
	// Snapshot the generated/baked fields, then overlay the sparse user strokes
	// (user_mask_*.bin) so painted cells overwrite the generator and untouched ones keep it.
	DetailLayers_CommitBaseFields();
	DetailLayers_ApplyUserMasks();
	}

	// Initialize cache-grid
	for (u32 i = 0; i < dm_cache_line; i++)
	{
	    for (u32 j = 0; j < dm_cache_line; j++)
	    {
	        Slot* slt = &cache_pool[i * dm_cache_line + j];
	
	        cache[i][j] = slt;
			UnpackSlot(j, i, slt);
	    }
	}

	u32 max_index = dm_slide_window_line*dm_slide_window_line;
	for (u32 index = 0; index < max_index; index++)
	{
		u32 _mz = index / dm_slide_window_line;
		u32 _mx = index % dm_slide_window_line;
		SlideSlot& MS = slide_window[_mz][_mx];
		for (int i = 0; i < dm_cache_count; i++)
		{
			int _z = i / dm_slide_window_count;
			int _x = i % dm_slide_window_count;
			MS.slots[_z * dm_slide_window_count + _x] = &cache[_mz * dm_slide_window_count + _z][_mx * dm_slide_window_count + _x];
		}
    }
}

// Sends the slot's blades whose lot is under Kept to the GPU's buffer of them, to the place it has or a new one
void CDetailManager::StoreSlot(Slot& S, float Kept)
{
	StoreScratch.clear();

	for (const SlotPart& Part : S.G)
	{
		for (const auto& List : Part.items)
		{
			for (const CDetail::SlotItem* Item : List)
			{
				if (GrassLot(Item->pos) >= Kept)
				{
					break;
				}

				StoreScratch.push_back(*Item);
			}
		}
	}

	const u32 Count = u32(StoreScratch.size());
	if (Count != S.StoreCount)
	{
		StoreRelease(S);
		S.StoreFirst = Count ? Store.Alloc(Count) : GrassSlotStore::Absent;
		S.StoreCount = S.StoreFirst != GrassSlotStore::Absent ? Count : 0;
	}

	// A slot that found no place is asked again by the next frame that shows it
	S.StoreKept = S.StoreCount == Count ? Kept : 0.0f;
	S.StoreWanted = Count;

	if (S.StoreCount)
	{
		Store.Queue(S.StoreFirst, StoreScratch.data(), Count);
	}
}

void CDetailManager::StoreRelease(Slot& S)
{
	if (S.StoreCount)
	{
		Store.Free(S.StoreFirst, S.StoreCount);
	}

	S.StoreFirst = GrassSlotStore::Absent;
	S.StoreCount = 0;
	S.StoreKept = 0.0f;
}

void CDetailManager::UnpackSlot(int gx, int gz, Slot* D)
{
	StoreRelease(*D);

	int sx = cg2w_X(gx);
	int sz = cg2w_Z(gz);
	DetailSlot&	DS = QueryDB(sx,sz);

	D->empty = (DS.id0==DetailSlot::ID_Empty)&&
				(DS.id1==DetailSlot::ID_Empty)&&
				(DS.id2==DetailSlot::ID_Empty)&&
				(DS.id3==DetailSlot::ID_Empty);

	// Unpacking
	u32 old_type = D->type;
	D->type = stPending;
	D->DS = &DS;
	D->has_trample = false;

	D->vis.box.min.set(sx*dm_slot_size, DS.r_ybase(), sz*dm_slot_size);
	D->vis.box.max.set(D->vis.box.min.x+dm_slot_size, DS.r_ybase()+DS.r_yheight(), D->vis.box.min.z+dm_slot_size);
	D->vis.box.grow(EPS_L);

	for (u32 i=0; i<dm_obj_in_slot; i++)
	{
		auto& items = D->G[i].items;
		for (u32 i = 0; i < 3; i++)
		{
			for (CDetail::SlotItem* item : items[i])
				items_pool.destroy(item);

			items[i].clear();
		}
	}

	if (old_type != stPending)
	{
		VERIFY(stPending == D->type);
		unpacked_slots.push_back(D);
		PendingClearReach = 0.0f;
	}
}

void CDetailManager::cache_Update(const Fvector& view)
{
	PROF_EVENT("cache_Update");

	// Deferred brush rebuild (requested from the render thread's editor). Runs here on
	// the DetailsTask worker so schedule_Update/UnpackSlot/UnpackSlotItems and the item
	// pool are only ever touched by this single thread - no more cross-thread pool
	// corruption when the brush circle sits over slots the streaming pipeline is shifting.
	if (m_brush_rebuild_around.exchange(false, std::memory_order_acquire))
		DetailLayers_RebuildSlotsAround(m_brush_rebuild_center, m_brush_rebuild_radius);

	int v_x = iFloor(view.x / dm_slot_size + .5f);
	int v_z = iFloor(view.z / dm_slot_size + .5f);

	bool bUpdateSlideWindow = m_slide_window_dirty.exchange(false, std::memory_order_acquire);
	bUpdateSlideWindow = bUpdateSlideWindow || (cache_cx != v_x) || (cache_cz != v_z);
	// *****	Cache shift
	{
		PROF_EVENT("cache_Tasks");
		while (cache_cx != v_x)
		{
			if (v_x > cache_cx)
			{
				// shift matrix to left
				cache_cx++;
				for (u32 z = 0; z < dm_cache_line; z++)
				{
					Slot* S = cache[z][0];
					for (u32 x = 1; x < dm_cache_line; x++)
						cache[z][x - 1] = cache[z][x];
					cache[z][dm_cache_line - 1] = S;
					UnpackSlot(dm_cache_line - 1, z, S);
				}
			}
			else
			{
				// shift matrix to right
				cache_cx--;
				for (u32 z = 0; z < dm_cache_line; z++)
				{
					Slot* S = cache[z][dm_cache_line - 1];
					for (u32 x = dm_cache_line - 1; x > 0; x--)
						cache[z][x] = cache[z][x - 1];
					cache[z][0] = S;
					UnpackSlot(0, z, S);
				}
			}
		}
		while (cache_cz != v_z)
		{
			if (v_z > cache_cz)
			{
				// shift matrix down a bit
				cache_cz++;
				for (u32 x = 0; x < dm_cache_line; x++)
				{
					Slot* S = cache[dm_cache_line - 1][x];
					for (u32 z = dm_cache_line - 1; z > 0; z--)
						cache[z][x] = cache[z - 1][x];
					cache[0][x] = S;
					UnpackSlot(x, 0, S);
				}
			}
			else
			{
				// shift matrix up
				cache_cz--;
				for (u32 x = 0; x < dm_cache_line; x++)
				{
					Slot* S = cache[0][x];
					for (u32 z = 1; z < dm_cache_line; z++)
						cache[z - 1][x] = cache[z][x];
					cache[dm_cache_line - 1][x] = S;
					UnpackSlot(x, dm_cache_line - 1, S);
				}
			}
		}
	}

	// Task performer: the game's is UnpackPending, after the frame's lists
	if (Device.IsEditorMode())
	{
		PROF_EVENT("cache_Decompress");
		bool bFullUnpack = false;
		int limit = 14;
		if (unpacked_slots.size() == dm_cache_size)
		{
			limit = dm_cache_size;
			bFullUnpack = true;
		}

		for (int iteration = 0; unpacked_slots.size() && (iteration < limit); iteration++)
		{
			u32 best_id = 0;
			float best_dist = flt_max;

			if (bFullUnpack)
				best_id = unpacked_slots.size() - 1;
			else
			{
				for (u32 entry = 0; entry < unpacked_slots.size(); entry++)
				{
					// Gain access to data
					Slot* S = unpacked_slots[entry];
					VERIFY(stPending == S->type);

					// Estimate
					Fvector C;
					S->vis.box.getcenter(C);
					float D = view.distance_to_sqr(C);

					// Select
					if (D < best_dist)
					{
						best_dist = D;
						best_id = entry;
					}
				}
			}

			// Decompress and remove task
			UnpackSlotItems(unpacked_slots[best_id]);
			unpacked_slots.erase(unpacked_slots.begin() + best_id);
		}
	}

    if (bUpdateSlideWindow)
	{
		PROF_EVENT("MegaUpdate");
		u32 max_index = dm_slide_window_line*dm_slide_window_line;
		for (u32 index = 0; index < max_index; index++)
		{
			u32 _mz = index / dm_slide_window_line;
			u32 _mx = index % dm_slide_window_line;
			SlideSlot& MS = slide_window[_mz][_mx];
			MS.empty = true;
			MS.vis = { Fsphere{ {0.f,0.f,0.f}, 0.f }, Fbox{ { 0.f,0.f,0.f }, { 0.f,0.f,0.f } }};
			bool empty_slot = true;
            for (int _i=0; _i<dm_cache_count; _i++)
			{
				Slot** slots = MS.slots[_i];
				Slot* S = *slots;
				if (!S->empty)
				{
					MS.vis.box.min.min(S->vis.box.min);
					MS.vis.box.max.max(S->vis.box.max);
					empty_slot = false;
				}
            }
			if(!empty_slot)
			{
				MS.vis.box.getsphere(MS.vis.sphere.P, MS.vis.sphere.R);
				MS.empty = false;
			}
        }
    }
}

// The pending slots within Reach of the eye, nearest first, those in view before those twice as near out of it. A task
// takes a millisecond of them, and more while slots in view wait: 20 us a slot in view, up to 8 ms
void CDetailManager::UnpackPending(const Fvector& Eye, float Reach, const CFrustum& View)
{
	PROF_EVENT("cache_Decompress");

	const float Margin = 2.0f * dm_slot_size;
	if (Eye.distance_to(PendingClearAt) + Reach <= PendingClearReach)
	{
		return;
	}

	const float Limit = _sqr(Reach + Margin);
	u64 InView = 0;
	PendingOrder.clear();

	for (Slot* S : unpacked_slots)
	{
		if (S->empty)
		{
			UnpackSlotItems(S);
			continue;
		}

		Fsphere Bounds;
		S->vis.box.getsphere(Bounds.P, Bounds.R);
		const float Distance = Eye.distance_to_sqr(Bounds.P);

		if (Distance <= Limit)
		{
			const bool IsSeen = View.testSphere_dirty(Bounds.P, Bounds.R);
			InView += IsSeen;
			PendingOrder.emplace_back(IsSeen ? Distance : Distance * 4.0f, S);
		}
	}

	const auto Later = [](const std::pair<float, Slot*>& A, const std::pair<float, Slot*>& B) { return A.first > B.first; };
	std::make_heap(PendingOrder.begin(), PendingOrder.end(), Later);
	const u64 Budget = std::min<u64>(1000 + InView * 20, 8000);
	CTimer Timer;
	Timer.Start();

	const bool IsUnbounded = Core.ParamsData.test(ECoreParams::autotest_dt);

	for (u32 Count = 0; !PendingOrder.empty() && (Count < 4 || IsUnbounded || Timer.GetElapsed_mcs() < Budget); ++Count)
	{
		std::pop_heap(PendingOrder.begin(), PendingOrder.end(), Later);
		UnpackSlotItems(PendingOrder.back().second);
		PendingOrder.pop_back();
	}

	if (PendingOrder.empty())
	{
		PendingClearAt = Eye;
		PendingClearReach = Reach + Margin;
	}

	std::erase_if(unpacked_slots, [](const Slot* S) { return S->type != stPending; });
}

void CDetailManager::DetailLayers_RebuildSlotsAround(const Fvector& center, float radius)
{
	if (cache.empty() || cache.front().size() != dm_cache_line)
		return;

	const int expand = iCeil(radius / dm_slot_size) + 1;
	const int slot_x = iFloor(center.x / dm_slot_size);
	const int slot_z = iFloor(center.z / dm_slot_size);

	for (int sz = slot_z - expand; sz <= slot_z + expand; sz++)
	{
		for (int sx = slot_x - expand; sx <= slot_x + expand; sx++)
		{
			// World slot cell [sx*slot, (sx+1)*slot); quick circle rejection on the center.
			const float cx = ((float)sx + 0.5f) * dm_slot_size;
			const float cz = ((float)sz + 0.5f) * dm_slot_size;
			const float dx = cx - center.x;
			const float dz = cz - center.z;
			if ((dx * dx + dz * dz) > radius * radius)
				continue;

			const int gx = w2cg_X(sx);
			const int gz = w2cg_Z(sz);
			if (gx < 0 || gz < 0 || gx >= (int)dm_cache_line || gz >= (int)dm_cache_line)
				continue;

			Slot*& S = cache[gz][gx];
			if (!S || S->empty)
				continue;
			if (S->type == stPending)
				continue; // queued by streaming; will read fresh masks

			UnpackSlot(gx, gz, S);
			// UnpackSlot re-queued it; decompress right away instead of letting the
			// pipeline redo it (double decompress would leak slot items).
			auto it = std::find(unpacked_slots.begin(), unpacked_slots.end(), S);
			if (it != unpacked_slots.end())
				unpacked_slots.erase(it);
			UnpackSlotItems(S);
		}
	}

	RequestSlideWindowUpdate();
}

// Publish a per-stroke slot rebuild to the DetailsTask worker (see cache_Update).
// Editor strokes write the mask one frame and the rebuild kicks in on the same frame
// the worker next runs - effectively immediate, but pooled/queued on the safe thread.
void CDetailManager::DetailLayers_RequestRebuildAround(const Fvector& center, float radius)
{
	m_brush_rebuild_center = center;
	m_brush_rebuild_radius = radius;
	m_brush_rebuild_around.store(true, std::memory_order_release);
}

DetailSlot&	CDetailManager::QueryDB(int sx, int sz)
{
	int db_x = sx+dtH.offs_x;
	int db_z = sz+dtH.offs_z;
	if ((db_x>=0) && (db_x<int(dtH.size_x)) && (db_z>=0) && (db_z<int(dtH.size_z)))
	{
		u32 linear_id = db_z*dtH.size_x + db_x;
		return dtSlots[linear_id];
	}
	else
	{
		// Empty slot
		DS_empty.w_id(0,DetailSlot::ID_Empty);
		DS_empty.w_id(1,DetailSlot::ID_Empty);
		DS_empty.w_id(2,DetailSlot::ID_Empty);
		DS_empty.w_id(3,DetailSlot::ID_Empty);
		return DS_empty;
	}
}

bool CDetailManager::HasGrassNear(const Fvector& Centre, float Radius) const
{
	if (!dtSlots)
	{
		return false;
	}

	const int FirstX = std::max(iFloor((Centre.x - Radius) / dm_slot_size) + dtH.offs_x, 0);
	const int LastX = std::min(iFloor((Centre.x + Radius) / dm_slot_size) + dtH.offs_x, int(dtH.size_x) - 1);
	const int FirstZ = std::max(iFloor((Centre.z - Radius) / dm_slot_size) + dtH.offs_z, 0);
	const int LastZ = std::min(iFloor((Centre.z + Radius) / dm_slot_size) + dtH.offs_z, int(dtH.size_z) - 1);

	for (int Z = FirstZ; Z <= LastZ; ++Z)
	{
		for (int X = FirstX; X <= LastX; ++X)
		{
			DetailSlot& Slot = dtSlots[u32(Z) * dtH.size_x + u32(X)];

			if (Slot.id0 == DetailSlot::ID_Empty && Slot.id1 == DetailSlot::ID_Empty && Slot.id2 == DetailSlot::ID_Empty && Slot.id3 == DetailSlot::ID_Empty)
			{
				continue;
			}

			const Fvector Low = { float(X - dtH.offs_x) * dm_slot_size, Slot.r_ybase(), float(Z - dtH.offs_z) * dm_slot_size };
			const Fvector Nearest = { std::clamp(Centre.x, Low.x, Low.x + dm_slot_size), std::clamp(Centre.y, Low.y, Low.y + Slot.r_yheight()), std::clamp(Centre.z, Low.z, Low.z + dm_slot_size) };

			if (Nearest.distance_to_sqr(Centre) <= Radius * Radius)
			{
				return true;
			}
		}
	}

	return false;
}
