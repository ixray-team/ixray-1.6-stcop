#include "stdafx.h"
#include "DetailManager.h"

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

void CDetailManager::cache_ReInitialize(u32 Size, float Fade, float Density)
{
	xrSRWLockGuard FieldsGuard(GeneratedFieldsAccess);
	// Centroid
	cache_Free();
	cache_Alloc(Size, Fade, Density);

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
		// Read disk once on level load; live changes reuse unchanged fields from memory.
		if (!m_detail_layers_baking || !DetailLayers_LoadFromBake())
		{
			const bool Changed = !ClusterFieldValid || ClusterFieldKey != GetClusterFieldKey() || !FMBFieldValid || FMBFieldKey != GetFMBFieldKey();
			BuildClusterField();
			BuildFMBField();
			if (m_detail_layers_baking || Changed)
			{
				// Save clean generated fields before applying user brush deltas.
				DetailLayers_SaveToBake();
			}
		}
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

void CDetailManager::UnpackSlot(int gx, int gz, Slot* D)
{
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
	++D->Generation;
	D->vis.clear();

	D->vis.box.min.set(sx*dm_slot_size, DS.r_ybase(), sz*dm_slot_size);
	D->vis.box.max.set(D->vis.box.min.x+dm_slot_size, DS.r_ybase()+DS.r_yheight(), D->vis.box.min.z+dm_slot_size);
	D->vis.box.grow(EPS_L);
	D->vis.box.getsphere(D->vis.sphere.P, D->vis.sphere.R);

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
	}
}

void CDetailManager::cache_Update(const Fvector& view, const Fvector& Direction, u32 FrameTime)
{
	PROF_EVENT("cache_Update");
	xrSRWLockGuard FieldsGuard(GeneratedFieldsAccess, true);

	// Deferred brush rebuild (requested from the render thread's editor). Runs here on
	// the vegetation producer so schedule_Update/UnpackSlot/UnpackSlotItems and the item
	// pool are only ever touched by this single thread - no more cross-thread pool
	// corruption when the brush circle sits over slots the streaming pipeline is shifting.
	if (m_brush_rebuild_around.exchange(false, std::memory_order_acquire))
		DetailLayers_RebuildSlotsAround(m_brush_rebuild_center, m_brush_rebuild_radius);

	int v_x = iFloor(view.x / dm_slot_size + .5f);
	int v_z = iFloor(view.z / dm_slot_size + .5f);

	const bool ResetWindow = !StreamingInitialized || std::abs(v_x - cache_cx) >= int(dm_cache_line) || std::abs(v_z - cache_cz) >= int(dm_cache_line);
	bool bUpdateSlideWindow = m_slide_window_dirty.exchange(false, std::memory_order_acquire);
	if (ResetWindow)
	{
		// Rebase once after a cold start or teleport, rather than shifting every intervening row.
		cache_cx = v_x;
		cache_cz = v_z;
		for (u32 Z = 0; Z < dm_cache_line; ++Z)
		{
			for (u32 X = 0; X < dm_cache_line; ++X)
			{
				UnpackSlot(X, Z, cache[Z][X]);
			}
		}
		bUpdateSlideWindow = true;
	}
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

	// Keep ready slots in the bounded grid, including those behind the camera.
	// Only pending slots are ranked; no instance cache is read from or written to disk.
	{
		PROF_EVENT("cache_Decompress");
		const bool EditorMode = Device.IsEditorMode();
		const float Prefetch = EditorMode ? 0.f : 4.f * dm_slot_size;
		const float PreparationRadius = dm_fade + Prefetch + dm_slot_size;
		const float PreparationRadiusSq = PreparationRadius * PreparationRadius;
		Fvector Prediction = view;
		if (!ResetWindow && FrameTime > StreamingPreviousTime)
		{
			Fvector Travel;
			Travel.sub(view, StreamingPreviousView);
			Travel.y = 0;
			Travel.mul(.35f * 1000.f / float(FrameTime - StreamingPreviousTime));
			const float Length = Travel.magnitude();
			if (Length > Prefetch && Length > EPS_L)
			{
				Travel.mul(Prefetch / Length);
			}
			Prediction.add(Travel);
		}
		for (Slot* Pending : unpacked_slots)
		{
			VERIFY(Pending->type == stPending);
			Fvector Centre;
			Pending->vis.box.getcenter(Centre);
			const float DX = Centre.x - view.x;
			const float DZ = Centre.z - view.z;
			const float DistanceSq = DX * DX + DZ * DZ;
			if (!EditorMode && DistanceSq > PreparationRadiusSq)
			{
				Pending->StreamingPriority = flt_max;
				continue;
			}
			// Stable spatial variation avoids a strict radial ordering of equally important cells.
			u32 Hash = u32(iFloor(Centre.x / dm_slot_size)) * 0x9e3779b9u ^ u32(iFloor(Centre.z / dm_slot_size)) * 0x85ebca6bu;
			Hash ^= Hash >> 16;
			const float PX = Centre.x - Prediction.x;
			const float PZ = Centre.z - Prediction.z;
			float Priority = DistanceSq * (.85f + .3f * float(Hash & 65535) / 65535.f) + .25f * (PX * PX + PZ * PZ);
			if (DX * Direction.x + DZ * Direction.z > 0)
			{
				Priority *= .8f;
			}
			if (DistanceSq < 24.f * 24.f)
			{
				Priority -= 1000000.f;
			}
			Pending->StreamingPriority = Priority;
		}
		auto LessUrgent = [](const Slot* A, const Slot* B)
		{
			return A->StreamingPriority > B->StreamingPriority;
		};
		std::make_heap(unpacked_slots.begin(), unpacked_slots.end(), LessUrgent);
		CTimer Budget;
		Budget.Start();
		u32 Processed = 0;
		while (!unpacked_slots.empty() && unpacked_slots.front()->StreamingPriority < flt_max)
		{
			std::pop_heap(unpacked_slots.begin(), unpacked_slots.end(), LessUrgent);
			Slot* Next = unpacked_slots.back();
			unpacked_slots.pop_back();
			UnpackSlotItems(Next);
			++Processed;
			// Finish the initial visible area before publishing it, as in the old full unpack.
			// Routine streaming yields after whole slots; one expensive slot may exceed the budget.
			if (!ResetWindow && (Processed >= 128 || Budget.GetElapsed_ms_f() >= (EditorMode ? 4.f : 2.f)))
			{
				break;
			}
		}
		bUpdateSlideWindow = bUpdateSlideWindow || Processed != 0;
		StreamingPreviousView = view;
		StreamingPreviousTime = FrameTime;
		StreamingInitialized = true;
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
			MS.vis.clear();
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

// Publish a per-stroke slot rebuild to the vegetation producer (see cache_Update).
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
