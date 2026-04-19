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

ICF void bwdithermap	(int levels, int magic[16][16])
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
	}
}

void CDetailManager::cache_Update(const Fvector& view)
{
	PROF_EVENT("cache_Update");
	int v_x = iFloor(view.x / dm_slot_size + .5f);
	int v_z = iFloor(view.z / dm_slot_size + .5f);

	bool bUpdateSlideWindow = (cache_cx != v_x) || (cache_cz != v_z);
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

	// Task performer
	{
		PROF_EVENT("cache_Decompress");
		if (!ps_r2_ls_flags.test(R2FLAG_FAST_DETAILS_UPDATE))
		{
			bool bFullUnpack = false;
			int limit = dm_max_decompress;
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
		else
		{
			for(Slot* S : unpacked_slots)
				UnpackSlotItems(S);
			unpacked_slots.clear();
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
