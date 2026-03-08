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

void CDetailManager::cache_Initialize	()
{
	// Centroid
	cache_cx			= 0;
	cache_cz			= 0;

	// Initialize cache-grid
	for (u32 i = 0; i < dm_cache_line; i++)
	{
	    for (u32 j = 0; j < dm_cache_line; j++)
	    {
	        Slot* slt = &cache_pool[i * dm_cache_line + j];
	
	        cache[i][j] = slt;
	        cache_Task(j, i, slt, true);
	    }
	}
	VERIFY	(cache_Validate());

	u32 max_index = dm_cache1_line*dm_cache1_line;
	for (u32 index = 0; index < max_index; index++)
	{
		u32 _mz = index / dm_cache1_line;
		u32 _mx = index % dm_cache1_line;
		CacheSlot1& MS = cache_level1[_mz][_mx];
		for (int i = 0; i < dm_cache_count; i++)
		{
			int _z = i / dm_cache1_count;
			int _x = i % dm_cache1_count;
			MS.slots[_z * dm_cache1_count + _x] = &cache[_mz * dm_cache1_count + _z][_mx * dm_cache1_count + _x];
		}
    }

	// Make dither matrix
	bwdithermap		(2,dither);

	cache_cx = cache_cz = dm_cache_line*2;
}

CDetailManager::Slot*	CDetailManager::cache_Query	(int r_x, int r_z)
{
	int			gx		= w2cg_X(r_x + cache_cx);	VERIFY	(gx>=0 && gx<dm_cache_line);
	int			gz		= w2cg_Z(r_z + cache_cz);	VERIFY	(gz>=0 && gz<dm_cache_line);
	return		cache	[gz][gx];
}

void 	CDetailManager::cache_Task		(int gx, int gz, Slot* D, bool init)
{
	int sx					= cg2w_X	(gx);
	int sz					= cg2w_Z	(gz);
	DetailSlot&	DS			= QueryDB	(sx,sz);

	D->empty				=	(DS.id0==DetailSlot::ID_Empty)&&
								(DS.id1==DetailSlot::ID_Empty)&&
								(DS.id2==DetailSlot::ID_Empty)&&
								(DS.id3==DetailSlot::ID_Empty);

	// Unpacking
	u32 old_type			= D->type;
	D->type					= stPending;
	D->sx					= sx;
	D->sz					= sz;

	D->vis.box.min.set		(sx*dm_slot_size,				DS.r_ybase(),					sz*dm_slot_size);
	D->vis.box.max.set		(D->vis.box.min.x+dm_slot_size,	DS.r_ybase()+DS.r_yheight(),	D->vis.box.min.z+dm_slot_size);
	D->vis.box.grow			(EPS_L);

	for (u32 i=0; i<dm_obj_in_slot; i++)
	{
		D->G[i].id = DS.r_id(i);
		D->G[i].items.clear	();
	}

	if (old_type != stPending)
	{
		VERIFY		(stPending == D->type);
		if (ps_r2_ls_flags.test(R2FLAG_FAST_DETAILS_UPDATE))
		{
			if(!init)
				cache_Decompress(D);
			else
				D->type = stReady;
		}
		else
			cache_task.push_back(D);
	}
}


BOOL	CDetailManager::cache_Validate	()
{
	for (u32 z=0; z<dm_cache_line; z++)
	{
		for (u32 x=0; x<dm_cache_line; x++)
		{
			int		w_x		= cg2w_X(x);
			int		w_z		= cg2w_Z(z);
			Slot*	D		= cache[z][x];

			if (D->sx	!= w_x)	return FALSE;
			if (D->sz	!= w_z)	return FALSE;
		}
	}
	return TRUE;
}

void	CDetailManager::cache_Update(Fvector& view)
{
	PROF_EVENT("cache_Update");
	int v_x = iFloor(view.x / dm_slot_size + .5f);
	int v_z = iFloor(view.z / dm_slot_size + .5f);

	bool bNeedMegaUpdate	= (cache_cx!=v_x)||(cache_cz!=v_z);
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
					for (u32 x = 1; x < dm_cache_line; x++)		cache[z][x - 1] = cache[z][x];
					cache[z][dm_cache_line - 1] = S;
					cache_Task(dm_cache_line - 1, z, S);
				}
			}
			else
			{
				// shift matrix to right
				cache_cx--;
				for (u32 z = 0; z < dm_cache_line; z++)
				{
					Slot* S = cache[z][dm_cache_line - 1];
					for (u32 x = dm_cache_line - 1; x > 0; x--)	cache[z][x] = cache[z][x - 1];
					cache[z][0] = S;
					cache_Task(0, z, S);
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
					for (u32 z = dm_cache_line - 1; z > 0; z--)	cache[z][x] = cache[z - 1][x];
					cache[0][x] = S;
					cache_Task(x, 0, S);
				}
			}
			else
			{
				// shift matrix up
				cache_cz--;
				for (u32 x = 0; x < dm_cache_line; x++)
				{
					Slot* S = cache[0][x];
					for (u32 z = 1; z < dm_cache_line; z++)		cache[z - 1][x] = cache[z][x];
					cache[dm_cache_line - 1][x] = S;
					cache_Task(x, dm_cache_line - 1, S);
				}
			}
		}
	}

	// Task performer
	{
		PROF_EVENT("cache_Decompress");
		if (!ps_r2_ls_flags.test(R2FLAG_FAST_DETAILS_UPDATE))
		{
			BOOL	bFullUnpack = FALSE;
			int limit = dm_max_decompress;
			if (cache_task.size() == dm_cache_size) { limit = dm_cache_size; bFullUnpack = TRUE; }

			for (int iteration = 0; cache_task.size() && (iteration < limit); iteration++) {
				u32		best_id = 0;
				float	best_dist = flt_max;

				if (bFullUnpack)
					best_id = cache_task.size() - 1;
				else
				{
					for (u32 entry = 0; entry < cache_task.size(); entry++)
					{
						// Gain access to data
						Slot* S = cache_task[entry];
						VERIFY(stPending == S->type);

						// Estimate
						Fvector		C;
						S->vis.box.getcenter(C);
						float		D = view.distance_to_sqr(C);

						// Select
						if (D < best_dist)
						{
							best_dist = D;
							best_id = entry;
						}
					}
				}

				// Decompress and remove task
				cache_Decompress(cache_task[best_id]);
				cache_task.erase(cache_task.begin() + best_id);
			}
		}
	}

    if (bNeedMegaUpdate)
	{
		PROF_EVENT("MegaUpdate");
		u32 max_index = dm_cache1_line*dm_cache1_line;
		for (u32 index = 0; index < max_index; index++)
		{
			u32 _mz = index / dm_cache1_line;
			u32 _mx = index % dm_cache1_line;
            CacheSlot1& MS 	= cache_level1[_mz][_mx];
			MS.empty		= TRUE;
            MS.vis.clear	();
            for (int _i=0; _i<dm_cache_count; _i++)
			{
                Slot& 	S 		= **MS.slots[_i];
                MS.vis.box.merge(S.vis.box);
				if (!S.empty)	MS.empty = FALSE;
            }
            MS.vis.box.getsphere(MS.vis.sphere.P,MS.vis.sphere.R);
        }
    }
}

DetailSlot&	CDetailManager::QueryDB(int sx, int sz)
{
	int db_x = sx+dtH.offs_x;
	int db_z = sz+dtH.offs_z;
	if ((db_x>=0) && (db_x<int(dtH.size_x)) && (db_z>=0) && (db_z<int(dtH.size_z)))
	{
		u32 linear_id				= db_z*dtH.size_x + db_x;
		return dtSlots				[linear_id];
	} else {
		// Empty slot
		DS_empty.w_id				(0,DetailSlot::ID_Empty);
		DS_empty.w_id				(1,DetailSlot::ID_Empty);
		DS_empty.w_id				(2,DetailSlot::ID_Empty);
		DS_empty.w_id				(3,DetailSlot::ID_Empty);
		return DS_empty;
	}
}
