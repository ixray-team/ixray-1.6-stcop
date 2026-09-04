#include "stdafx.h"
#include "DetailManager.h"

#include "../../xrEngine/GameMtlLib.h"
#include "../../xrCore/Collision/cl_intersect.h"

#ifdef _EDITOR
#include "../../Editors/LevelEditor/Editor/scene/scene.h"
#endif

#include "../../xrEngine/xr_ioc_cmd.h"


// Быстрый 2D-шум Перлина на основе хеширования
static float fastNoise2D(float x, float y)
{
	int ix = (int)floorf(x);
	int iy = (int)floorf(y);

	float fx = x - floorf(x);
	float fy = y - floorf(y);

	fx = fx * fx * (3.0f - 2.0f * fx);
	fy = fy * fy * (3.0f - 2.0f * fy);

	uint32_t h00 = (uint32_t)(ix * 374761393u + iy * 668265263u);
	uint32_t h10 = (uint32_t)((ix + 1) * 374761393u + iy * 668265263u);
	uint32_t h01 = (uint32_t)(ix * 374761393u + (iy + 1) * 668265263u);
	uint32_t h11 = (uint32_t)((ix + 1) * 374761393u + (iy + 1) * 668265263u);

	h00 = (h00 ^ (h00 >> 13)) * 1274126177u;
	h10 = (h10 ^ (h10 >> 13)) * 1274126177u;
	h01 = (h01 ^ (h01 >> 13)) * 1274126177u;
	h11 = (h11 ^ (h11 >> 13)) * 1274126177u;

	float n00 = (h00 & 0x7fffffff) / 2147483648.0f;
	float n10 = (h10 & 0x7fffffff) / 2147483648.0f;
	float n01 = (h01 & 0x7fffffff) / 2147483648.0f;
	float n11 = (h11 & 0x7fffffff) / 2147483648.0f;

	float nx0 = n00 * (1 - fx) + n10 * fx;
	float nx1 = n01 * (1 - fx) + n11 * fx;

	return nx0 * (1 - fy) + nx1 * fy;
}

// Integer hash for seed generation
static u32 hash_u32(u32 x)
{
	x = ((x >> 16) ^ x) * 0x45d9f3bu;
	x = ((x >> 16) ^ x) * 0x45d9f3bu;
	return (x >> 16) ^ x;
}

static float hash01(u32 seed)
{
	return (float)hash_u32(seed) / (float)0xFFFFFFFFu;
}

// Deterministic side of Perlin-noise clustering: select asset type for this position.
// Runs the same clustering math as the previous Random-based version but replaces the
// final Random.randF(0, sum) pick with a deterministic per-position hash, so the result
// depends only on (world_x, world_z, seed, params). Returns index within [0, asset_count).
static u32 select_clustered_asset(
	float world_x, float world_z,
	u32 seed, u32 asset_count,
	float sharpness, float warp,
	float patch_size_min, float patch_size_max
)
{
	if (asset_count == 0)
		return 0;

	float avg_patch = (patch_size_min + patch_size_max) * 0.5f;
	float base_freq = 1.0f / fmaxf(avg_patch, 0.01f);

	float scale_var = (patch_size_max - patch_size_min) / fmaxf(patch_size_max + patch_size_min, 0.001f);
	float scale_noise = fastNoise2D(world_x * 0.01f + seed * 0.7f, world_z * 0.01f + seed * 0.3f);
	float local_freq = base_freq * (1.0f + (scale_noise - 0.5f) * scale_var * 2.0f);

	float warped_x = world_x;
	float warped_z = world_z;

	if (warp > 0.001f)
	{
		float ws = local_freq * 1.5f;
		float disp = warp * (1.0f / fmaxf(local_freq, 0.001f)) * 0.05f;
		warped_x += fastNoise2D(world_x * ws + 500.0f, world_z * ws + 500.0f) * disp;
		warped_z += fastNoise2D(world_x * ws + 1500.0f, world_z * ws + 1500.0f) * disp;
	}

	float vals[64];
	u32 count = asset_count < 64 ? asset_count : 64;

	for (u32 t = 0; t < count; t++)
	{
		u32 ts = seed + t * 1337u;
		float ox = hash01(ts) * 1000.0f;
		float oz = hash01(ts + 1) * 1000.0f;

		vals[t] = fastNoise2D(warped_x * local_freq + ox, warped_z * local_freq + oz);
	}

	float min_val = vals[0], max_val = vals[0];
	for (u32 t = 1; t < count; t++)
	{
		if (vals[t] < min_val) min_val = vals[t];
		if (vals[t] > max_val) max_val = vals[t];
	}
	float range = max_val - min_val;

	float weights[64];
	float sum = 0.0f;
	for (u32 t = 0; t < count; t++)
	{
		float n = (range > 0.0001f) ? (vals[t] - min_val) / range : (1.0f / count);
		weights[t] = powf(n, sharpness);
		sum += weights[t];
	}

	// Deterministic pick: hash the cell index into [0,1] instead of Random.randF.
	u32 cell_x = (u32)iFloor(world_x / dm_slot_size);
	u32 cell_z = (u32)iFloor(world_z / dm_slot_size);
	u32 h = hash_u32(hash_u32(seed) ^ (cell_x * 374761393u + 668265263u));
	h = hash_u32(h ^ (cell_z * 2246822519u + 3266489917u));
	float rnd = hash01(h) * sum;

	float acc = 0.0f;
	for (u32 t = 0; t < count; t++)
	{
		acc += weights[t];
		if (rnd <= acc)
			return t;
	}
	return count - 1;
}

// Precompute the cluster-noise field over the whole detail DB grid.
// One u8 per DB slot (world cell = dm_slot_size); 255 => "skip, use fallback".
// Rebuilt on cache_ReInitialize (level load and any r__detail_* console change).
void CDetailManager::BuildClusterField()
{
	extern bool ps_r__detail_use_cluster_mix_tree_assets;
	extern float ps_r__detail_cluster_seed;
	extern float ps_r__detail_cluster_patch_size_min;
	extern float ps_r__detail_cluster_patch_size_max;
	extern float ps_r__detail_cluster_sharpness;
	extern float ps_r__detail_cluster_warp_min;
	extern float ps_r__detail_cluster_warp_max;

	u32 sx = dtH.size_x;
	u32 sz = dtH.size_z;
	cluster_field.resize(sx * sz);

	if (!ps_r__detail_use_cluster_mix_tree_assets || cluster_field.size() == 0)
	{
		fill(cluster_field.begin(), cluster_field.end(), u8(255));
		return;
	}

	float avg_warp = (ps_r__detail_cluster_warp_min + ps_r__detail_cluster_warp_max) * 0.5f;
	u32 seed = (u32)ps_r__detail_cluster_seed;
	u32 assets = alt_models_count > 0 ? alt_models_count : vanilla_grass_count;

	for (u32 z = 0; z < sz; z++)
	{
		for (u32 x = 0; x < sx; x++)
		{
			// Slot cell center in world space (mirrors QueryDB mapping).
			float world_x = ((float)int(x) - (float)dtH.offs_x) * dm_slot_size + dm_slot_size * 0.5f;
			float world_z = ((float)int(z) - (float)dtH.offs_z) * dm_slot_size + dm_slot_size * 0.5f;

			u32 use_count = (alt_models_count > 0) ? alt_models_count : vanilla_grass_count;
			u32 idx = select_clustered_asset(
				world_x, world_z,
				seed, use_count,
				ps_r__detail_cluster_sharpness, avg_warp,
				ps_r__detail_cluster_patch_size_min, ps_r__detail_cluster_patch_size_max
			);
			cluster_field[z * sx + x] = (u8)idx;
		}
	}
}

// Look up the index stored for the DB slot containing (world_x, world_z).
// asset_count gates the result (dynamic count may differ from build time);
// a 255 sentinel or out-of-range slot => 0 (falls back to native/sequential).
u32 CDetailManager::SampleClusterField(float world_x, float world_z, u32 asset_count) const
{
	if (asset_count == 0 || cluster_field.empty())
		return 0;

	int db_x = iFloor(world_x / dm_slot_size) + dtH.offs_x;
	int db_z = iFloor(world_z / dm_slot_size) + dtH.offs_z;

	if (db_x < 0 || db_z < 0 || (u32)db_x >= dtH.size_x || (u32)db_z >= dtH.size_z)
		return 0;

	u8 raw = cluster_field[(u32)db_z * dtH.size_x + (u32)db_x];
	if (raw == 255 || raw >= asset_count)
		return 0;
	return (u32)raw;
}


//--------------------------------------------------- Decompression
ICF float Interpolate(float* base, u32 x, u32 y, u32 size)
{
	float f = float(size);
	float fx = float(x) / f;
	float ifx = 1.f - fx;
	float fy = float(y) / f;
	float ify = 1.f - fy;

	float c01 = base[0] * ifx + base[1] * fx;
	float c23 = base[2] * ifx + base[3] * fx;

	float c02 = base[0] * ify + base[2] * fy;
	float c13 = base[1] * ify + base[3] * fy;

	float cx = ify * c01 + fy * c23;
	float cy = ifx * c02 + fx * c13;
	return (cx + cy) / 2;
}

ICF bool InterpolateAndDither(float* alpha255, u32 x, u32 y, u32 sx, u32 sy, u32 size, int dither[16][16])
{
	clamp(x, (u32)0, size - 1);
	clamp(y, (u32)0, size - 1);
	int c = iFloor(Interpolate(alpha255, x, y, size) + .5f);
	clamp(c, 0, 255);

	u32 row = (y + sy) % 16;
	u32 col = (x + sx) % 16;
	return c > dither[col][row];
}

ICF void ground_correction(Fmatrix& xform, const Fvector& ground_normal)
{
	xform.j = ground_normal;

	xform.i.crossproduct(xform.j, xform.k);
	xform.i.normalize();
	xform.k.crossproduct(xform.i, xform.j);
	xform.k.normalize();
}

void CDetailManager::UnpackSlotItems(Slot* S)
{
	VERIFY(S);
	Slot& D = *S;
	D.type = stReady;
	if (D.empty)
	{
		return;
	}

	DetailSlot& DS = *D.DS;

#ifdef _EDITOR
	extern ECORE_API CDB::COLLIDER XRC;
	XRC.box_options(CDB::OPT_FULL_TEST);
	// Select polygons
	SBoxPickInfoVec pinf;
	Scene->BoxPickObjects(D.vis.box, pinf, GetSnapList());
	u32 triCount = pinf.size();
#else
	xrc.box_options(CDB::OPT_FULL_TEST);
	xrc.box_query(g_pGameLevel->ObjectSpace.GetStaticModel(), D.vis.box);
	u32 triCount = xrc.r_count();
	xr_vector<CDB::TRI>& tris = g_pGameLevel->ObjectSpace.GetStaticTris();
	xr_vector<CDB::RESULT>& results = xrc.r_vec();
#endif

	if (0 == triCount)
	{
		return;
	}

	// Build shading table
	float alpha255[dm_obj_in_slot][4];
	for (int i = 0; i < dm_obj_in_slot; i++)
	{
		alpha255[i][0] = 255.f * float(DS.palette[i].a0) / 15.f;
		alpha255[i][1] = 255.f * float(DS.palette[i].a1) / 15.f;
		alpha255[i][2] = 255.f * float(DS.palette[i].a2) / 15.f;
		alpha255[i][3] = 255.f * float(DS.palette[i].a3) / 15.f;
	}

	extern float ps_r__detail_rnd_scale_min;
	extern float ps_r__detail_rnd_scale_max;

	extern bool ps_r__detail_fmb_use_layer_1;
	extern bool ps_r__detail_fmb_use_layer_2;
	extern bool ps_r__detail_fmb_use_layer_3;

	extern float ps_r__detail_fmb_layer_1_frequency;
	extern float ps_r__detail_fmb_layer_1_amplitude;
	extern float ps_r__detail_fmb_layer_1_seed;
	extern float ps_r__detail_fmb_layer_1_power;

	extern float ps_r__detail_fmb_layer_2_frequency;
	extern float ps_r__detail_fmb_layer_2_amplitude;
	extern float ps_r__detail_fmb_layer_2_seed;
	extern float ps_r__detail_fmb_layer_2_power;

	extern float ps_r__detail_fmb_layer_3_frequency;
	extern float ps_r__detail_fmb_layer_3_amplitude;
	extern float ps_r__detail_fmb_layer_3_seed;
	extern float ps_r__detail_fmb_layer_3_power;


	extern bool ps_r__detail_use_alternative_tree_assets;
	extern bool ps_r__detail_use_cluster_mix_tree_assets;

	extern float ps_r__detail_cluster_seed;
	extern float ps_r__detail_cluster_patch_size_min;
	extern float ps_r__detail_cluster_patch_size_max;
	extern float ps_r__detail_cluster_sharpness;
	extern float ps_r__detail_cluster_warp_min;
	extern float ps_r__detail_cluster_warp_max;

#ifndef _EDITOR
	float rnd_scale_min = ps_r__detail_rnd_scale_min;
	float rnd_scale_max = ps_r__detail_rnd_scale_max;
#else
	float rnd_scale_min = 0.5f;
	float rnd_scale_max = 0.9f;
#endif

	// Prepare to selection
	float density = ps_r__Detail_density;
	float jitter = density / 1.7f;
	u32 d_size = iCeil(dm_slot_size / density);

	s32 p_rnd = D.vis.box.max.x * D.vis.box.max.y * D.vis.box.max.z; // нужно для того чтобы убрать полосы(ряды)
	CRandom r_selection(p_rnd);
	CRandom r_jitter(p_rnd);
	CRandom r_yaw(p_rnd);
	CRandom r_scale(p_rnd);

	// Prepare to actual-bounds-calculations
	Fbox Bounds, ItemBB;
	Bounds.invalidate();
	Fvector Item_P;
	Fvector dir;
	dir.set(0, -1, 0);
	Fvector normal;
	normal.set(0, 1, 0);
	Fquaternion q;
	Fmatrix mResult;
	// Decompressing itself
	u32 max_items_in_slot = (d_size + 1) * (d_size + 1);
	bool empty_slot = true;

	/////
	float scale = 1.0f;
	float minScale = 1.0f;
	float maxScale = 1.0f;
	float noise = 1.0f;
	float baseScale = 1.0f;
	float offsetRange = 1.0f;
	float randomOffset = 1.0f;
	float layerScale = 0;
	/////

	for (u32 z = 0; z <= d_size; z++)
	{
		for (u32 x = 0; x <= d_size; x++)
		{
			// Iterpolate and dither palette
			FixedVector<int, dm_obj_in_slot> selected;

			if ((DS.id0 != DetailSlot::ID_Empty))
			{
				selected.push_back(0);
			}
			if ((DS.id1 != DetailSlot::ID_Empty))
			{
				selected.push_back(1);
			}
			if ((DS.id2 != DetailSlot::ID_Empty))
			{
				selected.push_back(2);
			}
			if ((DS.id3 != DetailSlot::ID_Empty))
			{
				selected.push_back(3);
			}

			// Select
			if (selected.empty())
			{
				continue;
			}

			u32 index = (selected.size() == 1) ? selected[0] : selected[r_selection.randI(selected.size())];

			// shift
			u32 shift_x = r_jitter.randI(16);
			u32 shift_z = r_jitter.randI(16);

			if (!InterpolateAndDither(alpha255[index], x, z, shift_x, shift_z, d_size, dither))
			{
				continue;
			}

			// Position (XZ)
			float rx = (float(x) / float(d_size)) * dm_slot_size + D.vis.box.min.x;
			float rz = (float(z) / float(d_size)) * dm_slot_size + D.vis.box.min.z;

			Item_P.set(rx + r_jitter.randFs(jitter), D.vis.box.max.y, rz + r_jitter.randFs(jitter));

			// Position (Y)
			float y = D.vis.box.min.y - 5;
			float r_u, r_v, r_range;
			bool no_push = false;
			for (u32 tid = 0; tid < triCount; tid++)
			{
#ifdef _EDITOR
				Fvector verts[3];
				SBoxPickInfo& I = pinf[tid];
				for (int k = 0; k < (int)I.inf.size(); k++)
				{
					VERIFY(I.s_obj);
					I.e_obj->GetFaceWorld(I.s_obj->_Transform(), I.e_mesh, I.inf[k].id, verts);
					if (CDB::TestRayTri(Item_P, dir, verts, r_u, r_v, r_range, true))
					{
						if (r_range >= 0)
						{
							float y_test = Item_P.y - r_range;
							if (y_test > y)
							{
								y = y_test;
							}
						}
						normal.mknormal(verts[0], verts[1], verts[2]);
					}
				}
#else
				CDB::RESULT& R = results[tid];
				CDB::TRI& T = tris[R.id];
				SGameMtl* mtl = GMLib.GetMaterialByIdx(T.material);

				if (mtl->Flags.test(SGameMtl::flPassable))
				{
					continue;
				}

				// Detect sector
				if (RImplementation.pOutdoorSector && T.sector < RImplementation.Sectors.size())
				{
					CSector* sector = (CSector*)RImplementation.Sectors[T.sector];
					if (sector != RImplementation.pOutdoorSector)
					{
						no_push = true;
						break;
					}
				}
				if (CDB::TestRayTri(Item_P, dir, R.verts, r_u, r_v, r_range, true))
				{
					if (r_range >= 0)
					{
						float y_test = Item_P.y - r_range;
						if (y_test > y)
						{
							y = y_test;
						}
					}
					normal.mknormal(R.verts[0], R.verts[1], R.verts[2]);
					break;
				}
#endif
			}
			if (no_push)
			{
				continue;
			}
			if (y < D.vis.box.min.y)
			{
				continue;
			}

			Item_P.y = y;

			empty_slot = false;
			u8 obj_id = DS.r_id(index);

			if (obj_id == DetailSlot::ID_Empty)
			{
				continue;
			}
#ifndef _EDITOR
			const CDetail& Dobj = objects[obj_id];
#else
			const CDetail& Dobj = *objects[obj_id];
#endif
			if (!(ps_r__detail_fmb_use_layer_1 || ps_r__detail_fmb_use_layer_2 ||  ps_r__detail_fmb_use_layer_3))
			{
				scale = r_scale.randF(Dobj.m_fMinScale * rnd_scale_min, Dobj.m_fMaxScale * rnd_scale_max);
			}
			else
			{
				minScale = Dobj.m_fMinScale * rnd_scale_min;
				maxScale = Dobj.m_fMaxScale * rnd_scale_max;

				float max_scale = 0;

				if (ps_r__detail_fmb_use_layer_1)
				{
					float n1 = fastNoise2D(Item_P.x * ps_r__detail_fmb_layer_1_frequency + ps_r__detail_fmb_layer_1_seed, Item_P.z * ps_r__detail_fmb_layer_1_frequency + ps_r__detail_fmb_layer_1_seed);
					float mapped1 = n1 * ps_r__detail_fmb_layer_1_amplitude + (1 - ps_r__detail_fmb_layer_1_amplitude) * 0.5f;
					float s1 = minScale + (maxScale - minScale) * mapped1 * ps_r__detail_fmb_layer_1_power;
					if (s1 > max_scale) max_scale = s1;
				}

				if (ps_r__detail_fmb_use_layer_2)
				{
					float n2 = fastNoise2D(Item_P.x * ps_r__detail_fmb_layer_2_frequency + ps_r__detail_fmb_layer_2_seed, Item_P.z * ps_r__detail_fmb_layer_2_frequency + ps_r__detail_fmb_layer_2_seed);
					float mapped2 = n2 * ps_r__detail_fmb_layer_2_amplitude + (1 - ps_r__detail_fmb_layer_2_amplitude) * 0.5f;
					float s2 = minScale + (maxScale - minScale) * mapped2 * ps_r__detail_fmb_layer_2_power;
					if (s2 > max_scale) max_scale = s2;
				}

				if (ps_r__detail_fmb_use_layer_3)
				{
					float n3 = fastNoise2D(Item_P.x * ps_r__detail_fmb_layer_3_frequency + ps_r__detail_fmb_layer_3_seed, Item_P.z * ps_r__detail_fmb_layer_3_frequency + ps_r__detail_fmb_layer_3_seed);
					float mapped3 = n3 * ps_r__detail_fmb_layer_3_amplitude + (1 - ps_r__detail_fmb_layer_3_amplitude) * 0.5f;
					float s3 = minScale + (maxScale - minScale) * mapped3 * ps_r__detail_fmb_layer_3_power;
					if (s3 > max_scale) max_scale = s3;
				}

				scale = max_scale;
			}

			mResult.k.x = r_yaw.randF(-0.99, 0.99);
			mResult.k.z = r_yaw.randF(-0.99, 0.99);

			u8 vis_ID = Dobj.m_Flags.is(DO_NO_WAVING) ? 0 : Random.randI(1, 3);

			bool is_leaf = Dobj.m_Flags.is(DO_NO_WAVING) || Dobj.number_vertices < 8;
			bool is_grass = !is_leaf;

			if (is_grass)
			{
				if (ps_r__detail_use_alternative_tree_assets && alt_models_count > 0)
				{
					if (ps_r__detail_use_cluster_mix_tree_assets)
					{
						u32 cluster_idx = SampleClusterField(Item_P.x, Item_P.z, alt_models_count);
						obj_id = (u8)(alt_models_start + cluster_idx);
					}
					else
					{
						for (u32 g = 0; g < vanilla_grass_count; g++)
						{
							if (vanilla_grass_indices[g] == obj_id)
							{
								if (g < alt_models_count)
									obj_id = (u8)(alt_models_start + g);
								break;
							}
						}
					}
				}
				else if (ps_r__detail_use_cluster_mix_tree_assets && vanilla_grass_count > 1)
				{
					u32 cluster_idx = SampleClusterField(Item_P.x, Item_P.z, vanilla_grass_count);
					obj_id = (u8)vanilla_grass_indices[cluster_idx];
				}
			}

			// чтобы (только) листики травы ложились на поверхность террейна
			//	if (vis_ID == 0)
			ground_correction(mResult, normal);
			float radius = scale * Dobj.bv_bb.getradius();

			ItemBB.setb(Item_P, {radius, radius, radius});
			Bounds.merge(ItemBB);
			// Save it
			q.set(mResult);
			q.normalize();

			float hemi = DS.r_qclr(DS.c_hemi, 15) + EPS;
			hemi = DS.r_qclr(DS.c_dir, 15) > 0.07f ? hemi : -hemi;

			CDetail::SlotItem* ItemP = items_pool.create();
			ItemP->quat = {q.x, q.y, q.z};
			ItemP->scale = scale;
			ItemP->pos = Item_P;
			ItemP->c_hemi = hemi;

			SlotPart& SP = D.G[index];
			SP.id = obj_id;
			SP.items[vis_ID].reserve(max_items_in_slot);
			SP.items[vis_ID].push_back(ItemP);
		}
	}
	if (empty_slot)
	{
		D.empty = TRUE;
		return;
	}

	// Update bounds to more tight and real ones
	D.vis.clear();
	D.vis.box.set(Bounds);
	D.vis.box.getsphere(D.vis.sphere.P, D.vis.sphere.R);
}