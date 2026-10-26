#include "stdafx.h"
#include <bit>
#include "DetailManager.h"
#include "xrRender_console.h"

#include "../../xrEngine/GameMtlLib.h"
#include "../../xrCore/Collision/cl_intersect.h"

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

namespace
{
bool HasClusterGrass(DetailSlot& Slot, const xr_vector<CDetail*>& Models)
{
	bool Grass = false;
	for (u32 I = 0; I < dm_obj_in_slot; ++I)
	{
		const u32 ID = Slot.r_id(I);
		if (ID != DetailSlot::ID_Empty && ID < Models.size())
		{
			const CDetail& Model = *Models[ID];
			if (Model.m_Flags.is(DO_NO_WAVING) || Model.number_vertices < 8)
			{
				return false;
			}
			const DetailPalette& Palette = Slot.palette[I];
			Grass |= Palette.a0 || Palette.a1 || Palette.a2 || Palette.a3;
		}
	}
	return Grass;
}

// Four baked field nodes define a continuous mixture inside each native slot.
// Adjacent slots share the same edge nodes, so their distributions meet continuously.
struct SlotDiffusion
{
	u32 Assets[dm_obj_in_slot] = {};
	u32 CornerGroups[4] = {};
	u32 Count = 0, Seed = 0;

	u32 SampleGroup(float X, float Z, u32 Key) const
	{
		if (Count == 1)
		{
			return 0;
		}
		X = clampr(X, 0.f, 1.f);
		Z = clampr(Z, 0.f, 1.f);
		const bool Right = float(hash_u32(Key) >> 8) * (1.f / 16777216.f) < X;
		const bool Bottom = float(hash_u32(Key ^ 0x9e3779b9u) >> 8) * (1.f / 16777216.f) < Z;
		return CornerGroups[u32(Right) | (u32(Bottom) << 1)];
	}
};
} // namespace

u32 CDetailManager::GetClusterSpeciesCount() const
{
	return ps_r__detail_use_alternative_tree_assets && alt_models_count ? u32(AlternativeSpeciesGroups.size()) : vanilla_grass_count;
}

u32 CDetailManager::GetClusterSpecies(u32 Model) const
{
	return ps_r__detail_use_alternative_tree_assets && Model < AlternativeModels.size() ? AlternativeModels[Model].Species : Model;
}

u32 CDetailManager::GetClusterFieldKey() const
{
	const float Settings[] = {
		float(dtH.size_x), float(dtH.size_z), float(dtH.offs_x), float(dtH.offs_z), float(alt_models_count), float(vanilla_grass_count), float(ps_r__detail_use_alternative_tree_assets), float(ps_r__detail_use_cluster_mix_tree_assets), ps_r__detail_cluster_seed, ps_r__detail_cluster_patch_size_min, ps_r__detail_cluster_patch_size_max, ps_r__detail_cluster_diffusion, ps_r__detail_cluster_sharpness, ps_r__detail_cluster_deformation, ps_r__detail_cluster_elongation, ps_r__detail_cluster_coverage_size
	};
	return crc32(Settings, sizeof(Settings)) ^ AlternativeCatalogCRC;
}

u32 CDetailManager::GetFMBFieldKey() const
{
	const float Settings[] = {
		float(dtH.size_x), float(dtH.size_z), float(dtH.offs_x), float(dtH.offs_z), float(ps_r__detail_fmb_use_layer_1), float(ps_r__detail_fmb_use_layer_2), float(ps_r__detail_fmb_use_layer_3), ps_r__detail_fmb_layer_1_frequency, ps_r__detail_fmb_layer_1_amplitude, ps_r__detail_fmb_layer_1_seed, ps_r__detail_fmb_layer_1_power, ps_r__detail_fmb_layer_2_frequency, ps_r__detail_fmb_layer_2_amplitude, ps_r__detail_fmb_layer_2_seed, ps_r__detail_fmb_layer_2_power, ps_r__detail_fmb_layer_3_frequency, ps_r__detail_fmb_layer_3_amplitude, ps_r__detail_fmb_layer_3_seed, ps_r__detail_fmb_layer_3_power
	};
	return crc32(Settings, sizeof(Settings));
}

// Precompute the cluster-noise field over the whole detail DB grid.
// One u8 per DB slot (world cell = dm_slot_size); 255 => "skip, use fallback".
// Rebuilt on cache_ReInitialize (level load and any r__detail_* console change).
void CDetailManager::BuildClusterField()
{
	const u32 Size = dtH.size_x * dtH.size_z;
	const u32 Key = GetClusterFieldKey();
	if (!Device.IsEditorMode() && ClusterFieldValid && ClusterFieldKey == Key && cluster_field_base.size() == Size && cluster_rnd_field_base.size() == Size)
	{
		cluster_field = cluster_field_base;
		cluster_rnd_field = cluster_rnd_field_base;
		return;
	}
	ClusterFieldKey = Key;
	ClusterFieldValid = true;
	cluster_field.assign(Size, u8(255));
	cluster_rnd_field.assign(Size, u8(255));
	const u32 Assets = GetClusterSpeciesCount();
	if (!ps_r__detail_use_cluster_mix_tree_assets || !Size || !Assets)
	{
		return;
	}
	const float Low = std::max(1.f, std::min(ps_r__detail_cluster_patch_size_min, ps_r__detail_cluster_patch_size_max));
	const float High = std::max(Low, std::max(ps_r__detail_cluster_patch_size_min, ps_r__detail_cluster_patch_size_max));
	const float Spacing = std::max(4.f, (Low + High) * 0.5f);
	const float Aspect = std::max(1.f, ps_r__detail_cluster_elongation);
	constexpr float MaxDeformation = 500.f;
	constexpr float DeformationScale = 0.1f;
	const float Warp = Spacing * clampr(ps_r__detail_cluster_deformation, 0.f, MaxDeformation) * DeformationScale;
	const float OriginX = -float(dtH.offs_x) * dm_slot_size, OriginZ = -float(dtH.offs_z) * dm_slot_size;
	const u32 CoreWidth = std::max(1u, u32(iCeil(float(dtH.size_x) * dm_slot_size / Spacing)));
	const u32 CoreHeight = std::max(1u, u32(iCeil(float(dtH.size_z) * dm_slot_size / Spacing)));
	// Cover the whole control range with the same seeds: changing deformation must not
	// create exterior patches, and even the largest warp must remain inside the search grid.
	const int Padding = iCeil(MaxDeformation * DeformationScale * 0.5f);
	const float GridOriginX = OriginX - float(Padding) * Spacing;
	const float GridOriginZ = OriginZ - float(Padding) * Spacing;
	const u32 Width = CoreWidth + u32(Padding) * 2;
	const u32 Height = CoreHeight + u32(Padding) * 2;
	const u32 Seed = u32(ps_r__detail_cluster_seed);
	struct Patch
	{
		float X, Z, InvRadius, InvAspect, Cos, Sin;
		u32 Asset;
	};
	xr_vector<Patch> Patches(Width * Height);
	for (u32 Z = 0; Z < Height; ++Z)
	{
		for (u32 X = 0; X < Width; ++X)
		{
			const int PX = int(X) - Padding, PZ = int(Z) - Padding;
			const bool CoreX = PX >= 0 && PX < int(CoreWidth), CoreZ = PZ >= 0 && PZ < int(CoreHeight);
			// Interior patches retain their seed, radius, species and direction as the padding changes.
			const u32 PatchKey = CoreX && CoreZ ? u32(PZ) * CoreWidth + u32(PX) : hash_u32(u32(PX) * 374761393u) ^ hash_u32(u32(PZ) * 668265263u) ^ 0x735a2d97u;
			const u32 H = hash_u32(Seed ^ PatchKey);
			Patch& P = Patches[Z * Width + X];
			const float CenterX = (float(PX) + 0.15f + hash01(H) * 0.7f) * Spacing;
			const float CenterZ = (float(PZ) + 0.15f + hash01(H + 1) * 0.7f) * Spacing;
			P.X = OriginX + (CoreX ? std::min(float(dtH.size_x) * dm_slot_size - EPS_L, CenterX) : CenterX);
			P.Z = OriginZ + (CoreZ ? std::min(float(dtH.size_z) * dm_slot_size - EPS_L, CenterZ) : CenterZ);
			P.InvRadius = 2.f / (Low + (High - Low) * hash01(H + 2));
			P.InvAspect = 1.f / (1.f + (Aspect - 1.f) * hash01(H + 3));
			const float Angle = hash01(H + 4) * PI_MUL_2;
			P.Cos = cosf(Angle);
			P.Sin = sinf(Angle);
			P.Asset = hash_u32(H + 5) % Assets;
		}
	}
	const float MaxReach = High * 0.5f * Aspect;
	for (u32 Z = 0; Z < dtH.size_z; ++Z)
	{
		for (u32 X = 0; X < dtH.size_x; ++X)
		{
			const u32 Index = Z * dtH.size_x + X;
			cluster_rnd_field[Index] = u8(hash_u32(Index ^ Seed) >> 24);
			if (!dtSlots || !HasClusterGrass(dtSlots[Index], objects))
			{
				continue;
			}
			const float WorldX = OriginX + float(X) * dm_slot_size;
			const float WorldZ = OriginZ + float(Z) * dm_slot_size;
			const float NX = WorldX / Spacing, NZ = WorldZ / Spacing;
			const float WX = WorldX + (fastNoise2D(NX * 0.45f + Seed, NZ * 0.45f) * 0.7f + fastNoise2D(NX * 1.3f, NZ * 1.3f + Seed) * 0.3f - 0.5f) * Warp;
			const float WZ = WorldZ + (fastNoise2D(NX * 0.45f, NZ * 0.45f + Seed + 173.f) * 0.7f + fastNoise2D(NX * 1.3f + Seed + 571.f, NZ * 1.3f) * 0.3f - 0.5f) * Warp;
			const int CX = std::clamp(iFloor((WX - GridOriginX) / Spacing), 0, int(Width) - 1);
			const int CZ = std::clamp(iFloor((WZ - GridOriginZ) / Spacing), 0, int(Height) - 1);
			float Best = flt_max;
			u32 Asset = 0;
			const auto Visit = [&](int PX, int PZ)
			{
				if (PX < 0 || PZ < 0 || PX >= int(Width) || PZ >= int(Height))
				{
					return;
				}
				const Patch& P = Patches[u32(PZ) * Width + u32(PX)];
				const float DX = WX - P.X, DZ = WZ - P.Z;
				const float U = (DX * P.Cos + DZ * P.Sin) * P.InvAspect * P.InvRadius;
				const float V = (-DX * P.Sin + DZ * P.Cos) * P.InvRadius;
				const float Score = U * U + V * V;
				if (Score < Best)
				{
					Best = Score;
					Asset = P.Asset;
				}
			};
			for (int Ring = 0; Ring <= int(std::max(Width, Height)); ++Ring)
			{
				for (int PX = std::max(0, CX - Ring); PX <= std::min(int(Width) - 1, CX + Ring); ++PX)
				{
					Visit(PX, CZ - Ring);
					if (Ring)
					{
						Visit(PX, CZ + Ring);
					}
				}
				for (int PZ = std::max(0, CZ - Ring + 1); PZ <= std::min(int(Height) - 1, CZ + Ring - 1); ++PZ)
				{
					Visit(CX - Ring, PZ);
					if (Ring)
					{
						Visit(CX + Ring, PZ);
					}
				}
				// Stop only when even the largest elongated patch outside this ring cannot win.
				float Distance = flt_max;
				if (CX - Ring > 0)
				{
					Distance = std::min(Distance, WX - GridOriginX - float(CX - Ring) * Spacing);
				}
				if (CZ - Ring > 0)
				{
					Distance = std::min(Distance, WZ - GridOriginZ - float(CZ - Ring) * Spacing);
				}
				if (CX + Ring + 1 < int(Width))
				{
					Distance = std::min(Distance, GridOriginX + float(CX + Ring + 1) * Spacing - WX);
				}
				if (CZ + Ring + 1 < int(Height))
				{
					Distance = std::min(Distance, GridOriginZ + float(CZ + Ring + 1) * Spacing - WZ);
				}
				if (Distance == flt_max || std::max(0.f, Distance) / MaxReach > sqrtf(Best))
				{
					break;
				}
			}
			cluster_field[Index] = u8(Asset);
		}
	}
	// Coverage belongs to the immutable base pattern, never to the strength-dependent result.
	BuildClusterCoverage();
	BuildClusterTransitions(Spacing);
	if (ps_r__detail_use_alternative_tree_assets && alt_models_count)
	{
		// Resolve variants during baking. Unpack consumes only ready model IDs.
		for (u32 Index = 0; Index < Size; ++Index)
		{
			const u32 Species = cluster_field[Index];
			if (Species < AlternativeSpeciesGroups.size())
			{
				const auto& Variants = AlternativeSpeciesGroups[Species].Models;
				// Multiply-high maps the full hash range evenly to the variant list.
				const u32 Choice = u32((u64(hash_u32(Index ^ Seed ^ 0xa511e9b3u)) * Variants.size()) >> 32);
				cluster_field[Index] = Variants[Choice];
			}
		}
	}
}

// Resolve boundary blur and adjacent-species micro patches once, then bake ready IDs.
void CDetailManager::BuildClusterTransitions(float Spacing)
{
	const float Diffusion = std::max(0.f, ps_r__detail_cluster_diffusion);
	const float BlendSigma = Spacing * 0.75f * (1.f - clampr(ps_r__detail_cluster_sharpness * 0.01f, 0.f, 1.f));
	if (Diffusion <= 0.f && BlendSigma <= 0.f)
	{
		return;
	}
	const xr_vector<u8> Owners = cluster_field;
	const u32 Size = u32(Owners.size());
	xr_vector<Fvector2> Foreign(Size);
	for (Fvector2& Node : Foreign)
	{
		Node.set(-1.f, -1.f);
	}
	// Raster propagation finds a stable neighbouring boundary species. Neither strength
	// parameter participates in this search, so increasing either never picks a new donor.
	const auto Refine = [&](int X, int Z, bool Forward)
	{
		const u32 Index = u32(Z) * dtH.size_x + u32(X);
		const u32 Main = Owners[Index];
		if (Main == 255)
		{
			return;
		}
		Fvector2& Best = Foreign[Index];
		float BestDistance = Best.x < 0.f ? flt_max : _sqr(float(X) - Best.x) + _sqr(float(Z) - Best.y);
		const auto Relax = [&](int NX, int NZ)
		{
			if (NX < 0 || NZ < 0 || NX >= int(dtH.size_x) || NZ >= int(dtH.size_z))
			{
				return;
			}
			const u32 Neighbor = u32(NZ) * dtH.size_x + u32(NX);
			if (Owners[Neighbor] == 255)
			{
				return;
			}
			Fvector2 Candidate = Foreign[Neighbor];
			if (Owners[Neighbor] != Main)
			{
				Candidate.set(float(NX), float(NZ));
			}
			if (Candidate.x < 0.f)
			{
				return;
			}
			const float Distance = _sqr(float(X) - Candidate.x) + _sqr(float(Z) - Candidate.y);
			if (Distance < BestDistance || (Distance == BestDistance && (Candidate.y < Best.y || (Candidate.y == Best.y && Candidate.x < Best.x))))
			{
				Best = Candidate;
				BestDistance = Distance;
			}
		};
		const int Direction = Forward ? -1 : 1;
		Relax(X + Direction, Z);
		Relax(X - 1, Z + Direction);
		Relax(X, Z + Direction);
		Relax(X + 1, Z + Direction);
	};
	for (u32 Pass = 0; Pass < 2; ++Pass)
	{
		for (int Z = 0; Z < int(dtH.size_z); ++Z)
		{
			for (int X = 0; X < int(dtH.size_x); ++X)
			{
				Refine(X, Z, true);
			}
		}
		for (int Z = int(dtH.size_z) - 1; Z >= 0; --Z)
		{
			for (int X = int(dtH.size_x) - 1; X >= 0; --X)
			{
				Refine(X, Z, false);
			}
		}
	}
	const float ScatterSigma = Spacing * Diffusion;
	const float ScatterChance = 0.25f * Diffusion / (0.25f + Diffusion);
	const float Seed = ps_r__detail_cluster_seed;
	for (u32 Index = 0; Index < Size; ++Index)
	{
		if (Owners[Index] == 255 || cluster_rnd_field[Index] == 255 || Foreign[Index].x < 0.f)
		{
			continue;
		}
		const float X = float(Index % dtH.size_x), Z = float(Index / dtH.size_x);
		const float Distance = std::max(0.f, sqrtf(_sqr(X - Foreign[Index].x) + _sqr(Z - Foreign[Index].y)) * dm_slot_size - dm_slot_size * 0.5f);
		const float BlendChance = BlendSigma > 0.f ? 0.5f * erfcf(Distance / (1.41421356f * BlendSigma)) : 0.f;
		const bool Blend = (float(cluster_rnd_field[Index]) + 0.5f) / 256.f < BlendChance;
		bool Scatter = false;
		if (ScatterSigma > 0.f)
		{
			const float Chance = ScatterChance * expf(-Distance * Distance / (2.f * ScatterSigma * ScatterSigma));
			// A fixed smooth micro mask creates little clumps rather than unrelated salt-and-pepper pixels.
			const float Micro = fastNoise2D(X * 0.37f + Seed * 0.31f, Z * 0.37f + Seed * 0.73f + 573.f);
			Scatter = Micro < Chance;
		}
		if (Blend || Scatter)
		{
			const u32 Source = u32(Foreign[Index].y) * dtH.size_x + u32(Foreign[Index].x);
			cluster_field[Index] = Owners[Source];
		}
	}
}

// Add underrepresented species to each area without clipping or restarting the global noise.
// Scratch arrays exist only during generation; slot and bake formats remain unchanged.
void CDetailManager::BuildClusterCoverage()
{
	extern bool ps_r__detail_use_alternative_tree_assets;
	extern float ps_r__detail_cluster_seed;
	extern float ps_r__detail_cluster_coverage_size;
	const u32 Assets = GetClusterSpeciesCount();
	if (!Assets || !dtSlots || cluster_field.size() != dtH.size_x * dtH.size_z)
	{
		return;
	}
	const u32 Step = std::max(1u, u32(iCeil(std::max(50.f, ps_r__detail_cluster_coverage_size) / dm_slot_size)));
	for (u32 Z0 = 0; Z0 < dtH.size_z; Z0 += Step)
	{
		for (u32 X0 = 0; X0 < dtH.size_x; X0 += Step)
		{
			const u32 Width = std::min(Step, dtH.size_x - X0), Height = std::min(Step, dtH.size_z - Z0);
			xr_vector<u32> Eligible;
			xr_vector<u8> Allowed(Width * Height, 0), Visited(Width * Height, 0);
			u32 Counts[255] = {};
			for (u32 Z = 0; Z < Height; ++Z)
			{
				for (u32 X = 0; X < Width; ++X)
				{
					const u32 Index = (Z0 + Z) * dtH.size_x + X0 + X;
					if (cluster_field[Index] < Assets && HasClusterGrass(dtSlots[Index], objects))
					{
						Allowed[Z * Width + X] = 1;
						Eligible.push_back(Z * Width + X);
						++Counts[cluster_field[Index]];
					}
				}
			}
			if (Eligible.empty())
			{
				continue;
			}
			const auto ProtectCoverage = [&]()
			{
				u32 Anchors[255];
				std::fill(std::begin(Anchors), std::end(Anchors), ~0u);
				const u32 Seed = u32(ps_r__detail_cluster_seed);
				for (u32 Local : Eligible)
				{
					const u32 Index = (Z0 + Local / Width) * dtH.size_x + X0 + Local % Width;
					const u32 Asset = cluster_field[Index];
					if (Anchors[Asset] == ~0u || hash_u32(Index ^ Seed) < hash_u32(Anchors[Asset] ^ Seed))
					{
						Anchors[Asset] = Index;
					}
				}
				for (u32 Asset = 0; Asset < Assets; ++Asset)
				{
					if (Anchors[Asset] != ~0u)
					{
						cluster_rnd_field[Anchors[Asset]] = 255;
					}
				}
			};
			// Reserve half of an equal share per species; the remaining area keeps the noise proportions.
			const u32 Target = std::max(1u, u32(Eligible.size()) / (Assets * 2));
			bool NeedsCoverage = false;
			for (u32 Asset = 0; Asset < Assets; ++Asset)
			{
				NeedsCoverage |= Counts[Asset] < Target;
			}
			if (!NeedsCoverage)
			{
				ProtectCoverage();
				continue;
			}
			CRandom AreaRandom(s32(hash_u32(u32(ps_r__detail_cluster_seed) ^ hash_u32(X0) ^ hash_u32(Z0 + 668265263u))));
			for (u32 I = u32(Eligible.size()); I > 1; --I)
			{
				std::swap(Eligible[I - 1], Eligible[AreaRandom.randI(I)]);
			}
			xr_vector<u32> Order(Assets);
			for (u32 I = 0; I < Assets; ++I)
			{
				Order[I] = I;
			}
			for (u32 I = Assets; I > 1; --I)
			{
				std::swap(Order[I - 1], Order[AreaRandom.randI(I)]);
			}
			struct Candidate
			{
				u32 Local;
				float Priority;
			};
			xr_vector<Candidate> Frontier;
			const auto Compare = [](const Candidate& A, const Candidate& B)
			{
				return A.Priority == B.Priority ? A.Local > B.Local : A.Priority > B.Priority;
			};
			u32 Cursor = 0;
			for (u32 Asset : Order)
			{
				while (Counts[Asset] < Target)
				{
					// Pick a donor that can lose area without breaking another species' minimum.
					while (Cursor < Eligible.size())
					{
						const u32 Local = Eligible[Cursor];
						const u32 Index = (Z0 + Local / Width) * dtH.size_x + X0 + Local % Width;
						if (Counts[cluster_field[Index]] > Target && Visited[Local] != Asset + 1)
						{
							break;
						}
						++Cursor;
					}
					if (Cursor == Eligible.size())
					{
						break;
					}
					const u32 Anchor = Eligible[Cursor++];
					const int AX = int(Anchor % Width), AZ = int(Anchor / Width);
					const float Low = std::max(1.f, std::min(ps_r__detail_cluster_patch_size_min, ps_r__detail_cluster_patch_size_max));
					const float High = std::max(Low, std::max(ps_r__detail_cluster_patch_size_min, ps_r__detail_cluster_patch_size_max));
					const float Diameter = AreaRandom.randF(Low, High) / dm_slot_size;
					const float Aspect = AreaRandom.randF(1.f, std::max(1.f, ps_r__detail_cluster_elongation));
					const float Angle = AreaRandom.randF(0.f, PI_MUL_2);
					const float Cos = cosf(Angle), Sin = sinf(Angle);
					const u32 Budget = std::max(1u, u32(iCeil(PI * 0.25f * Diameter * Diameter * Aspect)));
					u32 Added = 0;
					Frontier.clear();
					const auto Push = [&](int X, int Z)
					{
						if (X < 0 || Z < 0 || X >= int(Width) || Z >= int(Height))
						{
							return;
						}
						const u32 Local = u32(Z) * Width + u32(X);
						if (!Allowed[Local] || Visited[Local] == Asset + 1)
						{
							return;
						}
						Visited[Local] = u8(Asset + 1);
						const float DX = float(X - AX), DZ = float(Z - AZ);
						const float Noise = fastNoise2D(float(X + int(X0)) * 0.17f + float(Asset * 17), float(Z + int(Z0)) * 0.17f + ps_r__detail_cluster_seed);
						const float U = (DX * Cos + DZ * Sin) / Aspect, V = -DX * Sin + DZ * Cos;
						const float Shape = expf((Noise - 0.5f) * std::max(0.f, ps_r__detail_cluster_deformation) * 0.04f);
						Frontier.push_back({Local, (U * U + V * V) * Shape});
						std::push_heap(Frontier.begin(), Frontier.end(), Compare);
					};
					Push(AX, AZ);
					while (!Frontier.empty() && Counts[Asset] < Target && Added < Budget)
					{
						std::pop_heap(Frontier.begin(), Frontier.end(), Compare);
						const u32 Local = Frontier.back().Local;
						Frontier.pop_back();
						const int X = int(Local % Width), Z = int(Local / Width);
						const u32 Index = (Z0 + u32(Z)) * dtH.size_x + X0 + u32(X);
						const u32 Old = cluster_field[Index];
						if (Old != Asset && Counts[Old] > Target)
						{
							--Counts[Old];
							++Counts[Asset];
							cluster_field[Index] = u8(Asset);
							++Added;
						}
						for (int NZ = Z - 1; NZ <= Z + 1; ++NZ)
						{
							for (int NX = X - 1; NX <= X + 1; ++NX)
							{
								Push(NX, NZ);
							}
						}
					}
					for (const Candidate& Unused : Frontier)
					{
						Visited[Unused.Local] = 0;
					}
				}
			}
			ProtectCoverage();
		}
	}
}
// Precompute the combined FMB height factor over the whole detail DB grid.
// One float per DB slot CORNER (grid of (size_x+1)*(size_z+1)), bilinearly sampled
// per instance, so decompress has zero per-instance noise calls. The factor t is
// object-independent: final scale = minScale + (maxScale-minScale)*t.
// Rebuilt on cache_ReInitialize (level load and any r__detail_* console change).
void CDetailManager::BuildFMBField()
{
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

	u32 sx = dtH.size_x;
	u32 sz = dtH.size_z;
	const u32 Key = GetFMBFieldKey();
	if (!Device.IsEditorMode() && FMBFieldValid && FMBFieldKey == Key && fmb_field_base.size() == (sx + 1) * (sz + 1))
	{
		fmb_field = fmb_field_base;
		return;
	}
	FMBFieldKey = Key;
	FMBFieldValid = true;
	fmb_field.resize((sx + 1) * (sz + 1));

	if (!(ps_r__detail_fmb_use_layer_1 || ps_r__detail_fmb_use_layer_2 || ps_r__detail_fmb_use_layer_3))
	{
		fill(fmb_field.begin(), fmb_field.end(), 0.0f); // mid t; never sampled while all layers are off
		return;
	}

	for (u32 z = 0; z <= sz; z++)
	{
		for (u32 x = 0; x <= sx; x++)
		{
			// Slot corner world pos (mirrors QueryDB grid mapping).
			float world_x = ((float)int(x) - (float)dtH.offs_x) * dm_slot_size;
			float world_z = ((float)int(z) - (float)dtH.offs_z) * dm_slot_size;

			float t = -1e30f;

			if (ps_r__detail_fmb_use_layer_1)
			{
				float n = fastNoise2D(world_x * ps_r__detail_fmb_layer_1_frequency + ps_r__detail_fmb_layer_1_seed, world_z * ps_r__detail_fmb_layer_1_frequency + ps_r__detail_fmb_layer_1_seed);
				float mapped = n * ps_r__detail_fmb_layer_1_amplitude + (1 - ps_r__detail_fmb_layer_1_amplitude) * 0.5f;
				t = fmaxf(t, mapped * ps_r__detail_fmb_layer_1_power);
			}

			if (ps_r__detail_fmb_use_layer_2)
			{
				float n = fastNoise2D(world_x * ps_r__detail_fmb_layer_2_frequency + ps_r__detail_fmb_layer_2_seed, world_z * ps_r__detail_fmb_layer_2_frequency + ps_r__detail_fmb_layer_2_seed);
				float mapped = n * ps_r__detail_fmb_layer_2_amplitude + (1 - ps_r__detail_fmb_layer_2_amplitude) * 0.5f;
				t = fmaxf(t, mapped * ps_r__detail_fmb_layer_2_power);
			}

			if (ps_r__detail_fmb_use_layer_3)
			{
				float n = fastNoise2D(world_x * ps_r__detail_fmb_layer_3_frequency + ps_r__detail_fmb_layer_3_seed, world_z * ps_r__detail_fmb_layer_3_frequency + ps_r__detail_fmb_layer_3_seed);
				float mapped = n * ps_r__detail_fmb_layer_3_amplitude + (1 - ps_r__detail_fmb_layer_3_amplitude) * 0.5f;
				t = fmaxf(t, mapped * ps_r__detail_fmb_layer_3_power);
			}

			fmb_field[z * (sx + 1) + x] = t;
		}
	}
}

// Bilinear lookup of the combined FMB factor t at a world position.
// Field is one float per slot corner; falls back to 1.0f (== maxScale) when absent.
float CDetailManager::SampleFMBField(float world_x, float world_z) const
{
	if (fmb_field.empty())
		return 1.0f;

	u32 sx = dtH.size_x;
	u32 sz = dtH.size_z;

	float c_x = clampr(world_x / dm_slot_size + (float)dtH.offs_x, 0.f, (float)sx);
	float c_z = clampr(world_z / dm_slot_size + (float)dtH.offs_z, 0.f, (float)sz);

	u32 ix0 = (u32)iFloor(c_x);
	u32 iz0 = (u32)iFloor(c_z);
	u32 ix1 = std::min(ix0 + 1, sx);
	u32 iz1 = std::min(iz0 + 1, sz);
	float fx = c_x - (float)ix0;
	float fz = c_z - (float)iz0;

	u32 row0 = iz0 * (sx + 1);
	u32 row1 = iz1 * (sx + 1);

	float v00 = fmb_field[row0 + ix0];
	float v10 = fmb_field[row0 + ix1];
	float v01 = fmb_field[row1 + ix0];
	float v11 = fmb_field[row1 + ix1];

	float v0 = v00 + (v10 - v00) * fx;
	float v1 = v01 + (v11 - v01) * fx;
	return v0 + (v1 - v0) * fz;
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
	D.WindSampleBounds.invalidate();
	if (D.empty)
	{
		return;
	}

	DetailSlot& DS = *D.DS;

	// Select polygons: the editor picks its snap objects, the game queries the level collision
	const bool EditorMode = Device.IsEditorMode();
	xr_vector<Fvector> pick_verts;
	xr_vector<CDB::TRI>* tris = nullptr;
	xr_vector<CDB::RESULT>* results = nullptr;
	u32 triCount = 0;

	if (EditorMode)
	{
		PickSnapTriangles(D.vis.box, pick_verts);
		triCount = u32(pick_verts.size() / 3);
	}
	else
	{
		xrc.box_options(CDB::OPT_FULL_TEST);
		xrc.box_query(g_pGameLevel->ObjectSpace.GetStaticModel(), D.vis.box);
		triCount = xrc.r_count();
		tris = &g_pGameLevel->ObjectSpace.GetStaticTris();
		results = &xrc.r_vec();
	}

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

	float rnd_scale_min = EditorMode ? 0.5f : ps_r__detail_rnd_scale_min;
	float rnd_scale_max = EditorMode ? 0.9f : ps_r__detail_rnd_scale_max;

	SlotDiffusion Diffusion;
	const u32 AssetCount = ps_r__detail_use_alternative_tree_assets && alt_models_count > 0 ? alt_models_count : vanilla_grass_count;
	const int SlotX = iFloor(D.vis.box.min.x / dm_slot_size + .5f);
	const int SlotZ = iFloor(D.vis.box.min.z / dm_slot_size + .5f);
	const auto ReadAsset = [&](int X, int Z) -> u32
	{
		const int DX = X + dtH.offs_x, DZ = Z + dtH.offs_z;
		if (DX < 0 || DZ < 0 || DX >= int(dtH.size_x) || DZ >= int(dtH.size_z))
		{
			return 255;
		}
		const u32 Cell = u32(DZ) * dtH.size_x + u32(DX);
		if (Cell >= cluster_field.size() || cluster_field[Cell] >= AssetCount)
		{
			return 255;
		}
		return HasClusterGrass(dtSlots[Cell], objects) ? cluster_field[Cell] : 255;
	};
	const u32 MainAsset = ps_r__detail_use_cluster_mix_tree_assets ? ReadAsset(SlotX, SlotZ) : 255;
	if (MainAsset != 255)
	{
		const u32 FieldIndex = u32(SlotZ + dtH.offs_z) * dtH.size_x + u32(SlotX + dtH.offs_x);
		const u32 SeedByte = FieldIndex < cluster_rnd_field.size() ? cluster_rnd_field[FieldIndex] : 0;
		Diffusion.Seed = hash_u32(u32(SlotX)) ^ hash_u32(u32(SlotZ) + 374761393u) ^ SeedByte;
		for (u32 Corner = 0; Corner < 4; ++Corner)
		{
			const int NodeX = std::clamp(SlotX + int(Corner & 1u), -dtH.offs_x, int(dtH.size_x) - dtH.offs_x - 1);
			const int NodeZ = std::clamp(SlotZ + int(Corner >> 1u), -dtH.offs_z, int(dtH.size_z) - dtH.offs_z - 1);
			u32 Asset = ReadAsset(NodeX, NodeZ);
			// Missing edge nodes use the same global fallback for both adjacent slots.
			for (int NZ = NodeZ - 1; Asset == 255 && NZ <= NodeZ + 1; ++NZ)
			{
				for (int NX = NodeX - 1; Asset == 255 && NX <= NodeX + 1; ++NX)
				{
					Asset = ReadAsset(NX, NZ);
				}
			}
			if (Asset == 255)
			{
				Asset = MainAsset;
			}
			u32 Group = u32(std::find(Diffusion.Assets, Diffusion.Assets + Diffusion.Count, Asset) - Diffusion.Assets);
			if (Group == Diffusion.Count)
			{
				Diffusion.Assets[Diffusion.Count++] = Asset;
			}
			Diffusion.CornerGroups[Corner] = Group;
		}
		for (u32 I = 0; I < dm_obj_in_slot; ++I)
		{
			D.G[I].id = I < Diffusion.Count
				? (ps_r__detail_use_alternative_tree_assets && alt_models_count > 0 ? alt_models_start + Diffusion.Assets[I] : vanilla_grass_indices[Diffusion.Assets[I]])
				: DetailSlot::ID_Empty;
		}
	}

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
			if (Diffusion.Count)
			{
				const int RootX = iFloor(Item_P.x / dm_slot_size), RootZ = iFloor(Item_P.z / dm_slot_size);
				if ((RootX != SlotX || RootZ != SlotZ) && ReadAsset(RootX, RootZ) == 255)
				{
					// Jitter must not carry a cluster root onto leaf / empty terrain slots.
					continue;
				}
			}

			// Position (Y)
			float y = D.vis.box.min.y - 5;
			float r_u, r_v, r_range;
			bool no_push = false;
			for (u32 tid = 0; tid < triCount; tid++)
			{
				if (EditorMode)
				{
					Fvector* verts = &pick_verts[tid * 3];
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
					continue;
				}

				CDB::RESULT& R = (*results)[tid];
				CDB::TRI& T = (*tris)[R.id];
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
			const CDetail& Dobj = *objects[obj_id];
			if (!(ps_r__detail_fmb_use_layer_1 || ps_r__detail_fmb_use_layer_2 ||  ps_r__detail_fmb_use_layer_3) && user_fmb_mask.empty())
			{
				scale = r_scale.randF(Dobj.m_fMinScale * rnd_scale_min, Dobj.m_fMaxScale * rnd_scale_max);
			}
			else
			{
				minScale = Dobj.m_fMinScale * rnd_scale_min;
				maxScale = Dobj.m_fMaxScale * rnd_scale_max;

				// FMB поле предрассчитано на cache_ReInitialize (как cluster_field):
				// билинейная выборка вместо вызова шума на каждый инстанс.
				// Отрицательные значения — очистка травы пользователем (t<0 => без травы).
				float t = SampleFMBField(Item_P.x, Item_P.z);
				if (t < 0.f)
					continue;
				scale = minScale + (maxScale - minScale) * t;
			}

			mResult.k.x = r_yaw.randF(-0.99, 0.99);
			mResult.k.z = r_yaw.randF(-0.99, 0.99);

			u8 vis_ID = Dobj.m_Flags.is(DO_NO_WAVING) ? 0 : Random.randI(1, 3);

			bool is_leaf = Dobj.m_Flags.is(DO_NO_WAVING) || Dobj.number_vertices < 8;
			bool is_grass = !is_leaf;

			if (is_grass)
			{
				if (Diffusion.Count)
				{
					const u32 Key = Diffusion.Seed ^ hash_u32(z * (d_size + 1) + x);
					index = Diffusion.SampleGroup(Item_P.x / dm_slot_size - float(SlotX), Item_P.z / dm_slot_size - float(SlotZ), Key);
					obj_id = u8(D.G[index].id);
				}
				else if (!ps_r__detail_use_cluster_mix_tree_assets && ps_r__detail_use_alternative_tree_assets && alt_models_count > 0)
				{
					for (u32 G = 0; G < vanilla_grass_count; ++G)
					{
						if (vanilla_grass_indices[G] == obj_id)
						{
							if (G < alt_models_count)
							{
								obj_id = u8(alt_models_start + G);
							}
							break;
						}
					}
				}
			}

			// чтобы (только) листики травы ложились на поверхность террейна
			//	if (vis_ID == 0)
			ground_correction(mResult, normal);
			float radius = scale * objects[obj_id]->bv_bb.getradius();

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
			ItemP->trample_strength = 0.0f;
			ItemP->trample_visual = 0.0f;
			ItemP->trample_dirX = 0.0f;
			ItemP->trample_dirZ = 0.0f;
			ItemP->WindBend.set(0, 0, 0, 0);
			ItemP->WindPrevious.set(0, 0, 0, 0);
			ItemP->WindPreviousVelocity.set(0, 0, 0, 0);
			ItemP->WindVelocity.set(0, 0, 0, std::bit_cast<float>(u32(-1)));
			const CDetail& WindModel = *objects[obj_id];
			if (!WindModel.m_Flags.is(DO_NO_WAVING))
			{
				Fvector SamplePosition = Item_P;
				SamplePosition.y += .6f * ((WindModel.bv_bb.max.y - WindModel.bv_bb.min.y) * scale);
				D.WindSampleBounds.modify(SamplePosition);
			}

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
