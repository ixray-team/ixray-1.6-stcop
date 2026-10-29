#include "stdafx.h"
#include "RainPuddleWaterMap.h"
#include "../../xrEngine/GameMtlLib.h"

namespace RainPuddleWater
{
constexpr u32 MapSize = 2048;
constexpr float NoWater = -1.0e20f;

float Edge(const Fvector& A, const Fvector& B, float X, float Z)
{
	return (B.x - A.x) * (Z - A.z) - (B.z - A.z) * (X - A.x);
}
} // namespace RainPuddleWater

RainPuddleWaterMap::~RainPuddleWaterMap()
{
	Destroy();
}

void RainPuddleWaterMap::Destroy()
{
	if (Texture)
	{
		Texture->surface_set(nullptr);
		Texture.destroy();
	}
	Bounds.set(0.0f, 0.0f, 0.0f, 0.0f);
	HeightRange.set(0.0f, 0.0f, 0.0f, 0.0f);
}

void RainPuddleWaterMap::Build(CObjectSpace* Space)
{
	CTimer BuildTimer;
	BuildTimer.Start();
	Bounds.set(0.0f, 0.0f, 0.0f, 0.0f);
	HeightRange.set(0.0f, 0.0f, 0.0f, 0.0f);
	u32 Size = 1;
	xr_vector<float> Heights(1, RainPuddleWater::NoWater);
	xr_vector<xr_array<Fvector, 3>> WaterTriangles;
	Fbox WaterBounds;
	WaterBounds.invalidate();
	
	auto QueryData = xr_pair{&WaterTriangles, &WaterBounds};

	if (Space)
	{
		static CDB::COLLIDER xrc;
		xrc.custom_query(Space->GetStaticModel(), [](const CDB::MODEL& Model, const Fmatrix& ToWorldTransform, const CDB::BVHNode& Node, void* ptr)
		{
			return true;
		}, nullptr,
		[](const CDB::MODEL& Model, const Fmatrix& ToWorldTransform, CDB::ElementID InPrim, void* ptr)
		{
			VERIFY(InPrim.IsNotPointer);
			if (InPrim.IsInstance)
			{
				return;
			}
			
			auto& StaticTris = Model.tris;
			auto& verts = Model.verts;
			auto& Tri = StaticTris[InPrim.Index];
			
			if (Tri.material >= GMLib.CountMaterial())
			{
				return;
			}
			const SGameMtl* Material = GMLib.GetMaterialByIdx(Tri.material);
			// Passable liquid triangles describe water surfaces, rather than wet solid ground.
			if (!Material->Flags.test(SGameMtl::flLiquid) || !Material->Flags.test(SGameMtl::flPassable))
			{
				return;
			}
			
			auto& TriVerts = Tri.verts;
			xr_array<Fvector, 3> tri_verts;
			ToWorldTransform.transform_tiny(tri_verts[0], verts[TriVerts[0]]);
			ToWorldTransform.transform_tiny(tri_verts[1], verts[TriVerts[1]]);
			ToWorldTransform.transform_tiny(tri_verts[2], verts[TriVerts[2]]);
			
			if (abs(RainPuddleWater::Edge(tri_verts[0], tri_verts[1], tri_verts[2].x, tri_verts[2].z)) < EPS)
			{
				return;
			}
			((decltype(QueryData)*)ptr)->first->push_back(tri_verts);
			((decltype(QueryData)*)ptr)->second->modify(tri_verts[0]);
			((decltype(QueryData)*)ptr)->second->modify(tri_verts[1]);
			((decltype(QueryData)*)ptr)->second->modify(tri_verts[2]);
			
		}, &QueryData);

		if (!WaterTriangles.empty())
		{
			Size = RainPuddleWater::MapSize;
			Heights.assign(Size * Size, RainPuddleWater::NoWater);
			// Keep empty border texels so filtered coverage also fades at the map bounds.
			const float PaddingX = (WaterBounds.max.x - WaterBounds.min.x) * 2.0f / float(Size - 4);
			const float PaddingZ = (WaterBounds.max.z - WaterBounds.min.z) * 2.0f / float(Size - 4);
			WaterBounds.min.x -= PaddingX;
			WaterBounds.max.x += PaddingX;
			WaterBounds.min.z -= PaddingZ;
			WaterBounds.max.z += PaddingZ;
			HeightRange.set(WaterBounds.min.y - 1.0f, WaterBounds.max.y - WaterBounds.min.y + 2.0f, 0.0f, 0.0f);
			const float StepX = (WaterBounds.max.x - WaterBounds.min.x) / float(Size);
			const float StepZ = (WaterBounds.max.z - WaterBounds.min.z) / float(Size);
			Bounds.set(WaterBounds.min.x, WaterBounds.min.z, 1.0f / (StepX * Size), 1.0f / (StepZ * Size));

			for (auto& Triangle : WaterTriangles)
			{
				const Fvector& A = Triangle[0];
				const Fvector& B = Triangle[1];
				const Fvector& C = Triangle[2];
				const float Area = RainPuddleWater::Edge(A, B, C.x, C.z);
				// Cover boundary texels conservatively so shallow shoreline water has no gaps.
				const float RadiusA = 0.5f * (abs(C.x - B.x) * StepZ + abs(C.z - B.z) * StepX) / abs(Area);
				const float RadiusB = 0.5f * (abs(A.x - C.x) * StepZ + abs(A.z - C.z) * StepX) / abs(Area);
				const float RadiusC = 0.5f * (abs(B.x - A.x) * StepZ + abs(B.z - A.z) * StepX) / abs(Area);
				const float MinX = std::min({A.x, B.x, C.x});
				const float MaxX = std::max({A.x, B.x, C.x});
				const float MinZ = std::min({A.z, B.z, C.z});
				const float MaxZ = std::max({A.z, B.z, C.z});
				const int BeginX = std::clamp(int((MinX - WaterBounds.min.x) / StepX), 0, int(Size) - 1);
				const int EndX = std::clamp(int((MaxX - WaterBounds.min.x) / StepX), 0, int(Size) - 1);
				const int BeginZ = std::clamp(int((MinZ - WaterBounds.min.z) / StepZ), 0, int(Size) - 1);
				const int EndZ = std::clamp(int((MaxZ - WaterBounds.min.z) / StepZ), 0, int(Size) - 1);

				for (int Z = BeginZ; Z <= EndZ; ++Z)
				{
					for (int X = BeginX; X <= EndX; ++X)
					{
						const float WorldX = WaterBounds.min.x + (float(X) + 0.5f) * StepX;
						const float WorldZ = WaterBounds.min.z + (float(Z) + 0.5f) * StepZ;
						const float WeightA = RainPuddleWater::Edge(B, C, WorldX, WorldZ) / Area;
						const float WeightB = RainPuddleWater::Edge(C, A, WorldX, WorldZ) / Area;
						const float WeightC = 1.0f - WeightA - WeightB;
						if (WeightA >= -RadiusA - EPS && WeightB >= -RadiusB - EPS && WeightC >= -RadiusC - EPS)
						{
							float& Height = Heights[u32(Z) * Size + u32(X)];
							Height = std::max(Height, A.y * WeightA + B.y * WeightB + C.y * WeightC);
						}
					}
				}
			}
		}
	}

	// Store normalized height and coverage separately. Hardware filtering must not
	// interpolate a valid water height with the empty-cell sentinel.
	xr_vector<u32> Pixels(Heights.size(), 0);
	for (size_t Index = 0; Index < Heights.size(); ++Index)
	{
		if (Heights[Index] > RainPuddleWater::NoWater)
		{
			const float Height = std::clamp((Heights[Index] - HeightRange.x) / HeightRange.y, 0.0f, 1.0f);
			Pixels[Index] = u32(Height * 65535.0f + 0.5f) | 0xffff0000u;
		}
	}

	RHITextureDesc Description{};
	Description.Width = Size;
	Description.Height = Size;
	Description.Depth = 1;
	Description.MipLevels = 1;
	Description.Format = ERHI_FORMAT::R16G16_UNORM;
	Description.Usage = ERHI_USAGE::USAGE_DEFAULT;
	Description.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;
	RHISubResource Data{};
	Data.Data = Pixels.data();
	Data.DataSize = Size * sizeof(u32);
	IRHISurface* Surface = GRHI->CreateTexture2D(Description, Data);
	if (!Surface)
	{
		Bounds.set(0.0f, 0.0f, 0.0f, 0.0f);
		Msg("! Rain puddles: failed to create water height map");
		return;
	}
	Texture.create("$user$rain_puddles_water_height");
	Texture->surface_set(Surface);
	Surface->Release();
	if (Space)
	{
		Msg("* Rain puddles: water height map %ux%u, %u liquid triangles, built in %.2f ms", Size, Size, u32(WaterTriangles.size()), BuildTimer.GetElapsed_ms_f());
	}
}
