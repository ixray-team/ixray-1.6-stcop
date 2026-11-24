#include "stdafx.h"
#include "LevelPuddles.h"
#include "GameMtlLib.h"

namespace
{
	u16 FindPuddleMaterial(const char* Name)
	{
		const auto Material = GMLib.GetMaterialIt(Name);
		return Material == GMLib.LastMaterial() ? GAMEMTL_NONE_IDX : u16(Material - GMLib.FirstMaterial());
	}
}

void CLevelPuddles::Load()
{
	Clear();
	string_path Path;
	if (!FS.exist(Path, "$level$", "level.puddles"))
	{
		return;
	}

	CInifile Ini(Path);
	Surfaces.reserve(Ini.sections().size());
	Index.reserve(Ini.sections().size());
	for (const auto& Section : Ini.sections())
	{
		const char* Name = Section.Name.c_str();
		if (!Ini.line_exist(Name, "position") || !Ini.line_exist(Name, "rotation") ||
			!Ini.line_exist(Name, "size_xz") || !Ini.line_exist(Name, "max_height"))
		{
			continue;
		}

		const Fvector Position = Ini.r_fvector3(Name, "position");
		const Fvector2 Size = Ini.r_fvector2(Name, "size_xz");
		const float MaxHeight = Ini.r_float(Name, "max_height");
		float X = 0.f;
		float Y = 0.f;
		float Z = 0.f;
		const char* Rotation = Ini.r_string(Name, "rotation");
		const int AngleCount = Rotation ? sscanf(Rotation, "%f , %f , %f", &X, &Y, &Z) : 0;
		if ((AngleCount != 1 && AngleCount != 3) || !_valid(Position) || !std::isfinite(MaxHeight) ||
			!std::isfinite(Size.x) || !std::isfinite(Size.y) || Size.x <= 0.f || Size.y <= 0.f ||
			!std::isfinite(X) || !std::isfinite(Y) || !std::isfinite(Z))
		{
			continue;
		}

		Fmatrix World;
		if (AngleCount == 3)
		{
			// The original loader overwrote the X and Y rotations with the Z rotation.
			World.rotateZ(-Z);
		}
		else
		{
			World.rotateY(X);
		}
		World.mulB_43(Fmatrix().scale(Size.x, 1.f, Size.y));
		World.translate_over(Position);

		Fmatrix Inverse;
		if (!Inverse.invert_b(World))
		{
			continue;
		}

		const Fbox LocalBounds(-1.f, std::min(0.f, MaxHeight), -1.f, 1.f, std::max(0.f, MaxHeight), 1.f);
		Fbox Bounds;
		Bounds.xform(LocalBounds, World).grow(EPS_L);
		Index.push_back({Bounds, Inverse, u32(Surfaces.size())});
		Surfaces.push_back({World, MaxHeight, Size.magnitude()});
	}

	if (!Surfaces.empty())
	{
		Build(0, u32(Index.size()));
		WaterMaterial = FindPuddleMaterial(R"(materials\water)");
		HumanMaterial = FindPuddleMaterial(R"(creatures\human)");
		BulletMaterial = FindPuddleMaterial(R"(objects\bullet)");
	}
}

void CLevelPuddles::Clear()
{
	Surfaces.clear();
	Index.clear();
	Nodes.clear();
	WaterMaterial = GAMEMTL_NONE_IDX;
	HumanMaterial = GAMEMTL_NONE_IDX;
	BulletMaterial = GAMEMTL_NONE_IDX;
}

void CLevelPuddles::Build(u32 First, u32 Count)
{
	Fbox Bounds = Index[First].Bounds;
	for (u32 Item = First + 1; Item < First + Count; ++Item)
	{
		Bounds.merge(Index[Item].Bounds);
	}

	const u32 NodeIndex = u32(Nodes.size());
	Nodes.push_back({Bounds, First, Count, 0});
	if (Count <= 4)
	{
		return;
	}

	Fvector Size;
	Bounds.getsize(Size);
	int Axis = Size.x >= Size.z ? 0 : 2;
	if (Size.y > Size[Axis])
	{
		Axis = 1;
	}

	const u32 LeftCount = Count / 2;
	const auto Begin = Index.begin() + First;
	std::nth_element(Begin, Begin + LeftCount, Begin + Count, [Axis](const IndexedSurface& A, const IndexedSurface& B)
	{
		return A.Bounds.min[Axis] + A.Bounds.max[Axis] < B.Bounds.min[Axis] + B.Bounds.max[Axis];
	});
	Build(First, LeftCount);
	Nodes[NodeIndex].Right = u32(Nodes.size());
	Nodes[NodeIndex].Count = 0;
	Build(First + LeftCount, Count - LeftCount);
}

bool CLevelPuddles::Trace(const Fvector& Start, const Fvector& Direction, float Range, float Wetness, Hit& Result) const
{
	if (Nodes.empty() || Wetness <= 0.f || Range <= 0.f)
	{
		return false;
	}

	const Surface* Best = nullptr;
	TraceNode(0, Start, Direction, Wetness, Range, Best);
	if (!Best)
	{
		return false;
	}

	Result.Position.mad(Start, Direction, Range);
	Result.Normal = Best->World.j;
	return true;
}

void CLevelPuddles::TraceNode(u32 NodeIndex, const Fvector& Start, const Fvector& Direction, float Wetness,
	float& Range, const Surface*& Best) const
{
	const Node& Current = Nodes[NodeIndex];
	const Fvector End = Fvector().mad(Start, Direction, Range);
	Fbox SegmentBounds(Fvector(Start).min(End), Fvector(Start).max(End));
	if (!SegmentBounds.intersect(Current.Bounds))
	{
		return;
	}

	Fvector BoxHit;
	if (Fbox(Current.Bounds).Pick2(Start, Direction, BoxHit) == Fbox::rpNone)
	{
		return;
	}

	if (!Current.Count)
	{
		TraceNode(NodeIndex + 1, Start, Direction, Wetness, Range, Best);
		TraceNode(Current.Right, Start, Direction, Wetness, Range, Best);
		return;
	}

	for (u32 Item = Current.First; Item < Current.First + Current.Count; ++Item)
	{
		const IndexedSurface& Entry = Index[Item];
		if (!SegmentBounds.intersect(Entry.Bounds))
		{
			continue;
		}

		const Surface& Puddle = Surfaces[Entry.SurfaceIndex];
		Fvector LocalStart, LocalDirection;
		Entry.Inverse.transform_dir(LocalStart, Fvector().sub(Start, Puddle.World.c));
		Entry.Inverse.transform_dir(LocalDirection, Direction);
		if (LocalDirection.y >= -EPS_S)
		{
			continue;
		}

		const float Distance = (Wetness * Puddle.MaxHeight - LocalStart.y) / LocalDirection.y;
		if (Distance <= 0.f || Distance > Range)
		{
			continue;
		}

		const Fvector Point = Fvector().mad(LocalStart, LocalDirection, Distance);
		if (std::abs(Point.x) <= 1.f && std::abs(Point.z) <= 1.f)
		{
			Range = Distance;
			Best = &Puddle;
		}
	}
}

SGameMtlPair* CLevelPuddles::GetMaterialPair(u16 Material) const
{
	if (Material == GAMEMTL_NONE_IDX || WaterMaterial == GAMEMTL_NONE_IDX)
	{
		return nullptr;
	}

	return GMLib.GetMaterialPair(Material, WaterMaterial);
}

SGameMtlPair* CLevelPuddles::GetDefaultStepPair() const
{
	return GetMaterialPair(HumanMaterial);
}

SGameMtlPair* CLevelPuddles::GetDefaultBulletPair() const
{
	return GetMaterialPair(BulletMaterial);
}
