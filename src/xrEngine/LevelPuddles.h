#pragma once

struct SGameMtlPair;

class ENGINE_API CLevelPuddles
{
public:
	struct Surface
	{
		Fmatrix World;
		float MaxHeight;
		float Radius;
	};

	struct Hit
	{
		Fvector Position;
		Fvector Normal;
	};

	void Load();
	void Clear();
	bool Trace(const Fvector& Start, const Fvector& Direction, float Range, float Wetness, Hit& Result) const;
	SGameMtlPair* GetMaterialPair(u16 Material) const;
	SGameMtlPair* GetDefaultStepPair() const;
	SGameMtlPair* GetDefaultBulletPair() const;

	const xr_vector<Surface>& GetSurfaces() const { return Surfaces; }

private:
	struct IndexedSurface
	{
		Fbox Bounds;
		Fmatrix Inverse;
		u32 SurfaceIndex;
	};

	struct Node
	{
		Fbox Bounds;
		u32 First;
		u32 Count;
		u32 Right;
	};

	void Build(u32 First, u32 Count);
	void TraceNode(u32 NodeIndex, const Fvector& Start, const Fvector& Direction, float Wetness, float& Range, const Surface*& Best) const;

	xr_vector<Surface> Surfaces;
	xr_vector<IndexedSurface> Index;
	xr_vector<Node> Nodes;
	u16 WaterMaterial = u16(-1);
	u16 HumanMaterial = u16(-1);
	u16 BulletMaterial = u16(-1);
};
