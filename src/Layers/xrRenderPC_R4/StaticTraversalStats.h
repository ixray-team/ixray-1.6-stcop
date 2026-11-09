#pragma once

class CSector;

class CStaticTraversalStats
{
public:
	struct SectorCounters
	{
		CSector* Sector = nullptr;
		u32 Frustums = 0;
		u32 Visited = 0;
		u32 RejectValuable = 0;
		u32 RejectFrustum = 0;
		u32 RejectFullDetail = 0;
		u32 RejectHOM = 0;
		u32 RejectFog = 0;
		u32 PartialHierarchies = 0;
		u32 FullHierarchies = 0;
		u32 InsertCalls = 0;
		u32 InsertDuplicates = 0;
		u32 InsertDiscarded = 0;
		u32 Inserted = 0;
		float TimeMs = 0.f;
	};

	SectorCounters* Current = nullptr;

	void Request();
	bool BeginFrame();
	void BeginSector(CSector* Sector);
	void EndSector();
	void EndFrame();

private:
	std::atomic_bool Requested = false;
	bool Active = false;
	xr_vector<SectorCounters> Sectors;
	CTimer SectorTimer;
	CTimer FrameTimer;
};

extern CStaticTraversalStats g_StaticTraversalStats;
