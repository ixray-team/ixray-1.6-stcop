#pragma once

class CPortal;
class CSector;

class CPortalTraverser
{
public:
	enum
	{
		VQ_HOM = (1 << 0),
		VQ_SSA = (1 << 1),
		VQ_FADE = (1 << 3), // requires SSA to work
	};

public:
	static constexpr u32 MaxPortalVisits = 8;

	u32 i_marker;									 // input
	u32 i_options;									 // input:	culling options
	Fvector i_vBase;								 // input:	"view" point
	float NearDistance;								 // portals closer to the view point are crossed without narrowing
	Fmatrix i_mXFORM;								 // input:	4x4 xform
	CSector* i_start;								 // input:	starting point
	xr_vector<IRender_Sector*> r_sectors;			 // result
	xr_vector<std::pair<CPortal*, float>> f_portals; //
	ref_shader f_shader;
	ref_geom f_geom;

	// Views can traverse in parallel, so the clips stay here instead of on CSector / CPortal.
	struct PortalState
	{
		u32 Marker;
		u32 Visits;
		u32 FadeMarker;
		bool OnPath;
	};
	float ViewportNear = 0.f; // a copy: the device value can change under a worker
	xr_vector<xr_vector<CFrustum>> local_frustums;
	xr_vector<u32> local_sector_marker;
	xr_vector<PortalState> local_portals;

public:
	CPortalTraverser();
	void initialize();
	void destroy();
	void prepare_local_clips(u32 sector_count, u32 portal_count);
	bool IsVisited(u32 SectorIndex) const { return SectorIndex < local_sector_marker.size() && local_sector_marker[SectorIndex] == i_marker; }
	void traverse(IRender_Sector* start, CFrustum& F, Fvector& vBase, Fmatrix& mXFORM, u32 options);
	void fade_portal(CPortal* _p, float ssa);
	void fade_render(GfxCmdList& Cmd);
	bool IsNearPortal(const CPortal& Portal) const;
};
