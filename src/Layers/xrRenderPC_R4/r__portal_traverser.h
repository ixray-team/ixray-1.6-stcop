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
		VQ_SCISSOR = (1 << 2),
		VQ_FADE = (1 << 3), // requires SSA to work
	};

public:
	u32 i_marker;									 // input
	u32 i_options;									 // input:	culling options
	Fvector i_vBase;								 // input:	"view" point
	Fmatrix i_mXFORM;								 // input:	4x4 xform
	Fmatrix i_mXFORM_01;							 //
	CSector* i_start;								 // input:	starting point
	xr_vector<IRender_Sector*> r_sectors;			 // result
	xr_vector<std::pair<CPortal*, float>> f_portals; //
	ref_shader f_shader;
	ref_geom f_geom;

	// Reflection faces traverse together with the main view, so their clips
	// stay here instead of on CSector / CPortal.
	bool own_clips = false;
	xr_vector<xr_vector<CFrustum>> local_frustums;
	xr_vector<u32> local_sector_marker;
	xr_vector<u32> local_portal_marker;

public:
	CPortalTraverser();
	void initialize();
	void destroy();
	void prepare_local_clips(u32 sector_count, u32 portal_count);
	void traverse(IRender_Sector* start, CFrustum& F, Fvector& vBase, Fmatrix& mXFORM, u32 options);
	void fade_portal(CPortal* _p, float ssa);
	void fade_render(GfxCmdList& Cmd);
};
