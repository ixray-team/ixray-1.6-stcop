#if !defined(_DSGMANAGE_H_)
#define _DSGMANAGE_H_
#pragma once
#include "r__dsgraph_types.h"
#include "r__sector.h"
#include "../../xrCore/FixedMap.h"
class CDSGraphManager : public IDSGraphManager
{
public:
	enum
	{
		VQ_HOM = (1 << 0),
		VQ_SSA = (1 << 1),
		VQ_FADE = (1 << 2),				// requires SSA to work
	};

	R_dsgraph::DynamicSceneRgraph RGraph;
	struct TraverserData
	{
		u32 sector_marker;
		xr_vector<CFrustum> frustums;
		FixedMAP<CPortal*, u32> portals;
	};
	u32										i_marker;		// input
	u32										i_options;		// input:	culling options
	u32										i_doptions;
	Fvector									i_vBase;		// input:	"view" point
	CFrustum								i_frustum;		// input:	"view" frustum
	Fmatrix									i_mXFORM;		// input:	4x4 xform
	CSector* i_start;		// input:	starting point
	xrCriticalSection						P_CS, T_CS;

	FixedMAP<CSector*, TraverserData>		m_sector_frustums;
	FixedSet<dxRender_Visual*>				m_visuals_static, m_visuals_dynamic;
	xr_vector<ISpatialShared>				lstRenderables, lstLights;
	FixedMAP<CPortal*, float>				f_portals;
	sPoly S, D;
	ref_shader								f_shader;
	ref_geom								f_geom;

	CDSGraphManager(u32 options, u32 doptions, bool(&& mask)[7]) : i_options(options), i_doptions(doptions), i_marker(u32(-1)) { std::copy(mask, mask + 7, i_mask); }
	void initialize();
	void destroy();
	void traverse(CSector* start, CFrustum& F, Fvector& vBase, Fmatrix& mXFORM);
	IC bool is_sector_visible(CSector* sector)
	{
		if (sector == i_start) return true;

		if (auto node = m_sector_frustums.find(sector))
			return i_marker == node->val.sector_marker;

		return false;
	};
	CDSGraphManager& get_traverser_safed() { xrCriticalSectionGuard guard(&T_CS); return *this; };
	void fade_portal(CPortal* _p, float ssa);
	void fade_render();

	virtual void set_Object(IRenderable* O = nullptr);

	virtual void add_Static(IRenderVisual* pVisual, CFrustum& frustum, u32 planes);
	void add_leafs_Static(xr_vector<dxRender_Visual*>& children);
	void r_dsgraph_insert_static(dxRender_Visual* pVisual);

	virtual void add_Dynamic(IRenderVisual* piVisual, Fmatrix* xform);
	void add_leafs_Dynamic(xr_vector<dxRender_Visual*>& children, Fmatrix* xform);
	void r_dsgraph_insert_dynamic(dxRender_Visual* pVisual, Fmatrix* xform);


	void r_dsgraph_render_graph(R_dsgraph::mapDSGraphPasses* graph, u32 _priority, bool _clear = true, bool static_geometry = true);

	IC void r_dsgraph_render_graph(u32 _priority, bool _clear = true)
	{
		r_dsgraph_render_static(_priority, _clear);
		r_dsgraph_render_dynamic(_priority, _clear);
	}
	IC void r_dsgraph_render_static(u32 _priority, bool _clear = true)
	{
		PROF_EVENT("r_dsgraph_render_static");
		r_dsgraph_render_graph(RGraph.mapStaticPasses, _priority, _clear);
	};
	IC void	r_dsgraph_render_dynamic(u32 _priority, bool _clear = true)
	{
		PROF_EVENT("r_dsgraph_render_dynamic");
		r_dsgraph_render_graph(RGraph.mapDynamicPasses, _priority, _clear, false);
	};

	void r_dsgraph_render_graph_sorted(R_dsgraph::mapDSGraphItems& graph, bool _clear = true);
	void r_dsgraph_capture_hud();
	void r_dsgraph_render_hud();
	void r_dsgraph_render_hud_ui();
	void r_dsgraph_render_lods(bool _setup_zb, bool _clear);
	void r_dsgraph_render_sorted(bool render_hud = true);
	void r_dsgraph_render_sorted_hud();
	void r_dsgraph_render_emissive();
	void r_dsgraph_render_wmarks();
	void r_dsgraph_render_distort();


	void r_dsgraph_capture(bool lights = false, bool dynamic = false, CObject* O = nullptr);
	void r_dsgraph_capture_lights();
	void r_dsgraph_capture_static();
	void r_dsgraph_capture_dynamic(CObject* O = nullptr);

	void clear()
	{
		RGraph.clear();
		m_sector_frustums.clear();
		m_visuals_static.clear();
		m_visuals_dynamic.clear();
		lstRenderables.clear();
		lstLights.clear();
		f_portals.clear();
		i_marker = u32(-1);
	}
};

#endif // !defined(AFX_PORTAL_H__1FC2D371_4A19_49EA_BD1E_2D0F8DEBBF15__INCLUDED_)
