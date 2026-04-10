#pragma once
#include "../../xrUI/ui_defs.h"
struct LevelInspector final
{
	enum ESCENE_FLAGS
	{
		ESF_NONE					= 0U,
		ESF_DRAW_L_GRID				= bit_lshift(0U),
		ESF_DRAW_G_GRID				= bit_lshift(1U),
		ESF_DRAW_W_GRID				= bit_lshift(2U),
		ESF_DRAW_OBJECTS			= bit_lshift(3U),
		ESF_DRAW_ZONES				= bit_lshift(4U),
		ESF_DRAW_AI_PATHS			= bit_lshift(5U),
		ESF_DRAW_SELECTION			= bit_lshift(6U),
		ESF_DRAW_HUD				= bit_lshift(7U),

		ESF_DRAW_SPATIAL_SPACE		= bit_lshift(9U),
		ESF_DRAW_SPATIAL_SPACE_ALL	= bit_lshift(10U),
		ESF_DRAW_HOM				= bit_lshift(11U),
		ESF_DRAW_CFORM				= bit_lshift(12U),
		ESF_DRAW_CFORM_ALL			= bit_lshift(13U),
		ESF_DRAW_CFORM_TRIS			= bit_lshift(14U),
		ESF_DRAW_LEVEL_BOUNDS		= bit_lshift(15U),
	};
	
	enum ESELECTION_FLAGS
	{
		ESLF_NONE	= 0U,
		ESLF_O		= bit_lshift(0U),
		ESLF_Z		= bit_lshift(1U),
		ESLF_WP		= bit_lshift(2U),
		ESLF_GP		= bit_lshift(3U),
		ESLF_LG		= bit_lshift(4U),
	};


	enum EOBJECT_INFO
	{
		EOI_NONE			= 0U,
		EOI_SNAME			= bit_lshift(0U),
		EOI_LCNAME			= bit_lshift(1U),
		EOI_LNAME			= bit_lshift(2U),
		EOI_VNAME			= bit_lshift(3U),
		EOI_POSITION		= bit_lshift(4U),
		EOI_GVERTEX_LVERTEX = bit_lshift(5U),
		EOI_INI				= bit_lshift(6U),
		EOI_SCRIPT			= bit_lshift(7U),
		EOI_ACTOR			= bit_lshift(8U),
	};

	enum ESKELETON_INFO
	{
		ESI_NONE			= 0U,
		ESI_BONES			= bit_lshift(0U),
		ESI_BONES_INFO		= bit_lshift(1U),
		ESI_BONES_LINKS		= bit_lshift(2U),
		ESI_BONES_SHAPES	= bit_lshift(3U),
		ESI_BONES_VERTS		= bit_lshift(4U),
		ESI_HIT_SHAPES		= bit_lshift(5U),
		ESI_BBOXES			= bit_lshift(6U),
		ESI_FIRE_POINTS		= bit_lshift(7U),
		ESI_MAIN_BBOX		= bit_lshift(8U),
		ESI_MAIN_SPHERE		= bit_lshift(9U),
		ESI_MAIN_RSPHERE	= bit_lshift(10U),
		ESI_ACTOR			= bit_lshift(11U),
		ESI_PH_BBOX			= bit_lshift(12U),
	};

	enum EZONE_INFO
	{
		EZI_NONE				= 0U,
		EZI_RESTR				= bit_lshift(0U),
		EZI_SMART_TERRAIN		= bit_lshift(1U),
		EZI_LEVEL_CHANGER		= bit_lshift(2U),
		EZI_SMART_COVER			= bit_lshift(3U),
		EZI_CAMP_ZONE			= bit_lshift(4U),
		EZI_ANOMALY_ZONE		= bit_lshift(5U),
		EZI_ANOMAL_ZONE_LOGIC	= bit_lshift(6U),
		EZI_SIM_FACTION			= bit_lshift(7U),
	};

	enum class EWAYPOINT_INFO
	{
		EWI_NONE,
		EWI_PREFIX,
		EWI_LOCATION_ID,
		EWI_ALL,
	};

	struct lindex { u8 i1, i2; };
	struct tindex { u8 i1, i2, i3; };
	struct lvertex { Fvector v1, v2; u32 color = u32(0); };
	struct tvertex { Fvector v1, v2, v3; u32 color = u32(0); };

	collide::ray_defs RD{ zero_vel, zero_vel, 0.f, 0, collide::rq_target(0) };
	xr_vector<ISpatialShared> m_objects;
	xr_vector<lvertex> lines;
	xr_vector<tvertex> tris;
	collide::rq_results RQR;
	collide::rq_result RQ;
	xr_string selected_info_str, script_info;
	int selected_info_height = 0;

	CGameFont* dbg_font = nullptr;
	float font_spacing = 0.9f;
	xr_concurrent_unordered_map<shared_str, CGameFont*> m_clone_fonts_map;

	LevelInspector* hud_prims = nullptr;
	ui_shader shader;

	shared_str wp_prefix;
	bool wp_recalc = true;
	u32 zbuffer_key = 225u;
	u32 visible_currents_key = 224u;


	float cform_ssa = 0.00005;
	float cform_aabb_aspect_ratio = 1.0f;
	float zbuff_shift = 0.005f;

	Flags32 m_flags = { 0 };
	Flags32 m_selection_flags = { 0 };
	Flags32 m_selection_text_flags = { 0 };
	Flags32 m_skeleton_flags = { 0 };
	EWAYPOINT_INFO WaypointsFlags = EWAYPOINT_INFO::EWI_NONE;
	Flags32 m_zone_flags = { 0 };
	ESPATIAL_TYPE m_spatials_mask{ ESPATIAL_TYPE::NONE};

	BOOL zbuffer_enable = TRUE;
	BOOL visible_currents = TRUE;
	BOOL hud_mode = FALSE;

	LevelInspector(BOOL hm = FALSE);
	~LevelInspector();

	void OnRender();

	void TrisRender(xr_vector<tvertex>& _tris, bool clear = true);
	void LinesRender(xr_vector<lvertex>& _lines, bool clear = true);

	ICF bool append_text3d(const Fvector& pos, shared_str str = "+", u32 color = color_rgba(0, 255, 100, 255), CGameFont::EAligment align = CGameFont::alCenter)
	{
		Fvector4 v_res;
		if(hud_mode)
			Device.mFullTransform_hud_special.transform(v_res, pos);
		else
			Device.mFullTransform.transform(v_res, pos);

		float x = (1.f + v_res.x) / 2.f * (Device.TargetWidth);
		float y = (1.f - v_res.y) / 2.f * (Device.TargetHeight);

		if (v_res.z < 0 || v_res.w < 0)
			return false;

		if (v_res.x < -1.f || v_res.x > 1.f || v_res.y < -1.f || v_res.y>1.f)
			return false;

		append_text2d(x, y, str, color, align);

		return true;
	}

	ICF void append_text2d(float x, float y, shared_str str = "+", u32 color = color_rgba(0, 255, 100, 255), CGameFont::EAligment align = CGameFont::alCenter)
	{
		dbg_font->SetAligment(align);
		dbg_font->SetColor(color);
		dbg_font->OutSet(x, y);
		dbg_font->OutNext(*str);
	}

	ICF void append_text_next(shared_str str = "+")
	{
		dbg_font->OutNext(*str);
	}

	ICF void append_line(const Fvector& v1, const Fvector& v2, u32 clr) { swap_color_channels(clr); lines.push_back({ v1, v2, clr}); }
	ICF void append_line(lvertex& line) { swap_color_channels(line.color); lines.push_back(line); }
	ICF void append_line(lvertex&& line) { swap_color_channels(line.color); lines.push_back(line); }

	ICF void append_tri(const Fvector& v1, const Fvector& v2, const Fvector& v3, u32 clr) { swap_color_channels(clr); tris.push_back({ v1, v2, v3, clr }); }
	ICF void append_tri(tvertex& tri) { swap_color_channels(tri.color); tris.push_back(tri); }
	ICF void append_tri(tvertex&& tri) { swap_color_channels(tri.color); tris.push_back(tri); }
	ICF void append_trilines(tvertex& tri, u32 lcolor)
	{
		append_tri(tri);
		append_line({ tri.v1, tri.v2, lcolor });
		append_line({ tri.v1, tri.v3, lcolor });
		append_line({ tri.v3, tri.v2, lcolor });
	}
	ICF void append_trilines(const Fvector(&tri_verts)[3], u32 tcolor, u32 lcolor)
	{
		swap_color_channels(tcolor);
		swap_color_channels(lcolor);
		append_tri({ tri_verts[0],tri_verts[1],tri_verts[2], tcolor });
		
		append_line({ tri_verts[0], tri_verts[1], lcolor });
		append_line({ tri_verts[0], tri_verts[2], lcolor });
		append_line({ tri_verts[2], tri_verts[1], lcolor });
	}

	ICF void append_lines_arrow(const Fvector& arrow_start, const Fvector& arrow_dir, float arrow_len, u32 arrow_color)
	{
		Fvector arrow_end;
		arrow_end.mad(arrow_start, arrow_dir, arrow_len);

		Fvector perp1, perp2;
		if (abs(arrow_dir.x) < 0.9f)
			perp1.set(1.0f, 0.0f, 0.0f);
		else
			perp1.set(0.0f, 1.0f, 0.0f);

		perp2.crossproduct(perp1, arrow_dir);
		perp2.normalize();
		perp1.crossproduct(arrow_dir, perp2);
		perp1.normalize();

		float angle_deg = deg2rad(25.f);
		float cos_value = std::cos(angle_deg);
		float angles[3] = { 0.0f, 2.0943951f, 4.1887902f };
		Fvector tip_dir;
		for (int i = 0; i < 3; i++)
		{
			Fvector side_dir;
			side_dir.mul(perp1, std::cos(angles[i]));
			side_dir.mad(side_dir, perp2, std::sin(angles[i]));
			side_dir.normalize();

			tip_dir = Fvector(arrow_dir).invert() * cos_value + side_dir * angle_deg;
			tip_dir.normalize();

			append_line({ arrow_end, Fvector().mad(arrow_end, tip_dir, arrow_len * 0.3f), arrow_color });
		}

		append_line({ arrow_start, arrow_end, arrow_color });
	}


	ICF void append_axis(const Fmatrix& xform, float axis_scale = 0.1f, bool append_text = false)
	{
		Fvector pos_z = Fvector(xform.c).add(Fvector(xform.k).mul(axis_scale));
		Fvector pos_x = Fvector(xform.c).add(Fvector(xform.i).mul(axis_scale));
		Fvector pos_y = Fvector(xform.c).add(Fvector(xform.j).mul(axis_scale));
		append_line({xform.c, pos_z, color_rgba(0, 0, 255, 255)});//z
		append_line({xform.c, pos_x, color_rgba(255, 0, 0, 255)});//x
		append_line({xform.c, pos_y, color_rgba(0, 255, 0, 255)});//y
		if (!append_text)
			return;
		append_text3d(pos_z, "z", color_rgba(0, 0, 255, 255));
		append_text3d(pos_x, "x", color_rgba(255, 0, 0, 255));
		append_text3d(pos_y, "y", color_rgba(0, 255, 0, 255));
	}

	//RGBA=>BGRA
	ICF void swap_color_channels(u32& color)
	{
		color = (color & 0xFF00FF00) | ((color & 0x000000FF) << 16) | ((color & 0x00FF0000) >> 16);
	}

	template<typename V, typename L, typename T>
	ICF void append_geometry(const V* vertices, const L& line_indices, const T& tri_indices, u32 lcolor = u32(0), u32 tcolor = u32(0), void** buff = nullptr)
	{
		const size_t line_count = std::size(line_indices);
		const size_t tri_count = std::size(tri_indices);
		const size_t max_count = std::max(line_count, tri_count);
		IUIRender::r_vertL<3>** fastbuff3 = (IUIRender::r_vertL<3>**)buff;
		IUIRender::r_vertL<2>** fastbuff2 = (IUIRender::r_vertL<2>**)buff;
		for (size_t i = 0; i < max_count; ++i)
		{
			if (tcolor > 0 && i < tri_count)
			{
				if (fastbuff3)
				{
					*(*fastbuff3) =
					{
						vertices[tri_indices[i].i1], tcolor,
						vertices[tri_indices[i].i2], tcolor,
						vertices[tri_indices[i].i3], tcolor
					};
					(*fastbuff3)++;
				}
				else
					tris.push_back({
					vertices[tri_indices[i].i1],
					vertices[tri_indices[i].i2],
					vertices[tri_indices[i].i3],
					tcolor
						});
			}
			if (lcolor > 0 && i < line_count)
			{
				if (fastbuff2)
				{
					*(*fastbuff2) =
					{
						vertices[line_indices[i].i1], lcolor,
						vertices[line_indices[i].i2], lcolor
					};
					(*fastbuff2)++;
				}
				else
					lines.push_back({
					vertices[line_indices[i].i1],
					vertices[line_indices[i].i2],
					lcolor
						});
			}
		}
	}

	ICF void append_obb(const Fobb& obb, u32 lcolor = u32(0), u32 tcolor = u32(0), void** fastbuff = nullptr)
	{
		extern Fvector obb_vertices[8];
		extern lindex obb_lindices[12];
		extern tindex obb_tindices[12];

		swap_color_channels(lcolor);
		swap_color_channels(tcolor);
		Fmatrix matrix;
		obb.xform_full(matrix);
		Fvector vertices[std::size(obb_vertices)];
		for (size_t i = 0ULL; i < std::size(obb_vertices); ++i)
			matrix.transform_tiny(vertices[i], obb_vertices[i]);

		append_geometry(vertices, obb_lindices, obb_tindices, lcolor, tcolor, fastbuff);
	}

	ICF void append_aabb(const Fbox& box, u32 lcolor = u32(0), u32 tcolor = u32(0), void** fastbuff = nullptr)
	{
		extern lindex aabb_lindices[12];
		extern tindex aabb_tindices[12];

		swap_color_channels(lcolor);
		swap_color_channels(tcolor);
		Fvector vertices[8];
		const_cast<Fbox&>(box).getpoints(vertices);

		append_geometry(vertices, aabb_lindices, aabb_tindices, lcolor, tcolor, fastbuff);
	}

	ICF void append_sphere(const Fvector& pos, float radius, u32 lcolor = u32(0), u32 tcolor = u32(0), void** fastbuff = nullptr)
	{
		append_sphere(Fsphere{ pos, radius }, lcolor, tcolor, fastbuff);
	}

	ICF void append_sphere(const Fsphere& sphere, u32 lcolor = u32(0), u32 tcolor = u32(0), void** fastbuff = nullptr)
	{
		append_ellipse(sphere.P, Fvector{ sphere.R, sphere.R, sphere.R }, lcolor, tcolor, fastbuff);
	}

	ICF void append_ellipse(const Fvector& pos, const Fvector& scale, u32 lcolor = u32(0), u32 tcolor = u32(0), void** fastbuff = nullptr)
	{
		Fmatrix matrix;
		matrix.identity();
		matrix.scale(scale);
		matrix.translate_add(pos);
		append_ellipse(matrix, lcolor, tcolor, fastbuff);
	}

	ICF void append_ellipse(const Fmatrix& matrix, u32 lcolor = u32(0), u32 tcolor = u32(0), void** fastbuff = nullptr)
	{
		extern Fvector sphere_vertices[114];
		extern lindex sphere_lindices[48];
		extern tindex sphere_tindices[224];

		swap_color_channels(lcolor);
		swap_color_channels(tcolor);
		Fvector vertices[std::size(sphere_vertices)];
		for (size_t i = 0ULL; i < std::size(sphere_vertices); ++i)
			matrix.transform_tiny(vertices[i], sphere_vertices[i]);

		append_geometry(vertices, sphere_lindices, sphere_tindices, lcolor, tcolor, fastbuff);
	}

	ICF void append_cylinder(const Fcylinder& cylinder, u32 lcolor = u32(0), u32 tcolor = u32(0), void** fastbuff = nullptr)
	{
		extern Fvector cylinder_vertices[24];
		extern lindex cylinder_lindices[36];
		extern tindex cylinder_tindices[46];

		swap_color_channels(lcolor);
		swap_color_channels(tcolor);

		Fvector dir = cylinder.m_direction;
		dir.normalize_safe();

		Fvector up(0.f, 1.f, 0.f);
		float dot = dir.dotproduct(up);

		if (std::abs(dot) > 0.999f)
		{
			up.set(0.f, 0.f, 1.f);
			dot = dir.dotproduct(up);

			if (std::abs(dot) > 0.999f)
				up.set(1.f, 0.f, 0.f);
		}

		Fvector right;
		right.crossproduct(up, dir);
		right.normalize_safe();

		Fvector real_up;
		real_up.crossproduct(dir, right);
		real_up.normalize_safe();

		Fmatrix mR;
		mR.i = right;			  mR._14 = 0.f;
		mR.j = real_up;			  mR._24 = 0.f;
		mR.k = dir;				  mR._34 = 0.f;
		mR.c = cylinder.m_center; mR._44 = 1.f;

		Fmatrix mScale;
		float diam = 2.f * cylinder.m_radius;
		mScale.scale(diam, diam, cylinder.m_height);

		Fmatrix matrix;
		matrix.mul(mR, mScale);

		Fvector vertices[std::size(cylinder_vertices)];
		for (size_t i = 0ULL; i < std::size(cylinder_vertices); ++i)
			matrix.transform_tiny(vertices[i], cylinder_vertices[i]);

		append_geometry(vertices, cylinder_lindices, cylinder_tindices, lcolor, tcolor, fastbuff);
	}

	ICF void append_cone(const Fcylinder& cone/*когда-то добавлю отдельный класс Fcone*/, u32 lcolor = u32(0), u32 tcolor = u32(0), void** fastbuff = nullptr)
	{
		extern Fvector cone_vertices[17];
		extern lindex cone_lindices[32];
		extern tindex cone_tindices[30];

		swap_color_channels(lcolor);
		swap_color_channels(tcolor);

		Fvector dir = cone.m_direction;
		dir.normalize_safe();

		Fvector up(0.f, 1.f, 0.f);
		float dot = dir.dotproduct(up);

		if (std::abs(dot) > 0.999f)
		{
			up.set(0.f, 0.f, 1.f);
			dot = dir.dotproduct(up);

			if (std::abs(dot) > 0.999f)
				up.set(1.f, 0.f, 0.f);
		}

		Fvector right;
		right.crossproduct(up, dir);
		right.normalize_safe();

		Fvector real_up;
		real_up.crossproduct(dir, right);
		real_up.normalize_safe();

		Fmatrix mR;
		mR.i = right;			  mR._14 = 0.f;
		mR.j = real_up;			  mR._24 = 0.f;
		mR.k = dir;				  mR._34 = 0.f;
		mR.c = cone.m_center; mR._44 = 1.f;

		Fmatrix mScale;
		float diam = 2.f * cone.m_radius;
		mScale.scale(diam, diam, cone.m_height);

		Fmatrix matrix;
		matrix.mul(mR, mScale);

		Fvector vertices[std::size(cone_vertices)];
		for (size_t i = 0ULL; i < std::size(cone_vertices); ++i)
			matrix.transform_tiny(vertices[i], cone_vertices[i]);

		append_geometry(vertices, cone_lindices, cone_tindices, lcolor, tcolor, fastbuff);
	}

	ICF void append_graph_point(const Fvector& pos, u32 lcolor = u32(0), u32 tcolor = u32(0), void** fastbuff = nullptr)
	{
		extern Fvector graph_point_vertices[5];
		extern lindex graph_point_lindices[8];
		extern tindex graph_point_tindices[4];

		swap_color_channels(lcolor);
		swap_color_channels(tcolor);

		Fvector vertices[std::size(graph_point_vertices)];
		for (size_t i = 0ULL; i < std::size(graph_point_vertices); ++i)
			vertices[i] = pos + graph_point_vertices[i];

		append_geometry(vertices, graph_point_lindices, graph_point_tindices, lcolor, tcolor, fastbuff);
	}

	void DrawObjectInfo(CGameObject* GO, const Fvector& pos, Fvector2 xy);
	void DrawObjectsInfo();
	void DrawAIPaths();
	void DrawGameGraph();
	void DrawWayPoints();
	void DrawObjects();
	void DrawSpatials();
	void DrawHOM();
	void DrawCFORM();

	void DrawLevelGraph();

	void DrawHud();

	void DrawSkeleton(IKinematics* pKinematics, Fmatrix& xform, CGameObject* GO = nullptr);
};