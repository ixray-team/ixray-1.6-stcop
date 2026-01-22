#pragma once
#include "../../xrUI/ui_defs.h"
struct LevelInspector final
{
	enum ESCENE_FLAGS
	{
		ESF_NONE = 0,
		ESF_DRAW = 1,
		ESF_DRAW_L_GRID = 2,
		ESF_DRAW_G_GRID = 4,
		ESF_DRAW_W_GRID = 8,
		ESF_DRAW_OBJECTS = 16,
		ESF_DRAW_ZONES = 32,
		ESF_DRAW_AI_PATHS = 64,
		ESF_DRAW_SELECTION = 128,
		ESF_DRAW_HUD = 256,
		ESF_DRAW_ALL_SPATIALS = 512,
	};
	
	enum ESELECTION_FLAGS
	{
		ESLF_NONE = 0,
		ESLF_O = 1,
		ESLF_Z = 2,
		ESLF_WP = 4,
		ESLF_GP = 8,
		ESLF_LG = 16,
	};


	enum EOBJECT_INFO
	{
		EOI_NONE = 0,
		EOI_SNAME = 1,
		EOI_LCNAME = 2,
		EOI_LNAME = 4,
		EOI_VNAME = 8,
		EOI_INI = 16,
		EOI_POSITION = 32,
		EOI_GVERTEX_LVERTEX = 64,
		EOI_ACTOR = 128,
	};

	enum ESKELETON_INFO
	{
		ESI_NONE = 0,
		ESI_BONES = 1,
		ESI_BONES_INFO = 2,
		ESI_BONES_LINKS = 4,
		ESI_HIT_SHAPES = 8,
		ESI_BBOXES = 16,
		ESI_FIRE_POINTS = 32,
		ESI_MAIN_BBOX = 64,
		ESI_ACTOR = 128,
		ESI_PH_BBOX = 256,
	};

	enum EZONE_INFO
	{
		EZI_NONE = 0,
		EZI_RESTR = 1,
		EZI_SMART_TERRAIN = 2,
		EZI_LEVEL_CHANGER = 4,
		EZI_SMART_COVER = 8,
		EZI_CAMP_ZONE = 16,
		EZI_ANOMALY_ZONE = 32,
		EZI_ANOMAL_ZONE_LOGIC = 64,
		EZI_SIM_FACTION = 128,
	};

	enum EWAYPOINT_INFO
	{
		EWI_NONE = 0,
		EWI_PREFIX = 1,
		EWI_LICATION_ID = 2,
		EWI_ALL = 4,
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
	xr_string selected_info_str;
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




	Flags32 m_flags = { 0 };
	Flags32 m_selection_flags = { 0 };
	Flags32 m_selection_text_flags = { 0 };
	Flags32 m_skeleton_flags = { 0 };
	Flags32 m_waypoint_flags = { 0 };
	Flags32 m_zone_flags = { 0 };
	ESPATIAL_TYPE m_spatials_mask{ ESPATIAL_TYPE(-1)};

	BOOL zbuffer_enable = TRUE;
	BOOL visible_currents = TRUE;
	BOOL hud_mode = FALSE;

	LevelInspector(BOOL hm = FALSE);
	~LevelInspector();

	void OnRender();

	ICF bool append_text3d(const Fvector& pos, shared_str str = "+", u32 color = color_rgba(0, 255, 100, 255), CGameFont::EAligment align = CGameFont::alCenter)
	{
		Fvector4 v_res;
		if(hud_mode)
			Device.mFullTransform_hud_special.transform(v_res, pos);
		else
			Device.mFullTransform.transform(v_res, pos);

		float x = (1.f + v_res.x) / 2.f * (Device.Width);
		float y = (1.f - v_res.y) / 2.f * (Device.Height);

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
	ICF void append_geometry(const V* vertices, const L& line_indices, const T& tri_indices, u32 lcolor = u32(0), u32 tcolor = u32(0), IUIRender::LITFast** fastbuff = nullptr)
	{
		const size_t line_count = std::size(line_indices);
		const size_t tri_count = std::size(tri_indices);
		const size_t max_count = std::max(line_count, tri_count);

		for (size_t i = 0; i < max_count; ++i)
		{
			if (tcolor > 0 && i < tri_count)
			{
				if (fastbuff)
				{
					(*fastbuff)->p = vertices[tri_indices[i].i1];
					(*fastbuff)->color = tcolor;
					(*fastbuff)++;

					(*fastbuff)->p = vertices[tri_indices[i].i2];
					(*fastbuff)->color = tcolor;
					(*fastbuff)++;

					(*fastbuff)->p = vertices[tri_indices[i].i3];
					(*fastbuff)->color = tcolor;
					(*fastbuff)++;
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
				if (fastbuff)
				{
					(*fastbuff)->p = vertices[line_indices[i].i1];
					(*fastbuff)->color = lcolor;
					(*fastbuff)++;

					(*fastbuff)->p = vertices[line_indices[i].i2];
					(*fastbuff)->color = lcolor;
					(*fastbuff)++;
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

	ICF void append_obb(const Fobb& obb, u32 lcolor = u32(0), u32 tcolor = u32(0), IUIRender::LITFast** fastbuff = nullptr)
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

	ICF void append_aabb(const Fbox& box, u32 lcolor = u32(0), u32 tcolor = u32(0), IUIRender::LITFast** fastbuff = nullptr)
	{
		Fobb obb;
		obb.m_rotate.identity();
		box.get_CD(obb.m_translate, obb.m_halfsize);
		append_obb(obb, lcolor, tcolor, fastbuff);
	}

	ICF void append_sphere(const Fvector& pos, float radius, u32 lcolor = u32(0), u32 tcolor = u32(0), IUIRender::LITFast** fastbuff = nullptr)
	{
		append_sphere(Fsphere{ pos, radius }, lcolor, tcolor, fastbuff);
	}

	ICF void append_sphere(const Fsphere& sphere, u32 lcolor = u32(0), u32 tcolor = u32(0), IUIRender::LITFast** fastbuff = nullptr)
	{
		append_ellipse(sphere.P, Fvector{ sphere.R, sphere.R, sphere.R }, lcolor, tcolor, fastbuff);
	}

	ICF void append_ellipse(const Fvector& pos, const Fvector& scale, u32 lcolor = u32(0), u32 tcolor = u32(0), IUIRender::LITFast** fastbuff = nullptr)
	{
		Fmatrix matrix;
		matrix.identity();
		matrix.scale(scale);
		matrix.translate_add(pos);
		append_ellipse(matrix, lcolor, tcolor, fastbuff);
	}

	ICF void append_ellipse(const Fmatrix& matrix, u32 lcolor = u32(0), u32 tcolor = u32(0), IUIRender::LITFast** fastbuff = nullptr)
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

	ICF void append_cylinder(const Fcylinder& cylinder, u32 lcolor = u32(0), u32 tcolor = u32(0), IUIRender::LITFast** fastbuff = nullptr)
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

	ICF void append_cone(const Fcylinder& cone/*когда-то добавлю отдельный класс Fcone*/, u32 lcolor = u32(0), u32 tcolor = u32(0), IUIRender::LITFast** fastbuff = nullptr)
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

	ICF void append_graph_point(const Fvector& pos, u32 lcolor = u32(0), u32 tcolor = u32(0), IUIRender::LITFast** fastbuff = nullptr)
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

	void DrawLevelGraph();

	void DrawHud();

	void DrawSkeleton(IKinematics* pKinematics, Fmatrix& xform, CGameObject* GO = nullptr);
};