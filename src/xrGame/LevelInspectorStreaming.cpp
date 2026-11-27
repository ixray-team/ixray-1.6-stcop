#include "StdAfx.h"
#include "LevelInspector.h"
#include "GamePersistent.h"
#include "Level.h"
#include "Actor.h"
#include "ai_space.h"
#include "ai_object_location.h"
#include "patrol_path.h"
#include "patrol_path_storage.h"
#include "../xrEngine/GameMtlLib.h"
#include "../xrCore/Collision/override/Model.h"

extern LevelInspector::lindex aabb_lindices[12];
extern LevelInspector::tindex aabb_tindices[12];
u32 positionToColorWithAlpha(const Fvector& C);

namespace LevelInspectorStreamingDetail
{
	constexpr u32 MaxLines = 8192;
	constexpr u32 MaxTris = 4096;
	constexpr u32 MaxNodes = 4096;

	float FogDistance()
	{
		const CEnvDescriptor* descriptor = g_pGamePersistent->Environment().CurrentEnv;
		return descriptor != nullptr ? descriptor->fog_distance : 250.f;
	}

	bool HasRoom(const LevelInspector& inspector)
	{
		return inspector.lines.size() < MaxLines && inspector.tris.size() < MaxTris;
	}

	u32 NodeColor(const ILevelGraph& graph, u32 id)
	{
		if (!graph.is_accessible(id))
			return color_rgba(128, 128, 128, 180);

		u32 neighbors = 0;
		const ILevelGraph::CVertex* vertex = graph.vertex(id);
		for (u32 direction = 0; direction < 4; ++direction)
		{
			if (graph.valid_vertex_id(vertex->link(direction)))
				++neighbors;
		}
		switch (neighbors)
		{
		case 0: return color_rgba(0, 0, 255, 255);
		case 1: return color_rgba(0, 100, 255, 210);
		case 2: return color_rgba(0, 180, 255, 180);
		case 3: return color_rgba(50, 220, 255, 150);
		default: return color_rgba(200, 220, 50, 100);
		}
	}
}

void LevelInspector::DrawWayPointsStreaming()
{
	if (WaypointsFlags == EWAYPOINT_INFO::EWI_NONE || !ai().level_graph().has_nodes())
		return;

	constexpr float Size = 1.5f;
	constexpr float Height = Size * .85f;
	constexpr float Radius = Size * .5f;
	constexpr float Low = Size * .15f;
	constexpr u32 LinkColor = color_rgba(230, 155, 75, 75);
	constexpr u32 SelectedLinkColor = color_rgba(255, 250, 180, 255);
	const float Distance = visible_currents ? 50000.f : LevelInspectorStreamingDetail::FogDistance();
	IGameGraph& gameGraph = ai().game_graph();
	ILevelGraph& levelGraph = ai().level_graph();
	const Fvector& camera = Device.vCameraPosition;
	const CFrustum& frustum = ::Render->ViewBase;
	bool WaypointSelected = false;

	for (const auto& [pathName, path] : ai().patrol_paths().patrol_paths())
	{
		if (path == nullptr)
			continue;
		if (WaypointsFlags == EWAYPOINT_INFO::EWI_PREFIX && wp_prefix && pathName && pathName.c_str() != strstr(pathName.c_str(), wp_prefix.c_str()))
			continue;

		for (const auto& [pointId, vertex] : path->vertices())
		{
			if (vertex == nullptr || !LevelInspectorStreamingDetail::HasRoom(*this))
				continue;
			const CPatrolPoint& point = vertex->data();
			const IGameGraph::CVertex* gameVertex = gameGraph.vertex(point.game_vertex_id());
			const u8 levelId = gameVertex != nullptr ? gameVertex->level_id() : levelGraph.level_id();
			if (WaypointsFlags == EWAYPOINT_INFO::EWI_LOCATION_ID && levelId != levelGraph.level_id())
				continue;

			Fvector position = point.position();
			position.y += Height;
			if (camera.distance_to_sqr(position) > _sqr(Distance + Size))
				continue;
			if (!visible_currents && !frustum.testSphere_dirty(position, Size))
				continue;

			const u32 color = levelId != levelGraph.level_id() ? color_rgba(255, 0, 0, 255) : color_rgba(0, 255, 0, 255);
			Fbox bounds;
			bounds.setb(position, { Radius, Radius, Radius });
			Fobb obb;
			obb.identity();
			bounds.get_CD(obb.m_translate, obb.m_halfsize);

			bool selected = false;
			if (m_flags.test(ESF_DRAW_SELECTION) && m_selection_flags.test(ESLF_WP))
			{
				float range = RD.range;
				if (visible_currents || (!WaypointSelected && obb.intersect(camera, Device.vCameraDirection, range)))
				{
					bool text3d = append_text3d(position, pathName, color, CGameFont::alLeft);
					if (text3d)
						append_text_next(point.name());
					else
						append_text3d(position, point.name(), color, CGameFont::alLeft);
					for (u32 bit = 0; bit < 32; ++bit)
					{
						if ((point.flags() & (1u << bit)) != 0)
							append_text3d(position, shared_str().printf("flag: %u", bit), color, CGameFont::alLeft);
					}
					const auto level = gameGraph.header().levels().find(levelId);
					if (level != gameGraph.header().levels().end())
						append_text3d(position, level->second.name(), color, CGameFont::alLeft);
					append_selection_box(obb);
					if (!visible_currents)
					{
						selected_info_str = shared_str().printf("%s\n%s", pathName.c_str(), point.name().c_str()).c_str();
						selected_info_height = 2;
						WaypointSelected = true;
						RD.range = range;
					}
					selected = true;
				}
			}

			append_line({ { position.x + Radius, position.y, position.z }, { position.x - Radius, position.y, position.z }, color });
			append_line({ { position.x, position.y + Low, position.z }, { position.x, position.y - Height, position.z }, color });
			append_line({ { position.x, position.y, position.z + Radius }, { position.x, position.y, position.z - Radius }, color });

			for (const auto& edge : vertex->edges())
			{
				if (edge.vertex() == nullptr)
					continue;
				Fvector link = edge.vertex()->data().position();
				link.y += Height;
				const u32 linkColor = selected ? SelectedLinkColor : LinkColor;
				append_line({ link, position, linkColor });
				Fvector direction, right, normal, tip[2];
				normal.set(0.f, 1.f, 0.f);
				direction.sub(link, position).normalize();
				right.crossproduct(normal, direction).mul(.5f);
				direction.mul(2.f);
				normal.mul(.5f);
				tip[0].add(right, direction).mul(-.125f).add(link);
				right.invert();
				tip[1].add(right, direction).mul(-.125f).add(link);
				append_line({ link, tip[0], linkColor });
				append_line({ link, tip[1], linkColor });
				tip[0].add(normal, direction).mul(-.125f).add(link);
				normal.invert();
				tip[1].add(normal, direction).mul(-.125f).add(link);
				append_line({ link, tip[0], linkColor });
				append_line({ link, tip[1], linkColor });
			}
		}
	}
}

void LevelInspector::DrawGameGraphStreaming()
{
	IGameGraph& graph = ai().game_graph();
	ILevelGraph& levelGraph = ai().level_graph();
	if (graph.header().vertex_count() == 0 || !levelGraph.has_nodes())
		return;
	if (GraphConfig == nullptr)
	{
		string_path path;
		FS.update_path(path, "$game_config$", "game_graphs.ltx");
		GraphConfig = new CInifile(path);
	}

	const float distance = visible_currents ? 50000.f : LevelInspectorStreamingDetail::FogDistance();
	const Fvector& camera = Device.vCameraPosition;
	const CFrustum& frustum = ::Render->ViewBase;
	bool GameVertexSelected = false;
	for (u32 id = 0; id < graph.header().vertex_count(); ++id)
	{
		const IGameGraph::CVertex* vertex = graph.vertex(id);
		if (vertex == nullptr || vertex->level_id() != levelGraph.level_id())
			continue;
		const Fvector& position = vertex->level_point();
		if (camera.distance_to_sqr(position) > _sqr(distance + 2.f) || (!visible_currents && !frustum.testSphere_dirty(position, 2.f)))
			continue;

		const bool blue = vertex->vertex_type()[0] || vertex->vertex_type()[3];
		const bool blueBlack = vertex->vertex_type()[1] || vertex->vertex_type()[2];
		u32 lineColor = blue ? color_rgba(0, 255, 255, 255) : color_rgba(255, 0, 255, 255);
		u32 triColor = blueBlack ? color_rgba(0, 70, 255, 100) : blue ? color_rgba(0, 255, 255, 60) : color_rgba(255, 0, 255, 60);
		xr_string connections;
		shared_str nextLevel;
		IGameGraph::const_iterator begin, end;
		graph.begin(id, begin, end);
		for (auto edge = begin; edge != end; ++edge)
		{
			const IGameGraph::CVertex* child = graph.vertex(edge->vertex_id());
			if (child == nullptr)
				continue;
			connections += shared_str().printf(" %u", edge->vertex_id()).c_str();
			if (child->level_id() != levelGraph.level_id())
			{
				const auto level = graph.header().levels().find(child->level_id());
				if (level != graph.header().levels().end())
					nextLevel = level->second.name();
				continue;
			}

			const Fvector& childPosition = child->level_point();
			Fvector middle;
			middle.add(position, childPosition).mul(.5f);
			if (camera.distance_to_sqr(middle) > _sqr(distance + 2.f) || (!visible_currents && !frustum.testSphere_dirty(middle, 2.f)))
				continue;
			const u32 childLineColor = child->vertex_type()[0] || child->vertex_type()[3] ? color_rgba(0, 255, 255, 255) : lineColor;
			append_line({ Fvector(position).add(Fvector{ 0.f, 1.f, 0.f }), middle, lineColor });
			append_line({ middle, Fvector(childPosition).add(Fvector{ 0.f, 1.f, 0.f }), childLineColor });
		}

		if (m_flags.test(ESF_DRAW_SELECTION) && m_selection_flags.test(ESLF_GP))
		{
			Fobb obb;
			obb.identity();
			obb.m_translate = position;
			obb.m_halfsize.set(.5f, 1.f, .5f);
			float range = RD.range;
			if (visible_currents || (!GameVertexSelected && obb.intersect(camera, Device.vCameraDirection, range)))
			{
				append_text3d(Fvector(position).add(Fvector{ 0.f, 1.1f, 0.f }), shared_str().printf("%u", id), color_rgba(0, 255, 100, 255), CGameFont::alLeft);
				if (nextLevel.size())
					append_text3d(position, shared_str().printf("next_level: %s", nextLevel.c_str()), color_rgba(0, 255, 100, 255), CGameFont::alLeft);
				if (!connections.empty())
					append_text3d(position, shared_str().printf("connections:%s", connections.c_str()), color_rgba(0, 255, 100, 255), CGameFont::alLeft);
				for (u32 location = 0; location < 4 && GraphConfig != nullptr; ++location)
				{
					const shared_str section = shared_str().printf("location_%u", location);
					if (!GraphConfig->section_exist(section))
						continue;
					const u8 value = vertex->vertex_type()[location];
					if (value == 0 || value >= GraphConfig->r_section(section).Data.size())
						continue;
					append_text3d(position, GraphConfig->r_section(section).Data[value].second, color_rgba(0, 255, 100, 255), CGameFont::alLeft);
				}
				selected_info_str = shared_str().printf("%u\nconnections:%s", id, connections.c_str()).c_str();
				selected_info_height = 2;
				if (!visible_currents)
				{
					GameVertexSelected = true;
					RD.range = range;
				}
				triColor = color_rgba(255, 0, 0, 150);
			}
		}
		append_graph_point(position, lineColor, triColor);
		if (!LevelInspectorStreamingDetail::HasRoom(*this))
			break;
	}
}

void LevelInspector::DrawLevelGraphStreaming()
{
	ILevelGraph& graph = ai().level_graph();
	if (!graph.has_nodes() || graph.header().cell_size() <= 0.f)
		return;

	const float radius = graph.header().cell_size() * .35f;
	const float distance = std::min(LevelInspectorStreamingDetail::FogDistance(), 512.f);
	const Fvector& camera = Device.vCameraPosition;
	CFrustum frustum;
	frustum.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);
	UIRender->CacheSetXformWorld(Device.mFullTransform);
	UIRender->SetShader(*shader);
	UIRender->CacheSetCullMode(ERHI_CULLMODE::BACK);
	UIRender->zb_enable(zbuffer_enable);

	u32 selectedId = u32(-1);
	u32 selectedNeighbors[4] = { u32(-1), u32(-1), u32(-1), u32(-1) };
	if (m_flags.test(ESF_DRAW_SELECTION) && m_selection_flags.test(ESLF_LG) && RQR.r_count() && !RQ.valid() && g_actor != nullptr)
	{
		Fvector searchPosition;
		searchPosition.mad(RD.start, RD.dir, RD.range + .1f);
		selectedId = graph.vertex(g_actor->ai_location().level_vertex_id(), searchPosition);
		if (graph.valid_vertex_id(selectedId))
		{
			Fvector position = graph.vertex_position(selectedId);
			if (position.distance_to_sqr(searchPosition) > 1.f)
				selectedId = u32(-1);
			else
			{
				position.y += .06f;
				append_text3d(position, shared_str().printf("%u", selectedId), color_rgba(255, 0, 0, 255), CGameFont::alLeft);
				selected_info_str = shared_str().printf("%u", selectedId).c_str();
				selected_info_height = 1;
				for (u32 direction = 0; direction < 4; ++direction)
					selectedNeighbors[direction] = graph.vertex(selectedId)->link(direction);
			}
		}
	}

	Fvector minimum = camera;
	Fvector maximum = camera;
	minimum.sub(Fvector{ distance, distance, distance });
	maximum.add(Fvector{ distance, distance, distance });
	IUIRender::r_vertL<6> batch[32];
	u32 batchCount = 0;
	u32 nodeCount = 0;
	auto flush = [&]
	{
		if (batchCount == 0)
			return;
		IUIRender::r_vertL<6>** buffer = (IUIRender::r_vertL<6>**)UIRender->StartPrimitive(batchCount * 6u, IUIRender::ptTriList, IUIRender::pttL);
		for (u32 index = 0; index < batchCount; ++index)
			(*buffer)[index] = batch[index];
		*buffer += batchCount;
		UIRender->FlushPrimitive();
		batchCount = 0;
	};

	graph.iterate_vertices(minimum, maximum, [&](const ILevelGraph::CVertex& vertex)
	{
		if (nodeCount >= LevelInspectorStreamingDetail::MaxNodes)
			return;
		const u32 id = graph.vertex_id(&vertex);
		const Fvector position = graph.vertex_position(vertex);
		if (camera.distance_to_sqr(position) > _sqr(distance + radius) || !frustum.testSphere_dirty(position, radius))
			return;

		Fplane plane;
		pvDecompress(plane.n, vertex.plane());
		if (std::abs(plane.n.y) < EPS_S)
			return;
		plane.d = -plane.n.dotproduct(position);
		Fvector v1{ position.x - radius, position.y, position.z - radius };
		Fvector v2{ position.x + radius, position.y, position.z - radius };
		Fvector v3{ position.x + radius, position.y, position.z + radius };
		Fvector v4{ position.x - radius, position.y, position.z + radius };
		graph.project_point(plane, v1);
		graph.project_point(plane, v2);
		graph.project_point(plane, v3);
		graph.project_point(plane, v4);

		u32 color = LevelInspectorStreamingDetail::NodeColor(graph, id);
		if (id == selectedId || id == selectedNeighbors[0] || id == selectedNeighbors[1] || id == selectedNeighbors[2] || id == selectedNeighbors[3])
		{
			Fcolor highlight;
			highlight.set(color);
			highlight.mul_rgba(1.7f);
			color = highlight.get();
		}
		batch[batchCount++] = { v3, color, v2, color, v1, color, v4, color, v3, color, v1, color };
		++nodeCount;
		if (batchCount == std::size(batch))
			flush();
	});
	flush();
}

void LevelInspector::DrawCFORMStreaming()
{
	CDB::MODEL* model = g_pGameLevel->ObjectSpace.GetStaticModel();
	if (!model->has_tree())
		return;

	const Fvector camera = Device.vCameraPosition;
	const Fvector direction = Device.vCameraDirection;
	const CFrustum& frustum = ::Render->ViewBase;
	CDB::RESULT selected = {};
	selected.model = nullptr;
	selected.tris_id = size_t(-1);
	if (m_flags.test(ESF_DRAW_CFORM_TRIS))
	{
		CDB::COLLIDER ray;
		ray.ray_options(CDB::OPT_ONLYNEAREST);
		ray.ray_query(g_pGameLevel->ObjectSpace, camera, direction);
		if (ray.r_count() != 0)
			selected = ray.r_any();
	}

	struct SCformContext
	{
		LevelInspector* Inspector;
		Fvector Camera;
		const CFrustum* Frustum;
		const CDB::RESULT* Selected;
		float Fog;
		u32 Mask;
	} context = { this, camera, &frustum, &selected, LevelInspectorStreamingDetail::FogDistance(), frustum.getMask() };

	CDB::COLLIDER query;
	query.custom_query(g_pGameLevel->ObjectSpace,
		[](const CDB::MODEL& model, const Fmatrix& transform, const CDB::BVHNode& node, void* data)
		{
			SCformContext& context = *(SCformContext*)data;
			LevelInspector& inspector = *context.Inspector;
			if (!LevelInspectorStreamingDetail::HasRoom(inspector))
				return false;

			Fbox bounds;
			bounds.invalidate();
			for (size_t index = 0; index < node.GetSize(); ++index)
			{
				const CDB::ElementID element = node.GetElement(index);
				const bool leaf = !node.HasNode(index);
				Fbox child = leaf && element.IsInstance ? model.get_instances()[element.Index].GlobalAABB : node.GetAABB(index);
				for (u32 corner = 0; corner < 8; ++corner)
				{
					Fvector point;
					child.getpoint(corner, point);
					transform.transform_tiny(point);
					bounds.modify(point);
				}
			}
			Fvector center;
			Fvector extents;
			bounds.get_CD(center, extents);
			const float distanceSqr = context.Camera.distance_to_sqr(center) + EPS;
			const float radius = extents.magnitude();
			if (radius / distanceSqr <= inspector.cform_ssa || distanceSqr > _sqr(context.Fog + radius))
				return false;
			if (fcvNone == context.Frustum->testAABB(bounds.data(), context.Mask))
				return false;
			if (inspector.m_flags.test(ESF_DRAW_CFORM))
			{
				for (size_t index = 0; index < node.GetSize(); ++index)
				{
					const CDB::ElementID element = node.GetElement(index);
					const bool leaf = !node.HasNode(index);
					if ((!leaf && !inspector.m_flags.test(ESF_DRAW_CFORM_ALL)) || !LevelInspectorStreamingDetail::HasRoom(inspector))
						continue;
					Fbox childBounds;
					childBounds.invalidate();
					const Fbox& child = leaf && element.IsInstance ? model.get_instances()[element.Index].GlobalAABB : node.GetAABB(index);
					for (u32 corner = 0; corner < 8; ++corner)
					{
						Fvector point;
						child.getpoint(corner, point);
						transform.transform_tiny(point);
						childBounds.modify(point);
					}
					if (childBounds.contains(context.Camera))
						continue;
					Fvector vertices[8];
					childBounds.getpoints(vertices);
					childBounds.getcenter(center);
					for (const tindex& triangle : aabb_tindices)
						inspector.append_tri(vertices[triangle.i1], vertices[triangle.i2], vertices[triangle.i3], positionToColorWithAlpha(center));
					for (const lindex& edge : aabb_lindices)
						inspector.append_line(vertices[edge.i1], vertices[edge.i2], color_rgba(10, 10, 10, 122));
				}
			}
			return true;
		}, &context,
		m_flags.test(ESF_DRAW_CFORM_TRIS) ?
		[](const CDB::MODEL& model, const Fmatrix& transform, CDB::ElementID element, void* data)
		{
			if (!element.IsNotPointer)
				return;
			SCformContext& context = *(SCformContext*)data;
			LevelInspector& inspector = *context.Inspector;
			if ((context.Selected->model == &model && context.Selected->tris_id == element.Index) || element.IsInstance ||
				!LevelInspectorStreamingDetail::HasRoom(inspector))
				return;
			const CDB::TRI& triangle = model.get_tris()[element.Index];
			Fvector vertices[3];
			for (u32 index = 0; index < 3; ++index)
				transform.transform_tiny(vertices[index], model.get_verts()[triangle.verts[index]]);
			inspector.append_tri(vertices[0], vertices[1], vertices[2], color_rgba(100, 100, 100, 45));
			inspector.append_line(vertices[0], vertices[1], color_rgba(20, 20, 20, 255));
			inspector.append_line(vertices[0], vertices[2], color_rgba(20, 20, 20, 255));
			inspector.append_line(vertices[2], vertices[1], color_rgba(20, 20, 20, 255));
		} : nullptr, nullptr);

	if (selected.model != nullptr && selected.tris_id < selected.model->tris.size())
	{
		const CDB::TRI& triangle = selected.model->tris[selected.tris_id];
		Fvector vertices[3];
		for (u32 index = 0; index < 3; ++index)
			selected.ModelWorldTransform.transform_tiny(vertices[index], selected.model->verts[triangle.verts[index]]);
		append_tri(vertices[0], vertices[1], vertices[2], color_rgba(20, 20, 255, 45));
		append_line(vertices[0], vertices[1], color_rgba(10, 10, 10, 255));
		append_line(vertices[0], vertices[2], color_rgba(10, 10, 10, 255));
		append_line(vertices[2], vertices[1], color_rgba(10, 10, 10, 255));
		Fvector normal;
		normal.mknormal(vertices[0], vertices[1], vertices[2]);
		const float inradius = std::min(.5f, std::min(vertices[1].distance_to(vertices[0]), std::min(vertices[2].distance_to(vertices[0]), vertices[2].distance_to(vertices[1]))));
		Fvector arrow;
		arrow.mad(camera, direction, selected.range);
		Fvector end;
		end.mad(arrow, normal, inradius);
		append_line(arrow, end, color_rgba(10, 220, 255, 255));
		append_lines_arrow(arrow, normal, inradius, color_rgba(10, 220, 255, 255));
		if (SGameMtl* material = GMLib.GetMaterialByIdx(triangle.material))
			append_text3d(end, shared_str().printf("%s id[%d] idx[%d] s[%d]", material->m_Name.c_str(), material->ID, triangle.material, triangle.sector));
	}

	UIRender->CacheSetXformWorld(Device.mFullTransform);
	UIRender->SetShader(*shader);
	UIRender->zb_enable(zbuffer_enable);
	UIRender->CacheSetCullMode(m_flags.test(ESF_DRAW_CFORM) ? ERHI_CULLMODE::NONE : ERHI_CULLMODE::BACK);
	TrisRender(tris);
	LinesRender(lines);
	UIRender->CacheSetCullMode(ERHI_CULLMODE::BACK);
}
