#include "stdafx.h"
//#include "editor_render.h"
#include "ui_defs.h"
#include "../Include/xrRender/UIShader.h"
#include "../Include/xrRender/UIRender.h" 
#include "level_graph.h"
#include "ai_space.h"

void renderAiMap();

void embedded_editor_render()
{
	renderAiMap();
}

BOOL isRenderAiMap = false;

typedef Fvector2 t_node_tc[4];
static const float dtc = 0.25f;
static t_node_tc node_tc[16] =
{
	{ { 0.f + 0 * dtc,0.25f + 0 * dtc },{ 0.25f + 0 * dtc,0.25f + 0 * dtc },{ 0.25f + 0 * dtc,0.f + 0 * dtc },{ 0.f + 0 * dtc,0.f + 0 * dtc } },
	{ { 0.f + 1 * dtc,0.25f + 0 * dtc },{ 0.25f + 1 * dtc,0.25f + 0 * dtc },{ 0.25f + 1 * dtc,0.f + 0 * dtc },{ 0.f + 1 * dtc,0.f + 0 * dtc } },
	{ { 0.f + 2 * dtc,0.25f + 0 * dtc },{ 0.25f + 2 * dtc,0.25f + 0 * dtc },{ 0.25f + 2 * dtc,0.f + 0 * dtc },{ 0.f + 2 * dtc,0.f + 0 * dtc } },
	{ { 0.f + 3 * dtc,0.25f + 0 * dtc },{ 0.25f + 3 * dtc,0.25f + 0 * dtc },{ 0.25f + 3 * dtc,0.f + 0 * dtc },{ 0.f + 3 * dtc,0.f + 0 * dtc } },

	{ { 0.f + 0 * dtc,0.25f + 1 * dtc },{ 0.25f + 0 * dtc,0.25f + 1 * dtc },{ 0.25f + 0 * dtc,0.f + 1 * dtc },{ 0.f + 0 * dtc,0.f + 1 * dtc } },
	{ { 0.f + 1 * dtc,0.25f + 1 * dtc },{ 0.25f + 1 * dtc,0.25f + 1 * dtc },{ 0.25f + 1 * dtc,0.f + 1 * dtc },{ 0.f + 1 * dtc,0.f + 1 * dtc } },
	{ { 0.f + 2 * dtc,0.25f + 1 * dtc },{ 0.25f + 2 * dtc,0.25f + 1 * dtc },{ 0.25f + 2 * dtc,0.f + 1 * dtc },{ 0.f + 2 * dtc,0.f + 1 * dtc } },
	{ { 0.f + 3 * dtc,0.25f + 1 * dtc },{ 0.25f + 3 * dtc,0.25f + 1 * dtc },{ 0.25f + 3 * dtc,0.f + 1 * dtc },{ 0.f + 3 * dtc,0.f + 1 * dtc } },

	{ { 0.f + 0 * dtc,0.25f + 2 * dtc },{ 0.25f + 0 * dtc,0.25f + 2 * dtc },{ 0.25f + 0 * dtc,0.f + 2 * dtc },{ 0.f + 0 * dtc,0.f + 2 * dtc } },
	{ { 0.f + 1 * dtc,0.25f + 2 * dtc },{ 0.25f + 1 * dtc,0.25f + 2 * dtc },{ 0.25f + 1 * dtc,0.f + 2 * dtc },{ 0.f + 1 * dtc,0.f + 2 * dtc } },
	{ { 0.f + 2 * dtc,0.25f + 2 * dtc },{ 0.25f + 2 * dtc,0.25f + 2 * dtc },{ 0.25f + 2 * dtc,0.f + 2 * dtc },{ 0.f + 2 * dtc,0.f + 2 * dtc } },
	{ { 0.f + 3 * dtc,0.25f + 2 * dtc },{ 0.25f + 3 * dtc,0.25f + 2 * dtc },{ 0.25f + 3 * dtc,0.f + 2 * dtc },{ 0.f + 3 * dtc,0.f + 2 * dtc } },

	{ { 0.f + 0 * dtc,0.25f + 3 * dtc },{ 0.25f + 0 * dtc,0.25f + 3 * dtc },{ 0.25f + 0 * dtc,0.f + 3 * dtc },{ 0.f + 0 * dtc,0.f + 3 * dtc } },
	{ { 0.f + 1 * dtc,0.25f + 3 * dtc },{ 0.25f + 1 * dtc,0.25f + 3 * dtc },{ 0.25f + 1 * dtc,0.f + 3 * dtc },{ 0.f + 1 * dtc,0.f + 3 * dtc } },
	{ { 0.f + 2 * dtc,0.25f + 3 * dtc },{ 0.25f + 2 * dtc,0.25f + 3 * dtc },{ 0.25f + 2 * dtc,0.f + 3 * dtc },{ 0.f + 2 * dtc,0.f + 3 * dtc } },
	{ { 0.f + 3 * dtc,0.25f + 3 * dtc },{ 0.25f + 3 * dtc,0.25f + 3 * dtc },{ 0.25f + 3 * dtc,0.f + 3 * dtc },{ 0.f + 3 * dtc,0.f + 3 * dtc } },
};
static ui_shader sh_Tracer;

void renderAiMap()
{
	if (!isRenderAiMap)
		return;

	if (!sh_Tracer->inited())
		sh_Tracer->create("editor\\ai_node", "ed\\ed_ai_arrows_01");

	Fvector min_position, max_position;
	max_position = min_position = Device.vCameraPosition;
	min_position.sub(30.f);
	max_position.add(30.f);

	const auto& graph = ai().level_graph();
	CLevelGraph::const_vertex_iterator	 I, E;
	if (graph.valid_vertex_position(min_position))
		I = std::lower_bound(graph.begin(), graph.end(), graph.vertex_position(min_position).xz(), [](const auto& vertex, auto value) {return vertex.position().xz() < value; });
	else
		I = graph.begin();

	if (graph.valid_vertex_position(max_position)) {
		E = std::upper_bound(graph.begin(), graph.end(), graph.vertex_position(max_position).xz(), [](auto value, const auto& vertex) { return value < vertex.position().xz(); });
		if (E != graph.end()) ++E;
	}
	else
		E = graph.end();

	float	st = 0.98f * graph.header().cell_size() / 2;
	float	tt = 0.01f;
	Fvector	DUP;		DUP.set(0, 1, 0);

	u32 DvbSize = 1536 * 1024;// ðàçìåð áóôåðà rsDVB_Size from R_DStreams.cpp:12
	u32 PointSize = 24;// ðàçìåð äàííûõ ïîä îäíó òî÷êó hGeom_LIT.stride() from dxUIRender.cpp
	u32 PointCount = 6;// íà îäíó àè-íîäó òðåáóåòñÿ 2 òðåóãîëüíèêà = 6 òî÷åê
	u32 nodeBatch = DvbSize / PointSize / PointCount;
	UIRender->StartPrimitive(nodeBatch * PointCount, IUIRender::ptTriList, IUIRender::pttLIT);

	u32 iNode = 0;
	for (; I != E; ++I)
	{
		const CLevelGraph::CVertex& N = *I;
		Fvector			PC;
		PC = graph.vertex_position(N);

		if (Device.vCameraPosition.distance_to_sqr(PC) > 900.0f) continue;

		float			sr = graph.header().cell_size();
		if (!::Render->ViewBase.testSphere_dirty(PC, sr))
			continue;

		// unpack plane
		Fplane PL; Fvector vNorm;
		pvDecompress(vNorm, N.plane());
		PL.build(PC, vNorm);

		int k = 0;
		if (graph.valid_vertex_id(N.link(0))) k |= 1 << 0;
		if (graph.valid_vertex_id(N.link(1))) k |= 1 << 1;
		if (graph.valid_vertex_id(N.link(2))) k |= 1 << 2;
		if (graph.valid_vertex_id(N.link(3))) k |= 1 << 3;
		// create vertices
		Fvector		v, v1, v2, v3, v4;
		v.set(PC.x - st, PC.y, PC.z - st);	PL.intersectRayPoint(v, DUP, v1);	v1.mad(v1, PL.n, tt);	// minX,minZ
		v.set(PC.x + st, PC.y, PC.z - st);	PL.intersectRayPoint(v, DUP, v2);	v2.mad(v2, PL.n, tt);	// maxX,minZ
		v.set(PC.x + st, PC.y, PC.z + st);	PL.intersectRayPoint(v, DUP, v3);	v3.mad(v3, PL.n, tt);	// maxX,maxZ
		v.set(PC.x - st, PC.y, PC.z + st);	PL.intersectRayPoint(v, DUP, v4);	v4.mad(v4, PL.n, tt);	// minX,maxZ

		UIRender->PushPoint(v3.x, v3.y, v3.z, 0xffffffff, node_tc[k][2].x, node_tc[k][2].y);
		UIRender->PushPoint(v2.x, v2.y, v2.z, 0xffffffff, node_tc[k][1].x, node_tc[k][1].y);
		UIRender->PushPoint(v1.x, v1.y, v1.z, 0xffffffff, node_tc[k][0].x, node_tc[k][0].y);
		UIRender->PushPoint(v1.x, v1.y, v1.z, 0xffffffff, node_tc[k][0].x, node_tc[k][0].y);
		UIRender->PushPoint(v4.x, v4.y, v4.z, 0xffffffff, node_tc[k][3].x, node_tc[k][3].y);
		UIRender->PushPoint(v3.x, v3.y, v3.z, 0xffffffff, node_tc[k][2].x, node_tc[k][2].y);

		iNode++;
		if (iNode == nodeBatch) {
			UIRender->CacheSetXformWorld(Fidentity);
			UIRender->SetShader(*sh_Tracer);
			UIRender->FlushPrimitive();

			UIRender->StartPrimitive(nodeBatch, IUIRender::ptTriList, IUIRender::pttLIT);
		}
	}
	UIRender->CacheSetXformWorld(Fidentity);
	UIRender->SetShader(*sh_Tracer);
	UIRender->FlushPrimitive();
}