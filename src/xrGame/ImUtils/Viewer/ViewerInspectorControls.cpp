#include "StdAfx.h"
#include "ViewerSession.h"
#include "../../LevelInspector.h"

namespace ImUtils::Viewer
{
bool CViewerSession::SetInspectorNow(const char* category, bool enabled)
{
	if (!IsActive() || category == nullptr)
		return false;

	const auto synchronizeInspector = [this]
	{
		if (SessionInspector == nullptr)
			return;
		SessionInspector->m_flags = ViewerState.InspectorSceneFlags;
		SessionInspector->m_zone_flags = ViewerState.InspectorZoneFlags;
		SessionInspector->m_skeleton_flags = ViewerState.InspectorSkeletonFlags;
		SessionInspector->m_selection_flags = ViewerState.InspectorSelectionFlags;
		SessionInspector->m_selection_text_flags = ViewerState.InspectorTextFlags;
		SessionInspector->m_spatials_mask = static_cast<ESPATIAL_TYPE>(ViewerState.InspectorSpatialMask);
		SessionInspector->WaypointsFlags = static_cast<LevelInspector::EWAYPOINT_INFO>(ViewerState.InspectorWaypointMode);
		SessionInspector->wp_prefix = ViewerState.InspectorWaypointPrefix;
	};

	if (0 == _stricmp(category, "all_off"))
	{
		ViewerState.InspectorSceneFlags.zero();
		ViewerState.InspectorZoneFlags.zero();
		ViewerState.InspectorSkeletonFlags.zero();
		ViewerState.InspectorSelectionFlags.zero();
		ViewerState.InspectorTextFlags.zero();
		ViewerState.InspectorSpatialMask = 0;
		synchronizeInspector();
		return true;
	}

	if (0 == _stricmp(category, "ai_map"))
		ViewerState.InspectorSceneFlags.set(LevelInspector::ESF_DRAW_L_GRID, enabled);
	else if (0 == _stricmp(category, "waypoints"))
	{
		ViewerState.InspectorSceneFlags.set(LevelInspector::ESF_DRAW_W_GRID, enabled);
		ViewerState.InspectorWaypointMode = int(LevelInspector::EWAYPOINT_INFO::EWI_ALL);
	}
	else if (0 == _stricmp(category, "game_graph"))
		ViewerState.InspectorSceneFlags.set(LevelInspector::ESF_DRAW_G_GRID, enabled);
	else if (0 == _stricmp(category, "ai_paths"))
		ViewerState.InspectorSceneFlags.set(LevelInspector::ESF_DRAW_AI_PATHS, enabled);
	else if (0 == _stricmp(category, "smart_terrain"))
		ViewerState.InspectorZoneFlags.set(LevelInspector::EZI_SMART_TERRAIN, enabled);
	else if (0 == _stricmp(category, "cform"))
		ViewerState.InspectorSceneFlags.set(LevelInspector::ESF_DRAW_CFORM | LevelInspector::ESF_DRAW_CFORM_TRIS, enabled);
	else if (0 == _stricmp(category, "selection"))
	{
		ViewerState.InspectorSceneFlags.set(LevelInspector::ESF_DRAW_SELECTION, enabled);
		if (enabled)
		{
			ViewerState.InspectorSelectionFlags.assign(LevelInspector::ESLF_O | LevelInspector::ESLF_Z | LevelInspector::ESLF_WP | LevelInspector::ESLF_GP | LevelInspector::ESLF_LG);
			ViewerState.InspectorTextFlags.assign(LevelInspector::EOI_SNAME | LevelInspector::EOI_LNAME | LevelInspector::EOI_POSITION);
		}
		else
		{
			ViewerState.InspectorSelectionFlags.zero();
			ViewerState.InspectorTextFlags.zero();
		}
	}
	else
	{
		Msg("! viewer_inspector: expected ai_map, waypoints, game_graph, ai_paths, smart_terrain, cform, selection, or all_off");
		return false;
	}
	synchronizeInspector();
	Msg("* viewer_inspector: %s %s", category, enabled ? "enabled" : "disabled");
	return true;
}
}
