#include "StdAfx.h"
#include "ViewerSession.h"
#include "../../Level.h"

int ImUtils::Viewer::CViewerSession::RunResourceMatrixNow()
{
	if (!IsActive() || ViewerState.Primary || GetLevel() == nullptr)
	{
		Msg("! viewer_resource_matrix: a loaded parallel session is required");
		return 2;
	}

	for (u32 bit = 0; bit < IGame_Level::ResourceStateCount; ++bit)
	{
		if (ResourceState(ELevelResource(1u << bit)) == EResourceState::Loading)
			return 0;
	}

	Flags32 original;
	original.assign(GetLevel()->GetLevelDesc().Resources.flags);
	const bool paused = ViewerState.SimulationPaused;
	ViewerState.SimulationPaused = true;
	bool passed = true;
	for (u32 bit = 0; bit < IGame_Level::ResourceStateCount; ++bit)
	{
		const ELevelResource resource = ELevelResource(1u << bit);
		const char* name = LevelResourceName(resource).data();
		SetResourceNow(resource, false);
		if (ResourceState(resource) != EResourceState::NotLoaded)
		{
			Msg("! viewer_resource_matrix: %s did not reach not_loaded after unload", name);
			passed = false;
		}
		SetResourceNow(resource, true);
		const EResourceState state = ResourceState(resource);
		if (state == EResourceState::Failed || state == EResourceState::Loading)
		{
			Msg("! viewer_resource_matrix: %s entered %s after load", name, LevelResourceStateName(state).data());
			passed = false;
		}
		else
		{
			Msg("* viewer_resource_matrix: %s cycle accepted (%s)", name, LevelResourceStateName(state).data());
		}
	}

	for (u32 bit = 0; bit < IGame_Level::ResourceStateCount; ++bit)
	{
		const ELevelResource resource = ELevelResource(1u << bit);
		SetResourceNow(resource, original.test(bit));
		if (ResourceState(resource) == EResourceState::Failed)
			passed = false;
	}
	ViewerState.SimulationPaused = paused;
	Msg("* viewer_resource_matrix: every resource load/unload path -> %s", passed ? "PASS" : "FAIL");
	return passed ? 1 : 2;
}
