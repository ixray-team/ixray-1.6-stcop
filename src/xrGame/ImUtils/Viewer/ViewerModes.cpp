#include "StdAfx.h"
#include "ViewerModes.h"
#include "ViewerSession.h"

#include "../../../xrRHI/RHI.h"
#include "../../../xrEngine/ImGuiManager.h"
#include "../../Actor.h"
#include "../../Level.h"
#include "../../ai_space.h"
#include "../../level_graph.h"
#include "../../ai/stalker/ai_stalker.h"
#include "../../stalker_movement_manager_smart_cover.h"

namespace ImUtils::Viewer
{
	namespace ViewerModesDetail
	{
		struct SViewerPresetEntry
		{
			const char* Name;
			int ModeIndex;
			bool SpawnActor;
			bool ArmActor;
			u32 ResourceMask;
		};

		const u32 SandboxMask = u32(ELevelResource::All) & ~(u32(ELevelResource::Hud) | u32(ELevelResource::LevelScripts));
		const SViewerPresetEntry Presets[] =
		{
			{ "Gameplay + AI", 1, true, true, SandboxMask },
			{ "Render test", 2, true, true, SandboxMask },
			{ "Physics (no AI-map)", 1, true, true,
				u32(ELevelResource::LevelConfig) | u32(ELevelResource::Collision) |
				u32(ELevelResource::PhysicsWorld) | u32(ELevelResource::Spawn) |
				u32(ELevelResource::StaticSounds) | u32(ELevelResource::SoundEnv) | u32(ELevelResource::Ambience) },
		};
	}

	template <typename T>
	xr_unique_ptr<IViewerMode> MakeViewerMode()
	{
		return xr_unique_ptr<IViewerMode>(static_cast<IViewerMode*>(xr_make_unique<T>().release()));
	}

	void CViewerModeGameplay::Tick(CViewerSession& session)
	{
		CViewerEditor::Tick(session);
		if (!WanderNpcs || !session.IsActive())
			return;
		CLevel* level = session.GetLevel();
		if (level == nullptr)
			return;
		const ILevelGraph* graph = ai().get_level_graph();
		if (graph == nullptr || !graph->has_nodes() || Device.dwTimeGlobal < NextWanderTime)
			return;
		NextWanderTime = Device.dwTimeGlobal + 2000;
		const u32 nodeCount = graph->header().vertex_count();
		for (u32 i = 0; i < level->Objects.o_count(); ++i)
		{
			CObject* object = level->Objects.o_get_by_iterator(i);
			CAI_Stalker* stalker = object != nullptr ? object->cast_stalker() : nullptr;
			if (stalker == nullptr || stalker->getDestroy() || !stalker->g_Alive())
				continue;
			const u32 destNode = Random.randI(nodeCount);
			stalker->movement().set_path_type(MovementManager::ePathTypeLevelPath);
			stalker->movement().set_level_dest_vertex(destNode);
		}
	}

	void CViewerModeGameplay::RenderUI(CViewerSession& session)
	{
		if (ImGui::CollapsingHeader("Gameplay", ImGuiTreeNodeFlags_DefaultOpen))
		{
			ImGui::BeginDisabled(!session.IsActive());
			ImGui::InputText("Section", SpawnSection, sizeof(SpawnSection));
			if (ImGui::Button("Spawn at aim point"))
			{
				SViewerRequest request;
				request.Type = EViewerRequest::SpawnSection;
				xr_strcpy(request.Section, SpawnSection);
				session.PushRequest(request);
			}
			ImGui::SameLine();
			const bool spawnActor = ImGui::Button("Spawn actor");
			session.UiProbeObserve("Spawn actor", spawnActor);
			if (spawnActor)
			{
				SViewerRequest request;
				request.Type = EViewerRequest::SpawnActor;
				session.PushRequest(request);
			}
			ImGui::SameLine();
			bool hudEnabled = session.ActivePreset().SpawnActorWithHud;
			if (ImGui::Checkbox("HUD and inventory", &hudEnabled))
			{
				SViewerRequest request;
				request.Type = EViewerRequest::SetHudEnabled;
				request.BoolValue = hudEnabled;
				session.PushRequest(request);
			}
			ImGui::Text("Local actor: %s", session.GetState().HasActor ? "present (game bound)" : "none (free camera)");
			if (ImGui::Checkbox("Wander NPCs (C++ AI-map navigation)", &WanderNpcs))
			{
				SViewerRequest request;
				request.Type = EViewerRequest::SetWander;
				request.BoolValue = WanderNpcs;
				session.PushRequest(request);
			}
			ImGui::TextDisabled("NPCs and monsters without an AI-map remain in a safe degraded navigation state.");
			ImGui::EndDisabled();
		}
		CViewerEditor::RenderUI(session);
	}

	void CViewerModeRenderTest::ApplyToggles(CViewerSession& session) const
	{
		session.ViewerState.RenderPassOverride = true;
	}

	void CViewerModeRenderTest::OnEnter(CViewerSession& session)
	{
		CViewerEditor::OnEnter(session);
		Flags32& flags = session.ViewerState.RenderPassFlags;
		flags.set(rsDrawStatic | rsDrawDynamic | rsDetails | rsOcclusion, true);
		flags.set(rsWireframe, false);
		ApplyToggles(session);
	}

	void CViewerModeRenderTest::OnExit(CViewerSession& session)
	{
		session.ViewerState.RenderPassOverride = false;
		CViewerEditor::OnExit(session);
	}

	void CViewerModeRenderTest::RenderUI(CViewerSession& session)
	{
		if (ImGui::CollapsingHeader("Render passes", ImGuiTreeNodeFlags_DefaultOpen))
		{
			Flags32& flags = session.ViewerState.RenderPassFlags;
			bool changed = false;
			changed |= ImGui::CheckboxFlags("Static geometry", &flags.flags, rsDrawStatic);
			changed |= ImGui::CheckboxFlags("Dynamic geometry", &flags.flags, rsDrawDynamic);
			changed |= ImGui::CheckboxFlags("Grass/details", &flags.flags, rsDetails);
			changed |= ImGui::CheckboxFlags("Occlusion culling", &flags.flags, rsOcclusion);
			changed |= ImGui::CheckboxFlags("Wireframe", &flags.flags, rsWireframe);
			if (changed)
				ApplyToggles(session);
			ImGui::TextUnformatted("These toggles apply only to this session's viewport.");
		}
		CViewerEditor::RenderUI(session);
	}

	xr_unique_ptr<IViewerMode> CreateViewerLegacyMode(int index)
	{
		switch (index)
		{
		case 0:
			return MakeViewerMode<CViewerModeScene>();
		case 2:
			return MakeViewerMode<CViewerModeRenderTest>();
		default:
			return MakeViewerMode<CViewerModeGameplay>();
		}
	}

	int GetViewerModeCount()
	{
		return 3;
	}

	int GetViewerPresetCount()
	{
		return int(std::size(ViewerModesDetail::Presets));
	}

	const char* GetViewerPresetName(int index)
	{
		if (index < 0 || index >= int(std::size(ViewerModesDetail::Presets)))
			return "unknown";
		return ViewerModesDetail::Presets[index].Name;
	}

	void ApplyViewerPreset(int index, SViewerPreset& preset, int& modeIndex)
	{
		if (index < 0 || index >= int(std::size(ViewerModesDetail::Presets)))
			return;
		const ViewerModesDetail::SViewerPresetEntry& entry = ViewerModesDetail::Presets[index];
		preset.Resources.assign(entry.ResourceMask);
		preset.SpawnActor = entry.SpawnActor;
		preset.ArmActor = entry.ArmActor;
		modeIndex = entry.ModeIndex;
	}
}
