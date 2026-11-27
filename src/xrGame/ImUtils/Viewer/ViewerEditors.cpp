#include "StdAfx.h"
#include "ViewerEditors.h"
#include "ViewerSession.h"

#include "../../Level.h"
#include "../../xrServer.h"
#include "../../GamePersistent.h"
#include "../../../xrServerEntities/xrServer_Objects_ALife.h"
#include "../../../xrServerEntities/clsid_game.h"
#include "../../../xrEngine/xr_input.h"
#include "../../../xrEngine/Fmesh.h"
#include "../../../xrEngine/ImGuiManager.h"
#include "../../ai_space.h"
#include "../../level_graph.h"
#include "../../ai/stalker/ai_stalker.h"
#include "../../stalker_movement_manager_smart_cover.h"
#include "../../ai_monster_space.h"
#include "../../detail_path_manager.h"

namespace ImUtils::Viewer
{
	namespace ViewerEditorsDetail
	{
		template <typename T>
		xr_unique_ptr<IViewerMode> MakeViewerMode()
		{
			return xr_unique_ptr<IViewerMode>(static_cast<IViewerMode*>(xr_make_unique<T>().release()));
		}
	}

	bool CViewerPreviewObject::Spawn(CSE_ALifeDynamicObjectVisual& description, CLevel& level)
	{
		CLS_ID = CLSID_SCRIPT_OBJECT;
		Registered = level.Objects.RegisterExternal(*this);
		if (!Registered)
			return false;
		if (!net_Spawn(&description))
		{
			setDestroy(true);
			net_Destroy();
			return false;
		}
		Spawned = true;
		setVisible(true);
		return true;
	}

	void CViewerPreviewObject::Release()
	{
		if (!Spawned)
			return;
		setDestroy(true);
		net_Destroy();
		Spawned = false;
	}

	CGameObject* CViewerOgfEditor::PreviewObject(CViewerSession& session) const
	{
		CObject* object = session.GetLevel() != nullptr ? session.GetLevel()->Objects.net_Find(PreviewId) : nullptr;
		return object != nullptr ? object->cast_game_object() : nullptr;
	}

	void CViewerOgfEditor::Configure(SViewerPreset& preset)
	{
		CViewerEditor::Configure(preset);
		preset.Terrain.Kind = Sandbox::ETerrainKind::Flat;
		preset.Resources.zero();
		preset.Resources.set(u32(ELevelResource::LevelConfig) | u32(ELevelResource::Collision) | u32(ELevelResource::PhysicsWorld) | u32(ELevelResource::SoundEnv), true);
	}

	void CViewerOgfEditor::OnSessionReady(CViewerSession& session)
	{
		CenterCamera(session, Fvector().set(0.f, 1.f, 0.f), 2.5f);
		if (!RestoreLatest(session))
			LoadPreview(session, PreviewPath);
	}

	void CViewerOgfEditor::OnExit(CViewerSession& session)
	{
		ReleasePreview(session);
		CViewerEditor::OnExit(session);
	}

	void CViewerOgfEditor::ReleasePreview(CViewerSession& session)
	{
		CLevel* level = session.GetLevel();
		if (Preview != nullptr && level != nullptr && level->Objects.net_Find(PreviewId) == Preview)
		{
			Preview->Release();
			if (level->Server != nullptr)
				level->Server->FreeID(PreviewId, 0);
		}
		if (Preview != nullptr)
		{
			if (Preview->IsRegistered() && level != nullptr)
				level->Objects.Destroy(Preview);
			else
				xr_delete(Preview);
			Preview = nullptr;
		}
		PreviewId = ALife::INVALID_OBJECT_ID;
	}

	bool CViewerOgfEditor::ReplacePreview(CViewerSession& session, const char* path, bool duplicateVisual)
	{
		if (!session.IsActive() || session.SceneRenderer() == nullptr || path == nullptr || path[0] == 0)
			return false;
		CLevel* level = session.GetLevel();
		if (level->Server == nullptr)
			return false;
		string_path modelPath;
		xr_strcpy(modelPath, path);
		if (strext(modelPath) == nullptr)
			xr_strcat(modelPath, ".ogf");

		IReader* reader = FS.r_open(modelPath);
		if (reader == nullptr)
		{
			string_path resolved;
			if (FS.exist(resolved, _game_meshes_, modelPath))
				reader = FS.r_open(resolved);
		}
		if (reader == nullptr)
		{
			Msg("! %s: model '%s' is missing", Name(), modelPath);
			return false;
		}
		ogf_header header = {};
		const bool valid = reader->r_chunk_safe(OGF_HEADER, &header, sizeof(header)) && header.format_version == xrOGF_FormatVersion;
		if (reader->find_chunk(OGF_TEXTURE))
		{
			reader->r_stringZ(OgfTextureName, sizeof(OgfTextureName));
			reader->r_stringZ(OgfShaderName, sizeof(OgfShaderName));
		}
		else
		{
			OgfTextureName[0] = 0;
			OgfShaderName[0] = 0;
		}
		FS.r_close(reader);
		if (!valid)
		{
			Msg("! %s: model '%s' has an invalid OGF header", Name(), modelPath);
			return false;
		}

		ReleasePreview(session);
		CSE_Abstract* entity = F_entity_Create("script_object");
		CSE_ALifeDynamicObjectVisual* description = entity != nullptr ? dynamic_cast<CSE_ALifeDynamicObjectVisual*>(entity) : nullptr;
		if (description == nullptr)
		{
			if (entity != nullptr)
				F_entity_Destroy(entity);
			Msg("! %s: the stock visual entity type is unavailable", Name());
			return false;
		}
		description->ID = level->Server->PerformIDgen(ALife::INVALID_OBJECT_ID);
		description->o_Position.set(0.f, 1.f, 0.f);
		description->s_flags.set(M_SPAWN_OBJECT_LOCAL, true);
		description->set_name_replace("");
		description->m_ini_string = "";
		description->m_wVersion = SPAWN_VERSION;
		description->visual_name = modelPath;
		Preview = new CViewerPreviewObject();
		if (!Preview->Spawn(*description, *level))
		{
			Msg("! %s: cannot spawn model '%s'", Name(), modelPath);
			ReleasePreview(session);
			level->Server->FreeID(description->ID, 0);
			F_entity_Destroy(entity);
			return false;
		}
		PreviewId = description->ID;
		if (duplicateVisual)
		{
			IRenderVisual* original = Preview->Visual();
			IRenderVisual* duplicate = original != nullptr ? session.SceneRenderer()->model_Duplicate(original) : nullptr;
			if (duplicate == nullptr)
			{
				Msg("! %s: cannot duplicate the model's visual", Name());
				ReleasePreview(session);
				F_entity_Destroy(entity);
				return false;
			}
			session.SceneRenderer()->model_Delete(original);
			Preview->renderable.visual = duplicate;
			Preview->OnChangeVisual();
		}
		F_entity_Destroy(entity);
		xr_strcpy(PreviewPath, modelPath);
		Preview->spatial_move();
		Msg("* %s: loaded '%s' (id %u)%s", Name(), PreviewPath, PreviewId, duplicateVisual ? ", private visual" : "");
		return true;
	}

	bool CViewerOgfEditor::LoadPreview(CViewerSession& session, const char* path)
	{
		if (!ReplacePreview(session, path, dynamic_cast<CViewerMaterialEditor*>(this) != nullptr))
			return false;
		return CommitHistory("load model");
	}

	void CViewerOgfEditor::TickEditor(CViewerSession& session)
	{
		CGameObject* object = PreviewObject(session);
		if (object == nullptr)
		{
			Preview = nullptr;
			PreviewId = ALife::INVALID_OBJECT_ID;
			return;
		}
		Preview = static_cast<CViewerPreviewObject*>(object);
		if (RotationChanged && !ImGui::IsMouseDown(ImGuiMouseButton_Right))
			CommitHistory("rotate model");
		RotationChanged = false;
		if (ImGui::IsMouseDown(ImGuiMouseButton_Right) && GImGUIManager->IsCapturingInputs())
		{
			Ivector2 delta = {};
			pInput->iGetLastMouseDelta(delta);
			if (delta.x != 0 || delta.y != 0)
			{
				RotationYaw += delta.x * 0.005f;
				RotationPitch = clampr(RotationPitch + delta.y * 0.005f, -PI_DIV_2, PI_DIV_2);
				RotationChanged = true;
			}
		}
		Fmatrix transform = object->XFORM();
		transform.setXYZ(RotationPitch, RotationYaw, 0.f);
		object->XFORM() = transform;
		object->spatial_move();
	}

	bool CViewerOgfEditor::VerifyHistoryTestState(CViewerSession& session, u32 index)
	{
		return PreviewId != ALife::INVALID_OBJECT_ID && PreviewObject(session) != nullptr && fsimilar(RotationYaw, float(index));
	}

	bool CViewerOgfEditor::EditorRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value)
	{
		if (command == EViewerEditorCommand::Load)
			return LoadPreview(session, argument != nullptr && argument[0] != 0 ? argument : PreviewPath);
		if (command == EViewerEditorCommand::Rotate)
		{
			if (argument != nullptr && 0 == _stricmp(argument, "pitch"))
				RotationPitch = clampr(RotationPitch + deg2rad(value), -PI_DIV_2, PI_DIV_2);
			else
				RotationYaw += deg2rad(value);
			return CommitHistory("rotate model");
		}
		if (command == EViewerEditorCommand::SetParameter)
		{
			if (argument != nullptr && 0 == _stricmp(argument, "pitch"))
				RotationPitch = clampr(deg2rad(value), -PI_DIV_2, PI_DIV_2);
			else
				RotationYaw = deg2rad(value);
			return CommitHistory("set rotation");
		}
		if (command == EViewerEditorCommand::Reset)
		{
			RotationYaw = -PI_DIV_2;
			RotationPitch = 0.f;
			return CommitHistory("reset rotation");
		}
		return false;
	}

	bool CViewerOgfEditor::WriteState(IWriter& state)
	{
		state.w_stringZ(PreviewPath);
		state.w_float(RotationYaw);
		state.w_float(RotationPitch);
		return true;
	}

	bool CViewerOgfEditor::ReadState(IReader& state)
	{
		state.r_stringZ(PreviewPath, sizeof(PreviewPath));
		RotationYaw = state.r_float();
		RotationPitch = state.r_float();
		return _valid(RotationYaw) && _valid(RotationPitch);
	}

	void CViewerOgfEditor::Restored(CViewerSession& session)
	{
		ReplacePreview(session, PreviewPath, dynamic_cast<CViewerMaterialEditor*>(this) != nullptr);
	}

	void CViewerOgfEditor::DrawEditor(CViewerSession& session)
	{
		if (!ImGui::CollapsingHeader("Model", ImGuiTreeNodeFlags_DefaultOpen))
			return;
		ImGui::InputText("OGF path", PreviewPath, sizeof(PreviewPath));
		if (ImGui::Button("Load model"))
			session.RequestEditorCommand( EViewerEditorCommand::Load, PreviewPath, 0.f);
		ImGui::SameLine();
		const bool resetRotation = ImGui::Button("Reset rotation");
		session.UiProbeObserve("Reset rotation", resetRotation);
		if (resetRotation)
			session.RequestEditorCommand(EViewerEditorCommand::Reset, "rotation", 0.f);
		float yawDegrees = rad2deg(RotationYaw);
		if (ImGui::DragFloat("Yaw (degrees)", &yawDegrees, 0.25f))
			session.RequestEditorCommand(EViewerEditorCommand::SetParameter, "yaw", yawDegrees);
		float pitchDegrees = rad2deg(RotationPitch);
		if (ImGui::DragFloat("Pitch (degrees)", &pitchDegrees, 0.25f, -90.f, 90.f))
			session.RequestEditorCommand(EViewerEditorCommand::SetParameter, "pitch", pitchDegrees);
		ImGui::TextDisabled("Hold RMB and move the mouse to rotate the real game visual. Rotation history is recorded on release.");
		ImGui::Text("Preview object: %u", PreviewId);
	}

	void CViewerMaterialEditor::Configure(SViewerPreset& preset)
	{
		CViewerOgfEditor::Configure(preset);
	}

	void CViewerMaterialEditor::OnSessionReady(CViewerSession& session)
	{
		CViewerOgfEditor::OnSessionReady(session);
		if (ApplyMaterial(session))
			CommitHistory("apply material");
	}

	bool CViewerMaterialEditor::ApplyMaterial(CViewerSession& session)
	{
		CGameObject* object = PreviewObject(session);
		IRenderVisual* visual = object != nullptr ? object->Visual() : nullptr;
		if (visual == nullptr)
			return false;
		if (TexturePath[0] == 0 && OgfTextureName[0] != 0)
			xr_strcpy(TexturePath, OgfTextureName);
		if (TexturePath[0] == 0)
			return false;
		if (ShaderPath[0] == 0)
		{
			shared_str shader = visual->getCompiledShaderName();
			if (shader.size() == 0 && OgfShaderName[0] != 0)
				shader = OgfShaderName;
			if (shader.size() == 0)
				shader = visual->getShaderName();
			if (shader.size() == 0)
				shader = visual->dcast_PKinematics() != nullptr ? shared_str("deffer_model") : shared_str("def_shaders\\def_vertex");
			if (shader.size() == 0)
			{
				Msg("! %s: the model has no compatible shader", Name());
				return false;
			}
			xr_strcpy(ShaderPath, shader.c_str());
		}
		string_path texturePath;
		if (!FS.exist(texturePath, _game_textures_, TexturePath, ".dds"))
		{
			Msg("! %s: texture '%s' is missing", Name(), TexturePath);
			return false;
		}
		visual->set_shader(ShaderPath);
		visual->set_texture(TexturePath);
		visual->reload_shader();
		Msg("* %s: private visual shader '%s', texture '%s'", Name(), ShaderPath, TexturePath);
		return true;
	}

	bool CViewerMaterialEditor::EditorRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value)
	{
		if (command == EViewerEditorCommand::SetParameter &&
			(argument == nullptr || (0 != _stricmp(argument, "yaw") && 0 != _stricmp(argument, "pitch"))))
		{
			if (!ApplyMaterial(session))
				return false;
			return CommitHistory("apply material");
		}
		if (command == EViewerEditorCommand::Reset && (argument == nullptr || 0 != _stricmp(argument, "rotation")))
		{
			CGameObject* object = PreviewObject(session);
			IRenderVisual* visual = object != nullptr ? object->Visual() : nullptr;
			if (visual == nullptr)
				return false;
			visual->restore_shader();
			visual->restore_texture();
			return CommitHistory("restore material");
		}
		return CViewerOgfEditor::EditorRequest(session, command, argument, value);
	}

	bool CViewerMaterialEditor::WriteState(IWriter& state)
	{
		if (!CViewerOgfEditor::WriteState(state))
			return false;
		state.w_stringZ(ShaderPath);
		state.w_stringZ(TexturePath);
		return true;
	}

	bool CViewerMaterialEditor::ReadState(IReader& state)
	{
		if (!CViewerOgfEditor::ReadState(state))
			return false;
		state.r_stringZ(ShaderPath, sizeof(ShaderPath));
		state.r_stringZ(TexturePath, sizeof(TexturePath));
		return true;
	}

	void CViewerMaterialEditor::Restored(CViewerSession& session)
	{
		CViewerOgfEditor::Restored(session);
		ApplyMaterial(session);
	}

	void CViewerMaterialEditor::DrawEditor(CViewerSession& session)
	{
		CViewerOgfEditor::DrawEditor(session);
		if (!ImGui::CollapsingHeader("Material", ImGuiTreeNodeFlags_DefaultOpen))
			return;
		ImGui::InputText("Shader", ShaderPath, sizeof(ShaderPath));
		ImGui::InputText("Texture", TexturePath, sizeof(TexturePath));
		if (ImGui::Button("Apply to private visual"))
			session.RequestEditorCommand( EViewerEditorCommand::SetParameter, "apply", 0.f);
		const bool restore = ImGui::Button("Restore original material");
		session.UiProbeObserve("Restore original material", restore);
		if (restore)
			session.RequestEditorCommand(EViewerEditorCommand::Reset, nullptr, 0.f);
	}

	void CViewerAiEditor::Configure(SViewerPreset& preset)
	{
		CViewerEditor::Configure(preset);
		preset.Terrain.Kind = Sandbox::ETerrainKind::Flat;
		preset.Resources.set(u32(ELevelResource::AiMap) | u32(ELevelResource::PatrolPaths), true);
	}

	void CViewerAiEditor::OnSessionReady(CViewerSession& session)
	{
		CenterCamera(session, Fvector().set(0.f, 0.f, 0.f), 24.f, -0.8f);
		session.ViewerState.InspectorSceneFlags.set(LevelInspector::ESF_DRAW_L_GRID | LevelInspector::ESF_DRAW_W_GRID | LevelInspector::ESF_DRAW_AI_PATHS, true);
		session.ViewerState.InspectorWaypointMode = int(LevelInspector::EWAYPOINT_INFO::EWI_ALL);
	}

	void CViewerAiEditor::TickEditor(CViewerSession& session)
	{
		TestActorId = ALife::_OBJECT_ID(session.GetState().LastSpawnedId);
		const auto startProbe = [this](CAI_Stalker& stalker, u32 destination)
		{
			Wander = false;
			stalker.movement().set_path_type(MovementManager::ePathTypeNoPath);
			stalker.movement().set_path_type(MovementManager::ePathTypeLevelPath);
			stalker.movement().set_movement_type(MonsterSpace::eMovementTypeWalk);
			stalker.movement().set_level_dest_vertex(destination);
			const Fvector start = stalker.Position();
			Probe = {};
			Probe.ObjectId = TestActorId;
			Probe.StartPosition = start;
			Probe.Destination = destination;
			Probe.RemainingTicks = 240;
		};
		if (Probe.PendingTicks != 0)
		{
			CObject* object = session.GetLevel()->Objects.net_Find(TestActorId);
			CAI_Stalker* stalker = object != nullptr ? object->cast_stalker() : nullptr;
			if (stalker != nullptr)
				startProbe(*stalker, Probe.PendingDestination);
			else if (--Probe.PendingTicks == 0)
			{
				Probe.Completed = true;
				Probe.Passed = false;
				Msg("! AI Editor: timed out waiting for the spawned stalker -> FAIL");
			}
		}
		if (Probe.RemainingTicks != 0 && session.IsActive())
		{
			CObject* object = session.GetLevel()->Objects.net_Find(Probe.ObjectId);
			const CAI_Stalker* stalker = object != nullptr ? object->cast_stalker() : nullptr;
			if (stalker == nullptr)
			{
				Probe.RemainingTicks = 0;
				Probe.Completed = true;
				Probe.Passed = false;
				Msg("! AI Editor: navigation probe lost the stalker -> FAIL");
			}
			else if (--Probe.RemainingTicks == 0)
			{
				const ILevelGraph* graph = ai().get_level_graph();
				Probe.Distance = Probe.StartPosition.distance_to(stalker->Position());
				Probe.ClosestVertex = graph != nullptr && graph->has_nodes() ? graph->vertex_id(stalker->Position()) : u32(-1);
				const bool destinationBound = stalker->movement().level_dest_vertex_id() == Probe.Destination;
				Probe.Passed = Probe.Distance > 0.25f && graph != nullptr && graph->valid_vertex_id(Probe.ClosestVertex) && destinationBound;
				Probe.Completed = true;
				Msg("* AI Editor: navigation probe id=%u destination=%u closest=%u bound=%d distance=%.3f path=%u actual=%d path-type=%d movement-type=%d -> %s",
					Probe.ObjectId, Probe.Destination, Probe.ClosestVertex, destinationBound, Probe.Distance,
					u32(stalker->movement().path().size()), stalker->movement().actual(), int(stalker->movement().path_type()), int(stalker->movement().movement_type()), Probe.Passed ? "PASS" : "FAIL");
			}
		}
		if (!Wander || !session.IsActive() || session.GetState().SimulationTicks % 120 != 0)
			return;
		const ILevelGraph* graph = ai().get_level_graph();
		if (graph == nullptr || !graph->has_nodes())
			return;
		CLevel* level = session.GetLevel();
		for (u32 index = 0; index < level->Objects.o_count(); ++index)
		{
			CAI_Stalker* stalker = level->Objects.o_get_by_iterator(index)->cast_stalker();
			if (stalker == nullptr || stalker->getDestroy() || !stalker->g_Alive())
				continue;
			stalker->movement().set_path_type(MovementManager::ePathTypeNoPath);
			stalker->movement().set_path_type(MovementManager::ePathTypeLevelPath);
			stalker->movement().set_movement_type(MonsterSpace::eMovementTypeWalk);
			stalker->movement().set_level_dest_vertex(Random.randI(graph->header().vertex_count()));
		}
	}

	bool CViewerAiEditor::EditorRequest(CViewerSession& session, EViewerEditorCommand command, const char* argument, float value)
	{
		if (command == EViewerEditorCommand::Play)
		{
			Wander = true;
			Probe.RemainingTicks = 0;
			Probe.PendingTicks = 0;
			return true;
		}
		if (command == EViewerEditorCommand::Stop)
		{
			Wander = false;
			return true;
		}
		if (command == EViewerEditorCommand::SetParameter)
		{
			const ILevelGraph* graph = ai().get_level_graph();
			if (graph == nullptr || !graph->has_nodes() || value < 0.f || value >= graph->header().vertex_count())
			{
				Msg("! AI Editor: destination is unavailable without a valid AI-map");
				return false;
			}
			Destination = u32(value);
			CObject* object = session.GetLevel()->Objects.net_Find(TestActorId);
			CAI_Stalker* stalker = object != nullptr ? object->cast_stalker() : nullptr;
			if (stalker == nullptr)
			{
				Probe = {};
				Probe.Destination = Destination;
				Probe.PendingDestination = Destination;
				Probe.PendingTicks = 600;
				return true;
			}
			Wander = false;
			stalker->movement().set_path_type(MovementManager::ePathTypeNoPath);
			stalker->movement().set_path_type(MovementManager::ePathTypeLevelPath);
			stalker->movement().set_movement_type(MonsterSpace::eMovementTypeWalk);
			stalker->movement().set_level_dest_vertex(Destination);
			Probe = {};
			Probe.ObjectId = TestActorId;
			Probe.StartPosition = stalker->Position();
			Probe.Destination = Destination;
			Probe.RemainingTicks = 240;
			return true;
		}
		return false;
	}

	void CViewerAiEditor::DrawEditor(CViewerSession& session)
	{
		if (!ImGui::CollapsingHeader("AI test", ImGuiTreeNodeFlags_DefaultOpen))
			return;
		ImGui::Text("AI-map: %s", session.GetState().AiMapPresent ? "available" : "not loaded");
		const bool spawnStalker = ImGui::Button("Spawn stalker");
		session.UiProbeObserve("Spawn stalker", spawnStalker);
		if (spawnStalker)
		{
			SViewerRequest request;
			request.Type = EViewerRequest::SpawnSection;
			xr_strcpy(request.Section, "stalker");
			session.PushRequest(request);
		}
		ImGui::SameLine();
		if (ImGui::Button("Spawn dog"))
		{
			SViewerRequest request;
			request.Type = EViewerRequest::SpawnSection;
			xr_strcpy(request.Section, "m_dog_e");
			session.PushRequest(request);
		}
		bool wander = Wander;
		if (ImGui::Checkbox("Wander using native movement requests", &wander))
			session.RequestEditorCommand(wander ? EViewerEditorCommand::Play : EViewerEditorCommand::Stop);
		ImGui::InputInt("Destination node", reinterpret_cast<int*>(&Destination));
		if (ImGui::Button("Move last spawned stalker"))
			session.RequestEditorCommand( EViewerEditorCommand::SetParameter, "destination", float(Destination));
		TestActorId = ALife::_OBJECT_ID(session.GetState().LastSpawnedId);
		if (Probe.RemainingTicks != 0)
			ImGui::Text("Navigation probe: %u ticks remaining", Probe.RemainingTicks);
		else if (Probe.Completed)
			ImGui::Text("Navigation: %s; %.2f m; vertex %u", Probe.Passed ? "PASS" : "FAIL", Probe.Distance, Probe.ClosestVertex);
		ImGui::TextDisabled("The stock Level Inspector renders the session-local AI-map and patrol paths.");
	}

	xr_unique_ptr<IViewerMode> CreateViewerEditor(EViewerEditorKind kind)
	{
		switch (kind)
		{
		case EViewerEditorKind::OgfPreview:
			return ViewerEditorsDetail::MakeViewerMode<CViewerOgfEditor>();
		case EViewerEditorKind::ParticleEditor:
			return ViewerEditorsDetail::MakeViewerMode<CViewerParticleEditor>();
		case EViewerEditorKind::AiEditor:
			return ViewerEditorsDetail::MakeViewerMode<CViewerAiEditor>();
		case EViewerEditorKind::MaterialEditor:
			return ViewerEditorsDetail::MakeViewerMode<CViewerMaterialEditor>();
		default:
			return CreateViewerLegacyMode(1);
		}
	}
}
