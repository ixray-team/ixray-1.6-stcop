#include "stdafx.h"
#include "UI_LevelMacroCommands.h"
#include <filesystem>
#include <fstream>
#include <sstream>
#include <SDL3/SDL_process.h>
#include "../xrServerEntities/xrServer_Objects_ALife.h"

namespace EditorPlayCommands
{
	template<bool (EScene::*Action)()>
	CCommandVar CommandBuildPieStage(CCommandVar, CCommandVar)
	{
		if (!Scene->valid() || Scene->locked() || Scene->IsPlayInEditor() || LUI->LoaderEvent.IsBusy())
		{
			Msg("! Play in Editor build requires an idle scene");
			return 0u;
		}

		return u32((Scene->*Action)());
	}

	CCommandVar CommandExportLevelBuild(CCommandVar, CCommandVar)
	{
		LUI->LoaderEvent.Wait();
		if (!Scene->valid() || Scene->locked() || Scene->IsPlayInEditor())
		{
			return 0u;
		}

		const auto& LevelPath = Scene->m_LevelOp.m_FNLevelPath;
		if (!LevelPath.size() || !std::ranges::all_of(xr_string_view(LevelPath.c_str()), [](char Character)
		{
			return (Character >= 'a' && Character <= 'z') || (Character >= 'A' && Character <= 'Z') ||
				(Character >= '0' && Character <= '9') || Character == '_';
		}))
		{
			Msg("! Export level build requires a simple level directory name");
			return 0u;
		}

		string_path Output;
		FS.update_path(Output, "$game_levels$", LevelPath.c_str());
		std::error_code Error;
		if (std::filesystem::exists(Output, Error) || Error)
		{
			Msg("! Export level build requires an unused output directory: %s", Output);
			return 0u;
		}

		return u32(Builder.Compile(false, false) && !EContext.UI->NeedAbort());
	}

	CCommandVar CommandPlayInEditor(CCommandVar, CCommandVar)
	{
		if (!Scene->valid() || Scene->locked() || Scene->IsPlayInEditor() || LUI->LoaderEvent.IsBusy())
		{
			Msg("! Play in Editor requires an idle scene");
			return xr_string("Play in Editor requires an idle scene.");
		}

		if (const char* Error = Scene->Play())
		{
			ELog.Msg(mtError, "%s", Error);
			return xr_string(Error);
		}
		return 1u;
	}
}

namespace EditorAICommands
{
	thread_local nlohmann::json* g_AIStageReport = nullptr;

	void ReportAIStageError(const char* Code)
	{
		if (g_AIStageReport != nullptr)
		{
			(*g_AIStageReport)["compiler_error_code"] = Code;
		}
	}

	void ReportAIStageOutput(const std::string& Text, const std::filesystem::path& SpawnOutput, bool NewSpawnOutput, int ExitCode)
	{
		if (g_AIStageReport == nullptr)
		{
			return;
		}

		auto& Report = *g_AIStageReport;
		Report["compiler_exit_code"] = ExitCode;
		auto& Artifacts = Report["artifacts"];
		std::istringstream Lines(Text);
		std::string Line;
		while (std::getline(Lines, Line))
		{
			if (!Line.starts_with('{'))
			{
				continue;
			}
			const auto Data = nlohmann::json::parse(Line, nullptr, false);
			if (!Data.is_object())
			{
				continue;
			}
			if (ExitCode != 0 && Data.contains("code") && Data["code"].is_string())
			{
				Report["compiler_error_code"] = Data["code"];
			}
			if (ExitCode == 0 && Data.value("status", "") == "built" && Data.contains("artifact") && Data["artifact"].is_string())
			{
				std::error_code Error;
				const auto& Artifact = Data["artifact"].get_ref<const std::string&>();
				if (std::filesystem::is_regular_file(Artifact, Error))
				{
					Artifacts.push_back(Artifact);
				}
			}
		}

		if (NewSpawnOutput)
		{
			for (const char* Name : { "level.graph", "level.gct.raw", "game.graph", "all.spawn" })
			{
				std::error_code Error;
				const auto Artifact = SpawnOutput / Name;
				if (std::filesystem::is_regular_file(Artifact, Error))
				{
					Artifacts.push_back(Artifact.string());
				}
			}
		}
	}

	CCommandVar RunAIStage(const char* Stage, CCommandVar First, CCommandVar Second)
	{
		const char* Compiler = SDL_getenv("IXRAY_MACRO_XRAI");
		const char* Config = SDL_getenv("IXRAY_MACRO_FSLTX");
		if (Compiler == nullptr || Config == nullptr || !First.IsString() || !Second.IsString())
		{
			Msg("! Configure the xrAI compiler and SDK before running AI macro commands");
			ReportAIStageError("compiler_not_configured");
			return 0u;
		}

		const xr_string Level = First;
		const xr_string Directory = Second;
		if (Level.empty() || Directory.empty() || Level.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_") != xr_string::npos)
		{
			Msg("! AI macro commands require a level name and an output directory");
			ReportAIStageError("invalid_arguments");
			return 0u;
		}

		std::error_code Error;
		const auto Output = std::filesystem::absolute(Directory.c_str(), Error);
		if (Error)
		{
			ReportAIStageError("invalid_output_directory");
			return 0u;
		}
		std::filesystem::create_directories(Output, Error);
		const auto LogPath = Output / (std::string(Stage) + ".log");
		std::ofstream Log(LogPath, std::ios::binary | std::ios::trunc);
		if (Error || !Log)
		{
			Msg("! Cannot open the xrAI stage log");
			ReportAIStageError("log_open_failed");
			return 0u;
		}

		if (g_AIStageReport != nullptr)
		{
			(*g_AIStageReport)["log_path"] = LogPath.string();
		}

		const char* Args[] = { Compiler, "--headless-ai", Stage, Level.c_str(), "-fsltx", Config, nullptr, nullptr, nullptr };
		const auto SpawnOutput = (Output / "spawn").string();
		const bool IsSpawn = xr_strcmp(Stage, "spawn") == 0;
		const bool NewSpawnOutput = IsSpawn && !std::filesystem::exists(SpawnOutput, Error) && !Error;
		if (IsSpawn)
		{
			Args[6] = "--output-dir";
			Args[7] = SpawnOutput.c_str();
		}

		SDL_PropertiesID Properties = SDL_CreateProperties();
		SDL_SetPointerProperty(Properties, SDL_PROP_PROCESS_CREATE_ARGS_POINTER, Args);
		SDL_SetNumberProperty(Properties, SDL_PROP_PROCESS_CREATE_STDIN_NUMBER, SDL_PROCESS_STDIO_NULL);
		SDL_SetNumberProperty(Properties, SDL_PROP_PROCESS_CREATE_STDOUT_NUMBER, SDL_PROCESS_STDIO_APP);
		SDL_SetBooleanProperty(Properties, SDL_PROP_PROCESS_CREATE_STDERR_TO_STDOUT_BOOLEAN, true);
		const std::unique_ptr<SDL_Process, decltype(&SDL_DestroyProcess)> Process(SDL_CreateProcessWithProperties(Properties), SDL_DestroyProcess);
		SDL_DestroyProperties(Properties);
		if (!Process)
		{
			Log << SDL_GetError() << '\n';
			ReportAIStageError("compiler_launch_failed");
			return 0u;
		}

		int ExitCode = -1;
		size_t Size = 0;
		const std::unique_ptr<void, decltype(&SDL_free)> OutputText(SDL_ReadProcess(Process.get(), &Size, &ExitCode), SDL_free);
		if (OutputText)
		{
			const auto* Text = static_cast<const char*>(OutputText.get());
			Log.write(Text, Size);
			if (ExitCode != 0)
			{
				ReportAIStageError("compiler_failed");
			}
			ReportAIStageOutput(std::string(Text, Size), SpawnOutput, NewSpawnOutput, ExitCode);
		}
		else
		{
			Log << SDL_GetError() << '\n';
			ReportAIStageError("compiler_output_unavailable");
			ReportAIStageOutput({}, SpawnOutput, NewSpawnOutput, ExitCode);
		}
		Log << "\nxrAI exit code: " << ExitCode << '\n';
		Log.close();
		if (!Log && ExitCode == 0)
		{
			ReportAIStageError("log_write_failed");
		}
		return u32(OutputText && Log && ExitCode == 0);
	}

	CCommandVar CommandBuildAIMap(CCommandVar First, CCommandVar Second)
	{
		return RunAIStage("build", First, Second);
	}

	CCommandVar CommandVerifyAIMap(CCommandVar First, CCommandVar Second)
	{
		return RunAIStage("verify", First, Second);
	}

	CCommandVar CommandBuildAISpawn(CCommandVar First, CCommandVar Second)
	{
		return RunAIStage("spawn", First, Second);
	}
}

CCommandVar ExecAIStageMacroCommand(u32 Command, const xr_string& First, const xr_string& Second, nlohmann::json& Report)
{
	Report = {{"compiler_exit_code", nullptr}, {"compiler_error_code", nullptr}, {"log_path", nullptr}, {"artifacts", nlohmann::json::array()}};
	auto* Previous = std::exchange(EditorAICommands::g_AIStageReport, &Report);
	const xr_scope_exit Restore([Previous] { EditorAICommands::g_AIStageReport = Previous; });
	return ExecCommand(Command, First, Second);
}

namespace EditorSceneCommands
{
	bool ReadMacroVector(const xr_string& Text, Fvector& Value)
	{
		std::istringstream Input(Text.c_str());
		Input.imbue(std::locale::classic());
		return (Input >> Value.x >> Value.y >> Value.z) && (Input >> std::ws).eof() &&
			std::isfinite(Value.x) && std::isfinite(Value.y) && std::isfinite(Value.z);
	}

	ESceneAIMapTool* GetEditableAIMap()
	{
		LUI->LoaderEvent.Wait();
		auto* Tool = smart_cast<ESceneAIMapTool*>(Scene->GetTool(OBJCLASS_AIMAP));
		return !Scene->locked() && !Scene->IsPlayInEditor() && Tool && Tool->IsEditable() ? Tool : nullptr;
	}

	CCommandVar CommandAddAINode(CCommandVar First, CCommandVar)
	{
		Fvector Position;
		if (!First.IsString() || !ReadMacroVector(xr_string(First), Position))
		{
			return xr_string("Add AI node requires world coordinates X Y Z.");
		}
		auto* Tool = GetEditableAIMap();
		if (!Tool || Tool->GetSnapList()->empty())
		{
			return xr_string("Add AI node requires an editable AI map with a populated Snap List.");
		}
		g_LevelAutoBackup.Cancel();
		GWorldPartition.StreamInAll();
		Tool->OnSynchronize();
		const auto Before = Tool->Nodes().size();
		if (!Tool->AddNode(Position, false, true, 1) || Tool->Nodes().size() == Before)
		{
			return xr_string("Could not add an AI node at these coordinates.");
		}
		Scene->UndoSave();
		EContext.UI->RedrawScene();
		return 1u;
	}

	CCommandVar CommandGenerateAIMap(CCommandVar, CCommandVar)
	{
		auto* Tool = GetEditableAIMap();
		if (!Tool || Tool->GetSnapList()->empty())
		{
			return xr_string("Generate AI map requires an editable AI map with a populated Snap List.");
		}
		if (Tool->Nodes().empty())
		{
			return xr_string("Generate AI map requires at least one AI node.");
		}
		g_LevelAutoBackup.Cancel();
		if (!Tool->GenerateMap(false))
		{
			return xr_string("Could not generate the AI map. Check log.");
		}
		EContext.UI->RedrawScene();
		return u32(Tool->Nodes().size());
	}

	CCommandVar AddSpawnEntity(const xr_string& Section, const Fvector& Position, float Radius)
	{
		LUI->LoaderEvent.Wait();
		auto* Tool = Scene->GetOTool(OBJCLASS_SPAWNPOINT);
		if (Scene->locked() || Scene->IsPlayInEditor() || Tool == nullptr || !Tool->IsEditable() ||
			Section.empty() || Section.size() >= 240 || !pSettings->section_exist(Section.c_str()) ||
			!pSettings->line_exist(Section.c_str(), "class"))
		{
			return 0u;
		}
		string256 Name;
		Scene->GenObjectName(OBJCLASS_SPAWNPOINT, Name, Section.c_str());
		auto Object = xr_make_unique<CSpawnPoint>(nullptr, Name);
		if (!Object->CreateSpawnData(Section.c_str()))
		{
			return 0u;
		}
		Object->NumSetPosition(Position);
		if (Radius > 0.f)
		{
			auto* ShapeTool = Scene->GetOTool(OBJCLASS_SHAPE);
			if (!Object->m_SpawnData.GetEntity()->shape() || !ShapeTool || !ShapeTool->IsEditable())
			{
				Msg("! Add spherical zone requires a spawn section that supports shapes");
				return 0u;
			}

			string256 ShapeName;
			Scene->GenObjectName(OBJCLASS_SHAPE, ShapeName, Name);
			xr_unique_ptr<CCustomObject> Shape(ShapeTool->CreateObject(nullptr, ShapeName));
			if (!Shape || !Shape->Valid())
			{
				return 0u;
			}

			Fsphere Sphere;
			Sphere.identity();
			Sphere.R = Radius;
			static_cast<CEditShape*>(Shape.get())->add_sphere(Sphere);
			Shape->NumSetPosition(Position);
			if (!Object->AttachObject(Shape.get()))
			{
				return 0u;
			}
			Shape.release();
		}
		Scene->AppendObject(Object.release());
		return xr_string(Name);
	}

	CCommandVar CommandAddSpawnEntity(CCommandVar First, CCommandVar Second)
	{
		Fvector Position;
		if (!First.IsString() || !Second.IsString() || !ReadMacroVector(xr_string(Second), Position))
		{
			return 0u;
		}
		return AddSpawnEntity(xr_string(First), Position, 0.f);
	}

	CCommandVar CommandAddSphericalZone(CCommandVar First, CCommandVar Second)
	{
		if (!First.IsString() || !Second.IsString())
		{
			return 0u;
		}

		const xr_string Coordinates = Second;
		std::istringstream Input(Coordinates.c_str());
		Input.imbue(std::locale::classic());
		Fvector Position;
		float Radius;
		if (!(Input >> Position.x >> Position.y >> Position.z >> Radius) || !(Input >> std::ws).eof() ||
			!std::isfinite(Position.x) || !std::isfinite(Position.y) || !std::isfinite(Position.z) ||
			!std::isfinite(Radius) || Radius <= 0.f)
		{
			Msg("! Add spherical zone expects X Y Z and a positive radius");
			return 0u;
		}
		return AddSpawnEntity(xr_string(First), Position, Radius);
	}

	u16 ReadLevelMode(const xr_string& Mode)
	{
		if (Mode == "single")
		{
			return eGameIDSingle;
		}
		if (Mode == "deathmatch" || Mode == "mp")
		{
			return eGameIDDeathmatch;
		}
		if (Mode == "freemp")
		{
			return eGameIDFreeMP;
		}
		return eGameIDNoGame;
	}

	CCommandVar CommandSetLevelMode(CCommandVar First, CCommandVar)
	{
		const u16 Mode = First.IsString() ? ReadLevelMode(xr_string(First)) : eGameIDNoGame;
		LUI->LoaderEvent.Wait();
		if (Mode == eGameIDNoGame || Scene->locked() || Scene->IsPlayInEditor())
		{
			Msg("! Set level mode expects an editable scene and single, deathmatch, or freemp");
			return 0u;
		}
		Scene->m_LevelOp.m_mapUsage.m_GameType.assign(Mode);
		Scene->UndoSave();
		return u32(Mode);
	}

	CCommandVar CommandAddPlayerRespawn(CCommandVar First, CCommandVar Second)
	{
		const u16 Mode = First.IsString() ? ReadLevelMode(xr_string(First)) : eGameIDNoGame;
		Fvector Position;
		if ((Mode != eGameIDDeathmatch && Mode != eGameIDFreeMP) ||
			!Second.IsString() || !ReadMacroVector(xr_string(Second), Position))
		{
			Msg("! Add player respawn expects deathmatch or freemp and X Y Z");
			return 0u;
		}

		LUI->LoaderEvent.Wait();
		auto* Tool = Scene->GetOTool(OBJCLASS_SPAWNPOINT);
		if (Scene->locked() || Scene->IsPlayInEditor() || !Tool || !Tool->IsEditable() ||
			!Scene->m_LevelOp.m_mapUsage.MatchType(Mode))
		{
			return 0u;
		}

		string256 Name;
		Scene->GenObjectName(OBJCLASS_SPAWNPOINT, Name, Mode == eGameIDFreeMP ? "freemp_spawn" : "dm_spawn");
		char RespawnType[] = RPOINT_CHOOSE_NAME;
		auto Object = xr_make_unique<CSpawnPoint>(RespawnType, Name);
		Object->m_GameType.m_GameType.assign(Mode);
		Object->m_RP_TeamID = 0;
		Object->NumSetPosition(Position);
		Scene->AppendObject(Object.release());
		return xr_string(Name);
	}

	CCommandVar CommandValidateLevelMode(CCommandVar First, CCommandVar)
	{
		const u16 Mode = First.IsString() ? ReadLevelMode(xr_string(First)) : eGameIDNoGame;
		LUI->LoaderEvent.Wait();
		if (Mode == eGameIDNoGame || !Scene->valid() || Scene->locked() || Scene->IsPlayInEditor())
		{
			return 0u;
		}

		auto Issues = nlohmann::json::array();
		const auto AddIssue = [&Issues](const char* Code, const char* Name = "")
		{
			Issues.push_back({{"code", Code}, {"object", Name}});
			Msg("! Level validation: %s %s", Code, Name);
		};
		if (!Scene->m_LevelOp.m_mapUsage.MatchType(Mode))
		{
			AddIssue("mode_not_enabled");
		}
		if (Scene->ListObj(OBJCLASS_SCENEOBJECT).empty() && Scene->ListObj(OBJCLASS_GROUP).empty())
		{
			AddIssue("missing_static_geometry");
		}

		u32 Actors = 0;
		u32 GraphPoints = 0;
		u32 Respawns = 0;
		auto* AIMap = smart_cast<ESceneAIMapTool*>(Scene->GetTool(OBJCLASS_AIMAP));
		for (auto* Object : Scene->ListObj(OBJCLASS_SPAWNPOINT))
		{
			auto* Spawn = static_cast<CSpawnPoint*>(Object);
			if (Mode == eGameIDSingle)
			{
				Actors += Spawn->RefCompare("actor");
				GraphPoints += Spawn->IsGraphPoint();
				const auto* Entity = smart_cast<CSE_ALifeObject*>(Spawn->m_SpawnData.GetEntity());
				if (Entity && Entity->m_gameType.MatchType(Mode) && Entity->used_ai_locations() &&
					AIMap && !AIMap->Nodes().empty())
				{
					const auto& Position = Spawn->GetPosition();
					const bool NearNode = std::ranges::any_of(AIMap->Nodes(), [&Position](const SAINode* Node)
					{
						return std::abs(Node->Pos.x - Position.x) <= 1.f &&
							std::abs(Node->Pos.z - Position.z) <= 1.f &&
							std::abs(Node->Pos.y - Position.y) <= 1.5f;
					});
					if (!NearNode)
					{
						AddIssue("spawn_far_from_ai_map", Spawn->GetName());
					}
				}
			}
			else if (Spawn->m_Type == ptRPoint && Spawn->m_RP_Type == rptActorSpawn && Spawn->m_GameType.MatchType(Mode))
			{
				++Respawns;
				Fvector Start = Spawn->GetPosition();
				Start.y += 0.1f;
				Fvector Down;
				Down.set(0.f, -1.f, 0.f);
				SRayPickInfo Hit;
				if (!Scene->RayPickObject(2.f, Start, Down, OBJCLASS_SCENEOBJECT, &Hit, nullptr))
				{
					AddIssue("respawn_no_visible_ground", Spawn->GetName());
				}
			}
		}
		if (Mode == eGameIDSingle)
		{
			if (Actors != 1)
			{
				AddIssue("expected_one_actor");
			}
			if (GraphPoints == 0)
			{
				AddIssue("missing_graph_point");
			}
			if (!AIMap || AIMap->Nodes().empty())
			{
				AddIssue("missing_ai_map");
			}
		}
		else if (Respawns == 0)
		{
			AddIssue("missing_mode_respawn");
		}

		return xr_string(nlohmann::json{{"ready", Issues.empty()}, {"mode_mask", Mode}, {"actors", Actors},
			{"graph_points", GraphPoints}, {"respawns", Respawns}, {"issues", std::move(Issues)}}.dump().c_str());
	}

	CCommandVar CommandListSceneObjects(CCommandVar, CCommandVar)
	{
		LUI->LoaderEvent.Wait();
		if (Scene->locked())
		{
			return 0u;
		}

		ObjectList Objects;
		Scene->GetQueryObjects(Objects, OBJCLASS_DUMMY, -1, -1, -1);
		auto Result = nlohmann::json::array();
		for (const auto* Object : Objects)
		{
			const auto& Position = Object->GetPosition();
			Result.push_back({{"name", Object->GetName()}, {"position", {Position.x, Position.y, Position.z}}});
		}
		return xr_string(Result.dump(-1, ' ', false, nlohmann::json::error_handler_t::replace).c_str());
	}

	CCommandVar CommandDeleteSceneObject(CCommandVar First, CCommandVar)
	{
		if (!First.IsString())
		{
			return 0u;
		}

		const xr_string Name = First;
		LUI->LoaderEvent.Wait();
		if (Name.empty() || Scene->locked())
		{
			return 0u;
		}

		auto* Object = Scene->FindObjectByName(Name.c_str(), OBJCLASS_DUMMY);
		if (Object == nullptr || Object->Locked() || Object->m_CO_Flags.test(CCustomObject::flObjectInGroup))
		{
			Msg("! Cannot delete scene object [%s]", Name.c_str());
			return 0u;
		}

		auto* Tool = Scene->GetOTool(Object->FClassID);
		if (Tool == nullptr || !Tool->IsEditable() || !Object->OnSelectionRemove())
		{
			return 0u;
		}

		const xr_string RemovedName = Object->GetName();
		if (!Scene->RemoveObject(Object, false, true))
		{
			return 0u;
		}
		xr_delete(Object);
		Scene->UndoSave();
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
		return RemovedName;
	}

	CCommandVar CommandSearchLibraryModels(CCommandVar First, CCommandVar)
	{
		if (!First.IsString())
		{
			return 0u;
		}

		xr_string Query = First;
		xr_strlwr(Query);
		FS_FileSet Files;
		Lib.GetObjects(Files);
		auto Matches = nlohmann::json::array();
		constexpr size_t Limit = 100;
		size_t Count = 0;
		for (const auto& File : Files)
		{
			xr_string Name = File.name.c_str();
			xr_strlwr(Name);
			if (Name.find(Query) != xr_string::npos)
			{
				++Count;
				if (Matches.size() < Limit)
				{
					Matches.push_back(File.name.c_str());
				}
			}
		}

		return xr_string(nlohmann::json{{"models", std::move(Matches)}, {"total", Count}, {"truncated", Count > Limit}}.dump(
			-1, ' ', false, nlohmann::json::error_handler_t::replace).c_str());
	}

	CCommandVar CommandFindSceneModelUsage(CCommandVar First, CCommandVar)
	{
		if (!First.IsString())
		{
			return 0u;
		}

		const xr_string Reference = First;
		LUI->LoaderEvent.Wait();
		if (Reference.empty() || Scene->locked())
		{
			return 0u;
		}

		ObjectList Objects;
		Scene->GetQueryObjects(Objects, OBJCLASS_SCENEOBJECT, -1, -1, -1);
		auto Matches = nlohmann::json::array();
		for (auto* Object : Objects)
		{
			const auto* Model = static_cast<CSceneObject*>(Object)->RefName();
			if (Model && xr_stricmp(Model, Reference.c_str()) == 0)
			{
				const auto& Position = Object->GetPosition();
				const auto& Rotation = Object->GetRotation();
				const auto& Scale = Object->GetScale();
				Matches.push_back({{"name", Object->GetName()}, {"model", Model},
					{"position", {Position.x, Position.y, Position.z}},
					{"rotation_radians", {Rotation.x, Rotation.y, Rotation.z}},
					{"scale", {Scale.x, Scale.y, Scale.z}}});
			}
		}

		return xr_string(nlohmann::json{{"matches", std::move(Matches)}}.dump(
			-1, ' ', false, nlohmann::json::error_handler_t::replace).c_str());
	}

	CCommandVar CommandReplaceSceneModels(CCommandVar First, CCommandVar Second)
	{
		if (!First.IsString() || !Second.IsString())
		{
			return 0u;
		}

		const xr_string Reference = First;
		const xr_string Text = Second;
		const auto Request = nlohmann::json::parse(Text.c_str(), nullptr, false);
		if (Reference.empty() || !Request.is_object() || !Request.contains("replacement") ||
			!Request["replacement"].is_string() || !Request.contains("objects") ||
			!Request["objects"].is_array() || Request["objects"].empty())
		{
			Msg("! Replace scene models expects a model reference and a JSON object with replacement and object names");
			return 0u;
		}

		const auto& Replacement = Request["replacement"].get_ref<const nlohmann::json::string_t&>();
		if (Replacement.empty() || Replacement.size() >= 240 || Replacement.find('\0') != Replacement.npos ||
			xr_stricmp(Reference.c_str(), Replacement.c_str()) == 0)
		{
			Msg("! Replace scene models expects a different library model reference");
			return 0u;
		}

		LUI->LoaderEvent.Wait();
		auto* Tool = Scene->GetOTool(OBJCLASS_SCENEOBJECT);
		if (Scene->locked() || Tool == nullptr || !Tool->IsEditable())
		{
			return 0u;
		}

		xr_vector<CSceneObject*> Objects;
		xr_hash_set<CSceneObject*> Seen;
		for (const auto& Entry : Request["objects"])
		{
			if (!Entry.is_string())
			{
				return 0u;
			}
			const auto& Name = Entry.get_ref<const nlohmann::json::string_t&>();
			if (Name.empty() || Name.find('\0') != Name.npos)
			{
				return 0u;
			}

			auto* Object = static_cast<CSceneObject*>(Scene->FindObjectByName(Name.c_str(), OBJCLASS_SCENEOBJECT));
			const char* Model = Object ? Object->RefName() : nullptr;
			if (!Model || xr_stricmp(Model, Reference.c_str()) != 0 || Object->Locked() ||
				Object->m_CO_Flags.test(CCustomObject::flObjectInGroup) || !Seen.insert(Object).second)
			{
				Msg("! Cannot replace model on [%s]: expected a unique, editable object referencing [%s]",
					Name.c_str(), Reference.c_str());
				return 0u;
			}
			Objects.push_back(Object);
		}

		auto* Asset = Lib.CreateEditObject(Replacement.c_str());
		if (!Asset)
		{
			Msg("! Cannot load replacement model [%s]", Replacement.c_str());
			return 0u;
		}
		const xr_scope_exit ReleaseAsset([&Asset] { Lib.RemoveEditObject(Asset); });
		for (auto* Object : Objects)
		{
			if (!Object->StreamIn() || !Object->GetReference())
			{
				Msg("! Cannot load original model on [%s]", Object->GetName());
				return 0u;
			}
		}

		auto Replaced = nlohmann::json::array();
		const shared_str NewReference = Asset->m_LibName.c_str();
		for (auto* Object : Objects)
		{
			Object->ReplaceReference(NewReference);
			Replaced.push_back(Object->GetName());
		}
		GWorldPartition.MarkDirty();
		Scene->UndoSave();
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
		EContext.UI->RedrawScene();
		return xr_string(nlohmann::json{{"model", NewReference.c_str()}, {"replaced", std::move(Replaced)}}.dump(
			-1, ' ', false, nlohmann::json::error_handler_t::replace).c_str());
	}

	enum class SceneTransform
	{
		Position,
		Rotation,
		Scale
	};

	CCommandVar SetSceneObjectTransform(CCommandVar First, CCommandVar Second, SceneTransform Transform)
	{
		if (!First.IsString() || !Second.IsString())
		{
			return 0u;
		}

		const xr_string Name = First;
		Fvector Value;
		if (Name.empty() || !ReadMacroVector(xr_string(Second), Value))
		{
			Msg("! Scene object transform expects a name and three finite coordinates");
			return 0u;
		}

		if (Transform == SceneTransform::Scale && (Value.x <= 0.f || Value.y <= 0.f || Value.z <= 0.f))
		{
			Msg("! Scene object scale must be positive on all axes");
			return 0u;
		}

		LUI->LoaderEvent.Wait();
		auto* Tool = Scene->GetOTool(OBJCLASS_SCENEOBJECT);
		auto* Object = Scene->FindObjectByName(Name.c_str(), OBJCLASS_SCENEOBJECT);
		if (Scene->locked() || Tool == nullptr || !Tool->IsEditable() ||
			Object == nullptr || Object->Locked())
		{
			Msg("! Cannot transform scene object [%s]", Name.c_str());
			return 0u;
		}

		g_LevelAutoBackup.Cancel();
		if (Transform == SceneTransform::Rotation)
		{
			Value.mul(deg2rad(1.f));
			Object->NumSetRotation(Value);
		}
		else if (Transform == SceneTransform::Scale)
		{
			Object->NumSetScale(Value);
		}
		else
		{
			Object->NumSetPosition(Value);
		}
		GWorldPartition.MarkDirty();
		Scene->UndoSave();
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
		EContext.UI->RedrawScene();
		return xr_string(Object->GetName());
	}

	CCommandVar CommandMoveSceneObject(CCommandVar First, CCommandVar Second)
	{
		return SetSceneObjectTransform(First, Second, SceneTransform::Position);
	}

	CCommandVar CommandRotateSceneObject(CCommandVar First, CCommandVar Second)
	{
		return SetSceneObjectTransform(First, Second, SceneTransform::Rotation);
	}

	CCommandVar CommandScaleSceneObject(CCommandVar First, CCommandVar Second)
	{
		return SetSceneObjectTransform(First, Second, SceneTransform::Scale);
	}

	bool IsMacroObjectName(const xr_string& Name)
	{
		return !Name.empty() && Name.size() < sizeof(string256) &&
			Name.find_first_not_of("abcdefghijklmnopqrstuvwxyz0123456789_") == xr_string::npos;
	}

	CCommandVar CommandCreateWaypointRoute(CCommandVar First, CCommandVar Second)
	{
		if (!First.IsString() || !Second.IsString())
		{
			return 0u;
		}
		xr_string Name = First;
		xr_strlwr(Name);
		const xr_string Text = Second;
		const auto Points = nlohmann::json::parse(Text.c_str(), nullptr, false);
		if (!IsMacroObjectName(Name) || !Points.is_array() || Points.empty() || Points.size() > 256)
		{
			return 0u;
		}

		xr_vector<Fvector> Positions;
		for (const auto& Point : Points)
		{
			if (!Point.is_array() || Point.size() != 3 ||
				!Point[0].is_number() || !Point[1].is_number() || !Point[2].is_number())
			{
				return 0u;
			}
			Fvector Position;
			Position.set(Point[0].get<float>(), Point[1].get<float>(), Point[2].get<float>());
			if (!std::isfinite(Position.x) || !std::isfinite(Position.y) || !std::isfinite(Position.z))
			{
				return 0u;
			}
			Positions.push_back(Position);
		}

		LUI->LoaderEvent.Wait();
		auto* Tool = Scene->GetOTool(OBJCLASS_WAY);
		if (Scene->locked() || Scene->IsPlayInEditor() || !Tool || !Tool->IsEditable() ||
			Scene->FindObjectByName(Name.c_str(), static_cast<CCustomObject*>(nullptr)))
		{
			return 0u;
		}

		xr_unique_ptr<CWayObject> Route(static_cast<CWayObject*>(Tool->CreateObject(nullptr, Name.c_str())));
		auto* Start = Route->GetFirstSelected();
		if (!Start)
		{
			return 0u;
		}
		Start->MoveTo(Positions.front());
		auto* Previous = Start;
		for (size_t Index = 1; Index < Positions.size(); ++Index)
		{
			auto* Point = Route->AppendWayPoint();
			Point->MoveTo(Positions[Index]);
			Previous->AddSingleLink(Point);
			Previous = Point;
		}
		if (Previous != Start)
		{
			Previous->AddSingleLink(Start);
		}
		Scene->AppendObject(Route.release());
		return Name;
	}

	CCommandVar CommandAssignNpcLogic(CCommandVar First, CCommandVar Second)
	{
		if (!First.IsString() || !Second.IsString())
		{
			return 0u;
		}
		const xr_string Name = First;
		xr_string Config = Second;
		std::replace(Config.begin(), Config.end(), '/', '\\');
		if (Config.size() >= 240 || !Config.starts_with("scripts\\") || !Config.ends_with(".ltx") ||
			Config.find("..") != xr_string::npos ||
			Config.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-.\\") != xr_string::npos ||
			!FS.exist("$game_config$", Config.c_str()))
		{
			return 0u;
		}

		LUI->LoaderEvent.Wait();
		auto* Tool = Scene->GetOTool(OBJCLASS_SPAWNPOINT);
		if (Scene->locked() || Scene->IsPlayInEditor() || !Tool || !Tool->IsEditable())
		{
			return 0u;
		}
		auto* Spawn = static_cast<CSpawnPoint*>(Scene->FindObjectByName(Name.c_str(), OBJCLASS_SPAWNPOINT));
		auto* Entity = Spawn && !Spawn->Locked() ? Spawn->m_SpawnData.GetEntity() : nullptr;
		if (!Entity || (!Entity->cast_human_abstract() && !Entity->cast_trader()))
		{
			return 0u;
		}

		IReader Reader(const_cast<char*>(Entity->m_ini_string.c_str()), Entity->m_ini_string.size());
		const CInifile Ini(&Reader, FS.get_path("$game_config$")->m_Path);
		if (Ini.section_exist("logic"))
		{
			const char* Existing = Ini.r_string_nullable("logic", "cfg");
			if (Existing && Config == Existing)
			{
				return Name;
			}
			Msg("! NPC [%s] already has a logic section", Name.c_str());
			return 0u;
		}
		xr_string CustomData = Entity->m_ini_string.c_str() ? Entity->m_ini_string.c_str() : "";
		CustomData += "\r\n[logic]\r\ncfg = ";
		CustomData += Config;
		CustomData += "\r\n";
		g_LevelAutoBackup.Cancel();
		Entity->m_ini_string = CustomData.c_str();
		xr_delete(Entity->m_ini_file);
		Scene->UndoSave();
		return Name;
	}

	CCommandVar CommandInspectSceneObject(CCommandVar First, CCommandVar)
	{
		if (!First.IsString())
		{
			return 0u;
		}
		LUI->LoaderEvent.Wait();
		if (Scene->locked())
		{
			return 0u;
		}
		const xr_string Name = First;
		auto* Object = Scene->FindObjectByName(Name.c_str(), static_cast<CCustomObject*>(nullptr));
		if (!Object)
		{
			return 0u;
		}

		const auto Vector = [](const Fvector& Value) { return nlohmann::json::array({Value.x, Value.y, Value.z}); };
		nlohmann::json Result = {
			{"name", Object->GetName()}, {"type", Scene->GetTool(Object->FClassID)->ClassName()},
			{"position", Vector(Object->GetPosition())}, {"rotation_radians", Vector(Object->GetRotation())},
			{"scale", Vector(Object->GetScale())}
		};
		if (auto* Static = smart_cast<CSceneObject*>(Object))
		{
			const char* Model = Static->RefName();
			Result["model"] = Model ? nlohmann::json(Model) : nlohmann::json(nullptr);
		}
		if (auto* Route = smart_cast<CWayObject*>(Object))
		{
			auto& Points = Result["waypoints"] = nlohmann::json::array();
			for (const auto& Position : Route->GetPointPositions())
			{
				Points.push_back(Vector(Position));
			}
		}
		if (auto* Spawn = smart_cast<CSpawnPoint*>(Object))
		{
			if (auto* Entity = Spawn->m_SpawnData.GetEntity())
			{
				Result["spawn_section"] = Entity->s_name.c_str();
				IReader Reader(const_cast<char*>(Entity->m_ini_string.c_str()), Entity->m_ini_string.size());
				const CInifile Ini(&Reader, FS.get_path("$game_config$")->m_Path);
				const char* Config = Ini.r_string_nullable("logic", "cfg");
				Result["logic_config"] = Config ? nlohmann::json(Config) : nlohmann::json(nullptr);
			}
		}
		return xr_string(Result.dump(-1, ' ', false, nlohmann::json::error_handler_t::replace).c_str());
	}

	CCommandVar CommandRenameSceneObject(CCommandVar First, CCommandVar Second)
	{
		if (!First.IsString() || !Second.IsString())
		{
			return 0u;
		}

		const xr_string Name = First;
		xr_string NewName = Second;
		xr_strlwr(NewName);
		LUI->LoaderEvent.Wait();
		if (Name.empty() || !IsMacroObjectName(NewName) || Scene->locked())
		{
			return 0u;
		}

		auto* Object = Scene->FindObjectByName(Name.c_str(), OBJCLASS_DUMMY);
		if (Object == nullptr || Object->Locked() || Scene->FindObjectByName(NewName.c_str(), Object))
		{
			Msg("! Cannot rename scene object [%s] as [%s]", Name.c_str(), NewName.c_str());
			return 0u;
		}

		auto* Tool = Scene->GetOTool(Object->FClassID);
		if (Tool == nullptr || !Tool->IsEditable())
		{
			return 0u;
		}

		g_LevelAutoBackup.Cancel();
		Object->SetName(NewName.c_str());
		GWorldPartition.MarkDirty();
		Scene->UndoSave();
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
		return NewName;
	}

	CCommandVar CommandGroupSceneObjects(CCommandVar First, CCommandVar Second)
	{
		if (!First.IsString() || !Second.IsString())
		{
			return 0u;
		}

		xr_string GroupName = First;
		xr_strlwr(GroupName);
		LUI->LoaderEvent.Wait();
		auto* GroupTool = Scene->GetOTool(OBJCLASS_GROUP);
		if (!IsMacroObjectName(GroupName) || Scene->locked() || GroupTool == nullptr ||
			!GroupTool->IsEditable() || Scene->FindObjectByName(GroupName.c_str(), OBJCLASS_DUMMY))
		{
			return 0u;
		}

		const xr_string Members = Second;
		const auto Names = nlohmann::json::parse(Members.c_str(), nullptr, false);
		if (!Names.is_array() || Names.size() < 2)
		{
			Msg("! Group scene objects expects a JSON array with at least two object names");
			return 0u;
		}

		ObjectList Objects;
		xr_set<CCustomObject*> Seen;
		for (const auto& Name : Names)
		{
			if (!Name.is_string())
			{
				return 0u;
			}
			const auto& Text = Name.get_ref<const std::string&>();
			if (Text.empty() || Text.find('\0') != std::string::npos)
			{
				return 0u;
			}
			auto* Object = Scene->FindObjectByName(Text.c_str(), OBJCLASS_DUMMY);
			if (Object == nullptr || Object->Locked() || Object->GetOwner() ||
				Object->m_CO_Flags.test(CCustomObject::flObjectInGroup) || !Object->CanAttach() ||
				!Seen.insert(Object).second)
			{
				Msg("! Cannot group scene object [%s]", Text.c_str());
				return 0u;
			}
			auto* Tool = Scene->GetOTool(Object->FClassID);
			if (Tool == nullptr || !Tool->IsEditable())
			{
				return 0u;
			}
			Objects.push_back(Object);
		}

		auto Group = xr_make_unique<CGroupObject>(nullptr, GroupName.c_str());
		Group->GroupObjects(Objects);
		Scene->AppendObject(Group.release());
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
		return GroupName;
	}

	CCommandVar CommandDuplicateSceneObject(CCommandVar First, CCommandVar Second)
	{
		if (!First.IsString() || !Second.IsString())
		{
			return 0u;
		}

		const xr_string Name = First;
		xr_string CopyName = Second;
		xr_strlwr(CopyName);
		if (Name.empty() || !IsMacroObjectName(CopyName))
		{
			Msg("! Duplicate scene object expects a source name and a new name using letters, digits or underscores");
			return 0u;
		}

		LUI->LoaderEvent.Wait();
		auto* Tool = Scene->GetOTool(OBJCLASS_SCENEOBJECT);
		auto* Object = Scene->FindObjectByName(Name.c_str(), OBJCLASS_SCENEOBJECT);
		if (Scene->locked() || Tool == nullptr || !Tool->IsEditable() || Object == nullptr ||
			Scene->FindObjectByName(CopyName.c_str(), OBJCLASS_DUMMY) != nullptr)
		{
			Msg("! Cannot duplicate scene object [%s] as [%s]", Name.c_str(), CopyName.c_str());
			return 0u;
		}

		CMemoryWriter Writer;
		Object->SaveStream(Writer);
		IReader Reader(Writer.pointer(), Writer.size());
		auto Copy = xr_make_unique<CSceneObject>(nullptr, CopyName.c_str());
		if (!Copy->LoadStream(Reader))
		{
			return 0u;
		}
		Copy->SetName(CopyName.c_str());
		Scene->AppendObject(Copy.release());
		return CopyName;
	}

	CCommandVar CommandAddLibraryObject(CCommandVar First, CCommandVar Second)
	{
		if (!First.IsString() || !Second.IsString())
		{
			return 0u;
		}

		const xr_string Reference = First;
		Fvector Position;
		if (Reference.empty() || Reference.size() >= 240 || !ReadMacroVector(xr_string(Second), Position))
		{
			Msg("! Add library object expects a library reference and three finite coordinates");
			return 0u;
		}

		LUI->LoaderEvent.Wait();
		if (Scene->locked())
		{
			return 0u;
		}

		string256 Name;
		Scene->GenObjectName(OBJCLASS_SCENEOBJECT, Name, Reference.c_str());
		auto Object = xr_make_unique<CSceneObject>(nullptr, Name);
		if (Object->SetReference(Reference.c_str()) == nullptr)
		{
			Msg("! Cannot load library object [%s]", Reference.c_str());
			return 0u;
		}

		Object->SetPosition(Position);
		Scene->SelectObjects(false, OBJCLASS_SCENEOBJECT);
		Scene->AppendObject(Object.release());
		return xr_string(Name);
	}

	CCommandVar CommandSaveNewLevel(CCommandVar First, CCommandVar)
	{
		if (!First.IsString())
		{
			return 0u;
		}

		const xr_string Name = First;
		const std::filesystem::path File(Name.c_str());
		std::error_code Error;
		if (!File.is_absolute() || File.extension() != ".level" ||
			!std::filesystem::is_directory(File.parent_path(), Error) || Error ||
			std::filesystem::exists(File, Error) || Error ||
			std::filesystem::exists(File.parent_path() / File.stem(), Error) || Error)
		{
			Msg("! Save new level expects an unused absolute .level path in an existing directory");
			return 0u;
		}

		auto Result = ExecCommand(COMMAND_SAVE, Name, 0u);
		if (!Result.IsInteger() || u32(Result) == 0 || !std::filesystem::is_regular_file(File, Error) || Error)
		{
			return 0u;
		}
		return Name;
	}

}

void CLevelMain::RegisterMacroCommands()
{
	REGISTER_CMD_S(COMMAND_BUILD_AI_MAP, EditorAICommands::CommandBuildAIMap);
	REGISTER_CMD_S(COMMAND_VERIFY_AI_MAP, EditorAICommands::CommandVerifyAIMap);
	REGISTER_CMD_S(COMMAND_BUILD_AI_SPAWN, EditorAICommands::CommandBuildAISpawn);
	REGISTER_CMD_S(COMMAND_ADD_LIBRARY_OBJECT, EditorSceneCommands::CommandAddLibraryObject);
	REGISTER_CMD_S(COMMAND_SAVE_NEW_LEVEL, EditorSceneCommands::CommandSaveNewLevel);
	REGISTER_CMD_S(COMMAND_SEARCH_LIBRARY_MODELS, EditorSceneCommands::CommandSearchLibraryModels);
	REGISTER_CMD_S(COMMAND_FIND_SCENE_MODEL_USAGE, EditorSceneCommands::CommandFindSceneModelUsage);
	REGISTER_CMD_S(COMMAND_REPLACE_SCENE_MODELS, EditorSceneCommands::CommandReplaceSceneModels);
	REGISTER_CMD_S(COMMAND_MOVE_SCENE_OBJECT, EditorSceneCommands::CommandMoveSceneObject);
	REGISTER_CMD_S(COMMAND_ROTATE_SCENE_OBJECT, EditorSceneCommands::CommandRotateSceneObject);
	REGISTER_CMD_S(COMMAND_LIST_SCENE_OBJECTS, EditorSceneCommands::CommandListSceneObjects);
	REGISTER_CMD_S(COMMAND_ADD_AI_NODE, EditorSceneCommands::CommandAddAINode);
	REGISTER_CMD_S(COMMAND_GENERATE_AI_MAP, EditorSceneCommands::CommandGenerateAIMap);
	REGISTER_CMD_S(COMMAND_ADD_SPAWN_ENTITY, EditorSceneCommands::CommandAddSpawnEntity);
	REGISTER_CMD_S(COMMAND_ADD_SPHERICAL_ZONE, EditorSceneCommands::CommandAddSphericalZone);
	REGISTER_CMD_S(COMMAND_SET_LEVEL_MODE, EditorSceneCommands::CommandSetLevelMode);
	REGISTER_CMD_S(COMMAND_ADD_PLAYER_RESPAWN, EditorSceneCommands::CommandAddPlayerRespawn);
	REGISTER_CMD_S(COMMAND_VALIDATE_LEVEL_MODE, EditorSceneCommands::CommandValidateLevelMode);
	REGISTER_CMD_S(COMMAND_CREATE_WAYPOINT_ROUTE, EditorSceneCommands::CommandCreateWaypointRoute);
	REGISTER_CMD_S(COMMAND_ASSIGN_NPC_LOGIC, EditorSceneCommands::CommandAssignNpcLogic);
	REGISTER_CMD_S(COMMAND_INSPECT_SCENE_OBJECT, EditorSceneCommands::CommandInspectSceneObject);
	REGISTER_CMD_S(COMMAND_EXPORT_LEVEL_BUILD, EditorPlayCommands::CommandExportLevelBuild);
	REGISTER_CMD_S(COMMAND_BUILD_PIE_COLLISION, EditorPlayCommands::CommandBuildPieStage<&EScene::BuildCForm>);
	REGISTER_CMD_S(COMMAND_BUILD_PIE_AI_MAP, EditorPlayCommands::CommandBuildPieStage<&EScene::BuildAIMap>);
	REGISTER_CMD_S(COMMAND_BUILD_PIE_GAME_GRAPH, EditorPlayCommands::CommandBuildPieStage<&EScene::BuildGameGraph>);
	REGISTER_CMD_S(COMMAND_PLAY_IN_EDITOR, EditorPlayCommands::CommandPlayInEditor);
	REGISTER_CMD_S(COMMAND_DELETE_SCENE_OBJECT, EditorSceneCommands::CommandDeleteSceneObject);
	REGISTER_CMD_S(COMMAND_DUPLICATE_SCENE_OBJECT, EditorSceneCommands::CommandDuplicateSceneObject);
	REGISTER_CMD_S(COMMAND_SCALE_SCENE_OBJECT, EditorSceneCommands::CommandScaleSceneObject);
	REGISTER_CMD_S(COMMAND_RENAME_SCENE_OBJECT, EditorSceneCommands::CommandRenameSceneObject);
	REGISTER_CMD_S(COMMAND_GROUP_SCENE_OBJECTS, EditorSceneCommands::CommandGroupSceneObjects);
}
