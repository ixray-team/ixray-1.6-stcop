#include "StdAfx.h"
#include "ViewerSession.h"
#include "ViewerModes.h"
#include "ViewerViewport.h"
#include "ViewerGpuPool.h"

#include "../../../xrEngine/xr_ioc_cmd.h"
#include "../../../xrEngine/ImGuiManager.h"
#include "../../../xrEngine/IGame_Level.h"
#include "../../../xrEngine/device.h"
#include "../../../xrEngine/xr_input.h"
#include "../../../xrRHI/RHI.h"
#include "../../Sandbox/SandboxTerrain.h"
#include "../../Level.h"
#include "../../LevelInspector.h"
#include "../../GamePersistent.h"
#include "../../Actor.h"
#include "../../../xrEngine/CameraManager.h"
#include "../../../xrEngine/x_ray.h"
#include "../../../xrPhysics/ODE/PHWorld.h"
#include "../../ActorEffector.h"

namespace ImUtils::Viewer
{
// Window + console-command details. Named, not anonymous.
namespace ViewerWindowDetail
{
	// Visibility lives in the shared editor-state slot (dockspace menu "Tools ->
	// Sandbox", the window close button and the level_viewer console command all
	// write the same flag).
	bool& ViewerWindowVisible()
	{
		return Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_LevelViewer)];
	}

	// The ticker is registered at engine init and must exist before (and
	// without) a session: it is a trivial static object that never constructs
	// the session itself.
	CViewerSessionTicker SessionTicker;

	// ---------------------------------------------------------------------
	// Console commands: every action is a request pushed onto the session's
	// queue; the session executes it on the primary thread in its tick.
	// ---------------------------------------------------------------------

	class CCC_LevelViewer : public IConsole_Command
	{
	public:
		CCC_LevelViewer(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }
		void Save(IWriter*) override {}

		virtual void Execute(const char* args)
		{
			bool& visible = ViewerWindowVisible();
			if (args == nullptr || args[0] == 0)
			{
				visible = !visible;
				return;
			}

			string16 value = {};
			sscanf(args, "%15s", value);
			if (0 == _stricmp(value, "on") || 0 == xr_strcmp(value, "1") || 0 == _stricmp(value, "true"))
				visible = true;
			else if (0 == _stricmp(value, "off") || 0 == xr_strcmp(value, "0") || 0 == _stricmp(value, "false"))
				visible = false;
			else
				visible = !visible;
		}

		virtual void Status(TStatus& S)
		{
			xr_strcpy(S, ViewerWindowVisible() ? "on" : "off");
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "toggle the in-game level viewer window ('on/off' or empty to toggle)");
		}
	};

	// viewer_spawn <section>: spawn any engine section (item, monster, NPC, ...)
	// at the free-camera aim point through the in-proc server.
	class CCC_ViewerSpawn : public IConsole_Command
	{
	public:
		CCC_ViewerSpawn(const char* N) : IConsole_Command(N) {}

		virtual void Execute(const char* args)
		{
			string64 section = {};
			sscanf(args, "%63s", section);
			if (section[0] == 0)
			{
				Msg("! viewer_spawn <section> - e.g. viewer_spawn m_dog_e");
				return;
			}
			SViewerRequest request;
			request.Type = EViewerRequest::SpawnSection;
			xr_strcpy(request.Section, section);
			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "<section> - spawn an entity at the viewer camera aim point");
		}
	};

	// viewer_spawn_actor: spawn a player-bound actor at the free-camera aim point.
	class CCC_ViewerSpawnActor : public IConsole_Command
	{
	public:
		CCC_ViewerSpawnActor(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }

		virtual void Execute(const char* args)
		{
			SViewerRequest request;
			request.Type = EViewerRequest::SpawnActor;
			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "spawn a player-bound actor at the viewer camera aim point");
		}
	};

	class CCC_ViewerHud : public IConsole_Command
	{
	public:
		CCC_ViewerHud(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			SViewerRequest request;
			request.Type = EViewerRequest::SetHudEnabled;
			request.BoolValue = args != nullptr && atoi(args) != 0;
			EnsureViewerSession().PushRequest(request);
		}
		void Info(TInfo& I) override
		{
			xr_strcpy(I, sizeof(I), "<0|1> - enable stock HUD, inventory and weapon UI on actor spawn");
		}
	};

	class CCC_ViewerTestHud : public IConsole_Command
	{
	public:
		CCC_ViewerTestHud(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }
		void Execute(const char*) override
		{
			SViewerRequest request;
			request.Type = EViewerRequest::VerifyHud;
			EnsureViewerSession().PushRequest(request);
		}
		void Info(TInfo& I) override
		{
			xr_strcpy(I, sizeof(I), "verify the stock weapon and inventory UI, including open and close");
		}
	};

	class CCC_ViewerInventory : public IConsole_Command
	{
	public:
		CCC_ViewerInventory(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }
		void Execute(const char* args) override
		{
			char action[16] = {};
			sscanf(args != nullptr ? args : "", "%15s", action);
			SViewerRequest request;
			request.Type = EViewerRequest::ToggleInventory;
			if (0 == _stricmp(action, "open") || 0 == _stricmp(action, "close"))
			{
				xr_strcpy(request.Section, action);
				args += strlen(action);
			}
			request.HeightValue = args != nullptr && args[0] != 0 ? std::max(0, atoi(args)) : 0;
			EnsureViewerSession().PushRequest(request);
		}
		void Info(TInfo& I) override
		{
			xr_strcpy(I, sizeof(I), "[open|close] [delay_ticks] - control the stock inventory through CUIGameCustom");
		}
	};

	class CCC_ViewerViewportSize : public IConsole_Command
	{
	public:
		CCC_ViewerViewportSize(const char* N) : IConsole_Command(N) {}

		virtual void Execute(const char* args)
		{
			SViewerRequest request;
			request.Type = EViewerRequest::SetViewportSize;
			if (args != nullptr && (0 == _stricmp(args, "third") || 0 == xr_strcmp(args, "1/3") || 0 == xr_strcmp(args, "0")))
				request.IntValue = 0;
			else if (args != nullptr && (0 == _stricmp(args, "half") || 0 == xr_strcmp(args, "1/2") || 0 == xr_strcmp(args, "2/4") || 0 == xr_strcmp(args, "1")))
				request.IntValue = 1;
			else
			{
				Msg("! viewer_viewport_size <third|half>");
				return;
			}
			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "<third|half> - square viewport source, aspect-preserving letterboxing");
		}
	};

	class CCC_ViewerWindowSize : public IConsole_Command
	{
	public:
		CCC_ViewerWindowSize(const char* N) : IConsole_Command(N) {}

		void Execute(const char* args) override
		{
			int Width = 0;
			int Height = 0;
			if (args == nullptr || sscanf(args, "%d %d", &Width, &Height) != 2 || Width <= 0 || Height <= 0)
			{
				Msg("! viewer_window_size <width> <height>");
				return;
			}
			SViewerRequest request;
			request.Type = EViewerRequest::SetWindowSize;
			request.IntValue = Width;
			request.HeightValue = Height;
			EnsureViewerSession().PushRequest(request);
		}

		void Info(TInfo& I) override
		{
			xr_strcpy(I, sizeof(I), "<width> <height> - resize the viewer window and exercise the responsive viewport");
		}
	};

	// sandbox_start <flat|plane|sine|slope|crater|steps|heightmap:<path>|prebuilt:<level>> [resources_mask] [actor]
	// Starts a sandbox session exactly like pressing Start in the viewer window.
	class CCC_SandboxStart : public IConsole_Command
	{
	public:
		CCC_SandboxStart(const char* N) : IConsole_Command(N) {}

		virtual void Execute(const char* args)
		{
			string64 preset = {};
			sscanf(args, "%63s", preset);
			if (preset[0] == 0)
			{
				Msg("! sandbox_start <flat|plane|sine|slope|crater|steps|heightmap:<path>|prebuilt:<level>> [resources_mask] [actor]");
				return;
			}

			SViewerRequest request;
			request.Type = EViewerRequest::StartSession;
			SViewerPreset& params = request.Preset;

			const char* rest = args + xr_strlen(preset);
			while (*rest == ' ')
				++rest;

			if (0 == xr_strcmp(preset, "flat"))
			{
				params.Terrain.Kind = Sandbox::ETerrainKind::Flat;
			}
			else if (0 == strncmp(preset, "prebuilt:", 9) && preset[9])
			{
				params.Terrain.Kind = Sandbox::ETerrainKind::PrebuiltLevel;
				params.Terrain.PrebuiltLevel = preset + 9;
			}
			else if (0 == strncmp(preset, "heightmap:", 10) && preset[10])
			{
				params.Terrain.Kind = Sandbox::ETerrainKind::Heightmap;
				params.Terrain.HeightmapPath = preset + 10;
			}
			else if (Sandbox::AnalyticPresetFromString(preset, params.Terrain.Analytic))
			{
				params.Terrain.Kind = Sandbox::ETerrainKind::Analytic;
			}
			else
			{
				Msg("! sandbox_start: unknown preset '%s'", preset);
				return;
			}

			// Optional trailing tokens: a resources mask string and/or 'actor'.
			if (rest[0] != 0)
			{
				// 'actor' is not a resource: cut the token out so the strict
				// mask parser never sees it.
				string512 maskText;
				xr_strcpy(maskText, rest);
				if (char* tok = strstr(maskText, "actor"))
				{
					params.SpawnActor = true;
					params.ArmActor = true;
					const size_t tail = xr_strlen(tok + 5) + 1;
					memmove(tok, tok + 5, tail);
				}

				Flags32 mask;
				// Never feed an empty/commas-only string: the parser zeroes the
				// mask even for empty input.
				bool hasTokens = false;
				for (const char* c = maskText; *c; ++c)
				{
					if (*c != ' ' && *c != '\t' && *c != ',')
					{
						hasTokens = true;
						break;
					}
				}
				if (hasTokens && LevelResourceMaskFromString(maskText, mask))
					params.Resources = mask;
				else if (hasTokens)
					Msg("! sandbox_start: resources mask '%s' not understood, keeping defaults", maskText);
			}

			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "<preset> [resources_mask] [actor] - start a sandbox level session");
		}
	};

	class CCC_SandboxStartAlife : public IConsole_Command
	{
	public:
		CCC_SandboxStartAlife(const char* N) : IConsole_Command(N) {}

		virtual void Execute(const char* args)
		{
			string64 level = {};
			sscanf(args, "%63s", level);
			if (level[0] == 0)
			{
				Msg("! sandbox_start_alife <level> [spawn-file]");
				return;
			}
			SViewerRequest request;
			request.Type = EViewerRequest::StartSession;
			request.Preset.Terrain.Kind = Sandbox::ETerrainKind::PrebuiltLevel;
			request.Preset.Terrain.PrebuiltLevel = level;
			request.Preset.StartAlife = true;
			request.Preset.Resources.set(u32(ELevelResource::Hud), true);
			const char* spawnFile = args + xr_strlen(level);
			while (*spawnFile == ' ')
				++spawnFile;
			if (*spawnFile != 0)
				request.Preset.AlifeSpawnFile = spawnFile;
			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "<level> [spawn-file] - full single-player new game through the in-proc server and ALife");
		}
	};

	class CCC_SandboxStop : public IConsole_Command
	{
	public:
		CCC_SandboxStop(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }

		virtual void Execute(const char* args)
		{
			if (ViewerSession() == nullptr)
				return;
			SViewerRequest request;
			request.Type = EViewerRequest::StopSession;
			ViewerSession()->PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "stop the sandbox level session");
		}
	};

	// viewer_mode <index>: switch the viewer mode (0=Scene 1=Gameplay 2=RenderTest).
	class CCC_ViewerMode : public IConsole_Command
	{
	public:
		CCC_ViewerMode(const char* N) : IConsole_Command(N) {}

		virtual void Execute(const char* args)
		{
			SViewerRequest request;
			request.Type = EViewerRequest::SetMode;
			request.IntValue = atoi(args);
			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "<index> - viewer mode: 0=Scene 1=Gameplay 2=RenderTest");
		}
	};

	class CCC_ViewerWeather : public IConsole_Command
	{
	public:
		CCC_ViewerWeather(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			SViewerRequest request;
			request.Type = EViewerRequest::SetWeatherCycle;
			xr_strcpy(request.Section, args != nullptr ? args : "");
			EnsureViewerSession().PushRequest(request);
		}
		void Info(TInfo& I) override
		{
			xr_strcpy(I, sizeof(I), "<cycle> - use an installed CEnvironment weather cycle, e.g. default_rain");
		}
	};

	// viewer_shot [frames]: take an engine screenshot this many frames after the
	// session is active (debug/verification aid for the viewport render).
	class CCC_ViewerShot : public IConsole_Command
	{
	public:
		CCC_ViewerShot(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }

		virtual void Execute(const char* args)
		{
			SViewerRequest request;
			request.Type = EViewerRequest::Screenshot;
			request.IntValue = (args != nullptr && args[0] != 0) ? atoi(args) : 30;
			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "[frames] - screenshot N frames after the session is active (default 30)");
		}
	};

	// viewer_swapdump <name> [delay_ticks]: dump the swapchain backbuffer to a BMP in $logs$
	// (verification aid: proves the menu/game frame renders, works in the menu
	// where the autotest frame counter does not advance).
	class CCC_ViewerSwapDump : public IConsole_Command
	{
	public:
		CCC_ViewerSwapDump(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }

		virtual void Execute(const char* args)
		{
			string64 name = "swapdump.bmp";
			int delay = 0;
			if (args != nullptr && args[0] != 0)
				sscanf(args, "%63s %d", name, &delay);
			if (!xr_strstr(name, ".bmp"))
				xr_strcat(name, ".bmp");
			if (delay > 0)
			{
				SViewerRequest request;
				request.Type = EViewerRequest::DumpSwapchain;
				request.HeightValue = delay;
				xr_strcpy(request.Section, name);
				EnsureViewerSession().PushRequest(request);
				return;
			}
			ImUtils::Viewer::CViewerRenderer::DumpRTVToBmp(GRHI->DevicePtr->SwapChainRTV, name);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "<name.bmp> [delay_ticks] - dump the swapchain backbuffer to $logs$");
		}
	};

	// viewer_rt_verify [bmp_name] [delay_ticks]: read back the viewport render
	// target, report terrain coverage, and dump a BMP into $logs$ for visual
	// inspection. The delay lets the dump be scheduled past deferred spawns.
	class CCC_ViewerRtVerify : public IConsole_Command
	{
	public:
		CCC_ViewerRtVerify(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }

		virtual void Execute(const char* args)
		{
			SViewerRequest request;
			request.Type = EViewerRequest::VerifyRender;
			if (args != nullptr && args[0] != 0)
			{
				string64 name = {};
				int delay = 0;
				const int fields = sscanf(args, "%63s %d", name, &delay);
				if (fields >= 1)
					xr_strcpy(request.Section, name);
				if (fields >= 2 && delay > 0)
					request.IntValue = delay;
			}
			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "[bmp_name] [delay_ticks] - RT readback coverage check + BMP dump into logs");
		}
	};

	// viewer_wander [0|1]: toggle the scriptless NPC wander driver (Gameplay mode).
	class CCC_ViewerWander : public IConsole_Command
	{
	public:
		CCC_ViewerWander(const char* N) : IConsole_Command(N) {}

		virtual void Execute(const char* args)
		{
			SViewerRequest request;
			request.Type = EViewerRequest::SetWander;
			request.BoolValue = args == nullptr || args[0] == 0 || atoi(args) != 0;
			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "[0|1] - toggle the scriptless NPC wander driver (default on)");
		}
	};

	// viewer_test_spawns: in-game scenario test - spawns an item, a monster and
	// an NPC, then verifies next-frame that the objects exist. Works with and
	// without an AI-map (degradation case) and logs the outcome.
	class CCC_ViewerTestSpawns : public IConsole_Command
	{
	public:
		CCC_ViewerTestSpawns(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }

		virtual void Execute(const char* args)
		{
			SViewerRequest request;
			request.Type = EViewerRequest::RunSpawnTest;
			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "spawn item+monster+NPC at the aim point and verify they appear (degradation-safe)");
		}
	};

	class CCC_ViewerTestActor : public IConsole_Command
	{
	public:
		CCC_ViewerTestActor(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }

		virtual void Execute(const char* args)
		{
			SViewerRequest request;
			request.Type = EViewerRequest::TestActorMovement;
			request.BoolValue = args != nullptr && (0 == _stricmp(args, "backward") || 0 == _stricmp(args, "back") || 0 == _stricmp(args, "reverse"));
			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I)
		{
			xr_strcpy(I, sizeof(I), "[forward|backward] - drive stock movement input and verify actor movement and first-person binding");
		}
	};

	class CCC_ViewerTestPatrol : public IConsole_Command
	{
	public:
		CCC_ViewerTestPatrol(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			SViewerRequest request;
			request.Type = EViewerRequest::VerifyPatrolPaths;
			request.BoolValue = args != nullptr && args[0] != 0 && atoi(args) != 0;
			EnsureViewerSession().PushRequest(request);
		}
		void Info(TInfo& I) override
		{
			xr_strcpy(I, sizeof(I), "<0|1> - verify that patrol paths are unloaded or loaded");
		}
	};

	class CCC_ViewerPrimaryContinue : public IConsole_Command
	{
	public:
		CCC_ViewerPrimaryContinue(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }

		void Execute(const char*) override
		{
			if (g_pGameLevel == nullptr || g_pGameLevel->IsParallel())
				return;
			IInputReceiver* receiver = static_cast<CLevel*>(g_pGameLevel)->Flow.ActiveInput;
			if (receiver == nullptr)
				return;
			receiver->IR_OnKeyboardPress(SDL_SCANCODE_ESCAPE);
			if (static_cast<CLevel*>(g_pGameLevel)->Flow.ActiveInput == receiver)
				receiver->IR_OnKeyboardRelease(SDL_SCANCODE_ESCAPE);
		}
	};

	class CCC_ViewerPrimaryTest : public IConsole_Command
	{
	public:
		CCC_ViewerPrimaryTest(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			SViewerRequest request;
			request.Type = EViewerRequest::PrimaryTest;
			xr_strcpy(request.Section, args != nullptr ? args : "");
			EnsureViewerSession().PushRequest(request);
		}
		void Info(TInfo& info) override
		{
			xr_strcpy(info, sizeof(info), "<dialogue_prepare|dialogue_start|dialogue_stop|tutorial_start|tutorial_stop|quest_start|quest_complete|quest_status|anomaly_prepare|anomaly_enter|anomaly_status|anomaly_leave>");
		}
	};

	class CCC_ViewerLifecycleTest : public IConsole_Command
	{
	public:
		CCC_ViewerLifecycleTest(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			EnsureViewerSession().StartLifecycleTest(args);
		}
		void Info(TInfo& info) override
		{
			xr_strcpy(info, sizeof(info), "<viewer_first|in_game> <save_name> [sandbox|ogf|material|particle|ai] - real save/load and level-transition checks");
		}
	};

	class CCC_ViewerInspector : public IConsole_Command
	{
	public:
		CCC_ViewerInspector(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			string64 category = {};
			int enabled = 0;
			if (args == nullptr || sscanf(args, "%63s %d", category, &enabled) != 2)
			{
				Msg("! viewer_inspector <ai_map|waypoints|game_graph|ai_paths|smart_terrain|cform|selection|all_off> <0|1>");
				return;
			}
			SViewerRequest request;
			request.Type = EViewerRequest::SetInspector;
			request.BoolValue = enabled != 0;
			xr_strcpy(request.Section, category);
			EnsureViewerSession().PushRequest(request);
		}
	};

	class CCC_ViewerWireframe : public IConsole_Command
	{
	public:
		CCC_ViewerWireframe(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			string32 mode = {};
			string32 selection = {};
			if (args == nullptr || sscanf(args, "%31s %31s", mode, selection) < 1)
			{
				Msg("! viewer_wireframe <off|overlay|only> [object-id|-1|terrain|static]");
				return;
			}
			int objectId = -1;
			if (0 == _stricmp(selection, "last"))
			{
				const CViewerSession* session = ViewerSession();
				if (session == nullptr || session->GetState().LastSpawnedId < 0)
				{
					Msg("! viewer_wireframe: no object has been spawned in the selected session");
					return;
				}
				objectId = session->GetState().LastSpawnedId;
			}
			else if (0 == _stricmp(selection, "terrain"))
				objectId = int(IRender_interface::RenderWireframeTerrain);
			else if (0 == _stricmp(selection, "static"))
				objectId = int(IRender_interface::RenderWireframeStatic);
			else if (selection[0] != 0)
				objectId = atoi(selection);
			SViewerRequest request;
			request.Type = EViewerRequest::SetWireframe;
			request.HeightValue = objectId;
			if (0 == _stricmp(mode, "overlay"))
				request.IntValue = 1;
			else if (0 == _stricmp(mode, "only"))
				request.IntValue = 2;
			else
				request.IntValue = 0;
			EnsureViewerSession().PushRequest(request);
		}
	};

	class CCC_ViewerPause : public IConsole_Command
	{
	public:
		CCC_ViewerPause(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }

		void Execute(const char* args) override
		{
			if (args == nullptr || args[0] == 0 || 0 == _stricmp(args, "status"))
			{
				const CViewerSession* session = ViewerSession();
				if (session == nullptr)
					Msg("* viewer_pause: no session; no simulation or presentation work");
				else
				{
					const SViewerState& state = session->GetState();
					Msg("* viewer_pause: state=%d paused=%d presentation=%d visible=%d ticks=%llu pending=%u", int(state.Session), state.SimulationPaused, state.PresentationEnabled,
						ViewerWindowWasVisible(), static_cast<unsigned long long>(state.SimulationTicks), session->PendingRequests());
				}
				return;
			}
			SViewerRequest request;
			request.Type = EViewerRequest::SetSimulationPaused;
			request.BoolValue = atoi(args) != 0;
			EnsureViewerSession().PushRequest(request);
		}
	};

	class CCC_ViewerPresentation : public IConsole_Command
	{
	public:
		CCC_ViewerPresentation(const char* N) : IConsole_Command(N) {}

		void Execute(const char* args) override
		{
			SViewerRequest request;
			request.Type = EViewerRequest::SetPresentationEnabled;
			request.BoolValue = args != nullptr && atoi(args) != 0;
			EnsureViewerSession().PushRequest(request);
		}
	};

	class CCC_ViewerPrimaryProbe : public IConsole_Command
	{
	public:
		CCC_ViewerPrimaryProbe(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }

		void Execute(const char*) override
		{
			if (g_pGameLevel == nullptr || g_pGameLevel->IsParallel())
			{
				Msg("* viewer_primary: no primary level");
				return;
			}
			const shared_str name = g_pGameLevel->name();
			const FS_Path* alias = FS.get_path("$level$");
			const Fvector position = g_actor != nullptr ? g_actor->Position() : Fvector().set(0.f, 0.f, 0.f);
			Msg("* viewer_primary: level='%s' alias='%s' id=%u ready=%d actor=%p pos=[%.3f %.3f %.3f] ai=%p physics=%p environment=%p",
				name ? name.c_str() : "<unnamed>", alias->m_Add != nullptr ? alias->m_Add : "<root>", unsigned(pApp->Level_Current),
				int(g_pGameLevel->bReady), g_actor, position.x, position.y, position.z, g_ai_space, ph_world, g_pGamePersistent->pEnvironment);
		}
	};

	// viewer_dumptargets: dump the session's existing scene and G-buffer textures.
	class CCC_ViewerDumpTargets : public IConsole_Command
	{
	public:
		CCC_ViewerDumpTargets(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }

		virtual void Execute(const char* args) override
		{
			SViewerRequest request;
			request.Type = EViewerRequest::DumpSceneTargets;
			EnsureViewerSession().PushRequest(request);
		}

		virtual void Info(TInfo& I) override
		{
			xr_strcpy(I, sizeof(I), "dump the session's existing color, normal, position and lighting targets to $logs$");
		}
	};

	class CCC_ViewerRenderPass : public IConsole_Command
	{
	public:
		CCC_ViewerRenderPass(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			string64 name = {};
			int enabled = 0;
			if (sscanf(args, "%63s %d", name, &enabled) != 2)
			{
				Msg("! viewer_render_pass <static|dynamic|details|occlusion|wireframe|all> <0|1>");
				return;
			}
			SViewerRequest request;
			request.Type = EViewerRequest::SetRenderPass;
			request.BoolValue = enabled != 0;
			xr_strcpy(request.Section, name);
			EnsureViewerSession().PushRequest(request);
		}
	};

	class CCC_ViewerResource : public IConsole_Command
	{
	public:
		CCC_ViewerResource(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			string32 name = {};
			int enabled = 0;
			if (sscanf(args, "%31s %d", name, &enabled) != 2)
			{
				Msg("! viewer_resource <resource_name> <0|1> - load or unload one level resource");
				return;
			}
			Flags32 mask;
			if (!LevelResourceMaskFromString(name, mask) || mask.flags == 0 || (mask.flags & (mask.flags - 1)) != 0)
			{
				Msg("! viewer_resource: expected exactly one resource name");
				return;
			}
			SViewerRequest request;
			request.Type = EViewerRequest::SetResource;
			request.IntValue = IGame_Level::ResourceStateIndex(ELevelResource(mask.flags));
			request.BoolValue = enabled != 0;
			EnsureViewerSession().PushRequest(request);
		}
	};

	class CCC_ViewerResourceTest : public IConsole_Command
	{
	public:
		CCC_ViewerResourceTest(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }
		void Execute(const char*) override
		{
			SViewerRequest request;
			request.Type = EViewerRequest::RunResourceMatrix;
			EnsureViewerSession().PushRequest(request);
		}
	};

	class CCC_ViewerGroundDump : public IConsole_Command
	{
	public:
		CCC_ViewerGroundDump(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }
		void Execute(const char* args) override
		{
			SViewerRequest request;
			request.Type = EViewerRequest::DumpTerrainTexture;
			if (args != nullptr && args[0] != 0)
				xr_strcpy(request.Section, args);
			EnsureViewerSession().PushRequest(request);
		}
	};

	class CCC_ViewerSession : public IConsole_Command
	{
	public:
		CCC_ViewerSession(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			int index = -1;
			if (sscanf(args, "%d", &index) != 1 || index < 0 || index >= CViewerSessionManager::MaxSessions)
			{
				Msg("! viewer_session <0|1>");
				return;
			}
			CViewerSession& session = EnsureViewerSession(index);
			Msg("* Level viewer: selected session %d", session.Index());
		}
	};

	class CCC_ViewerPairStart : public IConsole_Command
	{
	public:
		CCC_ViewerPairStart(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			string64 level = "zaton";
			string64 spawn = "all";
			sscanf(args, "%63s %63s", level, spawn);
			SViewerPreset serverPreset;
			serverPreset.AlifeSpawnFile = spawn;
			serverPreset.StartAlife = true;
			serverPreset.Resources.set(u32(ELevelResource::Hud), true);
			if (0 == _stricmp(level, "flat"))
			{
				serverPreset.Terrain.Kind = Sandbox::ETerrainKind::Analytic;
				serverPreset.Terrain.Analytic = Sandbox::EAnalyticPreset::Plane;
				serverPreset.StartAlife = false;
				serverPreset.SpawnActor = false;
				serverPreset.ArmActor = true;
			}
			else
			{
				serverPreset.Terrain.Kind = Sandbox::ETerrainKind::PrebuiltLevel;
				serverPreset.Terrain.PrebuiltLevel = level;
			}
			serverPreset.Role = EViewerRole::Server;
			SViewerPreset clientPreset = serverPreset;
			clientPreset.StartAlife = false;
			clientPreset.SpawnActor = false;
			clientPreset.ArmActor = false;
			clientPreset.Role = EViewerRole::Client;

			CViewerSession& server = EnsureViewerSession(0);
			CViewerSession& client = EnsureViewerSession(1);
			SViewerRequest startServer;
			startServer.Type = EViewerRequest::StartSession;
			startServer.Preset = serverPreset;
			server.PushRequest(startServer);
			SViewerRequest startClient;
			startClient.Type = EViewerRequest::StartSession;
			startClient.Preset = clientPreset;
			client.PushRequest(startClient);
		}
	};

	class CCC_ViewerPairStop : public IConsole_Command
	{
	public:
		CCC_ViewerPairStop(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }
		void Execute(const char*) override
		{
			for (u32 index = CViewerSessionManager::MaxSessions; index-- > 0;)
			{
				if (CViewerSession* session = ViewerSessionAt(index))
				{
					SViewerRequest stop;
					stop.Type = EViewerRequest::StopSession;
					session->PushRequest(stop);
				}
			}
		}
	};

	class CCC_ViewerPairVerify : public IConsole_Command
	{
	public:
		CCC_ViewerPairVerify(const char* N) : IConsole_Command(N) { bEmptyArgsHandled = true; }
		void Execute(const char*) override
		{
			CViewerSession* server = ViewerSessionAt(0);
			CViewerSession* client = ViewerSessionAt(1);
			if (server == nullptr || client == nullptr || !server->IsActive() || !client->IsActive())
			{
				Msg("! viewer_pair_verify: server/client sessions are not both ready");
				return;
			}
			CLevel* serverLevel = server->GetLevel();
			CLevel* clientLevel = client->GetLevel();
			const bool ownerBound = serverLevel->Server != nullptr && serverLevel->Server->GetOwnerLevel() == serverLevel &&
				clientLevel->Server == nullptr && clientLevel->RemoteServerEndpoint() == serverLevel->Server;
			const xrClientData* hostOnServer = ownerBound ? static_cast<const xrClientData*>(serverLevel->Server->GetServerClient()) : nullptr;
			const xrClientData* clientOnServer = ownerBound ? serverLevel->Server->ID_to_client(clientLevel->GetClientID()) : nullptr;
			const bool profilesBound = hostOnServer != nullptr && hostOnServer->ps != nullptr && clientOnServer != nullptr &&
				clientOnServer->ps != nullptr && hostOnServer->ID != clientOnServer->ID;
			const CSE_Abstract* hostActor = hostOnServer != nullptr ? hostOnServer->owner : nullptr;
			const CSE_Abstract* assignedActor = clientOnServer != nullptr ? clientOnServer->owner : nullptr;
			const CObject* serverView = serverLevel->CurrentViewEntity();
			const CObject* clientView = clientLevel->CurrentViewEntity();
			const CActor* serverPlayer = server->PlayerActor();
			const CActor* clientPlayer = client->PlayerActor();
			const bool hostActorBound = hostActor != nullptr && serverView != nullptr && serverView->ID() == hostActor->ID && serverPlayer == serverView;
			const bool actorBound = assignedActor != nullptr && clientView != nullptr && clientView->Local() &&
				clientView->ID() == assignedActor->ID && clientPlayer == clientView && client->GetState().HasActor;
			const bool independentCameras = serverPlayer != nullptr && clientPlayer != nullptr && serverPlayer != clientPlayer &&
				serverPlayer->Cameras().Position().distance_to(clientPlayer->Cameras().Position()) > 0.05f;
			const bool movementPassed = server->ActorMovementPassed() && client->ActorMovementPassed() &&
				server->ActorMovementDistance() > 0.25f && client->ActorMovementDistance() > 0.25f;
			const bool passed = ownerBound && profilesBound && hostActorBound && actorBound && independentCameras && movementPassed;
			Msg("* viewer_pair_verify: server/client objects %u/%u, owner binding %s, profiles %s, host actor %u, client actor %u local=%d, player views %u/%u, camera distance %.3f, movement %.3f/%.3f -> %s",
				serverLevel->Objects.o_count(), clientLevel->Objects.o_count(), ownerBound ? "OK" : "FAIL", profilesBound ? "OK" : "FAIL",
				hostActor != nullptr ? hostActor->ID : 0, assignedActor != nullptr ? assignedActor->ID : 0,
				clientView != nullptr && clientView->Local(), serverPlayer != nullptr ? serverPlayer->ID() : 0, clientPlayer != nullptr ? clientPlayer->ID() : 0,
				independentCameras ? serverPlayer->Cameras().Position().distance_to(clientPlayer->Cameras().Position()) : 0.f,
				server->ActorMovementDistance(), client->ActorMovementDistance(), passed ? "PASS" : "FAIL");
		}
	};

	SViewerWindowState& WindowState(CViewerSession& session)
	{
		return session.GetWindowState();
	}

	ImVec4 ResourceStateColor(EResourceState state)
	{
		switch (state)
		{
		case EResourceState::Ready:   return ImVec4(0.35f, 0.95f, 0.35f, 1.f);
		case EResourceState::Missing: return ImVec4(0.95f, 0.80f, 0.20f, 1.f);
		case EResourceState::Failed:  return ImVec4(0.95f, 0.30f, 0.30f, 1.f);
		case EResourceState::Loading: return ImVec4(0.40f, 0.70f, 1.00f, 1.f);
		default:                      return ImVec4(0.60f, 0.60f, 0.60f, 1.f);
		}
	}

	// The selected session's stock-renderer output with optional debug overlays.
	// Panel dimensions are applied to the target through the request queue.
	void DrawViewport(CViewerSession& session)
	{
		if (!ImGui::CollapsingHeader("Viewport", ImGuiTreeNodeFlags_DefaultOpen))
			return;

		const SViewerState& state = session.GetState();
		const float FooterRows = state.Primary ? 1.f : 2.f;
		const float FooterHeight = FooterRows * (ImGui::GetFrameHeightWithSpacing() + ImGui::GetTextLineHeightWithSpacing());
		const ImVec2 available = ImGui::GetContentRegionAvail();
		ImGui::BeginChild("ViewerViewport", ImVec2(std::max(1.f, available.x), std::max(1.f, available.y - FooterHeight)), true,
			ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse);
		const ImVec2 canvasPos = ImGui::GetCursorScreenPos();
		const ImVec2 canvasSize = ImGui::GetContentRegionAvail();
		ImDrawList* drawList = ImGui::GetWindowDrawList();

		if (state.Primary && state.HasRenderScene)
		{
			const auto Crop = ViewerViewport::CenteredCoverCrop(float(Device.Width), float(Device.Height), canvasSize.x, canvasSize.y);
			ImGui::Image(GRHI->DevicePtr->RenderSRV, canvasSize, ImVec2(Crop.Min.x, Crop.Min.y), ImVec2(Crop.Max.x, Crop.Max.y));
		}
		else
		{
			const float SourceWidth = float(session.Renderer.IsReady() ? session.Renderer.ViewportWidth() : state.ViewportWidth);
			const float SourceHeight = float(session.Renderer.IsReady() ? session.Renderer.ViewportHeight() : state.ViewportHeight);
			const auto Crop = ViewerViewport::CenteredCoverCrop(SourceWidth, SourceHeight, canvasSize.x, canvasSize.y);
			if (session.Renderer.IsReady())
				ImGui::Image(session.Renderer.ViewSRV(), canvasSize, ImVec2(Crop.Min.x, Crop.Min.y), ImVec2(Crop.Max.x, Crop.Max.y));
			else
				drawList->AddRectFilled(canvasPos, ImVec2(canvasPos.x + canvasSize.x, canvasPos.y + canvasSize.y), IM_COL32(12, 16, 20, 255));
			if (!session.IsActive())
				drawList->AddText(ImVec2(canvasPos.x + 8.f, canvasPos.y + 8.f), IM_COL32(140, 140, 140, 255),
					state.Session == EViewerSessionState::Starting ? "loading..." : "no session");
			const int width = std::max(16, int(canvasSize.x));
			const int height = std::max(16, int(canvasSize.y));
			if (state.ViewportWidth != u32(width) || state.ViewportHeight != u32(height))
			{
				SViewerRequest request;
				request.Type = EViewerRequest::SetViewportSize;
				request.IntValue = width;
				request.HeightValue = height;
				session.PushRequest(request);
			}
		}
		ImGui::EndChild();

		const u32 width = session.Renderer.IsReady() ? session.Renderer.ViewportWidth() : state.ViewportWidth;
		const u32 height = session.Renderer.IsReady() ? session.Renderer.ViewportHeight() : state.ViewportHeight;
		ImGui::TextDisabled("%u x %u source pixels; the viewport follows the docked panel.", width, height);

		if (!state.Primary)
		{
			SViewerState& controls = session.ViewerState;
			ImGui::Checkbox("Capture game input (walk with stock bindings)", &controls.InputCapture);
			ImGui::TextDisabled("Alt enables editor input. Capture here to use the session's stock bindings; walking requires an actor.");
			if (ImGui::Checkbox("Pause session simulation", &controls.SimulationPaused))
			{
				SViewerRequest request;
				request.Type = EViewerRequest::SetSimulationPaused;
				request.BoolValue = controls.SimulationPaused;
				session.PushRequest(request);
			}
			ImGui::TextDisabled("Simulation ticks: %llu. Closing the window stops the session; presentation-only suspension keeps it ticking.", static_cast<unsigned long long>(state.SimulationTicks));
		}
	}

	void DrawTerrainEditor(SViewerWindowState& ui, bool sessionBusy)
	{
		if (!ImGui::CollapsingHeader("Terrain", ImGuiTreeNodeFlags_DefaultOpen))
			return;

		ImGui::BeginDisabled(sessionBusy);

		const char* terrainKinds[] = { "Flat", "Analytic", "Heightmap", "Prebuilt level" };
		ImGui::Combo("Kind", &ui.TerrainKind, terrainKinds, IM_ARRAYSIZE(terrainKinds));

		switch (Sandbox::ETerrainKind(ui.TerrainKind))
		{
		case Sandbox::ETerrainKind::Flat:
			ImGui::DragFloat("Size", &ui.Preset.Terrain.Size, 1.f, 16.f, 4096.f, "%.0f m");
			ImGui::DragFloat("Cell", &ui.Preset.Terrain.Cell, 0.05f, 0.25f, 16.f, "%.2f m");
			break;
		case Sandbox::ETerrainKind::Analytic:
		{
			const char* analyticPresets[] =
			{
				Sandbox::AnalyticPresetName(Sandbox::EAnalyticPreset::Plane),
				Sandbox::AnalyticPresetName(Sandbox::EAnalyticPreset::SineWaves),
				Sandbox::AnalyticPresetName(Sandbox::EAnalyticPreset::Slope),
				Sandbox::AnalyticPresetName(Sandbox::EAnalyticPreset::Crater),
				Sandbox::AnalyticPresetName(Sandbox::EAnalyticPreset::Steps),
			};
			ImGui::Combo("Preset", &ui.AnalyticPreset, analyticPresets, IM_ARRAYSIZE(analyticPresets));
			ImGui::DragFloat("Size", &ui.Preset.Terrain.Size, 1.f, 16.f, 4096.f, "%.0f m");
			ImGui::DragFloat("Cell", &ui.Preset.Terrain.Cell, 0.05f, 0.25f, 16.f, "%.2f m");
			ImGui::DragFloat("Amplitude", &ui.Preset.Terrain.Amplitude, 0.1f, 0.f, 100.f, "%.1f m");
			ImGui::DragFloat("Frequency", &ui.Preset.Terrain.Frequency, 0.001f, 0.001f, 1.f, "%.3f");
			break;
		}
		case Sandbox::ETerrainKind::Heightmap:
			ImGui::InputText("Heightmap", ui.HeightmapPath, sizeof(ui.HeightmapPath));
			ImGui::DragFloat("Height scale", &ui.Preset.Terrain.HeightScale, 0.1f, 0.f, 100.f, "%.1f");
			ImGui::DragFloat("Size", &ui.Preset.Terrain.Size, 1.f, 16.f, 4096.f, "%.0f m");
			ImGui::DragFloat("Cell", &ui.Preset.Terrain.Cell, 0.05f, 0.25f, 16.f, "%.2f m");
			break;
		case Sandbox::ETerrainKind::PrebuiltLevel:
			ImGui::InputText("Level folder", ui.PrebuiltLevel, sizeof(ui.PrebuiltLevel));
			ImGui::TextDisabled("Existing folder under $game_levels$ (e.g. l01_escape); no data is generated.");
			break;
		}

		ImGui::EndDisabled();
	}

	void DrawResourceMaskEditor(SViewerWindowState& ui, bool sessionBusy)
	{
		if (!ImGui::CollapsingHeader("Startup resources (next session)", ImGuiTreeNodeFlags_DefaultOpen))
			return;

		ImGui::TextDisabled("Editable while no session is starting or active. Use Live toggles for the running level.");
		ImGui::BeginDisabled(sessionBusy);

		if (ImGui::Button("All"))
			ui.Preset.Resources.assign(u32(ELevelResource::All));
		ImGui::SameLine();
		if (ImGui::Button("None"))
			ui.Preset.Resources.zero();
		ImGui::SameLine();
		if (ImGui::Button("Recommended"))
		{
			// Procedural sandbox: everything but the renderer scene graph and
			// the HUD. Prebuilt levels get the full stock set.
			ui.Preset.Resources.assign(u32(ELevelResource::All));
			if (ui.TerrainKind != int(Sandbox::ETerrainKind::PrebuiltLevel))
			{
				ui.Preset.Resources.set(u32(ELevelResource::RenderScene), false);
				ui.Preset.Resources.set(u32(ELevelResource::Hud), false);
			}
		}

		ImGui::TextDisabled("Mask: %s", LevelResourceMaskToString(ui.Preset.Resources).c_str());

		for (u32 bit = 0; bit < IGame_Level::ResourceStateCount; ++bit)
		{
			const ELevelResource res = ELevelResource(1u << bit);
			ImGui::PushID(int(bit));
			ImGui::CheckboxFlags(LevelResourceName(res).data(), &ui.Preset.Resources.flags, u32(res));
			ImGui::PopID();
		}

		ImGui::EndDisabled();
	}

	void DrawSessionControls(CViewerSession& session, SViewerWindowState& ui)
	{
		ImGui::SeparatorText("Session");

		const SViewerState& state = session.GetState();
		const bool starting = state.Session == EViewerSessionState::Starting;
		const bool active = session.IsActive();
		const bool busy = starting || active;

		ImGui::Text("Editor: %s", session.Mode() != nullptr ? session.Mode()->Name() : "Level Sandbox");

		if (active)
			ImGui::TextColored(ImVec4(0.35f, 0.95f, 0.35f, 1.f), "Active: %s", state.LevelFolder);
		else if (starting)
			ImGui::TextColored(ImVec4(0.95f, 0.80f, 0.20f, 1.f), "Starting: %s ...", state.LevelFolder);
		else
			ImGui::TextDisabled("No active level in session %u", session.Index());

		ImGui::BeginDisabled(busy);
		if (ui.Preset.EditorKind == EViewerEditorKind::LevelSandbox)
		{
			ImGui::Checkbox("Spawn actor on start", &ui.Preset.SpawnActor);
			ImGui::Checkbox("HUD, inventory and weapon UI", &ui.Preset.SpawnActorWithHud);
			ImGui::Checkbox("Arm actor (weapon in hands)", &ui.Preset.ArmActor);
		}
		if (ImGui::Button("Start"))
		{
			ui.SyncToPreset();
			SViewerRequest start;
			start.Type = EViewerRequest::StartSession;
			start.Preset = ui.Preset;
			session.PushRequest(start);
		}
		ImGui::EndDisabled();

		ImGui::SameLine();

		ImGui::BeginDisabled(!busy);
		if (ImGui::Button("Stop"))
		{
			SViewerRequest stop;
			stop.Type = EViewerRequest::StopSession;
			session.PushRequest(stop);
		}
		ImGui::EndDisabled();

		if (active)
			ImGui::Text("Compiled scene: %s", state.HasRenderScene ? "loaded" : "absent (runtime terrain allowed)");
	}

	void DrawResourceStates(CViewerSession& session)
	{
		if (!session.IsActive())
			return;
		if (!ImGui::CollapsingHeader("Resource states", ImGuiTreeNodeFlags_DefaultOpen))
			return;

		ImGui::TextDisabled("not_loaded: not requested or unloaded; loading: work in progress; ready: usable.\nmissing: requested but absent, fallback active; failed: present but invalid, fallback active.\nrender_scene is the compiled level geometry/sectors/HOM, not the analytical terrain fallback.");
		const SViewerState& state = session.GetState();
		for (u32 bit = 0; bit < IGame_Level::ResourceStateCount; ++bit)
		{
			const ELevelResource res = ELevelResource(1u << bit);
			const EResourceState resState = state.ResourceStates[bit];
			ImGui::TextColored(ResourceStateColor(resState), "%-18s %s",
				LevelResourceName(res).data(), LevelResourceStateName(resState).data());
		}
	}

	void DrawLiveToggles(CViewerSession& session)
	{
		if (!session.IsActive())
			return;
		if (!ImGui::CollapsingHeader("Live resource load / unload", ImGuiTreeNodeFlags_DefaultOpen))
			return;

		auto resourceToggle = [&session](ELevelResource resource, const char* label)
		{
			bool enabled = session.ResourceState(resource) == EResourceState::Ready;
			if (ImGui::Checkbox(label, &enabled))
			{
				SViewerRequest request;
				request.Type = EViewerRequest::SetResource;
				request.IntValue = IGame_Level::ResourceStateIndex(resource);
				request.BoolValue = enabled;
				session.PushRequest(request);
			}
		};
		constexpr ELevelResource resources[] =
		{
			ELevelResource::LevelConfig,
			ELevelResource::RenderScene,
			ELevelResource::Collision,
			ELevelResource::AiMap,
			ELevelResource::GameGraph,
			ELevelResource::CrossTable,
			ELevelResource::PatrolPaths,
			ELevelResource::Spawn,
			ELevelResource::StaticParticles,
			ELevelResource::StaticSounds,
			ELevelResource::SoundEnv,
			ELevelResource::Ambience,
			ELevelResource::EnvModifiers,
			ELevelResource::PhysicsWorld,
			ELevelResource::LevelScripts,
			ELevelResource::Hud,
		};
		for (const ELevelResource resource : resources)
			resourceToggle(resource, LevelResourceName(resource).data());
		ImGui::TextDisabled("Missing data degrades without crashing. Collision loss suspends physics.\nResource dependencies and unavailable replacements are reported above.");
	}

	void DrawFreeCameraControls(CViewerSession& session)
	{
		if (!ImGui::CollapsingHeader("Free camera", ImGuiTreeNodeFlags_DefaultOpen))
			return;

		SViewerState& controls = session.ViewerState;
		ImGui::Checkbox("Enabled", &controls.FreeCamActive);
		ImGui::SliderFloat("Speed", &controls.CamSpeed, 1.f, 100.f, "%.1f m/s");
		ImGui::Text("Position: %.1f  %.1f  %.1f", controls.CamPos.x, controls.CamPos.y, controls.CamPos.z);
		ImGui::TextDisabled("WASD move, Q/E down/up, arrows or RMB-drag to look, Shift = boost.");
	}

	void DrawLightingControls(CViewerSession& session)
	{
		if (!ImGui::CollapsingHeader("Lighting", ImGuiTreeNodeFlags_DefaultOpen))
			return;

		// Deferred rendering relights from the environment descriptor every
		// frame: moving a slider is visible on the next frame, no rebuild step.
		SViewerState& controls = session.ViewerState;
		constexpr const char* cycles[] = { "default_clear", "default_cloudy", "default_rain", "default_thunder" };
		ImGui::BeginDisabled(!session.IsActive());
		if (ImGui::BeginCombo("Weather cycle", controls.WeatherCycle))
		{
			for (const char* cycle : cycles)
			{
				if (ImGui::Selectable(cycle, xr_strcmp(controls.WeatherCycle, cycle) == 0))
				{
					SViewerRequest request;
					request.Type = EViewerRequest::SetWeatherCycle;
					xr_strcpy(request.Section, cycle);
					session.PushRequest(request);
				}
			}
			ImGui::EndCombo();
		}
		ImGui::EndDisabled();
		ImGui::Checkbox("Override session lighting", &controls.LightingOverride);
		ImGui::BeginDisabled(!controls.LightingOverride);
		ImGui::SliderFloat("Sun azimuth", &controls.SunAzimuthDeg, 0.f, 360.f, "%.0f deg");
		ImGui::SliderFloat("Sun elevation", &controls.SunElevationDeg, -90.f, 0.f, "%.0f deg");
		ImGui::SliderFloat("Sun intensity", &controls.SunIntensity, 0.f, 3.f, "%.2f");
		ImGui::SliderFloat("Ambient intensity", &controls.AmbientIntensity, 0.f, 3.f, "%.2f");
		ImGui::SliderFloat("Fog density", &controls.FogDensity, 0.f, 1.f, "%.2f");
		ImGui::EndDisabled();
		ImGui::TextDisabled("Applied to the parallel pass only; the primary session's\nweather is restored right after the pass.");
	}

	void DrawOverlayControls(CViewerSession& session)
	{
		if (!ImGui::CollapsingHeader("Level Inspector (session)", ImGuiTreeNodeFlags_DefaultOpen))
			return;

		SViewerState& controls = session.ViewerState;
		auto applyWireframe = [&]
		{
			SViewerRequest request;
			request.Type = EViewerRequest::SetWireframe;
			request.IntValue = controls.WireframeMode;
			request.HeightValue = controls.WireframeObjectId;
			session.PushRequest(request);
		};
		if (ImGui::Combo("Geometry wireframe", &controls.WireframeMode, "Off\0Solid + wireframe\0Wireframe only\0"))
			applyWireframe();
		if (ImGui::InputInt("Object ID (-1 all, -2 terrain, -3 static)", &controls.WireframeObjectId))
			applyWireframe();
		if (ImGui::Button("Use actor aim target"))
		{
			const CActor* actor = session.PlayerActor();
			const CGameObject* target = actor != nullptr ? actor->ObjectWeLookingAt() : nullptr;
			controls.WireframeObjectId = target != nullptr ? int(target->ID()) : -1;
			applyWireframe();
		}
		auto scene = [&](const char* label, u32 flag)
		{
			ImGui::CheckboxFlags(label, &controls.InspectorSceneFlags.flags, flag);
		};
		auto zone = [&](const char* label, u32 flag)
		{
			ImGui::CheckboxFlags(label, &controls.InspectorZoneFlags.flags, flag);
		};
		ImGui::TextDisabled("Stock world-space rendering; independent from the main game's Level Inspector.");
		scene("AI paths", LevelInspector::ESF_DRAW_AI_PATHS);
		scene("Game graph", LevelInspector::ESF_DRAW_G_GRID);
		scene("AI-map nodes", LevelInspector::ESF_DRAW_L_GRID);
		scene("Patrol paths / waypoints", LevelInspector::ESF_DRAW_W_GRID);
		if (controls.InspectorSceneFlags.test(LevelInspector::ESF_DRAW_W_GRID))
		{
			ImGui::Combo("Waypoint filter", &controls.InspectorWaypointMode, "None\0Path prefix\0Current level\0All\0");
			if (controls.InspectorWaypointMode == int(LevelInspector::EWAYPOINT_INFO::EWI_PREFIX))
				ImGui::InputText("Waypoint prefix", controls.InspectorWaypointPrefix, sizeof(controls.InspectorWaypointPrefix));
		}

		if (ImGui::CollapsingHeader("Zones and objects"))
		{
			scene("Objects", LevelInspector::ESF_DRAW_OBJECTS);
			scene("Zones", LevelInspector::ESF_DRAW_ZONES);
			zone("Restrictors", LevelInspector::EZI_RESTR);
			zone("Smart terrains", LevelInspector::EZI_SMART_TERRAIN);
			zone("Level changers", LevelInspector::EZI_LEVEL_CHANGER);
			zone("Smart covers", LevelInspector::EZI_SMART_COVER);
			zone("Camp zones", LevelInspector::EZI_CAMP_ZONE);
			zone("Anomaly zones", LevelInspector::EZI_ANOMALY_ZONE);
			zone("Anomaly logic", LevelInspector::EZI_ANOMAL_ZONE_LOGIC);
			zone("Simulation factions", LevelInspector::EZI_SIM_FACTION);
			ImGui::CheckboxFlags("Skeleton bones", &controls.InspectorSkeletonFlags.flags, LevelInspector::ESI_BONES);
			ImGui::CheckboxFlags("Bone links", &controls.InspectorSkeletonFlags.flags, LevelInspector::ESI_BONES_LINKS);
			ImGui::CheckboxFlags("Bone shapes", &controls.InspectorSkeletonFlags.flags, LevelInspector::ESI_BONES_SHAPES);
			ImGui::CheckboxFlags("Bone vertices", &controls.InspectorSkeletonFlags.flags, LevelInspector::ESI_BONES_VERTS);
			ImGui::CheckboxFlags("Physics boxes", &controls.InspectorSkeletonFlags.flags, LevelInspector::ESI_PH_BBOX);
			ImGui::CheckboxFlags("Actor physics", &controls.InspectorSkeletonFlags.flags, LevelInspector::ESI_ACTOR);
			scene("HUD-space primitives", LevelInspector::ESF_DRAW_HUD);
		}

		if (ImGui::CollapsingHeader("Collision and spatial data"))
		{
			scene("SOM", LevelInspector::ESF_DRAW_SOM);
			scene("HOM", LevelInspector::ESF_DRAW_HOM);
			scene("CForm boxes", LevelInspector::ESF_DRAW_CFORM);
			scene("CForm triangles", LevelInspector::ESF_DRAW_CFORM_TRIS);
			scene("All CForm boxes", LevelInspector::ESF_DRAW_CFORM_ALL);
			scene("Level bounds", LevelInspector::ESF_DRAW_LEVEL_BOUNDS);
			scene("Registered spatials", LevelInspector::ESF_DRAW_SPATIAL_SPACE);
			scene("Unregistered spatials", LevelInspector::ESF_DRAW_SPATIAL_SPACE_ALL);
			bool spatials = controls.InspectorSpatialMask != 0;
			if (ImGui::Checkbox("Object spatial mask", &spatials))
				controls.InspectorSpatialMask = spatials ? ~0ull : 0ull;
		}

		if (ImGui::CollapsingHeader("Selection"))
		{
			scene("Draw selected", LevelInspector::ESF_DRAW_SELECTION);
			ImGui::CheckboxFlags("Objects", &controls.InspectorSelectionFlags.flags, LevelInspector::ESLF_O);
			ImGui::CheckboxFlags("Zones", &controls.InspectorSelectionFlags.flags, LevelInspector::ESLF_Z);
			ImGui::CheckboxFlags("Waypoints", &controls.InspectorSelectionFlags.flags, LevelInspector::ESLF_WP);
			ImGui::CheckboxFlags("Graph points", &controls.InspectorSelectionFlags.flags, LevelInspector::ESLF_GP);
			ImGui::CheckboxFlags("AI nodes", &controls.InspectorSelectionFlags.flags, LevelInspector::ESLF_LG);
			ImGui::CheckboxFlags("Name", &controls.InspectorTextFlags.flags, LevelInspector::EOI_LNAME);
			ImGui::CheckboxFlags("Section", &controls.InspectorTextFlags.flags, LevelInspector::EOI_SNAME);
			ImGui::CheckboxFlags("Visual", &controls.InspectorTextFlags.flags, LevelInspector::EOI_VNAME);
			ImGui::CheckboxFlags("Class", &controls.InspectorTextFlags.flags, LevelInspector::EOI_LCNAME);
			ImGui::CheckboxFlags("Position", &controls.InspectorTextFlags.flags, LevelInspector::EOI_POSITION);
			ImGui::CheckboxFlags("Script", &controls.InspectorTextFlags.flags, LevelInspector::EOI_SCRIPT);
			ImGui::CheckboxFlags("Actor", &controls.InspectorTextFlags.flags, LevelInspector::EOI_ACTOR);
			ImGui::CheckboxFlags("Graph and level vertices", &controls.InspectorTextFlags.flags, LevelInspector::EOI_GVERTEX_LVERTEX);
			ImGui::CheckboxFlags("Custom data", &controls.InspectorTextFlags.flags, LevelInspector::EOI_INI);
			ImGui::CheckboxFlags("Inventory", &controls.InspectorTextFlags.flags, LevelInspector::EOI_INVENTORY);
		}
		ImGui::Checkbox("Depth test", &controls.InspectorZBuffer);
		ImGui::Checkbox("Show current-level data", &controls.InspectorVisibleCurrents);
	}

	void DrawModeSection(CViewerSession& session)
	{
		if (session.Mode() != nullptr)
			session.Mode()->RenderUI(session);
	}

	void DrawGpuPoolStats(CViewerSession& session)
	{
		if (!ImGui::CollapsingHeader("GPU pool"))
			return;

		if (!session.GpuPool.IsReady())
		{
			ImGui::TextDisabled("Pool not initialized (no session).");
			return;
		}

		const auto bar = [](const char* label, const COffsetAllocator::SStats& stats)
		{
			const float fraction = stats.Capacity ? float(stats.Used) / float(stats.Capacity) : 0.f;
			ImGui::Text("%s: %.2f / %.2f MB (%u allocs, %u free blocks)",
				label,
				float(stats.Used) / 1048576.f, float(stats.Capacity) / 1048576.f,
				stats.AllocationCount, stats.FreeBlockCount);
			ImGui::ProgressBar(fraction, ImVec2(-1.f, 0.f));
		};
		bar("Vertices", session.GpuPool.VertexStats());
		bar("Indices", session.GpuPool.IndexStats());
		bar("Constants", session.GpuPool.ConstantStats());
		ImGui::TextDisabled("Budget: %.2f MB (viewer_gpu_budget_kb)", float(session.GpuPool.BudgetBytes()) / 1048576.f);
	}
}

namespace ViewerWindowDetail
{
	struct SEditorWindowBinding
	{
		EViewerEditorKind Kind;
		EditorUI Slot;
		const char* Name;
	};

	constexpr SEditorWindowBinding EditorWindows[] =
	{
		{ EViewerEditorKind::LevelSandbox, EditorUI::Tools_LevelViewer, "sandbox" },
		{ EViewerEditorKind::OgfPreview, EditorUI::Tools_OgfViewer, "ogf" },
		{ EViewerEditorKind::ParticleEditor, EditorUI::Tools_ParticleEditor, "particle" },
		{ EViewerEditorKind::AiEditor, EditorUI::Tools_AiEditor, "ai" },
		{ EViewerEditorKind::MaterialEditor, EditorUI::Tools_MaterialEditor, "material" },
	};

	bool& EditorWindowVisible(EViewerEditorKind kind)
	{
		return Engine.External.EditorStates[u32(EditorWindows[u32(kind)].Slot)];
	}

	void RequestEditorStart(CViewerSession& session, EViewerEditorKind kind)
	{
		if (session.ViewerState.RequestedEditorKind == kind && session.State() != EViewerSessionState::Idle)
			return;
		for (const SEditorWindowBinding& binding : EditorWindows)
		{
			if (binding.Kind == kind)
				continue;
			EditorWindowVisible(binding.Kind) = false;
			ViewerSessionDetail::Manager->WindowsVisible[u32(binding.Kind)] = false;
		}
		SViewerWindowState& ui = WindowState(session);
		if (ui.Preset.EditorKind != kind)
		{
			const SViewerPreset defaults;
			ui.Preset.Resources = defaults.Resources;
			ui.Preset.SpawnActor = false;
			ui.Preset.StartAlife = false;
			ui.Preset.Role = EViewerRole::Standalone;
			ui.Preset.RemoteServer = nullptr;
		}
		ui.Preset.EditorKind = kind;
		ui.SyncToPreset();
		SViewerRequest request;
		request.Type = EViewerRequest::StartSession;
		request.Preset = ui.Preset;
		session.ViewerState.RequestedEditorKind = kind;
		session.PushRequest(request);
	}

	class CCC_ViewerEditor : public IConsole_Command
	{
	public:
		CCC_ViewerEditor(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			string32 kind = {};
			string32 state = {};
			if (args == nullptr || sscanf(args, "%31s %31s", kind, state) < 1)
			{
				Msg("! viewer_editor <sandbox|ogf|particle|ai|material> [on|off]");
				return;
			}
			for (const SEditorWindowBinding& binding : EditorWindows)
			{
				if (0 != _stricmp(kind, binding.Name))
					continue;
				bool& visible = EditorWindowVisible(binding.Kind);
				visible = state[0] == 0 || 0 == _stricmp(state, "on") || 0 == xr_strcmp(state, "1");
				if (visible)
					RequestEditorStart(EnsureViewerSession(0), binding.Kind);
				return;
			}
			Msg("! viewer_editor: unknown editor '%s'", kind);
		}
	};

	class CCC_ViewerEditorCommand : public IConsole_Command
	{
	public:
		CCC_ViewerEditorCommand(const char* N) : IConsole_Command(N) {}
		void Execute(const char* args) override
		{
			string32 command = {};
			string_path argument = {};
			float value = 0.f;
			if (args == nullptr || sscanf(args, "%31s %255[^\n]", command, argument) < 1)
			{
				Msg("! viewer_editor_command <load|apply|undo|redo|history|memory|play|stop|save|rotate|reset> [argument] [value]");
				return;
			}
			char* number = strrchr(argument, ' ');
			if (0 != _stricmp(command, "memory") && number != nullptr && (isdigit(u8(number[1])) || number[1] == '-' || number[1] == '+'))
			{
				value = float(atof(number + 1));
				number[0] = 0;
			}
			EViewerEditorCommand type = EViewerEditorCommand::SetParameter;
			if (0 == _stricmp(command, "load")) type = EViewerEditorCommand::Load;
			else if (0 == _stricmp(command, "undo")) type = EViewerEditorCommand::Undo;
			else if (0 == _stricmp(command, "redo")) type = EViewerEditorCommand::Redo;
			else if (0 == _stricmp(command, "history")) type = EViewerEditorCommand::TestHistory;
			else if (0 == _stricmp(command, "memory")) type = EViewerEditorCommand::ApplyMemory;
			else if (0 == _stricmp(command, "play")) type = EViewerEditorCommand::Play;
			else if (0 == _stricmp(command, "stop")) type = EViewerEditorCommand::Stop;
			else if (0 == _stricmp(command, "save")) type = EViewerEditorCommand::Save;
			else if (0 == _stricmp(command, "rotate")) type = EViewerEditorCommand::Rotate;
			else if (0 == _stricmp(command, "reset")) type = EViewerEditorCommand::Reset;
			else if (0 == _stricmp(command, "rate") || 0 == _stricmp(command, "destination") || 0 == _stricmp(command, "scale"))
			{
				value = float(atof(argument));
				xr_strcpy(argument, command);
			}
			EnsureViewerSession().RequestEditorCommand(type, argument, value);
		}
	};
}

bool ViewerWindowIsVisible()
{
	return ViewerWindowDetail::ViewerWindowVisible() || ViewerSecondWindowVisible();
}

void RegisterViewerConsoleCommand()
{
	// CMD1 pastes plain identifiers; pull the command classes into scope.
	using namespace ViewerWindowDetail;

	// Boot must be identical to stock: never honor a persisted open state -
	// the viewer opens only when the user clicks the menu item (that click is
	// the open edge that auto-starts the session).
	ViewerWindowVisible() = false;

	CMD1(CCC_LevelViewer, "level_viewer");
	CMD1(CCC_ViewerSpawn, "viewer_spawn");
	CMD1(CCC_ViewerSpawnActor, "viewer_spawn_actor");
	CMD1(CCC_ViewerHud, "viewer_hud");
	CMD1(CCC_ViewerTestHud, "viewer_test_hud");
	CMD1(CCC_ViewerInventory, "viewer_inventory");
	CMD1(CCC_ViewerViewportSize, "viewer_viewport_size");
	CMD1(CCC_ViewerWindowSize, "viewer_window_size");
	CMD1(CCC_ViewerTestSpawns, "viewer_test_spawns");
	CMD1(CCC_ViewerTestActor, "viewer_test_actor");
	CMD1(CCC_ViewerTestPatrol, "viewer_test_patrol");
	CMD1(CCC_ViewerPrimaryContinue, "viewer_primary_continue");
	CMD1(CCC_ViewerPrimaryTest, "viewer_primary_test");
	CMD1(CCC_ViewerLifecycleTest, "viewer_lifecycle_test");
	CMD1(CCC_ViewerInspector, "viewer_inspector");
	CMD1(CCC_ViewerWireframe, "viewer_wireframe");
	CMD1(CCC_ViewerPause, "viewer_pause");
	CMD1(CCC_ViewerPresentation, "viewer_presentation");
	CMD1(CCC_ViewerPrimaryProbe, "viewer_primary_probe");
	CMD1(CCC_ViewerWander, "viewer_wander");
	CMD1(CCC_ViewerMode, "viewer_mode");
	CMD1(CCC_ViewerShot, "viewer_shot");
	CMD1(CCC_ViewerSwapDump, "viewer_swapdump");
	CMD1(CCC_ViewerRtVerify, "viewer_rt_verify");
	CMD1(CCC_ViewerDumpTargets, "viewer_dumptargets");
	CMD1(CCC_ViewerGroundDump, "viewer_ground_dump");
	CMD1(CCC_ViewerResource, "viewer_resource");
	CMD1(CCC_ViewerResourceTest, "viewer_resource_test");
	CMD1(CCC_ViewerRenderPass, "viewer_render_pass");
	CMD1(CCC_ViewerWeather, "viewer_weather");
	CMD1(CCC_SandboxStart, "sandbox_start");
	CMD1(CCC_SandboxStartAlife, "sandbox_start_alife");
	CMD1(CCC_SandboxStop, "sandbox_stop");
	CMD1(CCC_ViewerSession, "viewer_session");
	CMD1(CCC_ViewerPairStart, "viewer_pair_start");
	CMD1(CCC_ViewerPairStop, "viewer_pair_stop");
	CMD1(CCC_ViewerPairVerify, "viewer_pair_verify");
	CMD1(CCC_ViewerEditor, "viewer_editor");
	CMD1(CCC_ViewerEditorCommand, "viewer_editor_command");
	for (const SEditorWindowBinding& binding : EditorWindows)
		EditorWindowVisible(binding.Kind) = false;
	RegisterViewerCvars();
	RegisterViewerCrashLogger();

	// The pump exists from init but never constructs the session by itself:
	// the session appears on the first window open / console command / autorun.
	SetViewerSessionManager(SessionTicker.Manager);
	Device.seqFrame.Add(&SessionTicker);
	Device.seqRender.Add(&SessionTicker, REG_PRIORITY_HIGH);
	Device.seqAppEnd.Add(&SessionTicker, REG_PRIORITY_HIGH);
}

namespace ViewerWindowDetail
{
	void DrawEditorWindow(CViewerSession& session, bool& open, const char* title)
	{
		const float ViewportSide = float(session.ViewportDimension());
		ImGui::SetNextWindowSizeConstraints(ImVec2(640.f, 400.f), ImVec2(FLT_MAX, FLT_MAX));
		ImGui::SetNextWindowSize(ImVec2(std::max(1180.f, ViewportSide + 440.f), std::max(720.f, ViewportSide + 180.f)), ImGuiCond_FirstUseEver);
		ImGui::SetNextWindowPos(ImVec2(40.f + session.Index() * 40.f, 40.f + session.Index() * 40.f), ImGuiCond_FirstUseEver);
		if (ImGui::Begin(title, &open))
		{
			ImGui::PushID(int(session.Index()));
			if (ImGui::BeginTable("LevelViewerLayout", 2, ImGuiTableFlags_Resizable | ImGuiTableFlags_BordersInnerV, ImVec2(0, ImGui::GetContentRegionAvail().y)))
			{
				ImGui::TableSetupColumn("Scene", ImGuiTableColumnFlags_WidthStretch);
				ImGui::TableSetupColumn("Controls", ImGuiTableColumnFlags_WidthFixed | ImGuiTableColumnFlags_NoResize, 340.f);
				ImGui::TableNextRow();
				ImGui::TableSetColumnIndex(0);
				ViewerWindowDetail::DrawViewport(session);
				ImGui::TableSetColumnIndex(1);
				if (ImGui::BeginChild("ViewerControls", ImVec2(0, 0), false, ImGuiWindowFlags_AlwaysVerticalScrollbar))
				{
					if (ImGui::BeginTable("ViewerInspectorGroups", 1, ImGuiTableFlags_BordersInnerH))
					{
						SViewerWindowState& ui = ViewerWindowDetail::WindowState(session);
						ImGui::TableNextRow();
						ImGui::TableSetColumnIndex(0);
						ImGui::SeparatorText("Session and mode");
						ViewerWindowDetail::DrawSessionControls(session, ui);
						ViewerWindowDetail::DrawModeSection(session);
						ImGui::TableNextRow();
						ImGui::TableSetColumnIndex(0);
						ImGui::SeparatorText("Level preset and resources");
						ViewerWindowDetail::DrawTerrainEditor(ui, session.State() != EViewerSessionState::Idle);
						ViewerWindowDetail::DrawResourceMaskEditor(ui, session.State() != EViewerSessionState::Idle);
						ViewerWindowDetail::DrawResourceStates(session);
						ViewerWindowDetail::DrawLiveToggles(session);
						ImGui::TableNextRow();
						ImGui::TableSetColumnIndex(0);
						ImGui::SeparatorText("Camera and weather");
						ViewerWindowDetail::DrawFreeCameraControls(session);
						ViewerWindowDetail::DrawLightingControls(session);
						ImGui::TableNextRow();
						ImGui::TableSetColumnIndex(0);
						ImGui::SeparatorText("System data and GPU");
						ViewerWindowDetail::DrawOverlayControls(session);
						ViewerWindowDetail::DrawGpuPoolStats(session);
						ImGui::EndTable();
					}
				}
				ImGui::EndChild();
				ImGui::EndTable();
			}
			ImGui::PopID();
		}
		ImGui::End();
	}
}

void RenderViewerWindow()
{
	for (const ViewerWindowDetail::SEditorWindowBinding& binding : ViewerWindowDetail::EditorWindows)
	{
		bool& visible = ViewerWindowDetail::EditorWindowVisible(binding.Kind);
		CViewerSessionManager& manager = *ViewerSessionDetail::Manager;
		bool& wasVisible = manager.WindowsVisible[u32(binding.Kind)];
		if (visible != wasVisible)
		{
			wasVisible = visible;
			if (visible)
				ViewerWindowDetail::RequestEditorStart(EnsureViewerSession(0), binding.Kind);
			else if (CViewerSession* session = ViewerSessionAt(0); session != nullptr && session->ActivePreset().EditorKind == binding.Kind && session->State() != EViewerSessionState::Idle)
			{
				SViewerRequest stop;
				stop.Type = EViewerRequest::StopSession;
				session->PushRequest(stop);
			}
		}
		if (visible)
		{
			if (CViewerSession* session = ViewerSessionAt(0); session != nullptr && session->ActivePreset().EditorKind == binding.Kind)
			{
				string64 title;
				xr_sprintf(title, "%s##LevelViewer0", session->Mode() != nullptr ? session->Mode()->Name() : "Level Sandbox");
				ViewerWindowDetail::DrawEditorWindow(*session, visible, title);
			}
		}
	}

	bool& secondVisible = ViewerSecondWindowVisible();
	if (secondVisible)
	{
		if (CViewerSession* session = ViewerSessionAt(1))
		{
			string64 title;
			xr_sprintf(title, "%s##LevelViewer1", session->Mode() != nullptr ? session->Mode()->Name() : "Level Client");
			ViewerWindowDetail::DrawEditorWindow(*session, secondVisible, title);
			if (!secondVisible && session->State() != EViewerSessionState::Idle)
			{
				SViewerRequest stop;
				stop.Type = EViewerRequest::StopSession;
				session->PushRequest(stop);
			}
		}
	}
}
}
