#pragma once

// In-game "level viewer" session driver.
//
// Architecture rules for this folder (per project decision):
//  - Deferred init: level/session resources are created only when a viewer is
//    opened or a console command requests one. The ticker owns bounded session
//    storage; no level is constructed at library load.
//  - Command-based: every state-changing action (start, stop, spawn, toggles,
//    mode/preset selection) is an SViewerRequest pushed into a fixed-capacity
//    ring queue and executed by RequestHandler_LevelViewer on the primary
//    thread inside the session tick. UI and console never touch the session
//    directly. (Idea mirrors RequestHandler_TextureEditor.)
//  - One named state struct: SViewerState is the session's full externally
//    visible state; the window renders from it.
//  - No anonymous namespaces; named detail namespaces instead.
//  - Streaming over caching: per-frame data (overlay) lives in fixed-capacity
//    raw arrays that are refilled every frame (sliding window) - no growing
//    caches, no per-frame allocation.
//
// Up to two independent sessions are supported, including a server/client pair.

#include "../../../xrEngine/LevelResources.h"
#include "../../../xrEngine/XR_IOConsole.h"
#include "../../../xrEngine/pure.h"
#include "../../HUDManager.h"
#include "../../../xrEngine/Engine.h"
#include "../../../xrEngine/IGame_Actor.h"
#include "../../../xrEngine/IGame_Persistent.h"
#include "../../LevelInspector.h"
#include "../../Sandbox/SandboxTerrain.h"
#include "ViewerGpuPool.h"
#include "ViewerRenderer.h"
#include "../../Sandbox/EmbeddedLevelResources.h"
#include "ViewerEditor.h"

class CActor;
extern CActor* g_actor;
extern CActor* g_actor_single;
class CAI_Space;
class CPHWorld;
class xrServer;
class player_hud;
class CCustomHUD;
class IGame_CustomUI;
class ISpatial_DB;
struct IGame_ObjectFactory;

namespace ImUtils::Viewer
{
	enum class EViewerSessionState : u8
	{
		Idle,       // no session, no pending start
		Starting,   // KERNEL:start deferred, level not up yet
		Active,     // our level object exists and name matches
	};

	enum class EViewerRole : u8
	{
		Standalone,
		Server,
		Client,
	};

	struct SViewerPreset
	{
		Sandbox::STerrainParams Terrain;
		Flags32                 Resources;
		bool                    SpawnActor = false;
		bool                    SpawnActorWithHud = true;
		bool                    ArmActor = true;
		bool                    StartAlife = false;
		EViewerRole             Role = EViewerRole::Standalone;
		xrServer*               RemoteServer = nullptr;
		xr_string               AlifeSpawnFile = "all";
		xr_string               WeatherCycle = "default_clear";
		EViewerEditorKind       EditorKind = EViewerEditorKind::LevelSandbox;
		SViewerMemoryConfig     Memory;

		SViewerPreset();
	};

	struct SViewerWindowState
	{
		SViewerPreset Preset;
		int TerrainKind = 0;
		int AnalyticPreset = 0;
		int PresetIndex = 0;
		int ModeIndex = 1;
		char HeightmapPath[256] = {};
		char PrebuiltLevel[64] = {};

		void SyncToPreset()
		{
			Preset.Terrain.Kind = Sandbox::ETerrainKind(TerrainKind);
			Preset.Terrain.Analytic = Sandbox::EAnalyticPreset(AnalyticPreset);
			Preset.Terrain.HeightmapPath = HeightmapPath;
			Preset.Terrain.PrebuiltLevel = PrebuiltLevel;
		}
	};

	// ------------------------------------------------------------------------
	// Requests (the command channel)
	// ------------------------------------------------------------------------

	enum class EViewerRequest : u8
	{
		None,
		StartSession,     // Preset
		StopSession,
		SpawnSection,     // Section
		SpawnActor,
		SetPatrolPaths,   // BoolValue
		SetResource,      // IntValue - ELevelResource bit index; BoolValue - load or unload
		SetRenderPass,    // Section - pass name; BoolValue - enabled
		SetMode,          // IntValue
		SetWeatherCycle,  // Section - CEnvironment weather-cycle name
		SetWander,        // BoolValue - NPC wander driver (Gameplay mode)
		Screenshot,       // IntValue - take a screenshot this many frames after activation
		VerifyRender,     // Section - RT readback coverage check + BMP dump path
		RunSpawnTest,
		VerifyPatrolPaths,  // BoolValue - expected registry availability
		TestActorMovement,
		DumpTerrainTexture,
		DumpSceneTargets,
		SetViewportSize,  // IntValue / HeightValue - pixel size; legacy 0: one-third, 1: one-half square
		SetWindowSize,    // IntValue / HeightValue - requested ImGui window dimensions
		SetHudEnabled,    // BoolValue - enable stock HUD and actor UI on spawn
		VerifyHud,        // Verify stock weapon and inventory UI
		ToggleInventory,  // HeightValue - optional delay in session ticks
		DumpSwapchain,    // Section - BMP name; HeightValue - optional delay in session ticks
		SetWireframe,     // IntValue - 0: off, 1: solid+wire, 2: wire-only; HeightValue - object id or -1 for all
		PrimaryTest,      // Section - native primary-game validation operation
		SetInspector,     // Section - inspector category; BoolValue - enabled
		SetSimulationPaused,
		SetPresentationEnabled,
		RunResourceMatrix,
		EditorCommand,
	};

	struct SViewerRequest
	{
		EViewerRequest Type = EViewerRequest::None;
		SViewerPreset  Preset;                 // StartSession
		char           Section[64] = {};       // SpawnSection
		string_path    Path = {};              // Editor asset or parameter
		int            IntValue = 0;           // SetMode / SetViewportSize / window width / editor command
		int            HeightValue = 0;        // SetWindowSize
		bool           BoolValue = false;      // SetPatrolPaths
		float          FloatValue = 0.f;
	};

	// Fixed-capacity ring (sliding window): producers (window, console commands)
	// push, the session tick drains. No heap, no reallocation, no locking -
	// all producers and the consumer run on the primary thread.
	class CViewerRequestQueue
	{
	public:
		static constexpr u32 Capacity = 16;

		// false when full (the request is dropped and logged).
		bool Push(const SViewerRequest& request);
		// Hands queued requests to the handler until empty; returns how many.
		template <typename F>
		u32 Drain(F&& handler)
		{
			u32 drained = 0;
			u32 todo = Count; // snapshot: a handler re-pushing runs it next tick
			while (todo != 0 && Count != 0)
			{
				--todo;
				SViewerRequest request = Buffer[Head];
				Head = (Head + 1) % Capacity;
				--Count;
				++drained;
				handler(request);
			}
			return drained;
		}
		u32 Pending() const { return Count; }
		void Clear()
		{
			for (SViewerRequest& request : Buffer)
				request = {};
			Head = 0;
			Tail = 0;
			Count = 0;
		}

	private:
		SViewerRequest Buffer[Capacity] = {};
		u32 Head = 0;
		u32 Tail = 0;
		u32 Count = 0;
	};

	// ------------------------------------------------------------------------
	// State snapshot (the named state struct the window renders from)
	// ------------------------------------------------------------------------

	struct SViewerState
	{
		EViewerSessionState Session = EViewerSessionState::Idle;
		EViewerEditorKind RequestedEditorKind = EViewerEditorKind::LevelSandbox;
		bool    Primary = false;          // stock KERNEL:start chain owns the global slots
		bool    HasRenderScene = false;
		bool    HasActor = false;         // session-bound player actor exists
		bool    AiMapPresent = false;
		bool    AiSimulationActive = false;
		char    LevelFolder[64] = {};
		int     ModeIndex = 0;
		u32     ObjectCount = 0;
		int     LastSpawnedId = -1;
		bool    SimulationPaused = false;
		bool    PresentationEnabled = true;
		u64     SimulationTicks = 0;

		// Live controls (written by the window on widget change, read by Tick).
		Fvector CamPos = { 0.f, 30.f, 0.f };
		float   CamYaw = 0.f;
		float   CamPitch = -0.7f;
		float   CamSpeed = 10.f;
		bool    FreeCamActive = false;    // off: the viewport follows the actor when one exists; on: free camera
		bool    InputCapture = false;     // forward game input to the session level
		int     ViewportSize = 1;         // 0: one-third, 1: one-half, 2: fill the docked viewport
		u32     ViewportWidth = 600;
		u32     ViewportHeight = 600;

		// Live lighting controls: the parallel render pass applies them to the
		// environment's current descriptor before the scene render and restores
		// it after. The deferred pipeline relights from the descriptor every
		// frame, so a slider change is visible on the very next frame.
		bool    LightingOverride = true;
		float   SunAzimuthDeg = 135.f;    // sun_longitude
		float   SunElevationDeg = -45.f;  // sun_altitude - negative: the sun shines downward
		float   SunIntensity = 1.f;       // scales sun_color
		float   AmbientIntensity = 1.f;   // scales ambient + hemi_color
		float   FogDensity = 0.15f;       // viewer-only fog override; weather is preserved outside the pass
		char    WeatherCycle[32] = "default_clear";

		Flags32 RenderPassFlags;
		bool    RenderPassOverride = false;
		int     WireframeMode = 0;
		int     WireframeObjectId = -1;

		Flags32 InspectorSceneFlags;
		Flags32 InspectorZoneFlags;
		Flags32 InspectorSkeletonFlags;
		Flags32 InspectorSelectionFlags;
		Flags32 InspectorTextFlags;
		u64     InspectorSpatialMask = 0;
		int     InspectorWaypointMode = 2;
		char    InspectorWaypointPrefix[32] = {};
		bool    InspectorZBuffer = true;
		bool    InspectorVisibleCurrents = true;

		EResourceState ResourceStates[16] = {};
	};

	struct SViewerUiProbeState
	{
		bool Active = false;
		bool RectangleSeen = false;
		bool WindowSeen = false;
		bool Pressed = false;
		bool Activated = false;
		EViewerEditorKind Editor = EViewerEditorKind::LevelSandbox;
		char Control[64] = {};
		char Window[64] = {};
		Fvector2 Center = {};
	};

	struct SViewerLifecycleTestState
	{
		enum class EStage : u8
		{
			WaitViewer,
			StartPrimary,
			SkipIntro,
			EditorChecks,
			SaveLoad,
			WaitSave,
			PrimaryGameplay,
			ChangeLevel,
			SaveTransition,
			NextEditor,
		};

		EStage Stage = EStage::WaitViewer;
		u32 Ticks = 0;
		int EditorIndex = 0;
		EViewerEditorKind ExpectedEditor = EViewerEditorKind::LevelSandbox;
		bool ViewerFirst = false;
		bool SingleEditor = false;
		bool WaitObserved = false;
		bool Active = false;
		bool StartCommandSent = false;
		bool ActorRequested = false;
		bool EditorStarted = false;
		bool InitialEditorChecksDone = false;
		bool GameplayCompleted = false;
		bool TransitionReload = false;
		u32 CheckIndex = 0;
		u32 CheckWait = 0;
		u64 HistoryChangedId = 0;
		SViewerUiProbeState Ui;
		bool RestoreCapture = false;
		bool OtherEditorsPending = false;
		bool RestoreMenu = false;
		bool MenuWasActive = false;
		char SaveName[64] = {};
		char ExpectedLevel[64] = {};
	};

	class CViewerSessionTicker;
	class IViewerMode;

	class CViewerSession
	{
		friend class CLevelSlotGuard;
		friend void RenderParallelPass(CLevel* level);
		friend class CViewerSessionTicker;
		friend class CViewerSessionManager;
		friend class CViewerEditor;
	public:
		// All actions arrive as requests (pushed by the window/console commands),
		// drained here once per tick on the primary thread.
		void PushRequest(const SViewerRequest& request);
		void RequestEditorCommand(EViewerEditorCommand command, const char* argument = nullptr, float value = 0.f);
		u32 PendingRequests() const { return Requests.Pending(); }
		template <typename F>
		u32 DrainRequests(F&& handler) { return Requests.Drain(handler); }
		bool RetryRequestWhileUnavailable(const SViewerRequest& request, bool requireActor = false);
		// The command handler: executes one request. Public so console commands
		// that must run synchronously (autorun) can call it directly.
		void RequestHandler_LevelViewer(const SViewerRequest& request);

		// True once the session's level object exists, is ready, and name matches.
		bool HasLevel() const;
		bool IsActive() const; // fully started (Active state && HasLevel)
		void StartLifecycleTest(const char* args);
		void UiProbeStart(const char* control, const char* windowName = nullptr);
		void UiProbeObserve(const char* control, bool activated);
		bool UiProbeActive() const { return Lifecycle.Ui.Active; }
		EViewerSessionState State() const { return ViewerState.Session; }
		const SViewerState& GetState() const { return ViewerState; }
		const SViewerPreset& ActivePreset() const { return Preset; }
		u32 Index() const { return SessionIndex; }
		SViewerWindowState& GetWindowState() { return WindowState; }
		// The session's level object, whichever path drives it (nullptr when idle).
		CLevel* GetLevel() const { return ViewerState.Primary ? (CLevel*)g_pGameLevel : SessionLevel; }
		const CActor* PlayerActor() const { return SessionActor; }
		float ActorMovementDistance() const { return ActorMovementLastDistance; }
		bool ActorMovementPassed() const { return ActorMovementSucceeded; }
		bool ActorMovementInProgress() const { return ActorMovementTimeLeft >= 0.f; }
			IRender_interface* SceneRenderer() const { return SessionRender; }
			u32 ViewportDimension() const;
			float ViewportAspect() const { return float(ViewerState.ViewportWidth) / float(std::max(1u, ViewerState.ViewportHeight)); }
			void SetCameraFocus(const Fvector& focus, float distance, float pitch);

		// Status query: the session level's per-resource state when active.
		EResourceState ResourceState(ELevelResource res) const;

		// Projects a world point into full-screen pixel coordinates (session
		// camera for parallel sessions, device camera for primary ones).
		bool ProjectWorldPoint(const Fvector& world, Fvector2& out) const;

		// Viewer modes (see ViewerModes.h); mode switching is a request.
		IViewerMode* Mode();

		// Primary thread only: drains requests, pumps the session, refreshes the
		// state snapshot and the overlay stream.
		void Tick();
		// The actual tick body; Tick() wraps it with an SEH guard so a faulting
		// sandbox never takes the process down.
		void TickInternal();

		// Isolated, budget-capped GPU storage for viewer geometry (see ViewerGpuPool.h).
		CViewerGpuPool GpuPool;
		// The parallel-session viewport renderer: draws the session terrain
		// (pool buffers) into its own render target shown in the viewport.
		CViewerRenderer Renderer;

		SViewerState   ViewerState;

	private:
		// Request handlers (one per request type).
		void HandleStart(const SViewerPreset& preset);
		void HandleStop(bool cancelRequests = true);
		bool SpawnSectionNow(const char* section, ALife::_OBJECT_ID* outObjectId = nullptr);
		bool SpawnActorNow();
		bool SetHudEnabledNow(bool enabled);
		bool VerifyHudNow();
		bool ToggleInventoryNow(const char* state = nullptr);
		bool SetPatrolPathsNow(bool enabled);
		void VerifyPatrolPathsNow(bool expectedLoaded);
		bool SetResourceNow(ELevelResource resource, bool enabled);
		bool SetRenderPassNow(const char* name, bool enabled);
		bool SetWeatherCycleNow(const char* name);
		void RunSpawnTestNow();
		void BeginActorMovementTest(bool backwards = false);
		void UpdateActorMovementTest();
		void RunInContext(const xr_delegate<void()>& work);
		int RunResourceMatrixNow();

		void UpdateFreeCamera();
		void UploadTerrainToGpuPool();
		// The terrain mesh is also injected into the stock renderer as a runtime
		// terrain scene (one synthetic outdoor sector) so the deferred pass draws
		// it; released on session stop. No-op when none was created.
		void ReleaseRuntimeTerrainScene();
		void ReleaseSessionEnvironment();
			void ReleaseSessionRender();
		void UpdateGameInput();      // forwards game input to the session level
		void UpdateArmActor();       // pending weapon Take+Activate once both exist
		void PumpDeferredCmds();     // deferred autorun entries ("@<ticks> <cmd>")
		void RefreshStateSnapshot();
		void RunAutorunOnce();       // $app_data_root$/viewer_autorun.ltx, once
		void PrimaryTestNow(const char* args);
		void TickLifecycleTest();
		void UiProbeTick();
		void TickEditorChecks();
		void TickPrimaryGameplayChecks();
		void SetLifecycleStage(SViewerLifecycleTestState::EStage stage);
		void FinishLifecycleTest(bool passed, const char* reason);
		bool SetWireframeNow(u32 mode, u16 objectId);
		bool SetInspectorNow(const char* category, bool enabled);

		Fvector AimPointOnTerrain() const;
		SViewerLifecycleTestState Lifecycle;
		ALife::_OBJECT_ID PrimaryNpcId = ALife::INVALID_OBJECT_ID;
		ALife::_OBJECT_ID PrimaryAnomalyId = ALife::INVALID_OBJECT_ID;

		SViewerPreset       Preset;
		mutable xr_string   SessionFolder;
		CViewerRequestQueue Requests;
		xr_unique_ptr<IViewerMode> ModeInstance;
		bool                SessionReadyNotified = false;
		bool                CameraInitialized = false;
		bool                AutorunDone = false;
		bool                ActorSpawnAttempted = false;
		bool                SpawnTestPending = false; // queued RunSpawnTest
		CEnvironment*       SessionEnvironment = nullptr;
		int                 ScreenshotCountdown = -1; // >=0: fires 'screenshot' this many frames after activation
		u16                 PendingArmWeaponId = u16(-1); // weapon to Take+Activate once it exists client-side
		u32                 ArmWaitFrames = 0;
		float               ActorMovementTimeLeft = -1.f;
		float               ActorMovementLastDistance = -1.f;
		bool                ActorMovementSucceeded = false;
		Fvector             ActorMovementOrigin = {};
		u16                 ActorMovementKey = SDL_SCANCODE_W;
			u32 PreviousTickMs = 0;
			float TickDeltaSeconds = 0.f;

		bool CapturedKeys[SDL_SCANCODE_COUNT] = {};

			// Deferred console commands from viewer_autorun.ltx ("@<ticks> <cmd>"),
		// so a test can start a level first and open the viewer afterwards.
		struct SDeferredCmd { char Text[256]; u32 WaitTicks; };
		static constexpr u32 DeferredCapacity = 128;
		SDeferredCmd        DeferredCmds[DeferredCapacity] = {};
		u32                 DeferredCmdCount = 0;
		u32                 DeferredCmdHead = 0;
		class CLevel*       SessionLevel = nullptr;	// owned when parallel; mirrors g_pGameLevel when primary

		// Session-local camera transform for the parallel overlay projection
		// (Device camera matrices are owned by the primary session / menu).
		Fmatrix             ViewerCamFull = {};

		// Session-owned actor globals, lent to the real globals by
		// CLevelSlotGuard for the duration of each session scope (see the guard).
		CActor*             SessionActor = nullptr;
		CActor*             SessionActorSingle = nullptr;
		IGame_Actor*        SessionActorItf = nullptr;

		// Session-owned AI space and physics world (parallel sessions only):
		// the sandbox gets its own AI-map/graph/patrol/cover state and its own
		// ODE world instead of touching the primary session's globals.
		CAI_Space*          SessionAiSpace = nullptr;
		CPHWorld*           SessionPhysicsWorld = nullptr;
		// Session-owned player HUD object: load_default()ed at creation, weapon
		// hud sections load into it on demand through the actor's own logic.
		// Actor code paths reach g_player_hud unconditionally, so a parallel
		// session lends its own instance instead of the primary session's.
		player_hud*         SessionPlayerHud = nullptr;
		struct LevelInspector* SessionInspector = nullptr;
		// Session-owned HUD manager: the renderer's hands pass calls through the
		// global g_hud, so a parallel session lends its own (ctor/dtor save and
		// restore the global around CCustomHUD's self-registration).
		::CHUDManager*    SessionHudManager = nullptr;
		IGame_CustomUI*     SessionGameUI = nullptr;
		// Session-owned spatial databases: the renderer's scene traversal reads
		// the global g_SpatialSpace*, so a parallel session lends its own.
		ISpatial_DB*        SessionSpatial = nullptr;
		ISpatial_DB*        SessionSpatialPhysic = nullptr;
				IRender_interface*  SessionRender = nullptr;
				Sandbox::EmbeddedLevelRegistration EmbeddedResources;

		// Session-local game params (level/single/...): the stock boot menu logic
		// (CGamePersistent::start_logo_intro) reads the global m_game_or_spawn and
		// skips the main menu entirely when it is non-empty, so a parallel session
		// must never write the global. The slot guard lends these to the global
		// only while session code runs (net_Start stages, net_Update, spawn).
		IGame_Persistent::params SessionGameParams;

		// The terrain was injected into the stock renderer as a runtime terrain
		// scene (CreateRuntimeTerrainScene succeeded); the pipeline pass drives
		// the viewport.
		bool                  SessionTerrainScene = false;
		bool                  AutorunOwner = false;
		u32                   SessionIndex = 0;
		SViewerWindowState    WindowState;
	};

#include "ViewerSessionManager.h"

	CViewerSession* ViewerSession();
	CViewerSession* ViewerSessionAt(u32 index);
	CViewerSession& EnsureViewerSession(u32 index = CViewerSessionManager::MaxSessions);
	CViewerSession* ViewerSessionForLevel(const CLevel* level);
	void SetViewerSessionManager(CViewerSessionManager& manager);
	void RestoreViewerPrimaryContext();
	void NotifyPrimaryLevelDestroyed(const CLevel* level);
	bool& ViewerSecondWindowVisible();
	bool& ViewerWindowWasVisible();

	// RAII slot lending: gameplay code resolves the level through the global
	// g_pGameLevel/Level() even mid-load, so a parallel session borrows the slot
	// for the duration of its pump/tick/spawn and restores it on scope exit.
	// The same lending covers the actor globals (g_actor/g_actor_single/
	// GActorInterface), the AI space (g_ai_space) and the physics world
	// (ph_world): a parallel session owns its own instances of each, and the
	// rest of the engine (menu, primary game, HUD update in on_idle) never
	// sees them. Out-of-line in ViewerSession.cpp to keep the heavy AI/physics
	// includes out of this header. The level filesystem alias and current level
	// index are restored on scope exit as well.
	class CLevelSlotGuard
	{
	public:
		CLevelSlotGuard(CViewerSession* session, class CLevel* level, bool primary, bool lockScheduler = true);
		~CLevelSlotGuard();
	private:
		CViewerSession* Session = nullptr;
		IGame_Level* PrevLevel = nullptr;
		CActor* PrevActor = nullptr;
		CActor* PrevActorSingle = nullptr;
		IGame_Actor* PrevActorItf = nullptr;
		CAI_Space* PrevAiSpace = nullptr;
		CScriptEngine* PrevScriptEngine = nullptr;
		CPHWorld* PrevPhysicsWorld = nullptr;
		player_hud* PrevPlayerHud = nullptr;
		CCustomHUD* PrevHud = nullptr;
		IGame_CustomUI* PrevGameUI = nullptr;
		ISpatial_DB* PrevSpatial = nullptr;
		ISpatial_DB* PrevSpatialPhysic = nullptr;
		IGame_Persistent::params PrevGameParams = {};
			IGame_ObjectFactory* PrevObjectFactory = nullptr;
		CEnvironment* PrevEnvironment = nullptr;
		CDB::MODEL* PrevSoundEnv = nullptr;
		CDB::MODEL* PrevSoundSom = nullptr;
		CObjectSpace* PrevSoundOccluder = nullptr;
		SoundZoneVector PrevSoundZones;
			IRender_interface* PrevRender = nullptr;
		xr_string PrevLevelAliasAdd;
		xr_string PrevLevelAliasPath;
		bool PrevLevelAliasHasAdd = false;
		u32 PrevLevelCurrent = u32(-1);
		bool Active = false;
		xrCriticalSectionGuard ContextGuard;
	};

	class CPrimaryEditorContextGuard
	{
	public:
		CPrimaryEditorContextGuard();
		~CPrimaryEditorContextGuard();

	private:
		xrCriticalSectionGuard ContextGuard;
		bool Active = false;
		IGame_Level* PrevLevel = nullptr;
		CActor* PrevActor = nullptr;
		CActor* PrevActorSingle = nullptr;
		IGame_Actor* PrevActorInterface = nullptr;
		CAI_Space* PrevAiSpace = nullptr;
		CScriptEngine* PrevScriptEngine = nullptr;
		player_hud* PrevPlayerHud = nullptr;
		CCustomHUD* PrevHud = nullptr;
		IGame_CustomUI* PrevGameUI = nullptr;
	};

	// ImGui entry points, wired up from RegisterImGuiInGame().
	void RenderViewerWindow();
	void RegisterViewerConsoleCommand();
	// Renders a parallel session's scene with the stock renderer and copies the
	// frame into the viewer's render target (called from CLevel::OnRender when
	// the level is a parallel session). No-op when no parallel session runs.
	void RenderParallelPass(CLevel* level);
	// True while the viewer window is shown (drives whether the pass renders).
	bool ViewerWindowIsVisible();
	// Installs the process-wide first-chance SEH logger (debugging aid; logs
	// fatal exceptions on any thread with an immediate stack trace).
	void RegisterViewerCrashLogger();
}
