#include "StdAfx.h"
#include "ViewerSession.h"
#include "ViewerEditors.h"
#include "ViewerViewport.h"

extern ENGINE_API ref_light precache_light;

#include "../../Level.h"
#include "../../LevelInspector.h"
#include "../../ai_space.h"
#include "../../level_graph.h"
#include "../../patrol_path_storage.h"
#include "../../patrol_path.h"
#include "../../patrol_point.h"
#include "../../alife_simulator.h"
#include "../../alife_smart_terrain_registry.h"
#include "../../../xrServerEntities/xrServer_Objects_ALife.h"
#include "../../Sandbox/SandboxLevelBuilder.h"
#include "../../game_sv_base.h"
#include "../../xrServer.h"
#include "../../Actor.h"
#include "../../script_binder.h"
#include "../../../xrScripts/script_engine.h"
#include "../../Inventory.h"
#include "../../inventory_item.h"
#include "../../player_hud.h"
#include "../../HUDManager.h"
#include "../../ActorEffector.h"
#include "../../game_sv_single.h"
#include "../../../xrEngine/xr_input.h"
#include "../../ui/UIGameCustom.h"
#include "../../ui/UIActorMenu.h"
#include "../../ui/UIWorldSpace.h"
#include "../../GamePersistent.h"

#include "../../../xrPhysics/ODE/PHWorld.h"
#include "../../../xrCore/Collision/ISpatial.h"
#include "../../../xrSound/New/SoundMixerInternal.h"
#include "../../../xrEngine/CustomHUD.h"
#include "../../../xrEngine/IGame_UICustom.h"
#include "../../../xrEngine/Render.h"

#include "../../../xrEngine/EngineApp.h"
#include "../../../xrEngine/EngineAPI.h"
#include "../../../xrEngine/Engine.h"
#include "../../../xrEngine/device.h"
#include "../../../xrEngine/ImGuiManager.h"
#include "../../../xrEngine/std_classes.h"
#include "../../../xrEngine/CameraManager.h"
#include "../../../xrEngine/Environment.h"
#include "../../../xrEngine/IGame_Persistent.h"
#include "../../../xrEngine/IGame_ObjectFactory.h"
#include "../../../xrCore/EventManager.h"

extern EGameIDs ParseStringToGameType(const char* str);
class CUIGameCustom;
extern CUIGameCustom* CurrentGameUI();

#include <cctype>
#include <filesystem>

namespace ImUtils::Viewer
{
namespace ViewerSessionDetail
{
	struct SCameraState
	{
		enum class ETransform : u8
		{
			View, Projection, Full, SavedView, SavedProjection, SavedFull,
			OldView, OldProjection, OldFull,
			HudView, HudProjection, HudFull, OldHudView, OldHudProjection, OldHudFull,
			HudSpecial, Count
		};
		enum class EVector : u8
		{
			Position, Direction, Right, Top, SavedPosition, SavedDirection, SavedRight, SavedTop, Count
		};

		CRenderDevice& RenderDevice;
		Fmatrix Transforms[u32(ETransform::Count)];
		Fvector Vectors[u32(EVector::Count)];
		float Fov;
		float Aspect;
		float Near;
		bool ScopeActive;
		SPPInfo Scope;
		bool Restored = false;

		explicit SCameraState(CRenderDevice& device) : RenderDevice(device)
		{
			Transforms[u32(ETransform::View)] = RenderDevice.mView;
			Transforms[u32(ETransform::Projection)] = RenderDevice.mProject;
			Transforms[u32(ETransform::Full)] = RenderDevice.mFullTransform;
			Transforms[u32(ETransform::SavedView)] = RenderDevice.mView_saved;
			Transforms[u32(ETransform::SavedProjection)] = RenderDevice.mProject_saved;
			Transforms[u32(ETransform::SavedFull)] = RenderDevice.mFullTransform_saved;
			Transforms[u32(ETransform::OldView)] = RenderDevice.mView_old;
			Transforms[u32(ETransform::OldProjection)] = RenderDevice.mProject_old;
			Transforms[u32(ETransform::OldFull)] = RenderDevice.mFullTransform_old;
			Transforms[u32(ETransform::HudView)] = RenderDevice.mView_hud;
			Transforms[u32(ETransform::HudProjection)] = RenderDevice.mProject_hud;
			Transforms[u32(ETransform::HudFull)] = RenderDevice.mFullTransform_hud;
			Transforms[u32(ETransform::OldHudView)] = RenderDevice.mView_hud_old;
			Transforms[u32(ETransform::OldHudProjection)] = RenderDevice.mProject_hud_old;
			Transforms[u32(ETransform::OldHudFull)] = RenderDevice.mFullTransform_hud_old;
			Transforms[u32(ETransform::HudSpecial)] = RenderDevice.mFullTransform_hud_special;
			Vectors[u32(EVector::Position)] = RenderDevice.vCameraPosition;
			Vectors[u32(EVector::Direction)] = RenderDevice.vCameraDirection;
			Vectors[u32(EVector::Right)] = RenderDevice.vCameraRight;
			Vectors[u32(EVector::Top)] = RenderDevice.vCameraTop;
			Vectors[u32(EVector::SavedPosition)] = RenderDevice.vCameraPosition_saved;
			Vectors[u32(EVector::SavedDirection)] = RenderDevice.vCameraDirection_saved;
			Vectors[u32(EVector::SavedRight)] = RenderDevice.vCameraRight_saved;
			Vectors[u32(EVector::SavedTop)] = RenderDevice.vCameraTop_saved;
			Fov = RenderDevice.fFOV;
			Aspect = RenderDevice.fASPECT;
			Near = RenderDevice.fViewportNear;
			ScopeActive = RenderDevice.ScopePostprocessActive;
			Scope = RenderDevice.ScopePostprocess;
		}

		void Restore()
		{
			if (Restored)
				return;
			Restored = true;
			RenderDevice.mView = Transforms[u32(ETransform::View)];
			RenderDevice.mProject = Transforms[u32(ETransform::Projection)];
			RenderDevice.mFullTransform = Transforms[u32(ETransform::Full)];
			RenderDevice.mView_saved = Transforms[u32(ETransform::SavedView)];
			RenderDevice.mProject_saved = Transforms[u32(ETransform::SavedProjection)];
			RenderDevice.mFullTransform_saved = Transforms[u32(ETransform::SavedFull)];
			RenderDevice.mView_old = Transforms[u32(ETransform::OldView)];
			RenderDevice.mProject_old = Transforms[u32(ETransform::OldProjection)];
			RenderDevice.mFullTransform_old = Transforms[u32(ETransform::OldFull)];
			RenderDevice.mView_hud = Transforms[u32(ETransform::HudView)];
			RenderDevice.mProject_hud = Transforms[u32(ETransform::HudProjection)];
			RenderDevice.mFullTransform_hud = Transforms[u32(ETransform::HudFull)];
			RenderDevice.mView_hud_old = Transforms[u32(ETransform::OldHudView)];
			RenderDevice.mProject_hud_old = Transforms[u32(ETransform::OldHudProjection)];
			RenderDevice.mFullTransform_hud_old = Transforms[u32(ETransform::OldHudFull)];
			RenderDevice.mFullTransform_hud_special = Transforms[u32(ETransform::HudSpecial)];
			RenderDevice.vCameraPosition = Vectors[u32(EVector::Position)];
			RenderDevice.vCameraDirection = Vectors[u32(EVector::Direction)];
			RenderDevice.vCameraRight = Vectors[u32(EVector::Right)];
			RenderDevice.vCameraTop = Vectors[u32(EVector::Top)];
			RenderDevice.vCameraPosition_saved = Vectors[u32(EVector::SavedPosition)];
			RenderDevice.vCameraDirection_saved = Vectors[u32(EVector::SavedDirection)];
			RenderDevice.vCameraRight_saved = Vectors[u32(EVector::SavedRight)];
			RenderDevice.vCameraTop_saved = Vectors[u32(EVector::SavedTop)];
			RenderDevice.fFOV = Fov;
			RenderDevice.fASPECT = Aspect;
			RenderDevice.fViewportNear = Near;
			RenderDevice.ScopePostprocessActive = ScopeActive;
			RenderDevice.ScopePostprocess = Scope;
			RenderDevice.CalculateTransforms();
		}

		~SCameraState() { Restore(); }
	};

	class CFrameTimeScope
	{
		float Previous = 0.f;
		bool Restore = false;

	public:
		explicit CFrameTimeScope(bool independent, float delta)
		{
			Restore = independent && delta > 0.f && Device.Paused();
			if (Restore)
			{
				Previous = Device.fTimeDelta;
				Device.fTimeDelta = delta;
			}
		}
		~CFrameTimeScope()
		{
			if (Restore)
				Device.fTimeDelta = Previous;
		}
	};
}

// ---------------------------------------------------------------------------
// The parallel render pass: the session's scene through the stock renderer.
// ---------------------------------------------------------------------------

void RenderParallelPass(CLevel* level)
{
	CViewerSession* session = ViewerSessionForLevel(level);
	if (session == nullptr || level == nullptr || session->GetLevel() != level || !session->IsActive())
		return;
	const bool windowVisible = session->GetState().PresentationEnabled && (session->Index() == 0 ? ViewerWindowWasVisible() : ViewerSecondWindowVisible());
	if (!windowVisible)
		return;
	if (!session->Renderer.IsReady())
		return;
	// A scene without compiled geometry still renders weather and its dynamic
	// entities through the same renderer. Pass toggles must not turn into a
	// different renderer or discard the environment.
	if (session->SceneRenderer() == nullptr)
		return;

	// The render phase runs concurrently with the GameThread: take the
	// scheduler lock BEFORE lending - otherwise a GameThread update already in
	// flight would see the lent g_pGameLevel and process our session's objects
	// mid-render (no AI/physics context there). With the lock first, a running
	// update finishes against the real slot, and the next one waits until the
	// slot is restored.
	xrCriticalSectionGuard schedulerGuard(&Engine.Sheduler.UpdateMutex());

	// Lend the session's full context (level, actor globals, AI space, physics
	// world, player HUD, spatial DBs) for the whole pass.
	CLevelSlotGuard guard(session, level, false);

	// Swap the device camera: with a bound player actor the session level's
	// camera manager already holds the actor camera (updated during the session
	// tick through the stock CActor path) - apply it exactly like the game does;
	// otherwise the viewer free-cam drives.
	ViewerSessionDetail::SCameraState Camera(Device);

	const SViewerState& state = session->GetState();
	Device.fASPECT = float(state.ViewportWidth) / float(std::max(1u, state.ViewportHeight));
	CActor* viewActor = state.HasActor && level->CurrentViewEntity() != nullptr ? level->CurrentViewEntity()->cast_actor() : nullptr;
	if (viewActor != nullptr)
	{
		viewActor->Cameras().ApplyDevice(Device.fViewportNear);
	}
	else
	{
		Device.vCameraPosition.set(state.CamPos);
		Device.vCameraDirection.setHP(state.CamYaw, state.CamPitch);
		Device.vCameraRight.crossproduct(Fvector().set(0.f, 1.f, 0.f), Device.vCameraDirection).normalize();
		Device.vCameraTop.crossproduct(Device.vCameraDirection, Device.vCameraRight).normalize();
		Device.mView.build_camera_dir(Device.vCameraPosition, Device.vCameraDirection, Device.vCameraTop);
		// The free camera needs its own perspective projection: the menu leaves a
		// stale/orthographic one in Device.mProject, which reads as "not a
		// perspective camera" in the viewport. Match the overlay's projection.
		float farPlane = 1000.f;
		if (g_pGamePersistent != nullptr && g_pGamePersistent->Environment().CurrentEnv != nullptr)
			farPlane = g_pGamePersistent->Environment().CurrentEnv->far_plane;
		Device.mProject.build_projection(deg2rad(Device.fFOV), Device.fASPECT, Device.fViewportNear, farPlane);
		Device.CalculateTransforms();
	}

	// Live lighting override: the deferred pipeline relights from the current
	// environment descriptor every frame, so adjusting it here takes effect on
	// this very render - there is no lightmap/compile step. Restored below.
	CEnvDescriptorMixer* envDesc = nullptr;
	Fvector savedSunDir = {}, savedSunColor = {}, savedAmbient = {}, savedSkyColor = {};
	Fvector4 savedHemi = {};
	float savedFogDensity = 0.f, savedFogNear = 0.f, savedFogFar = 0.f;
	if (state.LightingOverride && g_pGamePersistent != nullptr)
	{
		envDesc = g_pGamePersistent->Environment().CurrentEnv;
		if (envDesc != nullptr)
		{
			savedSunDir = envDesc->sun_dir;
			savedSunColor = envDesc->sun_color;
			savedAmbient = envDesc->ambient;
			savedSkyColor = envDesc->sky_color;
			savedHemi = envDesc->hemi_color;
			savedFogDensity = envDesc->fog_density;
			savedFogNear = envDesc->fog_near;
			savedFogFar = envDesc->fog_far;

			// Keep the weather texture's own sky colors; tinting it blue produces
			// a muddy brown result with the stock daytime cubemaps.
			envDesc->sun_dir.setHP(deg2rad(state.SunAzimuthDeg), deg2rad(state.SunElevationDeg));
			envDesc->sun_color.set(1.0f, 0.96f, 0.88f).mul(state.SunIntensity);
			envDesc->ambient.set(0.30f, 0.34f, 0.40f).mul(state.AmbientIntensity);
			envDesc->hemi_color.set(0.38f, 0.46f, 0.58f, 1.0f);
			envDesc->hemi_color.x *= state.AmbientIntensity;
			envDesc->hemi_color.y *= state.AmbientIntensity;
			envDesc->hemi_color.z *= state.AmbientIntensity;
			envDesc->sky_color.set(1.f, 1.f, 1.f);
			envDesc->fog_density = clampr(state.FogDensity, 0.f, 1.f);
			envDesc->fog_near = (1.f - envDesc->fog_density) * 0.85f * envDesc->fog_distance;
			envDesc->fog_far = 0.99f * envDesc->fog_distance;
		}
	}

	if (state.LightingOverride && !level->HasRenderScene)
		Render->set_fallback_sky(g_pGamePersistent->Environment());

	const Flags32 savedDeviceFlags = psDeviceFlags;
	if (state.RenderPassOverride)
	{
		const u32 mask = rsDrawStatic | rsDrawDynamic | rsDetails | rsOcclusion | rsWireframe;
		psDeviceFlags.flags = (psDeviceFlags.flags & ~mask) | (state.RenderPassFlags.flags & mask);
	}

	try
	{
		Render->set_wireframe_mode(u32(state.WireframeMode), u16(state.WireframeObjectId));
		Render->Calculate();
		Render->Render();
		if (level->game != nullptr)
		{
			level->game->OnRender();
			if (g_actor != nullptr && g_actor->g_Alive() && level->WorldSpaceUIManager != nullptr)
				level->WorldSpaceUIManager->OnRender();
			if (level->HasLevelResource(ELevelResource::Hud) && g_hud != nullptr)
			{
				Render->set_scene_render_target();
				static_cast<::CHUDManager*>(g_hud)->RenderUI();
			}
		}
		const bool inspectorActive = state.InspectorSceneFlags.flags != 0 || state.InspectorZoneFlags.flags != 0 ||
			state.InspectorSkeletonFlags.flags != 0 || state.InspectorSelectionFlags.flags != 0 || state.InspectorSpatialMask != 0;
		if (inspectorActive)
		{
			if (level->HasLevelResource(ELevelResource::Hud))
				Render->set_scene_render_target();
			else
				Render->set_scene_overlay_target();
			if (session->SessionInspector == nullptr)
				session->SessionInspector = new LevelInspector(false, false);
			LevelInspector& inspector = *session->SessionInspector;
			inspector.m_flags = state.InspectorSceneFlags;
			inspector.m_zone_flags = state.InspectorZoneFlags;
			inspector.m_skeleton_flags = state.InspectorSkeletonFlags;
			inspector.m_selection_flags = state.InspectorSelectionFlags;
			inspector.m_selection_text_flags = state.InspectorTextFlags;
			inspector.m_spatials_mask = static_cast<ESPATIAL_TYPE>(state.InspectorSpatialMask);
			inspector.WaypointsFlags = static_cast<LevelInspector::EWAYPOINT_INFO>(state.InspectorWaypointMode);
			inspector.wp_prefix = state.InspectorWaypointPrefix;
			inspector.zbuffer_enable = state.InspectorZBuffer;
			inspector.visible_currents = state.InspectorVisibleCurrents;
			inspector.OnRender();
		}
		if (level->HasLevelResource(ELevelResource::Hud))
			session->Renderer.ResolveFromSceneTexture(Render->get_scene_ui_color());
		else
			session->Renderer.ResolveFromSceneTexture(Render->get_scene_color());
	}
	catch (...)
	{
		Msg("! [sandbox] parallel render pass faulted");
	}
	psDeviceFlags = savedDeviceFlags;

	if (envDesc != nullptr)
	{
		envDesc->sun_dir = savedSunDir;
		envDesc->sun_color = savedSunColor;
		envDesc->ambient = savedAmbient;
		envDesc->sky_color = savedSkyColor;
		envDesc->hemi_color = savedHemi;
		envDesc->fog_density = savedFogDensity;
		envDesc->fog_near = savedFogNear;
		envDesc->fog_far = savedFogFar;
	}

	// Restore the camera for the primary session's pass.
	Camera.Restore();
}

u32 CViewerSession::ViewportDimension() const
{
	return std::max(1u, std::max(ViewerState.ViewportWidth, ViewerState.ViewportHeight));
}

// Implementation details of the viewer session driver. Named, not anonymous.
namespace ViewerSessionDetail
{
	// Option-string friendly folder names: [a-z0-9_] only, no spaces or slashes.
	xr_string MakePresetFolderName(const SViewerPreset& preset)
	{
		string128 name;
		switch (preset.Terrain.Kind)
		{
		case Sandbox::ETerrainKind::Flat:
			xr_strcpy(name, "viewer_flat");
			break;
		case Sandbox::ETerrainKind::Analytic:
			xr_sprintf(name, "viewer_%s", Sandbox::AnalyticPresetName(preset.Terrain.Analytic));
			break;
		case Sandbox::ETerrainKind::Heightmap:
			xr_strcpy(name, "viewer_heightmap");
			break;
		default:
			xr_strcpy(name, "viewer_custom");
			break;
		}

		for (char* c = name; *c; ++c)
		{
			const char lower = char(std::tolower(u8(*c)));
			const bool allowed = (lower >= 'a' && lower <= 'z') || (lower >= '0' && lower <= '9') || lower == '_';
			*c = allowed ? lower : '_';
		}
		return name;
	}

	// Perspective projection through a session-local full transform (parallel
	// sessions never touch Device camera matrices - those belong to the primary
	// session/menu).
	bool ProjectToScreen(const Fmatrix& fullTransform, const Fvector& world, Fvector2& out)
	{
		Fvector4 projected;
		fullTransform.transform(projected, world);
		if (projected.w <= 0.f || projected.z < 0.f)
			return false;

		const float invW = 1.f / projected.w;
		out.x = (1.f + projected.x * invW) * 0.5f * float(Device.Width);
		out.y = (1.f - projected.y * invW) * 0.5f * float(Device.Height);
		return true;
	}

	// Logs then re-raises so the engine crash handler writes a full dump.
	LONG LogViewerTickCrash(EXCEPTION_POINTERS* ep)
	{
		Msg("! [sandbox] viewer tick crashed: code 0x%08lX at %p (xrGame %p, xrEngineCore %p, xrNetServer %p, xrPhysics %p)",
			ep->ExceptionRecord->ExceptionCode, ep->ExceptionRecord->ExceptionAddress,
			GetModuleHandleA("xrGame.dll"), GetModuleHandleA("xrEngineCore.dll"), GetModuleHandleA("xrNetServer.dll"), GetModuleHandleA("xrPhysics.dll"));
		SandboxLogStackTrace(ep);
		return EXCEPTION_CONTINUE_SEARCH;
	}

	// Finds the ClientID of the (single) in-proc client of the local server.
	struct SFirstClientFinder
	{
		ClientID FoundId;
		bool     Found = false;

		void operator()(IClient* client)
		{
			if (!Found && client != nullptr)
			{
				FoundId = client->ID;
				Found = true;
			}
		}
	};

	bool FindLocalClientID(xrServer* server, ClientID& out)
	{
		SFirstClientFinder finder;
		server->ForEachClientDo(finder);
		if (finder.Found)
			out = finder.FoundId;
		return finder.Found;
	}

	IInputReceiver& SessionInputReceiver(CLevel& level)
	{
		if (IInputReceiver* receiver = level.Flow.InputReceiver())
			return *receiver;
		return level;
	}

	bool SupportsPhysicalSpawn(const CLevel& level, CPHWorld* world)
	{
		return world != nullptr && !world->IsFreezed() && level.GetResourceState(ELevelResource::Collision) == EResourceState::Ready;
	}

	const char* TerrainTextureName()
	{
		constexpr const char* candidates[] =
		{
			"detail\\detail_dirt_det1",
			"detail\\detail_earth_det1",
			"detail\\detail_gras_grass",
			"detail\\detail_grass",
			"detail\\detail_dirt",
			"terrain\\terrain_zaton",
			"terrain\\terrain_jupiter",
			"terrain\\terrain_pripyat",
		};
		for (const char* candidate : candidates)
		{
			string_path path;
			if (FS.exist(path, _game_textures_, candidate, ".dds"))
				return candidate;
		}
		return "$memory$viewer_ground";
	}

	// First-chance logger: sees fatal SEH exceptions on EVERY thread (including
	// ones without an SEH frame and before the engine's own crash handler,
	// whose minidump writer is unreliable). Logs code/address/module + stack.
	LONG CALLBACK FirstChanceCrashLog(EXCEPTION_POINTERS* ep)
	{
		const DWORD code = ep->ExceptionRecord->ExceptionCode;
		switch (code)
		{
		case 0xC0000005: // access violation
		case 0xC00000FD: // stack overflow
		case 0xC0000374: // heap corruption
		case 0xC0000409: // fastfail
		case 0xC000001D: // illegal instruction
		case 0xC0000006: // in-page error (paging I/O failed; often commit exhaustion)
		case 0xC000007F: // disk full / paging file too small (terminates with exit 127)
			break;
		default:
			return EXCEPTION_CONTINUE_SEARCH;
		}
		Msg("! [veh] first-chance fatal: code 0x%08lX at %p (tid %lu) (xrGame %p, xrEngineCore %p, xrPhysics %p, xrScripts %p)",
			code, ep->ExceptionRecord->ExceptionAddress, GetCurrentThreadId(),
			GetModuleHandleA("xrGame.dll"), GetModuleHandleA("xrEngineCore.dll"), GetModuleHandleA("xrPhysics.dll"), GetModuleHandleA("xrScripts.dll"));
		SandboxLogStackTrace(ep);
		return EXCEPTION_CONTINUE_SEARCH;
	}

	CViewerSessionManager* Manager = nullptr;
	xr_unique_ptr<CViewerSessionManager> ManagerOwner;
	bool AutorunProbed = false;
}

void RegisterViewerCrashLogger()
{
	AddVectoredExceptionHandler(1, &ViewerSessionDetail::FirstChanceCrashLog);
}

void SetViewerSessionManager(CViewerSessionManager& manager)
{
	if (ViewerSessionDetail::Manager == nullptr || !ViewerSessionDetail::Manager->Accessed)
	{
		ViewerSessionDetail::Manager = &manager;
		ViewerSessionDetail::ManagerOwner.reset();
	}
}

CViewerSession* ViewerSession()
{
	CViewerSessionManager* manager = ViewerSessionDetail::Manager;
	if (manager == nullptr || !manager->Accessed)
		return nullptr;
	return &manager->Sessions[manager->SelectedSession];
}

CViewerSession* ViewerSessionAt(u32 index)
{
	CViewerSessionManager* manager = ViewerSessionDetail::Manager;
	if (manager == nullptr || !manager->Accessed || index >= CViewerSessionManager::MaxSessions)
		return nullptr;
	return &manager->Sessions[index];
}

CViewerSession& EnsureViewerSession(u32 index)
{
	if (ViewerSessionDetail::Manager == nullptr)
	{
		ViewerSessionDetail::ManagerOwner = xr_make_unique<CViewerSessionManager>();
		ViewerSessionDetail::Manager = ViewerSessionDetail::ManagerOwner.get();
	}
	CViewerSessionManager& manager = *ViewerSessionDetail::Manager;
	manager.Accessed = true;
	if (g_pGameLevel != nullptr && !g_pGameLevel->IsParallel())
	{
		manager.PrimaryLevel = static_cast<CLevel*>(g_pGameLevel);
		manager.PrimaryActor = g_actor;
		manager.PrimaryPlayerHud = g_player_hud;
		manager.PrimaryHud = g_hud;
		manager.PrimaryGameUI = g_pGameCustom;
		manager.PrimaryLevel->Services.Hud = g_hud;
		manager.PrimaryLevel->Services.GameUI = g_pGameCustom;
	}
	if (manager.PrimaryAiSpace == nullptr)
	{
		manager.PrimaryAiSpace = g_ai_space;
		if (g_pGameLevel != nullptr && !g_pGameLevel->IsParallel() && g_pGameLevel->Services.AI != nullptr)
			manager.PrimaryAiSpace = g_pGameLevel->Services.AI;
	}
	CViewerSession& selected = manager.SelectSession(index);
	if (selected.Index() == 1)
		manager.SecondaryWindowVisible = true;
	return selected;
}

void RestoreViewerPrimaryContext()
{
	CViewerSessionManager* manager = ViewerSessionDetail::Manager;
	if (manager == nullptr || !manager->Accessed)
		return;
	if (g_pGameLevel != nullptr && !g_pGameLevel->IsParallel())
	{
		manager->PrimaryLevel = static_cast<CLevel*>(g_pGameLevel);
		manager->PrimaryActor = g_actor;
		manager->PrimaryPlayerHud = g_player_hud;
		manager->PrimaryHud = g_hud;
		manager->PrimaryGameUI = g_pGameCustom;
		manager->PrimaryLevel->Services.Hud = g_hud;
		manager->PrimaryLevel->Services.GameUI = g_pGameCustom;
		if (g_pGameLevel->Services.AI != nullptr)
			manager->PrimaryAiSpace = g_pGameLevel->Services.AI;
	}
	g_hud = manager->PrimaryHud;
	g_pGameCustom = manager->PrimaryGameUI;
	if (manager->PrimaryAiSpace == nullptr)
		return;
	g_ai_space = manager->PrimaryAiSpace;
	g_pScriptEngine = manager->PrimaryAiSpace->get_script_engine();
}

void NotifyPrimaryLevelDestroyed(const CLevel* level)
{
	CViewerSessionManager* manager = ViewerSessionDetail::Manager;
	if (manager == nullptr || level == nullptr || level->IsParallel())
		return;
	if (manager->PrimaryLevel == level)
	{
		manager->PrimaryLevel = nullptr;
		manager->PrimaryActor = nullptr;
		manager->PrimaryPlayerHud = nullptr;
		manager->PrimaryHud = nullptr;
		manager->PrimaryGameUI = nullptr;
	}
	manager->PrimaryAiSpace = nullptr;
	if (level->Services.AI != nullptr && g_ai_space == level->Services.AI)
		g_ai_space = nullptr;
	if (level->Services.ScriptEngine != nullptr && g_pScriptEngine == level->Services.ScriptEngine)
		g_pScriptEngine = nullptr;
}

CViewerSession* ViewerSessionForLevel(const CLevel* level)
{
	CViewerSessionManager* manager = ViewerSessionDetail::Manager;
	if (manager == nullptr)
		return nullptr;
	for (CViewerSession& session : manager->Sessions)
	{
		if (session.State() != EViewerSessionState::Idle && session.GetLevel() == level)
			return &session;
	}
	return nullptr;
}

bool& ViewerSecondWindowVisible()
{
	return ViewerSessionDetail::Manager->SecondaryWindowVisible;
}

bool& ViewerWindowWasVisible()
{
	return ViewerSessionDetail::Manager->WindowsVisible[u32(ViewerSessionDetail::Manager->Sessions[0].ActivePreset().EditorKind)];
}

// ---------------------------------------------------------------------------
// CLevelSlotGuard
// ---------------------------------------------------------------------------

CLevelSlotGuard::CLevelSlotGuard(CViewerSession* session, CLevel* level, bool primary, bool lockScheduler) :
	ContextGuard(lockScheduler ? &Engine.Sheduler.UpdateMutex() : nullptr)
{
	Active = !primary && level != nullptr;
	if (!Active)
		return;
	FS_Path* levelAlias = FS.get_path("$level$");
	PrevLevelAliasHasAdd = levelAlias->m_Add != nullptr;
	if (PrevLevelAliasHasAdd)
	{
		PrevLevelAliasAdd = levelAlias->m_Add;
	}
	else
	{
		PrevLevelAliasPath = levelAlias->m_Path != nullptr ? levelAlias->m_Path : "";
	}
	PrevLevelCurrent = pApp->Level_Current;
	if (session != nullptr && !session->SessionFolder.empty())
	{
		const xr_string aliasAdd = session->SessionFolder + Platform::kPreferredSeparator;
		if (levelAlias->m_Add == nullptr || _stricmp(levelAlias->m_Add, aliasAdd.c_str()) != 0)
		{
			levelAlias->_set(aliasAdd.c_str());
		}
		const int current = pApp->Level_ID(session->SessionFolder.c_str(), "1.0", false);
		if (current >= 0)
		{
			pApp->Level_Current = u32(current);
		}
	}
	Session = session;
	PrevObjectFactory = g_object_factory;
	PrevRender = Render;
	if (session != nullptr && session->SceneRenderer() != nullptr)
		Render = session->SceneRenderer();
	PrevLevel = g_pGameLevel;
	g_pGameLevel = level;
	PrevActor = g_actor;
	g_actor = session != nullptr ? session->SessionActor : nullptr;
	PrevActorSingle = g_actor_single;
	g_actor_single = session != nullptr ? session->SessionActorSingle : nullptr;
	PrevActorItf = GActorInterface;
	GActorInterface = session != nullptr ? session->SessionActorItf : nullptr;
	PrevAiSpace = g_ai_space;
	g_ai_space = session != nullptr ? session->SessionAiSpace : nullptr;
	PrevScriptEngine = g_pScriptEngine;
	if (session != nullptr && session->SessionAiSpace != nullptr)
	{
		CScriptEngine* sessionScript = session->SessionAiSpace->get_script_engine();
		if (sessionScript != nullptr || level->HasLevelResource(ELevelResource::LevelScripts))
			g_pScriptEngine = sessionScript;
	}
	PrevPhysicsWorld = ph_world;
	ph_world = session != nullptr ? session->SessionPhysicsWorld : nullptr;
	PrevPlayerHud = g_player_hud;
	g_player_hud = session != nullptr ? session->SessionPlayerHud : nullptr;
	PrevHud = g_hud;
	g_hud = session != nullptr ? session->SessionHudManager : nullptr;
	PrevGameUI = g_pGameCustom;
	g_pGameCustom = session != nullptr && session->SessionHudManager != nullptr && session->SessionHudManager->GetGameUI() != nullptr ? session->SessionHudManager->GetGameUI() : session != nullptr ? session->SessionGameUI : nullptr;
	PrevSpatial = g_SpatialSpace;
	g_SpatialSpace = session != nullptr ? session->SessionSpatial : nullptr;
	PrevSpatialPhysic = g_SpatialSpacePhysic;
	g_SpatialSpacePhysic = session != nullptr ? session->SessionSpatialPhysic : nullptr;
	PrevEnvironment = g_pGamePersistent->pEnvironment;
	if (session != nullptr && session->SessionEnvironment != nullptr)
		g_pGamePersistent->pEnvironment = session->SessionEnvironment;
	PrevSoundEnv = Sound->swap_geometry_env(level->SoundGeometry.Env);
	PrevSoundSom = Sound->swap_geometry_som(level->SoundGeometry.Som);
	PrevSoundOccluder = Sound->swap_geometry_occ(level->SoundGeometry.Occluder);
	XRay::Sound::Mixer::SwapZones(PrevSoundZones);
	XRay::Sound::Mixer::SwapZones(level->SoundGeometry.Zones);

	// Game params are global flow state the boot/menu logic reads (m_game_or_spawn
	// decides whether the main menu shows at all): lend the session's copy for the
	// scope, restore the owner's on exit. Without this the viewer's level folder in
	// m_game_or_spawn suppresses the main menu at boot.
	PrevGameParams = g_pGamePersistent->m_game_params;
	if (session != nullptr)
		g_pGamePersistent->m_game_params = session->SessionGameParams;
}

CLevelSlotGuard::~CLevelSlotGuard()
{
	if (!Active)
		return;
	if (Session != nullptr)
	{
		// Capture whatever this scope produced or cleared (the actor's
		// net_Spawn binds the globals, net_Destroy clears them; load stages
		// create/destroy the physics world).
		Session->SessionActor = g_actor;
		Session->SessionActorSingle = g_actor_single;
		Session->SessionActorItf = GActorInterface;
		Session->SessionPhysicsWorld = ph_world;
		Session->SessionGameUI = Session->SessionHudManager != nullptr && Session->SessionHudManager->GetGameUI() != nullptr ? Session->SessionHudManager->GetGameUI() : g_pGameCustom;
		Session->SessionHudManager = static_cast<::CHUDManager*>(g_hud);
		if (Session->SessionLevel != nullptr)
		{
			SLevelServices& services = Session->SessionLevel->Services;
			services.Actor = g_actor;
			services.Physics = ph_world;
			services.ScriptEngine = g_pScriptEngine;
			services.Environment = g_pGamePersistent->pEnvironment;
			services.Hud = g_hud;
			services.GameUI = g_pGameCustom;
		}
	}
	pApp->Level_Current = PrevLevelCurrent;
	if (PrevLevelAliasHasAdd)
	{
		for (u32 index = 0; index < pApp->Levels.size(); ++index)
		{
			if (_stricmp(PrevLevelAliasAdd.c_str(), pApp->Levels[index].folder) == 0)
			{
				pApp->Level_Current = index;
				break;
			}
		}
		FS_Path* levelAlias = FS.get_path("$level$");
		if (levelAlias->m_Add == nullptr || _stricmp(levelAlias->m_Add, PrevLevelAliasAdd.c_str()) != 0)
		{
			levelAlias->_set(PrevLevelAliasAdd.c_str());
		}
	}
	else
	{
		FS_Path* levelAlias = FS.get_path("$level$");
		const char* currentPath = levelAlias->m_Path != nullptr ? levelAlias->m_Path : "";
		if (levelAlias->m_Add != nullptr || xr_strcmp(currentPath, PrevLevelAliasPath.c_str()) != 0)
		{
			xr_free(levelAlias->m_Add);
			levelAlias->m_Add = nullptr;
			xr_free(levelAlias->m_Path);
			levelAlias->m_Path = xr_strdup(PrevLevelAliasPath.c_str());
		}
	}
	g_pGameLevel = PrevLevel;
	g_actor = PrevActor;
	g_actor_single = PrevActorSingle;
	GActorInterface = PrevActorItf;
	g_ai_space = PrevAiSpace;
	g_pScriptEngine = PrevScriptEngine;
	ph_world = PrevPhysicsWorld;
	g_player_hud = PrevPlayerHud;
	g_hud = PrevHud;
	g_pGameCustom = PrevGameUI;
	g_SpatialSpace = PrevSpatial;
	g_SpatialSpacePhysic = PrevSpatialPhysic;
	g_pGamePersistent->m_game_params = PrevGameParams;
	g_pGamePersistent->pEnvironment = PrevEnvironment;
	Sound->swap_geometry_env(PrevSoundEnv);
	Sound->swap_geometry_som(PrevSoundSom);
	Sound->swap_geometry_occ(PrevSoundOccluder);
	XRay::Sound::Mixer::SwapZones(PrevSoundZones);
	g_object_factory = PrevObjectFactory;
	Render = PrevRender;
}

CPrimaryEditorContextGuard::CPrimaryEditorContextGuard() : ContextGuard(&Engine.Sheduler.UpdateMutex())
{
	CViewerSessionManager* manager = ViewerSessionDetail::Manager;
	if (manager == nullptr || manager->PrimaryLevel == nullptr || manager->PrimaryLevel->IsParallel())
		return;
	Active = true;
	PrevLevel = g_pGameLevel;
	PrevActor = g_actor;
	PrevActorSingle = g_actor_single;
	PrevActorInterface = GActorInterface;
	PrevAiSpace = g_ai_space;
	PrevScriptEngine = g_pScriptEngine;
	PrevPlayerHud = g_player_hud;
	PrevHud = g_hud;
	PrevGameUI = g_pGameCustom;
	g_pGameLevel = manager->PrimaryLevel;
	g_actor = manager->PrimaryActor;
	g_actor_single = manager->PrimaryActor;
	GActorInterface = manager->PrimaryActor;
	g_ai_space = manager->PrimaryAiSpace;
	g_pScriptEngine = manager->PrimaryAiSpace != nullptr ? manager->PrimaryAiSpace->get_script_engine() : nullptr;
	g_player_hud = manager->PrimaryPlayerHud;
	g_hud = manager->PrimaryHud;
	g_pGameCustom = manager->PrimaryGameUI;
}

CPrimaryEditorContextGuard::~CPrimaryEditorContextGuard()
{
	if (!Active)
		return;
	g_pGameLevel = PrevLevel;
	g_actor = PrevActor;
	g_actor_single = PrevActorSingle;
	GActorInterface = PrevActorInterface;
	g_ai_space = PrevAiSpace;
	g_pScriptEngine = PrevScriptEngine;
	g_player_hud = PrevPlayerHud;
	g_hud = PrevHud;
	g_pGameCustom = PrevGameUI;
}

void CViewerSessionTicker::OnFrame()
{
	RestoreViewerPrimaryContext();
	CViewerSession* session = ViewerSession();
	if (session == nullptr)
	{
		// Autorun bootstrap: if $app_data_root$/viewer_autorun.ltx exists, it
		// drives a session (autotest/CI path). Probed once; console commands
		// (sandbox_start) construct the session the same way.
		if (!ViewerSessionDetail::AutorunProbed)
		{
			ViewerSessionDetail::AutorunProbed = true;
			string_path autorunPath;
			FS.update_path(autorunPath, _app_data_root_, "viewer_autorun.ltx");
			if (FS.exist(autorunPath))
				session = &EnsureViewerSession();
		}
		if (session == nullptr)
			return;
	}
	for (CViewerSession& current : ViewerSessionDetail::Manager->Sessions)
		current.Tick();
	RestoreViewerPrimaryContext();
}

void CViewerSessionTicker::OnRender()
{
	RestoreViewerPrimaryContext();
	if (ViewerSessionDetail::Manager == nullptr)
		return;
	for (CViewerSession& current : ViewerSessionDetail::Manager->Sessions)
		RenderParallelPass(current.SessionLevel);
	RestoreViewerPrimaryContext();
}

void CViewerSessionTicker::OnAppEnd()
{
	if (ViewerSessionDetail::Manager == nullptr)
		return;
	for (u32 index = CViewerSessionManager::MaxSessions; index-- > 0;)
		ViewerSessionDetail::Manager->Sessions[index].HandleStop();
	RestoreViewerPrimaryContext();
	ViewerSessionDetail::Manager->PrimaryAiSpace = nullptr;
}

bool CViewerRequestQueue::Push(const SViewerRequest& request)
{
	if (Count >= Capacity)
	{
		Msg("! Level viewer: request queue full (%u), dropping request type %u", Capacity, u32(request.Type));
		return false;
	}
	Buffer[Tail] = request;
	Tail = (Tail + 1) % Capacity;
	++Count;
	return true;
}

SViewerPreset::SViewerPreset()
{
	// Procedural presets keep the optional HUD and standalone level script
	// process disabled until requested. Each session owns its R4 scene.
	Resources.assign(u32(ELevelResource::All));
	Resources.set(u32(ELevelResource::Hud), false);
	Resources.set(u32(ELevelResource::LevelScripts), false);

	Terrain.Kind = Sandbox::ETerrainKind::Flat;
	Terrain.Size = 64.f;
	Terrain.Cell = 2.f;
	Terrain.Analytic = Sandbox::EAnalyticPreset::Plane;
	Terrain.Amplitude = 10.f;
	Terrain.Frequency = 0.05f;
	Terrain.HeightScale = 1.f;
	Memory.GpuBytes = CViewerGpuPool::DefaultBudgetBytes();
}

// ---------------------------------------------------------------------------
// Requests
// ---------------------------------------------------------------------------

void CViewerSession::PushRequest(const SViewerRequest& request)
{
	Requests.Push(request);
}

void CViewerSession::RequestEditorCommand(EViewerEditorCommand command, const char* argument, float value)
{
	SViewerRequest request;
	request.Type = EViewerRequest::EditorCommand;
	request.IntValue = int(command);
	xr_strcpy(request.Path, argument != nullptr ? argument : "");
	request.FloatValue = value;
	PushRequest(request);
}

void CViewerSession::RunInContext(const xr_delegate<void()>& work)
{
	// The owning tick already holds the scheduler lock. A loading worker can
	// execute while that tick waits for the worker, so a nested lock would deadlock.
	CLevelSlotGuard guard(this, SessionLevel, false, false);
	work();
}

bool CViewerSession::RetryRequestWhileUnavailable(const SViewerRequest& request, bool requireActor)
{
	if (IsActive() && (!requireActor || SessionActor != nullptr))
	{
		return false;
	}
	if (request.Type == EViewerRequest::EditorCommand)
	{
		if (ViewerState.Session != EViewerSessionState::Starting && request.HeightValue >= 600)
			return false;
		SViewerRequest retry = request;
		if (ViewerState.Session != EViewerSessionState::Starting)
			++retry.HeightValue;
		PushRequest(retry);
		return true;
	}
	if (ViewerState.Session != EViewerSessionState::Starting && request.IntValue >= 600)
	{
		return false;
	}
	SViewerRequest retry = request;
	if (ViewerState.Session != EViewerSessionState::Starting)
	{
		retry.IntValue = request.IntValue + 1;
	}
	PushRequest(retry);
	return true;
}

void CViewerSession::RequestHandler_LevelViewer(const SViewerRequest& request)
{
	switch (request.Type)
	{
	case EViewerRequest::StartSession:
		if (request.Preset.Role == EViewerRole::Client && request.Preset.RemoteServer == nullptr)
		{
			CViewerSession* server = ViewerSessionAt(0);
			if (server != nullptr && server->IsActive() && server->GetLevel()->Server != nullptr && server->GetLevel()->Server->game != nullptr)
			{
				SViewerRequest linked = request;
				linked.Preset.RemoteServer = server->GetLevel()->Server;
				HandleStart(linked.Preset);
			}
			else if (request.IntValue < 600)
			{
				SViewerRequest retry = request;
				retry.IntValue = request.IntValue + 1;
				PushRequest(retry);
			}
			else
			{
				Msg("! Level viewer: timed out waiting for the server before starting the client");
			}
		}
		else
		{
			HandleStart(request.Preset);
		}
		break;
	case EViewerRequest::StopSession:
		HandleStop();
		break;
	case EViewerRequest::SpawnSection:
		if (RetryRequestWhileUnavailable(request))
		{
			return;
		}
		SpawnSectionNow(request.Section);
		break;
	case EViewerRequest::SpawnActor:
		if (RetryRequestWhileUnavailable(request))
		{
			return;
		}
		SpawnActorNow();
		break;
	case EViewerRequest::SetRenderPass:
		SetRenderPassNow(request.Section, request.BoolValue);
		break;
	case EViewerRequest::SetWireframe:
		SetWireframeNow(u32(request.IntValue), u16(request.HeightValue));
		break;
	case EViewerRequest::PrimaryTest:
		PrimaryTestNow(request.Section);
		break;
	case EViewerRequest::SetInspector:
		SetInspectorNow(request.Section, request.BoolValue);
		break;
	case EViewerRequest::SetResource:
		SetResourceNow(ELevelResource(1u << request.IntValue), request.BoolValue);
		break;
	case EViewerRequest::SetPatrolPaths:
		SetPatrolPathsNow(request.BoolValue);
		break;
	case EViewerRequest::SetMode:
	{
		if (ModeInstance != nullptr)
			ModeInstance->OnExit(*this);
		ViewerState.ModeIndex = request.IntValue;
		ModeInstance = CreateViewerLegacyMode(ViewerState.ModeIndex);
		if (ModeInstance != nullptr)
			ModeInstance->OnEnter(*this);
		break;
	}
	case EViewerRequest::SetWeatherCycle:
		SetWeatherCycleNow(request.Section);
		break;
	case EViewerRequest::SetWander:
		if (CViewerModeGameplay* gameplay = dynamic_cast<CViewerModeGameplay*>(Mode()))
			gameplay->WanderNpcs = request.BoolValue;
		break;
	case EViewerRequest::RunSpawnTest:
		RunSpawnTestNow();
		break;
	case EViewerRequest::VerifyPatrolPaths:
		if (RetryRequestWhileUnavailable(request))
		{
			return;
		}
		VerifyPatrolPathsNow(request.BoolValue);
		break;
	case EViewerRequest::TestActorMovement:
		if (RetryRequestWhileUnavailable(request, true))
		{
			return;
		}
		BeginActorMovementTest(request.BoolValue);
		break;
	case EViewerRequest::Screenshot:
		ScreenshotCountdown = std::max(0, request.IntValue);
		Msg("* Level viewer: screenshot scheduled in %d frames", ScreenshotCountdown);
		break;
	case EViewerRequest::VerifyRender:
	{
		// IntValue is a wait budget in ticks (re-queued until it runs out), so
		// the dump can be scheduled past deferred spawns/arming.
		if (request.IntValue > 0)
		{
			SViewerRequest later = request;
			later.IntValue = request.IntValue - 1;
			PushRequest(later);
			return;
		}
		// Not active yet: retry next tick until the session comes up.
		if (!IsActive())
		{
			PushRequest(request);
			return;
		}
		if (!Renderer.IsReady())
		{
			Msg("! viewer_rt_verify: no active session with a viewport render target");
			break;
		}
		string_path outPath;
		FS.update_path(outPath, "$logs$", request.Section[0] ? request.Section : "viewer_rt.bmp");
		const float coverage = Renderer.VerifyReadback(outPath);
		Msg("* viewer_rt_verify: non-black scene coverage %.1f%% -> %s (dump: %s)",
			coverage * 100.f, coverage > 0.01f ? "PASS" : "FAIL", outPath);
		break;
	}
	case EViewerRequest::DumpTerrainTexture:
		if (Render != nullptr)
		{
			u32 size = 0;
			const char* textureName = xr_strcmp(request.Section, "sky.bmp") == 0 ? "sky\\sky_7_cube" : ViewerSessionDetail::TerrainTextureName();
			IRHISurface* surface = Render->load_texture(textureName, size, false);
			if (surface != nullptr)
			{
				Renderer.DumpSurfaceToBmp(surface, request.Section[0] ? request.Section : "ground.bmp");
				surface->Release();
			}
		}
		break;
	case EViewerRequest::DumpSceneTargets:
	{
		CLevelSlotGuard guard(this, SessionLevel, ViewerState.Primary);
		if (Render != nullptr)
		{
			constexpr const char* names[] = { "final", "color", "normal", "position", "accumulator", "terrain_texture" };
			for (const char* name : names)
			{
				IRHIShaderResourceView* view = Render->get_scene_texture(name);
				if (view == nullptr || view->GetSurface() == nullptr)
				{
					Msg("! viewer_dumptargets: '%s' is unavailable", name);
					continue;
				}
				string64 path;
				xr_sprintf(path, "viewer_%s.bmp", name);
				Renderer.DumpSurfaceToBmp(view->GetSurface(), path);
			}
		}
		break;
	}
	case EViewerRequest::SetWindowSize:
		if (request.IntValue > 0 && request.HeightValue > 0)
		{
			string64 title;
			xr_sprintf(title, "%s##LevelViewer%u", Mode() != nullptr ? Mode()->Name() : "Level Sandbox", SessionIndex);
			ImGui::SetWindowSize(title, ImVec2(float(request.IntValue), float(request.HeightValue)));
		}
		break;
	case EViewerRequest::SetHudEnabled:
		SetHudEnabledNow(request.BoolValue);
		break;
	case EViewerRequest::VerifyHud:
		if (RetryRequestWhileUnavailable(request, true))
		{
			return;
		}
		if (Preset.SpawnActorWithHud && Preset.ArmActor && request.IntValue < 600 &&
			(PendingArmWeaponId != u16(-1) || SessionPlayerHud == nullptr || SessionPlayerHud->attached_item(0) == nullptr || SessionActor->inventory().ActiveItem() == nullptr))
		{
			SViewerRequest retry = request;
			++retry.IntValue;
			PushRequest(retry);
			return;
		}
		VerifyHudNow();
		break;
	case EViewerRequest::ToggleInventory:
		if (RetryRequestWhileUnavailable(request, true))
		{
			return;
		}
		if (request.HeightValue > 0)
		{
			SViewerRequest delayed = request;
			--delayed.HeightValue;
			PushRequest(delayed);
			return;
		}
		ToggleInventoryNow(request.Section);
		break;
	case EViewerRequest::DumpSwapchain:
		if (ViewerState.Session == EViewerSessionState::Starting)
		{
			PushRequest(request);
			return;
		}
		if (request.HeightValue > 0)
		{
			SViewerRequest delayed = request;
			--delayed.HeightValue;
			PushRequest(delayed);
			return;
		}
		CViewerRenderer::DumpRTVToBmp(GRHI->DevicePtr->SwapChainRTV, request.Section[0] ? request.Section : "swapdump.bmp");
		break;
	case EViewerRequest::SetViewportSize:
		if (request.HeightValue > 0)
		{
			ViewerState.ViewportSize = 2;
			ViewerState.ViewportWidth = std::max(1, request.IntValue);
			ViewerState.ViewportHeight = std::max(1, request.HeightValue);
		}
		else
		{
			ViewerState.ViewportSize = request.IntValue == 0 ? 0 : 1;
			const u32 dimension = static_cast<u32>(std::max(1, std::min(Device.Width, Device.Height))) / (ViewerState.ViewportSize == 0 ? 3u : 2u);
			ViewerState.ViewportWidth = dimension;
			ViewerState.ViewportHeight = dimension;
		}
		if (ViewerState.Session != EViewerSessionState::Idle &&
			(Renderer.ViewportWidth() != ViewerState.ViewportWidth || Renderer.ViewportHeight() != ViewerState.ViewportHeight))
			Renderer.Init(ViewerState.ViewportWidth, ViewerState.ViewportHeight);
		break;
	case EViewerRequest::SetSimulationPaused:
		ViewerState.SimulationPaused = request.BoolValue;
		ViewerState.InputCapture = false;
		UpdateGameInput();
		Msg("* viewer_pause: paused=%d ticks=%llu", ViewerState.SimulationPaused, static_cast<unsigned long long>(ViewerState.SimulationTicks));
		break;
	case EViewerRequest::SetPresentationEnabled:
		ViewerState.PresentationEnabled = request.BoolValue;
		ViewerState.InputCapture = false;
		UpdateGameInput();
		Msg("* viewer_presentation: enabled=%d ticks=%llu", ViewerState.PresentationEnabled, static_cast<unsigned long long>(ViewerState.SimulationTicks));
		break;
	case EViewerRequest::RunResourceMatrix:
	{
		const int result = RunResourceMatrixNow();
		if (result == 0 && request.IntValue < 300)
		{
			SViewerRequest retry = request;
			++retry.IntValue;
			PushRequest(retry);
		}
		else if (result == 0)
		{
			Msg("! viewer_resource_matrix: resources remained loading after 300 ticks -> FAIL");
		}
		break;
	}
	case EViewerRequest::EditorCommand:
		if (RetryRequestWhileUnavailable(request))
			return;
		{
			CLevelSlotGuard guard(this, GetLevel(), ViewerState.Primary);
			if (Mode() != nullptr && !Mode()->HandleRequest(*this, EViewerEditorCommand(request.IntValue), request.Path, request.FloatValue))
				Msg("! %s: editor command %d was rejected", Mode()->Name(), request.IntValue);
		}
		break;
	default:
		break;
	}
}

// ---------------------------------------------------------------------------
// Session lifecycle
// ---------------------------------------------------------------------------

void CViewerSession::HandleStart(const SViewerPreset& preset)
{
	if (ViewerState.Session != EViewerSessionState::Idle)
		HandleStop(false);
	if (preset.Role == EViewerRole::Client)
	{
		if (preset.RemoteServer == nullptr)
		{
			Msg("! Level viewer: a client session requires an explicit server session.");
			return;
		}
		CLevel* serverLevel = preset.RemoteServer->GetOwnerLevel();
		if (serverLevel == nullptr || serverLevel->Services.ScriptEngine == nullptr ||
			serverLevel->Services.AI == nullptr || serverLevel->Services.AI->GetSharedScriptEngine() == nullptr)
		{
			Msg("! Level viewer: a client session requires the active server's shared script engine.");
			return;
		}
	}

	EmbeddedResources.Reset();
	const bool prebuilt = preset.Terrain.Kind == Sandbox::ETerrainKind::PrebuiltLevel;

	// Resolve the level folder. Embedded presets use the stock VFS/loader path;
	// other procedural presets are generated as needed, and prebuilt retail
	// levels are referenced without generating anything.
	xr_string folder;
	if (prebuilt)
	{
		folder = preset.Terrain.PrebuiltLevel;
		while (!folder.empty() && (folder.back() == '/' || folder.back() == '\\'))
			folder.pop_back();
		if (folder.empty())
		{
			Msg("! Level viewer: prebuilt level name is empty.");
			return;
		}
	}
	else
	{
		folder = ViewerSessionDetail::MakePresetFolderName(preset);
	}

	if (const Sandbox::EmbeddedLevel* embedded = Sandbox::FindEmbeddedLevel(folder.c_str()); embedded != nullptr)
	{
		if (!EmbeddedResources.Install(*embedded))
		{
			Msg("! Level viewer: cannot register embedded level '%s'.", folder.c_str());
			EmbeddedResources.Reset();
			return;
		}
		Msg("* Level viewer: embedded level '%s' registered (%u files)", folder.c_str(), unsigned(embedded->Count));
	}
	else if (!prebuilt && preset.Role != EViewerRole::Client)
	{
		Sandbox::RemoveLevel(folder.c_str());
		if (!Sandbox::BuildLevel(preset.Terrain, folder.c_str(), preset.Resources))
		{
			Msg("! Level viewer: Sandbox::BuildLevel failed for folder '%s'.", folder.c_str());
			return;
		}
	}

	// Verify the level is registered exactly like the stock 'start' flow will
	// look it up: the options string carries only the folder name, so the
	// version falls back to parse_level_version()'s implicit "1.0" (the archive
	// version compare in Level_ID does not apply to plain filesystem folders).
	pApp->Level_Scan();
	if (pApp->Level_ID(folder.c_str(), "1.0", false) == -1)
	{
		Msg("! Level viewer: level '%s' is not registered after Level_Scan.", folder.c_str());
		EmbeddedResources.Reset();
		return;
	}

	bool useAlife = prebuilt && preset.StartAlife && preset.Role != EViewerRole::Client;
	if (useAlife)
	{
		const char* spawnFile = preset.AlifeSpawnFile.c_str();
		const bool explicitPath = strchr(spawnFile, ':') != nullptr || strchr(spawnFile, '/') != nullptr || strchr(spawnFile, '\\') != nullptr;
		string_path spawnPath;
		const bool spawnExists = explicitPath ? std::filesystem::is_regular_file(spawnFile) : FS.exist(spawnPath, "$game_spawn$", spawnFile, ".spawn");
		if (!spawnExists)
		{
			Msg("! Level viewer: ALife spawn '%s' is unavailable; continuing without ALife or level scripts.", spawnFile);
			useAlife = false;
		}
	}

	SViewerPreset configured = preset;
	ModeInstance = CreateViewerEditor(configured.EditorKind);
	if (ModeInstance != nullptr)
		ModeInstance->Configure(configured);
	SLevelDesc desc;
	desc.Resources = configured.Resources;
	desc.Sandbox = !prebuilt;
	desc.RemoteTransport = preset.Role != EViewerRole::Standalone;
	desc.SpawnFile = preset.AlifeSpawnFile.c_str();
	desc.Resources.set(u32(ELevelResource::LevelScripts), useAlife || preset.Role == EViewerRole::Client || desc.Resources.test(u32(ELevelResource::LevelScripts)) || (preset.SpawnActor && preset.SpawnActorWithHud));

	Preset = configured;
	WindowState.Preset = configured;
	ViewerState.RequestedEditorKind = configured.EditorKind;
	if (ModeInstance != nullptr)
		ModeInstance->OnEnter(*this);
	SessionReadyNotified = false;
	CameraInitialized = false;
	ViewerState.SimulationPaused = false;
	ViewerState.PresentationEnabled = true;
	ViewerState.SimulationTicks = 0;
	ViewerState.LightingOverride = !prebuilt;
	if (useAlife)
	{
		Preset.SpawnActor = false;
		Preset.ArmActor = false;
		Msg("* Level viewer: starting a full ALife new game using '%s'", preset.AlifeSpawnFile.c_str());
	}
	SessionFolder = folder;
	ViewerState.Session = EViewerSessionState::Starting;
	ActorSpawnAttempted = false;

	// Dedicated GPU pool for viewer-owned geometry; budget from the cvar.
	GpuPool.Init(Preset.Memory.GpuBytes);
	// The target follows the docked viewport without changing the primary targets.
	Renderer.Init(ViewerState.ViewportWidth, ViewerState.ViewportHeight);
	if (!g_dedicated_server)
	{
		SessionRender = Render != nullptr ? Render->create_scene() : nullptr;
		if (SessionRender == nullptr)
		{
			Msg("! Level viewer: the active renderer does not support independent level scenes.");
			ViewerState.Session = EViewerSessionState::Idle;
			SessionFolder.clear();
			Renderer.Shutdown();
			GpuPool.Shutdown();
			EmbeddedResources.Reset();
			return;
		}
		Msg("* Level viewer: independent render scene '%s' created", SessionRender->scene_resource_prefix());
	}

	string1024 opServer;
	if (preset.Role == EViewerRole::Client)
		opServer[0] = 0;
	else
		xr_strconcat(opServer, folder.c_str(), useAlife ? "/single/alife/new" : "/single");

	// Prebuilt (real level): run as a parallel session too, NOT the stock primary
	// chain. The menu and the current game stay untouched while the level loads
	// through the session-local stage pump; its compiled geometry, skydome,
	// lightmaps and spawn all render through the stock deferred pipeline 1:1.
	// (The renderer's one scene graph is free whenever no game is running.)
	ViewerState.Primary = false;
	desc.Parallel = true;
	// Keep scene-only startup light. A player spawn can initialize the stock HUD
	// later through the same load/connect path used by a normal new game.
	if (!prebuilt)
		desc.Resources.set(u32(ELevelResource::Hud), preset.SpawnActor && preset.SpawnActorWithHud);
	Msg("* Level viewer: creating parallel level object...");
	CLevel* level = nullptr;
	{
		// The level ctor chain registers itself through g_pGameLevel (e.g.
		// pure_relcase member ctors). Let it, then hand the slot back to the
		// current owner: a parallel session must not claim the global. The
		// pending descriptor makes the ctor chain see Sandbox/Parallel/the
		// resource mask from the very first member init.
		xrCriticalSectionGuard schedulerGuard(&Engine.Sheduler.UpdateMutex());
		pApp->SetNextLevelDesc(desc);
		IGame_Level* previousLevel = g_pGameLevel;
		level = (CLevel*)NEW_INSTANCE(CLSID_GAME_LEVEL);
		g_pGameLevel = previousLevel;
	}
	if (level == nullptr)
	{
		Msg("! Level viewer: failed to create the level object.");
		ReleaseSessionRender();
		Renderer.Shutdown();
		GpuPool.Shutdown();
		EmbeddedResources.Reset();
		ViewerState.Session = EViewerSessionState::Idle;
		SessionFolder.clear();
		return;
	}
	Msg("* Level viewer: level object created, setting descriptor...");
	level->SetRemoteServer(preset.Role == EViewerRole::Client ? preset.RemoteServer : nullptr);
	level->SetLevelDesc(desc);
	level->ContextDispatcher.bind(this, &CViewerSession::RunInContext);

	if (useAlife)
	{
		xr_string spawnFile = preset.AlifeSpawnFile;
		std::replace(spawnFile.begin(), spawnFile.end(), '/', '\\');
		string1024 gameParams;
		xr_strconcat(gameParams, spawnFile.c_str(), "/single/alife/new");
		SessionGameParams.parse_cmd_line(gameParams);
	}
	else
	{
		if (preset.Role == EViewerRole::Client)
		{
			string1024 gameParams;
			xr_strconcat(gameParams, folder.c_str(), "/single");
			SessionGameParams.parse_cmd_line(gameParams);
		}
		else
		{
			SessionGameParams.parse_cmd_line(opServer);
		}
	}
	SessionGameParams.m_e_game_type = ParseStringToGameType("single");
	xr_shared_ptr<CScriptEngine> sharedScriptEngine;
	CALifeSimulator* sharedAlife = nullptr;
	if (preset.Role == EViewerRole::Client)
	{
		CAI_Space* serverAi = preset.RemoteServer->GetOwnerLevel()->Services.AI;
		sharedScriptEngine = serverAi != nullptr ? serverAi->GetSharedScriptEngine() : nullptr;
		if (game_sv_Single* single = preset.RemoteServer->game != nullptr ? preset.RemoteServer->game->cast_game_sv_single() : nullptr)
			sharedAlife = single->GetAlifeSimulator();
	}
	SessionAiSpace = new CAI_Space();
	SessionAiSpace->SetSharedScriptEngine(sharedScriptEngine);
	{
		CLevelSlotGuard scriptGuard(this, level, false);
		if (sharedScriptEngine != nullptr)
			g_pScriptEngine = sharedScriptEngine.get();
		SessionAiSpace->init(preset.Role != EViewerRole::Client);
		if (sharedAlife != nullptr)
			SessionAiSpace->SetAlifeBorrowed(sharedAlife);
		if (preset.Role != EViewerRole::Client && !useAlife)
		{
			luabind::functor<void> initializeNewGame;
			if (SessionAiSpace->script_engine().functor("alife_storage_manager.CALifeStorageManager_new_game", initializeNewGame))
				initializeNewGame(preset.AlifeSpawnFile.c_str());
		}
	}

	SessionSpatial = new ISpatial_DB();
	SessionSpatialPhysic = new ISpatial_DB();
	{
		CLevelSlotGuard guard(this, level, false);
		SessionPlayerHud = new player_hud();
		SessionPlayerHud->load_default();
		SessionEnvironment = new CEnvironment(true);
		SessionEnvironment->load();
		SessionEnvironment->SetGameTime(12.f * 3600.f, 1.f);
		const char* weather = !Preset.WeatherCycle.empty() && SessionEnvironment->WeatherCycles.contains(Preset.WeatherCycle.c_str()) ? Preset.WeatherCycle.c_str()
			: SessionEnvironment->WeatherCycles.contains("default_clear") ? "default_clear"
			: SessionEnvironment->WeatherCycles.empty() ? nullptr : SessionEnvironment->WeatherCycles.begin()->first.c_str();
		if (weather != nullptr)
		{
			xr_strcpy(ViewerState.WeatherCycle, weather);
			SessionEnvironment->SetWeather(weather, true);
		}
		SessionEnvironment->OnFrame();
		SessionHudManager = (::CHUDManager*)NEW_INSTANCE(CLSID_HUDMANAGER);
	}

	level->Services.AI = SessionAiSpace;
	level->Services.ScriptEngine = SessionAiSpace->get_script_engine();
	level->Services.Environment = SessionEnvironment;
	level->Services.Hud = SessionHudManager;
	level->Services.PlayerHud = SessionPlayerHud;
	level->Services.GameUI = SessionGameUI;
	level->Services.Spatial = SessionSpatial;
	level->Services.SpatialPhysic = SessionSpatialPhysic;
	level->ObjectSpace.SetSpatialDatabases(SessionSpatial, SessionSpatialPhysic);

	Msg("* Level viewer: net_Start...");
	SessionLevel = level;
	bool netStartOk = false;
	{
		CLevelSlotGuard guard(this, level, false);
		netStartOk = level->net_Start(opServer, "localhost");
	}
	if (!netStartOk)
	{
		Msg("! Level viewer: net_Start failed.");
		HandleStop();
		return;
	}
	Msg("* Level viewer: net_Start queued");
	Msg("* Level viewer: parallel session '%s' loading...", SessionFolder.c_str());
}

void CViewerSession::ReleaseSessionEnvironment()
{
	if (SessionEnvironment == nullptr)
		return;
	CLevelSlotGuard guard(this, SessionLevel, false);
	SessionEnvironment->unload();
	xr_delete(SessionEnvironment);
}

void CViewerSession::HandleStop(bool cancelRequests)
{
	if (cancelRequests)
		Requests.Clear();
	if (ModeInstance != nullptr)
	{
		CLevelSlotGuard guard(this, GetLevel(), ViewerState.Primary);
		ModeInstance->OnExit(*this);
	}
	if (ViewerState.Session == EViewerSessionState::Idle)
	{
		ModeInstance.reset();
		return;
	}

	if (SessionLevel != nullptr && SessionLevel->Server != nullptr && ViewerSessionDetail::Manager != nullptr)
	{
		for (CViewerSession& linked : ViewerSessionDetail::Manager->Sessions)
		{
			if (&linked != this && linked.Preset.RemoteServer == SessionLevel->Server && linked.State() != EViewerSessionState::Idle)
				linked.HandleStop();
		}
	}

	if (ViewerState.Primary)
	{
		// KERNEL events are FIFO: if our KERNEL:start is still queued, the
		// disconnect executes right after it and tears the session down cleanly.
		if (g_pGameLevel != nullptr || g_pEventManager->Event.Peek("KERNEL:start"))
			g_pEventManager->Event.Defer("KERNEL:disconnect");
	}
	else if (SessionLevel != nullptr)
	{
		SpawnTestPending = false;
		PendingArmWeaponId = u16(-1);
		SessionLevel->scheduled_tasks.clear();
		for (u32 key = 0; key < std::size(CapturedKeys); ++key)
		{
			if (CapturedKeys[key])
				SessionLevel->IR_OnKeyboardRelease(key);
			CapturedKeys[key] = false;
		}
		ActorMovementTimeLeft = -1.f;
		{
			// All destructors, including environment and renderer destruction,
			// resolve their services through this session's compatibility slots.
			CLevelSlotGuard guard(this, SessionLevel, false);
			SessionLevel->ReleaseSoundGeometry();
			SessionLevel->net_Stop();
			DEL_INSTANCE(SessionLevel);
			Msg("* viewer stop: level destroyed");
			xr_delete(SessionAiSpace);
			Msg("* viewer stop: AI space destroyed");
			xr_delete(SessionPlayerHud);
			xr_delete(SessionInspector);
			xr_delete(SessionHudManager);
			ReleaseSessionEnvironment();
			Msg("* viewer stop: environment destroyed");
			if (SessionSpatial != nullptr && precache_light && precache_light->SpatialComponent && precache_light->SpatialComponent->space == SessionSpatial)
				Device.ReleasePrecacheLight();
			xr_delete(SessionSpatial);
			xr_delete(SessionSpatialPhysic);
			ReleaseSessionRender();
			Msg("* viewer stop: render scene destroyed");
		}
	}

	ViewerState.Session = EViewerSessionState::Idle;
	ViewerState.Primary = false;
	SessionFolder.clear();
	SessionLevel = nullptr;
	SessionActor = nullptr;
	SessionActorSingle = nullptr;
	SessionActorItf = nullptr;
	SessionAiSpace = nullptr;
	SessionPhysicsWorld = nullptr;
	SessionPlayerHud = nullptr;
	SessionHudManager = nullptr;
	SessionGameUI = nullptr;
	SessionSpatial = nullptr;
	SessionSpatialPhysic = nullptr;

	// The runtime terrain and targets belong to this session's render scene.
	ReleaseSessionRender();
	Renderer.Shutdown();
	GpuPool.Shutdown();
	EmbeddedResources.Reset();
	ModeInstance.reset();
	SessionReadyNotified = false;
	CameraInitialized = false;
	ViewerState.InputCapture = false;
}

bool CViewerSession::HasLevel() const
{
	if (SessionFolder.empty())
		return false;
	const CLevel* level = ViewerState.Primary ? (const CLevel*)g_pGameLevel : SessionLevel;
	if (level == nullptr || !level->bReady || (level->GetLevelDesc().RemoteTransport && !level->NetStartOk()))
		return false;
	shared_str name = level->name();
	if (!name)
		return false;
	if (_stricmp(name.c_str(), SessionFolder.c_str()) != 0)
	{
		// The stock ALife registry selects the actor's graph level after the
		// initial new-game spawn. That is a level transition, not session loss.
		Msg("* Level viewer: game changed level '%s' -> '%s'", SessionFolder.c_str(), name.c_str());
		SessionFolder = name.c_str();
	}
	return true;
}

bool CViewerSession::IsActive() const
{
	return ViewerState.Session == EViewerSessionState::Active && HasLevel();
}

EResourceState CViewerSession::ResourceState(ELevelResource res) const
{
	CLevel* level = ViewerState.Primary ? (CLevel*)g_pGameLevel : SessionLevel;
	if (!IsActive() || level == nullptr)
		return EResourceState::NotLoaded;
	return level->GetResourceState(res);
}

bool CViewerSession::SetResourceNow(ELevelResource resource, bool enabled)
{
	if (!IsActive())
	{
		Msg("! Level viewer: resource toggle ignored - no active sandbox session");
		return false;
	}
	CLevelSlotGuard slotGuard(this, GetLevel(), ViewerState.Primary);
	if (resource == ELevelResource::RenderScene && !enabled)
		ReleaseRuntimeTerrainScene();
	const bool result = GetLevel()->SetResourceLoaded(resource, enabled);
	if (!ViewerState.Primary && resource == ELevelResource::Hud)
	{
		SessionHudManager = static_cast<::CHUDManager*>(g_hud);
		SessionGameUI = SessionHudManager != nullptr ? SessionHudManager->GetGameUI() : g_pGameCustom;
	}
	if (resource == ELevelResource::RenderScene && enabled && !GetLevel()->HasRenderScene)
		UploadTerrainToGpuPool();
	Msg("* Level viewer: resource '%s' %s -> %s (%s)", LevelResourceName(resource).data(), enabled ? "load" : "unload",
		LevelResourceStateName(GetLevel()->GetResourceState(resource)).data(), result ? "OK" : "unavailable");
	return result;
}

bool CViewerSession::SetPatrolPathsNow(bool enabled)
{
	return SetResourceNow(ELevelResource::PatrolPaths, enabled);
}

void CViewerSession::VerifyPatrolPathsNow(bool expectedLoaded)
{
	CLevelSlotGuard guard(this, SessionLevel, ViewerState.Primary);
	if (!IsActive())
	{
		Msg("! viewer_test_patrol: no active session -> FAIL");
		return;
	}
	const CPatrolPathStorage::PATROL_REGISTRY& paths = ai().patrol_paths().patrol_paths();
	u32 points = 0;
	for (const auto& [name, path] : paths)
	{
		if (path != nullptr)
			points += path->vertex_count();
	}
	const EResourceState state = ResourceState(ELevelResource::PatrolPaths);
	const bool loaded = state == EResourceState::Ready && !paths.empty() && points != 0;
	const bool passed = loaded == expectedLoaded && (expectedLoaded || paths.empty());
	Msg("* viewer_test_patrol: expected=%s state=%s paths=%u points=%u -> %s", expectedLoaded ? "loaded" : "unloaded",
		LevelResourceStateName(state).data(), u32(paths.size()), points, passed ? "PASS" : "FAIL");
}

bool CViewerSession::SetWireframeNow(u32 mode, u16 objectId)
{
	if (!IsActive() || mode > 2 || SessionRender == nullptr)
	{
		Msg("! viewer_wireframe: an active parallel render scene and mode 0, 1, or 2 are required");
		return false;
	}
	ViewerState.WireframeMode = int(mode);
	ViewerState.WireframeObjectId = int(objectId);
	SessionRender->set_wireframe_mode(mode, objectId);
	Msg("* viewer_wireframe: mode=%s object=%u", mode == 0 ? "off" : mode == 1 ? "solid+wire" : "wire-only", objectId);
	return true;
}

bool CViewerSession::SetRenderPassNow(const char* name, bool enabled)
{
	if (ViewerState.ModeIndex != 2)
	{
		if (IViewerMode* oldMode = Mode())
			oldMode->OnExit(*this);
		ViewerState.ModeIndex = 2;
		Mode()->OnEnter(*this);
	}
	ViewerState.RenderPassOverride = true;
	const u32 all = rsDrawStatic | rsDrawDynamic | rsDetails | rsOcclusion | rsWireframe;
	if (0 == _stricmp(name, "all"))
	{
		ViewerState.RenderPassFlags.assign(enabled ? all : 0);
		return true;
	}
	if (0 == _stricmp(name, "wireframe"))
		return SetWireframeNow(enabled ? 2 : 0, u16(ViewerState.WireframeObjectId));
	u32 flag = 0;
	if (0 == _stricmp(name, "static")) flag = rsDrawStatic;
	else if (0 == _stricmp(name, "dynamic")) flag = rsDrawDynamic;
	else if (0 == _stricmp(name, "details")) flag = rsDetails;
	else if (0 == _stricmp(name, "occlusion")) flag = rsOcclusion;
	if (flag == 0)
	{
		Msg("! viewer_render_pass: unknown pass '%s'", name != nullptr ? name : "<null>");
		return false;
	}
	ViewerState.RenderPassFlags.set(flag, enabled);
	return true;
}

bool CViewerSession::SetWeatherCycleNow(const char* name)
{
	if (!IsActive() || SessionEnvironment == nullptr)
	{
		Msg("! viewer_weather: an active session is required");
		return false;
	}
	if (name == nullptr || !SessionEnvironment->WeatherCycles.contains(name))
	{
		Msg("! viewer_weather: cycle '%s' is unavailable", name != nullptr ? name : "<null>");
		return false;
	}
	CLevelSlotGuard guard(this, SessionLevel, false);
	SessionEnvironment->SetWeather(name, true);
	Preset.WeatherCycle = name;
	xr_strcpy(ViewerState.WeatherCycle, name);
	Msg("* Level viewer: weather cycle '%s'", name);
	return true;
}

// ---------------------------------------------------------------------------
// Tick
// ---------------------------------------------------------------------------

void CViewerSession::Tick()
{
	__try
	{
		TickInternal();
	}
	__except (ViewerSessionDetail::LogViewerTickCrash(GetExceptionInformation()))
	{
	}
}

void CViewerSession::RunAutorunOnce()
{
	if (AutorunDone)
		return;
	AutorunDone = true;

	string_path autorunPath;
	FS.update_path(autorunPath, _app_data_root_, "viewer_autorun.ltx");
	IReader* reader = FS.r_open(autorunPath);
	if (reader == nullptr)
		return;

	Msg("* Level viewer: running viewer_autorun.ltx");
	while (!reader->eof())
	{
		xr_string line;
		reader->r_string(line);
		if (line.size() >= sizeof(SDeferredCmd::Text))
		{
			Msg("! Level viewer: autorun entry exceeds the 255-byte command limit and was skipped");
			continue;
		}
		if (line.empty() || line[0] == ';' || line[0] == '#')
			continue;

		// "@<ticks> <command>" uses the duration of N ticks at 60 Hz, so test
		// timing does not change with the render frame rate.
		if (line[0] == '@')
		{
			u32 ticks = 0;
			char cmd[256] = {};
			if (sscanf(line.c_str() + 1, "%u %255[^\n]", &ticks, cmd) == 2 && DeferredCmdCount < std::size(DeferredCmds))
			{
				SDeferredCmd& slot = DeferredCmds[(DeferredCmdHead + DeferredCmdCount) % DeferredCapacity];
				++DeferredCmdCount;
				slot.WaitTicks = ticks;
				xr_strcpy(slot.Text, cmd);
			}
			else
			{
				Msg("! Level viewer: deferred autorun entry rejected (invalid syntax or full queue): %s", line.c_str());
			}
			continue;
		}
		Console->Execute(line.c_str());
	}
	FS.r_close(reader);
}

void CViewerSession::PumpDeferredCmds()
{
	for (u32 index = 0; index < CViewerSessionManager::MaxSessions; ++index)
	{
		if (CViewerSession* session = ViewerSessionAt(index); session != nullptr && (session->State() == EViewerSessionState::Starting || session->ActorMovementInProgress()))
			return;
	}
	if (DeferredCmdCount == 0)
		return;

	SDeferredCmd& next = DeferredCmds[DeferredCmdHead];
	if (next.WaitTicks > 1)
	{
		--next.WaitTicks;
		return;
	}

	const SDeferredCmd command = next;
	DeferredCmdHead = (DeferredCmdHead + 1) % DeferredCapacity;
	--DeferredCmdCount;
	Msg("* Level viewer: deferred '%s'", command.Text);
	Console->Execute(command.Text);
}

void CViewerSession::TickInternal()
{
	const u32 now = Device.TimerAsync_MMT();
	if (PreviousTickMs == 0)
		PreviousTickMs = now;
	TickDeltaSeconds = std::min(float(now - PreviousTickMs) * 0.001f, 0.1f);
	PreviousTickMs = now;

	if (AutorunOwner)
	{
		RunAutorunOnce();
		PumpDeferredCmds();
	}

	Requests.Drain([this](const SViewerRequest& request)
	{
		RequestHandler_LevelViewer(request);
	});
	if (Lifecycle.RestoreMenu)
	{
		Lifecycle.RestoreMenu = false;
		Console->Execute("main_menu on");
	}
	UiProbeTick();
	TickLifecycleTest();

	CLevelSlotGuard slotGuard(this, SessionLevel, ViewerState.Primary);
	ViewerSessionDetail::CFrameTimeScope frameTime(!ViewerState.Primary, TickDeltaSeconds);

	// Track the deferred session start/stop first.
	switch (ViewerState.Session)
	{
	case EViewerSessionState::Starting:
		if (ViewerState.Primary)
		{
			if (HasLevel())
			{
				ViewerState.Session = EViewerSessionState::Active;
				Msg("* Level viewer: session '%s' is active.", SessionFolder.c_str());
			}
			else if (g_pGameLevel == nullptr && !g_pEventManager->Event.Peek("KERNEL:start"))
			{
				// The deferred start ran and failed (level not found, connect error).
				Msg("! Level viewer: level start did not complete.");
				ViewerState.Session = EViewerSessionState::Idle;
				SessionFolder.clear();
			}
		}
		else if (SessionLevel != nullptr)
		{
			// One stage per Tick, same cadence as the engine's on_idle pump.
			// (g_pGameLevel is already lent to this session for the whole tick
			// by the guard above.)
			const bool stillLoading = SessionLevel->PumpSandboxLoading();

			if (!stillLoading)
			{
				
				if (SessionLevel->NetStartOk() && SessionLevel->bReady)
				{
					ViewerState.Session = EViewerSessionState::Active;
					Msg("* Level viewer: parallel session '%s' is active.", SessionFolder.c_str());
					if (!SessionReadyNotified && Mode() != nullptr)
					{
						Mode()->OnSessionReady(*this);
						SessionReadyNotified = true;
					}
					if (!CameraInitialized)
					{
						ViewerState.CamPos.y = Sandbox::SampleHeight(Preset.Terrain, 0.f, 0.f) + 30.f;
						CameraInitialized = true;
					}
					UploadTerrainToGpuPool();
				}
				else
				{
					Msg("! Level viewer: parallel session failed to load.");
					HandleStop();
					return;
				}
			}
		}
		break;
	case EViewerSessionState::Active:
		if (!IsActive())
		{
			Msg("! [viewer] active check failed: state=%u folder='%s' level=%p ready=%d name='%s'",
				u32(ViewerState.Session), SessionFolder.c_str(), SessionLevel,
				SessionLevel != nullptr && SessionLevel->bReady,
				SessionLevel != nullptr ? SessionLevel->name().c_str() : "<null>");
			HandleStop();
			return;
		}
		break;
	default:
		break;
	}

	if (!IsActive())
	{
		RefreshStateSnapshot();
		return;
	}
	if (ViewerState.SimulationPaused)
	{
		ViewerState.InputCapture = false;
		UpdateGameInput();
		RefreshStateSnapshot();
		return;
	}

	if (SpawnTestPending)
	{
		SpawnTestPending = false;
		RunSpawnTestNow();
	}

	CLevel* level = GetLevel();

	// Deferred one-shot actor spawn: wait until the client game object exists
	// (it is created by M_SV_CONFIG_NEW_CLIENT during the load handshake).
	if (Preset.SpawnActor && !ActorSpawnAttempted && level != nullptr && level->game != nullptr)
	{
		ActorSpawnAttempted = true;
		SpawnActorNow();
	}

	UpdateArmActor();

	if (!ViewerState.Primary && GActorInterface != nullptr)
	{
		GActorInterface->UpdatePlayerHud();
	}

	// A parallel session is not on any engine sequence: pump it here, on the
	// primary thread, with the same per-frame cadence the engine would use.
	if (!ViewerState.Primary && level != nullptr)
	{
		GamePersistent().OnLevelFlow(*level);
		level->OnFrame();
		GamePersistent().OnWeatherFrame();
		level->SoundEvent_Dispatch();
		if (SessionHudManager != nullptr)
		{
			SessionHudManager->OnFrameMT();
		}
		g_pGamePersistent->OnFrameMT();
		g_pGamePersistent->UpdateParticles(level);
		level->net_Update();
		if (SessionRender != nullptr)
			SessionRender->update_scene();
		// The global scheduler skips this session's objects everywhere else
		// (owner filter in CSheduler); run their shedule_Update here, with the
		// level slot lent by the guard at the top of TickInternal.
		Engine.Sheduler.Update();
		// The session's physics world is not on the global seqFrame(MT) either
		// (it would be stepped by the GameThread without this session's
		// context); step it here, inline, after object updates.
		if (SessionPhysicsWorld != nullptr)
			SessionPhysicsWorld->OnFrame();
		UpdateActorMovementTest();
		++ViewerState.SimulationTicks;
	}

	const bool windowVisible = ViewerState.PresentationEnabled && (SessionIndex == 0 ? ViewerWindowWasVisible() : ViewerSecondWindowVisible());
	if (!windowVisible && !ViewerState.Primary)
	{
		ViewerState.InputCapture = false;
		UpdateGameInput();
		RefreshStateSnapshot();
		return;
	}

	if (ScreenshotCountdown >= 0)
	{
		if (ScreenshotCountdown == 0)
		{
			ScreenshotCountdown = -1;
			Console->Execute("screenshot");
		}
		else
			--ScreenshotCountdown;
	}

	UpdateFreeCamera();
	if (Renderer.IsReady())
	{
		if (ViewerState.Session != EViewerSessionState::Idle &&
			(Renderer.ViewportWidth() != ViewerState.ViewportWidth || Renderer.ViewportHeight() != ViewerState.ViewportHeight))
			Renderer.Init(ViewerState.ViewportWidth, ViewerState.ViewportHeight);
	}
	// The same renderer handles environment-only passes and complete scenes.
	const bool passDrivesViewport = IsActive() && windowVisible;
	if (!ViewerState.Primary && SessionEnvironment != nullptr && passDrivesViewport)
		SessionEnvironment->OnFrame();
	if (!ViewerState.Primary && IsActive() && !passDrivesViewport)
		Renderer.ClearScene();
	UpdateGameInput();

	if (IViewerMode* mode = Mode())
		mode->Tick(*this);

	RefreshStateSnapshot();
}

void CViewerSession::BeginActorMovementTest(bool backwards)
{
	ActorMovementSucceeded = false;
	ActorMovementLastDistance = -1.f;
	CLevelSlotGuard guard(this, SessionLevel, ViewerState.Primary);
	RefreshStateSnapshot();
	if (!IsActive() || SessionActor == nullptr || SessionLevel == nullptr)
	{
		Msg("! viewer_test_actor: an active session with a bound actor is required");
		return;
	}
	const ClientID clientId = SessionLevel->GetClientID();
	const xrClientData* client = SessionLevel->RemoteServerEndpoint() != nullptr ? SessionLevel->RemoteServerEndpoint()->ID_to_client(clientId) : nullptr;
	const ALife::_OBJECT_ID expectedActor = client != nullptr && client->owner != nullptr ? client->owner->ID : ALife::INVALID_OBJECT_ID;
	if (client != nullptr && (expectedActor == ALife::INVALID_OBJECT_ID || expectedActor != SessionActor->ID()))
	{
		Msg("! viewer_test_actor: client-id=%u server assigned actor %u, but the session bound actor %u -> FAIL", clientId.value(), expectedActor, SessionActor->ID());
		return;
	}
	ViewerState.FreeCamActive = false;
	ActorMovementKey = backwards ? SDL_SCANCODE_S : SDL_SCANCODE_W;
	ActorMovementOrigin = SessionActor->Position();
	ActorMovementTimeLeft = 3.f;
	IInputReceiver& input = ViewerSessionDetail::SessionInputReceiver(*SessionLevel);
	input.IR_OnKeyboardPress(SDL_SCANCODE_RETURN);
	input.IR_OnKeyboardRelease(SDL_SCANCODE_RETURN);
	Msg("* viewer_test_actor: transport=%d client-id=%u expected=%u actor=%u view=%u server=%p endpoint=%p; accepted normal continue input; driving stock %s input from [%.2f %.2f %.2f]",
		int(SessionLevel->GetLevelDesc().RemoteTransport), clientId.value(), expectedActor, SessionActor->ID(),
		SessionLevel->CurrentViewEntity() != nullptr ? SessionLevel->CurrentViewEntity()->ID() : ALife::INVALID_OBJECT_ID,
		SessionLevel->Server, SessionLevel->RemoteServerEndpoint(), backwards ? "backward" : "forward", ActorMovementOrigin.x, ActorMovementOrigin.y, ActorMovementOrigin.z);
}

void CViewerSession::UpdateActorMovementTest()
{
	if (ActorMovementTimeLeft == -1.f)
		return;
	if (!IsActive() || SessionActor == nullptr || SessionLevel == nullptr)
	{
		Msg("! viewer_test_actor: actor or session disappeared during the test");
		ActorMovementTimeLeft = -1.f;
		return;
	}
	if (ActorMovementTimeLeft > 0.f)
	{
		const bool firstTick = ActorMovementTimeLeft == 3.f;
		IInputReceiver& input = ViewerSessionDetail::SessionInputReceiver(*SessionLevel);
		input.IR_OnKeyboardHold(ActorMovementKey);
		if (firstTick)
		{
			CSE_Abstract* serverEntity = SessionLevel->Server != nullptr ? SessionLevel->Server->ID_to_entity(SessionActor->ID()) : nullptr;
			Msg("* viewer_test_actor: input-key=%s binding=%d input-disabled=%d receiver=%p level=%p entity=%p control=%p focus-locked=%d remote=%d alive=%d ui=%p external-handler=%d local=%d as-player=%d update-frame=%u real-state=%x",
				ActorMovementKey == SDL_SCANCODE_S ? "S" : "W", get_binded_action(ActorMovementKey), SessionLevel->InputDisabled, &input, SessionLevel, SessionLevel->CurrentEntity(), SessionLevel->CurrentControlEntity(),
				SessionActor->IsCameraFocusInputLocked(), SessionActor->Remote(), SessionActor->g_Alive(), CurrentGameUI(), SessionActor->input_external_handler_installed(),
				SessionActor->Local(), serverEntity != nullptr && serverEntity->s_flags.test(M_SPAWN_OBJECT_ASPLAYER), SessionActor->dwFrame_UpdateCL, SessionActor->GetMovementState(eReal));
			Msg("* viewer_test_actor: action-blocked=%d wishful-after-hold=%x primary-paused=%d session-dt=%.5f", SessionLevel->is_block_action(get_binded_action(ActorMovementKey)), SessionActor->GetMovementState(eWishful), Device.Paused(), Device.fTimeDelta);
		}
		const float previousTimeLeft = ActorMovementTimeLeft;
		ActorMovementTimeLeft -= TickDeltaSeconds;
		if (iFloor(ActorMovementTimeLeft) < iFloor(previousTimeLeft))
		{
			const Fvector& position = SessionActor->Position();
			Msg("* viewer_test_actor: remaining %.3f s, dt %.6f, position [%.3f %.3f %.3f]", ActorMovementTimeLeft, TickDeltaSeconds, position.x, position.y, position.z);
		}
		return;
	}

	SessionLevel->IR_OnKeyboardRelease(ActorMovementKey);
	const Fvector& position = SessionActor->Position();
	const float distance = ActorMovementOrigin.distance_to(position);
	const float heightDifference = std::abs(position.y - ActorMovementOrigin.y);
	const bool stockCameraBound = SessionLevel->CurrentViewEntity() == SessionActor && !ViewerState.FreeCamActive && SessionActor->active_cam() <= eacLookAt;
	const bool collisionReady = SessionLevel->GetResourceState(ELevelResource::Collision) == EResourceState::Ready;
	const bool introActive = SessionLevel->Flow.Intro != nullptr || !SessionLevel->Flow.IntroEvent.empty();
	Msg("* viewer_test_actor: wishful=%x can-move=%d talking=%d intro-active=%d animator=%d",
		SessionActor->GetMovementState(eWishful), SessionActor->CanMove(), SessionActor->IsTalking(),
		introActive, SessionActor->HudAnimator() != nullptr && SessionActor->HudAnimator()->IsAnyAnimatorActive());
	const bool passed = distance > 0.25f && heightDifference < 4.f && stockCameraBound && collisionReady && SessionPhysicsWorld != nullptr;
	ActorMovementLastDistance = distance;
	ActorMovementSucceeded = passed;
	Msg("* viewer_test_actor: distance %.3f m, height delta %.3f m, stock camera mode %d %s, collision %s -> %s",
		distance, heightDifference, int(SessionActor->active_cam()), stockCameraBound ? "bound" : "NOT bound", collisionReady ? "ready" : "missing", passed ? "PASS" : "FAIL");
	ActorMovementTimeLeft = -1.f;
}

void CViewerSession::UpdateGameInput()
{
	if (ViewerState.Primary)
		return;
	if (!ViewerState.InputCapture || !IsActive() || SessionLevel == nullptr || !GImGUIManager->IsCapturingInputs())
	{
		for (u32 key = 0; key < std::size(CapturedKeys); ++key)
		{
			if (CapturedKeys[key])
				SessionLevel->IR_OnKeyboardRelease(key);
			CapturedKeys[key] = false;
		}
		return;
	}

	IInputReceiver& input = ViewerSessionDetail::SessionInputReceiver(*SessionLevel);
	for (u32 key = 0; key < std::size(CapturedKeys); ++key)
	{
		const bool down = pInput->iGetAsyncKeyState(key);
		if (down && !CapturedKeys[key])
			input.IR_OnKeyboardPress(key);
		if (down)
			input.IR_OnKeyboardHold(key);
		else if (CapturedKeys[key])
			input.IR_OnKeyboardRelease(key);
		CapturedKeys[key] = down;
	}

	const ImGuiIO& io = ImGui::GetIO();
	if (io.MouseDelta.x != 0.f || io.MouseDelta.y != 0.f)
		input.IR_OnMouseMove(iFloor(io.MouseDelta.x), iFloor(io.MouseDelta.y));

	for (int btn = 0; btn < 3; ++btn)
	{
		if (ImGui::IsMouseDown(btn))
			input.IR_OnMouseHold(btn);
	}
}

IViewerMode* CViewerSession::Mode()
{
	if (ModeInstance == nullptr)
		ModeInstance = CreateViewerLegacyMode(ViewerState.ModeIndex);
	return ModeInstance.get();
}

bool CViewerSession::ProjectWorldPoint(const Fvector& world, Fvector2& out) const
{
	const Fmatrix& full = ViewerState.Primary ? Device.mFullTransform : ViewerCamFull;
	return ViewerSessionDetail::ProjectToScreen(full, world, out);
}

void CViewerSession::RefreshStateSnapshot()
{
	CLevel* level = GetLevel();
	if (!ViewerState.Primary && IsActive() && level != nullptr)
	{
		CObject* view = level->CurrentViewEntity();
		if (level->Server == nullptr && level->RemoteServerEndpoint() != nullptr)
		{
			const xrClientData* client = level->RemoteServerEndpoint()->ID_to_client(level->GetClientID());
			CObject* assigned = client != nullptr && client->owner != nullptr ? level->Objects.net_Find(client->owner->ID) : nullptr;
			if (assigned != nullptr && assigned->Local())
			{
				if (view != assigned)
				{
					if (CGameObject* current = level->CurrentEntity() != nullptr ? level->CurrentEntity()->cast_game_object() : nullptr)
						current->On_B_NotCurrentEntity();
					level->SetControlEntity(assigned);
					level->SetEntity(assigned);
				}
				view = assigned;
			}
		}
		CActor* player = view != nullptr && view->Local() ? view->cast_actor() : nullptr;
		SessionActor = player;
		SessionActorSingle = player;
		SessionActorItf = player;
	}
	ViewerState.HasRenderScene = IsActive() && level != nullptr && level->HasRenderScene;
	ViewerState.HasActor = SessionActor != nullptr;
	ViewerState.ObjectCount = (IsActive() && level != nullptr) ? level->Objects.o_count() : 0;

	xr_strcpy(ViewerState.LevelFolder, SessionFolder.c_str());

	if (IsActive())
	{
		const ILevelGraph* graph = ai().get_level_graph();
		ViewerState.AiMapPresent = graph != nullptr && graph->has_nodes();
		ViewerState.AiSimulationActive = ai().get_alife() != nullptr;
		for (u32 bit = 0; bit < IGame_Level::ResourceStateCount; ++bit)
			ViewerState.ResourceStates[bit] = level->GetResourceState(ELevelResource(1u << bit));
	}
	else
	{
		ViewerState.AiMapPresent = false;
		ViewerState.AiSimulationActive = false;
		for (u32 bit = 0; bit < IGame_Level::ResourceStateCount; ++bit)
			ViewerState.ResourceStates[bit] = EResourceState::NotLoaded;
	}
}

Fvector CViewerSession::AimPointOnTerrain() const
{
	Fvector dir;
	Fvector pos;
	if (SessionActor != nullptr && !ViewerState.FreeCamActive)
	{
		dir = SessionActor->Cameras().Direction();
		pos.mad(SessionActor->Cameras().Position(), dir, 15.f);
	}
	else
	{
		dir.setHP(ViewerState.CamYaw, ViewerState.CamPitch);
		pos.mad(ViewerState.CamPos, dir, 15.f);
	}

	CLevel* level = GetLevel();
	if (IsActive() && level != nullptr)
	{
		if (level->GetResourceState(ELevelResource::Collision) == EResourceState::Ready)
		{
			const Fbox& box = level->ObjectSpace.GetBoundingVolume();
			if (box.max.x - box.min.x > 4.f && box.max.z - box.min.z > 4.f)
			{
				pos.x = clampr(pos.x, box.min.x + 2.f, box.max.x - 2.f);
				pos.z = clampr(pos.z, box.min.z + 2.f, box.max.z - 2.f);
			}
		}
		Fvector start = pos; start.y += 20.f;
		collide::rq_result result;
		if (level->ObjectSpace.RayPick(start, Fvector().set(0.f, -1.f, 0.f), 200.f, collide::rqtStatic, result, nullptr))
		{
			pos.y = start.y - result.range;
			return pos;
		}
	}
	pos.y = Sandbox::SampleHeight(Preset.Terrain, pos.x, pos.z) + 1.f;
	return pos;
}

bool CViewerSession::SpawnSectionNow(const char* section, ALife::_OBJECT_ID* outObjectId)
{
	CLevel* level = GetLevel();
	if (!IsActive() || level == nullptr)
	{
		Msg("! Level viewer: spawn ignored - no active sandbox session.");
		return false;
	}
	if (Preset.EditorKind == EViewerEditorKind::OgfPreview || Preset.EditorKind == EViewerEditorKind::ParticleEditor || Preset.EditorKind == EViewerEditorKind::MaterialEditor)
	{
		Msg("! %s: gameplay spawning is not part of this preview editor", Mode()->Name());
		return false;
	}
	CLevelSlotGuard slotGuard(this, level, ViewerState.Primary);
	Msg("* Level viewer: spawning '%s' requested", section);
	if (!ViewerSessionDetail::SupportsPhysicalSpawn(*level, SessionPhysicsWorld))
	{
		Msg("! Level viewer: spawn of '%s' rejected: collision and a physics world are required", section);
		return false;
	}
	if (level->RemoteServerEndpoint() == nullptr || level->RemoteServerEndpoint()->game == nullptr)
	{
		Msg("! Level viewer: no in-proc server to spawn through.");
		return false;
	}

	game_sv_GameState* svGame = level->RemoteServerEndpoint()->game;
	const Fvector pos = AimPointOnTerrain();
	CSE_Abstract* entity = level->spawn_item(section, pos, ai().level_graph().vertex_id(pos), ALife::INVALID_OBJECT_ID, true);
	if (entity == nullptr)
	{
		Msg("! Level viewer: unknown section '%s'", section);
		return false;
	}

	ClientID clientId = level->GetClientID();
	if (clientId.value() == 0 && !ViewerSessionDetail::FindLocalClientID(level->RemoteServerEndpoint(), clientId))
	{
		Msg("! Level viewer: no local client on the server.");
		F_entity_Destroy(entity);
		return false;
	}

	CSE_Abstract* spawned = svGame->spawn_end(entity, clientId);
	if (spawned == nullptr)
	{
		Msg("! Level viewer: server rejected spawn of '%s'", section);
		return false;
	}
	if (outObjectId != nullptr)
		*outObjectId = spawned->ID;
	ViewerState.LastSpawnedId = int(spawned->ID);
	Msg("* Level viewer: spawned '%s' (id %u) at [%.1f %.1f %.1f]", section, u32(spawned->ID), pos.x, pos.y, pos.z);
	return true;
}

bool CViewerSession::SpawnActorNow()
{
	CLevel* level = GetLevel();
	if (!IsActive() || level == nullptr)
	{
		Msg("! Level viewer: actor spawn ignored - no active sandbox session.");
		return false;
	}
	if (SessionActor != nullptr)
	{
		Msg("! Level viewer: a player actor already exists.");
		return false;
	}
	if (Preset.EditorKind != EViewerEditorKind::LevelSandbox)
	{
		Msg("! %s: actor spawning is available only in the level sandbox", Mode()->Name());
		return false;
	}
	CLevelSlotGuard slotGuard(this, level, ViewerState.Primary);
	if (Preset.SpawnActorWithHud && !SetResourceNow(ELevelResource::Hud, true))
	{
		Msg("! Level viewer: actor spawn rejected because the stock HUD and script dependencies are unavailable");
		return false;
	}
	if (!ViewerSessionDetail::SupportsPhysicalSpawn(*level, SessionPhysicsWorld))
	{
		Msg("! Level viewer: actor spawn rejected: collision and a physics world are required");
		return false;
	}
	if (level->RemoteServerEndpoint() == nullptr || level->RemoteServerEndpoint()->game == nullptr)
	{
		Msg("! Level viewer: no in-proc server to spawn the actor through.");
		return false;
	}

	ClientID clientId = level->GetClientID();
	if (clientId.value() == 0 && !ViewerSessionDetail::FindLocalClientID(level->RemoteServerEndpoint(), clientId))
	{
		Msg("! Level viewer: no local client on the server.");
		return false;
	}

	game_sv_GameState* svGame = level->RemoteServerEndpoint()->game;
	const Fvector pos = AimPointOnTerrain();
	CSE_Abstract* entity = level->spawn_item("actor", pos, ai().level_graph().vertex_id(pos), ALife::INVALID_OBJECT_ID, true);
	if (entity == nullptr)
	{
		Msg("! Level viewer: 'actor' section is unavailable.");
		return false;
	}

	// Mirror of game_sv_mp::SpawnPlayer minus assign_RP (no respawn points in a
	// sandbox): the ASPLAYER flag makes the client bind this object as the
	// local player (g_actor), exactly like a real game.
	entity->s_flags.assign(M_SPAWN_OBJECT_LOCAL | M_SPAWN_OBJECT_ASPLAYER);

	CSE_Abstract* spawned = svGame->spawn_end(entity, clientId);
	if (spawned == nullptr)
	{
		Msg("! Level viewer: server rejected the actor spawn.");
		return false;
	}
	Msg("* Level viewer: actor spawned (id %u) at [%.1f %.1f %.1f]", u32(spawned->ID), pos.x, pos.y, pos.z);

	if (Preset.ArmActor && Preset.SpawnActorWithHud)
	{
		// The weapon travels the same in-proc spawn path; UpdateArmActor takes
		// it into the inventory and activates its slot once both objects exist.
		CSE_Abstract* weapon = svGame->spawn_begin("wpn_ak74");
		if (weapon != nullptr)
		{
			weapon->o_Position.set(pos);
			CSE_Abstract* spawnedWeapon = svGame->spawn_end(weapon, clientId);
			if (spawnedWeapon != nullptr)
			{
				PendingArmWeaponId = spawnedWeapon->ID;
				ArmWaitFrames = 0;
			}
			else
			{
				F_entity_Destroy(weapon);
				Msg("! Level viewer: server rejected the actor weapon spawn.");
			}
		}
	}
	return true;
}

bool CViewerSession::SetHudEnabledNow(bool enabled)
{
	Preset.SpawnActorWithHud = enabled;
	if (!enabled && SessionGameUI != nullptr)
	{
		static_cast<CUIGameCustom*>(SessionGameUI)->HideShownDialogs();
	}
	if (!IsActive())
	{
		return false;
	}
	return SetResourceNow(ELevelResource::Hud, enabled);
}

bool CViewerSession::VerifyHudNow()
{
	CLevelSlotGuard guard(this, SessionLevel, ViewerState.Primary);
	RefreshStateSnapshot();
	if (!IsActive() || SessionActor == nullptr || SessionLevel == nullptr)
	{
		Msg("! viewer_test_hud: an active session with a bound actor is required");
		return false;
	}
	CUIGameCustom* gameUI = SessionGameUI != nullptr ? static_cast<CUIGameCustom*>(SessionGameUI) : CurrentGameUI();
	if (!Preset.SpawnActorWithHud)
	{
		const bool disabled = SessionLevel->GetResourceState(ELevelResource::Hud) != EResourceState::Ready;
		const bool noMenu = gameUI == nullptr || gameUI->GetActiveInventoryWindow() == nullptr;
		Msg("* viewer_test_hud: HUD and inventory disabled -> %s", disabled && noMenu ? "PASS" : "FAIL");
		return disabled && noMenu;
	}

	const bool hudReady = SessionLevel->HasLevelResource(ELevelResource::Hud);
	const bool hasInventory = gameUI != nullptr && gameUI->ActorMenu() != nullptr;
	const bool hasWeapon = SessionPlayerHud != nullptr && SessionPlayerHud->attached_item(0) != nullptr;
	const bool armed = SessionActor->inventory().ActiveItem() != nullptr;
	const bool weaponReady = !Preset.ArmActor || (hasWeapon && armed);
	const bool scriptActorReady = ScriptBinding::HasActor() && (SessionLevel->GetLevelDesc().RemoteTransport || SessionLevel->HasLevelResource(ELevelResource::LevelScripts));
	luabind::functor<bool> achievement;
	const bool achievementReady = scriptActorReady && ai().script_engine().functor("xr_statistic.wealthy_functor", achievement);
	if (achievementReady)
	{
		achievement();
	}
	const bool passed = hudReady && hasInventory && weaponReady && scriptActorReady && achievementReady;
	Msg("* viewer_test_hud: hud=%d inventory=%d weapon=%d armed=%d script-actor=%d achievements=%d game-ui=%p -> %s", int(hudReady), int(hasInventory), int(hasWeapon), int(armed), int(scriptActorReady), int(achievementReady), gameUI, passed ? "PASS" : "FAIL");
	if (!passed)
	{
		return false;
	}
	SessionGameUI = gameUI;
	SessionLevel->Services.GameUI = gameUI;
	const bool restoreInventory = gameUI->GetActiveInventoryWindow() != nullptr;
	gameUI->HideShownDialogs();
	const bool opened = gameUI->ShowActorMenu() && gameUI->GetActiveInventoryWindow() != nullptr &&
		gameUI->ActorMenu()->GetHolder() == gameUI->GetDialogHolder();
	gameUI->HideShownDialogs();
	const bool closed = gameUI->GetActiveInventoryWindow() == nullptr;
	if (restoreInventory)
	{
		gameUI->ShowActorMenu();
	}
	const bool restored = !restoreInventory || gameUI->GetActiveInventoryWindow() != nullptr;
	Msg("* viewer_test_hud: stock weapon and inventory open/close/restore -> %s", opened && closed && restored ? "PASS" : "FAIL");
	return opened && closed && restored;
}

bool CViewerSession::ToggleInventoryNow(const char* state)
{
	CLevelSlotGuard guard(this, SessionLevel, ViewerState.Primary);
	if (!IsActive() || SessionActor == nullptr || !Preset.SpawnActorWithHud)
	{
		Msg("! viewer_inventory: an active actor with HUD enabled is required");
		return false;
	}
	CUIGameCustom* gameUI = SessionGameUI != nullptr ? static_cast<CUIGameCustom*>(SessionGameUI) : CurrentGameUI();
	if (gameUI == nullptr)
	{
		Msg("! viewer_inventory: the stock game UI is unavailable");
		return false;
	}
	const bool toggle = state == nullptr || state[0] == 0;
	if (!toggle && xr_strcmp(state, "open") != 0 && xr_strcmp(state, "close") != 0)
	{
		Msg("! viewer_inventory: expected open, close, or no state");
		return false;
	}
	const bool open = gameUI->GetActiveInventoryWindow() != nullptr;
	if (toggle ? open : xr_strcmp(state, "close") == 0)
	{
		gameUI->HideActorMenu();
	}
	else if (!open)
	{
		gameUI->ShowActorMenu();
	}
	SessionGameUI = gameUI;
	const bool shown = gameUI->GetActiveInventoryWindow() != nullptr;
	Msg("* Level viewer: inventory %s", shown ? "opened" : "closed");
	return shown;
}

void CViewerSession::UpdateArmActor()
{
	if (PendingArmWeaponId == u16(-1))
		return;

	CLevel* level = GetLevel();
	CObject* obj = (IsActive() && level != nullptr) ? level->Objects.net_Find(PendingArmWeaponId) : nullptr;
	CGameObject* itemObj = obj != nullptr ? obj->cast_game_object() : nullptr;

	if (SessionActor != nullptr && itemObj != nullptr)
	{
		CInventoryItem* item = itemObj->cast_inventory_item();
		if (item != nullptr)
		{
			// The stock auto-pickup may have beaten this driver to the weapon
			// (Take asserts on a second Take): only take it when it is in no
			// inventory yet, then activate its slot either way.
			if (item->m_pInventory == nullptr)
				SessionActor->inventory().Take(itemObj, false, true);
			if (item->m_pInventory == &SessionActor->inventory())
			{
				SessionActor->inventory().Activate(item->BaseSlot(), true);
				Msg("* Level viewer: actor armed with '%s' (slot %u)", itemObj->cNameSect().c_str(), u32(item->BaseSlot()));
			}
			else
				Msg("! Level viewer: arming skipped - the weapon is not in the actor's inventory.");
		}
		PendingArmWeaponId = u16(-1);
		return;
	}

	if (++ArmWaitFrames > 600)
	{
		Msg("! Level viewer: timed out arming the actor (weapon id %u).", u32(PendingArmWeaponId));
		PendingArmWeaponId = u16(-1);
	}
}

void CViewerSession::RunSpawnTestNow()
{
	CLevel* level = GetLevel();
	if (!IsActive() || level == nullptr)
	{
		// Queue: runs as soon as a session becomes active.
		SpawnTestPending = true;
		Msg("* viewer_test_spawns: queued (runs when the session is active)");
		return;
	}
	SpawnTestPending = false;
	CLevelSlotGuard slotGuard(this, level, ViewerState.Primary);

	const u32 objectsBefore = level->Objects.o_count();
	const bool hasAiMap = ai().get_level_graph() != nullptr && ai().level_graph().has_nodes();
	Msg("* viewer_test_spawns: objects=%u, ai-map=%s", objectsBefore, hasAiMap ? "present" : "ABSENT (degradation case)");
	if (!ViewerSessionDetail::SupportsPhysicalSpawn(*level, SessionPhysicsWorld))
	{
		Msg("* viewer_test_spawns: collision/physics unavailable; item, monster and NPC spawns rejected safely -> PASS (degraded contract)");
		return;
	}

	ALife::_OBJECT_ID itemId = ALife::INVALID_OBJECT_ID;
	ALife::_OBJECT_ID monsterId = ALife::INVALID_OBJECT_ID;
	ALife::_OBJECT_ID npcId = ALife::INVALID_OBJECT_ID;
	SpawnSectionNow("wpn_ak74", &itemId);
	SpawnSectionNow("m_dog_e", &monsterId);
	SpawnSectionNow("stalker", &npcId);

	// Verify on a later frame: spawns are queued through the in-proc server.
	level->schedule_callback(level, 500,
		[level, objectsBefore, hasAiMap, itemId, monsterId, npcId]()
		{
			if (level == nullptr)
			{
				Msg("! viewer_test_spawns: level gone before verification");
				return;
			}
			const auto modelReady = [level](ALife::_OBJECT_ID id)
			{
				CObject* object = id != ALife::INVALID_OBJECT_ID ? level->Objects.net_Find(id) : nullptr;
				return object != nullptr && !object->getDestroy() && object->Ready() && object->Visual() != nullptr && (object->getVisible() || object->H_Parent() != nullptr);
			};
			const bool itemReady = modelReady(itemId);
			const bool monsterReady = modelReady(monsterId);
			const bool npcReady = modelReady(npcId);
			const u32 objectsAfter = level->Objects.o_count();
			const int spawned = int(objectsAfter) - int(objectsBefore);
			Msg("* viewer_test_spawns: objects after = %u (spawned %+d), ai-map was %s, models item/monster/NPC=%d/%d/%d -> %s",
				objectsAfter, spawned, hasAiMap ? "present" : "absent", itemReady, monsterReady, npcReady,
				spawned >= 3 && itemReady && monsterReady && npcReady ? "PASS" : "FAIL");
		});
}

void CViewerSession::ReleaseRuntimeTerrainScene()
{
	if (!SessionTerrainScene)
		return;
	SessionTerrainScene = false;
	if (SessionRender != nullptr)
		SessionRender->DestroyRuntimeTerrainScene();
}

void CViewerSession::ReleaseSessionRender()
{
	if (SessionRender == nullptr)
		return;
	IRender_interface* previous = Render;
	Render = SessionRender;
	ReleaseRuntimeTerrainScene();
	SessionRender->destroy_scene();
	Render = previous;
	SessionRender = nullptr;
}

void CViewerSession::UploadTerrainToGpuPool()
{
	ReleaseRuntimeTerrainScene();

	if (Preset.Terrain.Kind == Sandbox::ETerrainKind::PrebuiltLevel)
		return;

	Sandbox::STerrainMesh mesh = Sandbox::BuildTerrainMesh(Preset.Terrain);
	if (mesh.Verts.empty() || mesh.Tris.empty())
		return;

	// CDB::TRI carries a 4-byte material field after the 3 indices - pack plain
	// u32 indices for the GPU.
	xr_vector<u32> indices;
	indices.resize(mesh.Tris.size() * 3);
	for (size_t t = 0; t < mesh.Tris.size(); ++t)
	{
		indices[t * 3 + 0] = mesh.Tris[t].verts[0];
		indices[t * 3 + 1] = mesh.Tris[t].verts[1];
		indices[t * 3 + 2] = mesh.Tris[t].verts[2];
	}

	// Inject the same mesh into the stock deferred pipeline as level static
	// geometry: the renderer wraps it in one synthetic outdoor sector, so the
	// parallel pass draws the terrain lit and textured and dynamic objects get a
	// valid sector. Refused (nullptr) when a compiled level scene is loaded; the
	// isolated terrain renderer below remains the fallback. The UV scale keeps
	// the format's [-32, 32) texcoord range for any terrain size.
	if (!g_dedicated_server && SessionRender != nullptr)
	{
		const float uvScale = std::min(0.25f, 30.f / std::max(Preset.Terrain.Size * 0.5f, 1.f));
		SessionTerrainScene = SessionRender->CreateRuntimeTerrainScene(
			mesh.Verts.data(), u32(mesh.Verts.size()),
			indices.data(), u32(indices.size()),
			uvScale, ViewerSessionDetail::TerrainTextureName(), &GpuPool) != nullptr;
		if (SessionTerrainScene)
			Msg("* Level viewer: terrain injected into the deferred pipeline (%u verts, %u tris)", u32(mesh.Verts.size()), u32(mesh.Tris.size()));
	}

}

void CViewerSession::SetCameraFocus(const Fvector& focus, float distance, float pitch)
{
	ViewerState.FreeCamActive = true;
	ViewerState.CamYaw = 0.f;
	ViewerState.CamPitch = pitch;
	Fvector direction;
	direction.setHP(ViewerState.CamYaw, ViewerState.CamPitch);
	ViewerState.CamPos.mad(focus, direction, -distance);
	CameraInitialized = true;
}

void CViewerSession::UpdateFreeCamera()
{
	// Parallel sessions never write Device camera matrices (those belong to the
	// primary session / menu); the viewport projects through the session-local
	// full transform (ViewerCamFull) instead.
	if (ViewerState.Primary)
		return;

	// With a bound player actor and free-cam off, the viewport follows the actor's
	// first-person camera (the actor's camera manager holds it).
	if (SessionActor != nullptr && !ViewerState.FreeCamActive && GetLevel() != nullptr)
	{
		CCameraManager& cams = SessionActor->Cameras();
		Fmatrix view;
		view.build_camera_dir(cams.Position(), cams.Direction(), cams.Up());
		Fmatrix proj;
		proj.build_projection(deg2rad(cams.Fov()), ViewportAspect(), Device.fViewportNear, 1000.f);
		ViewerCamFull.mul(proj, view);
		return;
	}

	// Free camera. Movement input is applied only while the free cam is active and
	// the viewer captures input, but the session-local view matrix is rebuilt every
	// tick regardless so the viewport always renders from a valid transform.
	if (ViewerState.FreeCamActive && GImGUIManager->IsCapturingInputs())
	{
		const ImGuiIO& io = ImGui::GetIO();
		const float dt = Device.fTimeDelta;

		// Look: arrow keys always, mouse while RMB is held.
		constexpr float LookRate = 1.6f;    // rad/s for keys
		constexpr float MouseSens = 0.0035f;
		if (ImGui::IsKeyDown(ImGuiKey_LeftArrow))
			ViewerState.CamYaw += LookRate * dt;
		if (ImGui::IsKeyDown(ImGuiKey_RightArrow))
			ViewerState.CamYaw -= LookRate * dt;
		if (ImGui::IsKeyDown(ImGuiKey_UpArrow))
			ViewerState.CamPitch += LookRate * dt;
		if (ImGui::IsKeyDown(ImGuiKey_DownArrow))
			ViewerState.CamPitch -= LookRate * dt;
		if (ImGui::IsMouseDown(ImGuiMouseButton_Right))
		{
			ViewerState.CamYaw -= io.MouseDelta.x * MouseSens;
			ViewerState.CamPitch -= io.MouseDelta.y * MouseSens;
		}
		clamp(ViewerState.CamPitch, -(PI_DIV_2 - 0.01f), PI_DIV_2 - 0.01f);

		// Move: WASD + QE (down/up), Shift boosts. Skipped while a text field
		// owns the keyboard (e.g. the heightmap path input).
		if (!io.WantTextInput)
		{
			const float step = ViewerState.CamSpeed * dt * (io.KeyShift ? 4.f : 1.f);
			Fvector dir;
			dir.setHP(ViewerState.CamYaw, ViewerState.CamPitch);
			Fvector right;
			right.crossproduct(Fvector().set(0.f, 1.f, 0.f), dir).normalize();

			if (ImGui::IsKeyDown(ImGuiKey_W))
				ViewerState.CamPos.mad(dir, step);
			if (ImGui::IsKeyDown(ImGuiKey_S))
				ViewerState.CamPos.mad(dir, -step);
			if (ImGui::IsKeyDown(ImGuiKey_D))
				ViewerState.CamPos.mad(right, step);
			if (ImGui::IsKeyDown(ImGuiKey_A))
				ViewerState.CamPos.mad(right, -step);
			if (ImGui::IsKeyDown(ImGuiKey_E))
				ViewerState.CamPos.y += step;
			if (ImGui::IsKeyDown(ImGuiKey_Q))
				ViewerState.CamPos.y -= step;
		}
	}

	Fvector dir;
	dir.setHP(ViewerState.CamYaw, ViewerState.CamPitch);
	Fvector right, top;
	right.crossproduct(Fvector().set(0.f, 1.f, 0.f), dir).normalize();
	top.crossproduct(dir, right).normalize();

	Fmatrix view;
	view.build_camera_dir(ViewerState.CamPos, dir, top);
	Fmatrix proj;
	proj.build_projection(deg2rad(Device.fFOV), ViewportAspect(), Device.fViewportNear, 1000.f);
	ViewerCamFull.mul(proj, view);
}


}
