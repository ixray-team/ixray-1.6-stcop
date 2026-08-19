// LevelEditor.cpp : Определяет точку входа для приложения.
//
#include "stdafx.h"

#include "Engine/XrGameManager.h"
#include "Engine/XRayEditor.h"

#include "Editor/Utils/ContentView.h"
#include "Editor/Scene/LEPhysics.h"
#include "Editor/AssetImport/TLegacyLevelImporter.h"
#include "Editor/AssetImport/TLegacyObjectImporter.h"
#include "Editor/Entry/Glow/glow.h"
#include "Editor/Entry/Group/GroupObject.h"
#include "Editor/Entry/Light/ELight.h"
#include "Editor/Entry/Portal/portal.h"
#include "Editor/Entry/Puddles/puddle.h"
#include "Editor/Entry/Shape/EShape.h"
#include "Editor/Entry/Sound/ESound_Environment.h"
#include "Editor/Entry/Sound/ESound_Source.h"
#include "Editor/Entry/Spawn/SpawnPoint.h"
#include "Editor/Entry/StaticObject/SceneObject.h"
#include "Editor/Entry/Terrain/Terrain.h"
#include "Editor/Entry/WayPoint/WayPoint.h"
#include "Editor/Terrain/HeightmapUtils.h"
#include "Editor/Tools/AIMap/ESceneAIMapTools.h"
#include "Editor/Tools/Details/ESceneDOTools.h"
#include "Editor/Tools/FogVolume/ESceneFogVolumeTools.h"
#include "Editor/Tools/Light/ESceneLightTools.h"
#include "Editor/Tools/Wallmark/ESceneWallmarkTools.h"
#include "../../Include/xrRender/TiramisuEditorRendererFactory.h"
#include "Renderer/Tiramisu/TiramisuEditorLegacySceneBridge.h"
#include "Renderer/Tiramisu/TiramisuEditorNriStartup.h"
#include "../xrECore/Editor/EditorRenderBackend.h"
#include "../xrECore/Editor/MaterialPreviewRenderer.h"

#include "../../xrPlay/Splash.h"

#include "../../xrEngine/std_classes.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/XR_IOConsole.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/string_table.h"
#include "../../xrEngine/x_ray.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrEngine/FPSCounter.h"
#include "../../xrCore/RenderDebugPolicy.h"
#include "../../xrCore/RenderDocIntegration.h"

#include <SceneAsset.h>
#include <SceneConversionDump.h>

#include <chrono>
#include <filesystem>
#include <fstream>
#include <iterator>

ECORE_API extern bool bIsLevelEditor;
void DragDrop(const xr_string&, int);

namespace
{
xr_string ReadEditorVirtualFile(const char* Alias, const char* RelativePath)
{
	IReader* Reader = FS.r_open(Alias, RelativePath);
	if (!Reader)
	{
		return {};
	}
	xr_string Result(static_cast<const char*>(Reader->pointer()), static_cast<size_t>(Reader->length()));
	FS.r_close(Reader);
	return Result;
}

xr_string ReadEditorDiskFile(const std::filesystem::path& Path)
{
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
	{
		return {};
	}
	const std::string Text{std::istreambuf_iterator<char>(Input), std::istreambuf_iterator<char>()};
	return xr_string(Text);
}

void LogConversionDiagnostics(
	const xr_vector<Tiramisu::Scene::FSceneConversionDiagnostic>&
		Diagnostics
)
{
	for (const Tiramisu::Scene::FSceneConversionDiagnostic& Diagnostic :
		 Diagnostics)
	{
		const char* Prefix = Diagnostic.Severity == "error" ? "!" : Diagnostic.Severity == "warning" ? "~"
																									 : "*";
		Msg("%s Legacy conversion smoke [%s]: %s", Prefix, Diagnostic.Code.c_str(), Diagnostic.Message.c_str());
	}
}

[[nodiscard]] bool HasConversionDiagnostic(
	const Tiramisu::Scene::FSceneConversionDump& Dump,
	const xr_string_view Code
)
{
	return std::ranges::any_of(
		Dump.Diagnostics,
		[Code](const Tiramisu::Scene::FSceneConversionDiagnostic& Diagnostic)
		{
			return Diagnostic.Code == Code;
		}
	);
}

struct FLegacyWallmarkSmokeFixture
{
	ESceneWallmarkTool* Tool = nullptr;
	ESceneWallmarkTool::wm_slot* Slot = nullptr;
	ESceneWallmarkTool::wallmark* Wallmark = nullptr;

	~FLegacyWallmarkSmokeFixture()
	{
		if (Tool && Slot)
		{
			std::erase(Tool->marks, Slot);
			Slot->items.clear();
		}
		xr_delete(Wallmark);
		xr_delete(Slot);
	}

	[[nodiscard]] bool Attach(EScene& LegacyScene)
	{
		Tool = smart_cast<ESceneWallmarkTool*>(
			LegacyScene.GetTool(OBJCLASS_WM)
		);
		if (!Tool)
		{
			return false;
		}
		Slot = new ESceneWallmarkTool::wm_slot(
			"effects\\wallmarkblend",
			"textures/default/default_white"
		);
		Wallmark = new ESceneWallmarkTool::wallmark();
		Wallmark->parent = Slot;
		Wallmark->w = 2.0f;
		Wallmark->h = 2.0f;
		Wallmark->verts.resize(3);
		Wallmark->verts[0].p.set(-1.0f, -1.0f, 0.0f);
		Wallmark->verts[0].t.set(0.0f, 0.0f);
		Wallmark->verts[1].p.set(1.0f, -1.0f, 0.0f);
		Wallmark->verts[1].t.set(1.0f, 0.0f);
		Wallmark->verts[2].p.set(-1.0f, 1.0f, 0.0f);
		Wallmark->verts[2].t.set(0.0f, 1.0f);
		Slot->items.push_back(Wallmark);
		Tool->marks.push_back(Slot);
		return true;
	}
};

struct FLegacyConversionSmokeOptions
{
	std::filesystem::path LevelSource = "rawdata/levels/1.level";
	std::filesystem::path OutputRoot;
	bool KeepOutput = false;
};

bool RunLegacyConversionSmoke(
	const FLegacyConversionSmokeOptions& Options = {}
)
{
	using namespace Tiramisu::Scene;
	const std::filesystem::path TemporaryRoot =
		Options.OutputRoot.empty()
			? std::filesystem::temp_directory_path() /
				  ("ixray-legacy-conversion-smoke-" + std::to_string(
														  std::chrono::steady_clock::now()
															  .time_since_epoch()
															  .count()
													  ))
			: Options.OutputRoot;
	struct FCleanup
	{
		std::filesystem::path Path;
		bool Enabled = true;
		~FCleanup()
		{
			if (!Enabled)
			{
				return;
			}
			std::error_code Error;
			std::filesystem::remove_all(Path, Error);
		}
	} Cleanup{TemporaryRoot, !Options.KeepOutput};

	const std::filesystem::path MaterialRoot =
		TemporaryRoot / "render_materials";
	const std::filesystem::path StaticMeshRoot =
		TemporaryRoot / "render_static_meshes";
	const std::filesystem::path RenderSceneRoot =
		TemporaryRoot / "render_scenes";
	std::error_code Error;
	std::filesystem::create_directories(TemporaryRoot, Error);
	if (!Error)
	{
		std::filesystem::copy("gamedata/render_materials", MaterialRoot, std::filesystem::copy_options::recursive, Error);
	}
	if (Error)
	{
		Msg("! Legacy conversion smoke could not copy the material root: %s",
			Error.message().c_str());
		return false;
	}

	const std::filesystem::path MissingLevelSource =
		TemporaryRoot / "missing.level";
	const FLegacyLevelImportResult FailedLevel =
		WriteLegacyLevelLoadFailureDump(
			MissingLevelSource, RenderSceneRoot, "level_import.source_open_failed", "Smoke-test source cannot be opened."
		);
	const FSceneConversionDumpParseResult FailedLevelDump =
		ParseSceneConversionDumpJson(
			ReadEditorDiskFile(FailedLevel.DumpPath)
		);
	if (FailedLevel.Succeeded || !FailedLevelDump.Succeeded() ||
		FailedLevelDump.Value.Status != ESceneConversionStatus::Failed ||
		FailedLevelDump.Value.Diagnostics.empty())
	{
		Msg("! Legacy conversion smoke did not publish a valid failure dump");
		return false;
	}

	const std::filesystem::path ObjectSource =
		"rawdata/objects/detail/det_hvosh.object";
	const FLegacyObjectImportResult FirstObject =
		ImportLegacyObjectAsset(
			ObjectSource, MaterialRoot, StaticMeshRoot
		);
	LogConversionDiagnostics(FirstObject.Diagnostics);
	if (!FirstObject.Succeeded)
	{
		Msg("! Legacy object conversion smoke failed. Dump: %s",
			FirstObject.DumpPath.generic_string().c_str());
		return false;
	}
	const FStaticMeshAssetParseResult Mesh =
		LoadStaticMeshAsset(FirstObject.TargetPath);
	const FSceneConversionDumpParseResult FirstObjectDump =
		ParseSceneConversionDumpJson(ReadEditorDiskFile(
			FirstObject.DumpPath
		));
	if (!Mesh.Succeeded() || !FirstObjectDump.Succeeded() ||
		FirstObjectDump.Value.Status !=
			ESceneConversionStatus::Succeeded ||
		FirstObjectDump.Value.TargetPayloadPath !=
			xr_string(FirstObject.TargetPayloadPath.generic_string()) ||
		!std::filesystem::is_regular_file(
			FirstObject.TargetPayloadPath
		))
	{
		Msg("! Legacy object conversion smoke produced an invalid asset or dump");
		return false;
	}

	const FLegacyObjectImportResult SecondObject =
		ImportLegacyObjectAsset(
			ObjectSource, MaterialRoot, StaticMeshRoot
		);
	const FSceneConversionDumpParseResult SecondObjectDump =
		ParseSceneConversionDumpJson(ReadEditorDiskFile(
			SecondObject.DumpPath
		));
	if (!SecondObject.Succeeded || !SecondObjectDump.Succeeded() ||
		SecondObject.TargetAssetId != FirstObject.TargetAssetId ||
		SecondObjectDump.Value.CreatedMaterialInstances != 0 ||
		SecondObjectDump.Value.ReusedMaterialInstances == 0)
	{
		Msg("! Legacy object conversion smoke is not deterministic or did not "
			"reuse MaterialInstance assets");
		return false;
	}

	Scene->setSkipCantFindDialog(true);
	const bool Loaded = Scene->LoadLTX(
		Options.LevelSource.string().c_str(), false
	);
	Scene->setSkipCantFindDialog(false);
	if (!Loaded)
	{
		Msg("! Legacy conversion smoke could not load '%s'",
			Options.LevelSource.generic_string().c_str());
		return false;
	}
	FLegacyWallmarkSmokeFixture WallmarkFixture;
	if (!WallmarkFixture.Attach(*Scene))
	{
		Msg("! Legacy conversion smoke could not attach a Wallmark fixture");
		return false;
	}
	const FLegacyLevelImportResult Level =
		ImportLoadedLegacyLevelAsset(Options.LevelSource, *Scene, MaterialRoot, StaticMeshRoot, RenderSceneRoot);
	LogConversionDiagnostics(Level.Diagnostics);
	if (!Level.Succeeded)
	{
		Msg("! Legacy level conversion smoke failed. Dump: %s",
			Level.DumpPath.generic_string().c_str());
		return false;
	}
	const FSceneConversionDumpParseResult LevelDump =
		ParseSceneConversionDumpJson(ReadEditorDiskFile(Level.DumpPath));
	const FResolvedRenderSceneResult NativeScene =
		LoadRenderSceneAsset(Level.TargetPath);
	if (!LevelDump.Succeeded() ||
		LevelDump.Value.Status != ESceneConversionStatus::Succeeded ||
		LevelDump.Value.AssetMappings.empty() ||
		LevelDump.Value.ComponentCount == 0 ||
		!NativeScene.Succeeded() ||
		NativeScene.Value.Scene.StaticMeshComponents.empty() ||
		NativeScene.Value.Scene.LightComponents.empty() ||
		NativeScene.Value.Scene.DecalComponents.empty() ||
		!HasConversionDiagnostic(
			LevelDump.Value,
			"level_import.wallmarks_migrated"
		) ||
		NativeScene.Value.Scene.LightComponents.front().Type !=
			ELightType::Directional ||
		!NativeScene.Value.Scene.LightComponents.front().CastShadows)
	{
		Msg("! Legacy level conversion smoke produced an invalid native scene "
			"with materials, geometry, lights, or audit dump");
		return false;
	}
	const FLegacyLevelImportResult SecondLevel =
		ImportLoadedLegacyLevelAsset(
			Options.LevelSource,
			*Scene,
			MaterialRoot,
			StaticMeshRoot,
			RenderSceneRoot
		);
	const FSceneConversionDumpParseResult SecondLevelDump =
		ParseSceneConversionDumpJson(
			ReadEditorDiskFile(SecondLevel.DumpPath)
		);
	const FResolvedRenderSceneResult SecondNativeScene =
		LoadRenderSceneAsset(SecondLevel.TargetPath);
	if (!SecondLevel.Succeeded || !SecondLevelDump.Succeeded() ||
		!SecondNativeScene.Succeeded() ||
		SecondLevel.TargetAssetId != Level.TargetAssetId ||
		SecondLevelDump.Value.CreatedMaterialInstances != 0 ||
		SecondLevelDump.Value.ReusedMaterialInstances == 0 ||
		SecondNativeScene.Value.Scene.DecalComponents.size() !=
			NativeScene.Value.Scene.DecalComponents.size() ||
		SecondNativeScene.Value.Scene.DecalComponents.front().Id !=
			NativeScene.Value.Scene.DecalComponents.front().Id ||
		SecondNativeScene.Value.Scene.DecalComponents.front().Material !=
			NativeScene.Value.Scene.DecalComponents.front().Material)
	{
		Msg("! Legacy level decal conversion is not deterministic or did not "
			"reuse MaterialInstance assets");
		return false;
	}
	Msg("* Legacy conversion smoke: success (meshes=%u, components=%u, "
		"lights=%zu, decals=%zu, materials created=%u, reused=%u)",
		LevelDump.Value.MeshCount,
		LevelDump.Value.ComponentCount,
		NativeScene.Value.Scene.LightComponents.size(),
		NativeScene.Value.Scene.DecalComponents.size(),
		LevelDump.Value.CreatedMaterialInstances,
		LevelDump.Value.ReusedMaterialInstances);
	if (Options.KeepOutput)
	{
		Msg("* Legacy conversion smoke output: %s",
			TemporaryRoot.generic_string().c_str());
		Msg("* Legacy conversion smoke dump: %s",
			Level.DumpPath.generic_string().c_str());
	}
	return true;
}
} // namespace

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, char* pCmdLine, int nCmdShow)
{
	bIsLevelEditor = true;
	const bool HiddenTestWindow = HasEditorCommandLineFlag(
		pCmdLine ? xr_string_view(pCmdLine) : xr_string_view(),
		"-editor-test-hidden"
	);

	if (!SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_EVENTS))
	{
		Msg("! SDL_Init Error: %s", SDL_GetError());
		return 0;
	}

	std::jthread SplashThread;
	if (!HiddenTestWindow)
	{
		splash::SetBackground(IDB_LE);
		SplashThread = std::jthread(splash::Show);
	}

	splash::SetProgressStatus(5, "Initializing Debugger");

	Debug._initialize(false);

	splash::SetProgressStatus(10, "Initializing Core System");

	const char* FSName = "fs.ltx";
	const char* fsgame_ltx_name = "-fsltx ";
	string_path fsgame = "";

	if (strstr(pCmdLine, fsgame_ltx_name))
	{
		int sz = xr_strlen(fsgame_ltx_name);
		sscanf(strstr(pCmdLine, fsgame_ltx_name) + sz, "%[^ ] ", fsgame);
	}

	CFilewatcher::instance().SetFilewatcherActive(true);
	Core._initialize("LevelEditor", ELogCallback, 1, fsgame[0] ? fsgame : FSName);
	const FEditorNriStartupConfig EditorNriConfig =
		ParseEditorNriStartupConfig(Core.Params ? Core.Params : "");
	if (!EditorNriConfig.IsValid())
	{
		Msg("! LevelEditor: -render-deterministic requires the exact "
			"-rdbg flag and the Tiramisu renderer");
		Core._destroy();
		return 11;
	}
	if (EditorNriConfig.DeterministicTest.Enabled)
	{
		Random.seed(static_cast<s32>(
			EditorNriConfig.DeterministicTest.RandomSeed
		));
		Msg("* LevelEditor: deterministic GPU test mode enabled "
			"(seed=%u, delta=%.6f, shader-time=%.3f)",
			EditorNriConfig.DeterministicTest.RandomSeed,
			EditorNriConfig.DeterministicTest.FixedDeltaSeconds,
			EditorNriConfig.DeterministicTest.FixedShaderTimeSeconds);
	}

	splash::SetProgressStatus(20, "Initializing Level Tools");

	Tools = new CLevelTool();
	LTools = static_cast<CLevelTool*>(Tools);

	splash::SetProgressStatus(25, "Registering UI Commands");

	UI = new CLevelMain();
	UI->RegisterCommands();
	UI->GeneralTabs.push_back({"Scene View", nullptr});

	LUI = static_cast<CLevelMain*>(UI);
	FTiramisuEditorRendererInstance EditorRendererInstance;
	IEditorRenderBackend* EditorNriBackend = nullptr;
	IEditorRenderBackend* PreviousEditorRenderBackend = nullptr;
	IMaterialPreviewRenderer* PreviousMaterialPreviewRenderer = nullptr;
	if (EditorNriConfig.Enabled)
	{
		R_ASSERT2(CreateTiramisuEditorRenderer(nullptr, EditorNriConfig.Api, EditorNriConfig.DeterministicTest, EditorRendererInstance), "xrRenderTiramisu failed to create the editor renderer");
		R_ASSERT2(EditorRendererInstance.IsValid(), "xrRenderTiramisu returned incomplete editor renderer interfaces");
		EditorNriBackend = EditorRendererInstance.EditorBackend;
		R_ASSERT2(UI->InstallRenderBackend(EditorRendererInstance.UiBackend), "The Tiramisu editor renderer must be installed before device initialization");
		PreviousEditorRenderBackend = InstallEditorRenderBackend(
			EditorRendererInstance.EditorBackend
		);
		PreviousMaterialPreviewRenderer = InstallMaterialPreviewRenderer(
			EditorRendererInstance.MaterialPreviewRenderer
		);
		Msg("* LevelEditor: xrRenderTiramisu editor presenter selected (%s)",
			EditorNriConfig.Api == ETiramisuEditorGraphicsApi::D3D12
				? "D3D12"
				: "Vulkan");
	}

	splash::SetProgressStatus(30, "Creating Editor Scene");

	Scene = new EScene();
	EditorScene = Scene;

	splash::SetProgressStatus(25, "Initializing Content View");

	GContentView = new CContentView;

	splash::SetProgressStatus(30, "Creating Main UI Form");

	UIMainForm* MainForm = new UIMainForm();

	pApp = new XRayEditor();
	g_XrGameManager = new XrGameManager();
	g_SEFactoryManager = new XrSEFactoryManager();

	splash::SetProgressStatus(40, "Loading Game Materials");

	// Initialize APP
	GameMaterialLibraryEditors->Load();

	splash::SetProgressStatus(55, "Initializing Game Persistent Objects");

	g_pGamePersistent = static_cast<IGame_Persistent*>(g_XrGameManager->Create(CLSID_GAME_PERSISTANT));
	if (EditorNriConfig.DeterministicTest.Enabled)
	{
		g_pGamePersistent->Environment().SetGameTime(
			EditorNriConfig.DeterministicTest.FixedWeatherTimeSeconds, 0.0f
		);
	}
	EDevice->seqAppStart.Process<&pureAppStart::OnAppStart>();

	splash::SetProgressStatus(65, "Setting Up Console");

	Console->Execute("default_controls");

	xr_strcpy(Console->ConfigFile, "user_editor.ltx");

	if (strstr(Core.Params, "-ltx "))
	{
		string64 c_name;
		sscanf(strstr(Core.Params, "-ltx ") + 5, "%[^ ] ", c_name);
		xr_strcpy(Console->ConfigFile, c_name);
	}

	Console->ExecuteScript(Console->ConfigFile);

	Console->Hide();

	splash::SetProgressStatus(75, "Performing Final UI Setup");

	::MainForm = MainForm;
	UI->Push(MainForm, false);

	pFPSCounter = new XRay::Hardware::FPSCounter();

	bool NeedExit = false;
	splash::SetProgressStatus(85, "Performing Final Checks");
	MainForm->GetRenderForm()->DragFunctor = DragDrop;

	splash::SetProgressStatus(90, "Finalizing UI Setup");
	GContentView->Init();
	UI->PushBegin(GContentView);
	splash::SetProgressStatus(100, "Finalizing");
	if (!HiddenTestWindow)
	{
		splash::Close();
	}

	const bool MaterialPreviewSmokeRequested =
		strstr(Core.Params, "-material-preview-smoke") != nullptr;
	const bool ViewportMaterialReloadSmokeRequested =
		strstr(Core.Params, "-viewport-material-reload-smoke") != nullptr;
	const bool EditorResizeSmokeRequested =
		strstr(Core.Params, "-editor-resize-smoke") != nullptr;
	const bool ViewportMaterialSmokeRequested =
		strstr(Core.Params, "-viewport-material-smoke") != nullptr ||
		ViewportMaterialReloadSmokeRequested || EditorResizeSmokeRequested;
	const bool LegacyConversionSmokeRequested =
		strstr(Core.Params, "-legacy-conversion-smoke") != nullptr;
	const bool ZatonConversionSmokeRequested =
		strstr(Core.Params, "-legacy-zaton-conversion-smoke") != nullptr;
	const bool ZatonRuntimeProfileRequested =
		strstr(Core.Params, "-legacy-zaton-runtime-profile") != nullptr;
	bool MaterialPreviewSmokeComplete = !MaterialPreviewSmokeRequested;
	bool ViewportMaterialSmokeComplete = !ViewportMaterialSmokeRequested;
	bool EditorResizeSmokeTriggered = false;
	u64 ResizeSmokeSurfaceRevision = 0;
	u64 ResizeSmokeSwapchainRevision = 0;
	u64 ResizeSmokePresentedFrameCount = 0;
	u64 ResizeSmokeViewportResourceRevision = 0;
	u64 ResizeSmokeTextureRedirectCount = 0;
	u64 ResizeSmokeStatisticsRevision = 0;
	u64 ResizeSmokeMaterialRevision = 0;
	u64 ResizeSmokePipelineKey = 0;
	int ProcessExitCode = 0;
	IMaterialPreviewRenderer* SmokePreviewRenderer = nullptr;
	FMaterialPreviewHandle SmokePreviewHandle;
	bool ZatonRuntimeProfileActive = false;
	size_t ZatonRuntimeWarmupFrames = 0;
	bool ZatonRuntimeCaptureStarted = false;
	xr_vector<double> ZatonRuntimeFrameMilliseconds;
	xr_string ZatonRuntimeProfilePath;
	// RenderDoc и AddressSanitizer заметно замедляют компиляцию shader permutations,
	// загрузку OGF и сериализацию capture. Обычный deterministic smoke сохраняет
	// прежний строгий дедлайн.
#if defined(IXRAY_ASAN_BUILD)
	constexpr bool IsAddressSanitizerBuild = true;
#else
	constexpr bool IsAddressSanitizerBuild = false;
#endif
	const auto SmokeTimeout = xrRenderDoc::IsLoaded()
		? std::chrono::seconds(240)
		: IsAddressSanitizerBuild
		? std::chrono::seconds(120)
		: std::chrono::seconds(60);
	const auto SmokeDeadline =
		std::chrono::steady_clock::now() + SmokeTimeout;
	if (MaterialPreviewSmokeRequested)
	{
		SmokePreviewRenderer = &GetMaterialPreviewRenderer();
		if (!EditorNriBackend || !SmokePreviewRenderer->IsAvailable())
		{
			Msg("! Material preview smoke requires -tiramisu-editor");
			ProcessExitCode = 2;
			GContentView->Destroy();
			NeedExit = true;
		}
		else
		{
			const xr_string MaterialJson = ReadEditorVirtualFile("$game_render_materials$", "standard_surface.material.json");
			const xr_string InstanceJson = ReadEditorVirtualFile("$game_render_materials$", "example_red.material-instance.json");
			const xr_string MaterialHlsl = ReadEditorVirtualFile("$game_shaders$", "r5\\materials\\StandardSurface.hlsl");
			SmokePreviewHandle = SmokePreviewRenderer->CreatePreview();
			if (MaterialJson.empty() || InstanceJson.empty() || MaterialHlsl.empty() ||
				!SmokePreviewHandle.IsValid())
			{
				Msg("! Material preview smoke setup failed: material=%zu, instance=%zu, hlsl=%zu, handle=%s",
					MaterialJson.size(),
					InstanceJson.size(),
					MaterialHlsl.size(),
					SmokePreviewHandle.IsValid() ? "valid" : "invalid");
				ProcessExitCode = 3;
				GContentView->Destroy();
				NeedExit = true;
			}
			else
			{
				FMaterialPreviewSource Source;
				Source.MaterialAssetId = "67e3bc21-9df5-4fc2-ab60-1ad7d02ad6e3";
				Source.MaterialJson = MaterialJson;
				Source.MaterialInstanceJson = InstanceJson;
				Source.GeneratedHlsl = MaterialHlsl;
				Source.Environment = "Studio";
				Source.Primitive = EMaterialPreviewPrimitive::Sphere;
				Source.Revision = 1;
				SmokePreviewRenderer->UpdatePreview(SmokePreviewHandle, Source);
				SmokePreviewRenderer->ResizePreview(SmokePreviewHandle, 512, 512);
				Msg("* Material preview smoke: compilation requested");
			}
		}
	}

	constexpr u32 SmokeSceneViewportId = 0x7ffffff0u;
	constexpr u32 SmokeParticlePreviewViewportId = 0x7fffffefu;
	constexpr u32 SmokeLegacyLightViewportId = 0x7fffffeeu;
	constexpr u32 ResizeSmokeViewportWidth = 704;
	constexpr u32 ResizeSmokeViewportHeight = 416;
	constexpr FEditorMaterialSlotId SmokeSceneMaterialSlot = {
		0x736d6f6b656d6174ull
	};
	constexpr FEditorMaterialSlotId SmokeSceneSpriteMaterialSlot = {
		0x7370726974656d61ull
	};
	constexpr FEditorMaterialSlotId SmokeSceneParticleMaterialSlot = {
		0x7061727469636c65ull
	};
	constexpr FEditorMaterialSlotId SmokeSceneDecalMaterialSlot = {
		0x646563616c6d6174ull
	};
	constexpr u64 SmokeSceneCloneMaterialSlotBase =
		0x636c6f6e65000100ull;
	constexpr u32 SmokeSceneCloneMaterialCount = 16;
	constexpr FEditorMaterialSlotId SmokeSceneLastCloneMaterialSlot = {
		SmokeSceneCloneMaterialSlotBase +
		SmokeSceneCloneMaterialCount - 1
	};
	if (ViewportMaterialSmokeRequested && !NeedExit)
	{
		if (!EditorNriBackend || !EditorNriBackend->IsAvailable())
		{
			Msg("! Viewport material smoke requires -tiramisu-editor");
			ProcessExitCode = 5;
			GContentView->Destroy();
			NeedExit = true;
		}
		else
		{
			FEditorParticleLibrarySnapshot ParticleLibrary;
			EditorNriBackend->CopyParticleLibrary(ParticleLibrary);
			const auto ParticleAsset = std::ranges::find_if(
				ParticleLibrary.Assets,
				[](const FEditorParticleAssetInfo& Asset)
				{
					return Asset.Type ==
						   EEditorParticleAssetType::Effect &&
						   Asset.HasCompiledActions;
				}
			);
			const auto ParticleGroupAsset = std::ranges::find_if(
				ParticleLibrary.Assets,
				[](const FEditorParticleAssetInfo& Asset)
				{
					return Asset.Type ==
						   EEditorParticleAssetType::Group &&
						   Asset.EnabledGroupEntryCount != 0 &&
						   Asset.GroupChildCallbackCount != 0;
				}
			);
			if (ParticleAsset == ParticleLibrary.Assets.end() ||
				ParticleGroupAsset == ParticleLibrary.Assets.end())
			{
				Msg("! Viewport material smoke requires particle effect and group assets");
				ProcessExitCode = 6;
				GContentView->Destroy();
				NeedExit = true;
			}

			xr_array<FEditorStaticMeshVertex, 9> Vertices;
			Vertices[0].Position = {-0.8f, -0.8f, 0.0f};
			Vertices[1].Position = {0.0f, 0.8f, 0.0f};
			Vertices[2].Position = {0.8f, -0.8f, 0.0f};
			Vertices[3].Position = {0.55f, -0.65f, -0.05f};
			Vertices[4].Position = {0.70f, -0.25f, -0.05f};
			Vertices[5].Position = {0.85f, -0.65f, -0.05f};
			Vertices[6].Position = {-0.85f, -0.65f, -0.04f};
			Vertices[7].Position = {-0.70f, -0.25f, -0.04f};
			Vertices[8].Position = {-0.55f, -0.65f, -0.04f};
			for (FEditorStaticMeshVertex& Vertex : Vertices)
			{
				Vertex.Normal = {0.0f, 0.0f, -1.0f};
				Vertex.Tangent = {1.0f, 0.0f, 0.0f, 1.0f};
			}
			Vertices[0].TexCoord = {0.0f, 1.0f};
			Vertices[1].TexCoord = {0.5f, 0.0f};
			Vertices[2].TexCoord = {1.0f, 1.0f};
			Vertices[3].TexCoord = {0.0f, 1.0f};
			Vertices[4].TexCoord = {0.5f, 0.0f};
			Vertices[5].TexCoord = {1.0f, 1.0f};
			Vertices[6].TexCoord = {0.0f, 1.0f};
			Vertices[7].TexCoord = {0.5f, 0.0f};
			Vertices[8].TexCoord = {1.0f, 1.0f};
			const xr_array<u32, 9> Indices = {
				0, 1, 2, 3, 4, 5, 6, 7, 8
			};
			const xr_array<FEditorStaticMeshSection, 3> Sections = {{{0, 3, SmokeSceneMaterialSlot}, {3, 3, SmokeSceneSpriteMaterialSlot}, {6, 3, SmokeSceneParticleMaterialSlot}}};
			const FEditorStaticMeshId MeshId = {0x736d6f6b656d6573ull};
			const xr_array<FEditorStaticMeshUpload, 1> Meshes = {{MeshId, 1, Vertices, Indices, Sections}};
			xr_array<FEditorStaticMeshInstance, 1> Instances;
			Instances[0].ObjectId = {0x736d6f6b656f626aull};
			Instances[0].MeshId = MeshId;
			// Exercise the X-Ray row-vector -> material draw-buffer matrix ABI.
			// An identity transform would not catch translation leaking into
			// clip-space W in the vertex shader.
			Instances[0].LocalToWorld[12] = 0.6f;
			Instances[0].Flags = EEditorSceneInstanceFlags::Selected;
				xr_vector<FEditorMaterialSlotSource> Materials = {
				{SmokeSceneMaterialSlot, "default", "textures/kung", "Viewport material smoke", EEditorMaterialSlotFlags::None},
				{SmokeSceneSpriteMaterialSlot, "editor\\spawn_icon", "textures/default/default_white", "Editor sprite smoke", EEditorMaterialSlotFlags::TwoSided},
				{SmokeSceneParticleMaterialSlot, "editor\\particle_additive", "textures/default/default_white", "Editor particle smoke", EEditorMaterialSlotFlags::TwoSided},
				{SmokeSceneDecalMaterialSlot, {}, "textures/default/default_white", "Projective decal smoke", EEditorMaterialSlotFlags::None, "c4c45d66-6cc9-4e62-89a8-f86eaf0b8014"}
			};
			Materials.reserve(4 + SmokeSceneCloneMaterialCount);
			for (u32 Index = 0;
				 Index < SmokeSceneCloneMaterialCount;
				 ++Index)
			{
				Materials.push_back({{SmokeSceneCloneMaterialSlotBase + Index}, "default", (Index & 1u) != 0 ? "briks\\briks_br2" : "textures\\kung", "Shared standard permutation smoke", EEditorMaterialSlotFlags::None});
			}
			xr_array<FEditorDebugLine, 1> DebugLines;
			DebugLines[0].Vertices[0] = {
				{-0.9f, 0.0f, -0.01f}, {1.0f, 0.0f, 0.0f, 1.0f}
			};
			DebugLines[0].Vertices[1] = {
				{0.9f, 0.0f, -0.01f}, {0.0f, 1.0f, 0.0f, 1.0f}
			};
			xr_array<FEditorDebugTriangle, 1> DebugTriangles;
			DebugTriangles[0].Vertices[0] = {
				{-0.2f, -0.2f, -0.02f}, {0.0f, 0.0f, 1.0f, 0.5f}
			};
			DebugTriangles[0].Vertices[1] = {
				{0.0f, 0.2f, -0.02f}, {0.0f, 0.0f, 1.0f, 0.5f}
			};
			DebugTriangles[0].Vertices[2] = {
				{0.2f, -0.2f, -0.02f}, {0.0f, 0.0f, 1.0f, 0.5f}
			};
			xr_array<FEditorOverlayLine, 1> OverlayLines;
			OverlayLines[0].Vertices[0] = {
				{-0.75f, 0.75f, 0.0f}, {1.0f, 0.0f, 0.0f, 1.0f}
			};
			OverlayLines[0].Vertices[1] = {
				{-0.25f, 0.75f, 0.0f}, {0.0f, 1.0f, 0.0f, 1.0f}
			};
			xr_array<FEditorOverlayTriangle, 1> OverlayTriangles;
			OverlayTriangles[0].Vertices[0] = {
				{0.25f, 0.75f, 0.0f}, {1.0f, 1.0f, 0.0f, 0.4f}
			};
			OverlayTriangles[0].Vertices[1] = {
				{0.75f, 0.75f, 0.0f}, {1.0f, 1.0f, 0.0f, 0.4f}
			};
			OverlayTriangles[0].Vertices[2] = {
				{0.5f, 0.25f, 0.0f}, {1.0f, 1.0f, 0.0f, 0.4f}
			};
			xr_array<FEditorOverlayText, 1> OverlayText;
			OverlayText[0].Position = {-0.75f, -0.75f};
			OverlayText[0].Color = {1.0f, 1.0f, 1.0f, 1.0f};
			OverlayText[0].ShadowColor = {0.0f, 0.0f, 0.0f, 1.0f};
			OverlayText[0].Text = "Tiramisu overlay";
			xr_array<FEditorSceneLight, 2> Lights;
			Lights[0].ObjectId = {0x736d6f6b656c6974ull};
			Lights[0].Type = EEditorSceneLightType::Directional;
			Lights[0].Color = {1.0f, 0.95f, 0.8f};
			Lights[0].Intensity = 2.0f;
			Lights[0].Flags = EEditorSceneLightFlags::CastShadows;
			Lights[1].ObjectId = {0x736d6f6b656c6932ull};
			Lights[1].Type = EEditorSceneLightType::Point;
			Lights[1].LocalToWorld[14] = -1.0f;
			Lights[1].Color = {0.2f, 0.45f, 1.0f};
			Lights[1].Intensity = 4.0f;
			Lights[1].Range = 4.0f;
			xr_array<FEditorParticleInstance, 2> ParticleInstances;
			ParticleInstances[0].ObjectId = {
				0x736d6f6b65706172ull
			};
			if (ParticleAsset != ParticleLibrary.Assets.end())
			{
				ParticleInstances[0].AssetName = ParticleAsset->Name;
			}
			ParticleInstances[0].LocalToWorld[12] = -0.4f;
			ParticleInstances[0].LocalToWorld[13] = 0.35f;
			ParticleInstances[0].Flags =
				EEditorParticleInstanceFlags::Playing;
			ParticleInstances[1].ObjectId = {
				0x736d6f6b65706772ull
			};
			if (ParticleGroupAsset != ParticleLibrary.Assets.end())
			{
				ParticleInstances[1].AssetName = ParticleGroupAsset->Name;
				ParticleInstances[1].AssetType =
					EEditorParticleAssetType::Group;
			}
			ParticleInstances[1].LocalToWorld[12] = 0.4f;
			ParticleInstances[1].LocalToWorld[13] = 0.35f;
			ParticleInstances[1].Flags =
				EEditorParticleInstanceFlags::Playing;
			xr_array<FEditorModelInstance, 1> ModelInstances;
			ModelInstances[0].ObjectId = {
				0x736d6f6b656f6766ull
			};
			ModelInstances[0].AssetName =
				"actors\\stalker_bandit\\stalker_bandit_1";
			ModelInstances[0].AnimationName = "norm_walk_fwd_0";
			ModelInstances[0].LocalToWorld[12] = -0.5f;
			xr_array<FEditorDecalInstance, 2> DecalInstances;
			DecalInstances[0].ObjectId = {0x736d6f6b65646563ull};
			DecalInstances[0].MaterialSlot = SmokeSceneDecalMaterialSlot;
			DecalInstances[0].LocalToWorld[0] = 1.2f;
			DecalInstances[0].LocalToWorld[5] = 1.2f;
			DecalInstances[0].LocalToWorld[10] = 0.2f;
			DecalInstances[0].LocalToWorld[12] = 0.6f;
			DecalInstances[1] = DecalInstances[0];
			DecalInstances[1].ObjectId = {0x736d6f6b656f6666ull};
			DecalInstances[1].LocalToWorld[12] = 100.0f;
			FEditorViewportSceneSnapshot Snapshot;
			Snapshot.Camera.View = {
				1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f
			};
			Snapshot.Camera.Projection = Snapshot.Camera.View;
			Snapshot.Camera.ViewProjection = Snapshot.Camera.View;
			Snapshot.Camera.WorldPosition = {0.0f, 0.0f, -3.0f};
			Snapshot.Camera.NearPlane = 0.05f;
			Snapshot.Camera.FarPlane = 100.0f;
			Snapshot.MaterialSlots = Materials;
			Snapshot.StaticMeshes = Meshes;
			Snapshot.Instances = Instances;
			Snapshot.DecalInstances = DecalInstances;
			Snapshot.ModelInstances = ModelInstances;
			Snapshot.Lights = Lights;
			Snapshot.ParticleInstances = ParticleInstances;
			Snapshot.DebugLines = DebugLines;
			Snapshot.DebugTriangles = DebugTriangles;
			Snapshot.OverlayLines = OverlayLines;
			Snapshot.OverlayTriangles = OverlayTriangles;
			Snapshot.OverlayText = OverlayText;
			Snapshot.DebugDrawRevision = 1;
			Snapshot.Revision = 1;
			EditorNriBackend->ResizeViewport(SmokeSceneViewportId, 512, 512);
			if (!EditorNriBackend->SubmitViewportScene(
					SmokeSceneViewportId, Snapshot
				))
			{
				Msg("! Viewport material smoke setup failed: %.*s",
					static_cast<int>(EditorNriBackend->GetLastDiagnostic().size()),
					EditorNriBackend->GetLastDiagnostic().data());
				ProcessExitCode = 6;
				GContentView->Destroy();
				NeedExit = true;
			}
			auto* LegacyLightTools = smart_cast<ESceneLightTool*>(
				Scene->GetOTool(OBJCLASS_LIGHT)
			);
			if (!LegacyLightTools)
			{
				Msg("! Legacy light bridge smoke has no light tools");
				ProcessExitCode = 6;
				GContentView->Destroy();
				NeedExit = true;
			}
			else
			{
				xr_vector<xr_pair<ESceneToolBase*, bool>>
					LegacyToolVisibility;
				auto MakeLegacyToolVisible =
					[&LegacyToolVisibility](ESceneToolBase* Tool)
				{
					if (!Tool)
					{
						return;
					}
					LegacyToolVisibility.emplace_back(
						Tool,
						Tool->IsVisible()
					);
					Tool->m_EditFlags.set(
						ESceneToolBase::flVisible,
						true
					);
				};
				MakeLegacyToolVisible(LegacyLightTools);
				auto* LegacyLight = static_cast<CLight*>(
					LegacyLightTools->CreateObject(
						nullptr,
						"tiramisu_legacy_light_smoke"
					)
				);
				LegacyLight->m_Type = ELight::ltSpot;
				LegacyLight->m_Color.set(0.25f, 0.5f, 1.0f, 1.0f);
				LegacyLight->m_Brightness = 3.0f;
				LegacyLight->m_Range = 8.0f;
				LegacyLight->m_Cone = deg2rad(50.0f);
				LegacyLight->m_Flags.set(ELight::flCastShadow, true);
				Fvector LightPosition = {0.0f, 1.0f, -1.0f};
				LegacyLight->SetPosition(LightPosition);
				LegacyLight->Select(true);
				LegacyLightTools->_AppendObject(LegacyLight);
				ESceneCustomOTool* LegacyShapeTools =
					Scene->GetOTool(OBJCLASS_SHAPE);
				MakeLegacyToolVisible(LegacyShapeTools);
				CEditShape* LegacyShape = nullptr;
				if (LegacyShapeTools)
				{
					LegacyShape = static_cast<CEditShape*>(
						LegacyShapeTools->CreateObject(
							nullptr,
							"tiramisu_legacy_shape_smoke"
						)
					);
					Fsphere Sphere;
					Sphere.P.set(-0.75f, 0.0f, 0.0f);
					Sphere.R = 0.5f;
					LegacyShape->add_sphere(Sphere);
					Fmatrix Box = Fidentity;
					Box.scale(1.0f, 0.5f, 0.75f);
					const Fvector BoxPosition = {0.75f, 0.0f, 0.0f};
					Box.translate_over(BoxPosition);
					LegacyShape->add_box(Box);
					LegacyShape->SetLoadedState();
					LegacyShape->Select(true);
					LegacyShapeTools->_AppendObject(LegacyShape);
				}
				ESceneCustomOTool* LegacyPuddleTools =
					Scene->GetOTool(OBJCLASS_PUDDLES);
				MakeLegacyToolVisible(LegacyPuddleTools);
				CPuddle* LegacyPuddle = nullptr;
				if (LegacyPuddleTools)
				{
					LegacyPuddle = static_cast<CPuddle*>(
						LegacyPuddleTools->CreateObject(
							nullptr,
							"tiramisu_legacy_puddle_smoke"
						)
					);
					const Fvector PuddlePosition = {0.0f, -0.25f, 0.0f};
					LegacyPuddle->SetPosition(PuddlePosition);
					LegacyPuddle->SetLoadedState();
					LegacyPuddle->Select(true);
					LegacyPuddleTools->_AppendObject(LegacyPuddle);
				}
				auto* LegacyWallmarkTools =
					smart_cast<ESceneWallmarkTool*>(
						Scene->GetTool(OBJCLASS_WM)
					);
				MakeLegacyToolVisible(LegacyWallmarkTools);
				ESceneWallmarkTool::wm_slot* LegacyWallmarkSlot = nullptr;
				ESceneWallmarkTool::wallmark* LegacyWallmark = nullptr;
				if (LegacyWallmarkTools)
				{
					LegacyWallmarkSlot = new ESceneWallmarkTool::wm_slot(
						"effects\\wallmarkblend",
						"textures/default/default_white"
					);
					LegacyWallmark = new ESceneWallmarkTool::wallmark();
					LegacyWallmark->parent = LegacyWallmarkSlot;
					LegacyWallmark->w = 0.8f;
					LegacyWallmark->h = 0.8f;
					LegacyWallmark->flags.set(
						ESceneWallmarkTool::wallmark::flSelected,
						true
					);
					LegacyWallmark->verts.resize(3);
					LegacyWallmark->verts[0].p.set(-0.4f, -0.4f, 0.0f);
					LegacyWallmark->verts[0].t.set(0.0f, 1.0f);
					LegacyWallmark->verts[1].p.set(-0.4f, 0.4f, 0.0f);
					LegacyWallmark->verts[1].t.set(0.0f, 0.0f);
					LegacyWallmark->verts[2].p.set(0.4f, 0.4f, 0.0f);
					LegacyWallmark->verts[2].t.set(1.0f, 0.0f);
					LegacyWallmark->bbox.invalidate();
					for (const FVF::LIT& Vertex : LegacyWallmark->verts)
					{
						LegacyWallmark->bbox.modify(Vertex.p);
					}
					LegacyWallmark->bbox.getcenter(
						LegacyWallmark->bounds.P
					);
					LegacyWallmark->bounds.R = 0.6f;
					LegacyWallmarkSlot->items.push_back(LegacyWallmark);
					LegacyWallmarkTools->marks.push_back(
						LegacyWallmarkSlot
					);
				}
				ESceneCustomOTool* LegacySoundTools =
					Scene->GetOTool(OBJCLASS_SOUND_SRC);
				MakeLegacyToolVisible(LegacySoundTools);
				ESoundSource* LegacySound = nullptr;
				if (LegacySoundTools)
				{
					LegacySound = static_cast<ESoundSource*>(
						LegacySoundTools->CreateObject(
							nullptr,
							"tiramisu_legacy_sound_smoke"
						)
					);
					const Fvector SoundPosition = {0.0f, 0.5f, 0.0f};
					LegacySound->SetPosition(SoundPosition);
					LegacySound->SetLoadedState();
					LegacySound->Select(true);
					LegacySoundTools->_AppendObject(LegacySound);
				}
				ESceneCustomOTool* LegacySoundEnvironmentTools =
					Scene->GetOTool(OBJCLASS_SOUND_ENV);
				MakeLegacyToolVisible(LegacySoundEnvironmentTools);
				ESoundEnvironment* LegacySoundEnvironment = nullptr;
				if (LegacySoundEnvironmentTools)
				{
					LegacySoundEnvironment =
						static_cast<ESoundEnvironment*>(
							LegacySoundEnvironmentTools->CreateObject(
								nullptr,
								"tiramisu_legacy_sound_env_smoke"
							)
						);
					LegacySoundEnvironment->SetLoadedState();
					LegacySoundEnvironment->Select(true);
					LegacySoundEnvironmentTools->_AppendObject(
						LegacySoundEnvironment
					);
				}
				ESceneCustomOTool* LegacyPortalTools =
					Scene->GetOTool(OBJCLASS_PORTAL);
				MakeLegacyToolVisible(LegacyPortalTools);
				CPortal* LegacyPortal = nullptr;
				if (LegacyPortalTools)
				{
					LegacyPortal = static_cast<CPortal*>(
						LegacyPortalTools->CreateObject(
							nullptr,
							"tiramisu_legacy_portal_smoke"
						)
					);
					LegacyPortal->Vertices() = {
						{-0.65f, -0.65f, 0.25f},
						{-0.65f, 0.65f, 0.25f},
						{0.65f, 0.65f, 0.25f},
						{0.65f, -0.65f, 0.25f}
					};
					LegacyPortal->SetLoadedState();
					LegacyPortal->Select(true);
					LegacyPortalTools->_AppendObject(LegacyPortal);
				}
				ESceneCustomOTool* LegacyGlowTools =
					Scene->GetOTool(OBJCLASS_GLOW);
				MakeLegacyToolVisible(LegacyGlowTools);
				CGlow* LegacyGlow = nullptr;
				if (LegacyGlowTools)
				{
					LegacyGlow = static_cast<CGlow*>(
						LegacyGlowTools->CreateObject(
							nullptr,
							"tiramisu_legacy_glow_smoke"
						)
					);
					LegacyGlow->m_TexName =
						"textures/default/default_white";
					LegacyGlow->m_fRadius = 0.35f;
					const Fvector GlowPosition = {0.0f, 0.0f, 0.5f};
					LegacyGlow->SetPosition(GlowPosition);
					LegacyGlow->SetLoadedState();
					LegacyGlow->Select(true);
					LegacyGlowTools->_AppendObject(LegacyGlow);
				}
				ESceneCustomOTool* LegacySpawnTools =
					Scene->GetOTool(OBJCLASS_SPAWNPOINT);
				MakeLegacyToolVisible(LegacySpawnTools);
				CSpawnPoint* LegacySpawn = nullptr;
				CSpawnPoint* LegacyIdleSpawn = nullptr;
				if (LegacySpawnTools)
				{
					LegacySpawn = static_cast<CSpawnPoint*>(
						LegacySpawnTools->CreateObject(
							const_cast<char*>(RPOINT_CHOOSE_NAME),
							"tiramisu_legacy_spawn_smoke"
						)
					);
					const Fvector SpawnPosition = {0.0f, 0.0f, 0.75f};
					static_cast<CCustomObject*>(LegacySpawn)->SetPosition(
						SpawnPosition
					);
					LegacySpawn->SetLoadedState();
					LegacySpawn->Select(true);
					LegacySpawnTools->_AppendObject(LegacySpawn);

					LegacyIdleSpawn = static_cast<CSpawnPoint*>(
						LegacySpawnTools->CreateObject(
							const_cast<char*>("campfire"),
							"tiramisu_legacy_idle_spawn_smoke"
						)
					);
					const Fvector IdleSpawnPosition = {
						0.5f,
						0.0f,
						0.75f
					};
					static_cast<CCustomObject*>(LegacyIdleSpawn)->SetPosition(
						IdleSpawnPosition
					);
					if (LegacyShapeTools)
					{
						auto* AttachedShape = static_cast<CEditShape*>(
							LegacyShapeTools->CreateObject(
								nullptr,
								"tiramisu_spawn_attached_shape_smoke"
							)
						);
						Fsphere AttachedSphere;
						AttachedSphere.identity();
						AttachedSphere.R = 0.4f;
						AttachedShape->add_sphere(AttachedSphere);
						static_cast<CCustomObject*>(AttachedShape)->SetPosition(
							IdleSpawnPosition
						);
						AttachedShape->SetLoadedState();
						LegacyShapeTools->_AppendObject(AttachedShape);
						if (!LegacyIdleSpawn->AttachObject(AttachedShape))
						{
							LegacyShapeTools->_RemoveObject(AttachedShape);
							xr_delete(AttachedShape);
						}
					}
					LegacyIdleSpawn->SetLoadedState();
					LegacySpawnTools->_AppendObject(LegacyIdleSpawn);
				}
				auto* LegacyAiMap = smart_cast<ESceneAIMapTool*>(
					Scene->GetTool(OBJCLASS_AIMAP)
				);
				MakeLegacyToolVisible(LegacyAiMap);
				SAINode* LegacyAiNodeA = nullptr;
				SAINode* LegacyAiNodeB = nullptr;
				if (LegacyAiMap)
				{
					LegacyAiNodeA = new SAINode();
					LegacyAiNodeB = new SAINode();
					LegacyAiNodeA->Pos.set(-0.25f, 0.0f, 0.0f);
					LegacyAiNodeB->Pos.set(0.25f, 0.0f, 0.0f);
					const Fvector AiNormal = {0.0f, 1.0f, 0.0f};
					LegacyAiNodeA->Plane.build(
						LegacyAiNodeA->Pos,
						AiNormal
					);
					LegacyAiNodeB->Plane.build(
						LegacyAiNodeB->Pos,
						AiNormal
					);
					LegacyAiNodeA->n3 = LegacyAiNodeB;
					LegacyAiNodeB->n1 = LegacyAiNodeA;
					LegacyAiNodeA->flags.set(
						SAINode::flSelected,
						true
					);
					LegacyAiMap->Nodes().push_back(LegacyAiNodeA);
					LegacyAiMap->Nodes().push_back(LegacyAiNodeB);
				}
				ESceneCustomOTool* LegacyWayTools =
					Scene->GetOTool(OBJCLASS_WAY);
				MakeLegacyToolVisible(LegacyWayTools);
				CWayObject* LegacyWay = nullptr;
				if (LegacyWayTools)
				{
					LegacyWay = static_cast<CWayObject*>(
						LegacyWayTools->CreateObject(
							nullptr,
							"tiramisu_legacy_way_smoke"
						)
					);
					CWayPoint* FirstWayPoint =
						LegacyWay->WayPoints().front();
					FirstWayPoint->MoveTo({-0.5f, 0.0f, 0.0f});
					CWayPoint* SecondWayPoint =
						LegacyWay->AppendWayPoint();
					SecondWayPoint->MoveTo({0.5f, 0.0f, 0.0f});
					FirstWayPoint->AddDoubleLink(SecondWayPoint);
					LegacyWay->SetLoadedState();
					LegacyWay->Select(true);
					LegacyWayTools->_AppendObject(LegacyWay);
				}
				ESceneCustomOTool* LegacyGroupTools =
					Scene->GetOTool(OBJCLASS_GROUP);
				MakeLegacyToolVisible(LegacyGroupTools);
				CGroupObject* LegacyGroup = nullptr;
				if (LegacyGroupTools)
				{
					LegacyGroup = static_cast<CGroupObject*>(
						LegacyGroupTools->CreateObject(
							nullptr,
							"tiramisu_legacy_group_smoke"
						)
					);
					LegacyGroup->SetLoadedState();
					LegacyGroup->Select(true);
					LegacyGroupTools->_AppendObject(LegacyGroup);
				}
				ESceneCustomOTool* LegacyTerrainTools =
					Scene->GetOTool(OBJCLASS_TERRAIN);
				MakeLegacyToolVisible(LegacyTerrainTools);
				CTerrain* LegacyTerrain = nullptr;
				if (LegacyTerrainTools)
				{
					LegacyTerrain = static_cast<CTerrain*>(
						LegacyTerrainTools->CreateObject(
							nullptr,
							"tiramisu_legacy_terrain_smoke"
						)
					);
					XRay::Editor::HeightmapUtils::SHeightMap HeightMap;
					HeightMap.Width = 2;
					HeightMap.Height = 2;
					HeightMap.Data = xr_alloc<float>(4);
					HeightMap.Data[0] = 0.20f;
					HeightMap.Data[1] = 0.25f;
					HeightMap.Data[2] = 0.30f;
					HeightMap.Data[3] = 0.35f;
					HeightMap.MinH = 0.20f;
					HeightMap.MaxH = 0.35f;
					XRay::Editor::HeightmapUtils::GenerateMeshByHeightmap(
						HeightMap,
						LegacyTerrain->GetReference(),
						1
					);
					for (CSurface* Surface :
						 LegacyTerrain->GetReference()->m_Surfaces)
					{
						Surface->SetShader("default");
						Surface->SetTexture(
							"textures/default/default_white"
						);
					}
					LegacyTerrain->SetLoadedState();
					static_cast<CCustomObject*>(LegacyTerrain)->Select(true);
					LegacyTerrainTools->ESceneCustomOTool::_AppendObject(
						LegacyTerrain
					);
				}
				ESceneCustomOTool* LegacyFogTools =
					Scene->GetOTool(OBJCLASS_FOG_VOL);
				MakeLegacyToolVisible(LegacyFogTools);
				EFogVolume* LegacyFogEmitter = nullptr;
				EFogVolume* LegacyFogOcclusion = nullptr;
				if (LegacyFogTools)
				{
					LegacyFogEmitter = static_cast<EFogVolume*>(
						LegacyFogTools->CreateObject(
							nullptr,
							"tiramisu_legacy_fog_emitter_smoke"
						)
					);
					const Fvector EmitterPosition = {-1.0f, 0.0f, 0.0f};
					LegacyFogEmitter->SetPosition(EmitterPosition);
					LegacyFogEmitter->SetLoadedState();
					static_cast<CCustomObject*>(LegacyFogEmitter)->Select(true);
					LegacyFogTools->_AppendObject(LegacyFogEmitter);

					LegacyFogOcclusion = static_cast<EFogVolume*>(
						LegacyFogTools->CreateObject(
							nullptr,
							"tiramisu_legacy_fog_occlusion_smoke"
						)
					);
					LegacyFogOcclusion->m_volumeType = fvOcclusion;
					LegacyFogOcclusion->SetDrawColor(
						0x2050a050u,
						0xff202020u
					);
					const Fvector OcclusionPosition = {1.0f, 0.0f, 0.0f};
					LegacyFogOcclusion->SetPosition(OcclusionPosition);
					LegacyFogOcclusion->SetLoadedState();
					LegacyFogTools->_AppendObject(LegacyFogOcclusion);
				}
				auto* LegacyDetails = static_cast<EDetailManager*>(
					Scene->GetTool(OBJCLASS_DO)
				);
				MakeLegacyToolVisible(LegacyDetails);
				bool LegacyDetailPrepared = false;
				bool LegacyDetailBasePrepared = false;
				CSceneObject* LegacyDetailBaseSnap = nullptr;
				if (LegacyDetails)
				{
					LegacyDetails->SetObjectsDrawEnabled(true);
					LegacyDetails->SetSlotBoxesDrawEnabled(true);
					EDetail* Detail = LegacyDetails->AppendDO(
						"detail\\det_hvosh"
					);
					if (Detail)
					{
						LegacyDetails->dtH.object_count = 1;
						LegacyDetails->dtH.offs_x = 0;
						LegacyDetails->dtH.offs_z = 0;
						LegacyDetails->dtH.size_x = 1;
						LegacyDetails->dtH.size_z = 1;
						LegacyDetails->dtSlots = xr_alloc<DetailSlot>(1);
						ZeroMemory(
							LegacyDetails->dtSlots,
							sizeof(DetailSlot)
						);
						LegacyDetails->dtSlots[0].w_y(-0.5f, 1.0f);
						for (u32 Part = 0; Part < dm_obj_in_slot; ++Part)
						{
							LegacyDetails->dtSlots[0].w_id(
								Part,
								DetailSlot::ID_Empty
							);
						}
						LegacyDetails->m_Selected.assign(1, 1);
						const Fvector& CameraPosition =
							UI->CurrentView().m_Camera.GetPosition();
						LegacyDetails->cache_Update(CameraPosition);
						if (!LegacyDetails->cache_pool.empty())
						{
							CDetailManager::Slot& Slot =
								LegacyDetails->cache_pool.front();
							Slot.empty = 0;
							Slot.type = CDetailManager::stReady;
							Slot.G[0].id = 0;
							CDetail::SlotItem* Item =
								LegacyDetails->items_pool.create();
							Item->quat.set(0.0f, 0.0f, 0.0f);
							Item->scale = 0.75f;
							Item->pos.set(CameraPosition);
							Item->pos.mad(
								UI->CurrentView().m_Camera.GetDirection(),
								2.0f
							);
							Item->c_hemi = 1.0f;
							Slot.G[0].items[0].push_back(Item);
							LegacyDetailPrepared = true;
						}

						LegacyDetailBaseSnap = new CSceneObject(
							nullptr,
							"tiramisu_detail_base_snap_smoke"
						);
						if (LegacyDetailBaseSnap->SetReference(
								"detail\\det_hvosh"
							) &&
							LegacyDetails->m_Base.LoadImage(
								"detail\\detail_asfalt_det1"
							))
						{
							Fvector SnapPosition = CameraPosition;
							SnapPosition.mad(
								UI->CurrentView().m_Camera.GetDirection(),
								2.0f
							);
							LegacyDetailBaseSnap->SetPosition(SnapPosition);
							LegacyDetailBaseSnap->SetLoadedState();
							Fbox SnapBox;
							if (LegacyDetailBaseSnap->GetBox(SnapBox))
							{
								LegacyDetails->GetSnapList()->push_back(
									LegacyDetailBaseSnap
								);
								LegacyDetails->m_Base.CreateRMFromObjects(
									SnapBox,
									*LegacyDetails->GetSnapList()
								);
								LegacyDetails->SetBaseTextureDrawEnabled(
									true,
									true
								);
								LegacyDetailBasePrepared =
									!LegacyDetails->m_Base
										.GetRenderMesh()
										.empty();
							}
						}
					}
				}
				if (!LegacyDetailPrepared || !LegacyDetailBasePrepared)
				{
					Msg(
						"! Legacy detail bridge smoke setup failed: "
						"instances=%u, base=%u",
						LegacyDetailPrepared ? 1u : 0u,
						LegacyDetailBasePrepared ? 1u : 0u
					);
				}
				else
				{
					Msg(
						"* Legacy detail bridge smoke prepared: "
						"visible=%u, render-data=%u, task-finished=%u",
						LegacyDetails->IsVisible() ? 1u : 0u,
						LegacyDetails->HasTiramisuRenderData() ? 1u : 0u,
						LegacyDetails->task_finished.load() ? 1u : 0u
					);
				}
				EditorNriBackend->ResizeViewport(
					SmokeLegacyLightViewportId,
					320,
					240
				);
				const bool LegacyLightSubmitted = LegacyDetailPrepared &&
					LegacyDetailBasePrepared &&
					SubmitEditorSceneToEditorRenderer(
						SmokeLegacyLightViewportId
					);
				if (LegacyDetails)
				{
					LegacyDetails->Clear();
				}
				xr_delete(LegacyDetailBaseSnap);
				LegacyLightTools->_RemoveObject(LegacyLight);
				xr_delete(LegacyLight);
				if (LegacyShape)
				{
					LegacyShapeTools->_RemoveObject(LegacyShape);
					xr_delete(LegacyShape);
				}
				if (LegacyPuddle)
				{
					LegacyPuddleTools->_RemoveObject(LegacyPuddle);
					xr_delete(LegacyPuddle);
				}
				if (LegacyWallmarkSlot)
				{
					std::erase(
						LegacyWallmarkTools->marks,
						LegacyWallmarkSlot
					);
					LegacyWallmarkSlot->items.clear();
					xr_delete(LegacyWallmark);
					xr_delete(LegacyWallmarkSlot);
				}
				if (LegacySound)
				{
					LegacySoundTools->_RemoveObject(LegacySound);
					xr_delete(LegacySound);
				}
				if (LegacySoundEnvironment)
				{
					LegacySoundEnvironmentTools->_RemoveObject(
						LegacySoundEnvironment
					);
					xr_delete(LegacySoundEnvironment);
				}
				if (LegacyPortal)
				{
					LegacyPortalTools->_RemoveObject(LegacyPortal);
					xr_delete(LegacyPortal);
				}
				if (LegacyGlow)
				{
					LegacyGlowTools->_RemoveObject(LegacyGlow);
					xr_delete(LegacyGlow);
				}
				if (LegacySpawn)
				{
					LegacySpawnTools->_RemoveObject(LegacySpawn);
					xr_delete(LegacySpawn);
				}
				if (LegacyIdleSpawn)
				{
					LegacySpawnTools->_RemoveObject(LegacyIdleSpawn);
					xr_delete(LegacyIdleSpawn);
				}
				if (LegacyAiMap && LegacyAiNodeA && LegacyAiNodeB)
				{
					LegacyAiMap->Nodes().pop_back();
					LegacyAiMap->Nodes().pop_back();
					xr_delete(LegacyAiNodeA);
					xr_delete(LegacyAiNodeB);
				}
				if (LegacyWay)
				{
					LegacyWayTools->_RemoveObject(LegacyWay);
					xr_delete(LegacyWay);
				}
				if (LegacyGroup)
				{
					LegacyGroupTools->_RemoveObject(LegacyGroup);
					xr_delete(LegacyGroup);
				}
				if (LegacyFogEmitter)
				{
					LegacyFogTools->_RemoveObject(LegacyFogEmitter);
					xr_delete(LegacyFogEmitter);
				}
				if (LegacyFogOcclusion)
				{
					LegacyFogTools->_RemoveObject(LegacyFogOcclusion);
					xr_delete(LegacyFogOcclusion);
				}
				if (LegacyTerrain)
				{
					LegacyTerrainTools->ESceneCustomOTool::_RemoveObject(
						LegacyTerrain
					);
					xr_delete(LegacyTerrain);
				}
				for (const auto& [Tool, WasVisible] :
					 LegacyToolVisibility)
				{
					Tool->m_EditFlags.set(
						ESceneToolBase::flVisible,
						WasVisible
					);
				}
				if (!LegacyLightSubmitted)
				{
					Msg("! Legacy light bridge smoke submit failed");
					ProcessExitCode = 6;
					GContentView->Destroy();
					NeedExit = true;
				}
			}
			FEditorParticleInstance PreviewParticle = ParticleInstances[0];
			PreviewParticle.ObjectId = {0x7072657669657770ull};
			PreviewParticle.LocalToWorld = {};
			PreviewParticle.LocalToWorld[0] = 1.0f;
			PreviewParticle.LocalToWorld[5] = 1.0f;
			PreviewParticle.LocalToWorld[10] = 1.0f;
			PreviewParticle.LocalToWorld[15] = 1.0f;
			FEditorViewportSceneSnapshot PreviewSnapshot;
			PreviewSnapshot.Camera = Snapshot.Camera;
			PreviewSnapshot.ParticleInstances =
				xr_span(&PreviewParticle, 1);
			PreviewSnapshot.DebugDrawRevision = 1;
			PreviewSnapshot.Revision = 1;
			EditorNriBackend->ResizeViewport(
				SmokeParticlePreviewViewportId,
				384,
				384
			);
			if (!EditorNriBackend->SubmitViewportScene(
					SmokeParticlePreviewViewportId,
					PreviewSnapshot
				))
			{
				Msg("! Particle preview smoke setup failed: %.*s",
					static_cast<int>(
						EditorNriBackend->GetLastDiagnostic().size()
					),
					EditorNriBackend->GetLastDiagnostic().data());
				ProcessExitCode = 6;
				GContentView->Destroy();
				NeedExit = true;
			}
			else
			{
				FEditorViewportPickRequest PickRequest;
				PickRequest.RayOrigin = {0.6f, 0.0f, -1.0f};
				PickRequest.RayDirection = {0.0f, 0.0f, 1.0f};
				PickRequest.MaxDistance = 10.0f;
				const FEditorViewportPickResult Pick =
					EditorNriBackend->PickViewport(
						SmokeSceneViewportId, PickRequest
					);
				if (!Pick.Hit || Pick.ObjectId != Instances[0].ObjectId ||
					Pick.MaterialSlot != SmokeSceneMaterialSlot)
				{
					Msg("! Viewport picking smoke failed");
					ProcessExitCode = 8;
					GContentView->Destroy();
					NeedExit = true;
				}
				else
				{
					Msg("* Viewport material smoke: scene submitted, CPU pick distance=%.3f",
						Pick.Distance);
				}
			}
		}
	}

	if ((LegacyConversionSmokeRequested || ZatonConversionSmokeRequested) &&
		!NeedExit)
	{
		if (!EditorNriBackend)
		{
			Msg("! Legacy conversion smoke requires -tiramisu-editor");
			ProcessExitCode = 9;
		}
		else
		{
			FLegacyConversionSmokeOptions SmokeOptions;
			if (ZatonConversionSmokeRequested)
			{
				SmokeOptions.LevelSource = std::filesystem::absolute(
					"rawdata/levels/!FinalSP/zaton.level"
				);
				SmokeOptions.OutputRoot = std::filesystem::absolute(
											  "build/test-results/tiramisu"
										  ) /
										  ("zaton-" + std::to_string(
														  std::chrono::system_clock::now()
															  .time_since_epoch()
															  .count()
													  ));
				SmokeOptions.KeepOutput = true;
				Msg("* Legacy conversion smoke: full zaton level selected");
			}
			if (!RunLegacyConversionSmoke(SmokeOptions))
			{
				ProcessExitCode = 10;
			}
		}
		GContentView->Destroy();
		NeedExit = true;
	}

	if (ZatonRuntimeProfileRequested && !NeedExit)
	{
		if (!HiddenTestWindow ||
			!EditorNriConfig.DeterministicTest.Enabled || !EditorNriBackend)
		{
			Msg(
				"! Zaton runtime profile requires Tiramisu, -rdbg, "
				"-render-deterministic and -editor-test-hidden"
			);
			ProcessExitCode = 13;
			GContentView->Destroy();
			NeedExit = true;
		}
		else
		{
			const std::filesystem::path ZatonPath =
				std::filesystem::absolute(
					"rawdata/levels/!FinalSP/zaton.level"
				);
			if (!std::filesystem::exists(ZatonPath) ||
				!ExecCommand(COMMAND_LOAD, xr_string(ZatonPath.string().c_str())))
			{
				Msg("! Zaton runtime profile could not start level loading");
				ProcessExitCode = 14;
				GContentView->Destroy();
				NeedExit = true;
			}
			else
			{
				LUI->LoaderEvent.wait();
				const std::filesystem::path ProfileDirectory =
					std::filesystem::absolute(
						"build/test-results/tiramisu/profiles"
					);
				std::filesystem::create_directories(ProfileDirectory);
				const char* BackendName =
					EditorNriConfig.Api ==
						ETiramisuEditorGraphicsApi::D3D12
					? "d3d12"
					: "vulkan";
				xr_string CaptureName =
					"zaton-runtime-" + xr_string(BackendName);
#if defined(IXRAY_ASAN_BUILD)
				CaptureName += "-asan";
#endif
				const std::filesystem::path ProfilePath =
					ProfileDirectory /
					(CaptureName + ".opt");
				ZatonRuntimeProfilePath = ProfilePath.string().c_str();
				constexpr size_t WarmupFrameCount = 60;
				constexpr size_t CaptureFrameCount = 60;
				ZatonRuntimeWarmupFrames = WarmupFrameCount;
				ZatonRuntimeFrameMilliseconds.reserve(CaptureFrameCount);
				ZatonRuntimeProfileActive = true;
				Msg(
					"* Zaton runtime profile: level loaded, warming up %zu "
					"frames before a %zu-frame capture (%s)",
					WarmupFrameCount,
					CaptureFrameCount,
					BackendName
				);
			}
		}
	}

	const bool RenderDocSmokeRequested =
		Core.ParamsData.test(ECoreParams::renderdoc) &&
		HasRenderCommandLineFlag(
			Core.Params ? Core.Params : "", "-renderdoc-capture"
		) &&
		(MaterialPreviewSmokeRequested || ViewportMaterialSmokeRequested);
	bool RenderDocSmokeCaptureArmed = false;
	if (RenderDocSmokeRequested && !NeedExit)
	{
		if (!xrRenderDoc::IsAvailable())
		{
			Msg("! RenderDoc smoke cannot access the in-application API");
			ProcessExitCode = 12;
			GContentView->Destroy();
			NeedExit = true;
		}
		else
		{
			LUI->SetRenderDocCaptureGateOpen(false);
			Msg(
				"* RenderDoc smoke: capture waits for ready material pipelines"
			);
		}
	}

	while (!NeedExit)
	{
		PROF_FRAME("LevelEditor Zaton Runtime");
		SDL_Event Event;
		while (SDL_PollEvent(&Event))
		{
			switch (Event.type)
			{
				case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
				{
					SDL_WindowID MainWndID = SDL_GetWindowID(g_AppInfo.Window);
					if (Event.window.windowID == MainWndID)
					{
						GContentView->Destroy();
						NeedExit = true;
					}

					break;
				}
				case SDL_EVENT_WINDOW_RESIZED:
				{
					SDL_WindowID MainWndID = SDL_GetWindowID(g_AppInfo.Window);
					if (UI && REDevice && Event.window.windowID == MainWndID)
					{
						if (Event.window.data1 != DevicePtr->Width || Event.window.data2 != DevicePtr->Height)
						{
							UI->Resize(Event.window.data1, Event.window.data2, true);
							EPrefs->SaveConfig();
						}
					}
					break;
				}
				case SDL_EVENT_WINDOW_SHOWN:
				case SDL_EVENT_WINDOW_MOUSE_ENTER:
					Device.b_is_Active = true;
					// if (UI) UI->OnAppActivate();

					break;
				case SDL_EVENT_WINDOW_HIDDEN:
				case SDL_EVENT_WINDOW_MOUSE_LEAVE:
					Device.b_is_Active = !!psDeviceFlags.test(rsDeviceActive);
					// if (UI)UI->OnAppDeactivate();
					break;

				case SDL_EVENT_KEY_DOWN:
					if (UI)
					{
						UI->KeyDown(Event.key.scancode, UI->GetShiftState());
						UI->ApplyShortCutInput(Event.key.scancode);

						if (UI->IsPlayInEditor())
						{
							if (pInput->IsAcquire)
							{
								pInput->KeyboardButtonUpdate(Event.key.scancode, true);
							}
							else if (Event.key.scancode == SDL_SCANCODE_LALT)
							{
								pInput->acquire();
								UI->IsEnableInput = false;
								ShowCursor(false);
							}
						}
					}
					break;
				case SDL_EVENT_KEY_UP:
					if (UI)
					{
						UI->KeyUp(Event.key.scancode, UI->GetShiftState());
						if (UI->IsPlayInEditor() && pInput->IsAcquire)
						{
							if (pInput->IsAcquire)
							{
								pInput->KeyboardButtonUpdate(Event.key.scancode, false);
							}
						}
					}
					break;
				case SDL_EVENT_MOUSE_MOTION:
				{
					if (UI->IsPlayInEditor() && !pInput->IsAcquire)
					{
						break;
					}

					pInput->MouseMotion(Event.motion.xrel, Event.motion.yrel);
				}
				break;
				case SDL_EVENT_MOUSE_WHEEL:
				{
					if (UI->IsPlayInEditor() && !pInput->IsAcquire)
					{
						break;
					}

					pInput->MouseScroll(Event.wheel.y);
				}
				break;
				case SDL_EVENT_MOUSE_BUTTON_DOWN:
				case SDL_EVENT_MOUSE_BUTTON_UP:
				{
					if (UI->IsPlayInEditor() && !pInput->IsAcquire)
					{
						break;
					}

					int mouse_button = 0;
					if (Event.button.button == SDL_BUTTON_LEFT)
					{
						mouse_button = 0;
					}
					if (Event.button.button == SDL_BUTTON_RIGHT)
					{
						mouse_button = 1;
					}
					if (Event.button.button == SDL_BUTTON_MIDDLE)
					{
						mouse_button = 2;
					}
					if (Event.type == SDL_EVENT_MOUSE_BUTTON_DOWN)
					{
						pInput->MousePressed(mouse_button);
					}
					else
					{
						pInput->MouseReleased(mouse_button);
					}
				}
				break;
			}

			if (!UI->ProcessEvent(&Event))
			{
				break;
			}
		}

		if (SmokePreviewHandle.IsValid() && !MaterialPreviewSmokeComplete)
		{
			SmokePreviewRenderer->RenderPreview(SmokePreviewHandle, 1.0f / 60.0f);
		}
		if (ViewportMaterialSmokeRequested && EditorNriBackend)
		{
			EditorNriBackend->CaptureViewport(SmokeSceneViewportId);
			EditorNriBackend->CaptureViewport(
				SmokeParticlePreviewViewportId
			);
			EditorNriBackend->CaptureViewport(
				SmokeLegacyLightViewportId
			);
		}

		const auto RuntimeFrameStart = std::chrono::steady_clock::now();
		MainForm->Frame();
		if (ZatonRuntimeProfileActive)
		{
			if (ZatonRuntimeWarmupFrames != 0)
			{
				--ZatonRuntimeWarmupFrames;
				if (ZatonRuntimeWarmupFrames == 0)
				{
					PROF_START_CAPTURE();
					ZatonRuntimeCaptureStarted = true;
					Msg(
						"* Zaton runtime profile: warmup complete, "
						"capture started"
					);
				}
				continue;
			}
			const double FrameMilliseconds =
				static_cast<double>(
					std::chrono::duration_cast<std::chrono::microseconds>(
						std::chrono::steady_clock::now() - RuntimeFrameStart
					).count()
				) / 1000.0;
			ZatonRuntimeFrameMilliseconds.push_back(FrameMilliseconds);
			const size_t FrameNumber =
				ZatonRuntimeFrameMilliseconds.size();
			if (FrameNumber <= 3 || FrameNumber % 5 == 0)
			{
				Msg(
					"* Zaton runtime profile: frame=%zu, cpu=%.2f ms",
					FrameNumber,
					FrameMilliseconds
				);
			}
			if (FrameNumber >= 60)
			{
				if (ZatonRuntimeCaptureStarted)
				{
					PROF_STOP_CAPTURE();
					PROF_SAVE_CAPTURE(
						ZatonRuntimeProfilePath.c_str()
					);
				}
				xr_vector<double> Sorted = ZatonRuntimeFrameMilliseconds;
				std::ranges::sort(Sorted);
				const double Median = Sorted[Sorted.size() / 2];
				const size_t P95Index = std::min(
					Sorted.size() - 1,
					(Sorted.size() * 95 + 99) / 100 - 1
				);
				Msg(
					"* Zaton runtime profile: success "
					"(frames=%zu, first=%.2f ms, p50=%.2f ms, "
					"p95=%.2f ms, max=%.2f ms, capture=%s)",
					Sorted.size(),
					ZatonRuntimeFrameMilliseconds.front(),
					Median,
					Sorted[P95Index],
					Sorted.back(),
					ZatonRuntimeProfilePath.c_str()
				);
				ZatonRuntimeProfileActive = false;
				GContentView->Destroy();
				NeedExit = true;
			}
		}

		if (SmokePreviewHandle.IsValid())
		{
			const FMaterialPreviewFrame Frame =
				SmokePreviewRenderer->GetPreviewFrame(SmokePreviewHandle);
			const bool TimedOut = std::chrono::steady_clock::now() >= SmokeDeadline;
			if (Frame.State == EMaterialPreviewState::Error || TimedOut)
			{
				if (Frame.Diagnostic.empty())
				{
					Msg("! Material preview smoke timed out");
				}
				else
				{
					Msg("! Material preview smoke failed: %.*s",
						static_cast<int>(Frame.Diagnostic.size()),
						Frame.Diagnostic.data());
				}
				ProcessExitCode = 4;
				SmokePreviewRenderer->DestroyPreview(SmokePreviewHandle);
				SmokePreviewHandle = {};
				GContentView->Destroy();
				NeedExit = true;
			}
			else if (Frame.State == EMaterialPreviewState::Ready &&
					 Frame.AcceptedRevision == 1 && Frame.Surface.IsValid())
			{
				Msg("* Material preview smoke: success (%ux%u)",
					Frame.Surface.Width,
					Frame.Surface.Height);
				if (!RenderDocSmokeRequested)
				{
					SmokePreviewRenderer->DestroyPreview(SmokePreviewHandle);
					SmokePreviewHandle = {};
				}
				MaterialPreviewSmokeComplete = true;
				if (ViewportMaterialSmokeComplete && !RenderDocSmokeRequested)
				{
					GContentView->Destroy();
					NeedExit = true;
				}
			}
		}

		if (ViewportMaterialSmokeRequested && !ViewportMaterialSmokeComplete &&
			EditorNriBackend)
		{
			const FEditorViewportMaterialStatus Status =
				EditorNriBackend->GetViewportMaterialStatus(
					SmokeSceneViewportId, SmokeSceneMaterialSlot
				);
			const FEditorViewportMaterialStatus SpriteStatus =
				EditorNriBackend->GetViewportMaterialStatus(
					SmokeSceneViewportId, SmokeSceneSpriteMaterialSlot
				);
			const FEditorViewportMaterialStatus ParticleStatus =
				EditorNriBackend->GetViewportMaterialStatus(
					SmokeSceneViewportId, SmokeSceneParticleMaterialSlot
				);
			const FEditorViewportMaterialStatus CloneStatus =
				EditorNriBackend->GetViewportMaterialStatus(
					SmokeSceneViewportId,
					SmokeSceneLastCloneMaterialSlot
				);
			const FEditorViewportSurface Surface =
				EditorNriBackend->GetViewportSurface(SmokeSceneViewportId);
			const FEditorViewportMaterialStatus ParticlePreviewStatus =
				EditorNriBackend->GetViewportMaterialStatus(
					SmokeParticlePreviewViewportId,
					SmokeSceneParticleMaterialSlot
				);
			const FEditorViewportMaterialStatus DecalStatus =
				EditorNriBackend->GetViewportMaterialStatus(
					SmokeSceneViewportId,
					SmokeSceneDecalMaterialSlot
				);
			const FEditorViewportSurface ParticlePreviewSurface =
				EditorNriBackend->GetViewportSurface(
					SmokeParticlePreviewViewportId
				);
			const FEditorViewportMaterialStatus LegacyLightStatus =
				EditorNriBackend->GetViewportMaterialStatus(
					SmokeLegacyLightViewportId,
					SmokeSceneMaterialSlot
				);
			const FEditorViewportSurface LegacyLightSurface =
				EditorNriBackend->GetViewportSurface(
					SmokeLegacyLightViewportId
				);
			const FRenderStatisticsSnapshot RendererStatistics =
				EditorNriBackend->GetRenderStatistics();
			const FEditorRenderLifecycleStatus LifecycleStatus =
				EditorNriBackend->GetRenderLifecycleStatus();
			const bool RendererStatisticsReady =
				RendererStatistics.Revision != 0 &&
				RendererStatistics.Frame.PassCount >= 2 &&
				RendererStatistics.Frame.DrawCallCount >= 10 &&
				RendererStatistics.Resources.TrackedBufferCount != 0 &&
				RendererStatistics.Resources.TrackedTextureCount != 0 &&
				RendererStatistics.Resources.TrackedPipelineCount != 0;
			const bool TimedOut = std::chrono::steady_clock::now() >= SmokeDeadline;
			const bool SurfaceMaterialFailed = Status.RequestedRevision != 0 &&
											   Status.AcceptedRevision < Status.RequestedRevision &&
											   !Status.Diagnostic.empty();
			const bool SpriteMaterialFailed = SpriteStatus.RequestedRevision != 0 &&
											  SpriteStatus.AcceptedRevision < SpriteStatus.RequestedRevision &&
											  !SpriteStatus.Diagnostic.empty();
			const bool ParticleMaterialFailed = ParticleStatus.RequestedRevision != 0 &&
												ParticleStatus.AcceptedRevision < ParticleStatus.RequestedRevision &&
												!ParticleStatus.Diagnostic.empty();
			const bool DecalMaterialFailed =
				DecalStatus.RequestedRevision != 0 &&
				DecalStatus.AcceptedRevision <
					DecalStatus.RequestedRevision &&
				!DecalStatus.Diagnostic.empty();
			const bool CloneMaterialFailed = CloneStatus.RequestedRevision != 0 &&
											 CloneStatus.AcceptedRevision < CloneStatus.RequestedRevision &&
											 !CloneStatus.Diagnostic.empty();
			if (SurfaceMaterialFailed || SpriteMaterialFailed ||
				ParticleMaterialFailed || DecalMaterialFailed ||
				CloneMaterialFailed || TimedOut)
			{
				const xr_string* MaterialDiagnostic =
					&CloneStatus.Diagnostic;
				if (SurfaceMaterialFailed)
				{
					MaterialDiagnostic = &Status.Diagnostic;
				}
				else if (SpriteMaterialFailed)
				{
					MaterialDiagnostic = &SpriteStatus.Diagnostic;
				}
				else if (ParticleMaterialFailed)
				{
					MaterialDiagnostic = &ParticleStatus.Diagnostic;
				}
				else if (DecalMaterialFailed)
				{
					MaterialDiagnostic = &DecalStatus.Diagnostic;
				}
				if (MaterialDiagnostic->empty())
				{
					Msg(
						"! Viewport material smoke timed out: "
						"main=%u/%u/%u, legacy=%u/%u/%u/%u, "
						"legacy-particles=%u/%u/%u, "
						"legacy-meta=%u/%u/%u/%u/%u, legacy-ready=%u",
						Status.DrawCount,
						Status.SelectionDrawCount,
						Status.PendingModelLoadCount,
						LegacyLightStatus.DrawCount,
						LegacyLightStatus.DebugLineCount,
						LegacyLightStatus.DebugTriangleCount,
						LegacyLightStatus.OverlayTextCount,
						LegacyLightStatus.ParticleInstanceCount,
						LegacyLightStatus.SimulatedParticleCount,
						LegacyLightStatus.ParticleBillboardDrawCount,
						LegacyLightStatus.SelectionDrawCount,
						LegacyLightStatus.LightCount,
						LegacyLightStatus.ParticleGroupInstanceCount,
						LegacyLightStatus.ParticleBillboardReady ? 1u : 0u,
						LegacyLightSurface.IsValid() ? 1u : 0u,
						LegacyLightStatus.Ready ? 1u : 0u
					);
				}
				else
				{
					Msg("! Viewport material smoke failed: %s",
						MaterialDiagnostic->c_str());
				}
				ProcessExitCode = 7;
				GContentView->Destroy();
				NeedExit = true;
			}
			else if (Status.Ready && SpriteStatus.Ready &&
					 ParticleStatus.Ready && DecalStatus.Ready &&
					 CloneStatus.Ready && ParticlePreviewStatus.Ready &&
					 CloneStatus.PipelineKey == Status.PipelineKey &&
					 Status.SharedPipelineReferenceCount >=
						 SmokeSceneCloneMaterialCount + 1 &&
					 Surface.IsValid() &&
					 ParticlePreviewSurface.IsValid() &&
					 LegacyLightSurface.IsValid() &&
					 LegacyLightStatus.LightCount == 2 &&
					 LegacyLightStatus.DrawCount == 6 &&
					 LegacyLightStatus.SelectionDrawCount == 2 &&
					 LegacyLightStatus.DebugOverlayReady &&
					 LegacyLightStatus.DebugLineCount >= 456 &&
					 LegacyLightStatus.DebugTriangleCount >= 432 &&
					 LegacyLightStatus.OverlayTextCount >= 4 &&
					 LegacyLightStatus.ParticleInstanceCount == 1 &&
					 LegacyLightStatus.ParticleGroupInstanceCount == 1 &&
					 LegacyLightStatus.SimulatedParticleCount != 0 &&
					 LegacyLightStatus.ParticleBillboardReady &&
					 LegacyLightStatus.ParticleBillboardDrawCount == 1 &&
					 LegacyLightStatus.DecalReady &&
					 LegacyLightStatus.DecalInstanceCount == 1 &&
					 LegacyLightStatus.DecalDrawCount == 1 &&
					 LegacyLightStatus.DecalCulledCount == 0 &&
					 Status.ModelInstanceCount == 1 &&
					 Status.ModelDrawCount != 0 &&
					 Status.PendingModelLoadCount == 0 &&
					 Status.ModelPickingReady &&
					 Status.ModelAnimationReady &&
					 Status.ModelSkinningReady &&
					 Status.AnimatedModelCount == 1 &&
					 Status.SkinnedModelCount == 1 &&
					 Status.GpuSkinnedModelCount == 1 &&
					 Status.ModelPaletteChanged &&
					 Status.ModelPaletteMatrixCount != 0 &&
					 Status.UploadedSkinningMatrixCount ==
						 Status.ModelPaletteMatrixCount * 2 &&
					 ParticlePreviewStatus.ParticleInstanceCount == 1 &&
					 ParticlePreviewStatus.ParticleGroupInstanceCount == 0 &&
					 ParticlePreviewStatus.SimulatedParticleCount != 0 &&
					 ParticlePreviewStatus.ParticleBillboardReady &&
					 ParticlePreviewStatus.ParticleBillboardDrawCount == 1 &&
					 ParticlePreviewStatus.DrawCount == 1 &&
					 Status.DrawCount == 3 + Status.ModelDrawCount +
						 Status.ParticleBillboardDrawCount +
						 Status.DecalDrawCount &&
					 Status.DecalReady &&
					 Status.DecalInstanceCount == 2 &&
					 Status.DecalDrawCount == 1 &&
					 Status.DecalCulledCount == 1 &&
					 Status.SelectionOverlayReady &&
					 Status.SelectionDrawCount == 3 &&
					 Status.DebugOverlayReady &&
					 Status.DebugLineCount ==
						 1 + Status.ParticleInstanceCount * 3 +
							 Status.SimulatedParticleCount * 3 &&
					 Status.DebugTriangleCount == 1 &&
					 Status.ScreenOverlayReady &&
					 Status.OverlayLineCount == 1 &&
					 Status.OverlayTriangleCount == 1 &&
					 Status.OverlayTextCount == 1 &&
					 Status.LightCount == 2 &&
					 Status.ParticleInstanceCount == 2 &&
					 Status.ParticleGroupInstanceCount == 1 &&
					 Status.ParticleChildInstanceCount != 0 &&
					 Status.SimulatedParticleCount != 0 &&
					 Status.ParticleBillboardReady &&
					 Status.ParticleBillboardCount >= 2 &&
					 Status.ParticleBillboardDrawCount >= 3 &&
					 RendererStatisticsReady &&
					 (!ViewportMaterialReloadSmokeRequested ||
					  (Status.ReloadCount >= 1 && SpriteStatus.ReloadCount >= 1 &&
					   ParticleStatus.ReloadCount >= 1 &&
					   DecalStatus.ReloadCount >= 1 &&
					   CloneStatus.ReloadCount >= 1)))
			{
				if (EditorResizeSmokeRequested)
				{
					if (!EditorResizeSmokeTriggered)
					{
						if (!LifecycleStatus.PresentationReady ||
							!LifecycleStatus.DedicatedRenderThreadActive ||
							LifecycleStatus.RenderExecutionThreadId == 0 ||
							LifecycleStatus.SwapchainRevision == 0 ||
							LifecycleStatus.PresentedFrameCount == 0 ||
							Surface.Revision == 0)
						{
							continue;
						}

						ResizeSmokeSurfaceRevision = Surface.Revision;
						ResizeSmokeSwapchainRevision =
							LifecycleStatus.SwapchainRevision;
						ResizeSmokePresentedFrameCount =
							LifecycleStatus.PresentedFrameCount;
						ResizeSmokeViewportResourceRevision =
							LifecycleStatus.ViewportResourceRevision;
						ResizeSmokeTextureRedirectCount =
							LifecycleStatus.ImGuiTextureRedirectCount;
						ResizeSmokeStatisticsRevision =
							RendererStatistics.Revision;
						ResizeSmokeMaterialRevision =
							Status.AcceptedRevision;
						ResizeSmokePipelineKey = Status.PipelineKey;

						EditorNriBackend->ResizeViewport(
							SmokeSceneViewportId,
							ResizeSmokeViewportWidth,
							ResizeSmokeViewportHeight
						);
						int WindowWidth = 0;
						int WindowHeight = 0;
						SDL_GetWindowSize(
							g_AppInfo.Window,
							&WindowWidth,
							&WindowHeight
						);
						const int TargetWindowWidth =
							WindowWidth > 800 ? WindowWidth - 160
											  : WindowWidth + 160;
						const int TargetWindowHeight =
							WindowHeight > 600 ? WindowHeight - 96
											   : WindowHeight + 96;
						SDL_SetWindowSize(
							g_AppInfo.Window,
							TargetWindowWidth,
							TargetWindowHeight
						);
						EditorResizeSmokeTriggered = true;
						Msg(
							"* Editor resize smoke: requested window=%dx%d, "
							"viewport=%ux%u",
							TargetWindowWidth,
							TargetWindowHeight,
							ResizeSmokeViewportWidth,
							ResizeSmokeViewportHeight
						);
						continue;
					}

					const bool ResizeReady =
						Surface.Width == ResizeSmokeViewportWidth &&
						Surface.Height == ResizeSmokeViewportHeight &&
						Surface.Revision > ResizeSmokeSurfaceRevision &&
						LifecycleStatus.PresentationReady &&
						LifecycleStatus.DedicatedRenderThreadActive &&
						LifecycleStatus.RenderExecutionThreadId != 0 &&
						LifecycleStatus.SwapchainRevision >
							ResizeSmokeSwapchainRevision &&
						LifecycleStatus.PresentedFrameCount >
							ResizeSmokePresentedFrameCount &&
						LifecycleStatus.ViewportResourceRevision >
							ResizeSmokeViewportResourceRevision &&
						LifecycleStatus.ImGuiTextureRedirectCount >
							ResizeSmokeTextureRedirectCount &&
						RendererStatistics.Revision >
							ResizeSmokeStatisticsRevision &&
						Status.AcceptedRevision ==
							ResizeSmokeMaterialRevision &&
						Status.PipelineKey == ResizeSmokePipelineKey;
					if (!ResizeReady)
					{
						continue;
					}
					Msg(
						"* Editor resize smoke: success "
						"(surface=%ux%u/r%llu, swapchain-r=%llu, "
						"presented=%llu, redirects=%llu, render-thread=%llu)",
						Surface.Width,
						Surface.Height,
						static_cast<unsigned long long>(Surface.Revision),
						static_cast<unsigned long long>(
							LifecycleStatus.SwapchainRevision
						),
						static_cast<unsigned long long>(
							LifecycleStatus.PresentedFrameCount
						),
						static_cast<unsigned long long>(
							LifecycleStatus.ImGuiTextureRedirectCount
						),
						static_cast<unsigned long long>(
							LifecycleStatus.RenderExecutionThreadId
						)
					);
				}
				Msg(
					"* Legacy Detail base-texture smoke: success "
					"(meshes=1, textured=1, depth-bias=1, GPU draws=1)"
				);
				Msg(
					"* Legacy Detail slot-box smoke: success "
					"(slots=1, selected=1, lines=12)"
				);
				Msg(
					"* Legacy CTerrain bridge smoke: success "
					"(meshes=1, selected=1, GPU draws=1)"
				);
				Msg(
					"* Legacy Fog Volume bridge smoke: success "
					"(emitter=1, occlusion=1)"
				);
				Msg(
					"* Legacy detail bridge smoke: success "
					"(models=1, instances=1, GPU draws=1)"
				);
				Msg(
					"* Viewport decal smoke: success (instances=%u, draws=%u, culled=%u, pass-ready=%u)",
					Status.DecalInstanceCount,
					Status.DecalDrawCount,
					Status.DecalCulledCount,
					Status.DecalReady ? 1u : 0u
				);
				Msg("* Viewport material smoke: success (%ux%u, draws=%u, selection=%u, debug-lines=%u, debug-triangles=%u, overlay-lines=%u, overlay-triangles=%u, overlay-text=%u, lights=%u, models=%u/%u/%u/%u/%u/%u/%u/%u/%u, particle-instances=%u, particle-groups=%u, particle-children=%u, simulated-particles=%u, particle-billboards=%u, particle-billboard-draws=%u, particle-preview=%ux%u/%u, legacy-gizmos=%u/%u/%u, legacy-glow=%u/%u, legacy-labels=%u, legacy-idle=%u/%u/%u, pipeline=%llu, shared-pipeline-refs=%u, sprite-pipeline=%llu, particle-pipeline=%llu, revision=%llu, reloads=%u/%u/%u/%u, stats-revision=%llu, passes=%u, gpu-draws=%u, triangles=%llu, buffers=%u/%llu, textures=%u/%llu, pipelines=%u, descriptors=%u, deferred=%u, cpu-ns=%llu, gpu-timing=%s)",
					Surface.Width,
					Surface.Height,
					Status.DrawCount,
					Status.SelectionDrawCount,
					Status.DebugLineCount,
					Status.DebugTriangleCount,
					Status.OverlayLineCount,
					Status.OverlayTriangleCount,
					Status.OverlayTextCount,
					Status.LightCount,
					Status.ModelInstanceCount,
					Status.ModelDrawCount,
					Status.PendingModelLoadCount,
					Status.ModelPickingReady ? 1u : 0u,
					Status.SkinnedModelCount,
					Status.GpuSkinnedModelCount,
					Status.ModelPaletteMatrixCount,
					Status.UploadedSkinningMatrixCount,
					Status.ModelPaletteChanged ? 1u : 0u,
					Status.ParticleInstanceCount,
					Status.ParticleGroupInstanceCount,
					Status.ParticleChildInstanceCount,
					Status.SimulatedParticleCount,
					Status.ParticleBillboardCount,
					Status.ParticleBillboardDrawCount,
					ParticlePreviewSurface.Width,
					ParticlePreviewSurface.Height,
					ParticlePreviewStatus.ParticleBillboardCount,
					LegacyLightStatus.LightCount,
					LegacyLightStatus.DebugLineCount,
					LegacyLightStatus.DebugTriangleCount,
					LegacyLightStatus.DrawCount,
					LegacyLightStatus.SelectionDrawCount,
					LegacyLightStatus.OverlayTextCount,
					LegacyLightStatus.ParticleInstanceCount,
					LegacyLightStatus.SimulatedParticleCount,
					LegacyLightStatus.ParticleBillboardDrawCount,
					static_cast<unsigned long long>(Status.PipelineKey),
					Status.SharedPipelineReferenceCount,
					static_cast<unsigned long long>(SpriteStatus.PipelineKey),
					static_cast<unsigned long long>(ParticleStatus.PipelineKey),
					static_cast<unsigned long long>(Status.AcceptedRevision),
					Status.ReloadCount,
					SpriteStatus.ReloadCount,
					ParticleStatus.ReloadCount,
					CloneStatus.ReloadCount,
					static_cast<unsigned long long>(
						RendererStatistics.Revision
					),
					RendererStatistics.Frame.PassCount,
					RendererStatistics.Frame.DrawCallCount,
					static_cast<unsigned long long>(
						RendererStatistics.Frame.TriangleCount
					),
					RendererStatistics.Resources.TrackedBufferCount,
					static_cast<unsigned long long>(
						RendererStatistics.Resources.TrackedBufferBytes
					),
					RendererStatistics.Resources.TrackedTextureCount,
					static_cast<unsigned long long>(
						RendererStatistics.Resources.TrackedTextureBytes
					),
					RendererStatistics.Resources.TrackedPipelineCount,
					RendererStatistics.Resources.TrackedDescriptorCount,
					RendererStatistics.Resources.DeferredResourceCount,
					static_cast<unsigned long long>(
						RendererStatistics.Frame.CpuFrameNanoseconds
					),
					RendererStatistics.Frame.GpuTimingValid ? "valid" : "not-collected");
				ViewportMaterialSmokeComplete = true;
				if (MaterialPreviewSmokeComplete && !RenderDocSmokeRequested)
				{
					GContentView->Destroy();
					NeedExit = true;
				}
			}
		}

		if (RenderDocSmokeRequested && MaterialPreviewSmokeComplete &&
			ViewportMaterialSmokeComplete)
		{
			if (!RenderDocSmokeCaptureArmed)
			{
				RenderDocSmokeCaptureArmed = true;
				LUI->SetRenderDocCaptureGateOpen(true);
				Msg(
					"* RenderDoc smoke: ready material frame capture armed"
				);
			}
			else if (LUI->IsRenderDocCaptureFinished())
			{
				if (!LUI->WasRenderDocCaptureSuccessful())
				{
					Msg("! RenderDoc smoke: ready frame capture failed");
					ProcessExitCode = 12;
				}
				GContentView->Destroy();
				NeedExit = true;
			}
		}

		if (g_pGamePersistent)
		{
			g_pGamePersistent->UpdateParticles();
		}
	}
	if (SmokePreviewHandle.IsValid())
	{
		SmokePreviewRenderer->DestroyPreview(SmokePreviewHandle);
	}
	if (SplashThread.joinable())
	{
		SplashThread.join();
	}
	xr_delete(g_FontManager);

	g_scene_physics.DestroyAll();
	g_scene_physics.DestroyObjectSpace();

	xr_delete(MainForm);
	if (EditorNriBackend)
	{
		(void)InstallMaterialPreviewRenderer(PreviousMaterialPreviewRenderer);
		(void)InstallEditorRenderBackend(PreviousEditorRenderBackend);
	}
	EditorNriBackend = nullptr;
	DestroyTiramisuEditorRenderer(EditorRendererInstance);
	// очищение памяти таблицы строк
	CStringTable::Destroy();
	xr_delete(pApp);
	xr_delete(g_XrGameManager);
	xr_delete(g_SEFactoryManager);
	Core._destroy();
	return ProcessExitCode;
}
