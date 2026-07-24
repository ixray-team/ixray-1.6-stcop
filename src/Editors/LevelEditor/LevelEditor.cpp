// LevelEditor.cpp : Определяет точку входа для приложения.
//
#include "stdafx.h"

#include "Engine/XrGameManager.h"
#include "Engine/XRayEditor.h"

#include "Editor/Utils/ContentView.h"
#include "Editor/Scene/LEPhysics.h"
#include "Editor/AssetImport/TLegacyLevelImporter.h"
#include "Editor/AssetImport/TLegacyObjectImporter.h"
#include "../../Include/xrRender/TiramisuEditorRendererFactory.h"
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
		return {};
	xr_string Result(static_cast<const char*>(Reader->pointer()),
		static_cast<size_t>(Reader->length()));
	FS.r_close(Reader);
	return Result;
}

xr_string ReadEditorDiskFile(const std::filesystem::path& Path)
{
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
		return {};
	const std::string Text{std::istreambuf_iterator<char>(Input),
		std::istreambuf_iterator<char>()};
	return xr_string(Text);
}

void LogConversionDiagnostics(
	const xr_vector<Tiramisu::Scene::FSceneConversionDiagnostic>&
		Diagnostics)
{
	for (const Tiramisu::Scene::FSceneConversionDiagnostic& Diagnostic :
		Diagnostics)
	{
		const char* Prefix = Diagnostic.Severity == "error" ? "!" :
			Diagnostic.Severity == "warning" ? "~" : "*";
		Msg("%s Legacy conversion smoke [%s]: %s", Prefix,
			Diagnostic.Code.c_str(), Diagnostic.Message.c_str());
	}
}

struct FLegacyConversionSmokeOptions
{
	std::filesystem::path LevelSource = "rawdata/levels/1.level";
	std::filesystem::path OutputRoot;
	bool KeepOutput = false;
};

bool RunLegacyConversionSmoke(
	const FLegacyConversionSmokeOptions& Options = {})
{
	using namespace Tiramisu::Scene;
	const std::filesystem::path TemporaryRoot =
		Options.OutputRoot.empty()
			? std::filesystem::temp_directory_path() /
				("ixray-legacy-conversion-smoke-" + std::to_string(
					std::chrono::steady_clock::now()
						.time_since_epoch().count()))
			: Options.OutputRoot;
	struct FCleanup
	{
		std::filesystem::path Path;
		bool Enabled = true;
		~FCleanup()
		{
			if (!Enabled)
				return;
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
		std::filesystem::copy("gamedata/render_materials", MaterialRoot,
			std::filesystem::copy_options::recursive, Error);
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
			MissingLevelSource, RenderSceneRoot,
			"level_import.source_open_failed",
			"Smoke-test source cannot be opened.");
	const FSceneConversionDumpParseResult FailedLevelDump =
		ParseSceneConversionDumpJson(
			ReadEditorDiskFile(FailedLevel.DumpPath));
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
			ObjectSource, MaterialRoot, StaticMeshRoot);
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
			FirstObject.DumpPath));
	if (!Mesh.Succeeded() || !FirstObjectDump.Succeeded() ||
		FirstObjectDump.Value.Status !=
			ESceneConversionStatus::Succeeded ||
		FirstObjectDump.Value.TargetPayloadPath !=
			xr_string(FirstObject.TargetPayloadPath.generic_string()) ||
		!std::filesystem::is_regular_file(
			FirstObject.TargetPayloadPath))
	{
		Msg("! Legacy object conversion smoke produced an invalid asset or dump");
		return false;
	}

	const FLegacyObjectImportResult SecondObject =
		ImportLegacyObjectAsset(
			ObjectSource, MaterialRoot, StaticMeshRoot);
	const FSceneConversionDumpParseResult SecondObjectDump =
		ParseSceneConversionDumpJson(ReadEditorDiskFile(
			SecondObject.DumpPath));
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
		Options.LevelSource.string().c_str(), false);
	Scene->setSkipCantFindDialog(false);
	if (!Loaded)
	{
		Msg("! Legacy conversion smoke could not load '%s'",
			Options.LevelSource.generic_string().c_str());
		return false;
	}
	const FLegacyLevelImportResult Level =
		ImportLoadedLegacyLevelAsset(Options.LevelSource, *Scene, MaterialRoot,
			StaticMeshRoot, RenderSceneRoot);
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
		NativeScene.Value.Scene.StaticMeshComponents.empty())
	{
		Msg("! Legacy level conversion smoke produced an invalid native scene "
			"or audit dump");
		return false;
	}
	Msg("* Legacy conversion smoke: success (meshes=%u, components=%u, "
		"materials created=%u, reused=%u)",
		LevelDump.Value.MeshCount, LevelDump.Value.ComponentCount,
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

	if (!SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_EVENTS))
	{
		Msg("! SDL_Init Error: %s", SDL_GetError());
		return 0;
	}

	splash::SetBackground(IDB_LE);
	std::jthread s(splash::Show);

	splash::SetProgressStatus(5, "Initializing Debugger");

	Debug._initialize(false);
	
	splash::SetProgressStatus(10, "Initializing Core System");

	const char* FSName = "fs.ltx";
	const char* fsgame_ltx_name = "-fsltx ";
	string_path fsgame = "";

	if (strstr(pCmdLine, fsgame_ltx_name)) {
		int						sz = xr_strlen(fsgame_ltx_name);
		sscanf(strstr(pCmdLine, fsgame_ltx_name) + sz, "%[^ ] ", fsgame);
	}

	CFilewatcher::instance().SetFilewatcherActive(true);
	Core._initialize("LevelEditor", ELogCallback, 1, fsgame[0] ? fsgame : FSName);
	const FEditorNriStartupConfig EditorNriConfig =
		ParseEditorNriStartupConfig(Core.Params ? Core.Params : "");
	if (!EditorNriConfig.IsValid())
	{
		Msg("! LevelEditor: -render-deterministic requires both "
			"-tiramisu-editor and the exact -rdbg flag");
		Core._destroy();
		return 11;
	}
	if (EditorNriConfig.DeterministicTest.Enabled)
	{
		Random.seed(static_cast<s32>(
			EditorNriConfig.DeterministicTest.RandomSeed));
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
	UI->GeneralTabs.push_back({ "Scene View", nullptr });

	LUI = static_cast<CLevelMain*>(UI);
	FTiramisuEditorRendererInstance EditorRendererInstance;
	IEditorRenderBackend* EditorNriBackend = nullptr;
	IEditorRenderBackend* PreviousEditorRenderBackend = nullptr;
	IMaterialPreviewRenderer* PreviousMaterialPreviewRenderer = nullptr;
	if (EditorNriConfig.Enabled)
	{
		R_ASSERT2(CreateTiramisuEditorRenderer(nullptr, EditorNriConfig.Api,
			EditorNriConfig.DeterministicTest, EditorRendererInstance),
			"xrRenderTiramisu failed to create the editor renderer");
		R_ASSERT2(EditorRendererInstance.IsValid(),
			"xrRenderTiramisu returned incomplete editor renderer interfaces");
		EditorNriBackend = EditorRendererInstance.EditorBackend;
		R_ASSERT2(UI->InstallRenderBackend(EditorRendererInstance.UiBackend),
			"The Tiramisu editor renderer must be installed before device initialization");
		PreviousEditorRenderBackend = InstallEditorRenderBackend(
			EditorRendererInstance.EditorBackend);
		PreviousMaterialPreviewRenderer = InstallMaterialPreviewRenderer(
			EditorRendererInstance.MaterialPreviewRenderer);
		Msg("* LevelEditor: xrRenderTiramisu editor presenter selected (%s)",
			EditorNriConfig.Api == ETiramisuEditorGraphicsApi::D3D12
				? "D3D12" : "Vulkan");
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
			EditorNriConfig.DeterministicTest.FixedWeatherTimeSeconds, 0.0f);
	}
	EDevice->seqAppStart.Process<&pureAppStart::OnAppStart>();

	splash::SetProgressStatus(65, "Setting Up Console");

	Console->Execute("default_controls");

	xr_strcpy(Console->ConfigFile, "user_editor.ltx");

	if (strstr(Core.Params, "-ltx ")) {
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
	splash::Close();

	const bool MaterialPreviewSmokeRequested =
		strstr(Core.Params, "-material-preview-smoke") != nullptr;
	const bool ViewportMaterialReloadSmokeRequested =
		strstr(Core.Params, "-viewport-material-reload-smoke") != nullptr;
	const bool ViewportMaterialSmokeRequested =
		strstr(Core.Params, "-viewport-material-smoke") != nullptr ||
		ViewportMaterialReloadSmokeRequested;
	const bool LegacyConversionSmokeRequested =
		strstr(Core.Params, "-legacy-conversion-smoke") != nullptr;
	const bool ZatonConversionSmokeRequested =
		strstr(Core.Params, "-legacy-zaton-conversion-smoke") != nullptr;
	bool MaterialPreviewSmokeComplete = !MaterialPreviewSmokeRequested;
	bool ViewportMaterialSmokeComplete = !ViewportMaterialSmokeRequested;
	int ProcessExitCode = 0;
	IMaterialPreviewRenderer* SmokePreviewRenderer = nullptr;
	FMaterialPreviewHandle SmokePreviewHandle;
	const auto SmokeDeadline = std::chrono::steady_clock::now() +
		std::chrono::seconds(60);
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
			const xr_string MaterialJson = ReadEditorVirtualFile("$game_render_materials$",
				"standard_surface.material.json");
			const xr_string InstanceJson = ReadEditorVirtualFile("$game_render_materials$",
				"example_red.material-instance.json");
			const xr_string MaterialHlsl = ReadEditorVirtualFile("$game_shaders$",
				"r5\\materials\\StandardSurface.hlsl");
			SmokePreviewHandle = SmokePreviewRenderer->CreatePreview();
			if (MaterialJson.empty() || InstanceJson.empty() || MaterialHlsl.empty() ||
				!SmokePreviewHandle.IsValid())
			{
				Msg("! Material preview smoke setup failed: material=%zu, instance=%zu, hlsl=%zu, handle=%s",
					MaterialJson.size(), InstanceJson.size(), MaterialHlsl.size(),
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
	constexpr FEditorMaterialSlotId SmokeSceneMaterialSlot = {
		0x736d6f6b656d6174ull};
	constexpr FEditorMaterialSlotId SmokeSceneSpriteMaterialSlot = {
		0x7370726974656d61ull};
	constexpr FEditorMaterialSlotId SmokeSceneParticleMaterialSlot = {
		0x7061727469636c65ull};
	constexpr u64 SmokeSceneCloneMaterialSlotBase =
		0x636c6f6e65000100ull;
	constexpr u32 SmokeSceneCloneMaterialCount = 16;
	constexpr FEditorMaterialSlotId SmokeSceneLastCloneMaterialSlot = {
		SmokeSceneCloneMaterialSlotBase +
			SmokeSceneCloneMaterialCount - 1};
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
				0, 1, 2, 3, 4, 5, 6, 7, 8};
			const xr_array<FEditorStaticMeshSection, 3> Sections = {{
				{0, 3, SmokeSceneMaterialSlot},
				{3, 3, SmokeSceneSpriteMaterialSlot},
				{6, 3, SmokeSceneParticleMaterialSlot}}};
			const FEditorStaticMeshId MeshId = {0x736d6f6b656d6573ull};
			const xr_array<FEditorStaticMeshUpload, 1> Meshes = {{
				MeshId, 1, Vertices, Indices, Sections}};
			xr_array<FEditorStaticMeshInstance, 1> Instances;
			Instances[0].ObjectId = {0x736d6f6b656f626aull};
			Instances[0].MeshId = MeshId;
			// Exercise the X-Ray row-vector -> material draw-buffer matrix ABI.
			// An identity transform would not catch translation leaking into
			// clip-space W in the vertex shader.
			Instances[0].LocalToWorld[12] = 0.6f;
			Instances[0].Flags = EEditorSceneInstanceFlags::Selected;
			xr_vector<FEditorMaterialSlotSource> Materials = {
				{SmokeSceneMaterialSlot, "default", "textures/kung",
					"Viewport material smoke", EEditorMaterialSlotFlags::None},
				{SmokeSceneSpriteMaterialSlot, "editor\\spawn_icon",
					"textures/default/default_white", "Editor sprite smoke",
					EEditorMaterialSlotFlags::TwoSided},
				{SmokeSceneParticleMaterialSlot, "editor\\particle_additive",
					"textures/default/default_white", "Editor particle smoke",
					EEditorMaterialSlotFlags::TwoSided}};
			Materials.reserve(3 + SmokeSceneCloneMaterialCount);
			for (u32 Index = 0;
				Index < SmokeSceneCloneMaterialCount; ++Index)
			{
				Materials.push_back({
					{SmokeSceneCloneMaterialSlotBase + Index},
					"default",
					(Index & 1u) != 0 ? "briks\\briks_br2" :
						"textures\\kung",
					"Shared standard permutation smoke",
					EEditorMaterialSlotFlags::None});
			}
			xr_array<FEditorDebugLine, 1> DebugLines;
			DebugLines[0].Vertices[0] = {
				{-0.9f, 0.0f, -0.01f}, {1.0f, 0.0f, 0.0f, 1.0f}};
			DebugLines[0].Vertices[1] = {
				{0.9f, 0.0f, -0.01f}, {0.0f, 1.0f, 0.0f, 1.0f}};
			xr_array<FEditorDebugTriangle, 1> DebugTriangles;
			DebugTriangles[0].Vertices[0] = {
				{-0.2f, -0.2f, -0.02f}, {0.0f, 0.0f, 1.0f, 0.5f}};
			DebugTriangles[0].Vertices[1] = {
				{0.0f, 0.2f, -0.02f}, {0.0f, 0.0f, 1.0f, 0.5f}};
			DebugTriangles[0].Vertices[2] = {
				{0.2f, -0.2f, -0.02f}, {0.0f, 0.0f, 1.0f, 0.5f}};
			xr_array<FEditorOverlayLine, 1> OverlayLines;
			OverlayLines[0].Vertices[0] = {
				{-0.75f, 0.75f, 0.0f}, {1.0f, 0.0f, 0.0f, 1.0f}};
			OverlayLines[0].Vertices[1] = {
				{-0.25f, 0.75f, 0.0f}, {0.0f, 1.0f, 0.0f, 1.0f}};
			xr_array<FEditorOverlayTriangle, 1> OverlayTriangles;
			OverlayTriangles[0].Vertices[0] = {
				{0.25f, 0.75f, 0.0f}, {1.0f, 1.0f, 0.0f, 0.4f}};
			OverlayTriangles[0].Vertices[1] = {
				{0.75f, 0.75f, 0.0f}, {1.0f, 1.0f, 0.0f, 0.4f}};
			OverlayTriangles[0].Vertices[2] = {
				{0.5f, 0.25f, 0.0f}, {1.0f, 1.0f, 0.0f, 0.4f}};
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
			FEditorViewportSceneSnapshot Snapshot;
			Snapshot.Camera.View = {
				1.0f, 0.0f, 0.0f, 0.0f,
				0.0f, 1.0f, 0.0f, 0.0f,
				0.0f, 0.0f, 1.0f, 0.0f,
				0.0f, 0.0f, 0.0f, 1.0f};
			Snapshot.Camera.Projection = Snapshot.Camera.View;
			Snapshot.Camera.ViewProjection = Snapshot.Camera.View;
			Snapshot.Camera.WorldPosition = {0.0f, 0.0f, -3.0f};
			Snapshot.Camera.NearPlane = 0.05f;
			Snapshot.Camera.FarPlane = 100.0f;
			Snapshot.MaterialSlots = Materials;
			Snapshot.StaticMeshes = Meshes;
			Snapshot.Instances = Instances;
			Snapshot.Lights = Lights;
			Snapshot.DebugLines = DebugLines;
			Snapshot.DebugTriangles = DebugTriangles;
			Snapshot.OverlayLines = OverlayLines;
			Snapshot.OverlayTriangles = OverlayTriangles;
			Snapshot.OverlayText = OverlayText;
			Snapshot.DebugDrawRevision = 1;
			Snapshot.Revision = 1;
			EditorNriBackend->ResizeViewport(SmokeSceneViewportId, 512, 512);
			if (!EditorNriBackend->SubmitViewportScene(
					SmokeSceneViewportId, Snapshot))
			{
				Msg("! Viewport material smoke setup failed: %.*s",
					static_cast<int>(EditorNriBackend->GetLastDiagnostic().size()),
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
						SmokeSceneViewportId, PickRequest);
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
					"rawdata/levels/!FinalSP/zaton.level");
				SmokeOptions.OutputRoot = std::filesystem::absolute(
					"build/test-results/tiramisu") /
					("zaton-" + std::to_string(
						std::chrono::system_clock::now()
							.time_since_epoch().count()));
				SmokeOptions.KeepOutput = true;
				Msg("* Legacy conversion smoke: full zaton level selected");
			}
			if (!RunLegacyConversionSmoke(SmokeOptions))
				ProcessExitCode = 10;
		}
		GContentView->Destroy();
		NeedExit = true;
	}

	while (!NeedExit)
	{
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
					UI->Resize(Event.window.data1, Event.window.data2, true);
					EPrefs->SaveConfig();
				}
				break;
			}
			case SDL_EVENT_WINDOW_SHOWN:
			case SDL_EVENT_WINDOW_MOUSE_ENTER:
				Device.b_is_Active = true;
				//if (UI) UI->OnAppActivate();

				break;
			case SDL_EVENT_WINDOW_HIDDEN:
			case SDL_EVENT_WINDOW_MOUSE_LEAVE:
				Device.b_is_Active = !!psDeviceFlags.test(rsDeviceActive);
				//if (UI)UI->OnAppDeactivate();
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
				}break;
			case SDL_EVENT_KEY_UP:
				if (UI) {
					UI->KeyUp(Event.key.scancode, UI->GetShiftState());
					if(UI->IsPlayInEditor() && pInput->IsAcquire) 
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
					break;

				pInput->MouseMotion(Event.motion.xrel, Event.motion.yrel);
			} break;
			case SDL_EVENT_MOUSE_WHEEL:
			{
				if (UI->IsPlayInEditor() && !pInput->IsAcquire)
					break;

				pInput->MouseScroll(Event.wheel.y);
			}break;
			case SDL_EVENT_MOUSE_BUTTON_DOWN:
			case SDL_EVENT_MOUSE_BUTTON_UP:
			{
				if (UI->IsPlayInEditor() && !pInput->IsAcquire)
					break;

				int mouse_button = 0;
				if (Event.button.button == SDL_BUTTON_LEFT) { mouse_button = 0; }
				if (Event.button.button == SDL_BUTTON_RIGHT) { mouse_button = 1; }
				if (Event.button.button == SDL_BUTTON_MIDDLE) { mouse_button = 2; }
				if (Event.type == SDL_EVENT_MOUSE_BUTTON_DOWN) {
					pInput->MousePressed(mouse_button);
				}
				else {
					pInput->MouseReleased(mouse_button);
				}
			}
			break;
			}

			if (!UI->ProcessEvent(&Event))
				break;
		}

		if (SmokePreviewHandle.IsValid())
			SmokePreviewRenderer->RenderPreview(SmokePreviewHandle, 1.0f / 60.0f);
		if (ViewportMaterialSmokeRequested && EditorNriBackend)
			EditorNriBackend->CaptureViewport(SmokeSceneViewportId);

		MainForm->Frame();

		if (SmokePreviewHandle.IsValid())
		{
			const FMaterialPreviewFrame Frame =
				SmokePreviewRenderer->GetPreviewFrame(SmokePreviewHandle);
			const bool TimedOut = std::chrono::steady_clock::now() >= SmokeDeadline;
			if (Frame.State == EMaterialPreviewState::Error || TimedOut)
			{
				if (Frame.Diagnostic.empty())
					Msg("! Material preview smoke timed out");
				else
					Msg("! Material preview smoke failed: %.*s",
						static_cast<int>(Frame.Diagnostic.size()), Frame.Diagnostic.data());
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
					Frame.Surface.Width, Frame.Surface.Height);
				SmokePreviewRenderer->DestroyPreview(SmokePreviewHandle);
				SmokePreviewHandle = {};
				MaterialPreviewSmokeComplete = true;
				if (ViewportMaterialSmokeComplete)
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
					SmokeSceneViewportId, SmokeSceneMaterialSlot);
			const FEditorViewportMaterialStatus SpriteStatus =
				EditorNriBackend->GetViewportMaterialStatus(
					SmokeSceneViewportId, SmokeSceneSpriteMaterialSlot);
			const FEditorViewportMaterialStatus ParticleStatus =
				EditorNriBackend->GetViewportMaterialStatus(
					SmokeSceneViewportId, SmokeSceneParticleMaterialSlot);
			const FEditorViewportMaterialStatus CloneStatus =
				EditorNriBackend->GetViewportMaterialStatus(
					SmokeSceneViewportId,
					SmokeSceneLastCloneMaterialSlot);
			const FEditorViewportSurface Surface =
				EditorNriBackend->GetViewportSurface(SmokeSceneViewportId);
			const FRenderStatisticsSnapshot RendererStatistics =
				EditorNriBackend->GetRenderStatistics();
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
			const bool CloneMaterialFailed = CloneStatus.RequestedRevision != 0 &&
				CloneStatus.AcceptedRevision < CloneStatus.RequestedRevision &&
				!CloneStatus.Diagnostic.empty();
			if (SurfaceMaterialFailed || SpriteMaterialFailed ||
				ParticleMaterialFailed || CloneMaterialFailed || TimedOut)
			{
				const xr_string& MaterialDiagnostic = SurfaceMaterialFailed
					? Status.Diagnostic : SpriteMaterialFailed
						? SpriteStatus.Diagnostic : ParticleMaterialFailed
							? ParticleStatus.Diagnostic :
								CloneStatus.Diagnostic;
				if (MaterialDiagnostic.empty())
					Msg("! Viewport material smoke timed out");
				else
					Msg("! Viewport material smoke failed: %s",
						MaterialDiagnostic.c_str());
				ProcessExitCode = 7;
				GContentView->Destroy();
				NeedExit = true;
			}
			else if (Status.Ready && SpriteStatus.Ready && ParticleStatus.Ready &&
				CloneStatus.Ready &&
				CloneStatus.PipelineKey == Status.PipelineKey &&
				Status.SharedPipelineReferenceCount >=
					SmokeSceneCloneMaterialCount + 1 &&
				Surface.IsValid() && Status.DrawCount == 3 &&
				Status.SelectionOverlayReady &&
				Status.SelectionDrawCount == 3 &&
				Status.DebugOverlayReady &&
				Status.DebugLineCount == 1 &&
				Status.DebugTriangleCount == 1 &&
				Status.ScreenOverlayReady &&
				Status.OverlayLineCount == 1 &&
				Status.OverlayTriangleCount == 1 &&
				Status.OverlayTextCount == 1 &&
				Status.LightCount == 2 &&
				RendererStatisticsReady &&
				(!ViewportMaterialReloadSmokeRequested ||
					(Status.ReloadCount >= 1 && SpriteStatus.ReloadCount >= 1 &&
						ParticleStatus.ReloadCount >= 1 &&
						CloneStatus.ReloadCount >= 1)))
			{
				Msg("* Viewport material smoke: success (%ux%u, draws=%u, selection=%u, debug-lines=%u, debug-triangles=%u, overlay-lines=%u, overlay-triangles=%u, overlay-text=%u, lights=%u, pipeline=%llu, shared-pipeline-refs=%u, sprite-pipeline=%llu, particle-pipeline=%llu, revision=%llu, reloads=%u/%u/%u/%u, stats-revision=%llu, passes=%u, gpu-draws=%u, triangles=%llu, buffers=%u/%llu, textures=%u/%llu, pipelines=%u, descriptors=%u, deferred=%u, cpu-ns=%llu, gpu-timing=%s)",
					Surface.Width, Surface.Height, Status.DrawCount,
					Status.SelectionDrawCount,
					Status.DebugLineCount, Status.DebugTriangleCount,
					Status.OverlayLineCount, Status.OverlayTriangleCount,
					Status.OverlayTextCount, Status.LightCount,
					static_cast<unsigned long long>(Status.PipelineKey),
					Status.SharedPipelineReferenceCount,
					static_cast<unsigned long long>(SpriteStatus.PipelineKey),
					static_cast<unsigned long long>(ParticleStatus.PipelineKey),
					static_cast<unsigned long long>(Status.AcceptedRevision),
					Status.ReloadCount, SpriteStatus.ReloadCount,
					ParticleStatus.ReloadCount, CloneStatus.ReloadCount,
					static_cast<unsigned long long>(
						RendererStatistics.Revision),
					RendererStatistics.Frame.PassCount,
					RendererStatistics.Frame.DrawCallCount,
					static_cast<unsigned long long>(
						RendererStatistics.Frame.TriangleCount),
					RendererStatistics.Resources.TrackedBufferCount,
					static_cast<unsigned long long>(
						RendererStatistics.Resources.TrackedBufferBytes),
					RendererStatistics.Resources.TrackedTextureCount,
					static_cast<unsigned long long>(
						RendererStatistics.Resources.TrackedTextureBytes),
					RendererStatistics.Resources.TrackedPipelineCount,
					RendererStatistics.Resources.TrackedDescriptorCount,
					RendererStatistics.Resources.DeferredResourceCount,
					static_cast<unsigned long long>(
						RendererStatistics.Frame.CpuFrameNanoseconds),
					RendererStatistics.Frame.GpuTimingValid ? "valid" :
						"not-collected");
				ViewportMaterialSmokeComplete = true;
				if (MaterialPreviewSmokeComplete)
				{
					GContentView->Destroy();
					NeedExit = true;
				}
			}
		}

		if (g_pGamePersistent)
			g_pGamePersistent->UpdateParticles();
	}
	if (SmokePreviewHandle.IsValid())
		SmokePreviewRenderer->DestroyPreview(SmokePreviewHandle);
	s.join();
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
	//очищение памяти таблицы строк
	CStringTable::Destroy();
	xr_delete(pApp);
	xr_delete(g_XrGameManager);
	xr_delete(g_SEFactoryManager);
	Core._destroy();
	return ProcessExitCode;
}
