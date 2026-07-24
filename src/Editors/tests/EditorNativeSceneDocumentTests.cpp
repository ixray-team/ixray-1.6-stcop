#include "../LevelEditor/Renderer/Tiramisu/TiramisuEditorNativeScene.h"

#include <SceneAsset.h>

#include <chrono>
#include <filesystem>
#include <fstream>
#include <iostream>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}

void WriteText(const std::filesystem::path& Path, const xr_string& Text)
{
	std::ofstream Output(Path, std::ios::binary);
	Output << Text;
}

FEditorNativeSceneSelectionFrustum MakeBoxFrustum(
	const xr_array<float, 3>& Minimum,
	const xr_array<float, 3>& Maximum)
{
	FEditorNativeSceneSelectionFrustum Frustum;
	Frustum.Planes = {
		{{1.0f, 0.0f, 0.0f}, -Maximum[0]},
		{{-1.0f, 0.0f, 0.0f}, Minimum[0]},
		{{0.0f, 1.0f, 0.0f}, -Maximum[1]},
		{{0.0f, -1.0f, 0.0f}, Minimum[1]},
		{{0.0f, 0.0f, 1.0f}, -Maximum[2]},
		{{0.0f, 0.0f, -1.0f}, Minimum[2]}};
	return Frustum;
}
} // namespace

int main()
{
	using namespace Tiramisu::Scene;
	const std::filesystem::path Root =
		std::filesystem::temp_directory_path() /
		("ixray-native-scene-document-" + std::to_string(
			std::chrono::steady_clock::now().time_since_epoch().count()));
	struct FCleanup
	{
		std::filesystem::path Path;
		~FCleanup()
		{
			std::error_code Error;
			std::filesystem::remove_all(Path, Error);
		}
	} Cleanup{Root};
	std::error_code Error;
	std::filesystem::create_directories(Root, Error);
	if (Error)
		return Fail("Cannot create test directory");

	FStaticMeshAsset Mesh;
	Mesh.Id = "a520c30d-b7a7-4a65-9915-558343d43d9a";
	Mesh.Name = "Triangle";
	Mesh.MaterialSlots.push_back(
		{"Surface", "128e21af-5c6f-4ec4-a2e3-8b44f90cb553", false});
	Mesh.Vertices = {
		{{{-1.0f, -1.0f, 0.0f}}},
		{{{0.0f, 1.0f, 0.0f}}},
		{{{1.0f, -1.0f, 0.0f}}}};
	Mesh.Indices = {0, 1, 2};
	Mesh.Sections.push_back({0, 3, 0});
	const std::filesystem::path MeshPath =
		Root / "triangle.static-mesh.json";
	if (!SaveStaticMeshAsset(MeshPath, Mesh).Succeeded())
		return Fail("Cannot save binary native static mesh");

	FRenderSceneAsset Scene;
	Scene.Id = "eb39395e-57b0-4bd7-a79b-f81062cf36ec";
	Scene.Name = "Editable native scene";
	FStaticMeshComponent Component;
	Component.Id = "294d16b8-ac73-4555-ae4c-4b56cde96256";
	Component.Name = "Triangle component";
	Component.StaticMesh = MeshPath.filename().generic_string();
	Scene.StaticMeshComponents.push_back(Component);
	const std::filesystem::path ScenePath =
		Root / "editable.render-scene.json";
	WriteText(ScenePath, SerializeRenderSceneAssetJson(Scene));

	TiramisuEditorNativeSceneDocument Document;
	xr_string Diagnostic;
	if (!Document.OpenRenderScene(ScenePath, Diagnostic) ||
		!Document.IsEditableRenderScene() || Document.IsDirty())
	{
		return Fail("Native render scene did not open as a clean document");
	}

	const u64 ObjectId = StableSceneIdHash(Component.Id);
	if (!Document.SelectObject(ObjectId,
			EEditorNativeSceneSelectionMode::Replace) ||
		Document.GetSelectionCount() != 1 ||
		!Document.IsComponentSelected(Component.Id))
	{
		return Fail("Native component selection failed");
	}

	if (!Document.BeginEditTransaction() ||
		!Document.TranslateSelected({1.0f, 2.0f, 3.0f}) ||
		!Document.TranslateSelected({0.5f, 0.0f, 0.0f}) ||
		!Document.EndEditTransaction() || !Document.IsDirty())
	{
		return Fail("Native transform transaction failed");
	}
	const auto* Edited = Document.GetScene();
	if (!Edited ||
		Edited->Scene.StaticMeshComponents[0].LocalToWorld[12] != 1.5f ||
		Edited->Scene.StaticMeshComponents[0].LocalToWorld[13] != 2.0f ||
		Edited->Scene.StaticMeshComponents[0].LocalToWorld[14] != 3.0f)
	{
		return Fail("Native translation was not applied");
	}

	if (!Document.Undo() || Document.IsDirty() ||
		Document.GetScene()->Scene.StaticMeshComponents[0]
			.LocalToWorld[12] != 0.0f ||
		!Document.Redo() || !Document.IsDirty())
	{
		return Fail("Native undo/redo did not restore document state");
	}

	if (!Document.Save(Diagnostic) || Document.IsDirty())
		return Fail("Native scene save failed");
	const FResolvedRenderSceneResult Reloaded = LoadRenderSceneAsset(ScenePath);
	if (!Reloaded.Succeeded() ||
		Reloaded.Value.Scene.StaticMeshComponents[0]
			.LocalToWorld[12] != 1.5f)
	{
		return Fail("Saved native scene did not round-trip");
	}

	const std::filesystem::path SaveAsRoot = Root / "copy";
	std::filesystem::create_directories(SaveAsRoot, Error);
	const std::filesystem::path SaveAsPath =
		SaveAsRoot / "copy.render-scene.json";
	if (Error || !Document.SaveAs(SaveAsPath, Diagnostic) ||
		Document.GetSourcePath() != SaveAsPath.lexically_normal())
	{
		return Fail("Native scene Save As failed");
	}
	const FResolvedRenderSceneResult SaveAsReloaded =
		LoadRenderSceneAsset(SaveAsPath);
	if (!SaveAsReloaded.Succeeded() ||
		SaveAsReloaded.Value.StaticMeshes.size() != 1)
	{
		return Fail("Save As did not rebase static-mesh references");
	}

	if (!Document.BeginEditTransaction() ||
		!Document.TranslateSelected({10.0f, 0.0f, 0.0f}) ||
		!Document.EndEditTransaction(false) || Document.IsDirty())
	{
		return Fail("Cancelled transaction changed the saved document");
	}

	const auto InitialDetails =
		Document.GetSingleSelectedComponentDetails();
	if (!InitialDetails || InitialDetails->Name != Component.Name ||
		InitialDetails->StaticMesh.empty() ||
		InitialDetails->MaterialSlots.size() != 1 ||
		InitialDetails->MaterialSlots[0].BaseMaterial !=
			Mesh.MaterialSlots[0].Material ||
		InitialDetails->MaterialSlots[0].HasOverride)
	{
		return Fail("Native component details are incomplete");
	}
	const u64 RevisionBeforeInvalidOverride =
		Document.GetRevision();
	if (Document.SetSelectedMaterialOverride(5, "invalid", false,
			Diagnostic) ||
		Document.GetRevision() != RevisionBeforeInvalidOverride)
	{
		return Fail("Invalid native material override changed the scene");
	}
	if (!Document.SetSelectedComponentName("Renamed component",
			Diagnostic) ||
		!Document.SetSelectedComponentVisibility(false) ||
		!Document.SetSelectedComponentPosition({2.0f, 3.0f, 4.0f}) ||
		!Document.SetSelectedMaterialOverride(0,
			"generated/test.material-instance.json", true, Diagnostic))
	{
		return Fail("Native component details edit failed");
	}
	const auto EditedDetails =
		Document.GetSingleSelectedComponentDetails();
	if (!EditedDetails ||
		EditedDetails->Name != "Renamed component" ||
		EditedDetails->Visible ||
		EditedDetails->Position != xr_array<float, 3>{2.0f, 3.0f, 4.0f} ||
		!EditedDetails->MaterialSlots[0].HasOverride ||
		EditedDetails->MaterialSlots[0].OverrideMaterial !=
			"generated/test.material-instance.json" ||
		!EditedDetails->MaterialSlots[0].OverrideTwoSided)
	{
		return Fail("Native edited details were not published");
	}
	if (!Document.Undo() ||
		Document.GetSingleSelectedComponentDetails()
			->MaterialSlots[0].HasOverride ||
		!Document.Redo() ||
		!Document.GetSingleSelectedComponentDetails()
			->MaterialSlots[0].HasOverride ||
		!Document.ClearSelectedMaterialOverride(0, Diagnostic) ||
		Document.GetSingleSelectedComponentDetails()
			->MaterialSlots[0].HasOverride ||
		!Document.Undo() ||
		!Document.GetSingleSelectedComponentDetails()
			->MaterialSlots[0].HasOverride)
	{
		return Fail("Native material override undo/clear failed");
	}
	if (!Document.Save(Diagnostic))
		return Fail("Native component details save failed");
	const FResolvedRenderSceneResult DetailsReloaded =
		LoadRenderSceneAsset(Document.GetSourcePath());
	if (!DetailsReloaded.Succeeded() ||
		DetailsReloaded.Value.Scene.StaticMeshComponents[0].Name !=
			"Renamed component" ||
		DetailsReloaded.Value.Scene.StaticMeshComponents[0].Visible ||
		DetailsReloaded.Value.Scene.StaticMeshComponents[0]
			.MaterialOverrides.size() != 1)
	{
		return Fail("Native component details did not round-trip");
	}

	xr_array<float, 16> AddedTransform =
		FStaticMeshComponent{}.LocalToWorld;
	AddedTransform[12] = 4.0f;
	if (!Document.AddStaticMeshComponent(
			MeshPath, AddedTransform, Diagnostic) ||
		Document.GetSelectionCount() != 1 ||
		Document.GetScene()->Scene.StaticMeshComponents.size() != 2 ||
		Document.RemoveSelected() != 1 ||
		Document.GetScene()->Scene.StaticMeshComponents.size() != 1)
	{
		return Fail("Native component add/remove failed");
	}
	if (!Document.Undo() ||
		Document.GetScene()->Scene.StaticMeshComponents.size() != 2 ||
		!Document.Redo() ||
		Document.GetScene()->Scene.StaticMeshComponents.size() != 1)
	{
		return Fail("Native component add/remove undo/redo failed");
	}

	FStaticMeshAsset OtherMesh = Mesh;
	OtherMesh.Id = "7f9313b7-9347-4cbe-adc2-c4dbb0563787";
	OtherMesh.Name = "Other triangle";
	OtherMesh.MaterialSlots[0] = {
		"Coating", "0de31eb6-0fba-4f89-8f61-355d1d59b5e0", false};
	OtherMesh.MaterialSlots.push_back(
		{"Detail", "573bbb5a-8cd8-46b5-89e2-89c037340ce7", true});
	const std::filesystem::path OtherMeshPath =
		Root / "other.static-mesh.json";
	if (!SaveStaticMeshAsset(OtherMeshPath, OtherMesh).Succeeded())
		return Fail("Cannot save heterogeneous bulk-details mesh");

	FRenderSceneAsset SelectionScene;
	SelectionScene.Id = "0a2ec6b2-ee69-4b51-975f-a5590c9d78cf";
	SelectionScene.Name = "Rectangle selection scene";
	FStaticMeshComponent SelectionNear = Component;
	SelectionNear.Id = "8e90aa84-b289-4862-b6ba-19b4dcd91f71";
	SelectionNear.Name = "Near";
	SelectionNear.MaterialOverrides.push_back(
		{0, "legacy/near.material-instance.json", true});
	FStaticMeshComponent SelectionFar = Component;
	SelectionFar.Id = "8fca2359-b73d-45ce-9291-f987aed5dafc";
	SelectionFar.Name = "Far";
	SelectionFar.StaticMesh = OtherMeshPath.filename().generic_string();
	SelectionFar.LocalToWorld[12] = 6.0f;
	FStaticMeshComponent SelectionHidden = Component;
	SelectionHidden.Id = "f03a58cd-e890-42ff-8aa2-8795ae3457ac";
	SelectionHidden.Name = "Hidden";
	SelectionHidden.Visible = false;
	SelectionScene.StaticMeshComponents = {
		SelectionNear, SelectionFar, SelectionHidden};
	const std::filesystem::path SelectionScenePath =
		Root / "selection.render-scene.json";
	WriteText(SelectionScenePath,
		SerializeRenderSceneAssetJson(SelectionScene));
	if (!Document.OpenRenderScene(SelectionScenePath, Diagnostic))
		return Fail("Rectangle selection scene did not open");

	const xr_array<xr_string, 2> BulkSelection = {
		SelectionNear.Id, SelectionFar.Id};
	if (Document.SelectComponents(BulkSelection,
			EEditorNativeSceneSelectionMode::Replace) != 2 ||
		Document.GetSelectionCount() != 2)
	{
		return Fail("Native outliner bulk selection failed");
	}
	const auto InitialBulkDetails =
		Document.GetSelectedComponentsMaterialDetails();
	if (!InitialBulkDetails ||
		InitialBulkDetails->ComponentCount != 2 ||
		InitialBulkDetails->MaterialSlots.size() != 1 ||
		!InitialBulkDetails->MaterialSlots[0].NameMixed ||
		!InitialBulkDetails->MaterialSlots[0].BaseMaterialMixed ||
		InitialBulkDetails->MaterialSlots[0].OverrideCount != 1 ||
		!InitialBulkDetails->MaterialSlots[0].OverrideMaterialMixed ||
		!InitialBulkDetails->MaterialSlots[0].OverrideTwoSidedMixed)
	{
		return Fail("Native bulk material mixed values are incorrect");
	}
	const auto SelectedWorldBounds =
		Document.GetWorldBounds(true);
	const auto AllWorldBounds =
		Document.GetWorldBounds(false);
	if (!SelectedWorldBounds || !AllWorldBounds ||
		SelectedWorldBounds->Minimum !=
			xr_array<float, 3>{-1.0f, -1.0f, 0.0f} ||
		SelectedWorldBounds->Maximum !=
			xr_array<float, 3>{7.0f, 1.0f, 0.0f} ||
		AllWorldBounds->Minimum != SelectedWorldBounds->Minimum ||
		AllWorldBounds->Maximum != SelectedWorldBounds->Maximum)
	{
		return Fail("Native selected/all world bounds are incorrect");
	}
	const u64 RevisionBeforeInvalidBulk =
		Document.GetRevision();
	if (Document.SetSelectedComponentsMaterialOverride(1,
			"invalid", std::nullopt, Diagnostic) ||
		Document.ClearSelectedMaterialOverride(1, Diagnostic) ||
		Document.GetRevision() != RevisionBeforeInvalidBulk)
	{
		return Fail("Invalid bulk material slot changed the scene");
	}
	if (!Document.SetSelectedComponentsMaterialOverride(0,
			"generated/bulk.material-instance.json", std::nullopt,
			Diagnostic))
	{
		return Fail("Native bulk material override failed");
	}
	const auto& BulkComponents =
		Document.GetScene()->Scene.StaticMeshComponents;
	if (BulkComponents[0].MaterialOverrides.size() != 1 ||
		BulkComponents[1].MaterialOverrides.size() != 1 ||
		BulkComponents[0].MaterialOverrides[0].Material !=
			"generated/bulk.material-instance.json" ||
		BulkComponents[1].MaterialOverrides[0].Material !=
			"generated/bulk.material-instance.json" ||
		!BulkComponents[0].MaterialOverrides[0].TwoSided ||
		BulkComponents[1].MaterialOverrides[0].TwoSided)
	{
		return Fail("Bulk material edit did not preserve mixed TwoSided");
	}
	if (!Document.Undo() || Document.IsDirty() ||
		Document.GetScene()->Scene.StaticMeshComponents[0]
			.MaterialOverrides.size() != 1 ||
		!Document.GetScene()->Scene.StaticMeshComponents[1]
			.MaterialOverrides.empty() ||
		!Document.Redo() ||
		!Document.SetSelectedMaterialOverride(0,
			"generated/bulk-explicit.material-instance.json", true,
			Diagnostic) ||
		!Document.GetScene()->Scene.StaticMeshComponents[0]
			.MaterialOverrides[0].TwoSided ||
		!Document.GetScene()->Scene.StaticMeshComponents[1]
			.MaterialOverrides[0].TwoSided ||
		!Document.Undo())
	{
		return Fail("Bulk material override was not one undo record");
	}
	if (!Document.ClearSelectedMaterialOverride(0, Diagnostic) ||
		!Document.GetScene()->Scene.StaticMeshComponents[0]
			.MaterialOverrides.empty() ||
		!Document.GetScene()->Scene.StaticMeshComponents[1]
			.MaterialOverrides.empty() ||
		!Document.Undo() || !Document.Undo() || Document.IsDirty())
	{
		return Fail("Bulk material clear/undo did not restore baseline");
	}

	if (Document.SetSelectedComponentsVisibility(false) != 2 ||
		Document.GetScene()->Scene.StaticMeshComponents[0].Visible ||
		Document.GetScene()->Scene.StaticMeshComponents[1].Visible ||
		!Document.IsDirty() || !Document.Undo() || Document.IsDirty() ||
		!Document.GetScene()->Scene.StaticMeshComponents[0].Visible ||
		!Document.GetScene()->Scene.StaticMeshComponents[1].Visible)
	{
		return Fail("Native outliner bulk visibility failed");
	}
	if (Document.SetAllComponentsVisibility(true) != 1 ||
		!Document.GetScene()->Scene.StaticMeshComponents[2].Visible ||
		!Document.Undo() || Document.IsDirty() ||
		Document.SetAllComponentsVisibility(false) != 2 ||
		Document.GetScene()->Scene.StaticMeshComponents[0].Visible ||
		Document.GetScene()->Scene.StaticMeshComponents[1].Visible ||
		!Document.Undo() || Document.IsDirty() ||
		Document.SetUnselectedComponentsVisibility(true) != 1 ||
		!Document.GetScene()->Scene.StaticMeshComponents[2].Visible ||
		!Document.Undo() || Document.IsDirty())
	{
		return Fail("Native visibility commands are not atomic");
	}
	Document.InvertSelection();
	if (Document.GetSelectionCount() != 1 ||
		!Document.IsComponentSelected(SelectionHidden.Id) ||
		Document.GetWorldBounds(true) ||
		Document.IsDirty())
	{
		return Fail("Native invert/hidden focus selection failed");
	}
	Document.InvertSelection();
	if (Document.GetSelectionCount() != 2 ||
		!Document.IsComponentSelected(SelectionNear.Id) ||
		!Document.IsComponentSelected(SelectionFar.Id))
	{
		return Fail("Native invert selection did not round-trip");
	}

	if (Document.DuplicateSelected(Diagnostic) != 2 ||
		Document.GetSelectionCount() != 2 ||
		Document.GetScene()->Scene.StaticMeshComponents.size() != 5)
	{
		return Fail("Native duplicate command failed");
	}
	const auto& DuplicatedComponents =
		Document.GetScene()->Scene.StaticMeshComponents;
	if (DuplicatedComponents[3].Id == SelectionNear.Id ||
		DuplicatedComponents[4].Id == SelectionFar.Id ||
		DuplicatedComponents[3].Name == SelectionNear.Name ||
		DuplicatedComponents[4].Name == SelectionFar.Name ||
		DuplicatedComponents[3].StaticMesh != SelectionNear.StaticMesh ||
		DuplicatedComponents[4].StaticMesh != SelectionFar.StaticMesh ||
		DuplicatedComponents[3].MaterialOverrides.size() != 1 ||
		DuplicatedComponents[3].MaterialOverrides[0].Material !=
			SelectionNear.MaterialOverrides[0].Material ||
		DuplicatedComponents[3].MaterialOverrides[0].TwoSided !=
			SelectionNear.MaterialOverrides[0].TwoSided ||
		!DuplicatedComponents[4].MaterialOverrides.empty() ||
		!Document.IsComponentSelected(DuplicatedComponents[3].Id) ||
		!Document.IsComponentSelected(DuplicatedComponents[4].Id) ||
		!Document.Undo() || Document.IsDirty() ||
		Document.GetScene()->Scene.StaticMeshComponents.size() != 3)
	{
		return Fail("Native duplicate did not preserve data/undo atomically");
	}
	if (Document.SelectComponents(BulkSelection,
			EEditorNativeSceneSelectionMode::Replace) != 2)
	{
		return Fail("Cannot prepare native clipboard test");
	}
	const u64 RevisionBeforeCopy = Document.GetRevision();
	if (Document.CopySelectedToClipboard(Diagnostic) != 2 ||
		Document.GetRevision() != RevisionBeforeCopy ||
		Document.IsDirty() ||
		Document.CutSelectedToClipboard(Diagnostic) != 2 ||
		Document.GetScene()->Scene.StaticMeshComponents.size() != 1 ||
		!Document.IsDirty() || !Document.Undo() || Document.IsDirty() ||
		Document.GetScene()->Scene.StaticMeshComponents.size() != 3)
	{
		return Fail("Native copy/cut clipboard semantics failed");
	}
	if (Document.PasteClipboard(Diagnostic) != 2 ||
		Document.GetSelectionCount() != 2 ||
		Document.GetScene()->Scene.StaticMeshComponents.size() != 5 ||
		!Document.IsDirty() || !Document.Undo() || Document.IsDirty() ||
		Document.GetScene()->Scene.StaticMeshComponents.size() != 3)
	{
		return Fail("Native paste was not one undo record");
	}

	const FEditorNativeSceneSelectionFrustum NearFrustum =
		MakeBoxFrustum({-0.1f, -0.1f, -1.0f},
			{0.1f, 0.1f, 1.0f});
	if (Document.SelectFrustum(NearFrustum,
			EEditorNativeSceneSelectionMode::Replace) != 1 ||
		Document.GetSelectionCount() != 1 ||
		!Document.IsComponentSelected(SelectionNear.Id) ||
		Document.IsComponentSelected(SelectionHidden.Id))
	{
		return Fail("Native rectangle replace selection failed");
	}
	const FEditorNativeSceneSelectionFrustum FarFrustum =
		MakeBoxFrustum({4.0f, -2.0f, -1.0f},
			{8.0f, 2.0f, 1.0f});
	if (Document.SelectFrustum(FarFrustum,
			EEditorNativeSceneSelectionMode::Add) != 1 ||
		Document.GetSelectionCount() != 2 ||
		!Document.IsComponentSelected(SelectionFar.Id) ||
		Document.SelectFrustum(NearFrustum,
			EEditorNativeSceneSelectionMode::Remove) != 1 ||
		Document.GetSelectionCount() != 1 ||
		!Document.IsComponentSelected(SelectionFar.Id) ||
		Document.SelectFrustum(FarFrustum,
			EEditorNativeSceneSelectionMode::Toggle) != 1 ||
		Document.GetSelectionCount() != 0)
	{
		return Fail("Native rectangle add/remove/toggle selection failed");
	}
	if (!Document.SelectObject(StableSceneIdHash(SelectionFar.Id),
			EEditorNativeSceneSelectionMode::Replace))
	{
		return Fail("Cannot prepare invalid-frustum selection test");
	}
	FEditorNativeSceneSelectionFrustum InvalidFrustum;
	InvalidFrustum.Planes.push_back({});
	if (Document.SelectFrustum(InvalidFrustum,
			EEditorNativeSceneSelectionMode::Replace) != 0 ||
		Document.GetSelectionCount() != 1 ||
		!Document.IsComponentSelected(SelectionFar.Id))
	{
		return Fail("Invalid native selection frustum changed selection");
	}

	const u64 RevisionBeforeCrossSceneCopy =
		Document.GetRevision();
	const auto FarWorldBounds = Document.GetWorldBounds(true);
	if (Document.CopySelectedToClipboard(Diagnostic) != 1 ||
		Document.GetRevision() != RevisionBeforeCrossSceneCopy ||
		!FarWorldBounds ||
		FarWorldBounds->Minimum !=
			xr_array<float, 3>{5.0f, -1.0f, 0.0f} ||
		FarWorldBounds->Maximum !=
			xr_array<float, 3>{7.0f, 1.0f, 0.0f})
	{
		return Fail("Cannot prepare cross-scene native clipboard");
	}
	Document.NewRenderScene("Clipboard target scene");
	if (Document.GetWorldBounds(false))
		return Fail("Empty native scene unexpectedly has world bounds");
	if (Document.PasteClipboard(Diagnostic) != 1 ||
		Document.GetSelectionCount() != 1 ||
		Document.GetScene()->Scene.StaticMeshComponents.size() != 1 ||
		Document.GetScene()->StaticMeshes.size() != 1)
	{
		return Fail("Native clipboard did not survive scene replacement");
	}
	const std::filesystem::path ClipboardScenePath =
		Root / "clipboard.render-scene.json";
	if (!Document.SaveAs(ClipboardScenePath, Diagnostic))
		return Fail("Cross-scene clipboard target did not save");
	const FResolvedRenderSceneResult ClipboardReloaded =
		LoadRenderSceneAsset(ClipboardScenePath);
	if (!ClipboardReloaded.Succeeded() ||
		ClipboardReloaded.Value.Scene.StaticMeshComponents.size() != 1 ||
		ClipboardReloaded.Value.StaticMeshes.size() != 1 ||
		ClipboardReloaded.Value.Scene.StaticMeshComponents[0].StaticMesh !=
			xr_string(OtherMeshPath.filename().generic_string()))
	{
		return Fail("Cross-scene clipboard mesh reference did not rebase");
	}

	Document.Close();
	if (Document.IsOpen() || Document.GetSelectionCount() != 0)
		return Fail("Closing native document did not clear state");
	Document.NewRenderScene("Empty native scene");
	if (!Document.IsOpen() || !Document.IsEditableRenderScene() ||
		!Document.IsDirty() || !Document.GetSourcePath().empty() ||
		!Document.GetScene() ||
		!Document.GetScene()->Scene.StaticMeshComponents.empty() ||
		!IsValidSceneStableId(Document.GetScene()->Scene.Id))
	{
		return Fail("New native render scene state is invalid");
	}
	if (Document.Save(Diagnostic) ||
		Diagnostic.find("target path is empty") == xr_string::npos)
	{
		return Fail("Unsaved native scene did not require Save As");
	}
	const std::filesystem::path EmptyScenePath =
		Root / "empty.render-scene.json";
	if (!Document.SaveAs(EmptyScenePath, Diagnostic) ||
		Document.IsDirty() ||
		Document.GetSourcePath() != EmptyScenePath.lexically_normal())
	{
		return Fail("New native render scene Save As failed");
	}
	const FResolvedRenderSceneResult EmptyReloaded =
		LoadRenderSceneAsset(EmptyScenePath);
	if (!EmptyReloaded.Succeeded() ||
		!EmptyReloaded.Value.Scene.StaticMeshComponents.empty())
	{
		return Fail("Empty native render scene did not round-trip");
	}

	xr_array<float, 16> LightTransform =
		FLightComponent{}.LocalToWorld;
	LightTransform[12] = 3.0f;
	LightTransform[13] = 4.0f;
	LightTransform[14] = 5.0f;
	if (!Document.AddLightComponent(
			ELightType::Spot, LightTransform, Diagnostic) ||
		Document.GetSelectionCount() != 1 ||
		Document.GetScene()->Scene.Version != RenderSceneAssetVersion ||
		Document.GetScene()->Scene.LightComponents.size() != 1)
	{
		return Fail("Native light creation failed");
	}
	const auto InitialLightDetails =
		Document.GetSingleSelectedLightDetails();
	const auto LightBounds = Document.GetWorldBounds(true);
	if (!InitialLightDetails ||
		InitialLightDetails->Type != ELightType::Spot ||
		InitialLightDetails->Position !=
			xr_array<float, 3>{3.0f, 4.0f, 5.0f} ||
		!LightBounds ||
		LightBounds->Minimum !=
			xr_array<float, 3>{2.5f, 3.5f, 4.5f} ||
		LightBounds->Maximum !=
			xr_array<float, 3>{3.5f, 4.5f, 5.5f})
	{
		return Fail("Native light details/bounds are incorrect");
	}

	FEditorNativeSceneLightDetails LightDetails =
		*InitialLightDetails;
	const u64 RevisionBeforeInvalidLight =
		Document.GetRevision();
	LightDetails.Range = 0.0f;
	if (Document.SetSelectedLightDetails(LightDetails, Diagnostic) ||
		Document.GetRevision() != RevisionBeforeInvalidLight)
	{
		return Fail("Invalid native light edit changed the document");
	}
	LightDetails = *InitialLightDetails;
	LightDetails.Name = "Edited spot";
	LightDetails.Color = {1.5f, 0.75f, 0.25f};
	LightDetails.Intensity = 4.0f;
	LightDetails.Range = 30.0f;
	LightDetails.InnerConeAngleDegrees = 12.0f;
	LightDetails.OuterConeAngleDegrees = 32.0f;
	LightDetails.CastShadows = false;
	if (!Document.BeginEditTransaction() ||
		!Document.SetSelectedLightDetails(LightDetails, Diagnostic))
	{
		return Fail("Native light edit transaction did not start");
	}
	LightDetails.Intensity = 6.0f;
	if (!Document.SetSelectedLightDetails(LightDetails, Diagnostic) ||
		!Document.EndEditTransaction())
	{
		return Fail("Native light edit transaction did not commit");
	}
	const auto EditedLightDetails =
		Document.GetSingleSelectedLightDetails();
	if (!EditedLightDetails ||
		EditedLightDetails->Name != "Edited spot" ||
		EditedLightDetails->Color != LightDetails.Color ||
		EditedLightDetails->Intensity != 6.0f ||
		EditedLightDetails->Range != 30.0f ||
		EditedLightDetails->CastShadows ||
		!Document.Undo() ||
		Document.GetSingleSelectedLightDetails()->Intensity != 1.0f ||
		!Document.Redo() ||
		Document.GetSingleSelectedLightDetails()->Intensity != 6.0f)
	{
		return Fail("Native light edit/undo/redo failed");
	}

	const xr_string OriginalLightId =
		Document.GetScene()->Scene.LightComponents[0].Id;
	const u64 RevisionBeforeLightCopy =
		Document.GetRevision();
	if (Document.CopySelectedToClipboard(Diagnostic) != 1 ||
		Document.GetRevision() != RevisionBeforeLightCopy ||
		Document.CutSelectedToClipboard(Diagnostic) != 1 ||
		!Document.GetScene()->Scene.LightComponents.empty() ||
		!Document.Undo() ||
		Document.GetScene()->Scene.LightComponents.size() != 1 ||
		Document.PasteClipboard(Diagnostic) != 1 ||
		Document.GetSelectionCount() != 1 ||
		Document.GetScene()->Scene.LightComponents.size() != 2 ||
		Document.GetScene()->Scene.LightComponents[1].Id ==
			OriginalLightId ||
		Document.GetScene()->Scene.LightComponents[1].Name !=
			"Edited spot Copy" ||
		Document.GetScene()->Scene.LightComponents[1].Intensity != 6.0f ||
		!Document.Undo() ||
		Document.GetScene()->Scene.LightComponents.size() != 1 ||
		!Document.SelectObject(StableSceneIdHash(OriginalLightId),
			EEditorNativeSceneSelectionMode::Replace))
	{
		return Fail("Native light copy/cut/paste/undo failed");
	}
	if (Document.DuplicateSelected(Diagnostic) != 1 ||
		Document.GetScene()->Scene.LightComponents.size() != 2 ||
		Document.GetScene()->Scene.LightComponents[1].Id ==
			OriginalLightId ||
		Document.GetScene()->Scene.LightComponents[1].Intensity != 6.0f ||
		!Document.Undo() ||
		Document.GetScene()->Scene.LightComponents.size() != 1)
	{
		return Fail("Native light duplicate/undo failed");
	}
	if (!Document.SelectObject(StableSceneIdHash(OriginalLightId),
			EEditorNativeSceneSelectionMode::Replace) ||
		Document.SetSelectedComponentsVisibility(false) != 1 ||
		Document.GetScene()->Scene.LightComponents[0].Visible ||
		!Document.Undo() ||
		!Document.GetScene()->Scene.LightComponents[0].Visible)
	{
		return Fail("Native light selection/visibility failed");
	}
	const FEditorNativeSceneSelectionFrustum LightFrustum =
		MakeBoxFrustum({2.0f, 3.0f, 4.0f},
			{4.0f, 5.0f, 6.0f});
	if (Document.SelectFrustum(LightFrustum,
			EEditorNativeSceneSelectionMode::Replace) != 1 ||
		!Document.IsComponentSelected(OriginalLightId) ||
		Document.RemoveSelected() != 1 ||
		!Document.GetScene()->Scene.LightComponents.empty() ||
		!Document.Undo() ||
		Document.GetScene()->Scene.LightComponents.size() != 1)
	{
		return Fail("Native light frustum/remove/undo failed");
	}
	if (!Document.Save(Diagnostic))
		return Fail("Native light scene did not save");
	const FResolvedRenderSceneResult LightReloaded =
		LoadRenderSceneAsset(Document.GetSourcePath());
	if (!LightReloaded.Succeeded() ||
		LightReloaded.Value.Scene.Version != RenderSceneAssetVersion ||
		LightReloaded.Value.Scene.LightComponents.size() != 1 ||
		LightReloaded.Value.Scene.LightComponents[0].Name !=
			"Edited spot" ||
		LightReloaded.Value.Scene.LightComponents[0].Intensity != 6.0f)
	{
		return Fail("Native light did not round-trip through Save");
	}

	if (!Document.SelectObject(StableSceneIdHash(OriginalLightId),
			EEditorNativeSceneSelectionMode::Replace) ||
		Document.CopySelectedToClipboard(Diagnostic) != 1)
	{
		return Fail("Cannot prepare cross-scene light clipboard");
	}
	Document.NewRenderScene("Light clipboard target");
	if (Document.PasteClipboard(Diagnostic) != 1 ||
		Document.GetScene()->Scene.Version != RenderSceneAssetVersion ||
		Document.GetScene()->Scene.LightComponents.size() != 1 ||
		Document.GetScene()->Scene.LightComponents[0].Id ==
			OriginalLightId ||
		Document.GetScene()->Scene.LightComponents[0].Name !=
			"Edited spot" ||
		Document.GetScene()->Scene.LightComponents[0].Intensity != 6.0f)
	{
		return Fail("Native light clipboard did not survive scene replacement");
	}
	const std::filesystem::path LightClipboardScenePath =
		Root / "light-clipboard.render-scene.json";
	if (!Document.SaveAs(LightClipboardScenePath, Diagnostic))
		return Fail("Cross-scene light clipboard target did not save");
	const FResolvedRenderSceneResult LightClipboardReloaded =
		LoadRenderSceneAsset(LightClipboardScenePath);
	if (!LightClipboardReloaded.Succeeded() ||
		LightClipboardReloaded.Value.Scene.LightComponents.size() != 1 ||
		LightClipboardReloaded.Value.Scene.LightComponents[0].Name !=
			"Edited spot" ||
		LightClipboardReloaded.Value.Scene.LightComponents[0].Intensity !=
			6.0f)
	{
		return Fail("Cross-scene light clipboard did not round-trip");
	}

	FRenderSceneAsset LegacyDocumentScene;
	LegacyDocumentScene.Version =
		LegacyStaticMeshOnlyRenderSceneAssetVersion;
	LegacyDocumentScene.Id =
		"5e4c443b-b94f-4f78-bf00-b5e118a4ac02";
	LegacyDocumentScene.Name = "Legacy native scene";
	const std::filesystem::path LegacyDocumentPath =
		Root / "legacy-v1.render-scene.json";
	WriteText(LegacyDocumentPath,
		SerializeRenderSceneAssetJson(LegacyDocumentScene));
	if (!Document.OpenRenderScene(LegacyDocumentPath, Diagnostic) ||
		!Document.AddLightComponent(
			ELightType::Point,
			FLightComponent{}.LocalToWorld, Diagnostic) ||
		Document.GetScene()->Scene.Version != RenderSceneAssetVersion ||
		!Document.Save(Diagnostic))
	{
		return Fail("Native scene v1 did not upgrade on light creation");
	}
	return 0;
}
