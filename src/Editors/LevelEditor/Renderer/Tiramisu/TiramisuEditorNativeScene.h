#pragma once

#include "TiramisuEditorTypes.h"

#include <SceneAsset.h>

#include <array>
#include <filesystem>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

enum class EEditorNativeSceneSelectionMode
{
	Replace,
	Add,
	Remove,
	Toggle
};

struct FEditorNativeSceneSelectionPlane
{
	xr_array<float, 3> Normal = {};
	float Distance = 0.0f;
};

struct FEditorNativeSceneSelectionFrustum
{
	// Points whose dot(Normal, Position) + Distance is <= 0 are inside.
	xr_vector<FEditorNativeSceneSelectionPlane> Planes;
};

struct FEditorNativeSceneBounds
{
	xr_array<float, 3> Minimum = {};
	xr_array<float, 3> Maximum = {};
};

struct FEditorNativeSceneMaterialSlotDetails
{
	u32 MaterialSlot = 0;
	xr_string Name;
	xr_string BaseMaterial;
	bool BaseTwoSided = false;
	bool HasOverride = false;
	xr_string OverrideMaterial;
	bool OverrideTwoSided = false;
};

struct FEditorNativeSceneComponentDetails
{
	xr_string Id;
	xr_string Name;
	xr_string StaticMesh;
	xr_array<float, 3> Position = {};
	bool Visible = true;
	xr_vector<FEditorNativeSceneMaterialSlotDetails> MaterialSlots;
};

struct FEditorNativeSceneLightDetails
{
	xr_string Id;
	xr_string Name;
	Tiramisu::Scene::ELightType Type =
		Tiramisu::Scene::ELightType::Point;
	xr_array<float, 3> Position = {};
	xr_array<float, 3> Color = {1.0f, 1.0f, 1.0f};
	float Intensity = 1.0f;
	float Range = 10.0f;
	float InnerConeAngleDegrees = 20.0f;
	float OuterConeAngleDegrees = 45.0f;
	bool Visible = true;
	bool CastShadows = true;
};

struct FEditorNativeSceneBulkMaterialSlotDetails
{
	u32 MaterialSlot = 0;
	size_t ComponentCount = 0;
	xr_string Name;
	bool NameMixed = false;
	xr_string BaseMaterial;
	bool BaseMaterialMixed = false;
	bool BaseTwoSided = false;
	bool BaseTwoSidedMixed = false;
	size_t OverrideCount = 0;
	xr_string OverrideMaterial;
	bool OverrideMaterialMixed = false;
	bool OverrideTwoSided = false;
	bool OverrideTwoSidedMixed = false;
};

struct FEditorNativeSceneBulkMaterialDetails
{
	size_t ComponentCount = 0;
	xr_vector<FEditorNativeSceneBulkMaterialSlotDetails> MaterialSlots;
};

class TiramisuEditorNativeSceneDocument
{
public:
	void NewRenderScene(xr_string_view Name = "Untitled Render Scene");
	[[nodiscard]] bool OpenStaticMesh(
		const std::filesystem::path& Path, xr_string& Diagnostic
	);
	[[nodiscard]] bool OpenRenderScene(
		const std::filesystem::path& Path, xr_string& Diagnostic
	);
	void Close() noexcept;

	[[nodiscard]] bool IsOpen() const noexcept;
	[[nodiscard]] bool IsEditableRenderScene() const noexcept;
	[[nodiscard]] bool IsDirty() const noexcept;
	[[nodiscard]] const Tiramisu::Scene::FResolvedRenderScene*
	GetScene() const noexcept;
	[[nodiscard]] bool IsComponentSelected(
		xr_string_view ComponentId
	) const;
	[[nodiscard]] size_t GetSelectionCount() const noexcept;
	[[nodiscard]] xr_optional<FEditorNativeSceneComponentDetails>
	GetSingleSelectedComponentDetails() const;
	[[nodiscard]] xr_optional<FEditorNativeSceneLightDetails>
	GetSingleSelectedLightDetails() const;
	[[nodiscard]] xr_optional<FEditorNativeSceneBulkMaterialDetails>
	GetSelectedComponentsMaterialDetails() const;
	[[nodiscard]] xr_optional<FEditorNativeSceneBounds>
	GetWorldBounds(bool SelectedOnly) const;
	void ClearSelection();
	[[nodiscard]] bool SelectObject(
		u64 ObjectId,
		EEditorNativeSceneSelectionMode Mode
	);
	[[nodiscard]] size_t SelectComponents(
		xr_span<const xr_string> ComponentIds,
		EEditorNativeSceneSelectionMode Mode
	);
	[[nodiscard]] size_t SelectFrustum(
		const FEditorNativeSceneSelectionFrustum& Frustum,
		EEditorNativeSceneSelectionMode Mode
	);
	void SelectAll();
	void InvertSelection();
	[[nodiscard]] bool AddStaticMeshComponent(
		const std::filesystem::path& StaticMeshPath,
		const xr_array<float, 16>& LocalToWorld,
		xr_string& Diagnostic
	);
	[[nodiscard]] bool AddLightComponent(
		Tiramisu::Scene::ELightType Type,
		const xr_array<float, 16>& LocalToWorld,
		xr_string& Diagnostic
	);
	[[nodiscard]] size_t DuplicateSelected(
		xr_string& Diagnostic
	);
	[[nodiscard]] size_t CopySelectedToClipboard(
		xr_string& Diagnostic
	);
	[[nodiscard]] size_t CutSelectedToClipboard(
		xr_string& Diagnostic
	);
	[[nodiscard]] size_t PasteClipboard(
		xr_string& Diagnostic
	);
	[[nodiscard]] size_t RemoveSelected();
	[[nodiscard]] bool SetSelectedComponentName(
		xr_string_view Name, xr_string& Diagnostic
	);
	[[nodiscard]] bool SetSelectedComponentVisibility(bool Visible);
	[[nodiscard]] size_t SetSelectedComponentsVisibility(bool Visible);
	[[nodiscard]] size_t SetUnselectedComponentsVisibility(bool Visible);
	[[nodiscard]] size_t SetAllComponentsVisibility(bool Visible);
	[[nodiscard]] bool SetSelectedComponentPosition(
		const xr_array<float, 3>& Position
	);
	[[nodiscard]] bool SetSelectedLightDetails(
		const FEditorNativeSceneLightDetails& Details,
		xr_string& Diagnostic
	);
	[[nodiscard]] bool SetSelectedMaterialOverride(
		u32 MaterialSlot, xr_string_view Material, bool TwoSided, xr_string& Diagnostic
	);
	[[nodiscard]] bool SetSelectedComponentsMaterialOverride(
		u32 MaterialSlot, xr_string_view Material, xr_optional<bool> TwoSided, xr_string& Diagnostic
	);
	[[nodiscard]] bool ClearSelectedMaterialOverride(
		u32 MaterialSlot, xr_string& Diagnostic
	);

	// Transform tools bracket continuous mouse input in one transaction so a
	// drag produces one undo record. Callback changes are validated before
	// publication and never expose renderer-specific types.
	[[nodiscard]] bool BeginEditTransaction();
	[[nodiscard]] bool TransformSelected(
		const std::function<void(xr_array<float, 16>&)>& Transform
	);
	[[nodiscard]] bool TranslateSelected(
		const xr_array<float, 3>& Delta
	);
	[[nodiscard]] bool EndEditTransaction(bool Commit = true);
	[[nodiscard]] bool Undo();
	[[nodiscard]] bool Redo();
	[[nodiscard]] bool Save(xr_string& Diagnostic);
	[[nodiscard]] bool SaveAs(
		const std::filesystem::path& Path, xr_string& Diagnostic
	);

	[[nodiscard]] const std::filesystem::path& GetSourcePath() const noexcept
	{
		return SourcePath;
	}
	[[nodiscard]] u64 GetRevision() const noexcept
	{
		return Revision;
	}

private:
	struct FClipboardEntry
	{
		Tiramisu::Scene::FStaticMeshComponent Component;
		Tiramisu::Scene::FStaticMeshAsset StaticMesh;
		std::filesystem::path StaticMeshSourcePath;
	};

	void PublishSceneChange();
	void UpdateDirtyState();
	void PruneSelection();

	Tiramisu::Scene::FResolvedRenderScene Scene;
	std::filesystem::path SourcePath;
	xr_hash_set<xr_string> SelectedComponents;
	xr_vector<Tiramisu::Scene::FResolvedRenderScene> UndoStack;
	xr_vector<Tiramisu::Scene::FResolvedRenderScene> RedoStack;
	xr_optional<Tiramisu::Scene::FResolvedRenderScene>
		TransactionBaseline;
	xr_vector<FClipboardEntry> Clipboard;
	xr_vector<Tiramisu::Scene::FLightComponent> LightClipboard;
	xr_string SavedSceneJson;
	u64 Revision = 0;
	bool Open = false;
	bool EditableRenderScene = false;
	bool TransactionChanged = false;
	bool Dirty = false;
};

[[nodiscard]] TiramisuEditorNativeSceneDocument& GetEditorNativeSceneDocument() noexcept;
