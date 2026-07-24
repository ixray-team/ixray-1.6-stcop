#pragma once

#include <MaterialDependencyWatcher.h>
#include <MaterialEditorDocument.h>
#include <MaterialInstanceEditorDocument.h>
#include <Editor/MaterialPreviewRenderer.h>

#include <array>
#include <cstdint>
#include <filesystem>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

struct ImNodesContext;

class UIMaterialEditorForm final : public IEditorWnd
{
public:
	UIMaterialEditorForm();
	~UIMaterialEditorForm() override;

	void Draw() override;
	void Show(bool Value = true) noexcept { bOpen = Value; }
	[[nodiscard]] bool OpenInstanceFile(
		const std::filesystem::path& InstancePath
	);

private:
	void DrawToolbar();
	void TickAutosave();
	void TickDependencyWatcher();
	void RefreshDependencyWatch();
	void ApplyDependencyChanges(
		xr_vector<Tiramisu::Editor::FMaterialDependencyChange> Changes,
		bool AllowDirtyReload
	);
	void DrawPalette();
	void DrawGraph();
	void DrawNodeProperties(const FMaterialGraphNode& Node);
	void DrawOutputPanel();
	void DrawPreviewPanel();
	void DrawDetailsPanel();
	[[nodiscard]] bool DrawParameterEditor(
		const FMaterialParameterDefinition& Parameter
	);
	void DrawInstancePanel();
	void Compile();
	void AddNode(xr_string_view Type);
	void DeleteSelection();
	void CopySelection();
	void PasteClipboard();
	void OpenMaterial();
	void OpenAutosave();
	void SaveMaterial(bool SaveAs);
	void OpenInstance();
	void SaveInstance(bool SaveAs);
	void LoadInstanceParent();
	[[nodiscard]] bool ResolveInstanceParent();
	void ResetPresentationState();
	void ReleasePreview();
	void SyncMaterialDrafts();
	void SyncInstanceDrafts();
	[[nodiscard]] std::filesystem::path MaterialRecoveryPath() const;
	[[nodiscard]] std::filesystem::path InstanceRecoveryPath() const;
	static void RemoveRecoveryFile(const std::filesystem::path& Path);

	struct FParameterEditorDraft
	{
		xr_array<char, 128> Name{};
		xr_array<char, 128> DisplayName{};
		xr_array<char, 128> Category{};
		xr_array<char, 512> Description{};
		xr_array<char, 512> DefaultText{};
		EMaterialParameterType Type =
			EMaterialParameterType::Scalar;
		FMaterialValue DefaultValue = 0.0f;
		bool HasMinimum = false;
		bool HasMaximum = false;
		float Minimum = 0.0f;
		float Maximum = 1.0f;
	};

	FParameterEditorDraft& GetParameterDraft(
		const FMaterialParameterDefinition& Parameter
	);
	static void SyncParameterDraft(FParameterEditorDraft& Draft, const FMaterialParameterDefinition& Parameter);
	bool CommitParameterDraft(
		const FMaterialParameterId& ParameterId
	);

	[[nodiscard]] int UiId(xr_string_view StableId);
	[[nodiscard]] static xr_string MakeStableId();
	[[nodiscard]] const FMaterialGraphPin* PinFromUiId(int Id) const;
	void SetDiagnostics(xr_vector<FMaterialDiagnostic> Diagnostics);

	Tiramisu::Editor::TiramisuMaterialEditorDocument Document;
	Tiramisu::Editor::TiramisuMaterialInstanceEditorDocument InstanceDocument;
	FMaterialGraphCompileResult CompileResult;
	xr_vector<FMaterialDiagnostic> Diagnostics;

	ImNodesContext* NodesContext = nullptr;
	xr_hash_map<xr_string, int> StableToUi;
	xr_hash_map<int, xr_string> UiToStable;
	xr_hash_set<int> PositionedNodes;
	xr_hash_map<xr_string, FMaterialValue> NodePropertyDrafts;
	xr_hash_map<xr_string, xr_array<char, 2048>> NodeStringDrafts;
	xr_hash_map<xr_string, FParameterEditorDraft> ParameterDrafts;
	xr_array<char, 128> Search{};
	xr_array<char, 256> MaterialNameDraft{};
	xr_array<char, 512> MaterialTemplateDraft{};
	xr_array<char, 256> InstanceNameDraft{};
	xr_array<char, 512> InstanceParentDraft{};
	xr_hash_map<xr_string, FMaterialValue> InstanceOverrideDrafts;
	xr_hash_map<xr_string, xr_array<char, 512>> InstanceStringDrafts;
	double NextAutosaveTime = 0.0;
	xr_string AutosaveStatus;
	Tiramisu::Editor::TiramisuMaterialDependencyWatcher DependencyWatcher;
	xr_vector<std::filesystem::path> ParentAssetDependencies;
	xr_vector<Tiramisu::Editor::FMaterialDependencyChange>
		PendingDependencyChanges;
	double NextDependencyPollTime = 0.0;
	xr_string DependencyStatus;
	bool AutoReloadDependencies = true;

	IMaterialPreviewRenderer* PreviewRenderer = nullptr;
	FMaterialPreviewHandle PreviewHandle;
	EMaterialPreviewPrimitive PreviewPrimitive = EMaterialPreviewPrimitive::Sphere;
	u32 PreviewWidth = 0;
	u32 PreviewHeight = 0;
	u64 PreviewRevision = 0;
	int PreviewEnvironment = 0;
	bool PreviewSourceDirty = true;
	xr_string SubmittedMaterialJson;
	xr_string SubmittedInstanceJson;
	xr_string SubmittedHlsl;
};
