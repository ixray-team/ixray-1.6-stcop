#pragma once

#include <MaterialDependencyWatcher.h>
#include <MaterialInstanceEditorDocument.h>
#include <Editor/MaterialPreviewRenderer.h>

#include <filesystem>

// Самостоятельное окно Material Instance. Оно не владеет graph/master
// документом и изменяет только parent и parameter overrides.
class UIMaterialInstanceEditorForm final : public IEditorWnd
{
public:
	UIMaterialInstanceEditorForm();
	~UIMaterialInstanceEditorForm() override;

	void Draw() override;
	void Show(bool Value = true) noexcept { bOpen = Value; }

	[[nodiscard]] bool OpenInstanceFile(
		const std::filesystem::path& InstancePath
	);
	[[nodiscard]] bool CreateInstanceFromMaster(
		const std::filesystem::path& MasterPath,
		const std::filesystem::path& InstancePath
	);

private:
	void DrawToolbar();
	void DrawInstanceDetails();
	void DrawOverrides();
	void DrawPreview();
	void DrawDiagnostics();
	void DrawJson();
	void TickAutosave();
	void TickDependencies();
	void RefreshDependencies();
	void NewInstance();
	void OpenInstance();
	void OpenAutosave();
	void SaveInstance(bool SaveAs);
	void ChooseParent();
	[[nodiscard]] bool SetParentFromFile(
		const std::filesystem::path& ParentPath
	);
	[[nodiscard]] bool ResolveParent();
	void SyncDrafts();
	void ReleasePreview();
	void SetDiagnostics(xr_vector<FMaterialDiagnostic> InDiagnostics);
	[[nodiscard]] std::filesystem::path RecoveryPath() const;

	Tiramisu::Editor::TiramisuMaterialInstanceEditorDocument Document;
	xr_vector<FMaterialDiagnostic> Diagnostics;
	xr_vector<std::filesystem::path> ParentDependencies;
	Tiramisu::Editor::TiramisuMaterialDependencyWatcher DependencyWatcher;
	xr_hash_map<xr_string, FMaterialValue> OverrideDrafts;
	xr_hash_map<xr_string, xr_array<char, 512>> StringDrafts;
	xr_array<char, 256> NameDraft{};
	xr_array<char, 512> ParentDraft{};
	xr_string AutosaveStatus;
	xr_string DependencyStatus;
	double NextAutosaveTime = 0.0;
	double NextDependencyPollTime = 0.0;

	IMaterialPreviewRenderer* PreviewRenderer = nullptr;
	FMaterialPreviewHandle PreviewHandle;
	EMaterialPreviewPrimitive PreviewPrimitive =
		EMaterialPreviewPrimitive::Sphere;
	u32 PreviewWidth = 0;
	u32 PreviewHeight = 0;
	u64 PreviewRevision = 0;
	int PreviewEnvironment = 0;
	bool PreviewDirty = true;
	xr_string SubmittedMaterialJson;
	xr_string SubmittedInstanceJson;
	xr_string SubmittedHlsl;
};
