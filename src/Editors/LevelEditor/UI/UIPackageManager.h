#pragma once

#include "EditorWnd.h"
#include "../Packages/PackageCatalog.h"
#include "../Packages/PackageOperations.h"

class UIPackageManager final : public IEditorWnd
{
public:
	static void Show();
	static void Update();
	static void Destroy();
	void Draw() override;
	~UIPackageManager() override;

private:
	using Action = SdkPackages::PackageOperations::Action;

	UIPackageManager();
	void Start(Action Operation, const SdkPackages::PackageInfo& Info, bool IsFromCatalog = false);
	void Poll();
	void Refresh(bool ResetChecks = true);
	void DrawToolbar();
	void DrawNavigation();
	void DrawList();
	void DrawDetails();
	void LoadCardDocuments(const SdkPackages::PackageRow& Row);
	void DrawAddDialog();
	void DrawSourceMenu();
	void ChooseLocalSource();
	void PollLocalSource();
	void BuildImportTree();
	void DrawImportDialog();
	void DrawImportDetails(bool HasLevelSelection, float Height);
	void DrawImportComparison();
	void DrawImportReport();
	void DrawImportDependencies(float Height);
	void InvalidateImportDependencies();
	bool Matches(const SdkPackages::PackageRow& Row) const;
	struct ImportNode
	{
		xr_map<xr_string, ImportNode> Folders;
		xr_vector<size_t> Files;
		xr_vector<size_t> Leaves;
	};
	void DrawImportNode(ImportNode& Node);

	static UIPackageManager* Form;
	SdkPackages::PackageStore Store;
	SdkPackages::PackageCatalog Catalog;
	SdkPackages::PackageOperations Operations;
	xr_vector<SdkPackages::PackageInfo> Installed;
	xr_vector<SdkPackages::PackageRow> Packages;
	xr_string SelectedKey;
	xr_string CardDocumentsKey;
	SdkPackages::PackageDocuments CardDocuments;
	xr_string CardDocumentsError;
	xr_string CardDocumentsSource;
	xr_string Message;
	xr_hash_map<xr_string, xr_string> CheckedCommits;
	bool IsBusy = false;
	bool IsSucceeded = false;
	bool NeedsPluginRefresh = false;
	bool ShouldOpenAddDialog = false;
	bool ShouldOpenSourceMenu = false;
	bool ShouldOpenImportDialog = false;
	bool ShouldOpenImportReport = false;
	bool ShouldCloseImportDialog = false;
	bool ShowExistingImportFiles = false;
	bool IsImportDetailsExpanded = false;
	bool ShouldOpenImportComparison = false;
	bool ShowOnlyComparisonDifferences = true;
	size_t ComparisonIndex = std::numeric_limits<size_t>::max();
	SdkPackages::AssetComparison ImportComparison;
	xr_string ComparisonError;
	bool CanReplaceImportFiles = false;
	bool IsDependencyCheckCurrent = false;
	bool ShouldShowImportDependencies = false;
	bool ShowOnlyDependencyProblems = true;
	xr_string DependencyCheckError;
	SdkPackages::DependencyReport ImportDependencies;
	SdkPackages::ImportPlan ImportPlan;
	SdkPackages::ImportReport ImportReport;
	ImportNode ImportTree;
	bool IsSortDescending = false;
	SdkPackages::CatalogView ActiveView = SdkPackages::CatalogView::All;
	SdkPackages::PackageSource AddSource = SdkPackages::PackageSource::GitHub;
	class SourceSelection final
	{
	public:
		SourceSelection() = default;
		~SourceSelection() = default;
		xr_atomic_bool IsReady = false;
		xr_string Path;
		xr_string Error;
	};
	xr_shared_ptr<SourceSelection> LocalSelection;
	xr_string LocalSourceError;
	char LocalPath[4096] = {};
	char RepositoryUrl[1024] = {};
	char Revision[256] = {};
	char Search[128] = {};
	int SelectedKind = 0;
};
