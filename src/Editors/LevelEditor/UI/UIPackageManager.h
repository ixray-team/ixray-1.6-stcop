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
	void DrawLocalBrowser();
	void LoadBrowser(const xr_path& Directory);
	bool Matches(const SdkPackages::PackageRow& Row) const;

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
	bool IsSortDescending = false;
	SdkPackages::CatalogView ActiveView = SdkPackages::CatalogView::All;
	SdkPackages::PackageSource AddSource = SdkPackages::PackageSource::GitHub;
	struct BrowserEntry
	{
		xr_path Path;
		bool IsDirectory;
	};
	xr_path BrowserDirectory;
	xr_path BrowserSelection;
	xr_vector<BrowserEntry> BrowserEntries;
	xr_string BrowserError;
	char LocalPath[4096] = {};
	char BrowserLocation[4096] = {};
	char RepositoryUrl[1024] = {};
	char Revision[256] = {};
	char Search[128] = {};
	int SelectedKind = 0;
};
