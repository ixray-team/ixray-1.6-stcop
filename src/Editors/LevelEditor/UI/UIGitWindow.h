#pragma once

#include "EditorWnd.h"
#include "../Editor/Utils/GitIntegration.h"

class UIGitWindow final : public IEditorWnd
{
public:
	UIGitWindow();
	virtual ~UIGitWindow() override;

	virtual void Draw() override;
	virtual void ResetBegin() override;
	virtual void ResetEnd() override;

	static void Update();
	static void Show();
	static void Close();
	static IC bool IsOpen() { return Form; }

	void DrawToolbar();
	void DrawTaskStatus();
	void DrawRemotesAndBranches();
	void DrawFilesPanel(float Width, float Height);
	void DrawCommitRow();
	void DrawHistory();
	void DrawLFSSettings();
	void DrawAddRemoteDialog();
	void DrawNewBranchDialog();

	static const char* GitStatusIcon(const EGitFileInfo& Info);

	char CommitMessage[1024] = {};
	char NewRemoteName[256] = {};
	char NewRemoteUrl[512] = {};
	char NewBranchName[256] = {};
	bool NewBranchCheckout = true;

	// Locally requested stage states not yet confirmed by `git status`.
	// The worker round-trip takes a while; without this the checkbox
	// snaps back to the stale snapshot right after clicking.
	xr_hash_map<xr_string, std::pair<bool, s64>> PendingStage;

	static UIGitWindow* Form;
};
