#pragma once
#include <nlohmann/json.hpp>

// The places of the windows of the Nodes tab. Every mode puts its windows into the same docks
enum class ENodesDock : u8
{
	Graph,
	Browser,
	Outliner,
	Details,
	Bottom,

	Count
};

struct SNodesWindow
{
	const char* Name = nullptr;
	ENodesDock Dock = ENodesDock::Graph;
};

// The windows of a mode are not drawn while another mode or tab is shown: the dock tab bars are
// made again in the order of the windows, so the selected tab of each dock is put back
class CNodesDock
{
public:
	// Once a frame before the windows
	void BeginFrame();
	// ImGui::Begin that keeps the selected tab of the dock. ImGui::End is called by the caller
	bool Begin(const char* Name);

private:
	xr_hash_map<u32, u32> Selection;
	int LastDrawFrame = -10;
	int RestoreFrames = 0;
};

// A node editor of the Nodes tab: Logic, Dialogs, Macros
class INodesMode
{
public:
	virtual ~INodesMode() = default;

	virtual const char* GetName() const = 0;
	virtual const char* GetIcon() const = 0;
	virtual void GetWindows(xr_vector<SNodesWindow>& Windows) const = 0;

	// The windows of the mode, inside the Nodes tab
	virtual void DrawMode() = 0;
	// The buttons of the mode after the common ones of the top bar
	virtual void DrawModeTopBar(float ButtonSize) = 0;

	virtual bool IsModeModified() const = 0;
	virtual bool SaveMode() = 0;
	virtual bool ReloadMode() = 0;
	virtual bool UndoMode(bool Redo) = 0;
	// The file of the mode in the Content Browser
	virtual xr_string GetModePath() const = 0;

	virtual void SerializeMode(nlohmann::json& Data, bool IsSaving) = 0;
};
