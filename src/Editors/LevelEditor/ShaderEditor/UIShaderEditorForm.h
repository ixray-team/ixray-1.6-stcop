#pragma once
#include "../Viewports/IViewport.h"

class CShaderTool;

// Shader Editor workspace: a top bar tab with its own dock space.
// While the tab is active the global Tools is the shader tool, so the xrECore input,
// render and frame hooks drive it.
class UIShaderEditorForm :
	public IViewport
{
public:
	UIShaderEditorForm();
	virtual ~UIShaderEditorForm();

	virtual void Draw() override;
	virtual void Render() override;

	static void Create(int Tab);
	static void Open();
	static void Destroy();
	static bool IsActive();
	// Shader tool while the Shaders tab is active, otherwise nullptr
	static CShaderTool* GetActiveTool();
	static void Update();
	// Saves every library of the shader tools
	static bool SaveAll();
	// Reloads the library of the current shader tool
	static bool ReloadCurrent();

private:
	void DrawRenderToolBar(ImVec2 Pos, ImVec2 Size);
	void DrawItemList();

	static UIShaderEditorForm* Form;

	CShaderTool* Tool = nullptr;
	bool WasActive = false;
	size_t PrevViewID = 0;
};
