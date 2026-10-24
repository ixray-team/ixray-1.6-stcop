#pragma once
#include "../Viewports/IViewport.h"

class CActorTools;
class UIKeyForm;
class UIActorLibrary;
class CContentView;

// Actor Editor workspace: a top bar tab with its own dock space.
// While the tab is active the global Tools is the actor tool, so the xrECore input,
// render and command hooks drive it.
class UIActorEditorForm :
	public IViewport
{
public:
	enum RenderMode
	{
		Render_Editor,
		Render_Engine
	};

	enum EAnimMode
	{
		e8bit,
		e16bit,
		e32bit
	};

	UIActorEditorForm();
	virtual ~UIActorEditorForm();

	virtual void Draw() override;
	virtual void Render() override;

	static void Create(int Tab);
	// Switches to the tab and loads the object when a file is given
	static void Open(const char* FileName = nullptr);
	static void Destroy();
	static bool IsActive();
	// Actor tool while the Actor Editor tab is active, otherwise nullptr
	static CActorTools* GetActiveTool();
	static void Update();
	// Menus of the main menu bar while the tab is active
	static void DrawMainMenu();
	// .object, .skl and .omf files dropped on the editor
	static void DropFile(const xr_string& File);

	static RenderMode GetRenderMode();
	static void SetRenderMode(bool EngineMode);
	static int GetPickMode();
	static UIKeyForm* GetKeyForm();
	static CContentView* GetContentView() { return ContentBrowser; }

private:
	void DrawRenderToolBar(ImVec2 Pos, ImVec2 Size);
	void DrawObjectTool();
	void DrawObjectItems();
	void DrawProperties();
	void DrawSkeletonFlag(const char* Id, const char* Icon, const char* Hint, u32 Flag, ImDrawFlags Rounding);
	void SetSmooth(ESmoothGroup Smooth);
	void SetAnim(EAnimMode Anim);

	static UIActorEditorForm* Form;
	static CContentView* ContentBrowser;

	CActorTools* Tool = nullptr;
	UIKeyForm* KeyForm = nullptr;
	UIActorLibrary* Library = nullptr;
	bool WasActive = false;
	size_t PrevViewID = 0;
	int PickMode = 2;
	RenderMode Mode = Render_Editor;
	EAnimMode AnimMode = e8bit;
	bool Simulate = false;
};
