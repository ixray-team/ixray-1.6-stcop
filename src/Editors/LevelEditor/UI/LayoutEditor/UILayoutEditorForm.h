#pragma once
#include "UILayoutDocument.h"
#include "LuaCompletion.h"
#include "../Coder/CodeEditorUtils.h"
// xr_unique_ptr checks the type when declared, so it can't be a forward declaration
#include "plugins/TextEditor.h"
#include "../../Nodes/UIDialogLocalization.h"
#include "Tree/Properties/UIPropertiesForm.h"

// UI Layout workspace: a top bar tab with a WYSIWYG editor for scripted dialogs.
// Saves configs\ui\<name>.xml, configs\ui\<name>_16.xml and scripts\<name>.script,
// the working copy goes to rawdata\widgets\<name>.uiw
class UILayoutEditorForm :
	public IEditorWnd,
	public pureFrame
{
public:
	UILayoutEditorForm();
	virtual ~UILayoutEditorForm();

	virtual void Draw() override;
	// Runs the queued Show() on the primary thread, while the game thread is idle
	virtual void OnFrame() override;

	static void Create(int Tab);
	static void Destroy();
	static bool IsActive();
	static bool IsModified();

	// Global commands while the tab is active
	static bool Save();
	static bool Open();
	static bool Undo(bool Redo);
	static bool DeleteSelection();

	// Opens a saved working widget in the tab and switches to it
	static bool OpenWidget(const char* LayoutName);
	// Calls <LayoutName>.Show() in the next frame of Play In Editor
	static void ShowInGame(const char* LayoutName);

private:
	struct STextureRegion
	{
		xr_string File;
		Fvector4 Rect = { 0.0f, 0.0f, 0.0f, 0.0f };
	};

	struct STextureCache
	{
		ref_texture Texture;
		ImVec2 UV0 = { 0.0f, 0.0f };
		ImVec2 UV1 = { 1.0f, 1.0f };
		bool Valid = false;
	};

	enum class EDragMode : u8
	{
		None,
		Move,
		Resize,
		Pan
	};

	void DrawToolbar();
	void DrawPalette();
	void DrawHierarchy();
	void DrawHierarchyNode(u32 Id);
	void DrawWidgetContextMenu(u32 Id);
	void DrawCanvas();
	void DrawWidget(ImDrawList* DrawList, const SLayoutWidget& Widget, const Fvector2& AbsPos, const Fvector2& Size);
	void DrawProperties();
	// Fills the shared properties form for the selection. Values point into Doc,
	// so it is rebuilt whenever the widget array may be replaced or reordered
	void RebuildProperties();
	void OnPropsModified();
	void OnNameChange(PropValue* Value);
	void OnLayoutNameChange(PropValue* Value);
	void OnTypeChange(PropValue* Value);
	void OnHandlerChange(PropValue* Value);
	void OnFontChange(PropValue* Value);
	void OnCenterClick(ButtonValue* Value, bool& IsModified, bool& Safe);
	void OnTextureClick(ButtonValue* Value, bool& IsModified, bool& Safe);
	void OnLocalizeClick(ButtonValue* Value, bool& IsModified, bool& Safe);
	void DrawCodePreview();
	xr_string GetFieldClass(const xr_string& Field);
	// Puts the generated region of the layout into the script text
	void SyncScript();
	// Takes the hand edited xml into the layout. False on xml errors
	bool ApplyXmlEdit();
	// Opens the script on the event handler of the widget, adding the callback when needed
	void GoToHandler(u32 Id);
	void DrawTexturePicker(SLayoutWidget& Widget);
	void DrawNameDialog();
	// Registers the widget text in the string tables and puts the new id into the widget
	void OpenLocalization(const SLayoutWidget& Widget);

	void HandleCanvasInput(const ImVec2& CanvasPos, const ImVec2& CanvasSize, bool Hovered);
	void HandleDrop(const ImVec2& CanvasPos);
	void FitCanvas(const ImVec2& CanvasSize);

	ImVec2 ToScreen(const Fvector2& Point) const;
	Fvector2 ToLayout(const ImVec2& Point) const;
	float Snap(float Value) const;
	// Topmost visible widget under the point, 0 when none
	u32 PickWidget(const Fvector2& Point) const;
	// Geometry as it is saved to the selected xml
	void GetPreviewRect(const SLayoutWidget& Widget, Fvector2& AbsPos, Fvector2& Size) const;

	void LoadTextureDescriptions();
	const STextureCache* GetTexture(const xr_string& Id);

	void PushUndo();
	void MarkModified();
	void Select(u32 Id);
	void AddWidget(ELayoutWidget Type, u32 Parent, const Fvector2* AbsPos);
	void DeleteSelected();
	void NewLayout();
	bool OpenLayout(const char* LayoutName);
	// FileName is relative to configs: ui\ui_mm_main.xml
	bool ImportLayout(const char* FileName);
	bool SaveLayout();

	static UILayoutEditorForm* Form;

	CUILayoutDocument Doc;
	u32 Selected = 0;
	bool Modified = false;

	xr_vector<CUILayoutDocument> UndoStack;
	xr_vector<CUILayoutDocument> RedoStack;

	// Canvas view
	ImVec2 Pan = { 0.0f, 0.0f };
	ImVec2 CanvasOrigin = { 0.0f, 0.0f };
	float Zoom = 1.0f;
	bool NeedFit = true;
	bool WidePreview = false;
	bool ShowGrid = true;
	bool ShowBounds = true;
	int GridStep = 8;

	// Canvas drag
	EDragMode DragMode = EDragMode::None;
	// Resize handle: bit 0 left, 1 right, 2 top, 3 bottom
	u32 DragEdges = 0;
	ImVec2 DragMouseStart = { 0.0f, 0.0f };
	Fvector2 DragPosStart = { 0.0f, 0.0f };
	Fvector2 DragSizeStart = { 0.0f, 0.0f };
	ImVec2 DragPanStart = { 0.0f, 0.0f };
	bool DragChanged = false;

	// Texture ids from configs\ui\textures_descr
	xr_hash_map<xr_string, STextureRegion> TextureRegions;
	xr_vector<xr_string> TextureIds;
	xr_hash_map<xr_string, STextureCache> TextureCache;
	bool TexturesLoaded = false;
	char TextureFilter[64] = {};

	// Open / Save As name input
	bool NameDialogOpen = false;
	bool NameDialogSave = false;
	// Lists the stock configs\ui windows instead of the layouts
	bool NameDialogImport = false;
	// Long enough for the configs\ui paths of the import
	char NameBuffer[256] = {};
	xr_vector<xr_string> ExistingLayouts;

	// Properties: the undo snapshot is the state before an edit starts
	xr_unique_ptr<UIPropertiesForm> Props;
	CUILayoutDocument PropsSnapshot;
	bool PropsEditing = false;
	bool PropsDirty = true;
	u32 PropsWidget = u32(-1);
	u32 PropsFont = 0;
	bool TexturePickerRequest = false;

	// Code views: the xml can be edited and applied, the script is edited in place
	xr_unique_ptr<TextEditor> XmlView;
	xr_unique_ptr<TextEditor> ScriptView;
	xr_string XmlViewText;
	int XmlViewTab = -1;
	bool XmlEdited = false;
	// The whole script with the user's code, saved as is with the generated region updated
	xr_string ScriptSource;
	xr_string ScriptViewText;
	bool ScriptRegionLost = false;
	int ScriptJumpLine = -1;
	bool PreviewTabRequest = false;
	CUIDialogLocalization Localization;

	int PreviewTab = 0;

	CLuaCompletion Completion;
	u32 ScriptLanguageGeneration = 0;

	CCodeSearch Search;
	xr_string StatusText;

	// Layout whose Show() runs in the next frame
	xr_string PendingShow;};
