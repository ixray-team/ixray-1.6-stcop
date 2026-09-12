//---------------------------------------------------------------------------
#pragma once

class ECORE_API UIMinimapEditorForm:
	public IEditorWnd
{
private:
	struct Element {
		xr_string name;
		ImVec2 position;
		ImVec2 RenderSize;
		ImVec2 FileSize;
		ref_texture Texture;
		xr_string TexturePath;
		
		bool EdSelected = false;
		bool EdLocked = false;
		bool EdHidden = false;
	};

	template<typename T>
	T Clamp(T value, T min, T max) {
		if (value < min) return min;
		if (value > max) return max;
		return value;
	}

	enum EdMode
	{
		None =0,
		Move,
		Resize,
	};
public:
	UIMinimapEditorForm();
	virtual ~UIMinimapEditorForm();
	virtual void Draw();

public:
	static void Update();
	static void Show();

private:
	static UIMinimapEditorForm* Form;

private:
	void LoadBGClick(const xr_string = "");
	int LoadTexture(Element&, const xr_string = "");

	void ShowMenu();
	void RenderCanvas();

	void ShowElementList();
	void ShowPreview();
	void UnselectAllElements();
	void SelectElement(Element&);

	void CreateElementPopup();

	void SaveFile(bool);
	void OpenFile();

	void ReloadMapInfo(const xr_string& fn);
	void ReloadLevelsList();

	bool GetTextureFromLevelLtx(const xr_string, xr_string&);

	xr_vector<Element> MapElements;
	xr_vector<xr_string> levels;

	bool m_DebugView = false;

	bool m_AlwaysDrawBorder = false;
	bool isDragging = false;
	bool PreviewMode = false;

	float m_Zoom = 1.0f;
	int m_ItemsOpacity = 255;

	ImVec2 initial_bg_position;
	ImVec2 initial_mouse_pos;
	
	Element* selectedElement=nullptr;

	xr_string ActiveFile;
	xr_string ActiveFileShort;
	bool isEdited;

	Element CreatingData;
	bool showMPLevels = false;

	EdMode m_EditMode = None;
	int m_saveResizeMode	= 0;
	int m_saveResizeOr		= 0;

	//bound
	void RenderBoundCanvas();
	bool BoundRectMode = false;
	bool BoundEditMode = false;
	float m_BZoom = 1.0f;
	ImVec2		m_BoundBackgroundPosition{ 0,0 };
	Fvector4	m_Bound { 0,0,0,0};
private:
	ref_texture BackgroundTexture;
	ref_texture TextureRemove;
	//
	ImVec2		m_BackgroundPosition;
	ImVec2		BackgroundRenderSize;
	ImVec2		BackgroundSize;
	//
	U32Vec      ImageData;
	xr_string	BackgroundTexturePath;
	bool		m_mp_mode = false;
};