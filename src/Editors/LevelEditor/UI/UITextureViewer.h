#pragma once

class CUITextureViewer :
	public IEditorWnd
{
public:
	CUITextureViewer();
	virtual ~CUITextureViewer();
	virtual void Draw();

	void DrawView();

	IC void Open() { bOpen = true; }
	IC void Close() { bOpen = false; }

	void LoadFromFile(const xr_path& File);

	void UpdateTexture();

private:
	void DrawToolbar();
	void DrawCanvas(const ImVec2& Size);
	void DrawStatusBar();
	void SetZoom(float NewZoom);

	ref_texture Texture;
	DXTUtils::ImageInfo SrcData;

	enum EChannel : u8
	{
		Channel_R = 1 << 0,
		Channel_G = 1 << 1,
		Channel_B = 1 << 2,
		Channel_A = 1 << 3
	};

	xr_string CurrentFileName;

	float Zoom = 1.f;
	// Offset of the image center from the canvas center, in screen pixels
	ImVec2 Pan = { 0.f, 0.f };
	bool FitToView = true;
	bool ShowChecker = true;
	bool ShowPixelGrid = false;
	// Source pixel under the cursor, -1 when the cursor is off the image
	int HoveredX = -1;
	int HoveredY = -1;

	u8 ChannelMask = Channel_R | Channel_G | Channel_B | Channel_A;
	bool GrayMode = false;
};
