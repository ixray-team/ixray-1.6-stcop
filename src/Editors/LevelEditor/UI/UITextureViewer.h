#pragma once

class CUITextureViewer :
	public IEditorWnd
{
public:
	CUITextureViewer();
	virtual ~CUITextureViewer();
	virtual void Draw();

	IC void Open() { bOpen = true; }
	IC void Close() { bOpen = false; }

	void LoadFromFile(const xr_path& File);

	void UpdateTexture();

private:
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
	u8 ChannelMask = Channel_R | Channel_G | Channel_B | Channel_A;
	bool GrayMode = false;
};