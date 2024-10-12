//---------------------------------------------------------------------------
#pragma once

class UIMinimapEditorForm:
	public IEditorWnd
{
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
	ImTextureID					m_Texture; 
	ImTextureID					m_TextureRemove;

private:
	void LoadClick();
	U32Vec      m_ImageData;
	u32         m_ImageW;
	u32         m_ImageH;
	u32         m_ImageA;
	Fbox2       map_bb;
	Fbox2       map_bb_loaded;
	Ivector2    image_draw_size;
};