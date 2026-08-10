#pragma once

class CEditableObject;
class CSurface;

class CUIUVView :
	public IEditorWnd
{
	ref_texture Texture;
	DXTUtils::ImageInfo SrcData;

	CEditableObject* SelectedObject;
	CSurface* CurrentSurface;

	float Zoom;
	ImVec2 PanOffset;
	bool NeedUpdate;

public:
	CUIUVView();
	virtual ~CUIUVView();

	virtual void Draw() override;

	void Show(bool State);
	void SetSurface(CSurface* surf, CEditableObject* obj);

private:
	void UpdateTexture();
	void DrawUVWireframe(ImDrawList* drawList, const ImVec2& origin, const ImVec2& drawSize, float scale, const ImVec2& texSize);
};