#pragma once
#include "../../../xrECore/Editor/MaterialPreview.h"

class CUIThmProperties:
	public IEditorWnd
{
public:
	CUIThmProperties();

	void Load(const xr_path& File);
	void Show();
	// Releases the GPU resources of the preview, before the device goes away
	void ClearPreview() { Preview.Clear(); }

	virtual void Draw() override;

private:
	static void OnPropChange(PropValue* prop);

private:
	class EImageThumbnail* ThmPtr = nullptr;
	UIPropertiesForm mProps;
	CMaterialPreview Preview;
	bool HasPreview = false;
	float PreviewHeight = 260.f;

	xr_string FileName;
};