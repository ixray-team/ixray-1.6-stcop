#pragma once

class CUIThmProperties:
	public IEditorWnd
{
public:
	void Load(const xr_path& File);
	void Show();

	virtual void Draw() override;

private:
	static void OnPropChange(PropValue* prop);

private:
	class EImageThumbnail* ThmPtr = nullptr;
	UIPropertiesForm mProps;

	xr_string FileName;
};