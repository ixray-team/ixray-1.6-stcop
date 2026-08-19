//---------------------------------------------------------------------------
#pragma once

class UIIconPicker :public IEditorWnd
{
public:
	UIIconPicker();
	virtual ~UIIconPicker();
	virtual void Draw();
public:
	static void Update();
	static void Show(const xr_string);
private:
	FS_FileSet	texture_map;

	xr_hash_map<xr_string, xr_string> Icons;
	static UIIconPicker* Form;
	xr_string file_path;
	
private:
	void InitItemList();
	void HideLib();
	bool ShowIcons();
	void InitPreviewTexture(const xr_string&, const string_path&);
};
