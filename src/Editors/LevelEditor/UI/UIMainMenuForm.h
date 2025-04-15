#pragma once
class UIMainMenuForm :public IEditorWnd
{
public:
	UIMainMenuForm();
	virtual ~UIMainMenuForm();
	virtual void Draw();
	void ExportLevelAsArchive();

private:
	ref_texture PlugPy;
	ref_texture PlugLua;
};

