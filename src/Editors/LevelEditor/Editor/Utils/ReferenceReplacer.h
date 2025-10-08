#pragma once

class UIReferenceReplacer :
	public IEditorWnd
{
public:
	UIReferenceReplacer();
	virtual ~UIReferenceReplacer();
	virtual void Draw();

	void UpdateReferences();

private:
	xr_set<xr_string> SceneReferences;

	string256 InputFile = {};
	string256 OutputFile = {};
	bool Selected = false;
};