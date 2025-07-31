#pragma once

class IPluginBase;

class CPluginUIRun :
	public IEditorWnd
{
	IPluginBase* InputPlugin;

public:
	CPluginUIRun(IPluginBase* Plug);

	virtual void Draw() override;
};