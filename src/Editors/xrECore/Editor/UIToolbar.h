#pragma once
#include "UI_ToolsCustom.h"

class ECORE_API CUIToolbar :
	public IEditorWnd
{

	bool bFocus = false;

	bool IsActiveAction = false;
	bool IsActiveAxis = false;
private:
	void CheckAction(ETAction Action);
	void CheckAxis(ETAxis Axis);
	void EndCheck();

public:
	virtual void OnCreate();
	virtual void Draw() override;
};
