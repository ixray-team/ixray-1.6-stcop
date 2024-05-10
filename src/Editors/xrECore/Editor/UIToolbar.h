#pragma once

class ECORE_API CUIToolbar :
	public XrUI
{
	ref_texture Select;
	ref_texture Move;
	ref_texture Rotate;
	ref_texture Scale;

	bool bFocus = false;
public:
	virtual void OnCreate();
	virtual void Draw() override;
};