#pragma once
#include "../xrEUI/XrUI.h"

class XREPROPS_API CUFileOpen :
	public XrUI
{
public:
	using AfterLoadCallbackType = void(*)(const xr_string&);
	AfterLoadCallbackType AfterLoadCallback = nullptr;

public:
	CUFileOpen();
	void ShowDialog(const char* Path, const char* Filter);
	void Draw() override;
};