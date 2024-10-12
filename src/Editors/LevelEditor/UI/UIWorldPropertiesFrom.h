#pragma once
class UIWorldPropertiesFrom :
	public IEditorWnd
{
public:
	UIWorldPropertiesFrom();
	virtual ~UIWorldPropertiesFrom();
	virtual void Draw();
	IC void Open() { bOpen = true; }
	IC void Close() { bOpen = false; }
};