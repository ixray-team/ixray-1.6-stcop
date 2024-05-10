#pragma once
class UIWorldPropertiesFrom :
	public XrUI
{
public:
	UIWorldPropertiesFrom();
	virtual ~UIWorldPropertiesFrom();
	virtual void Draw();
	IC void Open() { bOpen = true; }
	IC void Close() { bOpen = false; }
};