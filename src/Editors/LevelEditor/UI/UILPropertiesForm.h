#pragma once
class UILPropertiesForm :
	public IEditorWnd
{
public:
	UILPropertiesForm();
	virtual ~UILPropertiesForm();
	virtual void Draw();
	IC void Open() { bOpen = true; }
	IC void Close() { bOpen = false; }

public:
	volatile bool PropUpdateIsCompleted = true;
};