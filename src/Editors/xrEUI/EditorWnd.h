#pragma once
#include "stdafx.h"
class XREUI_API IEditorWnd
{
public:
	IEditorWnd() :bOpen(true) {}
	enum FUI
	{
		F_NoDelete=1,
	};
	Flags32 Flags;
	virtual ~IEditorWnd();
	/*ÓÄÀËÈÒÜ ÅÑËÈ ÍÅ ÏÐÈÃÎÄÈÒÑß*/
	virtual void ResetBegin();
	virtual void ResetEnd();
	bool IsClosed()const { return !bOpen; }

public:
	void BeginDraw() const;
	virtual void Draw() = 0;
	void EndDraw() const;

protected:
	bool bOpen;
	bool IsDocked = true;
	bool IsFocused = false;
	bool IsContextMenu = false;
};

