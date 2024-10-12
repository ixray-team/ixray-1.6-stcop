#pragma once

class XREUI_API CUIThemeManager :
	public IEditorWnd
{

	CUIThemeManager();
	~CUIThemeManager() = default;
	
public:
	static CUIThemeManager& Get();
	void InitDefault(bool Force = false);
	void Show(bool value);

	void Save();
	void Load();

protected:
	virtual void Draw();
	bool IsLoaded = false;

public:
	float TransparentDefault = 1.f;
	float TransparentUnfocused = 0.33f;
};