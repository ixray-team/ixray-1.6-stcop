#pragma once

#include <functional>

class XREUI_API CUIThemeManager :
	public IEditorWnd
{
	CUIThemeManager();
	~CUIThemeManager() = default;

public:
	static CUIThemeManager& Get();
	void InitDefault(int ThemeID = -1);
	void Show(bool value);

	// xrECore hooks this to request a viewport redraw whenever any viewport color
	// (FogColor / ClearColor) is edited in the Theme Editor.
	using FOnViewportColorChanged = std::function<void()>;
	void SetOnViewportColorChanged(FOnViewportColorChanged Cb) { OnViewportColorChanged = std::move(Cb); }

protected:
	virtual void Draw();
	bool IsLoaded = false;

public:
	float TransparentDefault = 1.f;
	float TransparentUnfocused = 0.33f;

	int ThemeID = 0;

	// Viewport colors — owned by the theme system.
	// Format: 0xAARRGGBB (matches xrEngine's u32 color convention).
	// Defaults match the legacy DEFAULT_CLEARCOLOR (0x00555555)
	// so first-frame behavior is unchanged.
	// Consumers read these every frame — no cache anywhere.
	//   - ClearColor: CEditorRenderDevice::Clear(), RenderSceneFrame
	//   - FogColor:   CToolCustom::GetCurrentFog()
	u32 ClearColor = 0x00555555;
	u32 FogColor = 0x00555555;

	// St4lker0k765: customizable log message colors
	ImVec4 log_color_default;
	ImVec4 log_color_error;
	ImVec4 log_color_warning;
	ImVec4 log_color_debug;

private:
	FOnViewportColorChanged OnViewportColorChanged;
};