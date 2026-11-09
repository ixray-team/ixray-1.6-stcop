#pragma once
#ifndef IMGUI_API
#include <imgui.h>
#endif

namespace ImGui
{
	extern ENGINE_API ImFont* LightFont;
	extern ENGINE_API ImFont* RegularFont;
	extern ENGINE_API ImFont* MediumFont;
	extern ENGINE_API ImFont* BoldFont;
}

class ENGINE_API CImGuiManager
{
private:
	using ImGuiCallback = std::function<void()>;

	struct DrawCommand
	{
		shared_str Name;
		ImGuiCallback Function;
	};

	xr_map<u32, DrawCommand> RenderFrameData;
	xr_vector<IReader*> ImGuiFontsPtr;

	bool DrawUIRender = true;
	bool CaptureInputs = false;
	bool HasPlatformWindowFocus = false;
	bool MouseLook = false;
	ImVec2 MouseLookDelta = ImVec2(0.0f, 0.0f);

public:
	ImGuiCallback PlatformInitCallback = nullptr;
	ImGuiCallback PlatformDestroyCallback = nullptr;
	ImGuiCallback PlatformNewFrameCallback = nullptr;

public:
	CImGuiManager() = default;
	~CImGuiManager() = default;

	void InitPlatform();
	void InitHardware();
	void Reset();
	void Destroy(bool HardwareOnly = false);

	void ApplyMainViewport(ImGuiCallback RenderFunction);
	void Subscribe(shared_str Name, u32 RenderPriority, ImGuiCallback&& RenderFunction);
	void Unsubscribe(shared_str Name);
	void UnsubscribeAll();

	void BeginRender();
	void Render();
	void AfterRender();

	void UpdateCapture();
	bool IsCapturingInputs() const;
	bool IsFocused() const;
	void SetMouseLook(bool IsEnabled);
	bool IsMouseLook() const;
	void AddMouseLook(float X, float Y);
	ImVec2 TakeMouseLook();

	void NewPlatformFrame() const;
	void NewHardwareFrame() const;

	bool IsGUIRendering() const;

public:
	static CImGuiManager& Instance();

	enum ERenderPriority
	{
		eHight = 1,
		eMedium = 24,
		eLow = 48
	};

private:
	void LoadImGuiFont(ImFont*& FontHandle, const char* Font, const float scale);
};
