#pragma once
#include <imgui.h>

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

public:
	ImGuiCallback PlatformInitCallback = nullptr;
	ImGuiCallback PlatformDestroyCallback = nullptr;
	ImGuiCallback PlatformNewFrameCallback = nullptr;

	ImGuiCallback HardwareInitCallback = nullptr;
	ImGuiCallback HardwareResetCallback = nullptr;
	ImGuiCallback HardwareDestroyCallback = nullptr;
	ImGuiCallback HardwareNewFrameCallback = nullptr;
	ImGuiCallback HardwareDrawDataCallback = nullptr;

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

	void BeginRender();
	void Render();
	void AfterRender();

	void UpdateCapture();
	bool IsCapturingInputs() const;

	void NewPlatformFrame() const;
	void NewHardwareFrame() const;

public:
	static CImGuiManager& Instance();

	enum ERenderPriority
	{
		eHight = 1,
		eMedium = 24,
		eLow = 48
	};

private:
	void LoadImGuiFont(ImFont*& FontHandle, const char* Font);
};