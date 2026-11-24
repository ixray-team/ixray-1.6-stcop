#pragma once
#ifndef IMGUI_API
#include <imgui.h>
#endif

struct SDL_Window;

namespace ImGui
{
	extern ENGINE_API ImFont* LightFont;
	extern ENGINE_API ImFont* RegularFont;
	extern ENGINE_API ImFont* MediumFont;
	extern ENGINE_API ImFont* BoldFont;
}

extern ENGINE_API float psImGuiScale;
extern ENGINE_API int psImGuiDockWindow;
extern ENGINE_API int psImGuiGameWindow;
// What the game's picture takes of the Game window: all of it, the largest part of a set shape, or a set size shown
// scaled into it
extern ENGINE_API int psGameViewMode;
extern ENGINE_API float psGameViewAspect;
extern ENGINE_API int psGameViewWidth;
extern ENGINE_API int psGameViewHeight;

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
	CTimer PlatformTimer;
	bool MouseLook = false;
	ImGuiStyle BaseStyle;
	float AppliedScale = 0.0f;
	ImVec2 MouseLookDelta = ImVec2(0.0f, 0.0f);
	u8 MouseLookSkip = 0;

public:
	ImGuiCallback PlatformInitCallback = nullptr;
	ImGuiCallback PlatformDestroyCallback = nullptr;
	ImGuiCallback PlatformNewFrameCallback = nullptr;
	ImGuiCallback HiddenCallback = nullptr;

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
	// The two halves of Render and of AfterRender: what the tools build and what their windows are, on the thread that
	// owns the window; what is drawn of them, on the thread that owns the graphics device
	void Build();
	void Draw();
	void UpdatePlatform();
	void RenderPlatform();

	void UpdateCapture();
	bool IsCapturingInputs() const;
	bool IsFocused() const;
	void SetMouseLook(bool IsEnabled);
	bool IsMouseLook() const;
	bool IsButtonHeld(bool IsMiddle) const;
	void AddMouseLook(float X, float Y);
	ImVec2 TakeMouseLook();

	void NewPlatformFrame() const;
	void NewHardwareFrame() const;

	bool IsGUIRendering() const;

	// The game's picture is a window among the tools (imgui_game_window), not the whole main window behind them
	bool IsGameViewWindow() const;
	// The mouse inside the picture, 0 to 1 a side
	ImVec2 GetGameViewMouse() const;
	// The mouse is over the picture and no tool window is over it there; the Game window itself does not count
	bool IsGameViewHovered() const;
	// The colour of the game's picture under the mouse, without the tools drawn over it
	bool ReadGameViewColor(float (&Color)[3]) const;
	// Where the picture is on the screen, in ImGui's coordinates: set by what draws it, every frame
	// The size the frame draws at for the Game window, no larger than the targets. False while the picture fills the
	// main window
	bool FitGameView(u32 MaxWidth, u32 MaxHeight, u32& Width, u32& Height) const;

	// Where the game's picture is on the screen, in ImGui's coordinates: the Game window's image, or the main
	// window's corner and the view's size. A tool that turns the mouse into a ray or draws over the picture
	// takes this, not the main viewport
	struct GameViewRect
	{
		ImVec2 Pos;
		ImVec2 Size;
	};

	GameViewRect GetGameViewRect() const { return { GameViewPos, GameViewSize }; }

	// The viewport and the OS window that show the picture: the Game window's own when it is out of the main
	// window, else the main ones. The game's keyboard focus, mouse grab and cursor warps belong to that window,
	// and what draws over the picture draws into that viewport
	ImGuiViewport* GetGameViewport() const;
	SDL_Window* GetGameWindow() const;

	// The room the Game window had for the picture when it was last drawn; zero while it is not shown
	ImVec2 GameViewRoom = ImVec2(0.0f, 0.0f);
	ImVec2 GameViewPos = ImVec2(0.0f, 0.0f);
	ImVec2 GameViewSize = ImVec2(0.0f, 0.0f);
	ImGuiID GameViewportId = 0;
	u32 GameWindowId = 0;

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
