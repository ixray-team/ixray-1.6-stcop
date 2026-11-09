#pragma once

class IRHISurface;
class IRHIShaderResourceView;

enum TShiftState_
{
	ssNone = 0,
	ssShift = 1,
	ssLeft = 2,
	ssRight = 4,
	ssCtrl = 8,
	ssAlt = 16,
};
using TShiftState = int;
inline int UIToolBarSize = 28;

struct XrMessageBoxButton
{
	const char* Label = "";
	int Result = 0;
};

struct ImGuiContext;

class XREUI_API XrUIManager
{
public:
	XrUIManager();
	void PushBegin(IEditorWnd*ui,bool need_deleted =true);
	void Push(IEditorWnd*ui,bool need_deleted =true);
	void Remove(IEditorWnd*ui);
	void Draw();
		
	virtual ~XrUIManager();

	void Initialize(HWND hWnd, const char*ini_path);
	void Destroy();

	bool ProcessEvent(void* Event);

	void BeginFrame();
	void EndFrame();
	void MDIUpdate();

	void ResetBegin();
	void ResetEnd(void* NewDevice);
	virtual bool ApplyShortCut(u32 Key, TShiftState Shift) = 0;

	inline float GetMenuBarHeight()const { return m_MenuBarHeight; }

	inline float GetMenuBarButtonHeight()const { return m_MenuBarButtonHeight; }


	inline TShiftState GetShiftState()const { return m_ShiftState; };
	virtual bool IsPlayInEditor() { return false; }
	void ApplyShortCutInput(DWORD Key);

	// Blocks in its own loop and draws the box with the editor UI, so it stays on top of the editor window.
	// Enter picks DefaultResult, Escape picks CancelResult. Returns -1 when it cannot be shown
	// (not the main thread, no swapchain, another box is open): the caller falls back to a system box
	int ShowModalMessage(const char* Title, const char* Text, const XrMessageBoxButton* Buttons, u32 Count, int DefaultResult, int CancelResult);

	float GetScaleDpi() const { return m_ScaleDpi; }
	float ScaleByDpi(float value) const { return value * m_ScaleDpi; }

protected:
	virtual void OnDrawUI();

private:
	float m_MenuBarHeight;
	float m_MenuBarButtonHeight;
	float m_ScaleDpi;
	
	TShiftState m_ShiftState;
	xr_atomic_bool Rendering = false;

	xr_vector<IEditorWnd*> ActualWindows;
	xr_vector<IEditorWnd*> NextWindows;

	string_path m_name_ini;

	// The message box loop can run inside a frame of the main context: it draws with a context of its own
	ImGuiContext* ModalContext = nullptr;
	bool InModalMessage = false;

	// Copy of the last editor frame: the message box draws it as a background
	IRHISurface* FrameSnapshot = nullptr;
	IRHIShaderResourceView* FrameSnapshotSRV = nullptr;
	bool HasFrameSnapshot = false;

	void CaptureFrameSnapshot();
	void ReleaseFrameSnapshot();

public: 
	template<typename T> 
	IC bool HasWindow() const
	{
		return std::any_of
		(
			ActualWindows.begin(), ActualWindows.end(),
			[](IEditorWnd* Form)
			{
				return smart_cast<T*>(Form);
			}
		);
	}

	bool IsEnableInput = true;
	int ActiveTabIndex = 0;
	// Notifications drawn over every window
	UIToastQueue Toasts;
	// Tab the top bar selects on its next draw (-1 = none)
	int RequestedTabIndex = -1;
	virtual void* LoadTexture(const char*) const { return nullptr; };
	void* SearchIcon = nullptr;
};

extern XREUI_API XrUIManager* GUIManager;