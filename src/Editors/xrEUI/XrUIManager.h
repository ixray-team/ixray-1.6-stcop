#pragma once

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
	EDragDropType DnDType = EDragDropType::None;
	virtual void* LoadTexture(const char*) const { return nullptr; };
	void* SearchIcon = nullptr;
};

extern XREUI_API XrUIManager* GUIManager;