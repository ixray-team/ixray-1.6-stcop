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
inline int UIToolBarSize = 24;

class XREUI_API XrUIManager
{
public:
	XrUIManager();
	void PushBegin(XrUI*ui,bool need_deleted =true);
	void Push(XrUI*ui,bool need_deleted =true);
	void Draw();
		
	virtual ~XrUIManager();

	void Initialize(HWND hWnd, IDirect3DDevice9* device,const char*ini_path);
	void Destroy();

	bool ProcessEvent(void* Event);

	void BeginFrame();
	void EndFrame();


	void ResetBegin();
	void ResetEnd(void* NewDevice);
	virtual bool 	ApplyShortCut(DWORD Key, TShiftState Shift)=0;

	inline float GetMenuBarHeight()const { return m_MenuBarHeight; }
	inline TShiftState GetShiftState()const { return m_ShiftState; };
	virtual bool IsPlayInEditor() { return false; }

protected:
	virtual void OnDrawUI();

public:
	void ApplyShortCutInput(DWORD Key);

private:
	float m_MenuBarHeight;
	TShiftState m_ShiftState;
	xr_vector<XrUI*> m_UIArray;
	string_path m_name_ini;

public: 
	template<typename T> 
	IC bool HasWindow() const
	{
		return std::any_of
		(
			m_UIArray.begin(), m_UIArray.end(),
			[](XrUI* Form)
			{
				return smart_cast<T*>(Form);
			}
		);
	}

	bool IsEnableInput = true;
};

