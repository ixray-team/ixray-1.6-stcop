#pragma once
#include "ui_base.h"

class UI_API CUIStatic;
class UI_API CUI3dStatic;

enum class UICursorAction : u32
{
	Select, Clickable, Drag, ScrollDrag, HoverSlotDrop, ClickLmb, ClickRmb, ScrollUp, ScrollDown, Drop, ScrollDrop, Idle, Context, Count
};

class UI_API CUICursor :
	public pureRender,
	public pureScreenResolutionChanged
{
	bool			bVisible;
	Fvector2		vPos;
	Fvector2		vPrevPos;
    CUI3dStatic*    m_3dstatic;
	void			InitInternal				();
	struct ActionBinding
	{
		bool IsEnabled = false;
		shared_str Section;
	};
	ActionBinding Bindings[static_cast<u32>(UICursorAction::Count)];
	xrCriticalSection AnimationGuard;
	bool AreAnimationsEnabled = false;
	bool AreAutomaticSectionsPaused = false;
	bool IsTextureChangePending = false;
	shared_str TextureOverride;
	shared_str DefaultTexture;
	shared_str TextureShader;
	void ApplyCursorTexture();
	u32 ActionMask = 0;
	Fvector2 Hotspot = {0.0f, 0.0f};
	Fvector2 OriginalTextureOffset = {0.0f, 0.0f};
	Ivector2 IdleInterval = {15000, 35000};
	u32 NextIdleTime = 0;
	u32 IdleRandomState = 1;
	u32 PulseEndTime = 0;
	bool IsPulseLooping = false;
	UICursorAction Pulse = UICursorAction::Count;
	shared_str SelectedSection;
	xr_vector<const void*> ContextOwners;
	const void* HoverOwner = nullptr;
	bool HasHoveredItem = false;
	bool IsHoveredButton = false;
	bool IsHoveredSlot = false;
	bool CanDropOnHover = false;
	bool HoverWasDragging = false;
	const void* HoveredItemKey = nullptr;
	const void* PressedItemKey = nullptr;
	bool IsLeftPressed = false;
	bool IsNativeDragging = false;
	bool IsWidgetDragging = false;
	u32 WidgetDragFrame = u32(-1);
	bool IsHoldGrab = false;
	u32 LeftPressTime = 0;
	u32 HoldGrabDelay = 1000;
	struct InventoryHover
	{
		const void* Owner = nullptr;
		bool HasItem = false;
		bool IsButton = false;
		bool IsSlot = false;
		bool CanDrop = false;
		const void* Item = nullptr;
	};
	InventoryHover PendingInventoryHover;
	u32 InventoryHoverFrame = u32(-1);
	void ApplyInventoryHover(const void* Owner, bool IsOver, bool HasItem, bool IsSlot, bool CanDrop, const void* Item, bool IsButton);
	void ConfigureAnimations();
	void RefreshAnimation();
	void ResetIdleTimer();
	void PulseAnimation(UICursorAction Action);
	void UpdateAnimation();
public:
	void SetCursorTexture(const char* Texture);
	void SetAutomaticSectionsPaused(bool IsPaused);
	bool AreCustomAnimationsEnabled() const { return AreAnimationsEnabled; }
					CUICursor					();
	virtual			~CUICursor					();
	virtual void	OnRender					();
	
	Fvector2		GetCursorPositionDelta		();

	Fvector2		GetCursorPosition			();
	void			SetUICursorPosition			(Fvector2 pos);
	void			UpdateCursorPosition		(int _dx, int _dy);
	virtual void	OnScreenResolutionChanged	();

	bool			IsVisible					() {return bVisible;}
	void			Show();
	void Hide();
	void SetDragging(bool IsDragging);
	void ReportWidgetDrag();
	void SetInventoryHover(const void* Owner, bool IsOver, bool HasItem, bool IsSlot = false, bool CanDrop = false, const void* Item = nullptr, bool IsButton = false);
	void OnInventoryItemPressed(const void* Item);
	void SetContextMenu(const void* Owner, bool IsOpen);
	void OnMouseButton(int Button, bool IsPressed);
	void OnMouseWheel(int Direction);
	void OnInputActivity();
	CUIStatic*		m_static_text;
	CUIStatic*		m_static;
	float			m_width_initial = 0.0f;
};
