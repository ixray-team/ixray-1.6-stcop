#pragma once

#include "../../xrUI/ui_defs.h"
#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "inventory_space.h"

class CInventoryItem;
class CInventory;
class CUIXml;
class CUIGamepadLegend;
class CUIStatic;
class CUI3dStatic;

class CUIRadialMenu : public CUIDialogWnd
{
	typedef CUIDialogWnd		inherited;

public:
	struct TexturedRectDrawData 
	{
		float x = 0.f;
		float y = 0.f;
		float width = 0.f;
		float height = 0.f;
	};

public:
	CUIRadialMenu();
	virtual ~CUIRadialMenu() override = default;

	virtual void Init() {};
	virtual void Init(CUIXml* pXml);
	virtual void TryActivateSelectedSector() { }
	virtual void Show(bool status) override;

	virtual bool OnKeyboardAction(int dik, EUIMessages keyboard_action) override { return false;  };

	virtual bool StopAnyMove() override { return false; }
	virtual bool NeedCursor()const override { return false; }
	virtual bool ForceCursorInput() override { return true; }
	virtual bool NeedCenterCursor()const override { return false; }
	virtual bool OnGamepadStickAction(int key, Fvector2 value, EUIMessages gamepad_action) override;
	virtual bool OnMouseAction(float x, float y, EUIMessages mouse_action) override;
	virtual bool OnGyroscopeAction(Fvector3 value) override { return true; } // do not use gyroscope for radial menu
	virtual bool OnTouchpadAction(Fvector2 value) override;

	bool isInitialized = false;
	CUIGamepadLegend* m_pGamepadLegend = nullptr;

protected:
	virtual void OnActivateSectorClicked();
	
protected:
	enum eRadialMenuSndAction 
	{
		eSndOpen = 0,
		eSndClose,
		eSndSwitch,
		eSndSelect,
		eSndFireMode,
		eSndGrenadeMode,
		eSndMax
	};
	const u32 clrSlotIcon = 0xAAFFFFFF;
	const u32 clrSlotIconBlocked = 0x55FFFFFF;

	ref_sound	sounds[eSndMax];
	void		PlaySnd(eRadialMenuSndAction a);

	shared_str textureDefault;
	shared_str textureSelected;
	shared_str textureFocused;
	shared_str textureFocusedSelected;

	int selected_index = -1;
	bool bWaitForZeroRStick = false;

	struct RadialMenuItem
	{
		u32 slot = u32(-1);
		bool alwaysShowIcon = false;
		CUI3dStatic* icon = nullptr;
		CUIStatic* background = nullptr;
		Fvector2 defaultSize;
	};
	xr_vector<RadialMenuItem> slotList;

	u32 sectors_count;

	float sector;

	Fvector2 backgroundSize;
	Fvector2 iconSize;
	Fvector2 backgroundPivot;
};