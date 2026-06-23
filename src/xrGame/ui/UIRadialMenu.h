#pragma once

#include "../../xrUI/ui_defs.h"
#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "inventory_space.h"

void draw_arc(ui_shader& shader, float x, float y, float radius, float radius_inside, float angle1, float angle2, u32 color1, u32 color2, u32 quality = 42);

class CInventoryItem;
class CInventory;
class CUIXml;
class CUIGamepadLegend;

class CUIRadialMenu : public CUIDialogWnd, public pureScreenResolutionChanged
{
	typedef CUIDialogWnd		inherited;

public:
	struct TexturedRectDrawData {
		u32 x = 0;
		u32 y = 0;
		u32 side = 0;
	};

public:
	CUIRadialMenu();
	virtual ~CUIRadialMenu();

	virtual void OnScreenResolutionChanged();
	virtual void Init() {};
	virtual void Init(CUIXml* pXml);
	virtual void TryActivateSelectedSector() { }
	virtual void Show(bool status);

	virtual bool OnKeyboardAction(int dik, EUIMessages keyboard_action) { return false;  };

	virtual bool StopAnyMove() { return false; }
	virtual bool NeedCursor()const { return false; }
	virtual bool ForceCursorInput() { return true; }
	virtual bool NeedCenterCursor()const { return false; }
	virtual bool OnGamepadStickAction(int key, Fvector2 value, EUIMessages gamepad_action);
	virtual bool OnMouseAction(float x, float y, EUIMessages mouse_action);

	bool isInitialized = false;
	CUIGamepadLegend* m_pGamepadLegend;

protected:
	void RecheckSizes();
	virtual void OnActivateSectorClicked();
	
	void DrawItem(TexturedRectDrawData& trdd, CInventoryItem* iitem, u32 color_mask);

protected:
	enum eRadialMenuSndAction {
		eSndOpen = 0,
		eSndClose,
		eSndSwitch,
		eSndSelect,
		eSndMax
	};

	ref_sound	sounds[eSndMax];
	void		PlaySnd(eRadialMenuSndAction a);

	ui_shader* crosshair_shader;

	int selected_index = -1;
	bool bWaitForZeroRStick = false;

	//read from xml
	u32 slotsInSectors[LAST_SLOT + 1];
	int sectors_count;
	float starting_angle;
	float inner_radius_ratio;//radius / inner radius
	float selected_radius_factor;
	float safezone_height_factor;

	float center_x;
	float center_y;
	float radius;
	float inner_radius;
	float selected_radius;

	u32 screen_width;
	u32 screen_height;

	float gap;
	float sector;

	u32 deselected_color;
	u32 selected_color;
	u32 sector_inner_side_color;
	u32 sector_outer_side_color;

};