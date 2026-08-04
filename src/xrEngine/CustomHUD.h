#pragma once

ENGINE_API extern Flags32		psHUD_Flags;
#define HUD_CROSSHAIR			(1<<0)
#define HUD_CROSSHAIR_DIST		(1<<1)
#define HUD_WEAPON				(1<<2)
#define HUD_INFO				(1<<3)
#define HUD_DRAW				(1<<4)
#define HUD_CROSSHAIR_RT		(1<<5)
#define HUD_WEAPON_RT			(1<<6)
#define HUD_CROSSHAIR_DYNAMIC	(1<<7)
#define HUD_CROSSHAIR_RT2		(1<<9)
#define HUD_DRAW_RT				(1<<10)
#define HUD_WEAPON_RT2			(1<<11)
#define HUD_DRAW_RT2			(1<<12)
#define HUD_CROSSHAIR_POINT		(1<<13)
#define HUD_MINIMAP				(1<<14)
#define HUD_HIDE_QUICK_SLOTS	(1<<15)
#define HUD_CONTEXTUAL_STATUS	(1<<16)

IC bool HUD_IsQuickSlotsAutoHide()
{
	return psHUD_Flags.test(HUD_HIDE_QUICK_SLOTS) || psHUD_Flags.test(HUD_CONTEXTUAL_STATUS);
}

class ENGINE_API IRender_Visual;
class CUI;

class ENGINE_API CCustomHUD:
	public DLL_Pure,
	public IEventReceiver,
	public pureScreenResolutionChanged
{
public:
					CCustomHUD				();
	virtual			~CCustomHUD				();

	virtual		void		Render_First			(){;}
	virtual		void		Render_Last				(){;}

	virtual		void		OnFrame					(){;}
	virtual		void		OnFrameMT				(){;}
	virtual		void		OnEvent					(EVENT E, u64 P1, u64 P2){;}

	virtual		void		Load					(){;}
	virtual		void		OnDisconnected			()=0;
	virtual		void		OnConnected				()=0;
	virtual		void		RenderActiveItemUI		()=0;
	virtual		bool		RenderActiveItemUIQuery	()=0;
	virtual		void		net_Relcase				(CObject *object) = 0;
};

extern ENGINE_API CCustomHUD* g_hud;