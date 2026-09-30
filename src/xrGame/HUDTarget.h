#pragma once

#include "HUDCrosshair.h"
#include "../xrCore/Collision/xr_collide_defs.h"


class CHUDManager;
class CLAItem;

struct SPickParam
{
	collide::rq_result RQ;
	float				power;
	u32					pass;
};

class CHUDTarget 
{
	ui_shader				hShader;
	float					accumulatedTime;
	SPickParam				PP;

	bool					m_bShowCrosshair;
	CHUDCrosshair			HUDCrosshair;

	u32						colorEnemy;
	u32						colorFriend;
	u32						colorNeutral;
	u32						colorDefault;
	CGameFont*				targetFont;
	bool					bInitialized;

	collide::rq_results	RQR;

public:
	static constexpr float NEAR_LIM = .5f;
	static constexpr float C_SIZE = .025f;
	static constexpr float PICKUP_DISTANCE = 2.f;
	static constexpr float SHOW_INFO_SPEED = 1.5f;
	static constexpr float HIDE_INFO_SPEED = 10.f;

							CHUDTarget	();
							~CHUDTarget	();
	void					CursorOnFrame ();
	void					Render		();
	void					Load		();
	collide::rq_result&		GetRQ		() {return PP.RQ;};
	float					GetRQVis	() {return PP.power;};
	CHUDCrosshair&			GetHUDCrosshair	() {return HUDCrosshair;}
	void					ShowCrosshair	(bool b);
	void					net_Relcase		(CObject* O);
};
