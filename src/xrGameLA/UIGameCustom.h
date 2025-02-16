#ifndef __XR_UIGAMECUSTOM_H__
#define __XR_UIGAMECUSTOM_H__
#pragma once

#include "script_export_space.h"
#include "../xrCore/object_interfaces.h"
#include "inventory_space.h"
#include "UIDialogHolder.h"
#include "../CustomHUD.h"
#include "ui/UIMessages.h"
#include "ui/UIWeatherEditor.h"

// refs
class CUI;
class CTeamBaseZone;
class game_cl_GameState;
class CUIDialogWnd;
class CUICaption;
class CUIStatic;
class CUIWindow;
class CUIXml;
class CUIInventoryWnd;
class CUITradeWnd;
class CUIPdaWnd;
class CUICarBodyWnd;
struct KillMessageStruct;
class CUIMainIngameWnd;
class CUIMessagesWindow;
class CUIWeatherEditor;

enum EGameTypes;

struct SDrawStaticStruct :public IPureDestroyableObject
{
	SDrawStaticStruct	();
	virtual	void	destroy			();
	CUIStatic*		m_static;
	float			m_endTime;
	shared_str		m_name;
	void			Draw();
	void			Update();
	CUIStatic*		wnd()		{return m_static;}
	bool			IsActual()	const;
	void			SetText		(LPCSTR);
};

struct SGameTypeMaps
{
	shared_str				m_game_type_name;
	EGameTypes				m_game_type_id;
	struct SMapItm{
		shared_str	map_name;
		shared_str	map_ver;
		bool operator ==(const SMapItm& other){return map_name==other.map_name && map_ver==other.map_ver;}
	};
	xr_vector<SMapItm>		m_map_names;
};

struct SGameWeathers
{
	shared_str				m_weather_name;
	shared_str				m_start_time;
};
typedef xr_vector<SGameWeathers>					GAME_WEATHERS;
typedef xr_vector<SGameWeathers>::iterator			GAME_WEATHERS_IT;
typedef xr_vector<SGameWeathers>::const_iterator	GAME_WEATHERS_CIT;

class CMapListHelper
{
	typedef xr_vector<SGameTypeMaps>	TSTORAGE;
	typedef TSTORAGE::iterator			TSTORAGE_IT;
	typedef TSTORAGE::iterator			TSTORAGE_CIT;
	TSTORAGE							m_storage;
	GAME_WEATHERS						m_weathers;

	void						Load			();
	SGameTypeMaps*				GetMapListInt	(const shared_str& game_type);
public:
	const SGameTypeMaps&		GetMapListFor	(const shared_str& game_type);
	const SGameTypeMaps&		GetMapListFor	(const EGameTypes game_id);
	const GAME_WEATHERS&		GetGameWeathers	();
};

extern CMapListHelper	gMapListHelper;

class CUIGameCustom :public DLL_Pure, public CDialogHolder
{
protected:
	CUIWindow*			m_window;
	CUICaption*			GameCaptions			() {return m_pgameCaptions;}
	CUICaption*			m_pgameCaptions;
	CUIXml*				m_msgs_xml;

	typedef xr_vector<SDrawStaticStruct*>	st_vec;
	typedef st_vec::iterator				st_vec_it;
	st_vec									m_custom_statics;

	bool				m_bShowGameIndicators;

public:
	CUIMainIngameWnd*	UIMainIngameWnd;
	CUIInventoryWnd*	m_InventoryMenu;
	CUIMessagesWindow*	m_pMessagesWnd;
	CUIPdaWnd*			m_PdaMenu;
	CUICarBodyWnd*		m_UICarBodyMenu;
	CUIWeatherEditor*	m_WeatherEditor;

	virtual	void		shedule_Update			(u32 dt) {};
	virtual void		SetClGame				(game_cl_GameState* g);
	
						CUIGameCustom			();
	virtual				~CUIGameCustom			();

	virtual	void		Init					(int stage)	{};
	
	virtual void		Render					();
	virtual void _BCL	OnFrame					();


	void			ShowGameIndicators		(bool b)			{m_bShowGameIndicators	= b;};
	bool			GameIndicatorsShown		()					{return m_bShowGameIndicators;};
	void			ShowCrosshair			(bool b)			{psHUD_Flags.set			(HUD_CROSSHAIR_RT, b);}
	bool			CrosshairShown			()					{return !!psHUD_Flags.test	(HUD_CROSSHAIR_RT);}
	
	virtual void		HideShownDialogs		(){};
	virtual void		ReInitShownUI			(){};
	virtual void		ReinitDialogs			(){};

	virtual void		AddCustomMessage		(LPCSTR id, float x, float y, float font_size, CGameFont *pFont, u16 alignment, u32 color);
	virtual void		AddCustomMessage		(LPCSTR id, float x, float y, float font_size, CGameFont *pFont, u16 alignment, u32 color, float flicker );
	virtual void		CustomMessageOut		(LPCSTR id, LPCSTR msg, u32 color);
	virtual void		RemoveCustomMessage		(LPCSTR id);

	SDrawStaticStruct*	AddCustomStatic			(LPCSTR id, bool bSingleInstance);
	SDrawStaticStruct*	GetCustomStatic			(LPCSTR id);
	void				RemoveCustomStatic		(LPCSTR id);

	void				CommonMessageOut		(LPCSTR text);

	IC CUIPdaWnd&		PdaMenu					() const { return *m_PdaMenu;   }

	virtual void		ChangeTotalMoneyIndicator(LPCSTR newMoneyString)		{};
	virtual void		DisplayMoneyChange		(LPCSTR deltaMoney)			{};
	virtual void		DisplayMoneyBonus		(KillMessageStruct* bonus)	{};

	virtual bool		OnKeyboardAction					(int dik, EUIMessages keyboard_action) {return false;};
	virtual bool		OnMouseAction					(float x, float y, EUIMessages mouse_action) {return false;};
	
	virtual void		UnLoad					();
	void				Load					();

	virtual	void					reset_ui				();
	void				OnConnected				();

	CUIXml*				m_actor_menu_item;

	DECLARE_SCRIPT_REGISTER_FUNCTION
}; // class CUIGameCustom
extern CUIGameCustom*		CurrentGameUI();


#endif