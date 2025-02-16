#include "pch_script.h"
#include "UIMapWnd.h"
#include "UIMap.h"
#include "UIXmlInit.h"

#include "../map_manager.h"
#include "UIInventoryUtilities.h"
#include "../map_location.h"

#include "UIScrollBar.h"
#include "UIFrameWindow.h"
#include "UIFrameLineWnd.h"
#include "UITabControl.h"
#include "UI3tButton.h"
#include "UIMapWndActions.h"
#include "UIMapWndActionsSpace.h"
#include "map_hint.h"
#include "../game_cl_base.h"

#include "../HUDManager.h"
#include "../script_engine.h"
#include "../uicursor.h"

#include "../../xrEngine/xr_input.h"		//remove me !!!
class	game_cl_GameState;

const	int			SCROLLBARS_SHIFT			= 5;
const	int			VSCROLLBAR_STEP				= 20; // В пикселях
const	int			HSCROLLBAR_STEP				= 20; // В пикселях

static bool			MAP_FLY_MODE				= true;


CUIMapWnd::CUIMapWnd()
{
	m_tgtMap				= NULL;
	m_GlobalMap				= NULL;
	m_flags.zero			();
	m_currentZoom			= 1.0f;
	m_hint					= NULL;
//.	m_selected_location		= NULL;
	m_text_hint				= NULL;
}

CUIMapWnd::~CUIMapWnd()
{
	delete_data			(m_ActionPlanner);
	delete_data			(m_GameMaps);
	delete_data			(m_hint);
	delete_data			(m_text_hint);
}


void CUIMapWnd::Init(LPCSTR xml_name, LPCSTR start_from)
{
	CUIXml uiXml;
	uiXml.Load	(CONFIG_PATH, UI_PATH, xml_name);

	string512	pth;
	// load map background
	CUIXmlInit xml_init;
	xr_strconcat(pth,start_from,":main_wnd");
	xml_init.InitWindow				(uiXml, pth, 0, this);


	m_UIMainFrame					= new CUIFrameWindow(); m_UIMainFrame->SetAutoDelete(true);
	AttachChild						(m_UIMainFrame);
	xr_strconcat(pth,start_from,":main_wnd:main_map_frame");
	xml_init.InitFrameWindow		(uiXml, pth, 0, m_UIMainFrame);

	m_UILevelFrame					= new CUIWindow(); m_UILevelFrame->SetAutoDelete(true);
	xr_strconcat(pth,start_from,":main_wnd:main_map_frame:level_frame");
	xml_init.InitWindow				(uiXml, pth, 0, m_UILevelFrame);
	m_UIMainFrame->AttachChild		(m_UILevelFrame);

	Frect r							= m_UILevelFrame->GetWndRect();

	m_UIMainScrollH					= new CUIScrollBar(); m_UIMainScrollH->SetAutoDelete(true);
	m_UIMainScrollH->InitScrollBar			(Fvector2().set(r.left, r.bottom + SCROLLBARS_SHIFT), r.right - r.left, true, "pda");
	m_UIMainScrollH->SetWindowName	("scroll_h");
	m_UIMainScrollH->SetStepSize	(_max(1,iFloor(m_UILevelFrame->GetWidth()/10)));
	m_UIMainScrollH->SetPageSize	(iFloor(m_UILevelFrame->GetWidth()));
	m_UIMainFrame->AttachChild		(m_UIMainScrollH);
	Register						(m_UIMainScrollH);
	AddCallbackStr						("scroll_h",SCROLLBAR_HSCROLL,CUIWndCallback::void_function(this,&CUIMapWnd::OnScrollH));

	m_UIMainScrollV					= new CUIScrollBar(); m_UIMainScrollV->SetAutoDelete(true);
	m_UIMainScrollV->InitScrollBar			(Fvector2().set(r.right + SCROLLBARS_SHIFT, r.top), m_UIMainScrollH->GetWndRect().bottom - r.top, false, "pda");
	m_UIMainScrollV->SetWindowName	("scroll_v");
	m_UIMainScrollV->SetStepSize	(_max(1,iFloor(m_UILevelFrame->GetHeight()/10)));
	m_UIMainScrollV->SetPageSize	(iFloor(m_UILevelFrame->GetHeight()));
	m_UIMainFrame->AttachChild		(m_UIMainScrollV);
	Register						(m_UIMainScrollV);
	AddCallbackStr						("scroll_v",SCROLLBAR_VSCROLL,CUIWndCallback::void_function(this,&CUIMapWnd::OnScrollV));

	UIMainMapHeader					= new CUIFrameLineWnd(); UIMainMapHeader->SetAutoDelete(true);
	m_UIMainFrame->AttachChild		(UIMainMapHeader);
	xr_strconcat(pth,start_from,":main_wnd:map_header_frame_line");
	xml_init.InitFrameLine			(uiXml, pth, 0, UIMainMapHeader);

	ZeroMemory						(m_ToolBar,sizeof(m_ToolBar));
	xr_string  sToolbar;
	sToolbar	= xr_string(start_from) + ":main_wnd:map_header_frame_line:tool_bar";

	EMapToolBtn		btnIndex;
	btnIndex		= eGlobalMap;
	xr_strconcat(pth, sToolbar.c_str(), ":global_map_btn");
	if(uiXml.NavigateToNode(pth,0)){
		m_ToolBar[btnIndex]				= new CUI3tButton(); m_ToolBar[btnIndex]->SetAutoDelete(true);
		xml_init.Init3tButton			(uiXml, pth, 0, m_ToolBar[btnIndex]);
		UIMainMapHeader->AttachChild	(m_ToolBar[btnIndex]);
		Register						(m_ToolBar[btnIndex]);
		AddCallbackStr						(*m_ToolBar[btnIndex]->WindowName(),BUTTON_CLICKED,CUIWndCallback::void_function(this,&CUIMapWnd::OnToolGlobalMapClicked));
	}

	btnIndex		= eActor;
	xr_strconcat(pth, sToolbar.c_str(), ":actor_btn");
	if(uiXml.NavigateToNode(pth,0)){
		m_ToolBar[btnIndex]				= new CUI3tButton(); m_ToolBar[btnIndex]->SetAutoDelete(true);
		xml_init.Init3tButton			(uiXml, pth, 0, m_ToolBar[btnIndex]);
		UIMainMapHeader->AttachChild	(m_ToolBar[btnIndex]);
		Register						(m_ToolBar[btnIndex]);
		AddCallbackStr						(*m_ToolBar[btnIndex]->WindowName(),BUTTON_CLICKED,CUIWndCallback::void_function(this,&CUIMapWnd::OnToolActorClicked));
	}


	btnIndex		= eZoomIn;
	xr_strconcat(pth, sToolbar.c_str(), ":zoom_in_btn");
	if(uiXml.NavigateToNode(pth,0)){
		m_ToolBar[btnIndex]				= new CUI3tButton(); m_ToolBar[btnIndex]->SetAutoDelete(true);
		xml_init.Init3tButton			(uiXml, pth, 0, m_ToolBar[btnIndex]);
		UIMainMapHeader->AttachChild	(m_ToolBar[btnIndex]);
		Register						(m_ToolBar[btnIndex]);
		AddCallbackStr						(*m_ToolBar[btnIndex]->WindowName(),BUTTON_CLICKED,CUIWndCallback::void_function(this, &CUIMapWnd::OnToolZoomInClicked));
	}
	btnIndex		= eZoomOut;
	xr_strconcat(pth, sToolbar.c_str(), ":zoom_out_btn");
	if(uiXml.NavigateToNode(pth,0)){
		m_ToolBar[btnIndex]				= new CUI3tButton(); m_ToolBar[btnIndex]->SetAutoDelete(true);
		xml_init.Init3tButton			(uiXml, pth, 0, m_ToolBar[btnIndex]);
		UIMainMapHeader->AttachChild	(m_ToolBar[btnIndex]);
		Register						(m_ToolBar[btnIndex]);
		AddCallbackStr						(*m_ToolBar[btnIndex]->WindowName(),BUTTON_CLICKED,CUIWndCallback::void_function(this, &CUIMapWnd::OnToolZoomOutClicked));
	}

	btnIndex		= eAddSpot;
	xr_strconcat(pth, sToolbar.c_str(), ":add_spot_btn");
	if(uiXml.NavigateToNode(pth,0) && IsGameTypeSingle() ){
		m_ToolBar[btnIndex]				= new CUI3tButton(); m_ToolBar[btnIndex]->SetAutoDelete(true);
		xml_init.Init3tButton			(uiXml, pth, 0, m_ToolBar[btnIndex]);
		UIMainMapHeader->AttachChild	(m_ToolBar[btnIndex]);
		Register						(m_ToolBar[btnIndex]);
		AddCallbackStr						(*m_ToolBar[btnIndex]->WindowName(),BUTTON_CLICKED,CUIWndCallback::void_function(this, &CUIMapWnd::OnToolAddSpotClicked));
	}
	/*btnIndex		= eRemoveSpot;
	strconcat(sizeof(pth),pth, sToolbar.c_str(), ":remove_spot_btn");
	if(uiXml.NavigateToNode(pth,0 ) && IsGameTypeSingle() ){
		m_ToolBar[btnIndex]				= new CUI3tButton(); m_ToolBar[btnIndex]->SetAutoDelete(true); m_ToolBar[btnIndex]->Enable(false);
		xml_init.Init3tButton			(uiXml, pth, 0, m_ToolBar[btnIndex]);
		UIMainMapHeader->AttachChild	(m_ToolBar[btnIndex]);
		Register						(m_ToolBar[btnIndex]);
		AddCallbackStr						(*m_ToolBar[btnIndex]->WindowName(),BUTTON_CLICKED,CUIWndCallback::void_function(this, &CUIMapWnd::OnToolRemoveSpotClicked));
	}

	btnIndex		= eHighlightSpot;
	strconcat(sizeof(pth),pth, sToolbar.c_str(), ":highlight_spot_btn");
	if(uiXml.NavigateToNode(pth,0) && IsGameTypeSingle() ){
		m_ToolBar[btnIndex]				= new CUI3tButton(); m_ToolBar[btnIndex]->SetAutoDelete(true);
		xml_init.Init3tButton			(uiXml, pth, 0, m_ToolBar[btnIndex]);
		UIMainMapHeader->AttachChild	(m_ToolBar[btnIndex]);
		Register						(m_ToolBar[btnIndex]);
		//AddCallbackStr						(*m_ToolBar[btnIndex]->WindowName(),BUTTON_CLICKED,CUIWndCallback::void_function(&CUIMapWnd::OnToolHighlightSpotClicked,this,_1,_2));
	}*/

	m_text_hint							= new CUITextWnd();
	xr_strconcat						(pth,start_from,":main_wnd:text_hint");
	xml_init.InitTextWnd				(uiXml, pth, 0, m_text_hint);

	m_hint								= new CUIMapHint();
	m_hint->Init						();
	m_hint->SetAutoDelete				(false);

// Load maps

	CInifile& gameLtx						= *pGameIni;

	m_GlobalMap								= new CUIGlobalMap(this);
	m_GlobalMap->SetAutoDelete				(true);
	m_GlobalMap->Init						("global_map",gameLtx,"hud\\default");

	m_UILevelFrame->AttachChild				(m_GlobalMap);
	m_GlobalMap->OptimalFit					(m_UILevelFrame->GetWndRect());
	m_GlobalMap->SetMinZoom					(m_GlobalMap->GetCurrentZoom());
	m_currentZoom							= m_GlobalMap->GetCurrentZoom();

	// initialize local maps
	xr_string sect_name;
	if( IsGameTypeSingle() )
		sect_name = "level_maps_single";
	else
		sect_name = "level_maps_mp";

	if (gameLtx.section_exist(sect_name.c_str())){
		CInifile::Sect& S		= gameLtx.r_section(sect_name.c_str());
		CInifile::SectCIt	it	= S.Data.begin(), end = S.Data.end();
		for (;it!=end; it++){
			shared_str map_name = it->first;
			xr_strlwr(map_name);
			R_ASSERT2	(m_GameMaps.end() == m_GameMaps.find(map_name), "Duplicate level name not allowed");
			
			CUICustomMap*& l = m_GameMaps[map_name];

			l = new CUILevelMap(this);
			
			l->Init(map_name, gameLtx, "hud\\default");

			l->OptimalFit( m_UILevelFrame->GetWndRect() );
		}
	}
#ifdef DEBUG
	GameMaps::iterator it = m_GameMaps.begin();
	GameMaps::iterator it2;
	for(;it!=m_GameMaps.end();++it){
		CUILevelMap* l = smart_cast<CUILevelMap*>(it->second);VERIFY(l);
		for(it2=it; it2!=m_GameMaps.end();++it2){
			if(it==it2) continue;
			CUILevelMap* l2 = smart_cast<CUILevelMap*>(it2->second);VERIFY(l2);
			if(l->GlobalRect().intersected(l2->GlobalRect())){
				Msg(" --error-incorrect map definition global rect of map [%s] intersects with [%s]", *l->MapName(), *l2->MapName());
			}
		}
		if(FALSE == l->GlobalRect().intersected(GlobalMap()->BoundRect())){
			Msg(" --error-incorrect map definition map [%s] places outside global map", *l->MapName());
		}

	}
#endif

	Register						(m_GlobalMap);
	m_ActionPlanner					= new CMapActionPlanner();
	m_ActionPlanner->setup			(this);
	m_flags.set						(lmFirst,TRUE);
}

void CUIMapWnd::Show(bool status)
{
	inherited::Show(status);
	if (status)
	{
		m_GlobalMap->Show			(true);
		m_GlobalMap->WorkingArea().set	(ActiveMapRect());
		GameMaps::iterator	it		= m_GameMaps.begin();
		for(;it!=m_GameMaps.end();++it){
			m_GlobalMap->AttachChild(it->second);
			it->second->Show		(true);
			it->second->WorkingArea().set	(ActiveMapRect());
		}
		if(	m_flags.test(lmFirst)){
			inherited::Update		();// only maps, not action planner
			OnToolActorClicked		(NULL,NULL);
			m_flags.set				(lmFirst,FALSE);
			}
		InventoryUtilities::SendInfoToActor("ui_pda_map_local");
	}else{
		if(GlobalMap()){
			GlobalMap()->DetachAll();
			GlobalMap()->Show(false);
		}
		GameMaps::iterator	it = m_GameMaps.begin();
		for(;it!=m_GameMaps.end();++it)
			it->second->DetachAll();
	}

	m_hint->SetOwner		(NULL);
}


void CUIMapWnd::AddMapToRender			(CUICustomMap* m)
{
	Register							( m );
	m_UILevelFrame->AttachChild			( m );
	m->Show								( true );
	m_UILevelFrame->BringToTop			( m );
	m->WorkingArea().set				( ActiveMapRect() );
}

void CUIMapWnd::RemoveMapToRender		(CUICustomMap* m)
{
	if( m!=GlobalMap() )
		m_UILevelFrame->DetachChild			(smart_cast<CUIWindow*>(m));
}

void CUIMapWnd::SetTargetMap			(const shared_str& name, const Fvector2& pos, bool bZoomIn)
{
	u16	idx								= GetIdxByName			(name);
	if (idx!=u16(-1)){
		CUICustomMap* lm				= GetMapByIdx			(idx);
		SetTargetMap					(lm, pos, bZoomIn);
	}
}

void CUIMapWnd::SetTargetMap			(const shared_str& name, bool bZoomIn)
{
	u16	idx								= GetIdxByName			(name);
	if (idx!=u16(-1)){
		CUICustomMap* lm				= GetMapByIdx			(idx);
		SetTargetMap					(lm, bZoomIn);
	}
}

void CUIMapWnd::SetTargetMap			(CUICustomMap* m, bool bZoomIn)
{
	m_tgtMap							= m;
	Fvector2							pos;
	Frect r								= m->BoundRect();
	r.getcenter							(pos);
	SetTargetMap						(m, pos, bZoomIn);
}

void CUIMapWnd::SetTargetMap			(CUICustomMap* m, const Fvector2& pos, bool bZoomIn)
{
	m_tgtMap							= m;

	if(m==GlobalMap()){
		CUIGlobalMap* gm				= GlobalMap();
		SetZoom							(gm->GetMinZoom());
		Frect vis_rect					= ActiveMapRect		();
		vis_rect.getcenter				(m_tgtCenter);
		Fvector2	_p;gm->GetAbsolutePos(_p);
		m_tgtCenter.sub					(_p);
		m_tgtCenter.div					(gm->GetCurrentZoom());
 	}else{

		if(bZoomIn/* && fsimilar(GlobalMap()->GetCurrentZoom(), GlobalMap()->GetMinZoom(),EPS_L )*/)
			SetZoom(GlobalMap()->GetMaxZoom());

		m_tgtCenter						= m->ConvertRealToLocalNoTransform(pos);
		m_tgtCenter.add					(m->GetWndPos()).div(GlobalMap()->GetCurrentZoom());
	}
	ResetActionPlanner				();
}

void CUIMapWnd::Draw()
{
	inherited::Draw();
	m_text_hint->Draw		();
	if(m_hint->GetOwner())	m_hint->Draw_();
}

bool CUIMapWnd::OnKeyboardHold(int dik)
{
	switch(dik){
		case DIK_UP:
		case DIK_DOWN:
		case DIK_LEFT:
		case DIK_RIGHT:
			{
				Fvector2 pos_delta; pos_delta.set(0.0f, 0.0f);


				if(dik==DIK_UP)					pos_delta.y	+= 1.0f;
				if(dik==DIK_DOWN)				pos_delta.y	-= 1.0f;
				if(dik==DIK_LEFT)				pos_delta.x	+= 1.0f;
				if(dik==DIK_RIGHT)				pos_delta.x	-= 1.0f;
				GlobalMap()->MoveWndDelta		(pos_delta);
				UpdateScroll					();
				m_hint->SetOwner				(NULL);
				return true;
			}break;
	}
	return false;
}

bool CUIMapWnd::OnKeyboardAction				(int dik, EUIMessages keyboard_action)
{
	switch(dik){
		case DIK_NUMPADMINUS:
			{
				SetZoom(GetZoom()/1.5f);
				ResetActionPlanner				();
				return true;
			}break;
		case DIK_NUMPADPLUS:
			{
				SetZoom(GetZoom()*1.5f);
				ResetActionPlanner				();
				return true;
			}break;
	}
	
	return inherited::OnKeyboardAction	(dik, keyboard_action);
}

bool CUIMapWnd::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if(inherited::OnMouseAction(x,y,mouse_action)) return true;
	Fvector2 cursor_pos = GetUICursor().GetCursorPosition();

	if(GlobalMap() && !GlobalMap()->Locked() && ActiveMapRect().in( cursor_pos ) ){
		switch (mouse_action)
		{
		case WINDOW_MOUSE_MOVE:
			if( pInput->iGetAsyncBtnState(0) ){
				GlobalMap()->MoveWndDelta	(GetUICursor().GetCursorPositionDelta());
				UpdateScroll					();
				m_hint->SetOwner				(NULL);
				return							true;
			}
		break;
/*
		case WINDOW_LBUTTON_DOWN:
			if (	((mouse_action==WINDOW_LBUTTON_DOWN)&&(m_flags.is_any(lmZoomIn+lmZoomOut))) || 
					(mouse_action==WINDOW_MOUSE_WHEEL_DOWN) ||
					(mouse_action==WINDOW_MOUSE_WHEEL_UP)	
				)
			{
				CUIGlobalMap* gm				= GlobalMap();
				if(m_flags.test(lmZoomIn))		SetZoom(GetZoom()*1.5f);
				else							SetZoom(GetZoom()/1.5f);
				m_tgtCenter						= cursor_pos;
				Fvector2 _p;					gm->GetAbsolutePos(_p);
				m_tgtCenter.sub					(_p);
				m_tgtCenter.div					(gm->GetCurrentZoom());
				ResetActionPlanner				();
				m_hint->SetOwner				(NULL);
				return							true;
			}
		break;

		case WINDOW_MOUSE_WHEEL_UP:
			m_UIMainScrollV->TryScrollDec		();
			m_hint->SetOwner					(NULL);
			return								true;
		break;

		case WINDOW_MOUSE_WHEEL_DOWN:
			m_UIMainScrollV->TryScrollInc		();
			m_hint->SetOwner					(NULL);
			return								true;
		break;
*/
		}	
	};

	if (	((mouse_action==WINDOW_LBUTTON_DOWN)&&(m_flags.is_any(lmZoomIn+lmZoomOut)))		|| 
			(mouse_action==WINDOW_MOUSE_WHEEL_DOWN)											||
			(mouse_action==WINDOW_MOUSE_WHEEL_UP)	
		)
		{
			bool b_zoom_in =	(mouse_action==WINDOW_LBUTTON_DOWN && m_flags.test(lmZoomIn)) || 
								(mouse_action==WINDOW_MOUSE_WHEEL_DOWN);

			CUIGlobalMap* gm				= GlobalMap();
			float _prev_zoom				= GetZoom();
			if(b_zoom_in)					SetZoom(GetZoom()*1.5f);
			else							SetZoom(GetZoom()/1.5f);

			if(!fsimilar(_prev_zoom, GetZoom()))
			{
				m_tgtCenter						= cursor_pos;
				Fvector2 _p;					gm->GetAbsolutePos(_p);
				m_tgtCenter.sub					(_p);
				m_tgtCenter.div					(gm->GetCurrentZoom());
				ResetActionPlanner				();
				m_hint->SetOwner				(NULL);
			}
			return								true;
	}
	else if ((mouse_action == WINDOW_LBUTTON_DOWN) && (m_flags.test(lmUserSpotAdd))) {

			Fvector RealPosition;

			if (ConvertCursorPosToMap(&RealPosition)) {

				CMapLocation* _mapLoc = UnderSpot(RealPosition);

				if (_mapLoc==NULL) {
					CreateSpotWindow(RealPosition);
				} else {

					CUserDefinedMapLocation* userML = smart_cast<CUserDefinedMapLocation*>(_mapLoc);
					ShowSettingsWindow(_mapLoc->GetType(),_mapLoc->ObjectID(), userML->PositionReal(),userML->LevelName());
				}
				

			}

	}


	return										false;
}

void CUIMapWnd::ShowSettingsWindow(LPCSTR type, u16 id, Fvector pos, shared_str levelName)
{
	luabind::functor<void>	lua_function;
	string256		fn;
	xr_strcpy		(fn, pSettings->r_string("lost_alpha_cfg", "on_init_settings_pda_spot"));
	R_ASSERT2 (ai().script_engine().functor<void>(fn,lua_function),make_string<const char*>("Can't find function %s",fn));
	
	string256 MapName;
	xr_strcpy(MapName, levelName._get()->value);
	lua_function								(id,type,pos,MapName);
}

CMapLocation* CUIMapWnd::UnderSpot(Fvector RealPosition)
{
	Fvector2 RealPositionXZ;
	RealPositionXZ.set(RealPosition.x,RealPosition.z);

	Locations Spots = Level().MapManager().Locations();
	Locations_it it;
	Fvector2 m_position_on_map;
	Fvector2 m_position_mouse = m_tgtMap->ConvertRealToLocal(RealPositionXZ);
	float TargetLocationDistance = 100.0f;
	CMapLocation* ml = NULL;

	for (it = Spots.begin(); it!=Spots.end(); ++it) {
		if ( (*it).location->IsUserDefined() ) {

			m_position_on_map = m_tgtMap->ConvertRealToLocal((*it).location->Position());

			float distance = m_position_on_map.distance_to(m_position_mouse);

			Fvector2 FvectorSize = (*it).location->SpotSize();
			float size = (FvectorSize.x+FvectorSize.y)/2;

			if ((distance < size ) && (distance<TargetLocationDistance)) {

				TargetLocationDistance = distance;
				ml = (*it).location;

			}
		}
	}
	return ml;
}

void CUIMapWnd::CreateSpotWindow(Fvector RealPosition)
{

	string256 MapName;
	xr_strcpy(MapName, m_tgtMap->MapName()._get()->value);

	luabind::functor<void>	lua_function;
	string256		fn;
	xr_strcpy		(fn, pSettings->r_string("lost_alpha_cfg", "on_init_pda_spot"));
	R_ASSERT2 (ai().script_engine().functor<void>(fn,lua_function),make_string<const char*>("Can't find function %s",fn));
	lua_function								(MapName, RealPosition);

}

void CUIMapWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent						(pWnd, msg, pData);
}

CUICustomMap*	CUIMapWnd::GetMapByIdx(u16 idx)
{
	VERIFY							(idx!=u16(-1));
	GameMapsPairIt it				= m_GameMaps.begin();
	std::advance					(it, idx);
	return							it->second;
}

u16 CUIMapWnd::GetIdxByName			(const shared_str& map_name)
{
	GameMapsPairIt it				= m_GameMaps.find(map_name);
	if(it==m_GameMaps.end()){	
		Msg							("~ Level Map '%s' not registered",map_name.c_str());
		return						u16(-1);
	}
	return (u16)std::distance		(m_GameMaps.begin(),it);
}

void CUIMapWnd::UpdateScroll()
{
	Fvector2 w_pos					= GlobalMap()->GetWndPos();
	m_UIMainScrollV->SetRange		(0,iFloor(GlobalMap()->GetHeight()));
	m_UIMainScrollH->SetRange		(0,iFloor(GlobalMap()->GetWidth()));

	m_UIMainScrollV->SetScrollPos	(iFloor(-w_pos.y));
	m_UIMainScrollH->SetScrollPos	(iFloor(-w_pos.x));
}


void CUIMapWnd::OnScrollV(CUIWindow*, void*)
{
	if (GlobalMap()){
		int s_pos					= m_UIMainScrollV->GetScrollPos();
		Fvector2 w_pos				= GlobalMap()->GetWndPos();
		GlobalMap()->SetWndPos	(w_pos.x,float(-s_pos));
	}
}

void CUIMapWnd::OnScrollH(CUIWindow*, void*)
{
	if (GlobalMap()){
		int s_pos					= m_UIMainScrollH->GetScrollPos();
		Fvector2 w_pos				= GlobalMap()->GetWndPos();
		GlobalMap()->SetWndPos	(float(-s_pos),w_pos.y);
	}
}


void CUIMapWnd::Update()
{
	if(m_GlobalMap)
		m_GlobalMap->WorkingArea().set(ActiveMapRect());
	inherited::Update			();
	m_ActionPlanner->update		();
}

void CUIMapWnd::SetZoom	( float value)
{
//	float _prev_zoom = m_currentZoom;
	m_currentZoom	= value;
	clamp		(m_currentZoom, GlobalMap()->GetMinZoom(), GlobalMap()->GetMaxZoom());
}



void CUIMapWnd::OnToolGlobalMapClicked	(CUIWindow* w, void*)
{
	if (GlobalMap()->Locked())			return;
	SetTargetMap(GlobalMap());
}


void CUIMapWnd::ResetActionPlanner()
{
	if (MAP_FLY_MODE){
		m_ActionPlanner->m_storage.set_property(1,false);
		m_ActionPlanner->m_storage.set_property(2,false);
		m_ActionPlanner->m_storage.set_property(3,false);
	}else{
		Frect m_desiredMapRect;
		GlobalMap()->CalcOpenRect	(m_tgtCenter,m_desiredMapRect,GetZoom());
		GlobalMap()->SetWndRect		(m_desiredMapRect);
		UpdateScroll				();
	}
}

void CUIMapWnd::OnToolZoomInClicked	(CUIWindow* w, void*)
{
	if (GlobalMap()->Locked())		return;

	if (!m_flags.test(lmZoomIn)){
		m_flags.zero();
		m_flags.set(lmZoomIn, true);
	}
	else{
		m_flags.zero();
		m_flags.set(lmZoomIn, false);
	}
	//ValidateToolBar					();
}

void CUIMapWnd::OnToolZoomOutClicked(CUIWindow* w, void*)
{
	if (GlobalMap()->Locked())		return;

	if (!m_flags.test(lmZoomOut)){
		m_flags.zero();
		m_flags.set(lmZoomOut, true);
	}
	else{
		m_flags.zero();
		m_flags.set(lmZoomOut, false);
	}
	//ValidateToolBar					();
}

void CUIMapWnd::OnToolAddSpotClicked(CUIWindow* w, void*)
{
	if (!m_flags.test(lmUserSpotAdd)){
		m_flags.zero();
		m_flags.set(lmUserSpotAdd, true);
	}
	else{
		m_flags.zero();
		m_flags.set(lmUserSpotAdd, false);
	}
	//ValidateToolBar						();
}

void CUIMapWnd::OnToolRemoveSpotClicked	(CUIWindow* w, void*)
{
	m_flags.zero		();
	CUI3tButton* btn = smart_cast<CUI3tButton*>(w);
	bool bPushed = btn->GetCheck		();
	m_flags.set							(lmUserSpotRemove,bPushed);
	ValidateToolBar						();
}
/*
void CUIMapWnd::RemoveSpot			()
{
	if(m_selected_location&&m_selected_location->CanBeUserRemoved()){
		Level().MapManager().RemoveMapLocation(m_selected_location);
		m_selected_location				= NULL;
		m_flags.set						(lmUserSpotRemove,FALSE);
		m_ToolBar[eRemoveSpot]->SetButtonMode(CUIButton::BUTTON_NORMAL);
	}
}
*/

/*
void CUIMapWnd::OnToolHighlightSpotClicked(CUIWindow* w, void*)
{
	m_flags.zero		();
	CUI3tButton* btn = smart_cast<CUI3tButton*>(w);
	bool bPushed = btn->GetCheck		();
	m_flags.set							(lmHighlightSpot,bPushed);
	ValidateToolBar						();
}
*/
/*
void CUIMapWnd::HighlightSpot			(CMapLocation* ml, bool state)
{
	if(m_selected_location){
		bool b = m_selected_location->PointerEnabled	();
		Level().MapManager().DisableAllPointers();
		if(b)
			m_selected_location->DisablePointer();
		else
			m_selected_location->EnablePointer();

		m_ToolBar[eHighlightSpot]->SetButtonMode(CUIButton::BUTTON_NORMAL);
		m_flags.set						(lmHighlightSpot,FALSE);
	}
}*/

void CUIMapWnd::ValidateToolBar			()
{
	CUI3tButton* btn	= NULL;
	btn					= m_ToolBar[eZoomIn];
	if (btn)
		btn->SetCheck	(!!m_flags.test(lmZoomIn));

	btn					= m_ToolBar[eZoomOut];
	if(btn)
		btn->SetCheck	(!!m_flags.test(lmZoomOut));

	btn					= m_ToolBar[eAddSpot];
	if(btn)
		btn->SetCheck	(!!m_flags.test(lmUserSpotAdd));

	btn					= m_ToolBar[eRemoveSpot];
	if(btn)
		btn->SetCheck	(!!m_flags.test(lmUserSpotRemove));

/*	btn					= m_ToolBar[eHighlightSpot];
	if(btn)
		btn->SetCheck	(!!m_flags.test(lmHighlightSpot));
*/
}


void CUIMapWnd::OnToolActorClicked		(CUIWindow*, void*)
{
	if (GlobalMap()->Locked())			return;

	Fvector v;				
	if (Level().CurrentEntity())
		v		= Level().CurrentEntity()->Position();
	else
		v		= Device.vCameraPosition;

	Fvector2 v2;
	v2.set						(v.x,v.z);

	CUICustomMap* lm			= NULL;
	u16	idx						= GetIdxByName			(Level().name());
	if (idx!=u16(-1)){
		lm						= GetMapByIdx			(idx);
	}else
		lm						= GlobalMap();

	SetTargetMap				(lm, v2, true);
}

bool CUIMapWnd::ConvertCursorPosToMap(Fvector* return_position)
{
	if (fsimilar(GlobalMap()->GetMinZoom(), GetZoom(), EPS_L)) return false;

	if (!m_tgtMap) return false;
	CUILevelMap* CurrentMap = smart_cast<CUILevelMap*>(m_tgtMap);										//map = CurrentMap
	if (!CurrentMap) return false;

	Frect PosOnMap = CurrentMap->GlobalRect();															//v1=PosOnMap
	Frect PosReal = CurrentMap->BoundRect();
	Fvector2 Position = m_GlobalMap->GetWndPos();														//v2=Positioin

	Position.div(m_GlobalMap->GetCurrentZoom());

	Position.abs(Position);
	Position.sub(PosOnMap.lt);
	
	//Где находимся от левого верхнего угла
	//Add cursor position
	Fvector2 CursorPos = GetUICursor().GetCursorPosition();
	CursorPos.sub(ActiveMapRect().lt);
	CursorPos.div(m_GlobalMap->GetCurrentZoom());
	Position.add(CursorPos);

	//Ratio: Meters to Pixels
	Fvector2 Ratio;
	Ratio.x = PosReal.width() /PosOnMap.width();														//Отношение пикселей к реальным метрам
	Ratio.y = PosReal.height() /PosOnMap.height();

	//Location center isn't usually in the map location center
	//Центр локации не всегда там где центр локации на карте
	Fvector2 OffsetPosition;
	OffsetPosition.set( PosReal.rb.x + PosReal.lt.x,PosReal.rb.y + PosReal.lt.y);
	OffsetPosition.div(2.0f);

	//Center on map. In PDA
	Fvector2 CenterOnMap;
	CenterOnMap.set( PosOnMap.rb.x-PosOnMap.lt.x, PosOnMap.rb.y-PosOnMap.lt.y );
	CenterOnMap.div(2.0f);


	Position.set(Position.x-CenterOnMap.x,CenterOnMap.y-Position.y);
	Position.mul(Ratio);
	Position.add(OffsetPosition);


	return_position->set(Position.x,0.0f,Position.y);
	return true;
}

/*
void CUIMapWnd::AddUserSpot			(CUILevelMap* lm)
{
	VERIFY(m_flags.test(lmUserSpotAdd) );

	Fvector2 cursor_pos = GetUICursor().GetPos();
	Fvector2 _p;lm->GetAbsolutePos(_p);
	cursor_pos.sub					(_p);
	Fvector2 p =					lm->ConvertLocalToReal(cursor_pos);
	Fvector pos;
	pos.set							(p.x, 0.0f, p.y);
	shared_str spot					= "user"; 
	CMapLocation* ml				= Level().MapManager().AddUserLocation(spot, lm->MapName(), pos);
	CGameTask* t					= Actor()->GameTaskManager().GiveGameTaskToActor("user_task",false);
	t->m_Objectives[0].object_id	= ml->ObjectID();
	t->m_Objectives[0].map_location	= spot;
	ml->SetHint						(t->m_Objectives[0].description);
	Actor()->GameTaskManager		().SetTaskState(t, 0, eTaskUserDefined);

	m_flags.set						(lmUserSpotAdd, FALSE);
	m_ToolBar[eAddSpot]->SetButtonMode(CUIButton::BUTTON_NORMAL);
}*/

bool is_in(const Frect& b1, const Frect& b2){
	return (b1.x1<b2.x1)&&(b1.x2>b2.x2)&&(b1.y1<b2.y1)&&(b1.y2>b2.y2);
}

void CUIMapWnd::ShowHint					(CUIWindow* parent, LPCSTR text)
{
	if(m_hint->GetOwner())	return;
	if(!text)				return;
	Fvector2 c_pos			= GetUICursor().GetCursorPosition();
	Frect vis_rect			= ActiveMapRect				();
	if(FALSE==vis_rect.in(c_pos)) return;

	m_hint->SetOwner		(parent);
	m_hint->SetText			(text);

	//select appropriate position
	Frect r;
	r.set					(0.0f, 0.0f, m_hint->GetWidth(), m_hint->GetHeight());
	r.add					(c_pos.x, c_pos.y);

	r.sub					(0.0f,r.height());
	if (false==is_in(vis_rect,r))
		r.sub				(r.width(),0.0f);
	if (false==is_in(vis_rect,r))
		r.add				(0.0f,r.height());

	if (false==is_in(vis_rect,r))
		r.add				(r.width(), 45.0f);

	m_hint->SetWndPos		(r.lt);
}

void CUIMapWnd::HideHint					(CUIWindow* parent)
{
	if(m_hint->GetOwner() == parent)
		m_hint->SetOwner	(NULL);
}

/*
void CUIMapWnd::Select				(CMapLocation* ml)
{
	m_selected_location		= NULL;

	if(!ml)		return;
	if(ml->CanBeSelected())
		m_selected_location = ml;

	if(!!m_flags.test(lmUserSpotRemove) && ml->CanBeUserRemoved() )
		RemoveSpot	();

	if(	!!m_flags.test(lmHighlightSpot))
		HighlightSpot	();
}*/

void CUIMapWnd::Hint					(const shared_str& text)
{
	if (xr_strcmp(text,"global_map")) {
		u16 __id = GetIdxByName(text);
		if (__id!=u16(-1)) m_tgtMap = GetMapByIdx(__id);
	}
	m_text_hint->SetTextST				(*text);
}

void CUIMapWnd::Reset()
{
	inherited::Reset			();
	ResetActionPlanner			();
}
