#include "pch_script.h"
#include "UIGameCustom.h"
#include "level.h"
#include "ui/UIXmlInit.h"
#include "ui/UIStatic.h"
#include "ui/UIMultiTextStatic.h"
#include "../xrCore/object_broker.h"
#include "../xrEngine/string_table.h"

#include "InventoryOwner.h"

#include "ui/UIPdaWnd.h"
#include "ui/UIMainIngameWnd.h"
#include "ui/UIMessagesWindow.h"
#include "ui/UIInventoryWnd.h"
#include "ui/UICarBodyWnd.h"

#include "actor.h"
#include "inventory.h"
#include "game_cl_base.h"

#include "../x_ray.h"

bool predicate_sort_stat(const SDrawStaticStruct* s1, const SDrawStaticStruct* s2) 
{
	return ( s1->IsActual() > s2->IsActual() );
}

struct predicate_find_stat 
{
	LPCSTR	m_id;
	predicate_find_stat(LPCSTR id):m_id(id)	{}
	bool	operator() (SDrawStaticStruct* s) 
	{
		return ( s->m_name==m_id );
	}
};

CUIGameCustom::CUIGameCustom()
	:m_pgameCaptions(nullptr), m_msgs_xml(nullptr), m_actor_menu_item(nullptr), m_window(nullptr), m_InventoryMenu(nullptr), m_PdaMenu(nullptr), m_UICarBodyMenu(nullptr), UIMainIngameWnd(nullptr), m_pMessagesWnd(nullptr), m_WeatherEditor(nullptr)
{
	ShowGameIndicators		(true);
	ShowCrosshair			(true);
}
bool g_b_ClearGameCaptions = false;

CUIGameCustom::~CUIGameCustom()
{
	delete_data				(m_custom_statics);
	g_b_ClearGameCaptions	= false;
}


void CUIGameCustom::OnFrame() 
{
	CDialogHolder::OnFrame();

	st_vec_it it = m_custom_statics.begin();
	st_vec_it it_e = m_custom_statics.end();
	for(;it!=it_e;++it)
		(*it)->Update();

	std::sort(	it, it_e, predicate_sort_stat );
	
	while(!m_custom_statics.empty() && !m_custom_statics.back()->IsActual())
	{
		delete_data					(m_custom_statics.back());
		m_custom_statics.pop_back	();
	}
	
	if(g_b_ClearGameCaptions)
	{
		delete_data				(m_custom_statics);
		g_b_ClearGameCaptions	= false;
	}
	m_window->Update();

	//update windows
	if( GameIndicatorsShown() && psHUD_Flags.is(HUD_DRAW|HUD_DRAW_RT) )
		UIMainIngameWnd->Update	();

	m_pMessagesWnd->Update();
}

void CUIGameCustom::Render()
{
	GameCaptions()->Draw();

	st_vec_it it = m_custom_statics.begin();
	st_vec_it it_e = m_custom_statics.end();
	for(;it!=it_e;++it)
		(*it)->Draw();

	m_window->Draw();

	CEntity* pEntity = smart_cast<CEntity*>(Level().CurrentEntity());
	if (pEntity)
	{
		CActor* pActor = smart_cast<CActor*>(pEntity);
	        if (pActor && pActor->HUDview() && pActor->g_Alive() &&
	            psHUD_Flags.is(HUD_WEAPON | HUD_WEAPON_RT | HUD_WEAPON_RT2))
	        {

			CInventory& inventory = pActor->inventory();
			for (auto slot_idx = inventory.FirstSlot(); slot_idx <= inventory.LastSlot(); ++slot_idx)
			{
				CInventoryItem* item = inventory.ItemFromSlot(slot_idx);
				if (item && item->render_item_ui_query())
				{
					item->render_item_ui();
				}
			}
		}

		if( GameIndicatorsShown() && psHUD_Flags.is(HUD_DRAW | HUD_DRAW_RT) )
		{
			UIMainIngameWnd->Draw();
			m_pMessagesWnd->Draw();
		} else {  //hack - draw messagess wnd in scope mode
			if (!m_PdaMenu->GetVisible())
				m_pMessagesWnd->Draw();
		}	
	}
	else
		m_pMessagesWnd->Draw();

	DoRenderDialogs();
}

void CUIGameCustom::AddCustomMessage		(LPCSTR id, float x, float y, float font_size, CGameFont *pFont, u16 alignment, u32 color)
{
	GameCaptions()->addCustomMessage(id,x,y,font_size,pFont,(CGameFont::EAligment)alignment,color,"");
}

void CUIGameCustom::AddCustomMessage		(LPCSTR id, float x, float y, float font_size, CGameFont *pFont, u16 alignment, u32 color, float flicker )
{
	AddCustomMessage(id,x,y,font_size, pFont, alignment, color);
	GameCaptions()->customizeMessage(id, CUITextBanner::tbsFlicker)->fPeriod = flicker;
}

void CUIGameCustom::CustomMessageOut(LPCSTR id, LPCSTR msg, u32 color)
{
	GameCaptions()->setCaption(id,msg,color,true);
}

void CUIGameCustom::RemoveCustomMessage		(LPCSTR id)
{
	GameCaptions()->removeCustomMessage(id);
}


SDrawStaticStruct* CUIGameCustom::AddCustomStatic(LPCSTR id, bool bSingleInstance)
{
	if(bSingleInstance)
	{
		st_vec::iterator it = std::find_if(m_custom_statics.begin(),m_custom_statics.end(), predicate_find_stat(id) );
		if(it!=m_custom_statics.end())
			return (*it);
	}
	
	CUIXmlInit xml_init;
	m_custom_statics.push_back		( new SDrawStaticStruct() );
	SDrawStaticStruct* sss			= m_custom_statics.back();

	sss->m_static					= new CUIStatic();
	sss->m_name						= id;
	xml_init.InitStatic				(*m_msgs_xml, id, 0, sss->m_static);
	float ttl						= m_msgs_xml->ReadAttribFlt(id, 0, "ttl", -1);
	if(ttl>0.0f)
		sss->m_endTime				= Device.fTimeGlobal + ttl;

	return sss;
}

SDrawStaticStruct* CUIGameCustom::GetCustomStatic(LPCSTR id)
{
	st_vec::iterator it = std::find_if(m_custom_statics.begin(),m_custom_statics.end(), predicate_find_stat(id));
	if(it!=m_custom_statics.end())
		return (*it);

	return nullptr;
}

void CUIGameCustom::RemoveCustomStatic(LPCSTR id)
{
	st_vec::iterator it = std::find_if(m_custom_statics.begin(),m_custom_statics.end(), predicate_find_stat(id) );
	if(it!=m_custom_statics.end())
	{
			delete_data				(*it);
		m_custom_statics.erase	(it);
	}
}

#include "ui/UIGameTutorial.h"

extern CUISequencer* g_tutorial;
extern CUISequencer* g_tutorial2;

void CUIGameCustom::reset_ui()
{
	if(g_tutorial2)
	{ 
		g_tutorial2->Destroy	();
		xr_delete				(g_tutorial2);
	}

	if(g_tutorial)
	{
		g_tutorial->Destroy	();
		xr_delete(g_tutorial);
	}
}

void CUIGameCustom::SetClGame(game_cl_GameState* g)
{
	g->SetGameUI(this);
}

void CUIGameCustom::UnLoad()
{
	xr_delete					(m_pgameCaptions);
	xr_delete					(m_msgs_xml);
	xr_delete					(m_actor_menu_item);
	xr_delete					(m_window);
	xr_delete					(UIMainIngameWnd);
	xr_delete					(m_pMessagesWnd);
	xr_delete					(m_InventoryMenu);
	xr_delete					(m_PdaMenu);
	xr_delete					(m_UICarBodyMenu);
	xr_delete					(m_WeatherEditor);
}

void CUIGameCustom::Load()
{
	if(g_pGameLevel)
	{
		R_ASSERT				(nullptr==m_pgameCaptions);
		m_pgameCaptions				= new CUICaption();

		R_ASSERT				(nullptr==m_msgs_xml);
		m_msgs_xml				= new CUIXml();
		m_msgs_xml->Load		(CONFIG_PATH, UI_PATH, "ui_custom_msgs.xml");

		R_ASSERT				(nullptr==m_actor_menu_item);
		m_actor_menu_item		= new CUIXml();
		m_actor_menu_item->Load	(CONFIG_PATH, UI_PATH, "actor_menu_item.xml");

		R_ASSERT				(nullptr==m_PdaMenu);
		m_PdaMenu				= new CUIPdaWnd			();
		
		R_ASSERT				(nullptr==m_window);
		m_window				= new CUIWindow			();

		R_ASSERT				(nullptr==UIMainIngameWnd);
		UIMainIngameWnd			= new CUIMainIngameWnd	();
		UIMainIngameWnd->Init	();

		R_ASSERT				(nullptr==m_InventoryMenu);
		m_InventoryMenu	= new CUIInventoryWnd	();

		R_ASSERT				(nullptr==m_UICarBodyMenu);
		m_UICarBodyMenu	= new CUICarBodyWnd		();

		R_ASSERT				(nullptr==m_pMessagesWnd);
		m_pMessagesWnd	= new CUIMessagesWindow();

		R_ASSERT				(nullptr==m_WeatherEditor);
		m_WeatherEditor = new CUIWeatherEditor();
		
		Init					(0);
		Init					(1);
		Init					(2);
	}
}

void CUIGameCustom::OnConnected()
{
	if(g_pGameLevel)
	{
		if(!UIMainIngameWnd)
			Load();

		UIMainIngameWnd->OnConnected();
	}
}

void CUIGameCustom::CommonMessageOut(LPCSTR text)
{
	m_pMessagesWnd->AddLogMessage(text);
}

SDrawStaticStruct::SDrawStaticStruct	()
{
	m_static	= nullptr;
	m_endTime	= -1.0f;	
}

void SDrawStaticStruct::destroy()
{
	delete_data(m_static);
}

bool SDrawStaticStruct::IsActual() const
{
	if(m_endTime<0)			return true;
	return (Device.fTimeGlobal < m_endTime);
}

void SDrawStaticStruct::SetText(LPCSTR text)
{
	m_static->Show(text!=nullptr);
	if(text)
	{
		m_static->TextItemControl()->SetTextST(text);
		m_static->ResetColorAnimation();
	}
}


void SDrawStaticStruct::Draw()
{
	if(m_static->IsShown())
		m_static->Draw();
}

void SDrawStaticStruct::Update()
{
	if(IsActual() && m_static->IsShown())	
		m_static->Update();
}

CMapListHelper	gMapListHelper;
xr_token		game_types[];

void CMapListHelper::Load()
{
	string_path					fn;
	FS.update_path				(fn, "$game_config$", "mp\\map_list.ltx");
	CInifile map_list_cfg		(fn);

	//read weathers set
	CInifile::Sect w			= map_list_cfg.r_section("weather");
	CInifile::SectCIt wi		= w.Data.begin();
	CInifile::SectCIt wi_e		= w.Data.end();
	for( ;wi!=wi_e; ++wi)
	{
		m_weathers.resize		(m_weathers.size()+1);
		SGameWeathers& gw		= m_weathers.back();
		gw.m_weather_name		= (*wi).first;
		gw.m_start_time			= (*wi).second;
	}

	//read original maps from config
	CInifile::RootIt it			= map_list_cfg.sections().begin();
	CInifile::RootIt it_e		= map_list_cfg.sections().end();
	for( ;it!=it_e; ++it)
	{
		m_storage.resize		(m_storage.size()+1);
		SGameTypeMaps&	Itm		= m_storage.back();
		Itm.m_game_type_name	= (*it)->Name;
		Itm.m_game_type_id		= (EGameTypes)get_token_id(game_types, Itm.m_game_type_name.c_str() );

		CInifile::SectCIt sit	= (*it)->Data.begin();
		CInifile::SectCIt sit_e	= (*it)->Data.end();
		
		for( ;sit!=sit_e; ++sit)
		{
			SGameTypeMaps::SMapItm	Itm_map;
			Itm_map.map_name				= (*sit).first;

			if(Itm.m_map_names.end()!=std::find(Itm.m_map_names.begin(),Itm.m_map_names.end(),Itm_map))
			{
				Msg("! duplicate map found [%s]", (*sit).first.c_str());
			} else {
//#ifndef MASTER_GOLD
				Msg("added map [%s]", (*sit).first.c_str());
//#endif // #ifndef MASTER_GOLD
				Itm.m_map_names.push_back	(Itm_map);
			}
		}
	}

	// scan for additional maps
	FS_FileSet			fset;
	FS.file_list		(fset,"$game_levels$",FS_ListFiles,"*level.ltx");

	FS_FileSetIt fit	= fset.begin();
	FS_FileSetIt fit_e	= fset.end();

	for( ;fit!=fit_e; ++fit)
	{
		string_path					map_cfg_fn;
		FS.update_path				(map_cfg_fn, "$game_levels$", (*fit).name.c_str());
		CInifile	map_ini			(map_cfg_fn);

		if(map_ini.section_exist("map_usage"))
		{
			CInifile::Sect S			= map_ini.r_section("map_usage");
			CInifile::SectCIt si		= S.Data.begin();
			CInifile::SectCIt si_e		= S.Data.end();
			for( ;si!=si_e; ++si)
			{
				const shared_str& game_type = (*si).first;
				SGameTypeMaps* M			= GetMapListInt(game_type);
				if(!M)
				{
					Msg						("--unknown game type-%s",game_type.c_str());
					m_storage.resize		(m_storage.size()+1);
					SGameTypeMaps&	Itm		= m_storage.back();
					Itm.m_game_type_name	= game_type;
					Itm.m_game_type_id		= (EGameTypes)get_token_id(game_types, game_type.c_str() );
					M						= &m_storage.back();
				}

				shared_str _map_name			= (*fit).name.substr(0,(*fit).name.find('\\')).c_str();
				shared_str _map_ver			= "1";

				SGameTypeMaps::SMapItm	Itm;
				Itm.map_name				= _map_name;
				Itm.map_ver					= _map_ver;
				
				if(M->m_map_names.end()!=std::find(M->m_map_names.begin(),M->m_map_names.end(),Itm))
				{
					Msg("! duplicate map found [%s] [%s]", _map_name.c_str(), _map_ver.c_str());
				} else {
//#ifndef MASTER_GOLD
					Msg("added map [%s] [%s]", _map_name.c_str(), _map_ver.c_str());
//#endif // #ifndef MASTER_GOLD
					M->m_map_names.push_back	(Itm);
				}
			}			
		}
	}

	R_ASSERT2	(m_storage.size(), "unable to fill map list");
	R_ASSERT2	(m_weathers.size(), "unable to fill weathers list");
}

const SGameTypeMaps& CMapListHelper::GetMapListFor(const shared_str& game_type)
{
	if( !m_storage.size() )
		Load		();

	return *GetMapListInt(game_type);
}

SGameTypeMaps* CMapListHelper::GetMapListInt(const shared_str& game_type)
{

	TSTORAGE_CIT it		= m_storage.begin();
	TSTORAGE_CIT it_e	= m_storage.end();
	for( ;it!=it_e; ++it)
	{
		if(game_type==(*it).m_game_type_name )
			return &(*it);
	}
	return nullptr;
}

const SGameTypeMaps& CMapListHelper::GetMapListFor(const EGameTypes game_id)
{
	if( !m_storage.size() )
	{
		Load		();
		R_ASSERT2	(m_storage.size(), "unable to fill map list");
	}
	TSTORAGE_CIT it		= m_storage.begin();
	TSTORAGE_CIT it_e	= m_storage.end();
	for( ;it!=it_e; ++it)
	{
		if(game_id==(*it).m_game_type_id )
			return (*it);
	}
	return m_storage[0];
}

const GAME_WEATHERS& CMapListHelper::GetGameWeathers() 
{
	if(!m_weathers.size())
		Load();

	return m_weathers;
}