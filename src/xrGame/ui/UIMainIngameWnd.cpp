#include "StdAfx.h"

#include "UIMainIngameWnd.h"
#include "UIMessagesWindow.h"
#include "UIZoneMap.h"


#include "../Actor.h"
#include "../ActorCondition.h"
#include "../EntityCondition.h"
#include "../CustomOutfit.h"
#include "../ActorHelmet.h"
#include "../PDA.h"
#include "../xrServerEntities/character_info.h"
#include "../Inventory.h"
#include "UIGameSP.h"
#include "../WeaponMagazined.h"
#include "../Missile.h"
#include "../Grenade.h"
#include "../xrServerEntities/xrServer_objects_ALife.h"
#include "../alife_simulator.h"
#include "../alife_object_registry.h"
#include "../game_cl_base.h"
#include "../Level.h"
#include "../seniority_hierarchy_holder.h"
#include "UIArtefactPanel.h"

#include "../date_time.h"
#include "../xrServerEntities/xrServer_Objects_ALife_Monsters.h"
#include "../../xrEngine/LightAnimLibrary.h"

#include "UIInventoryUtilities.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/UIXmlInit.h"
#include "UIMotionIcon.h"

#include "UIPdaMsgListItem.h"
#include "UIPdaWnd.h"
#include "../alife_registry_wrappers.h"

#include "../../xrEngine/string_table.h"
#include "../../xrEngine/CustomHUD.h"

#ifdef DEBUG
#	include "../attachable_item.h"
#	include "../../xrEngine/xr_input.h"
#endif

#include "../../xrUI/Widgets/UIScrollView.h"
#include "map_hint.h"
#include "../game_news.h"

#include "game_cl_capture_the_artefact.h"
#include "UIHudStatesWnd.h"
#include "UIActorMenu.h"

void test_draw	();
void test_key	(int dik);

#include "../Include/xrRender/Kinematics.h"


using namespace InventoryUtilities;
//BOOL		g_old_style_ui_hud			= FALSE;
const u32	g_clWhite					= 0xffffffff;

#define		DEFAULT_MAP_SCALE			1.f

#define		C_SIZE						0.025f
#define		NEAR_LIM					0.5f

#define		SHOW_INFO_SPEED				0.5f
#define		HIDE_INFO_SPEED				10.f
#define     QUICK_SLOTS_SHOW_SPEED       3.0f
#define     QUICK_SLOTS_HIDE_DELAY       2.0f
#define     QUICK_SLOTS_HIDE_SPEED       4.0f

constexpr auto C_ON_ENEMY = color_xrgb(0xff, 0, 0);
constexpr auto C_DEFAULT = color_xrgb(0xff, 0xff, 0xff);

#define				MAININGAME_XML				"maingame.xml"

CUIMainIngameWnd::CUIMainIngameWnd()
:/*m_pGrenade(nullptr),m_pItem(nullptr),*/m_pPickUpItem(nullptr),m_pMPChatWnd(nullptr),UIArtefactIcon(nullptr),m_pMPLogWnd(nullptr)
{
	UIStaticDiskIO				= nullptr;
	UIZoneMap					= new CUIZoneMap();
	UIWeaponJammedIcon			= nullptr;
	UIInvincibleIcon			= nullptr;
	UIArtefactIcon				= nullptr;
	UIPsyHealthIcon				= nullptr;
	UIStarvationIcon			= nullptr;
	UIRadiaitionIcon			= nullptr;
	UIWoundIcon					= nullptr;
	UIPdaOnline					= nullptr;
	UIStackPanelBoosters		= nullptr;
	UIStackPanelIndicators		= nullptr;
	m_ind_bleeding_svg_inited = false;
	m_ind_weapon_broken_svg_inited = false;
	m_ind_helmet_broken_svg_inited = false;
	m_ind_outfit_broken_svg_inited = false;
	m_ind_overweight_svg_inited = false;
	m_ind_radiation_svg_inited = false;
	m_ind_starvation_svg_inited = false;

	m_ind_boost_psy_svg_inited = false;
	m_ind_boost_radia_svg_inited = false;
	m_ind_boost_chem_svg_inited = false;
	m_ind_boost_wound_svg_inited = false;
	m_ind_boost_weight_svg_inited = false;
	m_ind_boost_health_svg_inited = false;
	m_ind_boost_power_svg_inited = false;
	m_ind_boost_rad_svg_inited = false;
}

#include "../../xrUI/Widgets/UIProgressShape.h"
extern CUIProgressShape* g_MissileForceShape;

CUIMainIngameWnd::~CUIMainIngameWnd()
{
	DestroyFlashingIcons		();
	xr_delete					(UIZoneMap);
	HUD_SOUND_ITEM::DestroySound(m_contactSnd);
	xr_delete					(g_MissileForceShape);
	xr_delete					(UIWeaponJammedIcon);
	xr_delete					(UIInvincibleIcon);
	xr_delete					(UIArtefactIcon);
	xr_delete					(UIPsyHealthIcon);
	xr_delete					(UIStarvationIcon);
	xr_delete					(UIRadiaitionIcon);
	xr_delete					(UIWoundIcon);
}

void CUIMainIngameWnd::Init()
{
	CUIXml						uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, MAININGAME_XML);

	CUIXmlInit					xml_init;
	
	if (uiXml.NavigateToNode("main"))
		xml_init.InitWindow(uiXml, "main", 0, this);

	Enable(false);

	//	AttachChild					(&UIStaticHealth);	xml_init.InitStatic			(uiXml, "static_health", 0, &UIStaticHealth);
	//	AttachChild					(&UIStaticArmor);	xml_init.InitStatic			(uiXml, "static_armor", 0, &UIStaticArmor);
	//	AttachChild					(&UIWeaponBack);
	//	xml_init.InitStatic			(uiXml, "static_weapon", 0, &UIWeaponBack);

	/*	UIWeaponBack.AttachChild	(&UIWeaponSignAmmo);
		xml_init.InitStatic			(uiXml, "static_ammo", 0, &UIWeaponSignAmmo);
		UIWeaponSignAmmo.SetEllipsis	(CUIStatic::eepEnd, 2);

		UIWeaponBack.AttachChild	(&UIWeaponIcon);
		xml_init.InitStatic			(uiXml, "static_wpn_icon", 0, &UIWeaponIcon);
		UIWeaponIcon.SetShader		(GetEquipmentIconsShader());
		UIWeaponIcon_rect			= UIWeaponIcon.GetWndRect();
	*/	//---------------------------------------------------------
	UIPickUpItemIcon = UIHelper::CreateStatic(uiXml, "pick_up_item", this);
	UIPickUpItemIcon->SetShader(GetEquipmentIconsShader());

	m_iPickUpItemIconWidth = UIPickUpItemIcon->GetWidth();
	m_iPickUpItemIconHeight = UIPickUpItemIcon->GetHeight();
	m_iPickUpItemIconX = UIPickUpItemIcon->GetWndRect().left;
	m_iPickUpItemIconY = UIPickUpItemIcon->GetWndRect().top;
	//---------------------------------------------------------

	//индикаторы 
	UIZoneMap->Init();

	// Подсказки, которые возникают при наведении прицела на объект
	UIStaticQuickHelp = UIHelper::CreateStatic(uiXml, "quick_info", this);

	uiXml.SetLocalRoot(uiXml.GetRoot());

	m_UIIcons = new CUIScrollView(); m_UIIcons->SetAutoDelete(true);
	xml_init.InitScrollView(uiXml, "icons_scroll_view", 0, m_UIIcons);
	AttachChild(m_UIIcons);

	if (uiXml.NavigateToNode("indicator_stack_panel", 0))
		UIStackPanelIndicators = UIHelper::CreateStackPanel(uiXml, "indicator_stack_panel", this);

	CUIWindow* indicatorParent = this;
	if (UIStackPanelIndicators)
		indicatorParent = UIStackPanelIndicators;
	
	if (uiXml.NavigateToNode("indicator_bleeding", 0))
		m_ind_bleeding = UIHelper::CreateStatic(uiXml, "indicator_bleeding", indicatorParent);
	if (uiXml.NavigateToNode("indicator_radiation", 0))
		m_ind_radiation = UIHelper::CreateStatic(uiXml, "indicator_radiation", indicatorParent);
	if (uiXml.NavigateToNode("indicator_starvation", 0))
		m_ind_starvation = UIHelper::CreateStatic(uiXml, "indicator_starvation", indicatorParent);
	const static bool enableThirst = EngineExternal()[EEngineExternalGame::EnableThirst];
	if (enableThirst)
		m_ind_thirst = UIHelper::CreateStatic(uiXml, "indicator_thirst", indicatorParent);

	const static bool enableSleepiness = EngineExternal()[EEngineExternalGame::EnableSleepiness];
	if (enableSleepiness)
		m_ind_sleepiness = UIHelper::CreateStatic(uiXml, "indicator_sleepiness", indicatorParent);

	if (uiXml.NavigateToNode("indicator_weapon_broken", 0))
		m_ind_weapon_broken = UIHelper::CreateStatic(uiXml, "indicator_weapon_broken", indicatorParent);
	if (uiXml.NavigateToNode("indicator_helmet_broken", 0))
		m_ind_helmet_broken = UIHelper::CreateStatic(uiXml, "indicator_helmet_broken", indicatorParent);
	if (uiXml.NavigateToNode("indicator_outfit_broken", 0))
		m_ind_outfit_broken = UIHelper::CreateStatic(uiXml, "indicator_outfit_broken", indicatorParent);
	if (uiXml.NavigateToNode("indicator_overweight", 0))
		m_ind_overweight = UIHelper::CreateStatic(uiXml, "indicator_overweight", indicatorParent);

	bool isRaster = EngineExternal().isRenderingUIRaster();

	auto pInitSVGForCUIStatic = [](CUIStatic* pElement, CUIXml& uiXml, bool& svg_init) -> void
		{
			if (!pElement)
				return;

		if (
			pElement->isSVGPresented() && EngineExternal().isRenderingUIRaster()==false)
		{
			R_ASSERT(pElement->WindowNodeName().size() > 0 && "must be valid! otherwise you passed invalid or not initialized element");

			LPCSTR pSVGFilename = pElement->getSVGFilename(uiXml, pElement->WindowNodeName().c_str(), 0);

			if (pSVGFilename)
			{
				Fvector2 scaled_w_and_h;
				UI().ClientToScreenScaled(scaled_w_and_h, pElement->GetWidth(), pElement->GetHeight());

				float fRequestedWidth = scaled_w_and_h.x;
				float fRequestedHeight = scaled_w_and_h.y;

				const ui_shader& svg_shader = UI().GetVectorShader(pSVGFilename, fRequestedWidth, fRequestedHeight);
				const Frect& svg_uv = UI().GetVectorUV(pSVGFilename, fRequestedWidth, fRequestedHeight);

				pElement->SetShader(svg_shader);
				pElement->SetTextureRect(svg_uv);

				// virtual callings are not cheap and for runtime better to reduce that overhead tbh so we have to cache at init stage 
				svg_init = true;
			}
		}
#ifdef DEBUG
		else
		{
			Msg("! [svg]: nor attribute nor nested node was presented for <%s>", pElement->WindowNodeName().c_str());
		}
#endif

		};

	// todo: refactor and make function that accept CUIStatic and initialize others...
	if (!isRaster)
	{

		pInitSVGForCUIStatic(m_ind_bleeding, uiXml, m_ind_bleeding_svg_inited);

		pInitSVGForCUIStatic(m_ind_weapon_broken, uiXml, m_ind_weapon_broken_svg_inited);
		pInitSVGForCUIStatic(m_ind_helmet_broken, uiXml, m_ind_helmet_broken_svg_inited);
		pInitSVGForCUIStatic(m_ind_outfit_broken, uiXml, m_ind_outfit_broken_svg_inited);
		pInitSVGForCUIStatic(m_ind_overweight, uiXml, m_ind_overweight_svg_inited);
		pInitSVGForCUIStatic(m_ind_radiation, uiXml, m_ind_radiation_svg_inited);
		pInitSVGForCUIStatic(m_ind_starvation, uiXml, m_ind_starvation_svg_inited);

	}


	if (!IsGameTypeSingle())
	{
		// Voice chat
		if (uiXml.NavigateToNode("icon_microphone"))
		{
			m_icon_microphone = UIHelper::CreateStatic(uiXml, "icon_microphone", this);
			m_icon_microphone->Show(true);
		}
		if (uiXml.NavigateToNode("voice_distance"))
		{
			m_voice_distance = UIHelper::CreateStatic(uiXml, "voice_distance", this);
		}

		SetActiveVoiceIcon(false);
	}

	if (uiXml.NavigateToNode("indicator_booster_stack_panel", 0))
		UIStackPanelBoosters = UIHelper::CreateStackPanel(uiXml, "indicator_booster_stack_panel", this);

	CUIWindow* boosterParent = this;
	if (UIStackPanelBoosters)
		boosterParent = UIStackPanelBoosters;

	if (uiXml.NavigateToNode("indicator_booster_psy", 0))
	{
		m_ind_boost_psy = UIHelper::CreateStatic(uiXml, "indicator_booster_psy", boosterParent);
		m_ind_boost_psy->Show(false);
	}
	if (uiXml.NavigateToNode("indicator_booster_radia", 0))
	{
		m_ind_boost_radia = UIHelper::CreateStatic(uiXml, "indicator_booster_radia", boosterParent);
		m_ind_boost_radia->Show(false);
	}
	if (uiXml.NavigateToNode("indicator_booster_chem", 0))
	{
		m_ind_boost_chem = UIHelper::CreateStatic(uiXml, "indicator_booster_chem", boosterParent);
		m_ind_boost_chem->Show(false);
	}
	if (uiXml.NavigateToNode("indicator_booster_wound", 0))
	{
		m_ind_boost_wound = UIHelper::CreateStatic(uiXml, "indicator_booster_wound", boosterParent);
		m_ind_boost_wound->Show(false);
	}
	if (uiXml.NavigateToNode("indicator_booster_weight", 0))
	{
		m_ind_boost_weight = UIHelper::CreateStatic(uiXml, "indicator_booster_weight", boosterParent);
		m_ind_boost_weight->Show(false);
	}
	if (uiXml.NavigateToNode("indicator_booster_health", 0))
	{
		m_ind_boost_health = UIHelper::CreateStatic(uiXml, "indicator_booster_health", boosterParent);
		m_ind_boost_health->Show(false);
	}
	if (uiXml.NavigateToNode("indicator_booster_power", 0))
	{
		m_ind_boost_power = UIHelper::CreateStatic(uiXml, "indicator_booster_power", boosterParent);
		m_ind_boost_power->Show(false);
	}
	if (uiXml.NavigateToNode("indicator_booster_rad", 0))
	{
		m_ind_boost_rad = UIHelper::CreateStatic(uiXml, "indicator_booster_rad", boosterParent);
		m_ind_boost_rad->Show(false);
	}
	
	useLegacyIndicators = !EngineExternal().ClearSkyMode();

	// Загружаем иконки 
	if ( IsGameTypeSingle() )
	{
		if (uiXml.NavigateToNode("starvation_static"))
		{
			UIStarvationIcon = UIHelper::CreateStatic(uiXml, "starvation_static", nullptr);
			UIStarvationIcon->Show(false);
		}

		if (uiXml.NavigateToNode("psy_health_static"))
		{
			UIPsyHealthIcon = UIHelper::CreateStatic(uiXml, "psy_health_static", nullptr);
			UIPsyHealthIcon->Show(false);
		}
	}

	if (uiXml.NavigateToNode("weapon_jammed_static") && !m_ind_weapon_broken)
	{
		UIWeaponJammedIcon = UIHelper::CreateStatic(uiXml, "weapon_jammed_static", nullptr);
		UIWeaponJammedIcon->Show(false);
	}
	if (uiXml.NavigateToNode("radiation_static"))
	{
		UIRadiaitionIcon = UIHelper::CreateStatic(uiXml, "radiation_static", nullptr);
		UIRadiaitionIcon->Show(false);
	}

	if (uiXml.NavigateToNode("wound_static"))
	{
		UIWoundIcon = UIHelper::CreateStatic(uiXml, "wound_static", nullptr);
		UIWoundIcon->Show(false);
	}

	UIInvincibleIcon = UIHelper::CreateStatic(uiXml, "invincible_static", nullptr);
	UIInvincibleIcon->Show(false);


	if ((GameID() == eGameIDArtefactHunt) || (GameID() == eGameIDCaptureTheArtefact))
	{
		UIArtefactIcon = UIHelper::CreateStatic(uiXml, "artefact_static", nullptr);
		UIArtefactIcon->Show(false);
	}

	shared_str warningStrings[7] =
	{
		"jammed",
		"radiation",
		"wounds",
		"starvation",
		"fatigue",
		"invincible",
		"artefact"
	};

	// Загружаем пороговые значения для индикаторов
	EWarningIcons j = ewiWeaponJammed;
	while (j < ewiInvincible)
	{
		// Читаем данные порогов для каждого индикатора
		shared_str cfgRecord = pSettings->r_string("main_ingame_indicators_thresholds", *warningStrings[static_cast<int>(j) - 1]);
		u32 count = _GetItemCount(*cfgRecord);

		char	singleThreshold[8];
		float	f = 0;
		for (u32 k = 0; k < count; ++k)
		{
			_GetItem(*cfgRecord, k, singleThreshold);
			sscanf(singleThreshold, "%f", &f);

			m_Thresholds[j].push_back(f);
		}

		j = static_cast<EWarningIcons>(j + 1);
	}


	// Flashing icons initialize
	uiXml.SetLocalRoot(uiXml.NavigateToNode("flashing_icons"));
	InitFlashingIcons(&uiXml);

	uiXml.SetLocalRoot(uiXml.GetRoot());

	// Car
	UICarPanel.Init();
	AttachChild(&UICarPanel);

	// Icons
	UIMotionIcon = new CUIMotionIcon(); UIMotionIcon->SetAutoDelete(true);
	const bool independent = UIMotionIcon->Init(UIZoneMap->MapFrame().GetWndRect());
	if (!independent)
		UIZoneMap->MapFrame().AttachChild(UIMotionIcon);
	else
		AttachChild(UIMotionIcon);

	if (uiXml.NavigateToNode("artefact_panel") && IsGameTypeSingle())
	{
		m_artefactPanel = new CUIArtefactPanel();
		m_artefactPanel->InitFromXML(uiXml, "artefact_panel", 0);
		this->AttachChild(m_artefactPanel);
	}

	m_ui_hud_states = new CUIHudStatesWnd();
	m_ui_hud_states->SetAutoDelete(true);
	AttachChild(m_ui_hud_states);
	m_ui_hud_states->InitFromXml(uiXml, "hud_states");

	if (uiXml.NavigateToNode("static_pda_online") && IsGameTypeSingleCompatible())
	{
		UIPdaOnline = new CUIStatic();
		xml_init.InitStatic(uiXml, "static_pda_online", 0, UIPdaOnline);
		UIZoneMap->Background().AttachChild(UIPdaOnline);
	}

	if (uiXml.NavigateToNode("disk_io"))
	{
		UIStaticDiskIO = UIHelper::CreateStatic(uiXml, "disk_io", this);
	}
	else
	{
		UIStaticDiskIO = new CUIStatic();
		AttachChild(UIStaticDiskIO);
		UIStaticDiskIO->SetWndPos(Fvector2().set(1000, 750));
		UIStaticDiskIO->SetWndSize(Fvector2().set(16, 16));
		UIStaticDiskIO->InitTexture("ui\\ui_disk_io");
		UIStaticDiskIO->SetTextureRect(Frect().set(0.f / UI().get_current_kx(), 0.f, 32 / UI().get_current_kx(), 32));
		UIStaticDiskIO->SetStretchTexture(true);
	}

	for (int i = 0; i < 4; i++)
	{
		string32 path;
		xr_sprintf(path, "quick_slot%d", i);
		if (uiXml.NavigateToNode(path))
		{
			m_quick_slots_icons.push_back(new CUIStatic());
			m_quick_slots_icons.back()->SetAutoDelete(true);
			AttachChild(m_quick_slots_icons.back());
			CUIXmlInit::InitStatic(uiXml, path, 0, m_quick_slots_icons.back());
			xr_sprintf(path, "%s:counter", path);
			UIHelper::CreateStatic(uiXml, path, m_quick_slots_icons.back());
		}
	}
	if (uiXml.NavigateToNode("quick_slot0_text", 0))
		m_QuickSlotText1 = UIHelper::CreateStatic(uiXml, "quick_slot0_text", this);
	if (uiXml.NavigateToNode("quick_slot1_text", 0))
		m_QuickSlotText2 = UIHelper::CreateStatic(uiXml, "quick_slot1_text", this);
	if (uiXml.NavigateToNode("quick_slot2_text", 0))
		m_QuickSlotText3 = UIHelper::CreateStatic(uiXml, "quick_slot2_text", this);
	if (uiXml.NavigateToNode("quick_slot3_text", 0))
		m_QuickSlotText4 = UIHelper::CreateStatic(uiXml, "quick_slot3_text", this);

	HUD_SOUND_ITEM::LoadSound("maingame_ui", "snd_new_contact", m_contactSnd, SOUND_TYPE_IDLE);


	// Quick slots panel hidden by default
	m_quick_slots_visible = EngineExternal()[EEngineExternalUI::ShowQuickSlotByDefault];
	m_quick_slots_force_visible = false; // Never force visibility on init to allow auto-hide
	m_quick_slots_force_visible_by_key = false; // Track if force visibility was set by key press
	m_quick_slots_alpha = 0.0f;
	m_quick_slots_last_interaction_time = 0.0f; // Initialize to 0 instead of negative value

	// Скрываем все элементы панели по умолчанию
	if (!m_quick_slots_visible)
	{
		for (const auto& slot : m_quick_slots_icons)
		{
			slot->Show(false);
			CUIWindow* counter = slot->FindChild("counter");
			if (counter)
				counter->Show(false);
		}
		if (m_QuickSlotText1) m_QuickSlotText1->Show(false);
		if (m_QuickSlotText2) m_QuickSlotText2->Show(false);
		if (m_QuickSlotText3) m_QuickSlotText3->Show(false);
		if (m_QuickSlotText4) m_QuickSlotText4->Show(false);
	}
}

float UIStaticDiskIO_start_time = 0.0f;
void CUIMainIngameWnd::Draw()
{
	CActor* pActor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;

	// show IO icon
	bool IOActive	= (FS.dwOpenCounter>0);
	if	(IOActive)	UIStaticDiskIO_start_time = Device.fTimeGlobal;

	if ((UIStaticDiskIO_start_time+1.0f) < Device.fTimeGlobal)	UIStaticDiskIO->Show(false); 
	else {
		u32		alpha			= clampr(iFloor(255.f*(1.f-(Device.fTimeGlobal-UIStaticDiskIO_start_time)/1.f)),0,255);
		UIStaticDiskIO->Show		( true  ); 
		UIStaticDiskIO->SetTextureColor(color_rgba(255,255,255,alpha));
	}
	FS.dwOpenCounter = 0;

	if (!IsGameTypeSingle())
	{
		float luminocity = Level().CurrentEntity()->cast_game_object()->ROS()->get_luminocity();
		float power = log(luminocity > .001f ? luminocity : .001f) * (1.f);
		luminocity = exp(power);

		static float cur_lum = luminocity;
		cur_lum = luminocity*0.01f + cur_lum*0.99f;
		UIMotionIcon->SetLuminosity((s16)iFloor(cur_lum*100.0f));
	}
	if (!pActor || !pActor->g_Alive())
	{
		return;
	}

	UIMotionIcon->SetNoise((s16)(0xffff&iFloor(pActor->m_snd_noise*100)));

	UIMotionIcon->Draw();


	const static bool noHUDonMaster = EngineExternal()[EEngineExternalUI::DisableHudRenderingOnMaster];
	if (noHUDonMaster)
	{
		bool renderHUD = noHUDonMaster ? g_SingleGameDifficulty < egdVeteran : true;
		UIZoneMap->disabled = !renderHUD;
	}

	if (psHUD_Flags.test(HUD_MINIMAP))
	{
		UIZoneMap->visible = true;
		UIZoneMap->Render();
	}

	bool tmp = UIMotionIcon->IsShown();
	UIMotionIcon->Show(false);
	CUIWindow::Draw();
	UIMotionIcon->Show(tmp);

	RenderQuickInfos();		
}

void CUIMainIngameWnd::SetActiveVoiceIcon(bool active)
{
	u32 a = active ? 255 : 100;
	if (m_icon_microphone)
	{
		u32 color = m_icon_microphone->GetTextureColor();
		m_icon_microphone->SetTextureColor(subst_alpha(color, a));
	}
	if (m_voice_distance)
	{
		u32 color = m_voice_distance->GetTextColor();
		m_voice_distance->SetTextColor(subst_alpha(color, a));
	}
}

void CUIMainIngameWnd::SetVoiceDistance(u8 distance)
{
	if (!m_voice_distance)
		return;

	string16 text;
	xr_sprintf(text, sizeof(text), "%u", distance);
	m_voice_distance->SetText(text);
}

void CUIMainIngameWnd::SetMPChatLog(CUIWindow* pChat, CUIWindow* pLog){
	m_pMPChatWnd = pChat;
	m_pMPLogWnd  = pLog;
}

void CUIMainIngameWnd::Update()
{
	CUIWindow::Update();
	CActor* pActor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;

	if (m_pMPChatWnd)
	{
		m_pMPChatWnd->Update();
	}

	if (m_pMPLogWnd)
	{
		m_pMPLogWnd->Update();
	}

	if (!pActor)
	{
		return;
	}

	if (psHUD_Flags.test(HUD_MINIMAP))
	{
		UIZoneMap->Update();
	}
	
//	UIHealthBar.SetProgressPos	(m_pActor->GetfHealth()*100.0f);
	UIMotionIcon->SetPower		(pActor->conditions().GetPower()*100.0f);
	
	UpdatePickUpItem			();

	TickQuickSlotsPanelFade();
	
	if( Device.dwFrame % 10 )
		return;

	game_PlayerState* lookat_player = Game().local_player;
	if (Level().IsDemoPlayStarted())
	{
		lookat_player = Game().lookat_player();
	}

	if (UIPdaOnline && !(Device.dwFrame % 20) && IsGameTypeSingleCompatible())
	{
		string256				text_str;
		CPda* _pda = pActor->GetPDA();
		u32 _cn = 0;
		if (_pda && 0 != (_cn = _pda->ActiveContactsNum()))
		{
			sprintf_s(text_str, "%d", _cn);
			UIPdaOnline->SetText(text_str);
		}
		else
		{
			UIPdaOnline->SetText("");
		}
	};

	bool b_God = ( GodMode() || ( !lookat_player ) )? true : lookat_player->testFlag(GAME_PLAYER_FLAG_INVINCIBLE);
	if ( b_God )
	{
		SetWarningIconColor( ewiInvincible, 0xffffffff );
	}
	else
	{
		SetWarningIconColor( ewiInvincible, 0x00ffffff );
	}
	
	UpdateMainIndicators();
	if (IsGameTypeSingle())
		return;

	// ewiArtefact
	if ( GameID() == eGameIDArtefactHunt )
	{
		bool b_Artefact = !!( pActor->inventory().ItemFromSlot(ARTEFACT_SLOT) );
		if ( b_Artefact )
		{
			SetWarningIconColor( ewiArtefact, 0xffffff00 );
		}
		else
		{
			SetWarningIconColor( ewiArtefact, 0x00ffffff );
		}
	}
	else if ( GameID() == eGameIDCaptureTheArtefact )
	{
		//this is a bad style... It left for backward compatibility
		//need to move this logic into UIGameCTA class
		//bool b_Artefact = (nullptr != m_pActor->inventory().ItemFromSlot(ARTEFACT_SLOT));
		game_cl_CaptureTheArtefact* cta_game = static_cast<game_cl_CaptureTheArtefact*>(&Game());
		R_ASSERT(cta_game);
		R_ASSERT(lookat_player);
		
		if ( ( pActor->ID() == cta_game->GetGreenArtefactOwnerID() ) ||
			 ( pActor->ID() == cta_game->GetBlueArtefactOwnerID()  ) )
		{
			SetWarningIconColor( ewiArtefact, 0xffff0000 );
		}
		else if ( pActor->inventory().ItemFromSlot(ARTEFACT_SLOT) ) //own artefact
		{
			SetWarningIconColor( ewiArtefact, 0xff00ff00 );
		}
		else
		{
			SetWarningIconColor(ewiArtefact, 0x00ffffff );
		}
	}

	if (!useLegacyIndicators)
		return;

	EWarningIcons i = ewiWeaponJammed;

	while (i < ewiInvincible)
	{
		float value = 0;
		switch (i)
		{
			//radiation
		case ewiRadiation:
			value = pActor->conditions().GetRadiation();
			break;
		case ewiWound:
			value = pActor->conditions().BleedingSpeed();
			break;
		case ewiWeaponJammed:
		{
			u16 slot = pActor->inventory().GetActiveSlot();
			CWeapon* weapon = smart_cast<CWeapon*>(pActor->inventory().ItemFromSlot(slot));
			if (weapon)
				value = 1 - weapon->GetConditionToShow();
			break;
		}
		case ewiStarvation:
			value = 1 - pActor->conditions().GetSatiety();
			break;
		case ewiPsyHealth:
			value = 1 - pActor->conditions().GetPsyHealth();
			break;
		default:
			R_ASSERT(!"Unknown type of warning icon");
		}

		xr_vector<float>::reverse_iterator	rit;

		// Сначала проверяем на точное соответсвие
		rit = std::find(m_Thresholds[i].rbegin(), m_Thresholds[i].rend(), value);

		// Если его нет, то берем последнее меньшее значение ()
		if (rit == m_Thresholds[i].rend()) {
			rit = std::find_if(m_Thresholds[i].rbegin(), m_Thresholds[i].rend(),
				[value](float threshold) { return threshold < value; });
		}
		// Минимальное и максимальное значения границы
		float min = m_Thresholds[i].front();
		float max = m_Thresholds[i].back();

		if (rit != m_Thresholds[i].rend()) {
			float v = *rit;
			SetWarningIconColor(i, color_argb(0xFF, clampr<u32>(static_cast<u32>(255 * ((v - min) / (max - min) * 2)), 0, 255),
				clampr<u32>(static_cast<u32>(255 * (2.0f - (v - min) / (max - min) * 2)), 0, 255),
				0));
		}
		else
			TurnOffWarningIcon(i);

		i = (EWarningIcons)(i + 1);
	}
}//update


void CUIMainIngameWnd::RenderQuickInfos()
{
	CActor* pActor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
	if (!pActor)
	{
		return;
	}

	static CGameObject *pObject = nullptr;
	LPCSTR actor_action	= pActor->GetDefaultActionForObject();
	UIStaticQuickHelp->Show(nullptr!=actor_action);

	if (nullptr != actor_action)
	{
		if (_stricmp(actor_action,UIStaticQuickHelp->GetText()))
		{
			UIStaticQuickHelp->SetTextST(actor_action);
		}
	}

	if (pObject != pActor->ObjectWeLookingAt())
	{
		UIStaticQuickHelp->SetTextST(actor_action ? actor_action : " ");
		UIStaticQuickHelp->ResetColorAnimation();
		pObject	= pActor->ObjectWeLookingAt();
	}
}

void CUIMainIngameWnd::ShowQuickSlotsPanel()
{
	m_quick_slots_visible = true;
	m_quick_slots_force_visible = false; // Reset force visibility to allow auto-hide
	m_quick_slots_last_interaction_time = Device.fTimeGlobal;
}

void CUIMainIngameWnd::HideQuickSlotsPanelImmediate()
{
	m_quick_slots_visible = false;
	m_quick_slots_force_visible = false;
	m_quick_slots_force_visible_by_key = false;
	m_quick_slots_alpha = 0.0f;
}

void CUIMainIngameWnd::SetQuickSlotsPanelVisible(bool visible)
{
	// Only update force_visible_by_key flag when setting to true
	// This allows us to track if panel was shown by key press
	if (visible)
	{
		m_quick_slots_force_visible = true;
		m_quick_slots_force_visible_by_key = true; // Track that visibility was set by key
		m_quick_slots_last_interaction_time = Device.fTimeGlobal;
	}
	else
	{
		// Only hide if panel was actually shown by key press
		// This prevents hiding when key binding is changed and old key is released
		if (m_quick_slots_force_visible_by_key)
		{
			m_quick_slots_force_visible = false;
			m_quick_slots_force_visible_by_key = false;
		}
	}
}

void CUIMainIngameWnd::TickQuickSlotsPanelFade()
{
	const float dt = Device.fTimeDelta;
	if (dt <= 0.0f)
	{
		return; // Protection against invalid delta time
	}

	// If hide quick slots option is disabled, panel is always visible
	const bool isHideQuickSlotsEnabled = psHUD_Flags.test(HUD_HIDE_QUICK_SLOTS);
	if (!isHideQuickSlotsEnabled)
	{
		m_quick_slots_alpha = 1.0f;
		m_quick_slots_visible = true;
		return;
	}

	bool should_be_visible = m_quick_slots_force_visible || m_quick_slots_visible;

	if (should_be_visible)
	{

		m_quick_slots_alpha += QUICK_SLOTS_SHOW_SPEED * dt;
		if (m_quick_slots_alpha > 1.0f)
			m_quick_slots_alpha = 1.0f;

		if (!m_quick_slots_force_visible && m_quick_slots_visible)
		{
			if ((Device.fTimeGlobal - m_quick_slots_last_interaction_time) >= QUICK_SLOTS_HIDE_DELAY)
			{
				m_quick_slots_visible = false;
			}
		}
	}
	else
	{

		if (m_quick_slots_alpha > 0.0f)
		{
			m_quick_slots_alpha -= QUICK_SLOTS_HIDE_SPEED * dt;
			if (m_quick_slots_alpha < 0.0f)
				m_quick_slots_alpha = 0.0f;
		}
	}
}

void CUIMainIngameWnd::ReceiveNews(GAME_NEWS_DATA* news)
{
	VERIFY(news->texture_name.size());

	CurrentGameUI()->m_pMessagesWnd->AddIconedPdaMessage(news);
	CurrentGameUI()->UpdatePda();
}

void CUIMainIngameWnd::SetWarningIconColorUI(CUIStatic* s, const u32 cl)
{
	int bOn = ( cl >> 24 );
	bool bIsShown = s->IsShown();

	if ( bOn )
	{
		s->SetTextureColor( cl );
	}

	if ( bOn && !bIsShown )
	{
		m_UIIcons->AddWindow	(s, false);
		s->Show					(true);
	}

	if ( !bOn && bIsShown )
	{
		m_UIIcons->RemoveWindow	(s);
		s->Show					(false);
	}
}

void CUIMainIngameWnd::SetWarningIconColor(EWarningIcons icon, const u32 cl)
{
	bool bMagicFlag = true;

	// Задаем цвет требуемой иконки
	switch(icon)
	{
	case ewiAll:
		bMagicFlag = false;
	case ewiWeaponJammed:
		if (UIWeaponJammedIcon && !m_ind_weapon_broken)
			SetWarningIconColorUI	(UIWeaponJammedIcon, cl);
		if (bMagicFlag) break;
	case ewiRadiation:
	{
		if (UIRadiaitionIcon)
			SetWarningIconColorUI(UIRadiaitionIcon, cl);
		if (bMagicFlag) break;
	}
	case ewiWound:
	{
		if (UIWoundIcon)
			SetWarningIconColorUI(UIWoundIcon, cl);
		if (bMagicFlag) break;
	}
	case ewiStarvation:
	{
		if (UIStarvationIcon)
			SetWarningIconColorUI(UIStarvationIcon, cl);
		if (bMagicFlag) break;	
	}
	case ewiPsyHealth:
	{
		if (UIPsyHealthIcon)
			SetWarningIconColorUI(UIPsyHealthIcon, cl);
		if (bMagicFlag) break;
	}
	case ewiInvincible:
		SetWarningIconColorUI	(UIInvincibleIcon, cl);
		if (bMagicFlag) break;
		break;
	case ewiArtefact:
		SetWarningIconColorUI	(UIArtefactIcon, cl);
		break;

	default:
		R_ASSERT(!"Unknown warning icon type");
	}
}

void CUIMainIngameWnd::TurnOffWarningIcon(EWarningIcons icon)
{
	SetWarningIconColor(icon, 0x00ffffff);
}

void CUIMainIngameWnd::SetFlashIconState_(EFlashingIcons type, bool enable)
{
	// Включаем анимацию требуемой иконки
	FlashingIcons_it icon = m_FlashingIcons.find(type);
	R_ASSERT2(icon != m_FlashingIcons.end(), "Flashing icon with this type not existed");
	icon->second->Show(enable);
}

void CUIMainIngameWnd::InitFlashingIcons(CUIXml* node)
{
	const char * const flashingIconNodeName = "flashing_icon";
	int staticsCount = node->GetNodesNum("", 0, flashingIconNodeName);

	CUIXmlInit xml_init;
	CUIStatic *pIcon = nullptr;
	// Пробегаемся по всем нодам и инициализируем из них статики
	for (int i = 0; i < staticsCount; ++i)
	{
		pIcon = new CUIStatic();
		xml_init.InitStatic(*node, flashingIconNodeName, i, pIcon);
		shared_str iconType = node->ReadAttrib(flashingIconNodeName, i, "type", "none");

		// Теперь запоминаем иконку и ее тип
		EFlashingIcons type = efiPdaTask;

		if		(iconType == "pda")		type = efiPdaTask;
		else if (iconType == "mail")	type = efiMail;
		else	R_ASSERT(!"Unknown type of mainingame flashing icon");

		R_ASSERT2(m_FlashingIcons.find(type) == m_FlashingIcons.end(), "Flashing icon with this type already exists");

		CUIStatic* &val	= m_FlashingIcons[type];
		val			= pIcon;

		AttachChild(pIcon);
		pIcon->Show(false);
	}
}

void CUIMainIngameWnd::DestroyFlashingIcons()
{
	for (FlashingIcons_it it = m_FlashingIcons.begin(); it != m_FlashingIcons.end(); ++it)
	{
		DetachChild(it->second);
		xr_delete(it->second);
	}

	m_FlashingIcons.clear();
}

void CUIMainIngameWnd::UpdateFlashingIcons()
{
	for (FlashingIcons_it it = m_FlashingIcons.begin(); it != m_FlashingIcons.end(); ++it)
	{
		it->second->Update();
	}
}

void CUIMainIngameWnd::AnimateContacts(bool b_snd)
{
	UIZoneMap->Counter_ResetClrAnimation();

	if(b_snd)
		HUD_SOUND_ITEM::PlaySound	(m_contactSnd, Fvector().set(0,0,0), 0, true );

}


void CUIMainIngameWnd::SetPickUpItem	(CInventoryItem* PickUpItem)
{
	m_pPickUpItem = PickUpItem;
};

void CUIMainIngameWnd::UpdatePickUpItem	()
{
	if (!m_pPickUpItem || !Level().CurrentViewEntity() || !Level().CurrentViewEntity()->cast_actor()) 
	{
		UIPickUpItemIcon->Show(false);
		return;
	};


	shared_str sect_name	= m_pPickUpItem->object().cNameSect();

	bool isRaster = EngineExternal().isRenderingUIRaster();
	if (!isRaster)
	{
		if (EngineExternal().isRenderingUIErrorFallbackToDefaultAtlas()==false)
		{
			isRaster = !(pSettings->line_exist(sect_name, kUIConfigField_InventoryVectorIcon));
		}
	}

	int m_iGridWidth = pSettings->r_u32(sect_name, "inv_grid_width");
	int m_iGridHeight = pSettings->r_u32(sect_name, "inv_grid_height");
	int m_iXPos = pSettings->r_u32(sect_name, "inv_grid_x");
	int m_iYPos = pSettings->r_u32(sect_name, "inv_grid_y");

	float scaleIcon = m_pPickUpItem->ScaleIcon;

	UIPickUpItemIcon->SetShader(InventoryUtilities::GetEquipmentIconsShader(m_pPickUpItem->IconsTexture.c_str()));

	float scale_x = m_iPickUpItemIconWidth /
		float(m_iGridWidth * INV_GRID_WIDTH(scaleIcon));
	float scale_y = m_iPickUpItemIconHeight /
		float(m_iGridHeight * INV_GRID_HEIGHT(scaleIcon));

	scale_x = (scale_x > 1) ? 1.0f : scale_x;
	scale_y = (scale_y > 1) ? 1.0f : scale_y;

	if (scaleIcon > 1.0f)
	{
		scale_x = m_iPickUpItemIconWidth /
			(m_iGridWidth * INV_GRID_WIDTH(scaleIcon) / 2);
		scale_y = m_iPickUpItemIconHeight /
			(m_iGridHeight * INV_GRID_HEIGHT(scaleIcon) / 2);

		scale_x = (scale_x > 1) ? 0.5f : scale_x / 2;
		scale_y = (scale_y > 1) ? 0.5f : scale_y / 2;
	}

	float scale = scale_x < scale_y ? scale_x : scale_y;
	Frect texture_rect = {};

	//properties used by inventory menu
	if (isRaster)
	{
		const char* icons_texture = READ_IF_EXISTS(pSettings, r_string, sect_name.c_str(), "icons_texture", nullptr);
		const ui_shader& ui_shader = InventoryUtilities::GetEquipmentIconsShader(icons_texture);
		UIPickUpItemIcon->SetShader(ui_shader);

		texture_rect.lt.set(m_iXPos * INV_GRID_WIDTH(scaleIcon), m_iYPos * INV_GRID_HEIGHT(scaleIcon));
		texture_rect.rb.set(m_iGridWidth * INV_GRID_WIDTH(scaleIcon), m_iGridHeight * INV_GRID_HEIGHT(scaleIcon));
		texture_rect.rb.add(texture_rect.lt);
	}
	else
	{
		float fRequestedWidth = m_iGridWidth * INV_GRID_WIDTH(scaleIcon) * scale * UI().get_current_kx();
		float fRequestedHeight = m_iGridHeight * INV_GRID_HEIGHT(scaleIcon) * scale;

		Fvector2 vRealWH;
		UI().ClientToScreenScaled(vRealWH, fRequestedWidth, fRequestedHeight);

		if (pSettings->line_exist(sect_name, kUIConfigField_InventoryVectorIcon))
		{
			xr_string_view icon_subpath = pSettings->r_string(sect_name, kUIConfigField_InventoryVectorIcon);

			if (icon_subpath.empty() == false)
			{
				const ui_shader& svg_shader = UI().GetVectorShader(icon_subpath, vRealWH.x, vRealWH.y);

				texture_rect = UI().GetVectorUV(icon_subpath, vRealWH.x, vRealWH.y);

				UIPickUpItemIcon->SetShader(svg_shader);
			}
			else
			{
				const ui_shader& default_shader = UI().GetVectorShader(_kDefaultSVGShader, vRealWH.x, vRealWH.y);

				texture_rect = UI().GetVectorUV(_kDefaultSVGShader, vRealWH.x, vRealWH.y);
				UIPickUpItemIcon->SetShader(default_shader);
			}
		}
		else
		{
			const ui_shader& default_shader = UI().GetVectorShader(_kDefaultSVGShader, vRealWH.x, vRealWH.y);
			texture_rect = UI().GetVectorUV(_kDefaultSVGShader, vRealWH.x, vRealWH.y);
			UIPickUpItemIcon->SetShader(default_shader);
		}
	}

	UIPickUpItemIcon->GetStaticItem()->SetTextureRect(texture_rect);
	UIPickUpItemIcon->SetWidth(m_iGridWidth * INV_GRID_WIDTH(scaleIcon) * scale * UI().get_current_kx());
	UIPickUpItemIcon->SetHeight(m_iGridHeight * INV_GRID_HEIGHT(scaleIcon) * scale);
	UIPickUpItemIcon->SetWndPos(Fvector2().set(m_iPickUpItemIconX + (m_iPickUpItemIconWidth - UIPickUpItemIcon->GetWidth()) / 2.0f,
		m_iPickUpItemIconY + (m_iPickUpItemIconHeight - UIPickUpItemIcon->GetHeight()) / 2.0f));
	UIPickUpItemIcon->SetTextureColor(color_rgba(255, 255, 255, 192));

	UIPickUpItemIcon->SetStretchTexture(true);
	UIPickUpItemIcon->Show(true);
};

void CUIMainIngameWnd::OnConnected()
{
	UIZoneMap->SetupCurrentMap();
	if ( m_ui_hud_states )
	{
		m_ui_hud_states->on_connected();
	}
}

void CUIMainIngameWnd::OnSectorChanged(int sector)
{
	UIZoneMap->OnSectorChanged(sector);
}

void CUIMainIngameWnd::reset_ui()
{
	m_pPickUpItem					= nullptr;
	UIMotionIcon->ResetVisibility	();
	if ( m_ui_hud_states )
	{
		m_ui_hud_states->reset_ui();
	}
}

void CUIMainIngameWnd::ShowZoneMap( bool status ) 
{ 
	UIZoneMap->visible = status; 
}

void CUIMainIngameWnd::DrawZoneMap() 
{
	if (psHUD_Flags.test(HUD_MINIMAP))
	{
		UIZoneMap->Render();
	}
}

void CUIMainIngameWnd::UpdateZoneMap() 
{
	if (psHUD_Flags.test(HUD_MINIMAP))
	{
		UIZoneMap->Update();
	}
}

void CUIMainIngameWnd::UpdateMainIndicators()
{
	CActor* pActor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
	if (!pActor)
	{
		return;
	}

	UpdateQuickSlots();
	if (IsGameTypeSingleCompatible())
		CurrentGameUI()->PdaMenu()->UpdateRankingWnd();

	u8 flags = 0;
	flags |= LA_CYCLIC;
	flags |= LA_ONLYALPHA;
	flags |= LA_TEXTURECOLOR;
// Bleeding icon
	float bleeding = pActor->conditions().BleedingSpeed();
	if (m_ind_bleeding)
	{
		if (fis_zero(bleeding, EPS))
		{
			m_ind_bleeding->Show(false);
			m_ind_bleeding->ResetColorAnimation();
		}
		else
		{
			m_ind_bleeding->Show(true);

			u32 texColor = m_ind_bleeding->GetTextureColor();
			_color casted(texColor);


			if (bleeding < 0.35f)
			{
				if (!m_ind_bleeding_svg_inited)
				{
					m_ind_bleeding->InitTexture("ui_inGame2_circle_bloodloose_green");
				}
				else
				{
					casted.r = 0.0f;
					casted.g = 100.0f / 255.0f;
					casted.b = 0.0f;
					
					m_ind_bleeding->SetTextureColor(casted.get());
				}

				m_ind_bleeding->SetColorAnimation("ui_slow_blinking_alpha", flags);
			}
			else if (bleeding < 0.7f)
			{
				if (!m_ind_bleeding_svg_inited)
				{
					m_ind_bleeding->InitTexture("ui_inGame2_circle_bloodloose_yellow");
				}
				else
				{
					casted.r = 180.0f / 255.0f;
					casted.g = 100.0f / 255.0f;
					casted.b = 0.0f;

					m_ind_bleeding->SetTextureColor(casted.get());
				}

				m_ind_bleeding->SetColorAnimation("	", flags);
			}
			else
			{
				if (!m_ind_bleeding_svg_inited)
				{
					m_ind_bleeding->InitTexture("ui_inGame2_circle_bloodloose_red");
				}
				else
				{
					casted.r = 200.0f / 255.0f;
					casted.g = 0.0f;
					casted.b = 0.0f;

					m_ind_bleeding->SetTextureColor(casted.get());
				}
				m_ind_bleeding->SetColorAnimation("ui_fast_blinking_alpha", flags);
			}
		}
	}
// Radiation icon
	float radiation = pActor->conditions().GetRadiation();
	if (m_ind_radiation)
	{
		if (fis_zero(radiation, EPS))
		{
			m_ind_radiation->Show(false);
			m_ind_radiation->ResetColorAnimation();
		}
		else
		{
			m_ind_radiation->Show(true);
			if (radiation < 0.35f)
			{
				m_ind_radiation->InitTexture("ui_inGame2_circle_radiation_green");
				m_ind_radiation->SetColorAnimation("ui_slow_blinking_alpha", flags);
			}
			else if (radiation < 0.7f)
			{
				m_ind_radiation->InitTexture("ui_inGame2_circle_radiation_yellow");
				m_ind_radiation->SetColorAnimation("ui_medium_blinking_alpha", flags);
			}
			else
			{
				m_ind_radiation->InitTexture("ui_inGame2_circle_radiation_red");
				m_ind_radiation->SetColorAnimation("ui_fast_blinking_alpha", flags);
			}
		}
	}
// Satiety icon
	if (m_ind_starvation)
	{
		float satiety = pActor->conditions().GetSatiety();
		float satiety_critical = pActor->conditions().SatietyCritical();
		float satiety_koef = (satiety - satiety_critical) / (satiety >= satiety_critical ? 1 - satiety_critical : satiety_critical);
		if (satiety_koef > 0.5)
			m_ind_starvation->Show(false);
		else
		{
			m_ind_starvation->Show(true);
			if (satiety_koef > 0.0f)
				m_ind_starvation->InitTexture("ui_inGame2_circle_hunger_green");
			else if (satiety_koef > -0.5f)
				m_ind_starvation->InitTexture("ui_inGame2_circle_hunger_yellow");
			else
				m_ind_starvation->InitTexture("ui_inGame2_circle_hunger_red");
		}
	}

// Thirst icon
	const static bool enableThirst = EngineExternal()[EEngineExternalGame::EnableThirst];
	if (enableThirst)
	{
		float thirst = pActor->conditions().GetThirst();
		float thirst_critical = pActor->conditions().ThirstCritical();
		float thirst_koef = (thirst - thirst_critical) / (thirst >= thirst_critical ? 1 - thirst_critical : thirst_critical);

		if (thirst_koef > 0.5f)
		{
			m_ind_thirst->Show(false);
		}
		else
		{
			m_ind_thirst->Show(true);
			if (thirst_koef > 0.0f)
				m_ind_thirst->InitTexture("ui_inGame2_circle_thirst_green");
			else if (thirst_koef > -0.5f)
				m_ind_thirst->InitTexture("ui_inGame2_circle_thirst_yellow");
			else
				m_ind_thirst->InitTexture("ui_inGame2_circle_thirst_red");
		}
	}

// Sleepiness icon
	const static bool enableSleepiness = EngineExternal()[EEngineExternalGame::EnableSleepiness];
	if (enableSleepiness)
	{
		float sleepiness = pActor->conditions().GetSleepiness();
		float sleepiness_critical = pActor->conditions().SleepinessCritical();
		float sleepiness_koef = (sleepiness - sleepiness_critical) / (sleepiness < sleepiness_critical ? 1 - sleepiness_critical : sleepiness_critical);

		if (sleepiness_koef < 0.5)
			m_ind_sleepiness->Show(false);
		else
		{
			m_ind_sleepiness->Show(true);
			if (sleepiness_koef > 0.0f)
				m_ind_sleepiness->InitTexture("ui_inGame2_circle_sleepiness_green");
			else if (sleepiness_koef > -0.5f)
				m_ind_sleepiness->InitTexture("ui_inGame2_circle_sleepiness_yellow");
			else
				m_ind_sleepiness->InitTexture("ui_inGame2_circle_sleepiness_red");
		}
	}

// Armor broken icon
	CCustomOutfit* outfit = pActor->GetOutfit();
	if (m_ind_outfit_broken)
	{
		m_ind_outfit_broken->Show(false);
		if (outfit)
		{
			float condition = outfit->GetCondition();
			if (condition < 0.75f)
			{
				m_ind_outfit_broken->Show(true);
				if (condition > 0.5f)
					m_ind_outfit_broken->InitTexture("ui_inGame2_circle_Armorbroken_green");
				else if (condition > 0.25f)
					m_ind_outfit_broken->InitTexture("ui_inGame2_circle_Armorbroken_yellow");
				else
					m_ind_outfit_broken->InitTexture("ui_inGame2_circle_Armorbroken_red");
			}
		}
	}
// Helmet broken icon
	if (m_ind_helmet_broken)
	{
		CHelmet* helmet = pActor->GetHelmet();
		m_ind_helmet_broken->Show(false);
		if (helmet)
		{
			float condition = helmet->GetCondition();
			if (condition < 0.75f)
			{
				m_ind_helmet_broken->Show(true);
				if (condition > 0.5f)
					m_ind_helmet_broken->InitTexture("ui_inGame2_circle_Helmetbroken_green");
				else if (condition > 0.25f)
					m_ind_helmet_broken->InitTexture("ui_inGame2_circle_Helmetbroken_yellow");
				else
					m_ind_helmet_broken->InitTexture("ui_inGame2_circle_Helmetbroken_red");
			}
		}
	}
// Weapon broken icon
	if (m_ind_weapon_broken)
	{
		u16 slot = pActor->inventory().GetActiveSlot();
		m_ind_weapon_broken->Show(false);
		if (slot == INV_SLOT_2 || slot == INV_SLOT_3 || slot == PISTOL_SLOT_NEW)
		{
			PIItem item_from_slot = pActor->inventory().ItemFromSlot(slot);
			CWeapon* weapon = item_from_slot ? item_from_slot->cast_weapon() : nullptr;
			if (weapon)
			{
				float condition = weapon->GetCondition();
				float start_misf_cond = weapon->GetMisfireStartCondition();
				float end_misf_cond = weapon->GetMisfireEndCondition();
				if (condition < start_misf_cond)
				{
					m_ind_weapon_broken->Show(true);
					if (condition > (start_misf_cond + end_misf_cond) / 2)
						m_ind_weapon_broken->InitTexture("ui_inGame2_circle_Gunbroken_green");
					else if (condition > end_misf_cond)
						m_ind_weapon_broken->InitTexture("ui_inGame2_circle_Gunbroken_yellow");
					else
						m_ind_weapon_broken->InitTexture("ui_inGame2_circle_Gunbroken_red");
				}
			}
		}
	}
// Overweight icon
	if (m_ind_overweight)
	{
		float cur_weight = pActor->inventory().TotalWeight();
		float max_weight = pActor->MaxWalkWeight();
		m_ind_overweight->Show(false);
		if (cur_weight >= max_weight - 10.0f && IsGameTypeSingleCompatible())
		{
			m_ind_overweight->Show(true);
			if (cur_weight > max_weight)
				m_ind_overweight->InitTexture("ui_inGame2_circle_Overweight_red");
			//else if(cur_weight>max_weight-10.0f)
			//	m_ind_overweight->InitTexture("ui_inGame2_circle_Overweight_yellow");
			else
				m_ind_overweight->InitTexture("ui_inGame2_circle_Overweight_yellow");
		}
	}
}

void CUIMainIngameWnd::UpdateQuickSlots()
{
	string32 tmp;
	LPCSTR str = g_pStringTable->translate("quick_use_str_1").c_str();
	strncpy_s(tmp, sizeof(tmp), str, 3);
	if (tmp[2] == ',')
		tmp[1] = '\0';
	if (m_QuickSlotText1)
		m_QuickSlotText1->SetTextST(tmp);

	str = g_pStringTable->translate("quick_use_str_2").c_str();
	strncpy_s(tmp, sizeof(tmp), str, 3);
	if (tmp[2] == ',')
		tmp[1] = '\0';
	if (m_QuickSlotText2)
		m_QuickSlotText2->SetTextST(tmp);

	str = g_pStringTable->translate("quick_use_str_3").c_str();
	strncpy_s(tmp, sizeof(tmp), str, 3);
	if (tmp[2] == ',')
		tmp[1] = '\0';
	if (m_QuickSlotText3)
		m_QuickSlotText3->SetTextST(tmp);

	str = g_pStringTable->translate("quick_use_str_4").c_str();
	strncpy_s(tmp, sizeof(tmp), str, 3);
	if (tmp[2] == ',')
		tmp[1] = '\0';
	if (m_QuickSlotText4)
		m_QuickSlotText4->SetTextST(tmp);

	CActor* pActor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
	if (!pActor)
	{
		return;
	}

	// If hide quick slots option is disabled, panel is always visible
	const bool isHideQuickSlotsEnabled = psHUD_Flags.test(HUD_HIDE_QUICK_SLOTS);
	bool should_show_panel = (!isHideQuickSlotsEnabled) || m_quick_slots_force_visible || m_quick_slots_visible;

	int i = -1;
	for (const auto& slot : m_quick_slots_icons)
	{
		++i;
		CUIWindow* finded_child = slot->FindChild("counter");
		if (CUIStatic* wnd = finded_child != nullptr ? finded_child->ui_cast_static() : nullptr)
		{
			shared_str item_name = g_quick_use_slots[i];
			if (item_name.size())
			{
				const u32 count = pActor->inventory().dwfGetSameItemCount(item_name.c_str(), true);
				string32 str;
				xr_sprintf(str, "%s%d", EngineExternal().GetInventoryItemCountPrefix().c_str(), count);
				wnd->TextItemControl()->SetText(str);

				wnd->Show(should_show_panel);

				bool isRaster = EngineExternal().isRenderingUIRaster();

				if (!isRaster)
				{
					if (EngineExternal().isRenderingUIErrorFallbackToDefaultAtlas() == false)
					{
						isRaster = !(pSettings->line_exist(item_name, kUIConfigField_InventoryVectorIcon));
					}
				}
				
				if (isRaster)
				{
					const char* icons_texture = READ_IF_EXISTS(pSettings, r_string, item_name.c_str(), "icons_texture", nullptr);
					slot->SetShader(InventoryUtilities::GetEquipmentIconsShader(icons_texture));
				}

				float scaleIcon = READ_IF_EXISTS(pSettings, r_float, item_name, "inv_scale", 1.0f);

				Frect texture_rect;
				texture_rect.x1 = pSettings->r_float(item_name, "inv_grid_x") * INV_GRID_WIDTH(scaleIcon);
				texture_rect.y1 = pSettings->r_float(item_name, "inv_grid_y") * INV_GRID_HEIGHT(scaleIcon);
				texture_rect.x2 = pSettings->r_float(item_name, "inv_grid_width") * INV_GRID_WIDTH(scaleIcon);
				texture_rect.y2 = pSettings->r_float(item_name, "inv_grid_height") * INV_GRID_HEIGHT(scaleIcon);
				texture_rect.rb.add(texture_rect.lt);



				if (isRaster)
				{
					slot->SetTextureRect(texture_rect);
				}
				else
				{
					float fWidth = texture_rect.width();
					float fHeight = texture_rect.height();

					xr_string_view svg_icon_name = pSettings->r_string(item_name, kUIConfigField_InventoryVectorIcon);

					if (svg_icon_name.empty() == false)
					{
						const ui_shader& svg_shader = UI().GetVectorShader(svg_icon_name, fWidth, fHeight);
						texture_rect = UI().GetVectorUV(svg_icon_name, fWidth, fHeight);
						slot->SetShader(svg_shader);
						slot->SetTextureRect(texture_rect);
					}
					else
					{
						const ui_shader& default_shader = UI().GetVectorShader(_kDefaultSVGShader, fWidth, fHeight);
						texture_rect = UI().GetVectorUV(_kDefaultSVGShader, fWidth, fHeight);
						slot->SetShader(default_shader);
						slot->SetTextureRect(texture_rect);
					}
				}


				slot->TextureOn();
				slot->SetStretchTexture(true);

				slot->Show(should_show_panel);

				if (should_show_panel)
				{
					const bool isEmpty = !count;
					const float alphaScale = isEmpty ? 128.0f : 255.0f;
					const u32 alpha = (u32)clampr(iFloor(m_quick_slots_alpha * alphaScale), 0, (int)alphaScale);

					const u32 color = subst_alpha(color_rgba(255, 255, 255, 255), alpha);
					wnd->SetTextureColor(color);
					slot->SetTextureColor(color);
					wnd->TextItemControl()->SetTextColor(subst_alpha(color_rgba(255, 255, 255, 255), 255));
				}
			}
			else
			{
				wnd->Show(false);
				slot->Show(false);
				slot->SetTextureColor(color_rgba(255, 255, 255, 0));
			}
		}
	}

	if (m_QuickSlotText1) m_QuickSlotText1->Show(should_show_panel);
	if (m_QuickSlotText2) m_QuickSlotText2->Show(should_show_panel);
	if (m_QuickSlotText3) m_QuickSlotText3->Show(should_show_panel);
	if (m_QuickSlotText4) m_QuickSlotText4->Show(should_show_panel);
}

void CUIMainIngameWnd::DrawMainIndicatorsForInventory()
{
	CActor* pActor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
	if (!pActor)
	{
		return;
	}

	UpdateQuickSlots();
	UpdateBoosterIndicators(pActor->conditions().GetCurBoosterInfluences());

	// Проверяем, должна ли панель быть видимой
	// If hide quick slots option is disabled, panel is always visible
	const bool isHideQuickSlotsEnabled = psHUD_Flags.test(HUD_HIDE_QUICK_SLOTS);
	bool should_show_panel = (!isHideQuickSlotsEnabled) || m_quick_slots_force_visible || m_quick_slots_visible;

	// Отрисовываем быстрые слоты только если панель должна быть видимой
	if (should_show_panel)
	{
		for (const auto& slot : m_quick_slots_icons)
			slot->Draw();

		if (m_QuickSlotText1)
			m_QuickSlotText1->Draw();
		if (m_QuickSlotText2)
			m_QuickSlotText2->Draw();
		if (m_QuickSlotText3)
			m_QuickSlotText3->Draw();
		if (m_QuickSlotText4)
			m_QuickSlotText4->Draw();
	}

	// Бустеры и другие индикаторы всегда отрисовываются (они не связаны с панелью быстрого доступа)
	if (m_ind_boost_psy && m_ind_boost_psy->IsShown())
	{
		m_ind_boost_psy->Update();
		m_ind_boost_psy->Draw();
	}

	if (m_ind_boost_radia && m_ind_boost_radia->IsShown())
	{
		m_ind_boost_radia->Update();
		m_ind_boost_radia->Draw();
	}

	if (m_ind_boost_chem && m_ind_boost_chem->IsShown())
	{
		m_ind_boost_chem->Update();
		m_ind_boost_chem->Draw();
	}

	if (m_ind_boost_wound && m_ind_boost_wound->IsShown())
	{
		m_ind_boost_wound->Update();
		m_ind_boost_wound->Draw();
	}

	if (m_ind_boost_weight && m_ind_boost_weight->IsShown())
	{
		m_ind_boost_weight->Update();
		m_ind_boost_weight->Draw();
	}

	if (m_ind_boost_health && m_ind_boost_health->IsShown())
	{
		m_ind_boost_health->Update();
		m_ind_boost_health->Draw();
	}

	if (m_ind_boost_power && m_ind_boost_power->IsShown())
	{
		m_ind_boost_power->Update();
		m_ind_boost_power->Draw();
	}

	if (m_ind_boost_rad && m_ind_boost_rad->IsShown())
	{
		m_ind_boost_rad->Update();
		m_ind_boost_rad->Draw();
	}
}

void CUIMainIngameWnd::UpdateBoosterIndicators(const xr_map<EBoostParams, SBooster> influences)
{
	if (m_ind_boost_psy)
		m_ind_boost_psy->Show(false);
	if (m_ind_boost_radia)
		m_ind_boost_radia->Show(false);
	if (m_ind_boost_chem)
		m_ind_boost_chem->Show(false);
	if (m_ind_boost_wound)
		m_ind_boost_wound->Show(false);
	if (m_ind_boost_weight)
		m_ind_boost_weight->Show(false);
	if (m_ind_boost_health)
		m_ind_boost_health->Show(false);
	if (m_ind_boost_power)
		m_ind_boost_power->Show(false);
	if (m_ind_boost_rad)
		m_ind_boost_rad->Show(false);

	LPCSTR str_flag	= "ui_slow_blinking_alpha";
	u8 flags = 0;
	flags |= LA_CYCLIC;
	flags |= LA_ONLYALPHA;
	flags |= LA_TEXTURECOLOR;

	for (auto& booster : influences)
	{
		switch(booster.second.m_type)
		{
			case eBoostHpRestore: 
				{
				if (m_ind_boost_health && booster.second.fBoostTime > 0.0f)
				{
					m_ind_boost_health->Show(true);
					string16 buf = {};
					xr_sprintf(buf, "%.0f", booster.second.fBoostTime);
					m_ind_boost_health->TextItemControl()->SetText(buf);
					if (booster.second.fBoostTime <= 3.0f)
						m_ind_boost_health->SetColorAnimation(str_flag, flags);
					else
						m_ind_boost_health->ResetColorAnimation();
				}
				}
				break;
			case eBoostPowerRestore: 
				{
				if (m_ind_boost_power && booster.second.fBoostTime > 0.0f)
				{
					m_ind_boost_power->Show(true);
					string16 buf = {};
					xr_sprintf(buf, "%.0f", booster.second.fBoostTime);
					m_ind_boost_power->TextItemControl()->SetText(buf);
					if (booster.second.fBoostTime <= 3.0f)
						m_ind_boost_power->SetColorAnimation(str_flag, flags);
					else
						m_ind_boost_power->ResetColorAnimation();
				}
				}
				break;
			case eBoostRadiationRestore: 
				{
				if (m_ind_boost_rad && booster.second.fBoostTime > 0.0f)
				{
					m_ind_boost_rad->Show(true);
					string16 buf = {};
					xr_sprintf(buf, "%.0f", booster.second.fBoostTime);
					m_ind_boost_rad->TextItemControl()->SetText(buf);
					if (booster.second.fBoostTime <= 3.0f)
						m_ind_boost_rad->SetColorAnimation(str_flag, flags);
					else
						m_ind_boost_rad->ResetColorAnimation();
				}
				}
				break;
			case eBoostBleedingRestore: 
				{
				if (m_ind_boost_wound && booster.second.fBoostTime > 0.0f)
				{
					m_ind_boost_wound->Show(true);
					string16 buf = {};
					xr_sprintf(buf, "%.0f", booster.second.fBoostTime);
					m_ind_boost_wound->TextItemControl()->SetText(buf);
					if (booster.second.fBoostTime <= 3.0f)
						m_ind_boost_wound->SetColorAnimation(str_flag, flags);
					else
						m_ind_boost_wound->ResetColorAnimation();
				}
				}
				break;
			case eBoostMaxWeight: 
				{
				if (m_ind_boost_weight && booster.second.fBoostTime > 0.0f)
				{
					m_ind_boost_weight->Show(true);
					string16 buf = {};
					xr_sprintf(buf, "%.0f", booster.second.fBoostTime);
					m_ind_boost_weight->TextItemControl()->SetText(buf);
					if (booster.second.fBoostTime <= 3.0f)
						m_ind_boost_weight->SetColorAnimation(str_flag, flags);
					else
						m_ind_boost_weight->ResetColorAnimation();
				}
				}
				break;
			case eBoostRadiationImmunity: 
			case eBoostRadiationProtection: 
				{
				if (m_ind_boost_radia && booster.second.fBoostTime > 0.0f)
				{
					m_ind_boost_radia->Show(true);
					string16 buf = {};
					xr_sprintf(buf, "%.0f", booster.second.fBoostTime);
					m_ind_boost_radia->TextItemControl()->SetText(buf);
					if (booster.second.fBoostTime <= 3.0f)
						m_ind_boost_radia->SetColorAnimation(str_flag, flags);
					else
						m_ind_boost_radia->ResetColorAnimation();
				}
				}
				break;
			case eBoostTelepaticImmunity: 
			case eBoostTelepaticProtection: 
				{
				if (m_ind_boost_psy && booster.second.fBoostTime > 0.0f)
				{
					m_ind_boost_psy->Show(true);
					string16 buf = {};
					xr_sprintf(buf, "%.0f", booster.second.fBoostTime);
					m_ind_boost_psy->TextItemControl()->SetText(buf);
					if (booster.second.fBoostTime <= 3.0f)
						m_ind_boost_psy->SetColorAnimation(str_flag, flags);
					else
						m_ind_boost_psy->ResetColorAnimation();
				}
				}
				break;
			case eBoostChemicalBurnImmunity: 
			case eBoostChemicalBurnProtection: 
				{
				if (m_ind_boost_chem && booster.second.fBoostTime > 0.0f)
				{
					m_ind_boost_chem->Show(true);
					string16 buf = {};
					xr_sprintf(buf, "%.0f", booster.second.fBoostTime);
					m_ind_boost_chem->TextItemControl()->SetText(buf);
					if (booster.second.fBoostTime <= 3.0f)
						m_ind_boost_chem->SetColorAnimation(str_flag, flags);
					else
						m_ind_boost_chem->ResetColorAnimation();
				}
				}
				break;
		}
	}
}