#include "StdAfx.h"
#include "UIHudStatesWnd.h"
#include "UIHudGroupCatalog.h"

#include "../Actor.h"
#include "../ActorCondition.h"
#include "../EntityCondition.h"
#include "../CustomOutfit.h"
#include "../ActorHelmet.h"
#include "../Inventory.h"
#include "../RadioactiveZone.h"
#include "../../xrUI/UIFontDefines.h"
#include "../Grenade.h"
#include "../../xrUI/UITextureMaster.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UI3dStatic.h"
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UIProgressShape.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIArrow.h"
#include "UIInventoryUtilities.h"
#include "../CustomDetectorZones.h"
#include "../ai/monsters/basemonster/base_monster.h"
#include "../PDA.h"
#include "WeaponMagazinedWGrenade.h"
#include "../Weapon.h"
#include "../WeaponKnife.h"
#include "../WeaponBinoculars.h"
#include "../Bolt.h"
#include "../../xrEngine/string_table.h"
#include "../../xrEngine/CustomHUD.h"

namespace
{

int PickStaticWpnIconXmlIndex(CUIXml& xml)
{
    XML_NODE* root = xml.GetLocalRoot() ? xml.GetLocalRoot() : xml.GetRoot();
    const int count = xml.GetNodesNum(root, "static_wpn_icon");
    if (count <= 0)
    {
        return 0;
    }
    for (int i = count - 1; i >= 0; --i)
    {
        if (xml.ReadAttribInt(root, "static_wpn_icon", i, "caliber", 0) == 1)
        {
            return i;
        }
    }
    for (int i = count - 1; i >= 0; --i)
    {
        const char* dm = xml.ReadAttrib(root, "static_wpn_icon", i, "display_mode", nullptr);
        if (dm && xr_strcmp(dm, "text") == 0)
        {
            return i;
        }
    }
    return 0;
}

bool HudApplyCatalogIconTexture(CUIStatic& wnd, const shared_str& textureId)
{
    if (!textureId.size())
    {
        return false;
    }
    const TEX_INFO texInfo = CUITextureMaster::FindItem(textureId);
    if (!texInfo.file.size())
    {
        return false;
    }
    wnd.TextureOn();
    wnd.SetTextureColor(color_rgba(255, 255, 255, 255));
    wnd.SetShader(InventoryUtilities::GetEquipmentIconsShader(texInfo.get_file_name()));
    wnd.SetTextureRect(texInfo.rect);
    wnd.SetStretchTexture(true);
    wnd.GetUIStaticItem().SetSize(Fvector2().set(texInfo.rect.width(), texInfo.rect.height()));
    return true;
}

static bool ApplyCaliberCatalogWeaponRow(
    const CUIHudGroupCatalog& catalog,
    u32 groupIndex,
    const shared_str& sectionLabel,
    CUIStatic* uiCaliberText,
    CUIStatic* uiWeaponIcon,
    CUIStatic* uiCaliberIcon)
{
    const SHudGroupDesc* groupDesc = catalog.GetGroup(groupIndex);
    if (!groupDesc)
    {
        return false;
    }
    const shared_str labelId = sectionLabel.size() ? sectionLabel : groupDesc->_shortLabel;
    const STRING_VALUE labelText = g_pStringTable->translate(labelId);
    CUIStatic* const textWnd = uiCaliberText ? uiCaliberText : uiWeaponIcon;
    if (textWnd)
    {
        textWnd->SetText(labelText.c_str());
        textWnd->Show(true);
    }
    if (uiCaliberIcon)
    {
        if (HudApplyCatalogIconTexture(*uiCaliberIcon, groupDesc->_iconTexture))
        {
            uiCaliberIcon->Show(true);
        }
        else
        {
            uiCaliberIcon->Show(false);
        }
    }
    return true;
}

static void ApplyCaliberCatalogGadgetLine(
    const CUIHudGroupCatalog& catalog,
    const shared_str& sectionId,
    CUIStatic* uiCaliberText,
    CUIStatic* uiWeaponIcon,
    CUIStatic* uiCaliberIcon,
    const char* nameShortFallback)
{
    shared_str sectionLabel;
    const bool hasCatalogText = catalog.FindSectionLabel(sectionId, sectionLabel) && sectionLabel.size();
    CUIStatic* const textWnd = uiCaliberText ? uiCaliberText : uiWeaponIcon;
    if (textWnd)
    {
        if (hasCatalogText)
        {
            const STRING_VALUE labelText = g_pStringTable->translate(sectionLabel);
            textWnd->SetText(labelText.c_str());
        }
        else
        {
            textWnd->SetText(nameShortFallback);
        }
        textWnd->Show(true);
    }
    if (uiCaliberIcon)
    {
        uiCaliberIcon->Show(false);
    }
}
} // namespace

CUIHudStatesWnd::CUIHudStatesWnd()
    : m_b_force_update(true),
    m_timer_1sec(0),
    m_last_health(0.0f),
    m_radia_self(0.0f),
    m_radia_hit(0.0f)
{
    LoadCallbackGlobals(m_isZoneTouch, m_onZoneTouch, "OnZoneTouch");

    for (int i = 0; i < ALife::infl_max_count; ++i)
    {
        m_zone_cur_power[i] = 0.0f;
//--        m_zone_max_power[i] = 1.0f;
        m_zone_feel_radius[i] = 1.0f;
    }
    m_zone_hit_type[ALife::infl_rad ] = ALife::eHitTypeRadiation;
    m_zone_hit_type[ALife::infl_fire] = ALife::eHitTypeBurn;
    m_zone_hit_type[ALife::infl_acid] = ALife::eHitTypeChemicalBurn;
    m_zone_hit_type[ALife::infl_psi ] = ALife::eHitTypeTelepatic;
    m_zone_hit_type[ALife::infl_electra] = ALife::eHitTypeShock;

	m_zone_feel_radius_max = 0.0f;
	
	m_health_blink = pSettings->read_if_exists<float>("actor_condition", "hud_health_blink", 0.f);
	clamp( m_health_blink, 0.0f, 1.0f );

    m_fake_indicators_update = false;
    m_arrow = nullptr;
    m_arrow_shadow = nullptr;
    UIStackPanelDangers = nullptr;
    //- Load_section();
}

CUIHudStatesWnd::~CUIHudStatesWnd()
{
    xr_delete(m_hud_group_catalog);
}

void CUIHudStatesWnd::reset_ui()
{
    if (g_pGameLevel)
    {
        Level().hud_zones_list->clear();
    }
}

ALife::EInfluenceType CUIHudStatesWnd::get_indik_type( ALife::EHitType hit_type )
{
    ALife::EInfluenceType iz_type = ALife::infl_max_count;
    switch (hit_type)
    {
    case ALife::eHitTypeRadiation:      iz_type = ALife::infl_rad;      break;
    case ALife::eHitTypeLightBurn:
    case ALife::eHitTypeBurn:           iz_type = ALife::infl_fire;     break;
    case ALife::eHitTypeChemicalBurn:   iz_type = ALife::infl_acid;     break;
    case ALife::eHitTypeTelepatic:      iz_type = ALife::infl_psi;      break;
    case ALife::eHitTypeShock:          iz_type = ALife::infl_electra;  break;// it hasnt CStatic

    case ALife::eHitTypeStrike:
    case ALife::eHitTypeWound:
    case ALife::eHitTypeExplosion:
    case ALife::eHitTypeFireWound:
    case ALife::eHitTypeWound_2:
    case ALife::eHitTypePhysicStrike:
        return ALife::infl_max_count;
    default:
        NODEFAULT;
    }
    return iz_type;
}

void CUIHudStatesWnd::InitFromXml( CUIXml& xml, const char* path )
{
    XML_NODE* stored_root = xml.GetLocalRoot();
    if (xml.NavigateToNode(path))
    {
        CUIXmlInit::InitWindow(xml, path, 0, this);
        XML_NODE* new_root = xml.NavigateToNode(path, 0);
        xml.SetLocalRoot(new_root);
    }

    if (xml.NavigateToNode("back"))
    {
        m_back            = UIHelper::CreateStatic( xml, "back", this );
    }

    if (xml.NavigateToNode("static_weapon"))
    {
        m_static_weapon = UIHelper::CreateStatic(xml, "static_weapon", this);
    }

    CUIWindow* healthBarParent = this;
    if (xml.NavigateToNode("static_health"))
    {
        m_static_health = UIHelper::CreateStatic(xml, "static_health", this);
        healthBarParent = m_static_health;
    }

    m_ui_health_bar   = UIHelper::CreateProgressBar( xml, "progress_bar_health", healthBarParent);
    m_ui_health_bar->IsExpressionSystem = xml.ReadAttrib("progress_bar_health", 0, "expression", nullptr) != nullptr;

    if (xml.NavigateToNode("back_v", 0))
    {
        m_back_v = UIHelper::CreateStatic(xml, "back_v", this);
    }
    if (xml.NavigateToNode("static_armor", 0))
    {
        m_static_armor = UIHelper::CreateStatic(xml, "static_armor", this);
    }


    if (xml.NavigateToNode("resist_back_rad", 0))
    {
        m_resist_back[ALife::infl_rad]  = UIHelper::CreateStatic( xml, "resist_back_rad", this );
    }
    if (xml.NavigateToNode("resist_back_fire", 0))
    {
        m_resist_back[ALife::infl_fire] = UIHelper::CreateStatic( xml, "resist_back_fire", this );
    }
    if (xml.NavigateToNode("resist_back_acid", 0))
    {
        m_resist_back[ALife::infl_acid] = UIHelper::CreateStatic( xml, "resist_back_acid", this );
    }
    if (xml.NavigateToNode("resist_back_psi", 0))
    {
        m_resist_back[ALife::infl_psi]  = UIHelper::CreateStatic( xml, "resist_back_psi", this );
    }
    if (xml.NavigateToNode("resist_back_starvation", 0))
    {
        m_resist_back_starvation = UIHelper::CreateStatic(xml, "resist_back_starvation", this);
    }
    // electra = no has CStatic!!

    if (xml.NavigateToNode("indik_stack_panel", 0))
    {
        UIStackPanelDangers = UIHelper::CreateStackPanel(xml, "indik_stack_panel", this);
    }

    CUIWindow* indicatorParent = this;
    if (UIStackPanelDangers)
    {
        indicatorParent = UIStackPanelDangers;
    }

    if (xml.NavigateToNode("indik_rad", 0))
    {
        m_indik[ALife::infl_rad]  = UIHelper::CreateStatic( xml, "indik_rad", indicatorParent);
    }
    if (xml.NavigateToNode("indik_fire", 0))
    {
        m_indik[ALife::infl_fire] = UIHelper::CreateStatic( xml, "indik_fire", indicatorParent);
    }
    if (xml.NavigateToNode("indik_acid", 0))
    {
        m_indik[ALife::infl_acid] = UIHelper::CreateStatic( xml, "indik_acid", indicatorParent);
    }
    if (xml.NavigateToNode("indik_psi", 0))
    {
        m_indik[ALife::infl_psi]  = UIHelper::CreateStatic( xml, "indik_psi", indicatorParent);
    }
    if (xml.NavigateToNode("indicator_starvation", 0))
    {
        m_ind_starvation = UIHelper::CreateStatic(xml, "indicator_starvation", this);
    }

    m_lanim_name                = xml.ReadAttrib( "indik_rad", 0, "light_anim", "" );
    if (xml.NavigateToNode("static_ammo", 0))
    {
        CUIWindow* ammoSignParent = this;
        if (m_static_weapon)
        {
            ammoSignParent = m_static_weapon;
        }

        m_ui_weapon_sign_ammo = UIHelper::CreateStatic(xml, "static_ammo", ammoSignParent);
    }

    if (xml.NavigateToNode("static_cur_ammo", 0))
    {
        m_ui_weapon_cur_ammo = UIHelper::CreateStatic(xml, "static_cur_ammo", this);
    }

    if (xml.NavigateToNode("static_fmj_ammo", 0))
    {
        m_ui_weapon_fmj_ammo = UIHelper::CreateStatic(xml, "static_fmj_ammo", this);
    }
    if (xml.NavigateToNode("static_ap_ammo", 0))
    {
        m_ui_weapon_ap_ammo = UIHelper::CreateStatic(xml, "static_ap_ammo", this);
    }

    //Alundaio: Option to display a third ammo type
    if (xml.NavigateToNode("static_third_ammo", 0))
    {
        m_ui_weapon_third_ammo = UIHelper::CreateStatic(xml, "static_third_ammo", this);
    }
    //-Alundaio

    if (xml.NavigateToNode("static_ammo_adaptive", 0))
    {
        m_use_adaptive_ammo_widget = true;
        xr_strcpy(m_adaptive_total_separator, xml.ReadAttrib("static_ammo_adaptive", 0, "separator", "/"));
        CUIWindow* adaptiveContainer = new CUIWindow();
        adaptiveContainer->SetAutoDelete(true);
        CUIXmlInit::InitWindow(xml, "static_ammo_adaptive", 0, adaptiveContainer);
        AttachChild(adaptiveContainer);
        m_ui_adaptive_clip = UIHelper::CreateStatic(xml, "static_ammo_adaptive:clip_text", adaptiveContainer);
        m_ui_adaptive_total = UIHelper::CreateStatic(xml, "static_ammo_adaptive:total_text", adaptiveContainer);
        if (m_ui_weapon_cur_ammo)
        {
            m_ui_weapon_cur_ammo->Show(false);
        }
        if (m_ui_weapon_fmj_ammo)
        {
            m_ui_weapon_fmj_ammo->Show(false);
        }
        if (m_ui_weapon_ap_ammo)
        {
            m_ui_weapon_ap_ammo->Show(false);
        }
        if (m_ui_weapon_third_ammo)
        {
            m_ui_weapon_third_ammo->Show(false);
        }
        if (m_ui_grenade)
        {
            m_ui_grenade->Show(false);
        }
    }
    else
    {
        m_use_adaptive_ammo_widget = false;
    }

    // HACK: St4lker0k765: idk why, but default values in CUIXmlInit::GetColor are glitchy as hell, so i'll try this instead
    if (xml.NavigateToNode("active_ammo_color", 0))
    {
        m_ui_weapon_ammo_color_active = CUIXmlInit::GetColor(xml, "active_ammo_color", 0, color_rgba(238, 155, 23, 255));
    }
    else
    {
        m_ui_weapon_ammo_color_active = color_rgba(238, 155, 23, 255);
    }

    if (xml.NavigateToNode("inactive_ammo_color", 0))
    {
        m_ui_weapon_ammo_color_inactive = CUIXmlInit::GetColor(xml, "inactive_ammo_color", 0, color_rgba(238, 155, 23, 150));
    }
    else
    {
        m_ui_weapon_ammo_color_inactive = color_rgba(238, 155, 23, 150);
    }


    // Fire mode HUD: optional icon mode or localized text labels (explicit XML flags).
    if (xml.NavigateToNode("static_fire_mode", 0))
    {
        m_fire_mode = UIHelper::CreateStatic( xml, "static_fire_mode", this );
        const int use_icon = xml.ReadAttribInt("static_fire_mode", 0, "use_icon", 0);
        const int use_text_labels = xml.ReadAttribInt("static_fire_mode", 0, "use_text_labels", 0);
        m_use_fire_mode_text_labels = (use_text_labels == 1);

        XML_NODE* fire_mode_node = xml.NavigateToNode("static_fire_mode", 0);

        if (m_use_fire_mode_text_labels)
        {
            m_use_fire_mode_icons = false;
            if (fire_mode_node)
            {
                const int label_count = xml.GetNodesNum(fire_mode_node, "mode_label");
                for (int i = 0; i < label_count; ++i)
                {
                    XML_NODE* label_node = xml.NavigateToNode(fire_mode_node, "mode_label", i);
                    if (!label_node)
                    {
                        continue;
                    }
                    const shared_str mode_text = xml.ReadAttrib(label_node, "text", "");
                    const shared_str st_id = xml.ReadAttrib(label_node, "st", "");
                    if (mode_text.size() && st_id.size())
                    {
                        m_fire_mode_label_map[mode_text] = st_id;
                    }
                }

                // Optional: reuse mode_mapping nodes with st="..." when migrating from icons.
                const int mapping_count = xml.GetNodesNum(fire_mode_node, "mode_mapping");
                for (int i = 0; i < mapping_count; ++i)
                {
                    XML_NODE* mapping_node = xml.NavigateToNode(fire_mode_node, "mode_mapping", i);
                    if (!mapping_node)
                    {
                        continue;
                    }
                    const shared_str mode_text = xml.ReadAttrib(mapping_node, "text", "");
                    const shared_str st_id = xml.ReadAttrib(mapping_node, "st", "");
                    if (mode_text.size() && st_id.size() && m_fire_mode_label_map.find(mode_text) == m_fire_mode_label_map.end())
                    {
                        m_fire_mode_label_map[mode_text] = st_id;
                    }
                }
            }

            if (m_fire_mode_label_map.empty())
            {
                m_fire_mode_label_map["1"] = "ui_fire_mode_single";
                m_fire_mode_label_map["a"] = "ui_fire_mode_auto";
                m_fire_mode_label_map["A"] = "ui_fire_mode_auto";
                m_fire_mode_label_map["2"] = "ui_fire_mode_burst_2";
                m_fire_mode_label_map["3"] = "ui_fire_mode_burst_3";
            }
        }
        else if (use_icon == 1)
        {
            m_use_fire_mode_icons = true;

            // Create icon widget using same position as text widget
            m_ui_fire_mode_icon = UIHelper::CreateStatic(xml, "static_fire_mode", this);
            m_ui_fire_mode_icon->SetShader(InventoryUtilities::GetEquipmentIconsShader());
            m_ui_fire_mode_icon->Show(false);

            // Hide text widget in icon mode
            m_fire_mode->Show(false);

            // Initialize fire mode icon mapping
            // Check for custom mappings in XML
            if (fire_mode_node)
            {
                int mapping_count = xml.GetNodesNum(fire_mode_node, "mode_mapping");
                for (int i = 0; i < mapping_count; ++i)
                {
                    XML_NODE* mapping_node = xml.NavigateToNode(fire_mode_node, "mode_mapping", i);
                    if (mapping_node)
                    {
                        shared_str mode_text = xml.ReadAttrib(mapping_node, "text", "");
                        shared_str icon_name = xml.ReadAttrib(mapping_node, "icon", "");
                        if (mode_text.size() && icon_name.size())
                        {
                            m_fire_mode_icon_map[mode_text] = icon_name;
                        }
                    }
                }
            }

            // Set default mappings if not specified in XML
            if (m_fire_mode_icon_map.empty())
            {
                m_fire_mode_icon_map["1"] = "icon_fmode_single";
                m_fire_mode_icon_map["a"] = "icon_fmode_auto";
                m_fire_mode_icon_map["A"] = "icon_fmode_auto";
                m_fire_mode_icon_map["2"] = "icon_fmode_2burst";
                m_fire_mode_icon_map["3"] = "icon_fmode_3burst";
            }
        }
    }

    if (xml.NavigateToNode("static_grenade", 0))
    {
        m_ui_grenade = UIHelper::CreateStatic(xml, "static_grenade", this);
    }

    CUIWindow* wpnIconParent = this;
    if (m_static_weapon)
    {
        wpnIconParent = m_static_weapon;
    }

    const int wpnIconXmlIdx = PickStaticWpnIconXmlIndex(xml);
    m_ui_weapon_icon = UIHelper::Create3dStatic(xml, "static_wpn_icon", wpnIconParent, true, wpnIconXmlIdx);
    m_ui_weapon_icon->SetShader(InventoryUtilities::GetEquipmentIconsShader());
    // Apply text style from ammo_text:text if present (AmmoText / addon layouts)
    if (xml.NavigateToNode("static_wpn_icon:ammo_text:text", wpnIconXmlIdx))
    {
        CUIXmlInit::InitText(xml, "static_wpn_icon:ammo_text:text", wpnIconXmlIdx, m_ui_weapon_icon);
    }
//  m_ui_weapon_icon->Enable    ( false );
    m_ui_weapon_icon_rect       = m_ui_weapon_icon->GetWndRect();

    m_weapon_icon_show_weapon_name = (xml.ReadAttribInt("static_wpn_icon", wpnIconXmlIdx, "show_weapon_name", 0) != 0);
    m_wpnIconHudMode = EWpnIconHudMode::Legacy;

    // Caliber HUD and hud_group_catalog load only when static_wpn_icon has caliber="1" on the node
    // selected by PickStaticWpnIconXmlIndex. Layouts without that attribute (e.g. maingame_16.xml) stay Legacy;
    // never enable the catalog from file presence alone.
    const int caliberHud = xml.ReadAttribInt("static_wpn_icon", wpnIconXmlIdx, "caliber", 0);
    const bool isCaliberHudEnabled = (caliberHud == 1 && m_ui_weapon_icon);

    shared_str catalogFile;
    if (isCaliberHudEnabled)
    {
        const char* explicitCat = xml.ReadAttrib("static_wpn_icon", wpnIconXmlIdx, "hud_group_catalog", "");
        if (explicitCat && explicitCat[0])
        {
            catalogFile = explicitCat;
        }
        else
        {
            catalogFile = xml.ReadAttrib("static_wpn_icon", wpnIconXmlIdx, "caliber_config", "hud_group_catalog.xml");
        }
    }

    if (isCaliberHudEnabled && catalogFile.size())
    {
        m_hud_group_catalog = new CUIHudGroupCatalog();
        if (m_hud_group_catalog->Load(catalogFile.c_str()))
        {
            m_wpnIconHudMode = EWpnIconHudMode::Caliber;
            m_ui_caliber_text = UIHelper::CreateStatic(xml, "static_wpn_icon:caliber_text", m_ui_weapon_icon, false, wpnIconXmlIdx);
            m_ui_caliber_icon = UIHelper::CreateStatic(xml, "static_wpn_icon:caliber_icon", m_ui_weapon_icon, false, wpnIconXmlIdx);
            if (m_ui_caliber_text)
            {
                m_ui_caliber_text->Show(false);
            }
            if (m_ui_caliber_icon)
            {
                m_ui_caliber_icon->Show(false);
            }
        }
        else
        {
            xr_delete(m_hud_group_catalog);
            Msg("! Caliber HUD: cannot load [%s] (configs\\ui\\); falling back to ammo name text on weapon icon",
                catalogFile.c_str());
            m_wpnIconHudMode = EWpnIconHudMode::AmmoText;
        }
    }

    if (m_wpnIconHudMode != EWpnIconHudMode::Caliber)
    {
        const char* displayMode = xml.ReadAttrib("static_wpn_icon", wpnIconXmlIdx, "display_mode", nullptr);
        if (displayMode && xr_strcmp(displayMode, "text") == 0)
        {
            m_wpnIconHudMode = EWpnIconHudMode::AmmoText;
        }
    }

    if (xml.NavigateToNode("progress_bar_armor", 0))
    {
        CUIWindow* armorBarParent = this;
        if (xml.GetLocalRoot() == stored_root)
        {
            armorBarParent = m_static_armor;
        }

        m_ui_armor_bar = UIHelper::CreateProgressBar(xml, "progress_bar_armor", armorBarParent);
        m_ui_armor_bar->IsExpressionSystem = xml.ReadAttrib("progress_bar_armor", 0, "expression", nullptr) != nullptr;
    }

    if (xml.NavigateToNode("progress", 0))
    {
        m_progress_self = new CUIProgressShape();
        m_progress_self->SetAutoDelete(true);
        AttachChild(m_progress_self);
        CUIXmlInit::InitProgressShape(xml, "progress", 0, m_progress_self);
    }

    if (xml.NavigateToNode("arrow", 0))
    {
        m_arrow = new CUIArrow();
        m_arrow->init_from_xml(xml, "arrow", this);
    }

    if (xml.NavigateToNode("arrow_shadow", 0))
    {
        m_arrow_shadow = new CUIArrow();
        m_arrow_shadow->init_from_xml(xml, "arrow_shadow", this);
    }

    if (xml.NavigateToNode("back_over_arrow", 0))
    {
        m_back_over_arrow = UIHelper::CreateStatic(xml, "back_over_arrow", this);
    }
    if (xml.NavigateToNode("progress_bar_stamina", 0))
    {
        m_ui_stamina_bar = UIHelper::CreateProgressBar(xml, "progress_bar_stamina", this);
        m_ui_stamina_bar->IsExpressionSystem = xml.ReadAttrib("progress_bar_stamina", 0, "expression", nullptr) != nullptr;
    }

    if (xml.NavigateToNode("bleeding", 0))
    {
        m_bleeding = UIHelper::CreateStatic(xml, "bleeding", this);
        m_bleeding->Show(false);
    }
    for (int i = 0; i < it_max; ++i)
    {
        m_cur_state_LA[i] = true;
        SwitchLA(false, static_cast<ALife::EInfluenceType>(i));
    }

    LoadContextualDisplaySettings(xml, path);

    if (m_back)
    {
        m_back_base_color = m_back->GetTextureColor();
    }
    if (m_bleeding)
    {
        m_bleeding_base_color = m_bleeding->GetTextureColor();
    }

    CaptureContextualBaseColors();

    xml.SetLocalRoot( stored_root );
}

void CUIHudStatesWnd::on_connected()
{
    Load_section();
    CaptureContextualBaseColors();
}

void CUIHudStatesWnd::Load_section()
{
    VERIFY( g_pGameLevel );
    if ( !Level().hud_zones_list )
    {
        Level().create_hud_zones_list();
        VERIFY( Level().hud_zones_list );
    }

//  m_actor_radia_factor = pSettings->r_float( "radiation_zone_detector", "actor_radia_factor" );
    Level().hud_zones_list->load( "all_zone_detector", "zone" );

    Load_section_type( ALife::infl_rad,     "radiation_zone_detector" );
    Load_section_type( ALife::infl_fire,    "fire_zone_detector" );
    Load_section_type( ALife::infl_acid,    "acid_zone_detector" );
    Load_section_type( ALife::infl_psi,     "psi_zone_detector" );
    Load_section_type( ALife::infl_electra, "electra_zone_detector" );  //no uistatic
}

void CUIHudStatesWnd::Load_section_type( ALife::EInfluenceType type, const char* section )
{
    if (!pSettings->section_exist(section))
    {
        return;
    }

    /*m_zone_max_power[type] = pSettings->r_float( section, "max_power" );
    if ( m_zone_max_power[type] <= 0.0f )
    {
        m_zone_max_power[type] = 1.0f;
    }*/
    m_zone_feel_radius[type] = pSettings->r_float( section, "zone_radius" );
    if ( m_zone_feel_radius[type] <= 0.0f )
    {
        m_zone_feel_radius[type] = 1.0f;
    }
    if ( m_zone_feel_radius_max < m_zone_feel_radius[type] )
    {
        m_zone_feel_radius_max = m_zone_feel_radius[type];
    }
    m_zone_threshold[type] = pSettings->r_float( section, "threshold" );
}

void CUIHudStatesWnd::Update()
{
    CActor* actor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
    if ( !actor )
    {
        return;
    }

    UpdateContextualTriggers(actor);

    UpdateHealth( actor );
    UpdateActiveItemInfo( actor );
    UpdateIndicators( actor );

    UpdateZones();

    TickContextualDisplay();

    inherited::Update();

    ApplyContextualAlpha();
}

void CUIHudStatesWnd::UpdateHealth( CActor* actor )
{
    if (!m_ui_health_bar->IsExpressionSystem)
    {
        float cur_health = actor->GetfHealth();
        m_ui_health_bar->SetProgressPos(iCeil(cur_health * 100.0f * 35.f) / 35.f);
        if (std::abs(cur_health - m_last_health) > m_health_blink)
        {
            m_last_health = cur_health;
            m_ui_health_bar->m_UIProgressItem.ResetColorAnimation();
        }
    }

    if (m_ui_stamina_bar && !m_ui_stamina_bar->IsExpressionSystem)
    {
        float cur_stamina = actor->conditions().GetPower();
        m_ui_stamina_bar->SetProgressPos(iCeil(cur_stamina * 100.0f * 35.f) / 35.f);
        if (!actor->conditions().IsCantSprint())
        {
            m_ui_stamina_bar->m_UIProgressItem.ResetColorAnimation();
        }
    }

    if (m_ui_armor_bar && !m_ui_armor_bar->IsExpressionSystem)
    {
        float cur_armor = 0.f;
        if (actor->GetOutfit() != nullptr && actor->GetHelmet() != nullptr)
        {
            cur_armor = (actor->GetOutfit()->GetCondition() * 0.5f) + (actor->GetHelmet()->GetCondition() * 0.5f);
        }
        else if (actor->GetOutfit() != nullptr)
        {
            cur_armor = actor->GetOutfit()->GetCondition();
        }
        else if (actor->GetHelmet() != nullptr)
        {
            cur_armor = actor->GetHelmet()->GetCondition();
        }

        m_ui_armor_bar->SetProgressPos(iCeil(cur_armor * 100.0f * 35.f) / 35.f);
    }

    CCustomOutfit* outfit = actor->GetOutfit();
    CHelmet* helmet = actor->GetHelmet();
    if ((outfit || helmet) && m_static_armor && m_ui_armor_bar)
    {
        m_static_armor->Show(true);
        m_ui_armor_bar->Show(true);
    }
    else if (m_static_armor && m_ui_armor_bar)
    {
        m_static_armor->Show(false);
        m_ui_armor_bar->Show(false);
    }

    if (actor->conditions().BleedingSpeed() > 0.01f && m_bleeding)
    {
        m_bleeding->Show(true);
    }
    else if (m_bleeding)
    {
        m_bleeding->Show(false);
    }
    if (m_progress_self)
    {
        m_progress_self->SetPos(m_radia_self);
    }
}

void CUIHudStatesWnd::HideCaliberHudWidgets()
{
    if (m_ui_caliber_text)
    {
        m_ui_caliber_text->Show(false);
    }
    if (m_ui_caliber_icon)
    {
        m_ui_caliber_icon->Show(false);
    }
    // XML may omit caliber_text: group label is drawn on m_ui_weapon_icon
    if (!m_ui_caliber_text && m_ui_weapon_icon && m_wpnIconHudMode == EWpnIconHudMode::Caliber)
    {
        m_ui_weapon_icon->SetText("");
    }
}

shared_str CUIHudStatesWnd::ResolveFireModeDisplayText(const shared_str& fireModeCode) const
{
    if (!m_use_fire_mode_text_labels || !fireModeCode.size() || !g_pStringTable)
    {
        return fireModeCode;
    }

    const auto it = m_fire_mode_label_map.find(fireModeCode);
    if (it == m_fire_mode_label_map.end() || !it->second.size())
    {
        return fireModeCode;
    }

    return g_pStringTable->translate(it->second);
}

void CUIHudStatesWnd::UpdateCaliberHudForItem(CInventoryItem* item)
{
    if (m_wpnIconHudMode != EWpnIconHudMode::Caliber || !m_hud_group_catalog || !m_hud_group_catalog->IsLoaded())
    {
        return;
    }
    if (!item)
    {
        HideCaliberHudWidgets();
        return;
    }

    CWeapon* wpn = item->cast_weapon();
    if (!wpn || wpn->cast_weapon_knife() || wpn->cast_weapon_binoculars())
    {
        HideCaliberHudWidgets();
        return;
    }

    if (wpn->m_ammoTypes.empty())
    {
        HideCaliberHudWidgets();
        return;
    }

    const u8 ammoType = wpn->GetAmmoType();
    if (ammoType >= wpn->m_ammoTypes.size())
    {
        HideCaliberHudWidgets();
        return;
    }

    const shared_str ammoSect = wpn->m_ammoTypes[ammoType];
    shared_str sectionLabel;
    u32 groupIdx = 0;
    if (!m_hud_group_catalog->TryGetSection(ammoSect, groupIdx, sectionLabel))
    {
        HideCaliberHudWidgets();
        return;
    }

    if (!ApplyCaliberCatalogWeaponRow(
            *m_hud_group_catalog, groupIdx, sectionLabel, m_ui_caliber_text, m_ui_weapon_icon, m_ui_caliber_icon))
    {
        HideCaliberHudWidgets();
        return;
    }
}

void CUIHudStatesWnd::UpdateActiveItemInfo(CActor* actor)
{
    PIItem item = actor->inventory().ActiveItem();
    if (item)
    {
        if (item->cast_bolt() || item->cast_custom_detector() || (m_use_adaptive_ammo_widget && (item->cast_weapon_knife() || item->cast_weapon_binoculars())))
        {
            if (m_ui_weapon_icon)
            {
                m_ui_weapon_icon->SetText("");
                m_ui_weapon_icon->SetVisual(nullptr);
                m_ui_weapon_icon->TextureOff();
                m_ui_weapon_icon->SetTextureColor(color_rgba(255, 255, 255, 0));
                m_ui_weapon_icon->Show(true);
            }

            if (m_fire_mode)
            {
                m_fire_mode->Show(false);
            }
            if (m_ui_fire_mode_icon)
            {
                m_ui_fire_mode_icon->Show(false);
            }
            if (m_ui_weapon_cur_ammo)
            {
                m_ui_weapon_cur_ammo->Show(false);
            }
            if (m_ui_weapon_fmj_ammo)
            {
                m_ui_weapon_fmj_ammo->Show(false);
            }
            if (m_ui_weapon_ap_ammo)
            {
                m_ui_weapon_ap_ammo->Show(false);
            }
            if (m_ui_weapon_third_ammo)
            {
                m_ui_weapon_third_ammo->Show(false);
            }
            if (m_ui_weapon_sign_ammo)
            {
                m_ui_weapon_sign_ammo->Show(false);
            }
            if (m_ui_grenade)
            {
                m_ui_grenade->Show(false);
            }
            if (m_ui_adaptive_clip)
            {
                m_ui_adaptive_clip->SetText("");
                m_ui_adaptive_clip->Show(false);
            }
            if (m_ui_adaptive_total)
            {
                m_ui_adaptive_total->SetText("");
                m_ui_adaptive_total->Show(false);
            }
            HideCaliberHudWidgets();

            if (m_wpnIconHudMode == EWpnIconHudMode::Caliber && m_hud_group_catalog && m_hud_group_catalog->IsLoaded() && m_ui_weapon_icon)
            {
                ApplyCaliberCatalogGadgetLine(*m_hud_group_catalog,
                    item->m_section_id,
                    m_ui_caliber_text,
                    m_ui_weapon_icon,
                    m_ui_caliber_icon,
                    item->NameShort());
            }

            if (m_static_weapon)
            {
                m_static_weapon->SetText(item->NameShort());
            }
            return;
        }

        if (item->cast_weapon_knife())
        {
            m_item_info.clear();
            if (m_use_adaptive_ammo_widget)
            {
                if (m_wpnIconHudMode == EWpnIconHudMode::AmmoText && m_ui_weapon_icon)
                {
                    m_ui_weapon_icon->SetTextureColor(color_rgba(255, 255, 255, 0));
                    m_ui_weapon_icon->SetText(item->NameShort());
                    m_ui_weapon_icon->Show(true);
                }
                else if (m_ui_weapon_icon)
                {
                    m_ui_weapon_icon->Show(false);
                    m_ui_weapon_icon->SetText("");
                }
            }
            else
            {
                if (m_ui_weapon_icon)
                {
                    SetAmmoIcon(item->m_section_id);
                }
            }
            if (m_static_weapon)
            {
                m_static_weapon->SetText(item->NameShort());
            }
            if (m_fire_mode)
            {
                m_fire_mode->Show(false);
            }
            if (m_ui_fire_mode_icon)
            {
                m_ui_fire_mode_icon->Show(false);
            }
            if (m_ui_weapon_cur_ammo)
            {
                m_ui_weapon_cur_ammo->Show(false);
            }
            if (m_ui_weapon_fmj_ammo)
            {
                m_ui_weapon_fmj_ammo->Show(false);
            }
            if (m_ui_weapon_ap_ammo)
            {
                m_ui_weapon_ap_ammo->Show(false);
            }
            if (m_ui_weapon_third_ammo)
            {
                m_ui_weapon_third_ammo->Show(false);
            }
            if (m_ui_weapon_sign_ammo)
            {
                m_ui_weapon_sign_ammo->Show(false);
            }
            if (m_ui_grenade)
            {
                m_ui_grenade->Show(false);
            }
            if (m_ui_adaptive_clip)
            {
                m_ui_adaptive_clip->SetText("");
                m_ui_adaptive_clip->Show(false);
            }
            if (m_ui_adaptive_total)
            {
                m_ui_adaptive_total->SetText("");
                m_ui_adaptive_total->Show(false);
            }
            HideCaliberHudWidgets();
            return;
        }

        if (m_b_force_update)
        {
            if (item->cast_weapon())
            {
                item->cast_weapon()->ForceUpdateAmmo();
            }
            m_b_force_update = false;
        }

        item->GetBriefInfo(m_item_info);

        if (m_static_weapon)
        {
            string256 ammoName;
            if (m_item_info.fire_mode.size())
            {
                xr_sprintf(ammoName, sizeof(ammoName), "%s (%s)", m_item_info.name.c_str(), m_item_info.fire_mode.c_str());
            }
            else
            {
                xr_sprintf(ammoName, "%s", m_item_info.name.c_str());
            }

            m_static_weapon->SetText(ammoName);
        }

        // Fire mode display: localized labels, icons, or raw queue code
        if (m_fire_mode)
        {
            if (m_use_fire_mode_text_labels)
            {
                m_fire_mode->SetText(ResolveFireModeDisplayText(m_item_info.fire_mode).c_str());
                m_fire_mode->Show(true);
                if (m_ui_fire_mode_icon)
                {
                    m_ui_fire_mode_icon->Show(false);
                }
            }
            else if (m_use_fire_mode_icons && m_ui_fire_mode_icon)
            {
                // Icon mode
                shared_str fire_mode_str = m_item_info.fire_mode.c_str();
                auto it = m_fire_mode_icon_map.find(fire_mode_str);

                if (it != m_fire_mode_icon_map.end())
                {
                    shared_str icon_name = it->second;

                    // Try to load texture from ui_textures_descr
                    if (CUITextureMaster::ItemExist(icon_name.c_str()))
                    {
                        m_ui_fire_mode_icon->InitTexture(icon_name.c_str());
                        m_ui_fire_mode_icon->SetStretchTexture(true);
                        m_ui_fire_mode_icon->Show(true);
                        m_fire_mode->Show(false);
                    }
                    else
                    {
                        // Fallback to text if icon not found
                        m_fire_mode->SetText(ResolveFireModeDisplayText(m_item_info.fire_mode).c_str());
                        m_fire_mode->Show(true);
                        m_ui_fire_mode_icon->Show(false);
                    }
                }
                else
                {
                    // No mapping found, use text
                    m_fire_mode->SetText(ResolveFireModeDisplayText(m_item_info.fire_mode).c_str());
                    m_fire_mode->Show(true);
                    m_ui_fire_mode_icon->Show(false);
                }
            }
            else
            {
                // Text mode (default raw codes: 1 / A / 2 / 3)
                m_fire_mode->SetText(m_item_info.fire_mode.c_str());
                m_fire_mode->Show(true);
                if (m_ui_fire_mode_icon)
                {
                    m_ui_fire_mode_icon->Show(false);
                }
            }
        }

        // Weapon icon: mode is chosen from static_wpn_icon XML (Caliber > AmmoText > Legacy)
        if (m_wpnIconHudMode == EWpnIconHudMode::AmmoText)
        {
            shared_str displayText;

            if (item->cast_weapon_binoculars())
            {
                displayText._set(item->NameShort());
            }
            else if (item->cast_grenade())
            {
                displayText._set(item->NameShort());
            }
            else if (item->cast_bolt())
            {
                displayText._set(item->NameShort());
            }
            else
            {
                CWeapon* weapon = item->cast_weapon();
                if (weapon)
                {
                    if (m_weapon_icon_show_weapon_name)
                    {
                        displayText._set(item->NameShort());
                    }
                    else if (weapon->m_ammoTypes.size() > 0)
                    {
                        u8 currAmmoType = weapon->GetAmmoType();
                        if (currAmmoType < weapon->m_ammoTypes.size())
                        {
                            const char* ammoSection = weapon->m_ammoTypes[currAmmoType].c_str();
                            if (pSettings->section_exist(ammoSection))
                            {
                                shared_str invNameShortId = pSettings->r_string(ammoSection, "inv_name_short");
                                displayText._set(g_pStringTable->translate(invNameShortId));
                            }
                        }
                    }
                }
            }

            if (displayText.size())
            {
                m_ui_weapon_icon->SetTextureColor(color_rgba(255, 255, 255, 0));
                m_ui_weapon_icon->SetText(displayText.c_str());
                m_ui_weapon_icon->Show(true);
            }
            else
            {
                m_ui_weapon_icon->Show(false);
            }
        }
        else if (m_wpnIconHudMode == EWpnIconHudMode::Caliber)
        {
            // Labels are on caliber_text / caliber_icon; parent must not draw SetAmmoIcon (would cover children visually)
            m_ui_weapon_icon->SetText("");
            m_ui_weapon_icon->SetVisual(nullptr);
            m_ui_weapon_icon->TextureOff();
            m_ui_weapon_icon->SetTextureColor(color_rgba(255, 255, 255, 0));
            m_ui_weapon_icon->Show(true);
        }
        else
        {
            m_ui_weapon_icon->TextureOn();
            m_ui_weapon_icon->SetText("");
            m_ui_weapon_icon->SetTextureColor(color_rgba(255, 255, 255, 255));
            SetAmmoIcon(m_item_info.icon.c_str());
        }

        if (m_use_adaptive_ammo_widget && m_ui_adaptive_clip && m_ui_adaptive_total)
        {
            CGrenade* grenade = item->cast_grenade();
            if (grenade)
            {
                const int clipCount = 1;
                int totalCount = m_item_info.cur_ammo.size() ? atoi(m_item_info.cur_ammo.c_str()) : 0;
                const bool isTotalInfinity = (xr_strcmp(m_item_info.cur_ammo.c_str(), "∞") == 0);
                totalCount = (totalCount >= 0) ? totalCount : 0;

                string64 clipBuf;
                string64 totalBuf;
                const bool hasSeparator = m_adaptive_total_separator[0] != 0;
                xr_sprintf(clipBuf, "%d", clipCount);
                if (isTotalInfinity)
                {
                    if (hasSeparator)
                    {
                        xr_sprintf(totalBuf, "%s %s", m_adaptive_total_separator, "∞");
                    }
                    else
                    {
                        xr_strcpy(totalBuf, "∞");
                    }
                }
                else
                {
                    if (hasSeparator)
                    {
                        xr_sprintf(totalBuf, "%s %d", m_adaptive_total_separator, totalCount);
                    }
                    else
                    {
                        xr_sprintf(totalBuf, "%d", totalCount);
                    }
                }

                m_ui_adaptive_clip->SetText(clipBuf);
                m_ui_adaptive_clip->Show(true);
                m_ui_adaptive_total->SetText(totalBuf);
                m_ui_adaptive_total->Show(true);

                if (m_ui_weapon_cur_ammo)
                {
                    m_ui_weapon_cur_ammo->Show(false);
                }
                if (m_ui_weapon_fmj_ammo)
                {
                    m_ui_weapon_fmj_ammo->Show(false);
                }
                if (m_ui_weapon_ap_ammo)
                {
                    m_ui_weapon_ap_ammo->Show(false);
                }
                if (m_ui_weapon_third_ammo)
                {
                    m_ui_weapon_third_ammo->Show(false);
                }
                if (m_ui_grenade)
                {
                    m_ui_grenade->Show(false);
                }
                HideCaliberHudWidgets();
                return;
            }

            CWeapon* weapon = item->cast_weapon();
            if (weapon && !weapon->cast_weapon_binoculars())
            {
                int clipCount = 0;
                int totalCount = 0;
                bool isClipInfinity = false;
                bool isTotalInfinity = false;

                CWeaponMagazinedWGrenade* wpnGL = item->cast_weapon_magazined_w_grenade();
                if (wpnGL && wpnGL->m_bGrenadeMode)
                {
                    isClipInfinity = (xr_strcmp(m_item_info.cur_ammo.c_str(), "∞") == 0);
                    isTotalInfinity = (xr_strcmp(m_item_info.grenade.c_str(), "∞") == 0);
                    if (!isClipInfinity)
                    {
                        clipCount = m_item_info.cur_ammo.size() ? atoi(m_item_info.cur_ammo.c_str()) : 0;
                    }
                    if (!isTotalInfinity)
                    {
                        totalCount = m_item_info.grenade.size() ? atoi(m_item_info.grenade.c_str()) : 0;
                    }
                }
                else
                {
                    CWeaponMagazined* wpnM = item->cast_weapon_magazined();
                    if (wpnM)
                    {
                        isClipInfinity = (xr_strcmp(m_item_info.cur_ammo.c_str(), "∞") == 0);
                        if (!isClipInfinity)
                        {
                            if (m_item_info.cur_ammo.size())
                            {
                                clipCount = atoi(m_item_info.cur_ammo.c_str());
                            }
                            else
                            {
                                clipCount = wpnM->GetAmmoElapsed() + wpnM->GetAmmoChamberElapsed();
                            }
                        }
                        const char* ammoStr = nullptr;
                        if (wpnM->m_ammoType == 0 && m_item_info.fmj_ammo.size())
                        {
                            ammoStr = m_item_info.fmj_ammo.c_str();
                        }
                        else if (wpnM->m_ammoType == 1 && m_item_info.ap_ammo.size())
                        {
                            ammoStr = m_item_info.ap_ammo.c_str();
                        }
                        else if (wpnM->m_ammoType == 2 && m_item_info.third_ammo.size())
                        {
                            ammoStr = m_item_info.third_ammo.c_str();
                        }
                        if (ammoStr)
                        {
                            isTotalInfinity = (xr_strcmp(ammoStr, "∞") == 0);
                            if (!isTotalInfinity)
                            {
                                totalCount = atoi(ammoStr);
                            }
                        }
                    }
                }

                clipCount = (clipCount >= 0) ? clipCount : 0;
                totalCount = (totalCount >= 0) ? totalCount : 0;

                string64 clipBuf;
                string64 totalBuf;
                const bool hasSeparator = m_adaptive_total_separator[0] != 0;
                if (isClipInfinity)
                {
                    xr_strcpy(clipBuf, "∞");
                }
                else
                {
                    xr_sprintf(clipBuf, "%d", clipCount);
                }
                if (isTotalInfinity)
                {
                    if (hasSeparator)
                    {
                        xr_sprintf(totalBuf, "%s %s", m_adaptive_total_separator, "∞");
                    }
                    else
                    {
                        xr_strcpy(totalBuf, "∞");
                    }
                }
                else
                {
                    if (hasSeparator)
                    {
                        xr_sprintf(totalBuf, "%s %d", m_adaptive_total_separator, totalCount);
                    }
                    else
                    {
                        xr_sprintf(totalBuf, "%d", totalCount);
                    }
                }

                m_ui_adaptive_clip->SetText(clipBuf);
                m_ui_adaptive_clip->Show(true);
                m_ui_adaptive_total->SetText(totalBuf);
                m_ui_adaptive_total->Show(true);

                if (m_ui_weapon_cur_ammo)
                {
                    m_ui_weapon_cur_ammo->Show(false);
                }
                if (m_ui_weapon_fmj_ammo)
                {
                    m_ui_weapon_fmj_ammo->Show(false);
                }
                if (m_ui_weapon_ap_ammo)
                {
                    m_ui_weapon_ap_ammo->Show(false);
                }
                if (m_ui_weapon_third_ammo)
                {
                    m_ui_weapon_third_ammo->Show(false);
                }
                if (m_ui_grenade)
                {
                    m_ui_grenade->Show(false);
                }
                UpdateCaliberHudForItem(item);
                return;
            }
            else
            {
                m_ui_adaptive_clip->SetText("");
                m_ui_adaptive_clip->Show(false);
                m_ui_adaptive_total->SetText("");
                m_ui_adaptive_total->Show(false);
                HideCaliberHudWidgets();
                return;
            }
        }

        if (m_ui_weapon_cur_ammo)
        {
            m_ui_weapon_cur_ammo->Show(true);
            m_ui_weapon_cur_ammo->SetText(m_item_info.cur_ammo.c_str());
        }

        if (m_wpnIconHudMode == EWpnIconHudMode::Caliber)
        {
            if (m_ui_weapon_fmj_ammo)
            {
                m_ui_weapon_fmj_ammo->Show(false);
            }
            if (m_ui_weapon_ap_ammo)
            {
                m_ui_weapon_ap_ammo->Show(false);
            }
        }
        else
        {
            if (m_ui_weapon_fmj_ammo)
            {
                m_ui_weapon_fmj_ammo->Show(true);
                m_ui_weapon_fmj_ammo->SetText(m_item_info.fmj_ammo.c_str());
                m_ui_weapon_fmj_ammo->SetTextColor(m_ui_weapon_ammo_color_inactive);
            }

            if (m_ui_weapon_ap_ammo)
            {
                m_ui_weapon_ap_ammo->Show(true);
                m_ui_weapon_ap_ammo->SetText(m_item_info.ap_ammo.c_str());
                m_ui_weapon_ap_ammo->SetTextColor(m_ui_weapon_ammo_color_inactive);
            }
        }

        if (m_ui_weapon_third_ammo)
        {
            m_ui_weapon_third_ammo->Show(true);
            m_ui_weapon_third_ammo->SetText(m_item_info.third_ammo.c_str());
            m_ui_weapon_third_ammo->SetTextColor(m_ui_weapon_ammo_color_inactive);
        }

        // Control visibility via alpha channel (active - opaque, inactive - semi-transparent)
        CWeaponMagazined* wpnm = item->cast_weapon_magazined();
        if (m_wpnIconHudMode != EWpnIconHudMode::Caliber)
        {
            if (wpnm)
            {
                if (wpnm->m_ammoType == 0 && m_ui_weapon_fmj_ammo)
                {
                    m_ui_weapon_fmj_ammo->SetTextColor(m_ui_weapon_ammo_color_active);
                }
                else if (wpnm->m_ammoType == 1 && m_ui_weapon_ap_ammo)
                {
                    m_ui_weapon_ap_ammo->SetTextColor(m_ui_weapon_ammo_color_active);
                }
                else if (wpnm->m_ammoType == 2 && m_ui_weapon_third_ammo)
                {
                    m_ui_weapon_third_ammo->SetTextColor(m_ui_weapon_ammo_color_active);
                }
            }
        }
        else if (wpnm && m_ui_weapon_third_ammo)
        {
            if (wpnm->m_ammoType == 2)
            {
                m_ui_weapon_third_ammo->SetTextColor(m_ui_weapon_ammo_color_active);
            }
            else
            {
                m_ui_weapon_third_ammo->SetTextColor(m_ui_weapon_ammo_color_inactive);
            }
        }

        if (m_ui_weapon_sign_ammo)
        {
            if (m_item_info.cur_ammo.size())
            {
                string64 temp;
                if (item->cast_missile() && item->cast_missile()->cast_grenade())
                {
                    xr_sprintf(temp, "%s", m_item_info.cur_ammo.c_str());
                }
                else
                {
                    xr_sprintf(temp, "%s/%s", m_item_info.cur_ammo.c_str(), m_item_info.total_ammo.c_str());
                }

                m_ui_weapon_sign_ammo->Show(true);
                m_ui_weapon_sign_ammo->SetText(temp);
            }
            else
            {
                m_ui_weapon_sign_ammo->Show(false);
            }
        }

        if (m_fire_mode)
        {
            m_fire_mode->Show(true);
        }

        if (m_ui_grenade)
        {
            m_ui_grenade->Show(true);
            m_ui_grenade->SetText(m_item_info.grenade.c_str());

            CWeaponMagazinedWGrenade* wpn = item->cast_weapon_magazined_w_grenade();
            if (wpn && wpn->m_bGrenadeMode)
            {
                m_ui_grenade->SetTextColor(m_ui_weapon_ammo_color_active);
            }
            else
            {
                m_ui_grenade->SetTextColor(m_ui_weapon_ammo_color_inactive);
            }
        }

        UpdateCaliberHudForItem(item);
    }
    else
    {
        m_ui_weapon_icon->Show(false);
        HideCaliberHudWidgets();

        if (m_ui_weapon_cur_ammo)
        {
            m_ui_weapon_cur_ammo->Show(false);
        }

        if (m_ui_adaptive_clip)
        {
            m_ui_adaptive_clip->SetText("");
            m_ui_adaptive_clip->Show(false);
        }
        if (m_ui_adaptive_total)
        {
            m_ui_adaptive_total->SetText("");
            m_ui_adaptive_total->Show(false);
        }

        if (m_ui_weapon_fmj_ammo)
        {
            m_ui_weapon_fmj_ammo->Show(false);
        }

        if (m_ui_weapon_ap_ammo)
        {
            m_ui_weapon_ap_ammo->Show(false);
        }

        if (m_ui_weapon_sign_ammo)
        {
            m_ui_weapon_sign_ammo->Show(false);
        }

        if (m_ui_weapon_third_ammo)
        {
            m_ui_weapon_third_ammo->Show(false); // Alundaio: Third Ammo
        }

        if (m_static_weapon)
        {
            m_static_weapon->SetText("");
        }

        if (m_fire_mode)
        {
            m_fire_mode->Show(false);
        }

        if (m_ui_fire_mode_icon)
        {
            m_ui_fire_mode_icon->Show(false);
        }

        if (m_ui_grenade)
        {
            m_ui_grenade->Show(false);
        }
    }
}

void CUIHudStatesWnd::SetAmmoIcon(const shared_str& sect_name)
{
    if (!sect_name.size())
    {
        m_ui_weapon_icon->Show(false);
        return;
    }
    m_ui_weapon_icon->Show(true);

    InventoryUtilities::InventoryIconParams icons_struct =
        InventoryUtilities::GetInventoryIconParams(sect_name.c_str());
    if (psActorFlags.test(AF_3D_ICONS_INV))
    {
        m_ui_weapon_icon->SetVisual(icons_struct._3d_static_visual);
        m_ui_weapon_icon->SetXYZ(icons_struct._3d_static_rotate);
        m_ui_weapon_icon->SetScaleFactor(icons_struct._3d_static_scale);
    }
    else
    {
        m_ui_weapon_icon->SetVisual(nullptr);
    }

    Frect texture_rect;
    float scaleIcon = icons_struct.scaleIcon;
    texture_rect.x1 = icons_struct.inv_grid_x * INV_GRID_WIDTH(scaleIcon);
    texture_rect.y1 = icons_struct.inv_grid_y * INV_GRID_HEIGHT(scaleIcon);
    texture_rect.x2 = icons_struct.inv_grid_width * INV_GRID_WIDTH(scaleIcon);
    texture_rect.y2 = icons_struct.inv_grid_height * INV_GRID_HEIGHT(scaleIcon);
    texture_rect.rb.add             (texture_rect.lt);
    m_ui_weapon_icon->GetUIStaticItem().SetTextureRect(texture_rect);
    m_ui_weapon_icon->SetStretchTexture(true);

    if (psActorFlags.test(AF_3D_ICONS_INV))
    {
        m_ui_weapon_icon->SetVisual(icons_struct._3d_static_visual);

        m_ui_weapon_icon->SetScaleFactor(icons_struct._3d_static_scale);
        Fvector fRot = icons_struct._3d_static_rotate;
        m_ui_weapon_icon->SetXYZ(fRot);
    }
    else
    {
        m_ui_weapon_icon->SetVisual(nullptr);
    }

    m_ui_weapon_icon->SetShader(InventoryUtilities::GetEquipmentIconsShader(icons_struct.icons_texture));

    float h = texture_rect.height() * EngineExternal().GetWeaponIconScaling();
    float w = texture_rect.width() * EngineExternal().GetWeaponIconScaling();

    // now perform only width scale for ammo, which (W)size >2
    if (texture_rect.width() > 2.01f * INV_GRID_WIDTH(scaleIcon))
    {
        w = INV_GRID_WIDTH(scaleIcon) * 1.5f;
    }

    m_ui_weapon_icon->SetWidth(w * UI().get_current_kx() / scaleIcon);
    m_ui_weapon_icon->SetHeight(h / scaleIcon);
}
// ------------------------------------------------------------------------------------------------
void CUIHudStatesWnd::UpdateZones()
{
    //float actor_radia = m_actor->conditions().GetRadiation() * m_actor_radia_factor;
    //m_radia_hit = _max( m_zone_cur_power[it_rad], actor_radia );

    CActor* actor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
    if ( !actor )
    {
        return;
    }
    CPda* const pda = actor->GetPDA();
    if (pda)
    {
        for (CObject* O : pda->feel_touch)
        {
            CBaseMonster* monster = O&&O->cast_game_object() ? O->cast_game_object()->cast_base_monster() : nullptr;
            if (!monster || !monster->g_Alive())
            {
                continue;
            }

            monster->play_detector_sound();
        }
    }


    m_radia_self = actor->conditions().GetRadiation();

    float zone_max_power = actor->conditions().GetZoneMaxPower(ALife::infl_rad);
    float power          = actor->conditions().GetInjuriousMaterialDamage();
    power = power / zone_max_power;
    clamp( power, 0.0f, 1.1f );
    if ( m_zone_cur_power[ALife::infl_rad] < power )
    {
        m_zone_cur_power[ALife::infl_rad] = power;
    }
    m_radia_hit = m_zone_cur_power[ALife::infl_rad];

/*  if ( Device.dwFrame % 20 == 0 )
    {
        Msg(" self = %.2f   hit = %.2f", m_radia_self, m_radia_hit );
    }*/
    float detectRadZonePower = std::max(actor->conditions().m_fRadiationZonePower, power * 10);
    if (m_arrow)
    {
        m_arrow->SetNewValue(detectRadZonePower);
    }
    if (m_arrow_shadow)
    {
        m_arrow_shadow->SetPos( m_arrow->GetPos() );
    }
/*
    power = actor->conditions().GetPsy();
    clamp( power, 0.0f, 1.1f );
    if ( m_zone_cur_power[ALife::infl_psi] < power )
    {
        m_zone_cur_power[ALife::infl_psi] = power;
    }
*/

    if ( !Level().hud_zones_list )
    {
        return;
    }

    for (int i = 0; i < ALife::infl_max_count; ++i)
    {
        if ( Device.fTimeDelta < 1.0f )
        {
            m_zone_cur_power[i] *= 0.9f * (1.0f - Device.fTimeDelta);
        }
        if ( m_zone_cur_power[i] < 0.01f )
        {
            m_zone_cur_power[i] = 0.0f;
        }
    }

    Fvector posf;
    posf.set(Level().CurrentControlEntity()->Position());
    Level().hud_zones_list->feel_touch_update( posf, m_zone_feel_radius_max );

    if ( Level().hud_zones_list->m_ItemInfos.size() == 0 )
    {
        return;
    }

    CZoneList::ItemsMapIt itb   = Level().hud_zones_list->m_ItemInfos.begin();
    CZoneList::ItemsMapIt ite   = Level().hud_zones_list->m_ItemInfos.end();
    for ( ; itb != ite; ++itb )
    {
        CAnomalyZone*       pZone = itb->first;
        ITEM_INFO&          zone_info = itb->second;
        ITEM_TYPE*          zone_type = zone_info.curr_ref;

        ALife::EHitType         hit_type = pZone->GetHitType();
        ALife::EInfluenceType   z_type = get_indik_type( hit_type );
/*      if ( z_type == indik_type_max )
        {
            continue;
        }
*/

        Fvector P           = Level().CurrentControlEntity()->Position();
        P.y                 -= 0.5f;
        float dist_to_zone  = 0.0f;
        float rad_zone      = 0.0f;
        pZone->CalcDistanceTo( P, dist_to_zone, rad_zone );
        clamp( dist_to_zone, 0.0f, flt_max * 0.5f );

        float fRelPow = ( dist_to_zone / (rad_zone + (z_type==ALife::infl_max_count)? 5.0f : m_zone_feel_radius[z_type] + 0.1f) ) - 0.1f;

        zone_max_power = actor->conditions().GetZoneMaxPower(z_type);
        power = pZone->Power( dist_to_zone, rad_zone );
        //power = power / zone_max_power;
        clamp( power, 0.0f, 1.1f );

        if ( (z_type!=ALife::infl_max_count) && (m_zone_cur_power[z_type] < power) ) //max
        {
            m_zone_cur_power[z_type] = power;
        }

        if ( dist_to_zone < rad_zone + 0.9f * ((z_type==ALife::infl_max_count)?5.0f:m_zone_feel_radius[z_type]) )
        {
            fRelPow *= 0.6f;
            if ( dist_to_zone < rad_zone )
            {
                fRelPow *= 0.3f;
                fRelPow *= ( 2.5f - 2.0f * power ); // звук зависит от силы зоны
            }
        }
        clamp( fRelPow, 0.0f, 1.0f );

        //определить текущую частоту срабатывания сигнала
        zone_info.cur_period = zone_type->freq.x + (zone_type->freq.y - zone_type->freq.x) * (fRelPow * fRelPow);

        if (zone_info.snd_time > zone_info.cur_period)
        {
            zone_info.snd_time = 0.0f;

            bool UseTochSound = true;
            if (m_isZoneTouch)
            {
                luabind::functor<bool> funct;
                R_ASSERT2(ai().script_engine().functor(m_onZoneTouch, funct), "Not found callback: OnZoneTouch");
                UseTochSound = funct(pZone->lua_game_object());
            }

            if (UseTochSound)
            {
                HUD_SOUND_ITEM::PlaySound(zone_type->detect_snds, Fvector().set(0, 0, 0), nullptr, true, false);
            }
        }
        else
        {
            zone_info.snd_time += Device.fTimeDelta;
        }
    }
}

void CUIHudStatesWnd::UpdateIndicators( CActor* actor )
{
    if (m_fake_indicators_update)
    {
        return;
    }

    UpdateSatiety(actor);

    for ( int i = 0; i < it_max ; ++i ) // it_max = ALife::infl_max_count-1
    {
        if (!m_indik[i])
        {
            return;
        }

        UpdateIndicatorType( actor, (ALife::EInfluenceType)i );
    }
}

void CUIHudStatesWnd::UpdateSatiety(CActor* actor) {
    float satiety = actor->conditions().GetSatiety();
    float satiety_critical = actor->conditions().SatietyCritical();
    float satiety_koef = (satiety - satiety_critical) / (satiety >= satiety_critical ? 1 - satiety_critical : satiety_critical);

    if (m_ind_starvation && satiety_koef > 0.5)
    {
        m_ind_starvation->SetTextureColor(color_rgba(255, 255, 255, 255));
    }
    else if (m_ind_starvation)
    {
        if (satiety_koef > 0.0f)
        {
            m_ind_starvation->SetTextureColor(color_rgba(0, 255, 0, 255));
        }
        else if (satiety_koef > -0.5f) {
            m_ind_starvation->SetTextureColor(color_rgba(255, 255, 0, 255));
        }
        else {
            m_ind_starvation->SetTextureColor(color_rgba(255, 0, 0, 255));
        }
    }
}

void CUIHudStatesWnd::UpdateIndicatorType( CActor* actor, ALife::EInfluenceType type )
{
    if ( type < ALife::infl_rad || ALife::infl_psi < type )
    {
        VERIFY2( 0, "Failed EIndicatorType for CStatic!" );
        return;
    }
    if (!m_indik[type])
    {
        return;
    }

    constexpr u32 c_white  = color_rgba( 255, 255, 255, 255 );
    constexpr u32 c_green  = color_rgba( 0, 255, 0, 255 );
    constexpr u32 c_yellow = color_rgba( 255, 255, 0, 255 );
    constexpr u32 c_red    = color_rgba( 255, 0, 0, 255 );

    const char* texture = "";
    string256 str;
    switch (type)
    {
        case ALife::infl_rad: texture = "ui_inGame2_triangle_Radiation_"; break;
        case ALife::infl_fire: texture = "ui_inGame2_triangle_Fire_"; break;
        case ALife::infl_acid: texture = "ui_inGame2_triangle_Biological_"; break;
        case ALife::infl_psi: texture = "ui_inGame2_triangle_Psy_"; break;
        default: NODEFAULT;
    }
    float           hit_power = m_zone_cur_power[type];
    ALife::EHitType hit_type  = m_zone_hit_type[type];

    CCustomOutfit* outfit = actor->GetOutfit();
    CHelmet* helmet = actor->GetHelmet();
    float protect = (outfit) ? outfit->GetDefHitTypeProtection( hit_type ) : 0.0f;
    protect += (helmet) ? helmet->GetDefHitTypeProtection(hit_type) : 0.0f;
    protect += actor->GetProtection_ArtefactsOnBelt( hit_type );

    CEntityCondition::BOOSTER_MAP& cur_booster_influences = actor->conditions().GetCurBoosterInfluences();
    CEntityCondition::BOOSTER_MAP::const_iterator it;
    if (hit_type == ALife::eHitTypeChemicalBurn)
    {
        it = cur_booster_influences.find(eBoostChemicalBurnProtection);
        if (it != cur_booster_influences.end())
        {
            protect += it->second.fBoostValue;
        }
    }
    else if (hit_type == ALife::eHitTypeRadiation)
    {
        it = cur_booster_influences.find(eBoostRadiationProtection);
        if (it != cur_booster_influences.end())
        {
            protect += it->second.fBoostValue;
        }
    }
    else if (hit_type == ALife::eHitTypeTelepatic)
    {
        it = cur_booster_influences.find(eBoostTelepaticProtection);
        if (it != cur_booster_influences.end())
        {
            protect += it->second.fBoostValue;
        }
    }

//  float max_power = actor->conditions().GetZoneMaxPower( hit_type );
//  protect = protect / max_power; // = 0..1
    m_indik[type]->Show(true);

    if (hit_power < EPS)
    {
        string256 greenTexture;
        // If we have green texture and white is missing
        // Assume it's CoP and use it's standard scheme
        xr_sprintf(greenTexture, sizeof(greenTexture), "%s%s", texture, "green");

        SwitchLA(false, type);
        xr_sprintf(str, sizeof(str), "%s%s", texture, "white");
        texture = str;

        if (CUITextureMaster::ItemExist(texture))
        {
            m_indik[type]->InitTexture(texture);
        }
        else if (CUITextureMaster::ItemExist(greenTexture))
            m_indik[type]->Show(false); // Use standard CoP scheme
        else
        {
            m_indik[type]->SetTextureColor(c_white);
        }

        actor->conditions().SetZoneDanger(0.0f, type);
        return;
    }

    m_indik[type]->Show(true);
    if ( hit_power <= protect )
    {
        SwitchLA( false, type );
        xr_sprintf(str, sizeof(str), "%s%s", texture, "green");
        texture = str;

        if (CUITextureMaster::ItemExist(texture))
        {
            m_indik[type]->InitTexture(texture);
        }
        else
        {
            m_indik[type]->SetTextureColor(c_green);
        }

        actor->conditions().SetZoneDanger( 0.0f, type );
        return;
    }
    if ( hit_power - protect < m_zone_threshold[type] )
    {
        SwitchLA( false, type );
        xr_sprintf(str, sizeof(str), "%s%s", texture, "yellow");
        texture = str;

        if (CUITextureMaster::ItemExist(texture))
        {
            m_indik[type]->InitTexture(texture);
        }
        else
        {
            m_indik[type]->SetTextureColor(c_yellow);
        }

        actor->conditions().SetZoneDanger( 0.0f, type );
        return;
    }
    SwitchLA( true, type );
    xr_sprintf(str, sizeof(str), "%s%s", texture, "red");
    texture = str;

    if (CUITextureMaster::ItemExist(texture))
    {
        m_indik[type]->InitTexture(texture);
    }
    else
    {
        m_indik[type]->SetTextureColor(c_red);
    }

    VERIFY(actor->conditions().GetZoneMaxPower(hit_type));
    actor->conditions().SetZoneDanger((hit_power-protect)/actor->conditions().GetZoneMaxPower(hit_type), type);
}

void CUIHudStatesWnd::SwitchLA( bool state, ALife::EInfluenceType type )
{
    if ( state == m_cur_state_LA[type] || !m_indik[type])
    {
        return;
    }

    if ( state )
    {
        m_indik[type]->SetColorAnimation( m_lanim_name.c_str(), LA_CYCLIC|LA_TEXTURECOLOR);
        m_cur_state_LA[type] = true;
    }
    else
    {
        m_indik[type]->SetColorAnimation( nullptr, 0);//off
        m_cur_state_LA[type] = false;
    }
}

float CUIHudStatesWnd::get_zone_cur_power( ALife::EHitType hit_type )
{
    ALife::EInfluenceType iz_type = get_indik_type( hit_type );
    if ( iz_type == ALife::infl_max_count )
    {
        return 0.0f;
    }
    return m_zone_cur_power[iz_type];
}

void CUIHudStatesWnd::DrawZoneIndicators()
{
    CActor* actor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
    if (!actor)
    {
        return;
    }

    UpdateIndicators(actor);

    for (int i = 0; i < it_max; ++i) // it_max = ALife::infl_max_count-1
    {
        if (m_indik[i] && m_indik[i]->IsShown())
        {
            m_indik[i]->Draw();
        }
    }
}

void CUIHudStatesWnd::FakeUpdateIndicatorType(u8 t, float power)
{
    ALife::EInfluenceType type = (ALife::EInfluenceType)t;
    if ( type < ALife::infl_rad || ALife::infl_psi < type )
    {
        VERIFY2( 0, "Failed EIndicatorType for CStatic!" );
        return;
    }

    CActor* actor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
    if (!actor)
    {
        return;
    }

    const char* texture = "";
    string128 str;
    switch (type)
    {
        case ALife::infl_rad: texture = "ui_inGame2_triangle_Radiation_"; break;
        case ALife::infl_fire: texture = "ui_inGame2_triangle_Fire_"; break;
        case ALife::infl_acid: texture = "ui_inGame2_triangle_Biological_"; break;
        case ALife::infl_psi: texture = "ui_inGame2_triangle_Psy_"; break;
        default: NODEFAULT;
    }
    float           hit_power = power;
    ALife::EHitType hit_type  = m_zone_hit_type[type];

    CCustomOutfit* outfit = actor->GetOutfit();
    CHelmet* helmet = actor->GetHelmet();
    float protect = (outfit) ? outfit->GetDefHitTypeProtection( hit_type ) : 0.0f;
    protect += (helmet) ? helmet->GetDefHitTypeProtection(hit_type) : 0.0f;
    protect += actor->GetProtection_ArtefactsOnBelt( hit_type );

    CEntityCondition::BOOSTER_MAP cur_booster_influences = actor->conditions().GetCurBoosterInfluences();
    CEntityCondition::BOOSTER_MAP::const_iterator it;
    if (hit_type == ALife::eHitTypeChemicalBurn)
    {
        it = cur_booster_influences.find(eBoostChemicalBurnProtection);
        if (it != cur_booster_influences.end())
        {
            protect += it->second.fBoostValue;
        }
    }
    else if (hit_type == ALife::eHitTypeRadiation)
    {
        it = cur_booster_influences.find(eBoostRadiationProtection);
        if (it != cur_booster_influences.end())
        {
            protect += it->second.fBoostValue;
        }
    }
    else if (hit_type == ALife::eHitTypeTelepatic)
    {
        it = cur_booster_influences.find(eBoostTelepaticProtection);
        if (it != cur_booster_influences.end())
        {
            protect += it->second.fBoostValue;
        }
    }

    float max_power = actor->conditions().GetZoneMaxPower( hit_type );
    protect = protect / max_power; // = 0..1

    if ( hit_power < EPS )
    {
        m_indik[type]->Show(false);
        actor->conditions().SetZoneDanger( 0.0f, type );
        return;
    }

    m_indik[type]->Show(true);
    if ( hit_power < protect )
    {
        xr_sprintf(str, sizeof(str), "%s%s", texture, "green");
        texture = str;
        m_indik[type]->InitTexture(texture);
        actor->conditions().SetZoneDanger( 0.0f, type );
        return;
    }
    if ( hit_power - protect < m_zone_threshold[type] )
    {
        xr_sprintf(str, sizeof(str), "%s%s", texture, "yellow");
        texture = str;
        m_indik[type]->InitTexture(texture);
        actor->conditions().SetZoneDanger( 0.0f, type );
        return;
    }
    xr_sprintf(str, sizeof(str), "%s%s", texture, "red");
    texture = str;
    m_indik[type]->InitTexture(texture);
    actor->conditions().SetZoneDanger( hit_power - protect, type );
}

void CUIHudStatesWnd::EnableFakeIndicators(bool enable)
{
    m_fake_indicators_update = enable;
}

bool CUIHudStatesWnd::IsContextualDisplayEnabled() const
{
    return psHUD_Flags.test(HUD_CONTEXTUAL_STATUS);
}

void CUIHudStatesWnd::LoadContextualDisplaySettings(CUIXml& xml, const char* path)
{
    string256 contextualPath;
    xr_sprintf(contextualPath, "%s:contextual_display", path);
    if (!xml.NavigateToNode(contextualPath, 0))
    {
        return;
    }

    m_context_show_speed = std::max(xml.ReadAttribFlt(contextualPath, 0, "show_speed", m_context_show_speed), 0.1f);
    m_context_hide_speed = std::max(xml.ReadAttribFlt(contextualPath, 0, "hide_speed", m_context_hide_speed), 0.1f);
    m_context_hide_delay = std::max(xml.ReadAttribFlt(contextualPath, 0, "hide_delay", m_context_hide_delay), 0.1f);
    m_context_health_threshold = std::max(xml.ReadAttribFlt(contextualPath, 0, "health_threshold", m_context_health_threshold), EPS);
}

void CUIHudStatesWnd::TriggerHealthContext()
{
    if (!IsContextualDisplayEnabled())
    {
        return;
    }

    m_health_context_active = true;
    m_health_context_last_time = Device.fTimeGlobal;
}

void CUIHudStatesWnd::TriggerWeaponContext()
{
    if (!IsContextualDisplayEnabled())
    {
        return;
    }

    m_weapon_context_active = true;
    m_weapon_context_last_time = Device.fTimeGlobal;
}

void CUIHudStatesWnd::UpdateContextualTriggers(CActor* actor)
{
    if (!IsContextualDisplayEnabled())
    {
        return;
    }

    const float curHealth = actor->GetfHealth();
    const float curStamina = actor->conditions().GetPower();

    if (m_context_stamina_for_track < 0.f)
    {
        m_last_health = curHealth;
        m_context_stamina_for_track = curStamina;
    }
    else
    {
        if (std::abs(curHealth - m_last_health) > m_context_health_threshold)
        {
            TriggerHealthContext();
            m_last_health = curHealth;
        }

        if (std::abs(curStamina - m_context_stamina_for_track) > m_context_health_threshold)
        {
            TriggerHealthContext();
            m_context_stamina_for_track = curStamina;
        }
    }

    PIItem activeItem = actor->inventory().ActiveItem();
    const shared_str activeSect = activeItem ? activeItem->m_section_id : shared_str();

    if (activeSect != m_context_active_item_sect)
    {
        m_context_active_item_sect = activeSect;
        if (activeItem && activeItem->cast_weapon())
        {
            TriggerWeaponContext();
        }
    }

    CWeapon* weapon = activeItem ? activeItem->cast_weapon() : nullptr;
    if (weapon)
    {
        const u8 weaponState = weapon->GetState();
        if (weaponState != m_context_weapon_state)
        {
            switch (weaponState)
            {
            case CWeapon::eFire:
            case CWeapon::eFire2:
            case CWeapon::eReload:
            case CWeapon::eSwitch:
            case CWeapon::eSwitchMode:
            case CWeapon::eFiremodeCheck:
                TriggerWeaponContext();
                break;
            default:
                break;
            }
            m_context_weapon_state = weaponState;
        }

        II_BriefInfo briefInfo;
        weapon->GetBriefInfo(briefInfo);

        string64 ammoSignature = {};
        xr_sprintf(ammoSignature, sizeof(ammoSignature), "%s|%s|%s|%s|%s",
            briefInfo.cur_ammo.c_str(),
            briefInfo.fmj_ammo.c_str(),
            briefInfo.ap_ammo.c_str(),
            briefInfo.total_ammo.c_str(),
            briefInfo.fire_mode.c_str());

        if (xr_strcmp(ammoSignature, m_context_ammo_signature) != 0)
        {
            if (m_context_ammo_signature[0] != 0)
            {
                TriggerWeaponContext();
            }
            xr_strcpy(m_context_ammo_signature, ammoSignature);
        }

        if (briefInfo.fire_mode != m_context_fire_mode)
        {
            if (m_context_fire_mode.size())
            {
                TriggerWeaponContext();
            }
            m_context_fire_mode = briefInfo.fire_mode;
        }
    }
    else
    {
        m_context_weapon_state = 0xff;
        m_context_ammo_signature[0] = 0;
        m_context_fire_mode = shared_str();
    }

    if (actor->IsActionKeyPressedInGame(kWPN_RELOAD))
    {
        PIItem peekItem = actor->inventory().ActiveItem();
        if (peekItem && peekItem->cast_weapon())
        {
            TriggerWeaponContext();
        }
    }
}

void CUIHudStatesWnd::TickContextualDisplay()
{
    const bool isContextual = IsContextualDisplayEnabled();
    if (isContextual != m_contextual_was_enabled)
    {
        if (isContextual)
        {
            m_health_block_alpha = 0.f;
            m_weapon_block_alpha = 0.f;
            m_health_context_active = false;
            m_weapon_context_active = false;
        }
        else
        {
            m_health_block_alpha = 1.f;
            m_weapon_block_alpha = 1.f;
            m_health_context_active = false;
            m_weapon_context_active = false;
            RestoreContextualColorsFromCache();
        }
        m_contextual_was_enabled = isContextual;
    }

    if (!isContextual)
    {
        m_health_block_alpha = 1.f;
        m_weapon_block_alpha = 1.f;
        return;
    }

    const float dt = Device.fTimeDelta;
    if (dt <= 0.f)
    {
        return;
    }

    if (m_health_context_active &&
        (Device.fTimeGlobal - m_health_context_last_time) >= m_context_hide_delay)
    {
        m_health_context_active = false;
    }

    if (m_weapon_context_active &&
        (Device.fTimeGlobal - m_weapon_context_last_time) >= m_context_hide_delay)
    {
        m_weapon_context_active = false;
    }

    const float healthTarget = m_health_context_active ? 1.f : 0.f;
    const float weaponTarget = m_weapon_context_active ? 1.f : 0.f;
    const float healthSpeed = m_health_context_active ? m_context_show_speed : m_context_hide_speed;
    const float weaponSpeed = m_weapon_context_active ? m_context_show_speed : m_context_hide_speed;

    if (m_health_block_alpha < healthTarget)
    {
        m_health_block_alpha += healthSpeed * dt;
        if (m_health_block_alpha > healthTarget)
        {
            m_health_block_alpha = healthTarget;
        }
    }
    else if (m_health_block_alpha > healthTarget)
    {
        m_health_block_alpha -= healthSpeed * dt;
        if (m_health_block_alpha < healthTarget)
        {
            m_health_block_alpha = healthTarget;
        }
    }

    if (m_weapon_block_alpha < weaponTarget)
    {
        m_weapon_block_alpha += weaponSpeed * dt;
        if (m_weapon_block_alpha > weaponTarget)
        {
            m_weapon_block_alpha = weaponTarget;
        }
    }
    else if (m_weapon_block_alpha > weaponTarget)
    {
        m_weapon_block_alpha -= weaponSpeed * dt;
        if (m_weapon_block_alpha < weaponTarget)
        {
            m_weapon_block_alpha = weaponTarget;
        }
    }
}

void CUIHudStatesWnd::CaptureStaticColorCache(CUIStatic* wnd, SContextualColorCache& cache) const
{
    if (!wnd)
    {
        return;
    }

    cache.texture = wnd->GetTextureColor();
    cache.text = wnd->GetTextColor();
}

void CUIHudStatesWnd::CaptureProgressColorCache(CUIProgressBar* bar, SContextualColorCache& progress, SContextualColorCache& background) const
{
    if (!bar)
    {
        return;
    }

    progress.texture = bar->m_UIProgressItem.GetTextureColor();
    background.texture = bar->m_UIBackgroundItem.GetTextureColor();
}

void CUIHudStatesWnd::CaptureContextualBaseColors()
{
    CaptureProgressColorCache(m_ui_health_bar, m_cache_health_progress, m_cache_health_background);
    CaptureProgressColorCache(m_ui_stamina_bar, m_cache_stamina_progress, m_cache_stamina_background);
    CaptureProgressColorCache(m_ui_armor_bar, m_cache_armor_progress, m_cache_armor_background);
    CaptureStaticColorCache(m_static_health, m_cache_static_health);
    CaptureStaticColorCache(m_static_armor, m_cache_static_armor);
    CaptureStaticColorCache(m_static_weapon, m_cache_static_weapon);
    CaptureStaticColorCache(m_ui_weapon_cur_ammo, m_cache_cur_ammo);
    CaptureStaticColorCache(m_ui_weapon_fmj_ammo, m_cache_fmj_ammo);
    CaptureStaticColorCache(m_ui_weapon_ap_ammo, m_cache_ap_ammo);
    CaptureStaticColorCache(m_ui_weapon_third_ammo, m_cache_third_ammo);
    CaptureStaticColorCache(m_ui_weapon_sign_ammo, m_cache_sign_ammo);
    CaptureStaticColorCache(m_ui_adaptive_clip, m_cache_adaptive_clip);
    CaptureStaticColorCache(m_ui_adaptive_total, m_cache_adaptive_total);
    CaptureStaticColorCache(m_fire_mode, m_cache_fire_mode);
    CaptureStaticColorCache(m_ui_fire_mode_icon, m_cache_fire_mode_icon);
    CaptureStaticColorCache(m_ui_caliber_text, m_cache_caliber_text);
    CaptureStaticColorCache(m_ui_caliber_icon, m_cache_caliber_icon);
    CaptureStaticColorCache(m_ui_weapon_icon, m_cache_weapon_icon);
    CaptureStaticColorCache(m_ui_grenade, m_cache_grenade);
}

void CUIHudStatesWnd::SyncDynamicWeaponColorCaches()
{
    if (!IsContextualDisplayEnabled() || m_use_adaptive_ammo_widget)
    {
        return;
    }

    CaptureStaticColorCache(m_ui_weapon_cur_ammo, m_cache_cur_ammo);
    CaptureStaticColorCache(m_ui_weapon_fmj_ammo, m_cache_fmj_ammo);
    CaptureStaticColorCache(m_ui_weapon_ap_ammo, m_cache_ap_ammo);
    CaptureStaticColorCache(m_ui_weapon_third_ammo, m_cache_third_ammo);
    CaptureStaticColorCache(m_ui_weapon_sign_ammo, m_cache_sign_ammo);
    CaptureStaticColorCache(m_ui_grenade, m_cache_grenade);
}

void CUIHudStatesWnd::ApplyStaticFromColorCache(CUIStatic* wnd, const SContextualColorCache& cache, float blockAlpha) const
{
    if (!wnd)
    {
        return;
    }

    if (blockAlpha <= 0.001f)
    {
        wnd->Show(false);
        return;
    }

    if (!wnd->IsShown())
    {
        return;
    }

    const u32 textureAlpha = (u32)clampr(iFloor(color_get_A(cache.texture) * blockAlpha), 0, 255);
    wnd->SetTextureColor(subst_alpha(cache.texture, textureAlpha));

    const u32 textAlpha = (u32)clampr(iFloor(color_get_A(cache.text) * blockAlpha), 0, 255);
    wnd->SetTextColor(subst_alpha(cache.text, textAlpha));
}

void CUIHudStatesWnd::ApplyProgressFromColorCache(
    CUIProgressBar* bar,
    const SContextualColorCache& progress,
    const SContextualColorCache& background,
    float blockAlpha) const
{
    if (!bar)
    {
        return;
    }

    if (blockAlpha <= 0.001f)
    {
        bar->Show(false);
        return;
    }

    bar->Show(true);

    const u32 progressAlpha = (u32)clampr(iFloor(color_get_A(progress.texture) * blockAlpha), 0, 255);
    bar->m_UIProgressItem.SetTextureColor(subst_alpha(progress.texture, progressAlpha));

    if (bar->IsShownBackground())
    {
        const u32 backgroundAlpha = (u32)clampr(iFloor(color_get_A(background.texture) * blockAlpha), 0, 255);
        bar->m_UIBackgroundItem.SetTextureColor(subst_alpha(background.texture, backgroundAlpha));
    }
}

void CUIHudStatesWnd::RestoreContextualColorsFromCache()
{
    if (m_back)
    {
        m_back->SetTextureColor(m_back_base_color);
        m_back->Show(true);
    }

    if (m_ui_health_bar)
    {
        m_ui_health_bar->Show(true);
        m_ui_health_bar->m_UIProgressItem.SetTextureColor(m_cache_health_progress.texture);
        if (m_ui_health_bar->IsShownBackground())
        {
            m_ui_health_bar->m_UIBackgroundItem.SetTextureColor(m_cache_health_background.texture);
        }
    }

    if (m_ui_stamina_bar)
    {
        m_ui_stamina_bar->Show(true);
        m_ui_stamina_bar->m_UIProgressItem.SetTextureColor(m_cache_stamina_progress.texture);
        if (m_ui_stamina_bar->IsShownBackground())
        {
            m_ui_stamina_bar->m_UIBackgroundItem.SetTextureColor(m_cache_stamina_background.texture);
        }
    }

    auto restoreStatic = [](CUIStatic* wnd, const SContextualColorCache& cache)
    {
        if (!wnd)
        {
            return;
        }

        wnd->SetTextureColor(cache.texture);
        wnd->SetTextColor(cache.text);
    };

    restoreStatic(m_static_health, m_cache_static_health);
    restoreStatic(m_static_armor, m_cache_static_armor);

    if (m_ui_armor_bar)
    {
        m_ui_armor_bar->m_UIProgressItem.SetTextureColor(m_cache_armor_progress.texture);
        if (m_ui_armor_bar->IsShownBackground())
        {
            m_ui_armor_bar->m_UIBackgroundItem.SetTextureColor(m_cache_armor_background.texture);
        }
    }

    if (m_bleeding)
    {
        m_bleeding->SetTextureColor(m_bleeding_base_color);
    }

    restoreStatic(m_static_weapon, m_cache_static_weapon);
    restoreStatic(m_ui_weapon_cur_ammo, m_cache_cur_ammo);
    restoreStatic(m_ui_weapon_fmj_ammo, m_cache_fmj_ammo);
    restoreStatic(m_ui_weapon_ap_ammo, m_cache_ap_ammo);
    restoreStatic(m_ui_weapon_third_ammo, m_cache_third_ammo);
    restoreStatic(m_ui_weapon_sign_ammo, m_cache_sign_ammo);
    restoreStatic(m_ui_adaptive_clip, m_cache_adaptive_clip);
    restoreStatic(m_ui_adaptive_total, m_cache_adaptive_total);
    restoreStatic(m_ui_grenade, m_cache_grenade);
    restoreStatic(m_fire_mode, m_cache_fire_mode);
    restoreStatic(m_ui_fire_mode_icon, m_cache_fire_mode_icon);
    restoreStatic(m_ui_caliber_text, m_cache_caliber_text);
    restoreStatic(m_ui_caliber_icon, m_cache_caliber_icon);
    restoreStatic(m_ui_weapon_icon, m_cache_weapon_icon);
}

void CUIHudStatesWnd::ApplyContextualAlpha()
{
    if (!IsContextualDisplayEnabled())
    {
        return;
    }

    SyncDynamicWeaponColorCaches();

    const float backAlpha = m_health_block_alpha;
    if (m_back)
    {
        if (backAlpha <= 0.001f)
        {
            m_back->Show(false);
        }
        else
        {
            m_back->Show(true);
            const u32 channelAlpha = (u32)clampr(iFloor(backAlpha * color_get_A(m_back_base_color)), 0, 255);
            m_back->SetTextureColor(subst_alpha(m_back_base_color, channelAlpha));
        }
    }

    ApplyProgressFromColorCache(m_ui_health_bar, m_cache_health_progress, m_cache_health_background, m_health_block_alpha);
    ApplyProgressFromColorCache(m_ui_stamina_bar, m_cache_stamina_progress, m_cache_stamina_background, m_health_block_alpha);
    ApplyStaticFromColorCache(m_static_health, m_cache_static_health, m_health_block_alpha);
    ApplyStaticFromColorCache(m_static_armor, m_cache_static_armor, m_health_block_alpha);
    ApplyProgressFromColorCache(m_ui_armor_bar, m_cache_armor_progress, m_cache_armor_background, m_health_block_alpha);

    if (m_bleeding)
    {
        if (m_health_block_alpha <= 0.001f)
        {
            m_bleeding->Show(false);
        }
        else if (m_bleeding->IsShown())
        {
            const u32 channelAlpha = (u32)clampr(iFloor(m_health_block_alpha * color_get_A(m_bleeding_base_color)), 0, 255);
            m_bleeding->SetTextureColor(subst_alpha(m_bleeding_base_color, channelAlpha));
        }
    }

    ApplyStaticFromColorCache(m_static_weapon, m_cache_static_weapon, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_weapon_cur_ammo, m_cache_cur_ammo, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_weapon_fmj_ammo, m_cache_fmj_ammo, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_weapon_ap_ammo, m_cache_ap_ammo, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_weapon_third_ammo, m_cache_third_ammo, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_weapon_sign_ammo, m_cache_sign_ammo, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_adaptive_clip, m_cache_adaptive_clip, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_adaptive_total, m_cache_adaptive_total, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_grenade, m_cache_grenade, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_fire_mode, m_cache_fire_mode, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_fire_mode_icon, m_cache_fire_mode_icon, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_caliber_text, m_cache_caliber_text, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_caliber_icon, m_cache_caliber_icon, m_weapon_block_alpha);
    ApplyStaticFromColorCache(m_ui_weapon_icon, m_cache_weapon_icon, m_weapon_block_alpha);
}
