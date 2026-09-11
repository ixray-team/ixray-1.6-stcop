////////////////////////////////////////////////////////////////////////////
//	Module 		: UIActorStateInfo.cpp
//	Created 	: 15.02.2008
//	Author		: Evgeniy Sokolov
//	Description : UI actor state window class implementation
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "UIActorStateInfo.h"
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UIProgressShape.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIStackPanel.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIFontDefines.h"
#include "../../xrUI/uiabstract.h"
#include "object_broker.h"

#include "UIHelperGame.h"
#include "../../xrUI/Widgets/UIArrow.h"
#include "UIHudStatesWnd.h"

#include "../Level.h"
#include "../location_manager.h"
#include "../player_hud.h"
#include "UIMainIngameWnd.h"
#include "UIGameCustom.h"

#include "../Actor.h"
#include "../ActorCondition.h"
#include "../EntityCondition.h"
#include "../CustomOutfit.h"
#include "../ActorHelmet.h"
#include "../Inventory.h"
#include "../Artefact.h"
#include "../../xrCore/Kernel/EngineExternal.h"

namespace
{
Fvector2 ParseSize2(LPCSTR value, Fvector2 def)
{
	Fvector2 result = def;
	if (!value || !value[0])
	{
		return result;
	}

	float x = def.x;
	float y = def.y;
	const int count = sscanf(value, "%f,%f", &x, &y);
	if (count == 1)
	{
		result.set(x, x);
	}
	else if (count >= 2)
	{
		result.set(x, y);
	}
	return result;
}

u32 ParseColor4(LPCSTR value, u32 def)
{
	if (!value || !value[0])
	{
		return def;
	}

	if (!strchr(value, ','))
	{
		const auto* defs = CUIXmlInit::GetColorDefs();
		const auto it = defs->find(shared_str(value));
		if (it != defs->end())
		{
			return it->second;
		}
		return def;
	}

	int r = color_get_R(def);
	int g = color_get_G(def);
	int b = color_get_B(def);
	int a = color_get_A(def);
	const int count = sscanf(value, "%d,%d,%d,%d", &r, &g, &b, &a);
	if (count >= 3)
	{
		return color_argb(a, r, g, b);
	}
	return def;
}

CGameFont* ResolveFont(LPCSTR fontName)
{
	if (fontName && fontName[0])
	{
		if (CGameFont* font = UI().Font().GetFont(fontName))
		{
			return font;
		}
	}
	return UI().Font().GetFont(LETTERICA16_FONT_NAME);
}

ETextAlignment ParseTextAlign(LPCSTR value, ETextAlignment def)
{
	if (!value || !value[0])
	{
		return def;
	}
	if (0 == xr_strcmp(value, "left")) return CGameFont::alLeft;
	if (0 == xr_strcmp(value, "center") || 0 == xr_strcmp(value, "centre")) return CGameFont::alCenter;
	if (0 == xr_strcmp(value, "right")) return CGameFont::alRight;
	return def;
}

enum class EStateRowSlot : u8
{
	Icon,
	Caption,
	Value
};

struct StateRowSlot
{
	EStateRowSlot type = EStateRowSlot::Caption;
	float width = -1.f;
	bool autoIconWidth = false;
};

bool ParseStateRowSlotType(LPCSTR name, EStateRowSlot& out)
{
	if (!name || !name[0])
	{
		return false;
	}
	if (0 == xr_strcmp(name, "icon")) { out = EStateRowSlot::Icon; return true; }
	if (0 == xr_strcmp(name, "caption") || 0 == xr_strcmp(name, "text")) { out = EStateRowSlot::Caption; return true; }
	if (0 == xr_strcmp(name, "value") || 0 == xr_strcmp(name, "number") || 0 == xr_strcmp(name, "percent")) { out = EStateRowSlot::Value; return true; }
	return false;
}

void ParseStateRowSlots(LPCSTR columns, LPCSTR layout, float valueWidth, float iconWidth, xr_vector<StateRowSlot>& out)
{
	out.clear();
	LPCSTR source = (columns && columns[0]) ? columns : ((layout && layout[0]) ? layout : "icon,caption,value");
	const int count = _GetItemCount(source);
	for (int i = 0; i < count; ++i)
	{
		string64 token;
		_GetItem(source, i, token);
		if (!token[0])
		{
			continue;
		}

		string64 name;
		string64 sizeToken;
		name[0] = 0;
		sizeToken[0] = 0;
		if (char* colon = strchr(token, ':'))
		{
			*colon = 0;
			xr_strcpy(name, token);
			xr_strcpy(sizeToken, colon + 1);
		}
		else
		{
			xr_strcpy(name, token);
		}

		EStateRowSlot type;
		if (!ParseStateRowSlotType(name, type))
		{
			continue;
		}

		bool duplicate = false;
		for (const StateRowSlot& existing : out)
		{
			if (existing.type == type)
			{
				duplicate = true;
				break;
			}
		}
		if (duplicate)
		{
			continue;
		}

		StateRowSlot slot;
		slot.type = type;
		if (sizeToken[0])
		{
			if (0 == xr_strcmp(sizeToken, "*") || 0 == xr_strcmp(sizeToken, "flex"))
			{
				slot.width = -1.f;
			}
			else if (0 == xr_strcmp(sizeToken, "auto"))
			{
				if (type == EStateRowSlot::Icon)
				{
					slot.autoIconWidth = true;
					slot.width = iconWidth;
				}
				else if (type == EStateRowSlot::Value)
				{
					slot.width = valueWidth;
				}
				else
				{
					slot.width = -1.f;
				}
			}
			else
			{
				slot.width = float(atof(sizeToken));
			}
		}
		else
		{
			if (type == EStateRowSlot::Icon)
			{
				slot.autoIconWidth = true;
				slot.width = iconWidth;
			}
			else if (type == EStateRowSlot::Value)
			{
				slot.width = valueWidth;
			}
			else
			{
				slot.width = -1.f;
			}
		}
		out.push_back(slot);
	}

	if (out.empty())
	{
		StateRowSlot icon;
		icon.type = EStateRowSlot::Icon;
		icon.autoIconWidth = true;
		icon.width = iconWidth;
		out.push_back(icon);

		StateRowSlot caption;
		caption.type = EStateRowSlot::Caption;
		caption.width = -1.f;
		out.push_back(caption);

		StateRowSlot value;
		value.type = EStateRowSlot::Value;
		value.width = valueWidth;
		out.push_back(value);
	}
}

bool ReadOptionalFloatAttr(tinyxml2::XMLElement* element, LPCSTR name, float& out)
{
	if (!element || !name || !element->Attribute(name))
	{
		return false;
	}
	out = element->FloatAttribute(name);
	return true;
}

bool ResolveOptionalFloat(tinyxml2::XMLElement* element, LPCSTR name, bool hasDefault, float defaultValue, float& out)
{
	if (ReadOptionalFloatAttr(element, name, out))
	{
		return true;
	}
	if (hasDefault)
	{
		out = defaultValue;
		return true;
	}
	return false;
}

struct OptionalFloat
{
	bool set = false;
	float value = 0.f;
};

OptionalFloat ReadOptionalFloatFromXml(CUIXml& xml, LPCSTR path, LPCSTR attrib)
{
	OptionalFloat result;
	LPCSTR raw = xml.ReadAttrib(path, 0, attrib, nullptr);
	if (raw && raw[0])
	{
		result.set = true;
		result.value = xml.ReadAttribFlt(path, 0, attrib, 0.f);
	}
	return result;
}
}

ui_actor_state_wnd::~ui_actor_state_wnd()
{
	delete_data( m_hint_wnd );
}

void ui_actor_state_wnd::init_from_xml( CUIXml& xml, const char* path )
{
	XML_NODE* stored_root = xml.GetLocalRoot();
	CUIXmlInit::InitWindow( xml, path, 0, this );

	XML_NODE* new_root = xml.NavigateToNode( path, 0 );
	xml.SetLocalRoot( new_root );

	if (xml.NavigateToNode("hint_wnd", 0))
	{
		m_hint_wnd = UIHelper::CreateHint( xml, "hint_wnd" );
	}

	if (xml.NavigateToNode("state_list", 0))
	{
		m_listMode = true;
		init_list_from_xml(xml);
		xml.SetLocalRoot( stored_root );
		return;
	}

	m_listMode = false;
	init_legacy_from_xml(xml);
	xml.SetLocalRoot( stored_root );
}

void ui_actor_state_wnd::init_legacy_from_xml(CUIXml& xml)
{
	for ( int i = 0; i < stt_count; ++i )
	{
		m_state[i] = new ui_actor_state_item();
		m_state[i]->SetAutoDelete( true );
		AttachChild( m_state[i] );
		m_state[i]->set_hint_wnd( m_hint_wnd );
	}
	if (xml.NavigateToNode("stamina_state"))
		m_state[stt_stamina]->init_from_xml( xml, "stamina_state" );
	m_state[stt_health]->init_from_xml( xml, "health_state");
	if (xml.NavigateToNode("bleeding_state"))
		m_state[stt_bleeding]->init_from_xml( xml, "bleeding_state");
	if (xml.NavigateToNode("radiation_state"))
		m_state[stt_radiation]->init_from_xml( xml, "radiation_state");
	if (xml.NavigateToNode("armor_state"))
		m_state[stt_armor]->init_from_xml( xml, "armor_state");

	if (xml.NavigateToNode("main_sensor"))
		m_state[stt_main]->init_from_xml( xml, "main_sensor");
	m_state[stt_fire]->init_from_xml( xml, "fire_sensor");
	m_state[stt_radia]->init_from_xml( xml, "radia_sensor");
	m_state[stt_acid ]->init_from_xml( xml, "acid_sensor");
	m_state[stt_psi]->init_from_xml( xml, "psi_sensor");
	if (xml.NavigateToNode("wound_sensor"))
		m_state[stt_wound]->init_from_xml( xml, "wound_sensor");
	if (xml.NavigateToNode("fire_wound_sensor"))
		m_state[stt_fire_wound]->init_from_xml( xml, "fire_wound_sensor");
	if (xml.NavigateToNode("shock_sensor"))
		m_state[stt_shock]->init_from_xml( xml, "shock_sensor");
	if (xml.NavigateToNode("power_sensor"))
		m_state[stt_power]->init_from_xml( xml, "power_sensor");
	
	if (xml.NavigateToNode("starvation_state"))
		m_state[stt_satiety]->init_from_xml(xml, "starvation_state");
	if (xml.NavigateToNode("thirst_state"))
		m_state[stt_thirst]->init_from_xml(xml, "thirst_state");
	if (xml.NavigateToNode("sleeping_state"))
		m_state[stt_sleep]->init_from_xml(xml, "sleeping_state");

	if (xml.NavigateToNode("intoxication_state"))
		m_state[stt_intoxication]->init_from_xml(xml, "intoxication_state");
}

void ui_actor_state_wnd::init_list_from_xml(CUIXml& xml)
{
	m_stateList = UIHelper::CreateStackPanel(xml, "state_list", this, true);
	if (!m_stateList)
	{
		return;
	}

	XML_NODE* listNode = xml.NavigateToNode("state_list", 0);
	const float listWidth = m_stateList->GetWidth();

	ui_actor_state_row::LayoutDefaults defaults;
	defaults.rowHeight = xml.ReadAttribFlt("state_list", 0, "row_height", 20.f);
	defaults.iconSize = ParseSize2(xml.ReadAttrib("state_list", 0, "icon_size", "18,18"), Fvector2().set(18.f, 18.f));
	defaults.iconColor = ParseColor4(xml.ReadAttrib("state_list", 0, "icon_color", nullptr), color_argb(255, 240, 140, 40));
	defaults.captionColor = ParseColor4(xml.ReadAttrib("state_list", 0, "caption_color", nullptr), color_argb(255, 200, 205, 210));
	defaults.valueColor = ParseColor4(xml.ReadAttrib("state_list", 0, "value_color", nullptr), color_argb(255, 200, 205, 210));
	defaults.magnitude = xml.ReadAttribFlt("state_list", 0, "value_magnitude", 100.f);
	defaults.format = xml.ReadAttrib("state_list", 0, "value_format", "%d%%");
	defaults.font = xml.ReadAttrib("state_list", 0, "font", LETTERICA16_FONT_NAME);
	defaults.layout = xml.ReadAttrib("state_list", 0, "layout", "icon,caption,value");
	defaults.columns = xml.ReadAttrib("state_list", 0, "columns", nullptr);
	defaults.pad = xml.ReadAttribFlt("state_list", 0, "pad", 4.f);
	defaults.valueWidth = xml.ReadAttribFlt("state_list", 0, "value_width", 42.f);
	defaults.captionAlign = xml.ReadAttrib("state_list", 0, "caption_align", "left");
	defaults.valueAlign = xml.ReadAttrib("state_list", 0, "value_align", "right");

	const OptionalFloat listCaptionX = ReadOptionalFloatFromXml(xml, "state_list", "caption_x");
	defaults.hasCaptionX = listCaptionX.set;
	defaults.captionX = listCaptionX.value;
	const OptionalFloat listCaptionY = ReadOptionalFloatFromXml(xml, "state_list", "caption_y");
	defaults.hasCaptionY = listCaptionY.set;
	defaults.captionY = listCaptionY.value;
	const OptionalFloat listValueX = ReadOptionalFloatFromXml(xml, "state_list", "value_x");
	defaults.hasValueX = listValueX.set;
	defaults.valueX = listValueX.value;
	const OptionalFloat listValueY = ReadOptionalFloatFromXml(xml, "state_list", "value_y");
	defaults.hasValueY = listValueY.set;
	defaults.valueY = listValueY.value;
	const OptionalFloat listIconX = ReadOptionalFloatFromXml(xml, "state_list", "icon_x");
	defaults.hasIconX = listIconX.set;
	defaults.iconX = listIconX.value;
	const OptionalFloat listIconY = ReadOptionalFloatFromXml(xml, "state_list", "icon_y");
	defaults.hasIconY = listIconY.set;
	defaults.iconY = listIconY.value;

	XML_NODE* storedRoot = xml.GetLocalRoot();
	xml.SetLocalRoot(listNode);

	for (tinyxml2::XMLElement* child = listNode ? listNode->FirstChildElement() : nullptr;
		child;
		child = child->NextSiblingElement())
	{
		LPCSTR tag = child->Name();
		if (!tag)
		{
			continue;
		}

		if (0 == xr_strcmp(tag, "item"))
		{
			auto* row = new ui_actor_state_row();
			row->SetAutoDelete(true);
			m_stateList->AttachChild(row);
			row->init_from_xml(xml, child, listWidth, defaults, m_hint_wnd);
			if (row->type() != stt_invalid)
			{
				m_listRows.push_back(row);
			}
		}
		else if (0 == xr_strcmp(tag, "separator"))
		{
			auto* separator = new CUIStatic();
			separator->SetAutoDelete(true);
			m_stateList->AttachChild(separator);

			const float height = child->FloatAttribute("height", 8.f);
			separator->SetWndSize(Fvector2().set(listWidth, height));
			separator->SetWndPos(Fvector2().set(0.f, 0.f));

			LPCSTR texture = child->Attribute("texture");
			if (!texture || !texture[0])
			{
				texture = "ui_inGame2_inventory_progress_bar";
			}
			separator->InitTexture(texture);
			separator->SetStretchTexture(true);
			separator->SetTextureColor(ParseColor4(child->Attribute("color"), color_argb(180, 120, 130, 140)));
		}
	}

	xml.SetLocalRoot(storedRoot);
}

ui_actor_state_wnd::EStateType ui_actor_state_wnd::ParseStateType(LPCSTR name)
{
	if (!name || !name[0])
	{
		return stt_invalid;
	}
	if (0 == xr_strcmp(name, "stamina")) return stt_stamina;
	if (0 == xr_strcmp(name, "health")) return stt_health;
	if (0 == xr_strcmp(name, "bleeding")) return stt_bleeding;
	if (0 == xr_strcmp(name, "radiation")) return stt_radiation;
	if (0 == xr_strcmp(name, "armor")) return stt_armor;
	if (0 == xr_strcmp(name, "main")) return stt_main;
	if (0 == xr_strcmp(name, "fire")) return stt_fire;
	if (0 == xr_strcmp(name, "radia")) return stt_radia;
	if (0 == xr_strcmp(name, "acid")) return stt_acid;
	if (0 == xr_strcmp(name, "psi")) return stt_psi;
	if (0 == xr_strcmp(name, "wound")) return stt_wound;
	if (0 == xr_strcmp(name, "fire_wound")) return stt_fire_wound;
	if (0 == xr_strcmp(name, "shock")) return stt_shock;
	if (0 == xr_strcmp(name, "power")) return stt_power;
	if (0 == xr_strcmp(name, "satiety") || 0 == xr_strcmp(name, "starvation")) return stt_satiety;
	if (0 == xr_strcmp(name, "thirst")) return stt_thirst;
	if (0 == xr_strcmp(name, "sleep") || 0 == xr_strcmp(name, "sleeping")) return stt_sleep;
	if (0 == xr_strcmp(name, "intoxication")) return stt_intoxication;
	return stt_invalid;
}

void ui_actor_state_row::init_from_xml(CUIXml& xml, XML_NODE* node, float rowWidth, const LayoutDefaults& defaults, UIHint* hintWnd)
{
	tinyxml2::XMLElement* element = node ? node->ToElement() : nullptr;
	if (!element)
	{
		return;
	}

	m_type = ui_actor_state_wnd::ParseStateType(element->Attribute("type"));
	const float rowHeight = element->FloatAttribute("row_height", defaults.rowHeight);
	SetWndSize(Fvector2().set(rowWidth, rowHeight));
	SetWndPos(Fvector2().set(0.f, 0.f));

	if (hintWnd)
	{
		set_hint_wnd(hintWnd);
	}

	LPCSTR hint = element->Attribute("hint_text");
	if (!hint || !hint[0])
	{
		hint = element->Attribute("caption");
	}
	if (hint && hint[0])
	{
		set_hint_text_ST(hint);
	}
	set_hint_delay(u32(element->IntAttribute("hint_delay", 800)));

	const Fvector2 iconSize = ParseSize2(element->Attribute("icon_size"), defaults.iconSize);
	const u32 iconColor = ParseColor4(element->Attribute("icon_color"), defaults.iconColor);
	const u32 captionColor = ParseColor4(element->Attribute("caption_color"), defaults.captionColor);
	const u32 valueColor = ParseColor4(element->Attribute("value_color"), defaults.valueColor);
	m_magnitude = element->FloatAttribute("value_magnitude", defaults.magnitude);
	LPCSTR format = element->Attribute("value_format");
	m_format = (format && format[0]) ? format : defaults.format;
	LPCSTR fontName = element->Attribute("font");
	CGameFont* font = ResolveFont((fontName && fontName[0]) ? fontName : defaults.font);

	const float pad = element->Attribute("pad") ? element->FloatAttribute("pad") : defaults.pad;
	const float valueWidth = element->Attribute("value_width") ? element->FloatAttribute("value_width") : defaults.valueWidth;
	LPCSTR layout = element->Attribute("layout");
	if (!layout || !layout[0])
	{
		layout = defaults.layout;
	}
	LPCSTR columns = element->Attribute("columns");
	if (!columns || !columns[0])
	{
		columns = defaults.columns;
	}
	const ETextAlignment captionAlign = ParseTextAlign(
		element->Attribute("caption_align") ? element->Attribute("caption_align") : defaults.captionAlign,
		CGameFont::alLeft);
	const ETextAlignment valueAlign = ParseTextAlign(
		element->Attribute("value_align") ? element->Attribute("value_align") : defaults.valueAlign,
		CGameFont::alRight);

	xr_vector<StateRowSlot> slots;
	ParseStateRowSlots(columns, layout, valueWidth, iconSize.x, slots);

	float fixedSum = 0.f;
	int flexCount = 0;
	for (StateRowSlot& slot : slots)
	{
		if (slot.type == EStateRowSlot::Icon && slot.autoIconWidth)
		{
			slot.width = iconSize.x;
		}
		if (slot.width < 0.f)
		{
			++flexCount;
		}
		else
		{
			fixedSum += slot.width;
		}
	}
	if (slots.size() > 1)
	{
		fixedSum += pad * float(slots.size() - 1);
	}
	const float flexWidth = (flexCount > 0) ? std::max(0.f, rowWidth - fixedSum) / float(flexCount) : 0.f;

	m_icon = new CUIStatic();
	m_icon->SetAutoDelete(true);
	AttachChild(m_icon);
	m_icon->Show(false);

	m_caption = new CUIStatic();
	m_caption->SetAutoDelete(true);
	AttachChild(m_caption);
	m_caption->Show(false);
	m_caption->TextItemControl()->SetFont(font);
	m_caption->SetTextColor(captionColor);
	m_caption->SetTextAlignment(captionAlign);
	m_caption->SetVTextAlignment(valCenter);
	LPCSTR caption = element->Attribute("caption");
	if (caption && caption[0])
	{
		m_caption->SetTextST(caption);
	}

	m_value = new CUIStatic();
	m_value->SetAutoDelete(true);
	AttachChild(m_value);
	m_value->Show(false);
	m_value->TextItemControl()->SetFont(font);
	m_value->SetTextColor(valueColor);
	m_value->SetTextAlignment(valueAlign);
	m_value->SetVTextAlignment(valCenter);
	m_value->TextItemControl()->SetText("0%");

	float cursorX = 0.f;
	for (const StateRowSlot& slot : slots)
	{
		const float width = (slot.width < 0.f) ? flexWidth : slot.width;
		float posX = cursorX;
		float posY = 0.f;
		float sizeX = width;
		float sizeY = rowHeight;

		if (slot.type == EStateRowSlot::Icon)
		{
			float absX = 0.f;
			float absY = 0.f;
			const bool hasAbsX = ResolveOptionalFloat(element, "icon_x", defaults.hasIconX, defaults.iconX, absX);
			if (hasAbsX)
			{
				posX = absX;
			}
			if (ResolveOptionalFloat(element, "icon_y", defaults.hasIconY, defaults.iconY, absY))
			{
				posY = absY;
			}
			else
			{
				posY = (rowHeight - iconSize.y) * 0.5f;
			}
			sizeX = iconSize.x;
			sizeY = iconSize.y;
			if (hasAbsX && element->Attribute("icon_width"))
			{
				sizeX = element->FloatAttribute("icon_width");
			}
			if (element->Attribute("icon_height"))
			{
				sizeY = element->FloatAttribute("icon_height");
			}

			m_icon->Show(true);
			m_icon->SetWndPos(Fvector2().set(posX, posY));
			m_icon->SetWndSize(Fvector2().set(sizeX, sizeY));
			LPCSTR iconTex = element->Attribute("icon");
			if (iconTex && iconTex[0])
			{
				m_icon->InitTexture(iconTex);
				m_icon->SetStretchTexture(true);
				m_icon->SetTextureColor(iconColor);
			}
		}
		else if (slot.type == EStateRowSlot::Caption)
		{
			float absX = 0.f;
			float absY = 0.f;
			const bool hasAbsX = ResolveOptionalFloat(element, "caption_x", defaults.hasCaptionX, defaults.captionX, absX);
			if (hasAbsX)
			{
				posX = absX;
			}
			if (ResolveOptionalFloat(element, "caption_y", defaults.hasCaptionY, defaults.captionY, absY))
			{
				posY = absY;
			}
			if (hasAbsX && element->Attribute("caption_width"))
			{
				sizeX = element->FloatAttribute("caption_width");
			}
			if (element->Attribute("caption_height"))
			{
				sizeY = element->FloatAttribute("caption_height");
			}

			m_caption->Show(true);
			m_caption->SetWndPos(Fvector2().set(posX, posY));
			m_caption->SetWndSize(Fvector2().set(sizeX, sizeY));
		}
		else
		{
			float absX = 0.f;
			float absY = 0.f;
			const bool hasAbsX = ResolveOptionalFloat(element, "value_x", defaults.hasValueX, defaults.valueX, absX);
			if (hasAbsX)
			{
				posX = absX;
			}
			if (ResolveOptionalFloat(element, "value_y", defaults.hasValueY, defaults.valueY, absY))
			{
				posY = absY;
			}
			if (hasAbsX && element->Attribute("value_width"))
			{
				sizeX = element->FloatAttribute("value_width");
			}
			if (element->Attribute("value_height"))
			{
				sizeY = element->FloatAttribute("value_height");
			}

			m_value->Show(true);
			m_value->SetWndPos(Fvector2().set(posX, posY));
			m_value->SetWndSize(Fvector2().set(sizeX, sizeY));
		}

		cursorX += width + pad;
	}
}

void ui_actor_state_row::set_value(float normalized)
{
	if (!m_value)
	{
		return;
	}

	float clamped = clampr(normalized, 0.f, 1.f);
	int v = iFloor(clamped * m_magnitude + 0.5f);
	clamp(v, 0, iFloor(m_magnitude + 0.5f));

	string64 text;
	xr_sprintf(text, sizeof(text), m_format.c_str() ? m_format.c_str() : "%d%%", v);
	m_value->TextItemControl()->SetText(text);
}

void ui_actor_state_wnd::SetListValue(EStateType type, float normalized)
{
	for (ui_actor_state_row* row : m_listRows)
	{
		if (row && row->type() == type)
		{
			row->set_value(normalized);
		}
	}
}

void ui_actor_state_wnd::UpdateActorInfo(CInventoryOwner* owner)
{
	CActor* actor = owner->cast_actor();
	if (actor == nullptr)
	{
		return;
	}

	if (m_listMode)
	{
		UpdateActorInfoList(actor);
		return;
	}

	UpdateActorInfoLegacy(actor);
}

void ui_actor_state_wnd::UpdateActorInfoList(CActor* actor)
{
	SetListValue(stt_health, actor->conditions().GetHealth());
	SetListValue(stt_thirst, actor->conditions().GetThirst());
	SetListValue(stt_satiety, actor->conditions().GetSatiety());
	SetListValue(stt_sleep, actor->conditions().GetSleepiness());
	SetListValue(stt_radiation, actor->conditions().GetRadiation());

	const static bool enableMedIntoxication = EngineExternal()[EEngineExternalGame::EnableMedIntoxication];
	SetListValue(stt_intoxication, enableMedIntoxication ? actor->conditions().GetIntoxication() : 0.f);

	float stamina = actor->GetRestoreSpeed(ALife::ePowerRestoreSpeed);
	SetListValue(stt_stamina, clampr(stamina, 0.f, 1.f));

	CCustomOutfit* outfit = actor->GetOutfit();
	CHelmet* helmet = actor->GetHelmet();

	float fwou_value = 0.0f;
	float burn_value = 0.0f;
	float radi_value = 0.0f;
	float cmbn_value = 0.0f;
	float tele_value = 0.0f;
	float woun_value = 0.0f;
	float shoc_value = 0.0f;

	const auto& cur_booster_influences = actor->conditions().GetCurBoosterInfluences();
	CEntityCondition::BOOSTER_MAP::const_iterator it;
	it = cur_booster_influences.find(eBoostRadiationProtection);
	if (it != cur_booster_influences.end())
		radi_value += it->second.fBoostValue;

	it = cur_booster_influences.find(eBoostChemicalBurnProtection);
	if (it != cur_booster_influences.end())
		cmbn_value += it->second.fBoostValue;

	it = cur_booster_influences.find(eBoostTelepaticProtection);
	if (it != cur_booster_influences.end())
		tele_value += it->second.fBoostValue;

	if (outfit)
	{
		burn_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeBurn);
		radi_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeRadiation);
		cmbn_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeChemicalBurn);
		tele_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeTelepatic);
		woun_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeWound);
		shoc_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeShock);

		IKinematics* ikv = PKinematics(actor->Visual());
		VERIFY(ikv);
		u16 spine_bone = ikv->LL_BoneID("bip01_spine");
		float boneArmor = outfit->GetBoneArmor(spine_bone);
		SetListValue(stt_armor, clampr(boneArmor, 0.f, 1.f));

		fwou_value += boneArmor * outfit->GetCondition();
		if (!outfit->bIsHelmetAvaliable)
		{
			u16 head_bone = ikv->LL_BoneID("bip01_head");
			fwou_value += outfit->GetBoneArmor(head_bone) * outfit->GetCondition();
		}
	}
	else
	{
		SetListValue(stt_armor, 0.f);
	}

	if (helmet)
	{
		burn_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeBurn);
		radi_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeRadiation);
		cmbn_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeChemicalBurn);
		tele_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeTelepatic);
		woun_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeWound);
		shoc_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeShock);

		IKinematics* ikv = PKinematics(actor->Visual());
		VERIFY(ikv);
		u16 spine_bone = ikv->LL_BoneID("bip01_head");
		fwou_value += helmet->GetBoneArmor(spine_bone) * helmet->GetCondition();
	}

	const auto getProtection = [&](float& valueRef, ALife::EHitType hitType) -> float
	{
		valueRef += actor->GetProtection_ArtefactsOnBelt(hitType);
		return actor->conditions().GetZoneMaxPower(hitType);
	};

	auto setProtection = [&](EStateType type, float value, ALife::EHitType hitType)
	{
		const float max_power = getProtection(value, hitType);
		const float norm = (max_power > EPS) ? clampr(value / max_power, 0.f, 1.f) : 0.f;
		SetListValue(type, norm);
	};

	setProtection(stt_fire, burn_value, ALife::eHitTypeBurn);
	setProtection(stt_radia, radi_value, ALife::eHitTypeRadiation);
	setProtection(stt_acid, cmbn_value, ALife::eHitTypeChemicalBurn);
	setProtection(stt_psi, tele_value, ALife::eHitTypeTelepatic);
	setProtection(stt_wound, woun_value, ALife::eHitTypeWound);
	setProtection(stt_shock, shoc_value, ALife::eHitTypeShock);
	setProtection(stt_fire_wound, fwou_value, ALife::eHitTypeFireWound);

	const float maxPowerRestore = actor->conditions().GetMaxPowerRestoreSpeed();
	float powerNorm = 0.f;
	if (maxPowerRestore > EPS)
	{
		powerNorm = clampr(actor->GetRestoreSpeed(ALife::ePowerRestoreSpeed) / maxPowerRestore, 0.f, 1.f);
	}
	SetListValue(stt_power, powerNorm);
}

void ui_actor_state_wnd::UpdateActorInfoLegacy(CActor* actor)
{
	float value = 0.0f;

	if (!m_state[stt_health]->m_progress->IsExpressionSystem)
	{
		value = actor->conditions().GetHealth();
		value = floor(value * 55) / 55;
		m_state[stt_health]->set_progress(value);
	}

	if (m_state[stt_thirst]->m_progress != nullptr && !m_state[stt_thirst]->m_progress->IsExpressionSystem)
	{
		value = actor->conditions().GetThirst();
		m_state[stt_thirst]->set_progress(value);
	}
	
	if (m_state[stt_satiety]->m_progress != nullptr && !m_state[stt_satiety]->m_progress->IsExpressionSystem)
	{
		const static bool enableMedIntoxication = EngineExternal()[EEngineExternalGame::EnableMedIntoxication];
		value = enableMedIntoxication
			? actor->conditions().GetIntoxication()
			: actor->conditions().GetSatiety();
		m_state[stt_satiety]->set_progress(value);
	}
	
	if (m_state[stt_sleep]->m_progress != nullptr && !m_state[stt_sleep]->m_progress->IsExpressionSystem)
	{
		value = actor->conditions().GetSleepiness();
		m_state[stt_sleep]->set_progress(value);
	}

	value = actor->GetRestoreSpeed(ALife::ePowerRestoreSpeed);
	m_state[stt_stamina]->set_text(value);

	value = actor->conditions().BleedingSpeed();
	m_state[stt_health]->show_static((value > 0.01f));
	
	m_state[stt_bleeding]->show_static(false, 1);
	m_state[stt_bleeding]->show_static(false, 2);
	m_state[stt_bleeding]->show_static(false, 3);
	if(!fis_zero(value, EPS))
	{
		if(value<0.35f)
			m_state[stt_bleeding]->show_static(true, 1);
		else if(value<0.7f)
			m_state[stt_bleeding]->show_static(true, 2);
		else 
			m_state[stt_bleeding]->show_static(true, 3);
	}

	value = actor->conditions().GetRadiation();
	m_state[stt_radiation]->show_static(false, 1);
	m_state[stt_radiation]->show_static(false, 2);
	m_state[stt_radiation]->show_static(false, 3);
	if(!fis_zero(value, EPS))
	{
		if(value<0.35f)
			m_state[stt_radiation]->show_static(true, 1);
		else if(value<0.7f)
			m_state[stt_radiation]->show_static(true, 2);
		else 
			m_state[stt_radiation]->show_static(true, 3);
	}
	m_state[stt_main]->set_progress_shape(value);

	const static bool enableMedIntoxication = EngineExternal()[EEngineExternalGame::EnableMedIntoxication];
	if (enableMedIntoxication)
	{
		if (m_state[stt_intoxication]->m_progress != nullptr && !m_state[stt_intoxication]->m_progress->IsExpressionSystem)
		{
			value = actor->conditions().GetIntoxication();
			m_state[stt_intoxication]->set_progress(value);
		}

		value = actor->conditions().GetIntoxication();
		m_state[stt_intoxication]->show_static(false, 1);
		m_state[stt_intoxication]->show_static(false, 2);
		m_state[stt_intoxication]->show_static(false, 3);
		if (!fis_zero(value, EPS))
		{
			if (value < 0.35f)
				m_state[stt_intoxication]->show_static(true, 1);
			else if (value < 0.7f)
				m_state[stt_intoxication]->show_static(true, 2);
			else
				m_state[stt_intoxication]->show_static(true, 3);
		}
	}

	CCustomOutfit* outfit = actor->GetOutfit();
	CHelmet* helmet = actor->GetHelmet();

	m_state[stt_fire_wound]->set_progress(0.0f);
	m_state[stt_fire]->set_progress(0.0f);
	m_state[stt_radia]->set_progress(0.0f);
	m_state[stt_acid]->set_progress(0.0f);
	m_state[stt_psi]->set_progress(0.0f);
	m_state[stt_wound]->set_progress(0.0f);
	m_state[stt_shock]->set_progress(0.0f);
	m_state[stt_power]->set_progress(0.0f);

	float fwou_value = 0.0f;
	float burn_value = 0.0f;
	float radi_value = 0.0f;
	float cmbn_value = 0.0f;
	float tele_value = 0.0f;
	float woun_value = 0.0f;
	float shoc_value = 0.0f;

	const auto& cur_booster_influences = actor->conditions().GetCurBoosterInfluences();
	CEntityCondition::BOOSTER_MAP::const_iterator it;
	it = cur_booster_influences.find(eBoostRadiationProtection);
	if (it != cur_booster_influences.end())
		radi_value += it->second.fBoostValue;

	it = cur_booster_influences.find(eBoostChemicalBurnProtection);
	if (it != cur_booster_influences.end())
		cmbn_value += it->second.fBoostValue;

	it = cur_booster_influences.find(eBoostTelepaticProtection);
	if (it != cur_booster_influences.end())
		tele_value += it->second.fBoostValue;

	if(outfit)
	{
		burn_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeBurn);
		radi_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeRadiation);
		cmbn_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeChemicalBurn);
		tele_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeTelepatic);
		woun_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeWound);
		shoc_value += outfit->GetDefHitTypeProtection(ALife::eHitTypeShock);

		IKinematics* ikv = PKinematics(actor->Visual());
		VERIFY(ikv);
		u16 spine_bone = ikv->LL_BoneID("bip01_spine");

		value = outfit->GetBoneArmor(spine_bone);
		m_state[stt_armor]->set_text(value);

		fwou_value += value * outfit->GetCondition();
		if(!outfit->bIsHelmetAvaliable)
		{
			u16 spine_bone_ = ikv->LL_BoneID("bip01_head");
			fwou_value += outfit->GetBoneArmor(spine_bone_)*outfit->GetCondition();
		}
	}
	else
	{
		m_state[stt_armor]->set_text(0.0f);
	}

	if(helmet)
	{
		burn_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeBurn);
		radi_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeRadiation);
		cmbn_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeChemicalBurn);
		tele_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeTelepatic);
		woun_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeWound);
		shoc_value += helmet->GetDefHitTypeProtection(ALife::eHitTypeShock);

		IKinematics* ikv = PKinematics(actor->Visual());
		VERIFY(ikv);
		u16 spine_bone = ikv->LL_BoneID("bip01_head");
		fwou_value += helmet->GetBoneArmor(spine_bone)*helmet->GetCondition();
	}
	const auto getProtection = [&](float& valueRef, ALife::EHitType hitType) -> float
		{
			valueRef += actor->GetProtection_ArtefactsOnBelt(hitType);
			return actor->conditions().GetZoneMaxPower(hitType);
		};

	{
		const float max_power = getProtection(burn_value, ALife::eHitTypeBurn);
		update_round_states(stt_fire, burn_value, max_power);
	}
	{
		const float max_power = getProtection(radi_value, ALife::eHitTypeRadiation);
		update_round_states(stt_radia, radi_value, max_power);
	}
	{
		const float max_power = getProtection(cmbn_value, ALife::eHitTypeChemicalBurn);
		update_round_states(stt_acid, cmbn_value, max_power);
	}
	{
		const float max_power = getProtection(tele_value, ALife::eHitTypeTelepatic);
		update_round_states(stt_psi, tele_value, max_power);
	}
	{
		const float max_power = getProtection(woun_value, ALife::eHitTypeWound);
		update_round_states(stt_wound, woun_value, max_power);
	}
	{
		const float max_power = getProtection(shoc_value, ALife::eHitTypeShock);
		update_round_states(stt_shock, shoc_value, max_power);
	}
	{
		const float max_power = getProtection(fwou_value, ALife::eHitTypeFireWound);
		update_round_states(stt_fire_wound, fwou_value, max_power);
	}
	{
		if (m_state[stt_power]->m_progress && !m_state[stt_power]->m_progress->IsExpressionSystem)
		{
			value = actor->GetRestoreSpeed(ALife::ePowerRestoreSpeed) / actor->conditions().GetMaxPowerRestoreSpeed();
			value = floor(value * 31) / 31;

			m_state[stt_power]->set_progress(value);
		}
	}

	UpdateHitZone();
}

void ui_actor_state_wnd::update_round_states(EStateType stt_type, float initial, float max_power)
{
	auto state = m_state[stt_type];

	const float progress = floor(initial / max_power * 31) / 31;
	const float arrow = initial / max_power;
	
	if (!state->set_progress(progress) && stt_type != stt_main)
	{
		state->set_arrow(arrow);
		state->set_text(arrow);
	}
}

void ui_actor_state_wnd::UpdateHitZone()
{
	if (m_listMode)
	{
		return;
	}

	CUIHudStatesWnd* wnd = CurrentGameUI()->UIMainIngameWnd->get_hud_states();
	VERIFY( wnd );
	if ( !wnd )
	{
		return;
	}
	wnd->UpdateZones();

	if (m_state[stt_main])
	{
		CActor* actor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
		float detectRadZonePower = std::max(actor->conditions().m_fRadiationZonePower, wnd->m_radia_hit * 10);
		m_state[stt_main]->set_arrow(detectRadZonePower);
	}
}

void ui_actor_state_wnd::Draw()
{
	inherited::Draw();
	if (m_hint_wnd)
	{
		m_hint_wnd->Draw();
	}
}

void ui_actor_state_wnd::Show( bool status )
{
	inherited::Show( status );
	ShowChildren( status );
}

ui_actor_state_item::ui_actor_state_item()
{
	m_static		= nullptr;
	m_static2		= nullptr;
	m_static3		= nullptr;
	m_progress		= nullptr;
	m_sensor		= nullptr;
	m_arrow			= nullptr;
	m_arrow_shadow	= nullptr;
	m_magnitude		= 1.0f;
}

ui_actor_state_item::~ui_actor_state_item()
{
}

void ui_actor_state_item::init_from_xml( CUIXml& xml, const char* path )
{
	CUIXmlInit::InitWindow( xml, path, 0, this);

	XML_NODE* stored_root = xml.GetLocalRoot();
	XML_NODE* new_root = xml.NavigateToNode( path, 0 );
	xml.SetLocalRoot( new_root );

	const char* hint_text = xml.Read( "hint_text", 0, "no hint" );
	set_hint_text_ST( hint_text );
	
	set_hint_delay( (u32)xml.ReadAttribInt( "hint_text", 0, "delay" ) );

	if ( xml.NavigateToNode( "state_progress", 0 ) )	
	{
		m_progress = UIHelper::CreateProgressBar( xml, "state_progress", this );
		m_progress->IsExpressionSystem =
			xml.ReadAttrib(path, 0, "expression", nullptr) != nullptr
			|| xml.ReadAttrib("state_progress", 0, "expression", nullptr) != nullptr;
	}
	if ( xml.NavigateToNode( "progress_shape", 0 ) )	
	{
		m_sensor = new CUIProgressShape();
		AttachChild( m_sensor );
		m_sensor->SetAutoDelete( true );
		CUIXmlInit::InitProgressShape( xml, "progress_shape", 0, m_sensor );
	}
	if ( xml.NavigateToNode( "arrow", 0 ) )	
	{
		m_arrow = new CUIArrow();
		m_arrow->init_from_xml( xml, "arrow", this );
	}
	if ( xml.NavigateToNode( "arrow_shadow", 0 ) )	
	{
		m_arrow_shadow = new CUIArrow();
		m_arrow_shadow->init_from_xml( xml, "arrow_shadow", this );
	}
	if ( xml.NavigateToNode( "icon", 0 ) )	
	{
		m_static = UIHelper::CreateStatic( xml, "icon", this );
		m_magnitude = xml.ReadAttribFlt( "icon", 0, "magnitude", 1.0f );
		m_static->TextItemControl()->SetText("");
	}
	if ( xml.NavigateToNode( "icon2", 0 ) )	
	{
		m_static2 = UIHelper::CreateStatic( xml, "icon2", this );
		m_magnitude = xml.ReadAttribFlt("icon2", 0, "magnitude", 1.0f);
		m_static2->TextItemControl()->SetText("");
	}
	if ( xml.NavigateToNode( "icon3", 0 ) )	
	{
		m_static3 = UIHelper::CreateStatic( xml, "icon3", this );
		m_magnitude = xml.ReadAttribFlt("icon3", 0, "magnitude", 1.0f);
		m_static3->TextItemControl()->SetText("");
	}
	set_arrow( 0.0f );
	xml.SetLocalRoot( stored_root );
}


bool ui_actor_state_item::set_text( float value )
{
	if (!m_static)
	{
		return false;
	}

	int v = (int)( value * m_magnitude + 0.49f );
	clamp( v, 0, 99 );
	string32 text_res;
	xr_sprintf( text_res, sizeof(text_res), "%d", v );
	m_static->TextItemControl()->SetText( text_res );
	return true;
}

bool ui_actor_state_item::set_progress( float value )
{
	if ( !m_progress )
	{
		return false;
	}
	m_progress->SetProgressPos( value );
	return true;
}

bool ui_actor_state_item::set_progress_shape( float value )
{
	if ( !m_sensor )
	{
		return false;
	}
	m_sensor->SetPos( value );
	return true;
}

int ui_actor_state_item::set_arrow( float value )
{
	if ( !m_arrow )
	{
		return 0;	
	}
	m_arrow->SetNewValue( value );
	if ( !m_arrow_shadow )
	{
		return 1;
	}
	m_arrow_shadow->SetPos( m_arrow->GetPos() );
	return 2;
}


bool ui_actor_state_item::show_static( bool status, u8 number )
{
	switch(number)
	{
	case 1:
		if(!m_static)
			return false;
		m_static->Show(status);
		break;
	case 2:
		if(!m_static2)
			return false;
		m_static2->Show(status);
		break;
	case 3:
		if(!m_static3)
			return false;
		m_static3->Show(status);
		break;
	default:
		return false;
	}
	return true;
}
