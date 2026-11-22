#include "StdAfx.h"
#include "UIKeyBinding.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/Widgets/UIEditKeyBind.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/xr_level_controller.h"
#include "../../xrEngine/string_table.h"

CUIKeyBinding::CUIKeyBinding()
	: m_scroll_wnd(nullptr)
	, m_quickSlotsItem(nullptr)
	, m_quickSlotsKey1(nullptr)
	, m_quickSlotsKey2(nullptr)
{
	for (int i=0; i<3; i++)
		AttachChild		(&m_header[i]);

	AttachChild			(&m_frame);
}

void CUIKeyBinding::InitFromXml(CUIXml& xml_doc, const char* path)
{
	CUIXmlInit::InitWindow		(xml_doc, path, 0, this);
	string256					buf;
	m_scroll_wnd				= new CUIScrollView(); m_scroll_wnd->SetAutoDelete(true); AttachChild(m_scroll_wnd);
	CUIXmlInit::InitScrollView	(xml_doc, xr_strconcat(buf,path,":scroll_view"),0, m_scroll_wnd);

	CUIXmlInit::InitFrameWindow	(xml_doc, xr_strconcat(buf,path,":frame"),		0, &m_frame);
	CUIXmlInit::InitFrameLine		(xml_doc, xr_strconcat(buf,path,":header_1"),	0, &m_header[0]);
	CUIXmlInit::InitFrameLine		(xml_doc, xr_strconcat(buf,path,":header_2"),	0, &m_header[1]);
	CUIXmlInit::InitFrameLine		(xml_doc, xr_strconcat(buf,path,":header_3"),	0, &m_header[2]);

	FillUpList					(xml_doc, path);
}

void CUIKeyBinding::FillUpList(CUIXml& xml_doc_ui, const char* path_ui)
{
	string256		buf;
	CUIXml			xml_doc;
	xml_doc.Load							(CONFIG_PATH, UI_PATH, "ui_keybinding.xml");

	xr_set<shared_str> listedActions;

	const auto addGroup = [&](const char* groupName)
	{
		CUIStatic* pItem					= new CUIStatic();
		CUIXmlInit::InitStatic				(xml_doc_ui, xr_strconcat(buf,path_ui,":scroll_view:item_group"),	0, pItem);
		pItem->TextItemControl()->SetTextST		(groupName);
		m_scroll_wnd->AddWindow				(pItem, true);
	};

	const auto addCommand = [&](const char* captionId, const shared_str& exe)
	{
		CUIStatic* pItem				= new CUIStatic();
		CUIXmlInit::InitStatic			(xml_doc_ui, xr_strconcat(buf,path_ui,":scroll_view:item_key"),	0, pItem);
		const char* layerLabel			= Hotkeys::LayersEnabled() ? Hotkeys::ContextLabel(Hotkeys::Context(Hotkeys::Find(exe.c_str()))) : "";
		if (layerLabel && layerLabel[0])
		{
			xr_string caption			= g_pStringTable->translate(captionId).c_str();
			caption						+= "  (";
			caption						+= g_pStringTable->translate(layerLabel).c_str();
			caption						+= ")";
			pItem->TextItemControl()->SetText(caption.c_str());
		}
		else
		{
			pItem->TextItemControl()->SetTextST(captionId);
		}
		m_scroll_wnd->AddWindow			(pItem, true);
		listedActions.insert			(exe);

		float item_width				= m_header[1].GetWidth()-3.0f;
		float item_pos					= m_header[1].GetWndPos().x;
		CUIEditKeyBind* pEditKB1		= new CUIEditKeyBind(true);pEditKB1->SetAutoDelete(true);
		pEditKB1->InitKeyBind			(Fvector2().set(item_pos, 0.0f), Fvector2().set(item_width, pItem->GetWndSize().y) );
		pEditKB1->AssignProps			(exe,"key_binding");
		pItem->AttachChild				(pEditKB1);

		item_width						= m_header[2].GetWidth()-3.0f;
		item_pos						= m_header[2].GetWndPos().x;
		CUIEditKeyBind* pEditKB2		= new CUIEditKeyBind(false);pEditKB2->SetAutoDelete(true);
		pEditKB2->InitKeyBind			(Fvector2().set(item_pos, 0.0f), Fvector2().set(item_width, pItem->GetWndSize().y) );
		pEditKB2->AssignProps			(exe,"key_binding");
		pItem->AttachChild				(pEditKB2);

		if (0 == xr_strcmp(*exe, "show_quick_slots"))
		{
			m_quickSlotsItem = pItem;
			m_quickSlotsKey1 = pEditKB1;
			m_quickSlotsKey2 = pEditKB2;

			const bool isHideQuickSlotsEnabled = HUD_IsQuickSlotsAutoHide();
			pItem->Show(true);
			pEditKB1->Show(true);
			pEditKB2->Show(true);
			pItem->Enable(isHideQuickSlotsEnabled);
			pEditKB1->Enable(isHideQuickSlotsEnabled);
			pEditKB2->Enable(isHideQuickSlotsEnabled);
		}
	};

	int groupsCount = xml_doc.GetNodesNum	("",0,"group");

	for (int i = 0; i<groupsCount; ++i)
	{
		shared_str grp_name					= xml_doc.ReadAttrib("group",i,"name");
		R_ASSERT							(xr_strlen(grp_name));
		addGroup							(grp_name.c_str());

		int commandsCount					= xml_doc.GetNodesNum("group",i,"command");
		XML_NODE* tab_node					= xml_doc.NavigateToNode("group",i);
		xml_doc.SetLocalRoot				(tab_node);

		for (int j = 0; j<commandsCount; ++j)
		{
			shared_str command_id			= xml_doc.ReadAttrib("command",j,"id");
			shared_str exe					= xml_doc.ReadAttrib("command",j,"exe");

#ifdef DEBUG
			if ( kNOTBINDED == action_name_to_id(*exe) )
			{
				Msg("action [%s] not exist. update data",exe.c_str());
				continue;
			}
#endif
			addCommand						(command_id.c_str(), exe);
		}
		xml_doc.SetLocalRoot				(xml_doc.GetRoot());
	}

	xr_vector<EGameActions> optionActions;
	Hotkeys::OptionsActions					(optionActions);
	shared_str currentGroup;
	for (EGameActions actionId : optionActions)
	{
		const shared_str exe				= id_to_action_name(actionId);
		if (!exe.size() || listedActions.contains(exe))
		{
			continue;
		}
		const SHotkeyInfo* info				= Hotkeys::Info(actionId);
		if (info->OptionsGroup != currentGroup)
		{
			currentGroup					= info->OptionsGroup;
			addGroup						(currentGroup.c_str());
		}
		addCommand							(info->Label.size() ? info->Label.c_str() : exe.c_str(), exe);
	}

	if (Hotkeys::LayersEnabled())
	{
		struct SLayerCommand
		{
			shared_str group;
			shared_str exe;
			shared_str caption;
			int hint;
		};
		xr_vector<SLayerCommand> layerCommands;
		for (int id = kFIRST_CUSTOM_ACTION; id < kNOTBINDED && actions[id].action_name; ++id)
		{
			const shared_str exe			= actions[id].action_name;
			const SHotkeyInfo* info			= Hotkeys::Info(EGameActions(id));
			if (listedActions.contains(exe) || (info && info->OptionsGroup.size()))
			{
				continue;
			}
			const char* group				= Hotkeys::ContextLabel(Hotkeys::Context(EGameActions(id)));
			if (!group || !group[0])
			{
				continue;
			}
			layerCommands.push_back({group, exe, info && info->Label.size() ? info->Label : exe, info ? info->Hint : 0});
		}
		std::stable_sort(layerCommands.begin(), layerCommands.end(), [](const SLayerCommand& left, const SLayerCommand& right)
		{
			const int group = xr_strcmp(left.group, right.group);
			return group != 0 ? group < 0 : left.hint < right.hint;
		});
		shared_str layerGroup;
		for (const SLayerCommand& command : layerCommands)
		{
			if (command.group != layerGroup)
			{
				layerGroup					= command.group;
				addGroup					(layerGroup.c_str());
			}
			addCommand						(command.caption.c_str(), command.exe);
		}
	}
#ifdef DEBUG
    CheckStructure							(xml_doc);
#endif
}

#ifdef DEBUG
void CUIKeyBinding::CheckStructure(CUIXml& xml_doc)
{
	bool first = true;
	CUIStatic*	pItem = nullptr;
	
	for (int i=0; true; i++)
	{
		const char* action_name = actions[i].action_name;
		if (action_name)
		{
			if (IsActionExist(action_name, xml_doc))
				continue;
			else
			{

				if (first)
				{
					pItem					= new CUIStatic();
					pItem->SetWndPos		(Fvector2().set(0,0));
					pItem->SetWndSize		(Fvector2().set(m_scroll_wnd->GetWndSize().x,20.0f));
					pItem->SetText			("NEXT ITEMS NOT DESCRIBED IN COMMAND DESC LIST");
					first					= false;
					pItem->SetAutoDelete	(true);
					m_scroll_wnd->AddWindow	(pItem, true);
				}
				
				pItem						= new CUIStatic();
				pItem->SetWndPos			(Fvector2().set(0,0));
				pItem->SetWndSize			(Fvector2().set(m_scroll_wnd->GetWndSize().x,20.0f));
				pItem->SetText				(action_name);
				pItem->SetAutoDelete		(true);
				m_scroll_wnd->AddWindow		(pItem, true);
			}
		}
		else
			break;				
	}
}

bool CUIKeyBinding::IsActionExist(const char* action, CUIXml& xml_doc)
{
	bool ret				= false;
	int groupsCount			= xml_doc.GetNodesNum("",0,"group");

	for(int i = 0; i<groupsCount; i++)
	{
		// add group items
		int commandsCount	= xml_doc.GetNodesNum("group",i,"command");
		XML_NODE* tab_node	= xml_doc.NavigateToNode("group",i);
		xml_doc.SetLocalRoot(tab_node);

		for(int j = 0; j<commandsCount; ++j)
		{
			// first field of list item
			shared_str command_id = xml_doc.ReadAttrib("command",j,"exe");
			if (0 == xr_strcmp(action, *command_id))
			{
				ret = true;
				break;
			}
		}
		xml_doc.SetLocalRoot(xml_doc.GetRoot());
		if (ret)
			break;
	}
	return ret;
}
#endif

void CUIKeyBinding::Update()
{
	CUIWindow::Update();
	
	// Only update if scroll window is initialized (InitFromXml completed)
	if (m_scroll_wnd)
	{
		UpdateQuickSlotsBindingState();
	}
}

void CUIKeyBinding::UpdateQuickSlotsBindingState()
{
	// Check if elements exist and are valid
	if (m_quickSlotsItem && m_quickSlotsKey1 && m_quickSlotsKey2)
	{
		// Safely access psHUD_Flags
		const bool isHideQuickSlotsEnabled = HUD_IsQuickSlotsAutoHide();
		// Element is always visible
		m_quickSlotsItem->Show(true);
		m_quickSlotsKey1->Show(true);
		m_quickSlotsKey2->Show(true);
		// Enable/disable for interaction based on setting
		m_quickSlotsItem->Enable(isHideQuickSlotsEnabled);
		m_quickSlotsKey1->Enable(isHideQuickSlotsEnabled);
		m_quickSlotsKey2->Enable(isHideQuickSlotsEnabled);
	}
}
