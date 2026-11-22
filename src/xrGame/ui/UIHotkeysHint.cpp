#include "StdAfx.h"
#include "UIHotkeysHint.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/ui_base.h"
#include "../../xrUI/Widgets/UILines.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrEngine/xr_level_controller.h"
#include "../../xrEngine/string_table.h"
#include "../../xrEngine/xr_input.h"

namespace HotkeysHintChips
{
constexpr const char* KeyNode = "key";
constexpr const char* KeyBackNode = "key_back";
constexpr const char* LabelNode = "label";
constexpr const char* BindingSeparator = ",";
constexpr const char* StylesFile = "hotkey_styles.xml";

CUIXml& StyleXml()
{
	static CUIXml Xml;
	static bool Loaded = false;
	if (!Loaded)
	{
		Loaded = true;
		Xml.Load(CONFIG_PATH, UI_PATH, StylesFile);
	}
	return Xml;
}

XML_NODE* FindStyle(const char* Id)
{
	CUIXml& Xml = StyleXml();
	if (!Id || !Id[0] || !Xml.GetRoot())
	{
		return nullptr;
	}
	Xml.SetLocalRoot(Xml.GetRoot());
	return Xml.NavigateToNodeWithAttribute("style", "id", Id);
}

xr_string Trim(const xr_string& Text)
{
	const size_t First = Text.find_first_not_of(" \t");
	if (First == xr_string::npos)
	{
		return "";
	}
	const size_t Last = Text.find_last_not_of(" \t");
	return Text.substr(First, Last - First + 1);
}

void SplitBinding(const xr_string& Binding, xr_vector<xr_string>& Result)
{
	size_t Start = 0;
	while (Start <= Binding.size())
	{
		const size_t Separator = Binding.find(BindingSeparator, Start);
		const xr_string Part = Trim(Binding.substr(Start, Separator == xr_string::npos ? xr_string::npos : Separator - Start));
		if (!Part.empty())
		{
			Result.push_back(Part);
		}
		if (Separator == xr_string::npos)
		{
			break;
		}
		Start = Separator + 1;
	}
}

float TextWidth(CUIStatic& Static, const char* Text)
{
	CUILines* Lines = Static.TextItemControl();
	CGameFont* Font = Lines ? Lines->GetFont() : nullptr;
	if (!Font || !Text || !Text[0])
	{
		return 0.0f;
	}

	float Width = Font->SizeOf_(Text);
	UI().ClientToScreenScaledWidth(Width);
	return Width;
}
}

void CUIHotkeysHint::CreateAll(CUIXml& Xml, CUIWindow* Parent, xr_vector<CUIHotkeysHint*>& Result)
{
	if (!Parent)
	{
		return;
	}
	for (const char* Path : {"hotkeys_hint", "hotkey_bar", "hotkey"})
	{
		for (int Index = 0; Xml.NavigateToNode(Path, Index); ++Index)
		{
			CUIHotkeysHint* Hint = new CUIHotkeysHint();
			Hint->SetAutoDelete(true);
			Parent->AttachChild(Hint);
			Hint->InitFromXml(Xml, Path, Index);
			Result.push_back(Hint);
		}
	}
}

void CUIHotkeysHint::InitFromXml(CUIXml& Xml, const char* Path, int Index)
{
	CUIXmlInit::InitStatic(Xml, Path, Index, this);

	XML_NODE* Root = Xml.NavigateToNode(Path, Index);
	if (!Root)
	{
		return;
	}

	CUIXml* TemplateXml = &Xml;
	XML_NODE* TemplateRoot = Root;
	XML_NODE* StyleRoot = nullptr;
	const char* StyleId = Xml.ReadAttrib(Root, "style", "");
	if (StyleId && StyleId[0])
	{
		StyleRoot = HotkeysHintChips::FindStyle(StyleId);
		if (!StyleRoot)
		{
			Msg("! [hotkeys] style [%s] not found in %s", StyleId, HotkeysHintChips::StylesFile);
		}
		else if (!Xml.NavigateToNode(Root, HotkeysHintChips::KeyNode, 0))
		{
			TemplateXml = &HotkeysHintChips::StyleXml();
			TemplateRoot = StyleRoot;
		}
	}

	ChipMode = TemplateXml->NavigateToNode(TemplateRoot, HotkeysHintChips::KeyNode, 0) != nullptr;
	if (ChipMode)
	{
		KeyFormat = "%s";
	}
	if (StyleRoot)
	{
		ReadLayout(HotkeysHintChips::StyleXml(), StyleRoot);
	}
	ReadLayout(Xml, Root);

	ReadItems(Xml, Root);
	AutoContexts = Xml.ReadAttrib(Root, "context", "");
	RebuildAutoItems();

	if (ChipMode)
	{
		InitChips(*TemplateXml, TemplateRoot, u32(FixedItems.size()) + (AutoContexts.size() ? MaxAutoItems : 0));
	}

	SeenRevision = Hotkeys::Revision();
	SeenControllerMode = pInput && pInput->GetControllerMode();
	Refresh();
}

void CUIHotkeysHint::ReadLayout(CUIXml& Xml, XML_NODE* Root)
{
	Separator = Xml.ReadAttrib(Root, "separator", Separator.c_str());
	LabelSeparator = Xml.ReadAttrib(Root, "label_separator", LabelSeparator.c_str());
	KeyFormat = Xml.ReadAttrib(Root, "key_format", KeyFormat.c_str());
	if (KeyFormat.find("%s") == xr_string::npos)
	{
		KeyFormat = ChipMode ? "%s" : "[%s]";
	}
	GamepadKeyFormat = Xml.ReadAttrib(Root, "gamepad_key_format", GamepadKeyFormat.c_str());
	if (GamepadKeyFormat.find("%s") == xr_string::npos)
	{
		GamepadKeyFormat = "%s";
	}

	KeyPadding = Xml.ReadAttribFlt(Root, "key_padding", KeyPadding);
	KeyMinWidth = Xml.ReadAttribFlt(Root, "key_min_width", KeyMinWidth);
	KeySpacing = Xml.ReadAttribFlt(Root, "key_spacing", KeySpacing);
	LabelSpacing = Xml.ReadAttribFlt(Root, "label_spacing", LabelSpacing);
	ItemSpacing = Xml.ReadAttribFlt(Root, "item_spacing", ItemSpacing);
	const char* Align = Xml.ReadAttrib(Root, "chips_align", "");
	if (Align && Align[0])
	{
		ChipsAlign = Align[0];
	}
}

void CUIHotkeysHint::ReadItems(CUIXml& Xml, XML_NODE* Root)
{
	FixedItems.clear();
	const int Count = Xml.GetNodesNum(Root, "item");
	for (int i = 0; i < Count; ++i)
	{
		SHotkeyItem Item;
		Item.Action = Xml.ReadAttrib(Root, "item", i, "action", "");
		Item.GamepadAction = Xml.ReadAttrib(Root, "item", i, "gamepad_action", "");
		Item.FixedKey = Xml.ReadAttrib(Root, "item", i, "key", "");
		Item.Label = Xml.Read(Root, "item", i, "");
		if (!Item.Label.size())
		{
			Item.Label = Xml.ReadAttrib(Root, "item", i, "label", "");
		}
		Item.Context = Xml.ReadAttrib(Root, "item", i, "context", "");
		if (Item.Action.size() || Item.FixedKey.size())
		{
			FixedItems.push_back(Item);
		}
	}

	const char* Action = Xml.ReadAttrib(Root, "action", "");
	if (FixedItems.empty() && Action && Action[0])
	{
		SHotkeyItem Item;
		Item.Action = Action;
		Item.GamepadAction = Xml.ReadAttrib(Root, "gamepad_action", "");
		Item.Label = Xml.ReadAttrib(Root, "label", "");
		FixedItems.push_back(Item);
	}
}

void CUIHotkeysHint::RebuildAutoItems()
{
	HotkeyItems = FixedItems;
	if (!AutoContexts.size())
	{
		return;
	}

	u32 Added = 0;
	xr_vector<EGameActions> Actions;
	const int ContextCount = _GetItemCount(AutoContexts.c_str());
	for (int ContextIndex = 0; ContextIndex < ContextCount && Added < MaxAutoItems; ++ContextIndex)
	{
		string128 ContextName;
		_GetItem(AutoContexts.c_str(), ContextIndex, ContextName);
		Hotkeys::ContextActions(ContextName, Actions);
		for (EGameActions Id : Actions)
		{
			if (Added >= MaxAutoItems)
			{
				break;
			}
			SHotkeyItem Item;
			Item.Action = id_to_action_name(Id);
			if (const SHotkeyInfo* Info = Hotkeys::Info(Id))
			{
				Item.Label = Info->Label;
			}
			HotkeyItems.push_back(Item);
			++Added;
		}
	}
}

void CUIHotkeysHint::Update()
{
	inherited::Update();

	const u32 Revision = Hotkeys::Revision();
	const bool ControllerMode = pInput && pInput->GetControllerMode();
	if (Revision == SeenRevision && ControllerMode == SeenControllerMode)
	{
		return;
	}
	SeenRevision = Revision;
	SeenControllerMode = ControllerMode;
	RebuildAutoItems();
	Refresh();
}

void CUIHotkeysHint::InitChips(CUIXml& Xml, XML_NODE* TemplateRoot, u32 Count)
{
	XML_NODE* StoredRoot = Xml.GetLocalRoot();
	Xml.SetLocalRoot(TemplateRoot);

	const bool HasBack = Xml.NavigateToNode(HotkeysHintChips::KeyBackNode, 0) != nullptr;
	const char* BackType = HasBack ? Xml.ReadAttrib(HotkeysHintChips::KeyBackNode, 0, "type", "static") : "";

	ChipsLayer = new CUIWindow();
	ChipsLayer->SetAutoDelete(true);
	ChipsLayer->SetWndPos(Fvector2().set(0.0f, 0.0f));
	ChipsLayer->SetWndSize(GetWndSize());
	AttachChild(ChipsLayer);

	Chips.resize(Count);
	for (SChip& Chip : Chips)
	{
		for (u32 KeyIndex = 0; KeyIndex < MaxKeysPerItem; ++KeyIndex)
		{
			if (HasBack)
			{
				if (!xr_strcmp(BackType, "frame"))
				{
					Chip.Back[KeyIndex] = UIHelper::CreateFrameWindow(Xml, HotkeysHintChips::KeyBackNode, ChipsLayer, false);
				}
				else if (!xr_strcmp(BackType, "line"))
				{
					Chip.Back[KeyIndex] = UIHelper::CreateFrameLine(Xml, HotkeysHintChips::KeyBackNode, ChipsLayer, false);
				}
				else
				{
					Chip.Back[KeyIndex] = UIHelper::CreateStatic(Xml, HotkeysHintChips::KeyBackNode, ChipsLayer, false);
				}
			}

			Chip.Key[KeyIndex] = UIHelper::CreateStatic(Xml, HotkeysHintChips::KeyNode, ChipsLayer, false);
		}

		Chip.Label = UIHelper::CreateStatic(Xml, HotkeysHintChips::LabelNode, ChipsLayer, false);
	}

	Xml.SetLocalRoot(StoredRoot);
	SetText("");
	HideChips();
}

void CUIHotkeysHint::Refresh()
{
	if (HotkeyItems.empty())
	{
		if (ChipMode)
		{
			HideChips();
		}
		else
		{
			TextItemControl()->SetText("");
		}
		return;
	}

	xr_vector<SResolvedItem> Items;
	ResolveItems(Items);

	if (ChipMode)
	{
		RefreshChips(Items);
		return;
	}

	size_t ItemCount = Items.size();
	xr_string Text = BuildText(Items, ItemCount);
	while (ItemCount > 1 && !FitsWidth(Text))
	{
		--ItemCount;
		Text = BuildText(Items, ItemCount);
	}

	TextItemControl()->SetText(Text.c_str());
}

void CUIHotkeysHint::SetActiveContexts(const xr_vector<shared_str>& Contexts)
{
	if (Contexts == ActiveContexts)
	{
		return;
	}
	ActiveContexts = Contexts;
	Refresh();
}

bool CUIHotkeysHint::IsContextActive(const shared_str& Context) const
{
	if (!Context.size())
	{
		return true;
	}

	const int Count = _GetItemCount(Context.c_str());
	for (int i = 0; i < Count; ++i)
	{
		string64 Name;
		_GetItem(Context.c_str(), i, Name);
		const bool Negate = Name[0] == '!';
		const char* Id = Negate ? Name + 1 : Name;
		const bool Present = std::find_if(ActiveContexts.begin(), ActiveContexts.end(),
			[Id](const shared_str& Active) { return !xr_strcmp(Active.c_str(), Id); }) != ActiveContexts.end();
		if (Present == Negate)
		{
			return false;
		}
	}
	return true;
}

void CUIHotkeysHint::ResolveItems(xr_vector<SResolvedItem>& Result) const
{
	Result.clear();
	const shared_str NotBound = g_pStringTable->translate("st_key_notbinded");
	const bool ControllerMode = pInput && pInput->GetControllerMode();

	for (u32 ItemIndex = 0; ItemIndex < HotkeyItems.size(); ++ItemIndex)
	{
		const SHotkeyItem& Item = HotkeyItems[ItemIndex];
		if (!IsContextActive(Item.Context))
		{
			continue;
		}

		SResolvedItem Resolved;
		Resolved.Source = &Item;
		Resolved.Index = ItemIndex;

		if (Item.FixedKey.size() && !ControllerMode)
		{
			Resolved.Keys.push_back(Item.FixedKey.c_str());
		}

		const shared_str& Actions = (ControllerMode && Item.GamepadAction.size()) ? Item.GamepadAction : Item.Action;
		const int KeyCount = Resolved.Keys.empty() && Actions.size() ? _GetItemCount(Actions.c_str()) : 0;
		for (int i = 0; i < KeyCount; ++i)
		{
			string64 ActionName;
			_GetItem(Actions.c_str(), i, ActionName);

			string128 Binding;
			GetActionAllBinding(ActionName, Binding, sizeof(Binding));
			if (!Binding[0] || (NotBound.size() && !xr_strcmp(Binding, NotBound.c_str())))
			{
				continue;
			}
			Resolved.Keys.push_back(Binding);
		}

		if (!Resolved.Keys.empty())
		{
			Result.push_back(Resolved);
		}
	}
}

xr_string CUIHotkeysHint::BuildText(const xr_vector<SResolvedItem>& Items, size_t ItemCount) const
{
	const bool ControllerMode = pInput && pInput->GetControllerMode();
	const xr_string& Format = ControllerMode ? GamepadKeyFormat : KeyFormat;

	xr_string Result;
	for (size_t ItemIndex = 0; ItemIndex < Items.size() && ItemIndex < ItemCount; ++ItemIndex)
	{
		const SResolvedItem& Item = Items[ItemIndex];
		xr_string Keys;
		for (const xr_string& Binding : Item.Keys)
		{
			string256 Key;
			xr_sprintf(Key, Format.c_str(), Binding.c_str());
			if (!Keys.empty())
			{
				Keys += " ";
			}
			Keys += Key;
		}

		if (!Result.empty())
		{
			Result += Separator;
		}
		Result += Keys;
		if (Item.Source->Label.size())
		{
			Result += LabelSeparator;
			Result += g_pStringTable->translate(Item.Source->Label).c_str();
		}
	}
	return Result;
}

bool CUIHotkeysHint::FitsWidth(const xr_string& Text) const
{
	CUILines* Lines = const_cast<CUIHotkeysHint*>(this)->TextItemControl();
	CGameFont* Font = Lines ? Lines->GetFont() : nullptr;
	if (!Font)
	{
		return true;
	}

	float Width = Font->SizeOf_(Text.c_str());
	UI().ClientToScreenScaledWidth(Width);
	return Width <= GetWidth();
}

void CUIHotkeysHint::HideChips()
{
	for (SChip& Chip : Chips)
	{
		for (u32 KeyIndex = 0; KeyIndex < MaxKeysPerItem; ++KeyIndex)
		{
			if (Chip.Back[KeyIndex])
			{
				Chip.Back[KeyIndex]->Show(false);
			}
			if (Chip.Key[KeyIndex])
			{
				Chip.Key[KeyIndex]->Show(false);
			}
		}
		if (Chip.Label)
		{
			Chip.Label->Show(false);
		}
	}
}

float CUIHotkeysHint::ChipKeyWidth(CUIStatic& Key, const xr_string& Text) const
{
	const float Width = HotkeysHintChips::TextWidth(Key, Text.c_str()) + KeyPadding * 2.0f;
	const float MinWidth = KeyMinWidth > 0.0f ? KeyMinWidth : Key.GetHeight();
	return std::max(Width, MinWidth);
}

float CUIHotkeysHint::ChipLabelWidth(CUIStatic& Label, const char* Text) const
{
	return HotkeysHintChips::TextWidth(Label, Text);
}

float CUIHotkeysHint::MeasureItem(const SResolvedItem& Item) const
{
	if (Item.Index >= Chips.size())
	{
		return 0.0f;
	}
	const SChip& Chip = Chips[Item.Index];
	const bool ControllerMode = pInput && pInput->GetControllerMode();
	const xr_string& Format = ControllerMode ? GamepadKeyFormat : KeyFormat;

	float Width = 0.0f;
	u32 KeyIndex = 0;
	for (const xr_string& Binding : Item.Keys)
	{
		xr_vector<xr_string> Parts;
		HotkeysHintChips::SplitBinding(Binding, Parts);
		for (const xr_string& Part : Parts)
		{
			if (KeyIndex >= MaxKeysPerItem || !Chip.Key[KeyIndex])
			{
				break;
			}
			string256 Text;
			xr_sprintf(Text, Format.c_str(), Part.c_str());
			Width += (KeyIndex ? KeySpacing : 0.0f) + ChipKeyWidth(*Chip.Key[KeyIndex], Text);
			++KeyIndex;
		}
	}

	if (Chip.Label && Item.Source->Label.size())
	{
		const shared_str LabelText = g_pStringTable->translate(Item.Source->Label);
		Width += LabelSpacing + ChipLabelWidth(*Chip.Label, LabelText.c_str());
	}
	return Width;
}

void CUIHotkeysHint::RefreshChips(const xr_vector<SResolvedItem>& Items)
{
	HideChips();
	if (!ChipsLayer)
	{
		return;
	}

	ChipsLayer->SetWndSize(GetWndSize());

	size_t ItemCount = Items.size();
	float TotalWidth = 0.0f;
	while (ItemCount > 0)
	{
		TotalWidth = 0.0f;
		for (size_t ItemIndex = 0; ItemIndex < ItemCount; ++ItemIndex)
		{
			TotalWidth += (ItemIndex ? ItemSpacing : 0.0f) + MeasureItem(Items[ItemIndex]);
		}
		if (TotalWidth <= GetWidth() || ItemCount == 1)
		{
			break;
		}
		--ItemCount;
	}

	float X = 0.0f;
	if (ChipsAlign == 'c')
	{
		X = (GetWidth() - TotalWidth) * 0.5f;
	}
	else if (ChipsAlign == 'r')
	{
		X = GetWidth() - TotalWidth;
	}

	const bool ControllerMode = pInput && pInput->GetControllerMode();
	const xr_string& Format = ControllerMode ? GamepadKeyFormat : KeyFormat;
	const float Height = GetHeight();

	for (size_t ItemIndex = 0; ItemIndex < ItemCount; ++ItemIndex)
	{
		const SResolvedItem& Item = Items[ItemIndex];
		if (Item.Index >= Chips.size())
		{
			break;
		}
		SChip& Chip = Chips[Item.Index];
		if (ItemIndex)
		{
			X += ItemSpacing;
		}

		u32 KeyIndex = 0;
		for (const xr_string& Binding : Item.Keys)
		{
			xr_vector<xr_string> Parts;
			HotkeysHintChips::SplitBinding(Binding, Parts);
			for (const xr_string& Part : Parts)
			{
				if (KeyIndex >= MaxKeysPerItem || !Chip.Key[KeyIndex])
				{
					break;
				}

				string256 Text;
				xr_sprintf(Text, Format.c_str(), Part.c_str());
				CUIStatic& Key = *Chip.Key[KeyIndex];
				if (KeyIndex)
				{
					X += KeySpacing;
				}

				const float Width = ChipKeyWidth(Key, Text);
				const float Y = (Height - Key.GetHeight()) * 0.5f;
				Key.SetWndPos(Fvector2().set(X, Y));
				Key.SetWidth(Width);
				Key.SetText(Text);
				Key.Show(true);

				if (CUIWindow* Back = Chip.Back[KeyIndex])
				{
					Back->SetWndPos(Fvector2().set(X, (Height - Back->GetHeight()) * 0.5f));
					Back->SetWidth(Width);
					Back->Show(true);
				}

				X += Width;
				++KeyIndex;
			}
		}

		if (Chip.Label && Item.Source->Label.size())
		{
			const shared_str LabelText = g_pStringTable->translate(Item.Source->Label);
			CUIStatic& Label = *Chip.Label;
			X += LabelSpacing;
			const float Width = ChipLabelWidth(Label, LabelText.c_str());
			Label.SetWndPos(Fvector2().set(X, (Height - Label.GetHeight()) * 0.5f));
			Label.SetWidth(Width);
			Label.SetText(LabelText.c_str());
			Label.Show(true);
			X += Width;
		}
	}
}
