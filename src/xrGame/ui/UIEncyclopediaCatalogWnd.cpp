#include "StdAfx.h"
#include "UIEncyclopediaCatalogWnd.h"

#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UI3dStatic.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrCore/Kernel/EngineExternal.h"
#include "../../xrEngine/xr_input.h"
#include "UIInventoryUtilities.h"

namespace EncyclopediaCatalogWndHelpers
{
constexpr const char* ItemPath = "list_item";
constexpr const char* HeaderPath = "group_header";

bool HasInventoryIcon(const char* Section)
{
	return Section && Section[0] && pSettings->section_exist(Section) && pSettings->line_exist(Section, "inv_grid_x") && pSettings->line_exist(Section, "inv_grid_y") && pSettings->line_exist(Section, "inv_grid_width") && pSettings->line_exist(Section, "inv_grid_height") && pSettings->line_exist(Section, "visual");
}

bool ApplyIcon(CUIStatic* Static, const SEncyclopediaEntry& Entry, Fvector2& Size)
{
	if (!Static)
	{
		return false;
	}
	if (Entry.Icon.size())
	{
		Static->InitTexture(Entry.Icon.c_str());
		const Frect& Rect = Static->GetTextureRect();
		Size.set(Rect.width(), Rect.height());
	}
	else if (HasInventoryIcon(Entry.ItemSection.c_str()))
	{
		const InventoryUtilities::InventoryIconParams Params = InventoryUtilities::GetInventoryIconParams(Entry.ItemSection.c_str());
		Static->InitTexture(Params.icons_texture && Params.icons_texture[0] ? Params.icons_texture : "ui\\ui_icon_equipment");
		const float CellWidth = INV_GRID_WIDTH(Params.scaleIcon);
		const float CellHeight = INV_GRID_HEIGHT(Params.scaleIcon);
		Frect Rect;
		Rect.set(Params.inv_grid_x * CellWidth, Params.inv_grid_y * CellHeight, (Params.inv_grid_x + Params.inv_grid_width) * CellWidth, (Params.inv_grid_y + Params.inv_grid_height) * CellHeight);
		Static->SetTextureRect(Rect);
		Size.set(Rect.width(), Rect.height());
	}
	else
	{
		Static->TextureOff();
		Static->Show(false);
		return false;
	}
	Static->SetStretchTexture(true);
	Static->TextureOn();
	Static->Show(true);
	return true;
}

void PlaceIcon(CUIWindow* Wnd, const Frect& Layout, const Fvector2& Size, bool KeepAspect)
{
	if (!Wnd)
	{
		return;
	}
	Fvector2 Final;
	Final.set(Layout.x2, Layout.y2);
	if (KeepAspect && Size.x > 0.f && Size.y > 0.f)
	{
		const float Scale = std::min(Layout.x2 / Size.x, Layout.y2 / Size.y);
		Final.set(Size.x * Scale, Size.y * Scale);
	}
	Wnd->SetWndPos(Fvector2().set(Layout.x1 + (Layout.x2 - Final.x) * 0.5f, Layout.y1 + (Layout.y2 - Final.y) * 0.5f));
	Wnd->SetWndSize(Final);
}
}

void UIEncyclopediaCatalogItem::InitFromXml(CUIXml& Xml, const char* Path, CUIEncyclopediaCatalogWnd* OwnerWnd)
{
	Owner = OwnerWnd;
	CUIXmlInit::InitWindow(Xml, Path, 0, this);
	BaseHeight = GetHeight();
	Padding = Xml.ReadAttribFlt(Path, 0, "padding", 6.f);

	XML_NODE* StoredRoot = Xml.GetLocalRoot();
	Xml.SetLocalRoot(Xml.NavigateToNode(Path, 0));

	if (Xml.NavigateToNode("selection", 0))
	{
		const char* SelectionType = Xml.ReadAttrib("selection", 0, "type", "static");
		if (!xr_strcmp(SelectionType, "line"))
		{
			Selection = UIHelper::CreateFrameLine(Xml, "selection", this, false);
		}
		else if (!xr_strcmp(SelectionType, "frame"))
		{
			Selection = UIHelper::CreateFrameWindow(Xml, "selection", this, false);
		}
		else
		{
			Selection = UIHelper::CreateStatic(Xml, "selection", this, false);
		}
		if (Selection)
		{
			Selection->Show(false);
		}
	}
	if (Xml.NavigateToNode("new_mark", 0))
	{
		NewMark = UIHelper::CreateStatic(Xml, "new_mark", this, false);
	}
	if (Xml.NavigateToNode("icon", 0))
	{
		Icon = UIHelper::CreateStatic(Xml, "icon", this, false);
		if (Icon)
		{
			IconKeepAspect = Xml.ReadAttribInt("icon", 0, "keep_aspect", 0) != 0;
			IconLayout.set(Icon->GetWndPos().x, Icon->GetWndPos().y, Icon->GetWidth(), Icon->GetHeight());
		}
	}
	if (Xml.NavigateToNode("title", 0))
	{
		Title = UIHelper::CreateStatic(Xml, "title", this, false);
	}

	Xml.SetLocalRoot(StoredRoot);
}

void UIEncyclopediaCatalogItem::Setup(const SEncyclopediaEntry& Entry, u32 TitleColor)
{
	Key = Entry.Key;

	if (Icon)
	{
		Fvector2 Size;
		if (EncyclopediaCatalogWndHelpers::ApplyIcon(Icon, Entry, Size))
		{
			EncyclopediaCatalogWndHelpers::PlaceIcon(Icon, IconLayout, Size, IconKeepAspect);
		}
	}

	float Height = BaseHeight;
	if (Title)
	{
		Title->SetText(Entry.Title.c_str());
		Title->AdjustHeightToText();
		Height = std::max(Height, Title->GetWndPos().y + Title->GetHeight() + Padding);
	}
	SetNew(Entry.IsNew, TitleColor);

	SetHeight(Height);
	if (Selection)
	{
		Selection->SetHeight(Height);
	}
}

void UIEncyclopediaCatalogItem::SetNew(bool IsNew, u32 TitleColor)
{
	if (NewMark)
	{
		NewMark->Show(IsNew);
	}
	if (Title)
	{
		Title->SetTextColor(TitleColor);
	}
}

void UIEncyclopediaCatalogItem::SetSelected(bool Selected)
{
	if (Selection)
	{
		Selection->Show(Selected);
	}
}

bool UIEncyclopediaCatalogItem::OnMouseAction(float X, float Y, EUIMessages MouseAction)
{
	if (Owner && MouseAction == WINDOW_LBUTTON_DOWN)
	{
		Owner->OnItemClicked(this);
		return true;
	}
	return inherited::OnMouseAction(X, Y, MouseAction);
}

bool UIEncyclopediaGroupHeader::OnMouseAction(float X, float Y, EUIMessages MouseAction)
{
	if (Owner && MouseAction == WINDOW_LBUTTON_DOWN)
	{
		Owner->ToggleGroup(Id);
		return true;
	}
	return inherited::OnMouseAction(X, Y, MouseAction);
}

void CUIEncyclopediaCatalogWnd::Init(const char* XmlName)
{
	Xml.Load(CONFIG_PATH, UI_PATH, XmlName);
	CUIXmlInit::InitWindow(Xml, "catalog", 0, this);

	CUIXmlInit::InitAutoStaticGroup(Xml, "catalog", 0, this);

	List = new CUIScrollView();
	List->SetAutoDelete(true);
	AttachChild(List);
	CUIXmlInit::InitScrollView(Xml, "list", 0, List);

	if (Xml.NavigateToNode("t_empty", 0))
	{
		EmptyText = UIHelper::CreateStatic(Xml, "t_empty", this, false);
	}

	if (Xml.NavigateToNode(EncyclopediaCatalogWndHelpers::HeaderPath, 0))
	{
		GroupCollapsible = Xml.ReadAttribInt(EncyclopediaCatalogWndHelpers::HeaderPath, 0, "collapsible", 0) != 0;
		GroupCounters = Xml.ReadAttribInt(EncyclopediaCatalogWndHelpers::HeaderPath, 0, "counters", 0) != 0;
		GroupPrefixOpen = Xml.ReadAttrib(EncyclopediaCatalogWndHelpers::HeaderPath, 0, "prefix_open", "");
		GroupPrefixClosed = Xml.ReadAttrib(EncyclopediaCatalogWndHelpers::HeaderPath, 0, "prefix_closed", "");
	}
	ColorNormal = CUIXmlInit::GetColor(Xml, "list_item:colors:normal", 0, ColorNormal);
	ColorNew = CUIXmlInit::GetColor(Xml, "list_item:colors:new", 0, ColorNew);

	if (!Xml.NavigateToNode("details", 0))
	{
		return;
	}

	Details = new CUIWindow();
	Details->SetAutoDelete(true);
	AttachChild(Details);
	CUIXmlInit::InitWindow(Xml, "details", 0, Details);

	XML_NODE* StoredRoot = Xml.GetLocalRoot();
	Xml.SetLocalRoot(Xml.NavigateToNode("details", 0));

	if (Xml.NavigateToNode("empty", 0))
	{
		DetailsEmpty = UIHelper::CreateStatic(Xml, "empty", Details, false);
	}
	if (Xml.NavigateToNode("image", 0))
	{
		DetailsImage = UIHelper::CreateStatic(Xml, "image", Details, false);
	}
	if (Xml.NavigateToNode("subtitle", 0))
	{
		DetailsSubtitle = UIHelper::CreateStatic(Xml, "subtitle", Details, false);
	}
	if (Xml.NavigateToNode("icon", 0))
	{
		DetailsIcon = UIHelper::CreateStatic(Xml, "icon", Details, false);
		IconGap = Xml.ReadAttribFlt("icon", 0, "gap", 6.f);
		IconKeepAspect = Xml.ReadAttribInt("icon", 0, "keep_aspect", 1) != 0;
		IconPushBody = Xml.ReadAttribInt("icon", 0, "push_body", 1) != 0;
		Use3dIcons = Xml.ReadAttribBool("icon", 0, "use_3d", EngineExternal()[EEngineExternalGame::Enable3DIcons]);
		if (DetailsIcon)
		{
			IconBase.set(DetailsIcon->GetWndPos().x, DetailsIcon->GetWndPos().y, DetailsIcon->GetWidth(), DetailsIcon->GetHeight());
			DetailsIcon->Show(false);
		}
		if (Use3dIcons)
		{
			DetailsIcon3d = UIHelper::Create3dStatic(Xml, "icon", Details, false);
			if (DetailsIcon3d)
			{
				DetailsIcon3d->Show(false);
			}
		}
	}
	if (Xml.NavigateToNode("body", 0))
	{
		DetailsBody = new CUIScrollView();
		DetailsBody->SetAutoDelete(true);
		Details->AttachChild(DetailsBody);
		CUIXmlInit::InitScrollView(Xml, "body", 0, DetailsBody);
		BodyBasePos = DetailsBody->GetWndPos();
		BodyBaseHeight = DetailsBody->GetHeight();
	}

	Xml.SetLocalRoot(StoredRoot);
}

void CUIEncyclopediaCatalogWnd::Show(bool Status)
{
	inherited::Show(Status);
	if (Status)
	{
		Refresh();
	}
}

void CUIEncyclopediaCatalogWnd::Update()
{
	inherited::Update();
	if (NeedRebuild)
	{
		RebuildList();
	}
}

bool CUIEncyclopediaCatalogWnd::OnKeyboardAction(int Dik, EUIMessages KeyboardAction)
{
	if (KeyboardAction == WINDOW_KEY_PRESSED)
	{
		if (Dik == SDL_SCANCODE_UP)
		{
			MoveSelection(false);
			return true;
		}
		if (Dik == SDL_SCANCODE_DOWN)
		{
			MoveSelection(true);
			return true;
		}
	}
	return inherited::OnKeyboardAction(Dik, KeyboardAction);
}

void CUIEncyclopediaCatalogWnd::OnItemClicked(UIEncyclopediaCatalogItem* Item)
{
	if (Item)
	{
		SelectEntry(Item->Key);
	}
}

void CUIEncyclopediaCatalogWnd::ToggleGroup(const shared_str& GroupId)
{
	if (!GroupCollapsible)
	{
		return;
	}
	const auto Found = std::find(CollapsedGroups.begin(), CollapsedGroups.end(), GroupId);
	if (Found != CollapsedGroups.end())
	{
		CollapsedGroups.erase(Found);
	}
	else
	{
		CollapsedGroups.push_back(GroupId);
	}
	NeedRebuild = true;
}

void CUIEncyclopediaCatalogWnd::Refresh()
{
	EncyclopediaCatalog::Collect(Groups);
	RebuildList();
}

u32 CUIEncyclopediaCatalogWnd::TitleColor(bool IsNew) const
{
	return IsNew ? ColorNew : ColorNormal;
}

SEncyclopediaEntry* CUIEncyclopediaCatalogWnd::FindEntry(const shared_str& EntryKey)
{
	if (!EntryKey.size())
	{
		return nullptr;
	}
	for (SEncyclopediaGroup& Group : Groups)
	{
		for (SEncyclopediaEntry& Entry : Group.Entries)
		{
			if (Entry.Key == EntryKey)
			{
				return &Entry;
			}
		}
	}
	return nullptr;
}

void CUIEncyclopediaCatalogWnd::RebuildList()
{
	NeedRebuild = false;
	if (!List)
	{
		return;
	}
	const int ScrollPos = List->GetCurrentScrollPos();
	List->Clear();

	const bool HasHeader = Xml.NavigateToNode(EncyclopediaCatalogWndHelpers::HeaderPath, 0) != nullptr;
	shared_str FirstVisible;
	for (const SEncyclopediaGroup& Group : Groups)
	{
		const bool Collapsed = GroupCollapsible && std::find(CollapsedGroups.begin(), CollapsedGroups.end(), Group.Id) != CollapsedGroups.end();
		if (HasHeader)
		{
			UIEncyclopediaGroupHeader* Header = new UIEncyclopediaGroupHeader();
			CUIXmlInit::InitStatic(Xml, EncyclopediaCatalogWndHelpers::HeaderPath, 0, Header);
			Header->SetOwner(this, Group.Id);
			xr_string Text = GroupCollapsible ? (Collapsed ? GroupPrefixClosed : GroupPrefixOpen) : xr_string();
			Text += Group.Caption;
			if (GroupCounters)
			{
				string32 Count;
				xr_sprintf(Count, " (%u)", u32(Group.Entries.size()));
				Text += Count;
			}
			Header->SetText(Text.c_str());
			List->AddWindow(Header, true);
		}
		if (Collapsed)
		{
			continue;
		}
		for (const SEncyclopediaEntry& Entry : Group.Entries)
		{
			UIEncyclopediaCatalogItem* Item = new UIEncyclopediaCatalogItem();
			Item->InitFromXml(Xml, EncyclopediaCatalogWndHelpers::ItemPath, this);
			Item->Setup(Entry, TitleColor(Entry.IsNew));
			List->AddWindow(Item, true);
			if (!FirstVisible.size())
			{
				FirstVisible = Entry.Key;
			}
		}
	}

	if (EmptyText)
	{
		EmptyText->Show(Groups.empty());
	}

	List->ForceUpdate();
	List->SetScrollPos(ScrollPos);

	SelectEntry(FindEntry(SelectedKey) ? SelectedKey : FirstVisible);
}

void CUIEncyclopediaCatalogWnd::SelectEntry(const shared_str& EntryKey)
{
	SelectedKey = EntryKey;
	SEncyclopediaEntry* Entry = FindEntry(EntryKey);
	const bool WasNew = Entry && Entry->IsNew;
	if (WasNew)
	{
		EncyclopediaCatalog::MarkRead(*Entry);
		Entry->IsNew = false;
	}

	if (List)
	{
		for (CUIWindow* Window : List->Items())
		{
			UIEncyclopediaCatalogItem* Item = smart_cast<UIEncyclopediaCatalogItem*>(Window);
			if (!Item)
			{
				continue;
			}
			const bool Selected = Entry && Item->Key == EntryKey;
			Item->SetSelected(Selected);
			if (Selected && WasNew)
			{
				Item->SetNew(false, TitleColor(false));
			}
		}
	}
	UpdateDetails();
}

void CUIEncyclopediaCatalogWnd::MoveSelection(bool Next)
{
	xr_vector<shared_str> Keys;
	for (const SEncyclopediaGroup& Group : Groups)
	{
		if (GroupCollapsible && std::find(CollapsedGroups.begin(), CollapsedGroups.end(), Group.Id) != CollapsedGroups.end())
		{
			continue;
		}
		for (const SEncyclopediaEntry& Entry : Group.Entries)
		{
			Keys.push_back(Entry.Key);
		}
	}
	if (Keys.empty())
	{
		return;
	}
	auto Current = std::find(Keys.begin(), Keys.end(), SelectedKey);
	if (Current == Keys.end())
	{
		SelectEntry(Keys.front());
		return;
	}
	if (Next && Current + 1 != Keys.end())
	{
		SelectEntry(*(Current + 1));
	}
	else if (!Next && Current != Keys.begin())
	{
		SelectEntry(*(Current - 1));
	}
}

bool CUIEncyclopediaCatalogWnd::Apply3dIcon(const shared_str& Section)
{
	if (!DetailsIcon3d || !EncyclopediaCatalogWndHelpers::HasInventoryIcon(Section.c_str()))
	{
		return false;
	}
	const InventoryUtilities::InventoryIconParams Params = InventoryUtilities::GetInventoryIconParams(Section.c_str());
	if (!Params._3d_static_visual || !Params._3d_static_visual[0])
	{
		return false;
	}
	DetailsIcon3d->SetIconVisual(Params._3d_static_visual);
	if (!DetailsIcon3d->GetVisual())
	{
		return false;
	}
	Fvector Rotate = Params._3d_static_rotate;
	DetailsIcon3d->SetXYZ(Rotate);
	DetailsIcon3d->SetScaleFactor(Params._3d_static_scale);
	DetailsIcon3d->SetIconOffset(Params.IconOffset);
	DetailsIcon3d->CaptureIconBones();
	DetailsIcon3d->SetWndPos(Fvector2().set(IconBase.x1, IconBase.y1));
	DetailsIcon3d->SetWndSize(Fvector2().set(IconBase.x2, IconBase.y2));
	DetailsIcon3d->Show(true);
	return true;
}

void CUIEncyclopediaCatalogWnd::UpdateVisuals(const SEncyclopediaEntry* Entry)
{
	const bool HasImage = DetailsImage && Entry && Entry->Image.size();
	if (DetailsImage)
	{
		DetailsImage->Show(HasImage);
		if (HasImage)
		{
			DetailsImage->InitTexture(Entry->Image.c_str());
			DetailsImage->SetStretchTexture(true);
			DetailsImage->TextureOn();
		}
	}

	if (DetailsIcon3d)
	{
		DetailsIcon3d->Show(false);
	}
	bool ShowIcon = false;
	if (Entry && !HasImage)
	{
		if (Use3dIcons && !Entry->Icon.size() && Entry->ItemSection.size() && Apply3dIcon(Entry->ItemSection))
		{
			ShowIcon = true;
			if (DetailsIcon)
			{
				DetailsIcon->Show(false);
			}
		}
		else if (DetailsIcon)
		{
			Fvector2 Size;
			ShowIcon = EncyclopediaCatalogWndHelpers::ApplyIcon(DetailsIcon, *Entry, Size);
			if (ShowIcon)
			{
				EncyclopediaCatalogWndHelpers::PlaceIcon(DetailsIcon, IconBase, Size, IconKeepAspect);
			}
		}
	}
	else if (DetailsIcon)
	{
		DetailsIcon->Show(false);
	}

	if (!DetailsBody)
	{
		return;
	}
	float Top = BodyBasePos.y;
	if (!HasImage && DetailsImage)
	{
		Top = DetailsImage->GetWndPos().y;
	}
	if (ShowIcon && IconPushBody)
	{
		Top = std::max(Top, IconBase.y1 + IconBase.y2 + IconGap);
	}
	const float Bottom = BodyBasePos.y + BodyBaseHeight;
	DetailsBody->SetWndPos(Fvector2().set(BodyBasePos.x, Top));
	DetailsBody->SetHeight(std::max(0.f, Bottom - Top));
}

CUIStatic* CUIEncyclopediaCatalogWnd::AddLine(const shared_str& Style, const char* Text)
{
	if (!DetailsBody)
	{
		return nullptr;
	}
	string256 Path;
	xr_strconcat(Path, "details:line_", Style.size() ? Style.c_str() : "text");
	if (!Xml.NavigateToNode(Path, 0))
	{
		xr_strcpy(Path, "details:line_text");
		if (!Xml.NavigateToNode(Path, 0))
		{
			return nullptr;
		}
	}
	CUIStatic* Static = new CUIStatic();
	CUIXmlInit::InitStatic(Xml, Path, 0, Static);
	Static->SetText(Text);
	Static->AdjustHeightToText();
	DetailsBody->AddWindow(Static, true);
	return Static;
}

void CUIEncyclopediaCatalogWnd::UpdateDetails()
{
	if (!Details)
	{
		return;
	}
	const SEncyclopediaEntry* Entry = FindEntry(SelectedKey);
	const bool HasEntry = Entry != nullptr;

	if (DetailsEmpty)
	{
		DetailsEmpty->Show(!HasEntry);
	}
	if (DetailsSubtitle)
	{
		DetailsSubtitle->Show(HasEntry);
		DetailsSubtitle->SetText(HasEntry ? Entry->Subtitle.c_str() : "");
	}
	UpdateVisuals(Entry);

	if (!DetailsBody)
	{
		return;
	}
	DetailsBody->Clear();
	DetailsBody->Show(HasEntry);
	if (!HasEntry)
	{
		return;
	}
	for (const SEncyclopediaLine& Line : Entry->Lines)
	{
		AddLine(Line.Style, Line.Text.c_str());
	}
	DetailsBody->ForceUpdate();
	DetailsBody->ScrollToBegin();
}
