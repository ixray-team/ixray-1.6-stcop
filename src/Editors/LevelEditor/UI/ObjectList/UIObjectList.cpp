#include "stdafx.h"
#include "IconsFontAwesome6.h"
#include "imgui_internal.h"

UIObjectList* UIObjectList::Form = nullptr;

static const char* DragPayload = "OUTLINER_OBJECTS";

static const char* ClassIcon(ObjClassID ClassID)
{
	switch (ClassID)
	{
	case OBJCLASS_GROUP:		return ICON_FA_LAYER_GROUP;
	case OBJCLASS_GLOW:			return ICON_FA_SUN;
	case OBJCLASS_SCENEOBJECT:	return ICON_FA_CUBE;
	case OBJCLASS_LIGHT:		return ICON_FA_LIGHTBULB;
	case OBJCLASS_SHAPE:		return ICON_FA_SHAPES;
	case OBJCLASS_SOUND_SRC:	return ICON_FA_VOLUME_HIGH;
	case OBJCLASS_SPAWNPOINT:	return ICON_FA_LOCATION_DOT;
	case OBJCLASS_WAY:			return ICON_FA_ROUTE;
	case OBJCLASS_SECTOR:		return ICON_FA_VECTOR_SQUARE;
	case OBJCLASS_PORTAL:		return ICON_FA_DOOR_OPEN;
	case OBJCLASS_SOUND_ENV:	return ICON_FA_WAVE_SQUARE;
	case OBJCLASS_PS:			return ICON_FA_FIRE;
	case OBJCLASS_FOG_VOL:		return ICON_FA_CLOUD;
	case OBJCLASS_PUDDLES:		return ICON_FA_DROPLET;
	case OBJCLASS_TERRAIN:		return ICON_FA_MOUNTAIN;
	default:					return ICON_FA_CUBE;
	}
}

static bool ContainsNoCase(const char* Str, const char* Sub)
{
	if (!Sub[0])
	{
		return true;
	}

	for (; *Str; ++Str)
	{
		const char* A = Str;
		const char* B = Sub;
		while (*A && *B && tolower((u8)*A) == tolower((u8)*B))
		{
			++A;
			++B;
		}

		if (!*B)
		{
			return true;
		}
	}
	return false;
}

static bool HasName(CCustomObject* Object)
{
	return Object->GetName() && Object->GetName()[0];
}

static void SortObjects(ObjectList& List)
{
	List.sort([](CCustomObject* A, CCustomObject* B)
		{
			if (!HasName(A))
				return false;
			if (!HasName(B))
				return true;

			return NaturalCompare(A->GetName(), B->GetName());
		}
	);
}

// Borderless icon button used for the visibility and lock columns
static bool IconToggle(const char* Icon, bool Active)
{
	ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0, 0, 0, 0));
	ImGui::PushStyleColor(ImGuiCol_Text, ImGui::GetStyleColorVec4(Active ? ImGuiCol_Text : ImGuiCol_TextDisabled));
	const bool Clicked = ImGui::SmallButton(Icon);
	ImGui::PopStyleColor(2);
	return Clicked;
}

UIObjectList::UIObjectList()
{
	VisibilityFilter = EVisibilityFilter::All;
	OnlyCurrentTool = false;
	CurrentClass = OBJCLASS_DUMMY;
	Filter[0] = 0;
	AnchorRow = -1;
	CursorRow = -1;
	ScrollTarget = nullptr;
	ScrollCentered = true;
	LastSingleSelection = nullptr;
	RenameObject = nullptr;
	RenameFocus = false;
	RenameBuffer[0] = 0;
	NeedRebuild = true;
}

UIObjectList::~UIObjectList()
{
}

void UIObjectList::Draw()
{
	ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(300, 300));

	if (!ImGui::Begin("Outliner", &bOpen))
	{
		ImGui::PopStyleVar(1);
		ImGui::End();
		return;
	}

	IsDocked = ImGui::IsWindowDocked();
	IsFocused = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);

	{
		xrCriticalSectionGuard guard(LoaderCS);

		if (NeedRebuild || (OnlyCurrentTool && LTools->CurrentClassID() != CurrentClass))
		{
			Rebuild();
		}

		DrawToolbar();
		DrawTree();
	}

	ImGui::PopStyleVar(1);
	ImGui::End();
}

void UIObjectList::DrawToolbar()
{
	const float FilterButtonWidth = ImGui::GetFrameHeight() + ImGui::GetStyle().FramePadding.x * 2;

	ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x - FilterButtonWidth - ImGui::GetStyle().ItemSpacing.x);
	if (ImGui::InputTextWithHint("##Search", ICON_FA_MAGNIFYING_GLASS " Search...", Filter, sizeof(Filter)))
	{
		AnchorRow = -1;
	}

	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_FILTER, ImVec2(FilterButtonWidth, 0)))
	{
		ImGui::OpenPopup("OutlinerFilter");
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("Filter options");
	}

	if (ImGui::BeginPopup("OutlinerFilter"))
	{
		if (ImGui::RadioButton("All objects", VisibilityFilter == EVisibilityFilter::All))
			VisibilityFilter = EVisibilityFilter::All;
		if (ImGui::RadioButton("Visible only", VisibilityFilter == EVisibilityFilter::Visible))
			VisibilityFilter = EVisibilityFilter::Visible;
		if (ImGui::RadioButton("Hidden only", VisibilityFilter == EVisibilityFilter::Hidden))
			VisibilityFilter = EVisibilityFilter::Hidden;

		ImGui::Separator();
		if (ImGui::Checkbox("Current tool only", &OnlyCurrentTool))
		{
			NeedRebuild = true;
		}

		ImGui::Separator();
		if (ImGui::MenuItem("Expand all"))
		{
			CollapsedFolders.clear();
		}
		if (ImGui::MenuItem("Collapse all"))
		{
			for (const Folder& F : Folders)
				CollapsedFolders.insert(F.ClassID);
			ExpandedNodes.clear();
		}
		ImGui::EndPopup();
	}
}

void UIObjectList::DrawTree()
{
	TrackExternalSelection();

	const bool Filtering = Filter[0] || VisibilityFilter != EVisibilityFilter::All;
	BuildRows(Filtering);
	HandleKeyboard(Filtering);

	int ScrollRow = -1;
	if (ScrollTarget)
	{
		for (int i = 0; i < (int)Rows.size(); i++)
		{
			if (Rows[i].ObjectNode && Rows[i].ObjectNode->Object == ScrollTarget)
			{
				ScrollRow = i;
				break;
			}
		}
		if (ScrollRow < 0)
		{
			ScrollTarget = nullptr;
		}
	}

	const ImGuiTableFlags Flags = ImGuiTableFlags_BordersV | ImGuiTableFlags_BordersOuterH | ImGuiTableFlags_Resizable | ImGuiTableFlags_RowBg | ImGuiTableFlags_NoBordersInBody | ImGuiTableFlags_ScrollY;
	if (ImGui::BeginTable("Outliner", 4, Flags, ImVec2(0, -ImGui::GetFrameHeightWithSpacing())))
	{
		const float IconColumnWidth = ImGui::GetFrameHeight();
		ImGui::TableSetupScrollFreeze(0, 1);
		ImGui::TableSetupColumn(ICON_FA_EYE, ImGuiTableColumnFlags_WidthFixed | ImGuiTableColumnFlags_NoResize, IconColumnWidth);
		ImGui::TableSetupColumn(ICON_FA_LOCK, ImGuiTableColumnFlags_WidthFixed | ImGuiTableColumnFlags_NoResize, IconColumnWidth);
		ImGui::TableSetupColumn("Label", ImGuiTableColumnFlags_WidthStretch | ImGuiTableColumnFlags_NoHide);
		ImGui::TableSetupColumn("Type", ImGuiTableColumnFlags_WidthFixed, 110.0f);
		ImGui::TableHeadersRow();

		ImGuiListClipper Clipper;
		Clipper.Begin((int)Rows.size());
		if (ScrollRow >= 0)
		{
			Clipper.IncludeItemByIndex(ScrollRow);
		}

		while (Clipper.Step())
		{
			for (int i = Clipper.DisplayStart; i < Clipper.DisplayEnd; i++)
			{
				ImGui::PushID(i);
				if (Rows[i].ObjectNode)
				{
					DrawObjectRow(Rows[i], i, Filtering);
					if (i == ScrollRow)
					{
						ScrollToCurrentRow();
						ScrollTarget = nullptr;
					}
				}
				else
				{
					DrawFolderRow(*Rows[i].OwnerFolder, Filtering);
				}
				ImGui::PopID();
			}
		}

		// Dropping on empty space takes the objects out of their groups
		DrawEmptySpaceDropTarget();

		// Click on empty space clears the selection
		if (ImGui::IsWindowHovered() && ImGui::IsMouseClicked(ImGuiMouseButton_Left) && !ImGui::IsAnyItemHovered() && !ImGui::GetIO().KeyCtrl)
		{
			Scene->SelectObjects(false, OBJCLASS_DUMMY);
			AnchorRow = -1;
			EContext.UI->RedrawScene();
		}

		ImGui::EndTable();
	}

	u32 Total = 0;
	u32 Selected = 0;
	for (const Folder& F : Folders)
	{
		for (const Node& N : F.Nodes)
		{
			Total++;
			Selected += N.Object->Selected() ? 1 : 0;
		}
	}
	ImGui::TextDisabled("%u objects (%u selected)", Total, Selected);
}

void UIObjectList::DrawFolderRow(Folder& F, bool Filtering)
{
	ImGui::TableNextRow();

	bool AnyVisible = false;
	bool AllLocked = !F.Nodes.empty();
	for (const Node& N : F.Nodes)
	{
		AnyVisible |= N.Object->Visible();
		AllLocked &= N.Object->Locked();
	}

	ImGui::TableNextColumn();
	if (IconToggle(AnyVisible ? ICON_FA_EYE : ICON_FA_EYE_SLASH, AnyVisible))
	{
		for (Node& N : F.Nodes)
			ShowNode(N, !AnyVisible);
		EContext.UI->RedrawScene();
	}

	ImGui::TableNextColumn();
	if (IconToggle(AllLocked ? ICON_FA_LOCK : ICON_FA_LOCK_OPEN, AllLocked))
	{
		for (Node& N : F.Nodes)
			LockNode(N, !AllLocked);
		EContext.UI->RedrawScene();
	}

	ImGui::TableNextColumn();
	const bool Collapsed = !Filtering && CollapsedFolders.contains(F.ClassID);
	ImGui::SetNextItemOpen(!Collapsed);

	string256 Label;
	xr_sprintf(Label, "%s %s", Collapsed ? ICON_FA_FOLDER : ICON_FA_FOLDER_OPEN, F.Name.c_str());
	const bool Open = ImGui::TreeNodeEx(Label, ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_NoTreePushOnOpen | ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_OpenOnDoubleClick);

	if (!Filtering && ImGui::IsItemToggledOpen())
	{
		if (Open)
			CollapsedFolders.erase(F.ClassID);
		else
			CollapsedFolders.insert(F.ClassID);
	}

	if (ImGui::BeginDragDropTarget())
	{
		if (ImGui::AcceptDragDropPayload(DragPayload))
			DropSelection(nullptr);
		ImGui::EndDragDropTarget();
	}

	// Ctrl+click on a folder selects all of its objects
	if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen() && ImGui::GetIO().KeyCtrl)
	{
		for (Node& N : F.Nodes)
		{
			if (PassesFilter(N.Object))
				N.Object->Select(true);
		}
		EContext.UI->RedrawScene();
	}

	ImGui::TableNextColumn();
	ImGui::TextDisabled("%u", (u32)F.Nodes.size());
}

void UIObjectList::DrawObjectRow(const Row& R, int RowIndex, bool Filtering)
{
	CCustomObject* Object = R.ObjectNode->Object;
	ImGui::TableNextRow();

	ImGui::TableNextColumn();
	if (IconToggle(Object->Visible() ? ICON_FA_EYE : ICON_FA_EYE_SLASH, Object->Visible()))
	{
		const bool Show = !Object->Visible();
		if (Object->Selected() && !Show)
			ShowSelected(false);
		else
			ShowNode(*R.ObjectNode, Show);
		EContext.UI->RedrawScene();
	}

	ImGui::TableNextColumn();
	if (IconToggle(Object->Locked() ? ICON_FA_LOCK : ICON_FA_LOCK_OPEN, Object->Locked()))
	{
		if (Object->Selected())
			LockSelected(!Object->Locked());
		else
			LockNode(*R.ObjectNode, !Object->Locked());
		EContext.UI->RedrawScene();
	}

	ImGui::TableNextColumn();
	const float Indent = ImGui::GetStyle().IndentSpacing * R.Depth;
	if (Indent > 0)
		ImGui::Indent(Indent);

	ImGuiTreeNodeFlags Flags = ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_NoTreePushOnOpen;
	const bool HasChildren = !R.ObjectNode->Children.empty();
	if (HasChildren)
	{
		Flags |= ImGuiTreeNodeFlags_OpenOnArrow;
		ImGui::SetNextItemOpen(Filtering || ExpandedNodes.contains(Object));
	}
	else
	{
		Flags |= ImGuiTreeNodeFlags_Leaf;
	}
	if (Object->Selected())
	{
		Flags |= ImGuiTreeNodeFlags_Selected;
	}

	if (!Object->Visible())
		ImGui::PushStyleColor(ImGuiCol_Text, ImGui::GetStyleColorVec4(ImGuiCol_TextDisabled));

	string256 Label;
	const bool Renaming = RenameObject == Object;
	const float RenameOffset = ImGui::GetCursorPosX() + ImGui::GetTreeNodeToLabelSpacing() + ImGui::CalcTextSize(ClassIcon(Object->FClassID)).x + ImGui::CalcTextSize(" ").x;
	xr_sprintf(Label, "%s %s", ClassIcon(Object->FClassID), Renaming ? "" : (HasName(Object) ? Object->GetName() : "<unnamed>"));
	const bool Open = ImGui::TreeNodeEx(Label, Flags);

	if (!Object->Visible())
		ImGui::PopStyleColor();

	if (Renaming)
	{
		DrawRenameField(RenameOffset);
	}

	if (HasChildren && !Filtering && ImGui::IsItemToggledOpen())
	{
		if (Open)
			ExpandedNodes.insert(Object);
		else
			ExpandedNodes.erase(Object);
	}

	if (ImGui::IsItemClicked(ImGuiMouseButton_Left) && !ImGui::IsItemToggledOpen())
	{
		ClickObject(Object, RowIndex);
	}
	if (!Renaming && ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left) && !ImGui::IsItemToggledOpen())
	{
		BeginRename(Object);
	}

	if (!Renaming && ImGui::BeginDragDropSource())
	{
		if (!Object->Selected())
			ClickObject(Object, RowIndex);

		ImGui::SetDragDropPayload(DragPayload, nullptr, 0);
		ImGui::Text("%s %s", ClassIcon(Object->FClassID), Object->GetName());
		ImGui::EndDragDropSource();
	}

	if (ImGui::BeginDragDropTarget())
	{
		if (ImGui::AcceptDragDropPayload(DragPayload))
		{
			CGroupObject* Group = smart_cast<CGroupObject*>(Object);
			if (!Group)
				Group = smart_cast<CGroupObject*>(Object->GetOwner());
			DropSelection(Group);
		}
		ImGui::EndDragDropTarget();
	}
	if (ImGui::IsItemClicked(ImGuiMouseButton_Right) && !Object->Selected())
	{
		ClickObject(Object, RowIndex);
	}

	if (ImGui::BeginPopupContextItem("ObjectContext"))
	{
		DrawContextMenu();
		ImGui::EndPopup();
	}

	if (Indent > 0)
		ImGui::Unindent(Indent);

	ImGui::TableNextColumn();
	const char* RefName = Object->RefName();
	ImGui::TextDisabled("%s", RefName && RefName[0] ? RefName : R.OwnerFolder->Name.c_str());
	if (RefName && RefName[0] && ImGui::IsItemHovered())
	{
		ImGui::SetTooltip("%s", RefName);
	}
}

void UIObjectList::DrawContextMenu()
{
	if (ImGui::MenuItem(ICON_FA_CROSSHAIRS " Focus", "F"))
		FocusSelected();
	if (ImGui::MenuItem(ICON_FA_PEN " Rename", "F2"))
		BeginRename(LastSingleSelection);

	ImGui::Separator();
	if (ImGui::MenuItem(ICON_FA_EYE " Show"))
		ShowSelected(true);
	if (ImGui::MenuItem(ICON_FA_EYE_SLASH " Hide"))
		ShowSelected(false);

	ImGui::Separator();
	if (ImGui::MenuItem(ICON_FA_LOCK " Lock"))
		LockSelected(true);
	if (ImGui::MenuItem(ICON_FA_LOCK_OPEN " Unlock"))
		LockSelected(false);
}

void UIObjectList::ClickObject(CCustomObject* Object, int RowIndex)
{
	const ImGuiIO& IO = ImGui::GetIO();

	if (IO.KeyShift && AnchorRow >= 0 && AnchorRow < (int)Rows.size())
	{
		if (!IO.KeyCtrl)
			Scene->SelectObjects(false, OBJCLASS_DUMMY);

		const int From = std::min(AnchorRow, RowIndex);
		const int To = std::max(AnchorRow, RowIndex);
		for (int i = From; i <= To; i++)
		{
			if (Rows[i].ObjectNode)
				Rows[i].ObjectNode->Object->Select(true);
		}
	}
	else if (IO.KeyCtrl)
	{
		Object->Select(-1);
		AnchorRow = RowIndex;
	}
	else
	{
		Scene->SelectObjects(false, OBJCLASS_DUMMY);
		Object->Select(true);
		AnchorRow = RowIndex;
	}

	CursorRow = RowIndex;

	// Selection made here must not scroll the list
	LastSingleSelection = Object;
	ScrollTarget = nullptr;
	EContext.UI->RedrawScene();
}

void UIObjectList::FocusSelected()
{
	Fbox Box;
	Box.invalidate();

	for (const Folder& F : Folders)
	{
		for (const Node& N : F.Nodes)
		{
			Fbox ObjectBox;
			if (N.Object->Selected() && N.Object->GetBox(ObjectBox))
				Box.merge(ObjectBox);
		}
	}

	if (Box.is_valid())
	{
		EContext.UI->CurrentView().m_Camera.ZoomExtents(Box);
		EContext.UI->RedrawScene();
	}
}

void UIObjectList::ShowSelected(bool Flag)
{
	for (Folder& F : Folders)
	{
		for (Node& N : F.Nodes)
		{
			if (N.Object->Selected())
				ShowNode(N, Flag);
		}
	}
	EContext.UI->RedrawScene();
}

// Visibility and lock of a group or sector apply to everything it contains
void UIObjectList::ShowNode(Node& N, bool Flag)
{
	N.Object->Show(Flag);
	for (Node& Child : N.Children)
		ShowNode(Child, Flag);
}

void UIObjectList::LockNode(Node& N, bool Flag)
{
	N.Object->Lock(Flag);
	for (Node& Child : N.Children)
		LockNode(Child, Flag);
}

void UIObjectList::LockSelected(bool Flag)
{
	for (Folder& F : Folders)
	{
		for (Node& N : F.Nodes)
		{
			if (N.Object->Selected())
				LockNode(N, Flag);
		}
	}
	EContext.UI->RedrawScene();
}

// Scroll to an object selected in the viewport and reveal its folder and group
void UIObjectList::TrackExternalSelection()
{
	CCustomObject* Single = nullptr;
	Folder* SingleFolder = nullptr;
	u32 Count = 0;

	for (Folder& F : Folders)
	{
		for (Node& N : F.Nodes)
		{
			if (N.Object->Selected())
			{
				Count++;
				Single = N.Object;
				SingleFolder = &F;
			}
		}
	}

	if (Count != 1)
	{
		if (Count == 0)
			LastSingleSelection = nullptr;
		return;
	}

	if (Single == LastSingleSelection)
	{
		return;
	}

	LastSingleSelection = Single;
	ScrollTarget = Single;
	ScrollCentered = true;
	CollapsedFolders.erase(SingleFolder->ClassID);
}

bool UIObjectList::PassesFilter(CCustomObject* Object) const
{
	switch (VisibilityFilter)
	{
	case EVisibilityFilter::Visible:
		if (!Object->Visible())
			return false;
		break;
	case EVisibilityFilter::Hidden:
		if (Object->Visible())
			return false;
		break;
	default:
		break;
	}

	return ContainsNoCase(HasName(Object) ? Object->GetName() : "", Filter);
}

bool UIObjectList::NodePasses(const Node& N) const
{
	if (PassesFilter(N.Object))
		return true;

	for (const Node& Child : N.Children)
	{
		if (NodePasses(Child))
			return true;
	}
	return false;
}

bool UIObjectList::FolderPasses(const Folder& F) const
{
	for (const Node& N : F.Nodes)
	{
		if (NodePasses(N))
			return true;
	}
	return false;
}

void UIObjectList::BuildRows(bool Filtering)
{
	Rows.clear();

	for (Folder& F : Folders)
	{
		if (Filtering && !FolderPasses(F))
			continue;

		Rows.push_back({ &F, nullptr, 0 });

		if (!Filtering && CollapsedFolders.contains(F.ClassID))
			continue;

		for (Node& N : F.Nodes)
			AppendNodeRows(F, N, 1, Filtering);
	}
}

void UIObjectList::AppendNodeRows(Folder& F, Node& N, int Depth, bool Filtering)
{
	if (Filtering && !NodePasses(N))
		return;

	Rows.push_back({ &F, &N, Depth });

	if (!Filtering && !ExpandedNodes.contains(N.Object))
		return;

	for (Node& Child : N.Children)
		AppendNodeRows(F, Child, Depth + 1, Filtering);
}

void UIObjectList::Rebuild()
{
	NeedRebuild = false;
	Folders.clear();
	Rows.clear();
	AnchorRow = -1;
	CursorRow = -1;
	ScrollTarget = nullptr;
	RenameObject = nullptr;
	CurrentClass = LTools->CurrentClassID();

	for (SceneToolsMapPairIt it = Scene->FirstTool(); it != Scene->LastTool(); ++it)
	{
		if (it->first == OBJCLASS_DUMMY)
			continue;

		ESceneCustomOTool* Tool = smart_cast<ESceneCustomOTool*>(it->second);
		if (!Tool)
			continue;

		if (OnlyCurrentTool && CurrentClass != OBJCLASS_DUMMY && it->first != CurrentClass)
			continue;

		ObjectList List = Tool->GetObjects();
		SortObjects(List);

		Folder F;
		F.ClassID = it->first;
		F.Name = Tool->ClassDesc();

		for (CCustomObject* Object : List)
		{
			// Grouped objects are listed under their group
			if (Object->GetOwner())
				continue;

			Node N;
			N.Object = Object;

			if (CGroupObject* Group = smart_cast<CGroupObject*>(Object))
			{
				ObjectList Members;
				Group->GetObjects(Members);
				SortObjects(Members);
				for (CCustomObject* Member : Members)
					N.Children.push_back({ Member, {} });
			}
			else if (CSector* Sector = smart_cast<CSector*>(Object))
			{
				// A sector holds meshes; list each owning object once
				ObjectList Members;
				for (const CSectorItem& Item : Sector->GetItems())
				{
					if (Item.GetSceneObject() && std::find(Members.begin(), Members.end(), Item.GetSceneObject()) == Members.end())
						Members.push_back(Item.GetSceneObject());
				}
				SortObjects(Members);
				for (CCustomObject* Member : Members)
					N.Children.push_back({ Member, {} });
			}

			F.Nodes.push_back(std::move(N));
		}

		if (!F.Nodes.empty())
			Folders.push_back(std::move(F));
	}
}

void UIObjectList::ScrollToCurrentRow()
{
	if (ScrollCentered)
	{
		ImGui::SetScrollHereY(0.5f);
		return;
	}

	// Keyboard navigation only scrolls when the row leaves the view
	const float RowBottom = ImGui::GetCursorPosY();
	const float RowTop = RowBottom - ImGui::GetFrameHeight();
	const float ViewTop = ImGui::GetScrollY() + ImGui::GetFrameHeightWithSpacing();
	const float ViewBottom = ImGui::GetScrollY() + ImGui::GetWindowHeight();

	if (RowTop < ViewTop)
		ImGui::SetScrollHereY(0.0f);
	else if (RowBottom > ViewBottom)
		ImGui::SetScrollHereY(1.0f);
}

bool UIObjectList::IsRowOpen(const Row& R, bool Filtering) const
{
	if (Filtering)
		return true;
	if (!R.ObjectNode)
		return !CollapsedFolders.contains(R.OwnerFolder->ClassID);
	return ExpandedNodes.contains(R.ObjectNode->Object);
}

bool UIObjectList::HasRowChildren(const Row& R) const
{
	return !R.ObjectNode || !R.ObjectNode->Children.empty();
}

void UIObjectList::SetRowOpen(const Row& R, bool Open)
{
	if (!R.ObjectNode)
	{
		if (Open)
			CollapsedFolders.erase(R.OwnerFolder->ClassID);
		else
			CollapsedFolders.insert(R.OwnerFolder->ClassID);
	}
	else if (Open)
	{
		ExpandedNodes.insert(R.ObjectNode->Object);
	}
	else
	{
		ExpandedNodes.erase(R.ObjectNode->Object);
	}
}

void UIObjectList::MoveCursor(int NewRow, bool Extend)
{
	if (Rows.empty())
		return;

	CursorRow = std::clamp(NewRow, 0, (int)Rows.size() - 1);
	const Row& R = Rows[CursorRow];
	if (!R.ObjectNode)
		return;

	CCustomObject* Object = R.ObjectNode->Object;
	Scene->SelectObjects(false, OBJCLASS_DUMMY);

	if (Extend && AnchorRow >= 0 && AnchorRow < (int)Rows.size())
	{
		const int From = std::min(AnchorRow, CursorRow);
		const int To = std::max(AnchorRow, CursorRow);
		for (int i = From; i <= To; i++)
		{
			if (Rows[i].ObjectNode)
				Rows[i].ObjectNode->Object->Select(true);
		}
	}
	else
	{
		Object->Select(true);
		AnchorRow = CursorRow;
	}

	LastSingleSelection = Object;
	ScrollTarget = Object;
	ScrollCentered = false;
	EContext.UI->RedrawScene();
}

void UIObjectList::HandleKeyboard(bool Filtering)
{
	if (!IsFocused || RenameObject || ImGui::GetIO().WantTextInput || Rows.empty())
		return;

	if (CursorRow >= (int)Rows.size())
		CursorRow = -1;

	if (CursorRow < 0 && LastSingleSelection)
	{
		for (int i = 0; i < (int)Rows.size(); i++)
		{
			if (Rows[i].ObjectNode && Rows[i].ObjectNode->Object == LastSingleSelection)
			{
				CursorRow = i;
				break;
			}
		}
	}

	const bool Shift = ImGui::GetIO().KeyShift;

	if (ImGui::IsKeyPressed(ImGuiKey_DownArrow))
	{
		MoveCursor(CursorRow + 1, Shift);
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_UpArrow))
	{
		MoveCursor(CursorRow < 0 ? 0 : CursorRow - 1, Shift);
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_Home))
	{
		MoveCursor(0, Shift);
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_End))
	{
		MoveCursor((int)Rows.size() - 1, Shift);
	}
	else if (CursorRow >= 0 && ImGui::IsKeyPressed(ImGuiKey_RightArrow))
	{
		const Row& R = Rows[CursorRow];
		if (HasRowChildren(R))
		{
			if (!IsRowOpen(R, Filtering))
				SetRowOpen(R, true);
			else
				MoveCursor(CursorRow + 1, false);
		}
	}
	else if (CursorRow >= 0 && ImGui::IsKeyPressed(ImGuiKey_LeftArrow))
	{
		const Row& R = Rows[CursorRow];
		if (!Filtering && HasRowChildren(R) && IsRowOpen(R, Filtering))
		{
			SetRowOpen(R, false);
		}
		else
		{
			// Jump to the parent row
			for (int i = CursorRow - 1; i >= 0; i--)
			{
				if (Rows[i].Depth < R.Depth)
				{
					MoveCursor(i, false);
					break;
				}
			}
		}
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_F2, false))
	{
		BeginRename(LastSingleSelection);
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_F, false))
	{
		FocusSelected();
	}
}

bool UIObjectList::ConsumesShortcut(u32 Key)
{
	if (!Form || !Form->IsFocused || EContext.UI->ActiveTabIndex != 0)
		return false;

	switch (Key)
	{
	case SDL_SCANCODE_UP:
	case SDL_SCANCODE_DOWN:
	case SDL_SCANCODE_LEFT:
	case SDL_SCANCODE_RIGHT:
	case SDL_SCANCODE_F2:
	case SDL_SCANCODE_F:
	case SDL_SCANCODE_RETURN:
		return true;
	default:
		return false;
	}
}

void UIObjectList::BeginRename(CCustomObject* Object)
{
	if (!Object)
		return;

	RenameObject = Object;
	RenameFocus = true;
	xr_strcpy(RenameBuffer, HasName(Object) ? Object->GetName() : "");
}

void UIObjectList::DrawRenameField(float OffsetX)
{
	ImGui::SameLine(OffsetX);
	ImGui::SetNextItemWidth(-1);
	if (RenameFocus)
	{
		ImGui::SetKeyboardFocusHere();
		RenameFocus = false;
	}

	if (ImGui::InputText("##Rename", RenameBuffer, sizeof(RenameBuffer), ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_AutoSelectAll))
	{
		CommitRename();
	}
	else if (ImGui::IsItemDeactivated())
	{
		// Escape cancels, clicking elsewhere keeps the new name
		if (!ImGui::IsKeyPressed(ImGuiKey_Escape))
			CommitRename();
		RenameObject = nullptr;
	}
}

void UIObjectList::CommitRename()
{
	CCustomObject* Object = RenameObject;
	RenameObject = nullptr;
	if (!Object)
		return;

	xr_string NewName = RenameBuffer;
	xr_strlwr(NewName);
	if (NewName.empty() || NewName == Object->GetName())
		return;

	if (Scene->FindObjectByName(NewName.c_str(), Object))
	{
		ELog.Msg(mtError, "Object with name '%s' already exists", NewName.c_str());
		return;
	}

	Object->SetName(NewName.c_str());
	Scene->UndoSave();
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	NeedRebuild = true;
}

void UIObjectList::CollectSelected(const Node& N, ObjectList& List)
{
	if (N.Object->Selected() && std::find(List.begin(), List.end(), N.Object) == List.end())
		List.push_back(N.Object);

	for (const Node& Child : N.Children)
		CollectSelected(Child, List);
}

void UIObjectList::DrawEmptySpaceDropTarget()
{
	const ImVec2 Min = ImGui::GetCursorScreenPos();
	const ImVec2 Avail = ImGui::GetContentRegionAvail();
	if (Avail.y <= 0)
		return;

	const ImRect Rect(Min, ImVec2(Min.x + ImGui::GetWindowWidth(), Min.y + Avail.y));
	if (ImGui::BeginDragDropTargetCustom(Rect, ImGui::GetID("OutlinerEmpty")))
	{
		if (ImGui::AcceptDragDropPayload(DragPayload))
			DropSelection(nullptr);
		ImGui::EndDragDropTarget();
	}
}

// Moves the selection into Target, or out of its groups when Target is null
void UIObjectList::DropSelection(CGroupObject* Target)
{
	ObjectList Selected;
	for (const Folder& F : Folders)
	{
		for (const Node& N : F.Nodes)
			CollectSelected(N, Selected);
	}

	bool Changed = false;
	if (Target)
	{
		ObjectList ToAttach;
		for (CCustomObject* Object : Selected)
		{
			if (Object != Target && Object->GetOwner() != Target && !smart_cast<CGroupObject*>(Object))
				ToAttach.push_back(Object);
		}

		if (!ToAttach.empty())
		{
			Target->AttachObjects(ToAttach);
			ExpandedNodes.insert(Target);
			Changed = true;
		}
	}
	else
	{
		for (CCustomObject* Object : Selected)
		{
			if (CGroupObject* Group = smart_cast<CGroupObject*>(Object->GetOwner()))
			{
				Group->DetachObject(Object);
				Changed = true;
			}
		}
	}

	if (Changed)
	{
		Scene->UndoSave();
		NeedRebuild = true;
		EContext.UI->RedrawScene();
	}
}

void UIObjectList::Update()
{
	if (Form)
	{
		if (!Form->IsClosed())
		{
			// The outliner belongs to the scene workspace only
			if (EContext.UI->ActiveTabIndex != 0)
			{
				return;
			}

			Form->BeginDraw();
			Form->Draw();
			Form->EndDraw();
		}
		else
		{
			xr_delete(Form);
		}
	}
}

void UIObjectList::Show()
{
	if (Form == nullptr)
	{
		Form = new UIObjectList();
	}

	if (!Form->bOpen)
	{
		Refresh();
	}
}

void UIObjectList::Close()
{
	xr_delete(Form);
}

void UIObjectList::Refresh()
{
	if (Form == nullptr)
		return;

	xrCriticalSectionGuard guard(Form->LoaderCS);
	Form->NeedRebuild = true;
}
