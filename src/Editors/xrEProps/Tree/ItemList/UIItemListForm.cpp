#include "stdafx.h"
#include "IconsFontAwesome6.h"

UIItemListForm::UIItemListForm()
{
	m_Flags.zero();
	m_UseMenuEdit = false;
	m_Filter = "";
}

UIItemListForm::~UIItemListForm()
{
	ClearList();
}

void UIItemListForm::Draw()
{
	m_UseMenuEdit = false;

	if (!m_Filter.empty())
	{
		ResetAutoExpand(&m_GeneralNode);
		SetAutoExpandForFilter(&m_GeneralNode);
	}

	ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 0));
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 3));
	DrawMenuEdit();
	DrawNode(&m_GeneralNode);
	ImGui::PopStyleVar(2);
	if (!m_UseMenuEdit)
	{
		m_edit_node = nullptr;
	}
}

void UIItemListForm::DrawNode(Node* N)
{
	if (N->Type == FNT_Root)
	{
		for (Node& Node : N->Nodes)
		{
			if (IsNodeTrueFolder(Node) && IsDrawFolder(&Node))
			{
				DrawNode(&Node);
				ImGui::Separator();
			}
		}

		for (Node& Node : N->Nodes)
		{
			if (!IsNodeTrueFolder(Node))
			{
				if (Node.Object && Node.Object->Visible())
				{
					if (m_Filter.empty())
					{
						DrawNode(&Node);
						ImGui::Separator();
					}
					else
					{
						xr_string NodeNameLower = Node.Name.c_str();
						xr_string FilterLower = m_Filter.c_str();
						xr_strlwr(NodeNameLower);
						xr_strlwr(FilterLower);

						if (NodeNameLower.Contains(FilterLower))
						{
							DrawNode(&Node);
							ImGui::Separator();
						}
					}
				}
			}
		}
	}
	else if (N->IsFolder())
	{
		if (N->Selected || N->AutoExpand)
		{
			ImGui::SetNextItemOpen(true);
		}

		ImGui::AlignTextToFramePadding();
		ImGuiTreeNodeFlags FolderFlags = ImGuiTreeNodeFlags_OpenOnArrow;

		if (IsFolderBullet(N))
		{
			FolderFlags |= ImGuiTreeNodeFlags_Bullet;
		}

		if (IsFolderSelected(N))
		{
			FolderFlags |= ImGuiTreeNodeFlags_Selected;
		}

		if (N->Icon.size() > 0)
		{
			ImGui::Text(N->Icon.c_str());
			ImGui::SameLine();
		}

		xr_string builder = N->Prefix.c_str();
		builder.append(N->Name.c_str());

		if (ImGui::TreeNodeEx(builder.c_str(), FolderFlags))
		{
			DrawAfterFolderNode(true, N);
			if (ImGui::IsItemClicked() && N->Object)
			{
				IsItemClicked(N);
			}

			for (Node& Node : N->Nodes)
			{
				if (IsNodeTrueFolder(Node) && IsDrawFolder(&Node))
				{
					ImGui::Separator();
					DrawNode(&Node);
				}
			}

			for (Node& Node : N->Nodes)
			{
				if (!IsNodeTrueFolder(Node))
				{
					if (Node.Object && Node.Object->Visible())
					{
						if (m_Filter.empty())
						{
							ImGui::Separator();
							DrawNode(&Node);
						}
						else
						{
							xr_string NodeNameLower = Node.Name.c_str();
							xr_string FilterLower = m_Filter.c_str();
							xr_strlwr(NodeNameLower);
							xr_strlwr(FilterLower);
							if (NodeNameLower.Contains(FilterLower))
							{
								ImGui::Separator();
								DrawNode(&Node);
							}
						}
					}
				}
			}
			ImGui::TreePop();
		}
		else
		{
			DrawAfterFolderNode(false, N);
			if (ImGui::IsItemClicked() && N->Object)
			{
				IsItemClicked(N);
			}
		}

		N->Selected = false;
	}
	else if (N->IsObject())
	{
		DrawItem(N);
		N->Selected = false;
	}
}

void UIItemListForm::ClearList()
{
	m_GeneralNode = Node();

	ClearSelectedItems();
	for (ListItem* item : m_Items)
	{
		xr_delete(item);
	}
	m_Items.clear();
}

void UIItemListForm::RemoveSelectItem()
{
	if (!m_SelectedItems.size() || m_Flags.test(fMultiSelect))
		return;
	for (auto b = m_Items.begin(), e = m_Items.end(); b != e; b++)
	{
		if (*b == m_SelectedItems.back())
		{
			m_Items.erase(b);
			break;
		}
	}
	m_GeneralNode = Node();
	for (ListItem* item : m_Items)
	{
		Node* N = AppendObject(&m_GeneralNode, item->Key());
		VERIFY(N);
		N->Object = item;
		N->Icon = item->Icon();
	}
}

void UIItemListForm::ClearSelected()
{
	ClearSelectedItems();
	if (!OnItemFocusedEvent.empty())
		OnItemFocusedEvent(0);
	if (m_Flags.test(fMultiSelect))
	{
		if (!OnItemsFocusedEvent.empty())
			OnItemsFocusedEvent(m_SelectedItems);
	}
}

void UIItemListForm::SelectItem(const char* name, bool ClearOld)
{
	if (name == nullptr)
		return;

	Node* N = SelectObject(&m_GeneralNode, name);

	R_ASSERT3(N, "Item not found", name);
	if(!N) {
		return;
	}

	if (ClearOld)
	{
		ClearSelectedItems();
	}

	if (m_Flags.test(fMultiSelect))
	{
		N->Object->selected = true;
		if (N)
			m_SelectedItems.push_back(N->Object);
		if (!OnItemFocusedEvent.empty())
			OnItemFocusedEvent(N->Object);
		if (!OnItemsFocusedEvent.empty())
			OnItemsFocusedEvent(m_SelectedItems);
	}
	else
	{
		if (N)
			m_SelectedItems.push_back(N->Object);
		if (!OnItemFocusedEvent.empty())
			OnItemFocusedEvent(N->Object);
	}
}

bool UIItemListForm::GetSelected(RStringVec& items) const
{
	for (ListItem* prop : m_SelectedItems)
	{
		items.push_back(prop->key);
	}
	return true;
}
int UIItemListForm::GetSelected(const char* pref, ListItemsVec& items, bool bOnlyObject)
{
	for (ListItem* prop : m_SelectedItems)
	{
		if (prop && (!bOnlyObject || (bOnlyObject && prop->m_Object)))
		{
			xr_string key = *prop->key;
			if (pref)
			{
				if (0 == key.find(pref))
					items.push_back(prop);
			}
			else
				items.push_back(prop);
		}
	}
	return items.size();
}
void UIItemListForm::AssignItems(ListItemsVec& items, const char* name_selection, bool clear_Folder, bool save_selected)
{
	RStringVec selection_items;

	if (save_selected)
		GetSelected(selection_items);

	ClearList();

	m_Items = items;

	if (!clear_Folder)
	{
		ClearObject(&m_GeneralNode);
	}
	else
	{
		m_GeneralNode = Node();
	}

	for (ListItem* item : m_Items)
	{
		item->Parent = this;
		Node* N = AppendObject(&m_GeneralNode, item->Key());
		VERIFY(N);

		if (N)
		{
			N->Object = item;
			N->Prefix = item->Prefix();
			N->Icon = item->Icon();
		}
	}
	if (name_selection)
	{
		Node* N = SelectObject(&m_GeneralNode, name_selection);
		ClearSelectedItems();
		if (m_Flags.test(fMultiSelect))
		{
			N->Object->selected = true;
		}
		if (N)
			m_SelectedItems.push_back(N->Object);
	}
	if (save_selected)
	{
		for (shared_str& name : selection_items)
		{
			Node* N = Find(&m_GeneralNode, name.c_str());
			if (N)
			{
				if (m_Flags.test(fMultiSelect))
				{
					N->Object->selected = true;
				}
				m_SelectedItems.push_back(N->Object);
			}
		}
	}
}

void UIItemListForm::DrawMenuEdit()
{
	if (ImGui::BeginPopupContextItem("MenuEdit"))
	{
		ImGui::PopStyleVar(2);
		m_UseMenuEdit = true;

		Node* N = m_edit_node == 0 ? &m_GeneralNode : m_edit_node;
		
		if (VerifyItemCreateFunc(N))
		{
			if (ImGui::MenuItem("Create"))
			{
				for (int i = 0; i < 256; i++)
				{
					string4096 name;
					if (i == 0)
						xr_strcpy(name, "new");
					else
						xr_sprintf(name, "new_%d", i);

					string_path path;
					path[0] = 0;
					if (N->Path.c_str() && N->Path.c_str()[0])
					{
						xr_strcpy(path, N->Path.c_str());
					}
					if (N->IsFolder() && N->Name.c_str() && N->Name.c_str()[0])
					{
						if (path[0])
							xr_strcat(path, "\\");
						xr_strcat(path, N->Name.c_str());
					}
					if (path[0])
						xr_strcat(path, "\\");
					xr_strcat(path, name);
					if (!Find(&m_GeneralNode, path))
					{
						OnItemCreateEvent(path);
						ImGui::CloseCurrentPopup();
						m_edit_node = nullptr;
						break;
					}
				}
			}
			if (ImGui::IsItemHovered())
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		}
		if (VerifyItemCloneFunc(N))
		{
			if (ImGui::MenuItem("Clone"))
			{
				string_path parent_path;
				parent_path[0] = 0;
				Node* N = m_edit_node == 0 ? &m_GeneralNode : m_edit_node;
				GetFullPath(N, parent_path);
				for (int i = 0; i < 256; i++)
				{
					string4096 name;
					if (i == 0)
						xr_sprintf(name, "%s_clone", m_edit_node->Name.c_str());
					else
						xr_sprintf(name, "%s_clone_%d", m_edit_node->Name.c_str(), i);

					Node* N = m_edit_node == 0 ? &m_GeneralNode : m_edit_node;
					string_path path;
					path[0] = 0;
					if (N->Path.c_str() && N->Path.c_str()[0])
					{
						xr_strcpy(path, N->Path.c_str());
					}
					if (IsNodeTrueFolder(*m_edit_node))
					{
						if (path[0])
							xr_strcat(path, "\\");
						xr_strcat(path, m_edit_node->Name.c_str());
					}
					if (path[0])
						xr_strcat(path, "\\");
					xr_strcat(path, name);
					if (!Find(&m_GeneralNode, path))
					{
						OnItemCloneEvent(parent_path, path);
						ImGui::CloseCurrentPopup();
						m_edit_node = nullptr;
						break;
					}
				}
			}
			if (ImGui::IsItemHovered())
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			if (!OnItemCreateEvent.empty() || !OnItemCloneEvent.empty())
			{
				ImGui::Separator();
			}
		}

		if (VerifyFolderCreateFunc(N))
		{
			if (ImGui::MenuItem("Create Folder"))
			{
				for (int i = 0; i < 256; i++)
				{
					string4096 full_path, name;
					if (i == 0)
						xr_strcpy(name, "new_Folder");
					else
						xr_sprintf(name, "new_Folder_%d", i);

					if (N->Path.c_str() && N->Path.c_str()[0])
						xr_strcpy(full_path, N->Path.c_str());
					else
						full_path[0] = 0;
					if (N->IsFolder() && N->Name.c_str() && N->Name.c_str()[0])
					{
						if (full_path[0])
							xr_strcat(full_path, "\\");
						xr_strcat(full_path, N->Name.c_str());
					}
					if (full_path[0])
						xr_strcat(full_path, "\\");
					xr_strcat(full_path, name);

					if (!FindFolder(&m_GeneralNode, full_path) && AppendFolder(&m_GeneralNode, full_path))
					{
						ImGui::CloseCurrentPopup();
						m_edit_node = nullptr;
						break;
					}
				}
			}
		}
		if (ImGui::IsItemHovered())
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		if (m_edit_node && m_edit_node != &m_GeneralNode)
		{
			ImGui::Separator();
			if (VerifyItemRenameFunc(N))
			{
				if (ImGui::BeginMenu("Rename"))
				{
					ImGui::InputText("New Name", m_edit_name, sizeof(m_edit_name));
					if (ImGui::Button("Ok"))
					{
						string4096 full_path;
						if (m_edit_node->Path.c_str() && m_edit_node->Path.c_str()[0])
							xr_strcpy(full_path, m_edit_node->Path.c_str());
						else
							full_path[0] = 0;

						if (full_path[0])
							xr_strcat(full_path, "\\");
						xr_strcat(full_path, m_edit_name);
						if (Move(&m_GeneralNode, m_edit_node, full_path))
						{
							ImGui::CloseCurrentPopup();
							m_edit_node = nullptr;
						}
					}
					if (ImGui::IsItemHovered())
						ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					ImGui::SameLine();
					if (ImGui::Button("Cancel"))
					{
						ImGui::CloseCurrentPopup();
					}
					if (ImGui::IsItemHovered())
						ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
					ImGui::EndMenu();
				}
			}
			if (ImGui::IsItemHovered())
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			if (VerifyItemMoveFunc(N))
			{
				if (ImGui::BeginMenu("Move"))
				{
					auto Action = ItemMoveActionSlots.find(GetItemMoveActionSlot.empty() ? ENodeMoveActionSlot::Default : GetItemMoveActionSlot(N));
					R_ASSERT(Action != ItemMoveActionSlots.end());
					if (Action->second(N))
					{
						m_edit_node = nullptr;
					}
					ImGui::EndMenu();
				}
			}
			if (ImGui::IsItemHovered())
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			if (ImGui::MenuItem("Delete"))
			{
				Remove(&m_GeneralNode, m_edit_node, true, false);
				ImGui::CloseCurrentPopup();
				m_edit_node = nullptr;
			}
			if (ImGui::IsItemHovered())
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		}

		ImGui::EndPopup();
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 0));
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 3));
	}
}

bool UIItemListForm::ItemMoveActionDefault(Node* Node)
{
	bool IsProcessed = false;
	ImGui::InputText("New Path", m_edit_path, sizeof(m_edit_path));
	if (ImGui::Button("Ok"))
	{
		string4096 full_path;
		xr_strcpy(full_path, m_edit_path);
		if (m_edit_path[0])
		{
			xr_strcat(full_path, "\\");
		}
		xr_strcat(full_path, m_edit_node->Name.c_str());
		if (Move(&m_GeneralNode, m_edit_node, full_path))
		{
			ImGui::CloseCurrentPopup();
			IsProcessed = true;
		}
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
	}
	ImGui::SameLine();
	if (ImGui::Button("Cancel"))
	{
		ImGui::CloseCurrentPopup();
	}
	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
	}
	return IsProcessed;
}

void UIItemListForm::DrawAfterFolderNode(bool is_open, Node* Node)
{
	if (m_Flags.is(fMenuEdit))
	{
		if (ImGui::OpenPopupOnItemClick2("MenuEdit", 1))
		{
			m_UseMenuEdit = true;
			m_edit_node = Node;
			xr_strcpy(m_edit_path, Node->Path.c_str());
			xr_strcpy(m_edit_name, Node->Name.c_str());
		}
	}
	if (is_open && m_Flags.is(fMenuEdit))
	{
		DrawMenuEdit();
	}
}

void UIItemListForm::DrawItem(Node* Node)
{
	if (!Node->Object->Visible())
		return;

	if (!m_Filter.empty())
	{
		xr_string NodeNameLower = Node->Name.c_str();
		xr_string FilterLower = m_Filter.c_str();
		xr_strlwr(NodeNameLower);
		xr_strlwr(FilterLower);
		if (!NodeNameLower.Contains(FilterLower))
			return;
	}

	ImGuiTreeNodeFlags Flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
	if (m_Flags.test(fMultiSelect))
	{
		if (Node->Object && Node->Object->selected)
			Flags |= ImGuiTreeNodeFlags_Bullet;
		if (m_SelectedItems.size() && m_SelectedItems.back() == Node->Object)
			Flags |= ImGuiTreeNodeFlags_Selected;
	}
	else
	{
		if (m_SelectedItems.size() && m_SelectedItems.back() == Node->Object)
			Flags |= ImGuiTreeNodeFlags_Selected;
	}
	if (m_edit_node == Node)
		Flags |= ImGuiTreeNodeFlags_Selected;

	if (Node->Icon.size() > 0)
	{
		ImGui::Text(Node->Icon.c_str());
		ImGui::SameLine();
	}
	ImGui::TreeNodeEx(Node->Name.c_str(), Flags);

	if (!OnDrawItemExtraEvent.empty())
	{
		OnDrawItemExtraEvent(*Node);
	}

	if (m_Flags.is(fMenuEdit))
	{
		if (ImGui::OpenPopupOnItemClick2("MenuEdit", 1))
		{
			m_UseMenuEdit = true;
			m_edit_node = Node;
			xr_strcpy(m_edit_path, Node->Path.c_str());
			xr_strcpy(m_edit_name, Node->Name.c_str());
		}
	}

	if (ImGui::IsItemClicked())
	{
		if (m_Flags.test(fMultiSelect))
		{
			if (!ImGui::GetIO().KeyCtrl && !ImGui::GetIO().KeyShift)
			{
				ClearSelectedItems();
			}
			if (Node->Object->selected)
			{
				Node->Object->selected = false;
				auto p = std::find_if(m_SelectedItems.begin(), m_SelectedItems.end(), [&Node](ListItem* a)
					{
						return a == Node->Object;
					});
				VERIFY(p != m_SelectedItems.end());
				m_SelectedItems.erase(p);

				if (!OnItemUnfocusedEvent.empty())
					OnItemUnfocusedEvent(Node->Object);
			}
			else
			{
				if (ImGui::GetIO().KeyShift && !m_SelectedItems.empty())
				{
					ListItem* LastItem = m_SelectedItems.back();
					auto Begin = std::find(m_Items.begin(), m_Items.end(), LastItem);
					auto End = std::find(m_Items.begin(), m_Items.end(), Node->Object);

					if (Begin > End)
					{
						std::swap(Begin, End);
					}

					for (auto Iter = Begin; Iter <= End; ++Iter)
					{
						ListItem* NodeObj = *Iter;

						if (!NodeObj->selected)
						{
							NodeObj->selected = true;
							m_SelectedItems.push_back(NodeObj);
						}

						if (!OnItemFocusedEvent.empty())
						{
							OnItemFocusedEvent(NodeObj);
						}
					}

					if (!OnItemsFocusedEvent.empty())
					{
						OnItemsFocusedEvent(m_SelectedItems);
					}
				}
				else
				{
					Node->Object->selected = true;
					m_SelectedItems.push_back(Node->Object);
					if (!OnItemFocusedEvent.empty())
						OnItemFocusedEvent(Node->Object);
					if (!OnItemsFocusedEvent.empty())
						OnItemsFocusedEvent(m_SelectedItems);
				}
			}
		}
		else
		{
			ClearSelectedItems();
			m_SelectedItems.push_back(Node->Object);
			if (!OnItemFocusedEvent.empty())
				OnItemFocusedEvent(Node->Object);
		}
	}
}

bool UIItemListForm::IsDrawFolder(Node* InputNode)
{
	if (m_Flags.test(fMenuEdit) && !m_Filter.empty())
	{
		bool Result = false;
		for (Node& N : InputNode->Nodes)
		{
			if (N.IsObject())
			{
				if (N.Object && N.Object->Visible())
				{
					xr_string NodeNameLower = N.Name.c_str();
					xr_string FilterLower = m_Filter.c_str();
					xr_strlwr(NodeNameLower);
					xr_strlwr(FilterLower);
					if (NodeNameLower.Contains(FilterLower))
					{
						Result = true;
						break;
					}
				}
			}
			else
			{
				if (IsDrawFolder(&N))
				{
					Result = true;
					break;
				}
			}
		}
		return Result;
	}

	if (InputNode->Object)
	{
		return InputNode->Object->Visible();
	}

	bool Result = m_Flags.test(fMenuEdit);

	for (Node& N : InputNode->Nodes)
	{
		Result = Result || IsDrawFolder(&N);
	}
	return Result;
}

void UIItemListForm::IsItemClicked(Node* Node)
{
	ClearSelectedItems();
	Node->Object->selected = true;
	m_SelectedItems.push_back(Node->Object);
	if (!OnItemFocusedEvent.empty())
		OnItemFocusedEvent(Node->Object);
	if (m_Flags.test(fMultiSelect))
	{
		if (!OnItemsFocusedEvent.empty())
			OnItemsFocusedEvent(m_SelectedItems);
	}
}

bool UIItemListForm::IsFolderBullet(Node* Node)
{
	return false;
}

bool UIItemListForm::IsFolderSelected(Node* Node)
{
	if (m_Flags.test(fMultiSelect))
	{
		return Node->Object && Node->Object->selected;
	}
	else if (m_SelectedItems.size() && m_SelectedItems.back() == Node->Object)
	{
		return true;
	}
	return Node == m_edit_node;
}

bool UIItemListForm::VerifyItemCloneFunc(UIItemListForm::Node* Node)
{
	if (!VerifyItemClone.empty() && !VerifyItemClone(Node))
	{
		return false;
	}
	return !OnItemCloneEvent.empty() && m_edit_node && !IsNodeTrueFolder(*m_edit_node);
}

bool UIItemListForm::VerifyItemCreateFunc(UIItemListForm::Node* Node)
{
	if (!VerifyItemCreate.empty() && !VerifyItemCreate(Node))
	{
		return false;
	}
	return !OnItemCreateEvent.empty();
}

bool UIItemListForm::VerifyFolderCreateFunc(UIItemListForm::Node* Node)
{
	return VerifyFolderCreate.empty() || VerifyFolderCreate(Node);
}

bool UIItemListForm::VerifyItemRenameFunc(UIItemListForm::Node* Node)
{
	return VerifyItemRename.empty() || VerifyItemRename(Node);
}

bool UIItemListForm::VerifyItemMoveFunc(UIItemListForm::Node* Node)
{
	return VerifyItemMove.empty() || VerifyItemMove(Node);
}

void UIItemListForm::EventRenameNode(Node* Node, const char* old_path, const char* new_path)
{
	EItemType type = TYPE_FOLDER;
	if (Node->IsObject())
	{
		type = TYPE_OBJECT;
		Node->Object->key = new_path;
	}
	if (!OnItemRenameEvent.empty())
		OnItemRenameEvent(*Node, old_path, new_path, type);
}

void UIItemListForm::EventRemoveNode(Node* Node, const char* path)
{
	if (!OnItemRemoveEvent.empty())
	{
		OnItemRemoveEvent(*Node);
	}
}

bool UIItemListForm::EventPreRemoveNode(Node* Node)
{
	if (!OnItemPreRemoveEvent.empty())
	{
		return OnItemPreRemoveEvent(*Node);
	}
	return true;
}

void UIItemListForm::ClearSelectedItems()
{
	for (auto ptr : m_SelectedItems)
	{
		ptr->selected = false;
	}
	m_SelectedItems.clear();
}

void UIItemListForm::ClearObject(Node* N)
{
	for (int i = N->Nodes.size() - 1; i >= 0; i--)
	{
		if (N->Nodes[i].IsObject())
		{
			N->Nodes.erase(N->Nodes.begin() + i);
		}
		else
		{
			ClearObject(&N->Nodes[i]);
		}
	}
}

void UIItemListForm::ResetAutoExpand(Node* N)
{
	N->AutoExpand = false;
	for (Node& child : N->Nodes)
	{
		ResetAutoExpand(&child);
	}
}

bool UIItemListForm::SetAutoExpandForFilter(Node* N)
{
	if (N->IsObject())
	{
		return false;
	}

	bool hasMatch = false;
	for (Node& child : N->Nodes)
	{
		if (child.IsObject())
		{
			if (child.Object && child.Object->Visible())
			{
				xr_string NodeNameLower = child.Name.c_str();
				xr_string FilterLower = m_Filter.c_str();
				xr_strlwr(NodeNameLower);
				xr_strlwr(FilterLower);
				if (NodeNameLower.Contains(FilterLower))
				{
					hasMatch = true;
				}
			}
		}
		else
		{
			if (SetAutoExpandForFilter(&child))
			{
				hasMatch = true;
			}
		}
	}

	if (hasMatch)
	{
		N->AutoExpand = true;
	}

	return hasMatch;
}