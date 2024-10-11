#include "stdafx.h"

UIObjectList* UIObjectList::Form = nullptr;
UIObjectList::UIObjectList():m_Root("")
{
	m_Mode = M_Visible;
	m_Filter[0] = 0;
}

UIObjectList::~UIObjectList()
{
}

void UIObjectList::Draw()
{
	ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(400, 400));

	if (!ImGui::Begin("Object List", &bOpen))
	{
		ImGui::PopStyleVar(1);
		ImGui::End();
		return;
	}

	IsDocked = ImGui::IsWindowDocked();
	IsFocused = ImGui::IsWindowFocused();


	{
		ImGui::BeginGroup();
		if (ImGui::RadioButton("All", m_Mode == M_All))
		{
			m_Mode = M_All;
			m_Root.ClearSelcted();
		}
		ImGui::SameLine();
		if (ImGui::RadioButton("Visible Only", m_Mode == M_Visible))
		{
			m_Mode = M_Visible;
			m_Root.ClearSelcted();
		}
		ImGui::SameLine();
		if (ImGui::RadioButton("Invisible Only", m_Mode == M_Inbvisible))
		{
			m_Mode = M_Inbvisible;
			m_Root.ClearSelcted();
		}
		ImGui::Separator();
		
		float BtnWidth = ImGui::GetWindowWidth() / 3 - 20;

		if (ImGui::Button("Focus", ImVec2(BtnWidth, 0)))
		{
			for (UITreeItem* Item : m_Root.Items)
			{
				UIObjectListItem* RItem = (UIObjectListItem*)Item;
				if (RItem->bIsSelected)
				{
					RItem->Object->Select(true);
					Fbox bb;
					if (RItem->Object->GetBox(bb))
						UI->CurrentView().m_Camera.ZoomExtents(bb);

					UI->RedrawScene();

					break;
				}
			}
		}
		ImGui::SameLine();
		if (ImGui::Button("Show", ImVec2(BtnWidth, 0)))
		{
			for (UITreeItem* Item : m_Root.Items)
			{
				UIObjectListItem* RItem = (UIObjectListItem*)Item;
				if (RItem->bIsSelected)
				{
					RItem->Object->Show(true);
				}
			}
		}
		ImGui::SameLine();
		if (ImGui::Button("Hide", ImVec2(BtnWidth, 0)))
		{
			for (UITreeItem* Item : m_Root.Items)
			{
				UIObjectListItem* RItem = (UIObjectListItem*)Item;
				if (RItem->bIsSelected)
				{
					RItem->Object->Show(false);
				}

			}
		}
		ImGui::Separator();

		DrawObjects();
		if (ImGui::InputText("##value", m_Filter, sizeof(m_Filter)))
		{
			m_Root.ClearSelcted();
		}
		ImGui::EndGroup();

	}

	ImGui::PopStyleVar(1);
	ImGui::End();
}

void UIObjectList::Update()
{
	if (Form)
	{
		if (!Form->IsClosed())
		{
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
	if (Form == nullptr)Form = new UIObjectList();
	Refresh();
}

void UIObjectList::Close()
{
	xr_delete(Form);
}

void UIObjectList::Refresh()
{
	if (Form == nullptr)
		return;

	Form->m_Root = UIObjectListItem("");

	Form->m_cur_cls = LTools->CurrentClassID();
	for (SceneToolsMapPairIt it = Scene->FirstTool(); it != Scene->LastTool(); ++it)
	{
		ESceneCustomOTool* ot = dynamic_cast<ESceneCustomOTool*>(it->second);
		if (ot && ((Form->m_cur_cls == OBJCLASS_DUMMY) || (it->first == Form->m_cur_cls)))
		{
			if (it->first == OBJCLASS_DUMMY)
				continue;

			ObjectList lst = ot->GetObjects();
			size_t Index = 0;

			lst.sort([](CCustomObject* A, CCustomObject* B)
				{
					if (A->GetName() == nullptr)
						return false;

					if (B->GetName() == nullptr)
						return true;

					size_t BLen = strlen(B->GetName());
					for (size_t Iter = 0; Iter < strlen(A->GetName()); Iter++)
					{
						if (Iter >= BLen)
							return false;

						if (A->GetName()[Iter] > B->GetName()[Iter])
							return false;
						else if(A->GetName()[Iter] < B->GetName()[Iter])
							return true;
					}
				}
			
			);

			for (CCustomObject* Obj : lst)
			{
				if (Obj->GetName() == 0 || Obj->GetName()[0] == 0)
				{
					continue;
				}
				else
				{
					UIObjectListItem* Item = static_cast<UIObjectListItem*>(Form->m_Root.AppendItem(Obj->GetName(), 0)); 
					VERIFY(Item);

					Item->Object = Obj;
				}
				
			}
			
		}
	}
	Form->m_LastSelected = nullptr;
}

void UIObjectList::DrawObjects()
{
	if (LTools->CurrentClassID() != m_cur_cls)Refresh();

	static ImGuiTableFlags flags = ImGuiTableFlags_BordersV | ImGuiTableFlags_BordersOuterH | ImGuiTableFlags_Resizable | ImGuiTableFlags_RowBg | ImGuiTableFlags_NoBordersInBody| ImGuiTableFlags_ScrollY;

	if (ImGui::BeginTable("objects", 1, flags, ImVec2(0, -ImGui::GetFrameHeight() - 4)))
	{
		IsDocked = ImGui::IsWindowDocked();
		IsFocused = ImGui::IsWindowFocused();

		ImGui::TableSetupScrollFreeze(1, 1);
		ImGui::TableSetupColumn("Label", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableHeadersRow();
		m_Root.DrawRoot();
		ImGui::EndTable();
	}
}


