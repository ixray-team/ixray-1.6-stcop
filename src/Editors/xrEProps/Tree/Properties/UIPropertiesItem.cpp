#include "stdafx.h"


UIPropertiesItem::UIPropertiesItem(shared_str Name, UIPropertiesForm* propertiesFrom):UITreeItem(Name),PropertiesFrom(propertiesFrom)
{
	PItem = nullptr;
}

UIPropertiesItem::~UIPropertiesItem()
{
}

void UIPropertiesItem::Draw()
{
	ImGui::TableNextRow();
	ImGui::TableNextColumn();
	if (PItem&&PItem->m_Flags.test(PropItem::flShowCB))
	{
		const char* CheckName = make_string<const char*>("##value_%s", PItem->Key());
		if (ImGui::CheckboxFlags(CheckName, &PItem->m_Flags.flags, PropItem::flCBChecked))
		{
			PItem->OnChange();
			PropertiesFrom->Modified();
		}
		ImGui::SameLine(0, 2);
	}
	if (Items.size())
	{
		ImGuiTreeNodeFlags FloderFlags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_DefaultOpen;
		if (IsSelect)
		{
			ImVec4 TextColor = ImGui::GetStyle().Colors[ImGuiCol_Text];
			TextColor.x = 1;
			TextColor.y = 0.1;
			TextColor.z = 0.1;
			TextColor.w = 0.7f;

			ImGui::PushStyleColor(ImGuiCol_Text, TextColor);
		}

		bool open = ImGui::TreeNodeEx(Name.c_str(), FloderFlags);

		if (IsSelect)
		{
			ImGui::PopStyleColor();
		}
		ImGui::TableNextColumn();
		DrawItem();
		if (open)
		{
			for (UITreeItem* Item : Items)
			{
				static_cast<UIPropertiesItem*>(Item)->Draw();
			}
			ImGui::TreePop();
		}
	}
	else
	{
		constexpr size_t Flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_Bullet | ImGuiTreeNodeFlags_NoTreePushOnOpen | ImGuiTreeNodeFlags_SpanFullWidth;
		if (IsTexture)
		{
			MultiChooseValue* Prop = (MultiChooseValue*)PItem->GetFrontValue();
			ImTextureID Image = GUIManager->LoadTexture(Prop->Values[0]->GetValue().c_str());
			ImGui::Image(Image, { 100, 100 });
			//ImGui::TreeNodeEx((xr_string("##tree_") + *Name).c_str(), Flags);
		}
		else
		{
			ImGui::TreeNodeEx(*Name, Flags);
		}

		ImGui::TableNextColumn();
		DrawItem();
	}
}

void UIPropertiesItem::DrawRoot()
{
	VERIFY(PItem == nullptr);
	for (UITreeItem* Item : Items)
	{
		static_cast<UIPropertiesItem*>(Item)->Draw();
	}
}

void UIPropertiesItem::DrawItem()
{
	if (!PItem)return;

	EPropType type = PItem->Type();
	//
	switch (type)
	{
	case PROP_CANVAS:
	{
		if (PItem->m_Flags.test(PropItem::flMixed))
		{
			ImGui::TextDisabled(PItem->GetDrawText().c_str());

		}
		else
		{
			ImGui::PushItemWidth(-1);
			CanvasValue* val = dynamic_cast<CanvasValue*>(PItem->GetFrontValue()); R_ASSERT(val);
			if (!val->OnDrawCanvasEvent.empty())
				val->OnDrawCanvasEvent(val);
		}
	}
	break;
	case PROP_BUTTON:

		if (PItem->m_Flags.test(PropItem::flMixed))
		{
			ImGui::TextDisabled(PItem->GetDrawText().c_str());

		}
		else
		{
			ImGui::PushID(Name.c_str());
			bool bRes = false;
			bool bSafe = false;
			ButtonValue* V = dynamic_cast<ButtonValue*>(PItem->GetFrontValue()); R_ASSERT(V);
			if (!V->value.empty())
			{
				ImGui::PushItemWidth(-1);
				float size = float(ImGui::CalcItemWidth());
				float dx = floorf(size / float(V->value.size()));
				float offset = size - (dx * V->value.size());
				V->btn_num = V->value.size();
				for (RStringVecIt it = V->value.begin(); it != V->value.end(); it++)
				{

					int k = it - V->value.begin();
					if (ImGui::Button(it->c_str(), ImVec2(dx + offset, 0)))
					{
						V->btn_num = k;

						bRes |= V->OnBtnClick(bSafe);
					}
					offset = 0;
					ImGui::SameLine(0, 2);
				}
			}
			else
			{
				ImGui::Text("");
			}
			ImGui::PopID();
		}
		break;
	case PROP_WAVE:
	case PROP_UNDEF:
		break;
	case PROP_CAPTION:
	{
		ImGui::TextDisabled(PItem->GetDrawText().c_str());
	}
	break;

	default:
		ImGui::PushID(Name.c_str());
		if (PropertiesFrom->IsReadOnly())
		{
			if (type == PROP_BOOLEAN)
			{
				FlagValueCustom* V = dynamic_cast<FlagValueCustom*>(PItem->GetFrontValue()); VERIFY(V);
				ImGui::TextDisabled(V->GetValueEx() ? "true" : "false");
			}
			else
			{
				ImGui::TextDisabled(PItem->GetDrawText().c_str());
			}
		}
		else if (PItem->m_Flags.test(PropItem::flMixed) && !PItem->m_Flags.test(PropItem::flIgnoreMixed))
		{
			if (ImGui::Button("(Mixed)", ImVec2(-1, 0)))
			{
				RemoveMixed();
			}
		}
		else
		{
			if (PItem->m_Flags.test(PropItem::flDisabled))
			{
				if (type == PROP_FLAG)
				{
					FlagValueCustom* V = dynamic_cast<FlagValueCustom*>(PItem->GetFrontValue()); VERIFY(V);
					ImGui::TextDisabled(V->GetValueEx() ? "true" : "false");
				}
				else
				{
					ImGui::TextDisabled(PItem->GetDrawText().c_str());
				}
			}
			else
			{
				ImGui::PushItemWidth(-1);
				DrawProp();
			}
		}
		ImGui::PopID();
		break;
	}
}

UITreeItem* UIPropertiesItem::CreateItem(shared_str Name)
{
	return new UIPropertiesItem(Name,PropertiesFrom);
}
