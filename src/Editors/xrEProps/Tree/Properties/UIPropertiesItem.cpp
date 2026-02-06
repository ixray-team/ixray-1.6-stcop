#include "stdafx.h"
#include "WaveForm.h"

UIPropertiesItem::UIPropertiesItem(shared_str Name, UIPropertiesForm* propertiesFrom):
	UITreeItem(Name),
	PropertiesFrom(propertiesFrom)
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

	if (!Items.empty())
	{
		ImGui::TableSetBgColor(ImGuiTableBgTarget_RowBg0, ImGui::GetColorU32(ImGuiCol_TableRowBgAlt));
		constexpr ImGuiTreeNodeFlags FloderFlags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_AllowOverlap;
		if (IsSelect)
		{
			ImVec4 TextColor = ImGui::GetStyle().Colors[ImGuiCol_Text];
			TextColor.x = 1;
			TextColor.y = 0.1;
			TextColor.z = 0.1;
			TextColor.w = 0.7f;

			ImGui::PushStyleColor(ImGuiCol_Text, TextColor);
		}

		//float LastCursorPosX = ImGui::GetCursorPosX();
		bool open = ImGui::TreeNodeEx((xr_string("##") + *Name).c_str(), FloderFlags);
		
		if (IsSelect)
		{
			ImGui::PopStyleColor();
		}

		ImDrawList* DrawList = ImGui::GetWindowDrawList();
		ImVec2 row_min = ImGui::GetItemRectMin();
		ImVec2 text_pos = row_min;
		text_pos.x += 19;

		DrawList->AddText(text_pos, ImGui::GetColorU32(ImGuiCol_Text), *Name);
		ImGui::TableNextColumn();
		DrawList->AddText(text_pos, ImGui::GetColorU32(ImGuiCol_Text), *Name);

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
		ImGui::TableSetBgColor(ImGuiTableBgTarget_RowBg0, ImGui::GetColorU32(ImGuiCol_TableRowBg));
		if (IsTexture)
		{
			MultiChooseValue* Prop = (MultiChooseValue*)PItem->GetFrontValue();

			if (Prop != nullptr && !Prop->Values.empty())
			{
				shared_str TexName = Prop->Values[0]->value ? Prop->Values[0]->GetValue() : "";
				if (TexName.size() > 0)
				{
					ImTextureID Image = GUIManager->LoadTexture(*TexName);
					ImGui::SetCursorPosX(ImGui::GetCursorPosX() - 12);
					ImGui::Image(Image, { 100, 100 });
				}
			}
		}
		else
		{
			ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 2);
			ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 3);
			ImGui::TextUnformatted(*Name);
			//ImGui::TreeNodeEx(*Name, Flags | ImGuiTreeNodeFlags_SelectableDontClosePopup);
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
	if (!PItem)
		return;

	EPropType type = PItem->Type();
	switch (type)
	{
		case PROP_WAVE:
		{
			WaveValue* V = dynamic_cast<WaveValue*>(PItem->GetFrontValue());
			WaveForm edit_val = V->GetValue();
			PItem->BeforeEdit<WaveValue, WaveForm>(edit_val);

			if (CWaveForm::form == nullptr)
			{
				CWaveForm::form = new CWaveForm;
				GUIManager->Push(CWaveForm::form, false);
			}
			ImGui::PushID(V);
			if (ImGui::Button("[Wave]"))
			{
				CWaveForm::form->ItemKey = PItem->Key();
				CWaveForm::form->Run(&edit_val);
			}
			ImGui::PopID();

			if (CWaveForm::form->ItemKey == PItem->Key())
			{
				if (WaveForm* WaveInfo = CWaveForm::form->GetResult())
				{
					if (PItem->AfterEdit<WaveValue, WaveForm>(*WaveInfo))
					{
						if (PItem->ApplyValue<WaveValue, WaveForm>(*WaveInfo))
						{
							PropertiesFrom->Modified();
						}
					}
				}
			}
			break;
		}
		case PROP_UNDEF: break;
		
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
				{
					val->OnDrawCanvasEvent(val);
				}
			}
		}
		break;
		case PROP_BUTTON:
		{
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
		}
		case PROP_CAPTION:
		{
			ImGui::TextDisabled(PItem->GetDrawText().c_str());
			break;
		}
		default:
		{
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
}

UITreeItem* UIPropertiesItem::CreateItem(shared_str Name)
{
	return new UIPropertiesItem(Name,PropertiesFrom);
}