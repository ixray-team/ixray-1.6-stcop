#include "stdafx.h"
#include "WaveForm.h"
#include "imgui_internal.h"

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
	const float   TableRowHeight	= XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableRowHeight);
	const ImColor RowColor0			= XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelTint);
	const ImColor RowColor1			= XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelBorderTint);
	const ImColor RowColor2			= XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::TableTint);
	const ImVec2  CellPadding		= ImVec2(XRay::ImGui::GetEditorSize(XRay::ImGui::ButtonTextPaddingY), XRay::ImGui::GetEditorSize(XRay::ImGui::TableTextPaddingY));

	ImGui::TableNextRow(ImGuiTableRowFlags_None, TableRowHeight + 1.f);	// Important inner border size compensation
	ImGui::TableSetBgColor(ImGuiTableBgTarget_RowBg0, ImGui::GetColorU32(ImGui::GetStyle().Colors[ImGuiCol_ChildBg]));
	ImGui::TableNextColumn();
	ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 1.0f);												// Important move away from horizontal inner border

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

	// ---------------------- //
	// Table Collapse buttons //
	// ---------------------- //
	if (!Items.empty())
	{
		ImVec2 node_cursor = ImGui::GetCursorScreenPos();

		ImGui::TableSetBgColor(ImGuiTableBgTarget_RowBg0, RowColor1);
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, CellPadding);
		ImGui::PushStyleColor(ImGuiCol_Header, RowColor1.Value);

		bool open = XRay::ImGui::CollapsingHeader(*Name, ImGuiTreeNodeFlags_DefaultOpen);

		ImGui::PopStyleColor();
		ImGui::PopStyleVar();

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
		if (IsTexture)
		{
			MultiChooseValue* Prop = (MultiChooseValue*)PItem->GetFrontValue();

			if (Prop != nullptr && !Prop->Values.empty())
			{
				shared_str TexName = Prop->Values[0]->value ? Prop->Values[0]->GetValue() : "";
				if (TexName.size() > 0)
				{
					float TexSize = (TableRowHeight + 1.f) * 4.f;
					//ImVec2 Rect = ImGui::GetCurrentTable().;
					float CellWhdth = ImGui::GetContentRegionMax().x;

					ImTextureID Image = GUIManager->LoadTexture(*TexName);
					//ImGui::SetCursorPosX(ImGui::GetCursorPosX());
					ImGui::SetCursorPosX((CellWhdth - TexSize) / 2.f);
					ImGui::Image(Image, { TexSize, TexSize });
				}
			}
		}
		else
		{
			ImGui::TableSetBgColor(ImGuiTableBgTarget_CellBg, ImGui::GetColorU32(RowColor0.Value));
			//ImGui::SetCursorPos(ImGui::GetCursorPos() + CellPadding);
			//ImGui::TextUnformatted(*Name);
			XRay::ImGui::TextFramed(*Name, { -1, -1 }, { 0.f, 0.5f }, false);
		}

		ImGui::TableNextColumn();
		ImGui::TableSetBgColor(ImGuiTableBgTarget_CellBg, ImGui::GetColorU32(RowColor2.Value));
		ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 1.0f); // Important move away from horizontal inner border
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
    const float   TableRowHeight	= XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableRowHeight);
	const ImVec2  CellPadding		= ImVec2(XRay::ImGui::GetEditorSize(XRay::ImGui::ButtonTextPaddingY), XRay::ImGui::GetEditorSize(XRay::ImGui::TableTextPaddingY));
	const ImColor RowColor1			= XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelTint);
	const ImColor RowColor2			= XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::TableTint);
	const ImVec2  ButtonPadding		= ImVec2(XRay::ImGui::GetEditorSize(XRay::ImGui::ButtonPaddingW), XRay::ImGui::GetEditorSize(XRay::ImGui::ButtonPaddingH));

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
			if (XRay::ImGui::Button("[Wave]"))
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
					//float dx = floorf(size / float(V->value.size()));
					//float offset = size - (dx * V->value.size());
					float buttonSize = size / float(V->value.size());
					V->btn_num = V->value.size();

					for (RStringVecIt it = V->value.begin(); it != V->value.end(); it++)
					{
						int k = it - V->value.begin();				// dx + offset
						if (XRay::ImGui::Button(it->c_str(), ImVec2(buttonSize - 1.f, TableRowHeight)))
						{
							V->btn_num = k;

							bRes |= V->OnBtnClick(bSafe);
						}
						//offset = 0;
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
			ImGui::SetCursorPos(ImGui::GetCursorPos() + CellPadding);
			ImGui::TextDisabled(PItem->GetDrawText().c_str());
			break;
		}

		case PROP_CHOOSE_TEXTURE:
		{
			MultiChooseValue* Prop = (MultiChooseValue*)PItem->GetFrontValue();

			ImVec2 originalCellPadding = ImGui::GetStyle().CellPadding;
			ImVec2 originalFramePadding = ImGui::GetStyle().FramePadding;
			ImVec2 originalItemSpacing = ImGui::GetStyle().ItemSpacing;

			// Óáèðàåì âåðòèêàëüíûå îòñòóïû
			ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(originalCellPadding.x, 0));
			ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(originalFramePadding.x, 0));
			ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(originalItemSpacing.x, 0));
			ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, 0.0f);

			ImGui::TableSetBgColor(ImGuiTableBgTarget_CellBg, ImGui::GetColorU32(RowColor1.Value));
			ImGui::PushStyleColor(ImGuiCol_Button, RowColor2.Value);

			if (ImGui::BeginTable("##multi_choose_table", 2, ImGuiTableFlags_BordersInner))
			{
				// If you are implementing UI localization, forgive us this hardcoded width :'-)
				ImGui::TableSetupColumn("Key", ImGuiTableColumnFlags_WidthFixed);
				ImGui::TableSetupColumn("Value", ImGuiTableColumnFlags_WidthStretch);

				for (ChooseValue* ChooseItem : Prop->Values)
				{
					xr_string text = ChooseItem->Owner()->Key();
					xr_string TextValue = ChooseItem->Owner()->GetDrawText();

					if (TextValue.empty())
					{
						text = NONE_CAPTION;
						TextValue = NONE_CAPTION;
					}
					else
					{
						xr_path ExtractName = text;
						text = ExtractName.xfilename();
					}

					ImGui::PushID(ChooseItem);
					ImGui::TableNextRow(ImGuiTableRowFlags_None, TableRowHeight + 1.f);	// Important inner border size compensation

					ImGui::TableSetColumnIndex(0);

					XRay::ImGui::TextFramed(text.c_str(), { 0, -1 }, { 0.f, 0.5f }, false);

					ImGui::TableSetColumnIndex(1);
					ImGui::NextColumn();
					ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 1.0f);												// Important move away from horizontal inner border
					if (ImGui::Button(TextValue.c_str(), ImVec2(-1, TableRowHeight)))
					{
						ChooseValue* V = dynamic_cast<ChooseValue*>(ChooseItem->Owner()->GetFrontValue());
						VERIFY(V);

						shared_str edit_val = V->GetValue();
						if (!edit_val.size())
						{
							edit_val = V->m_StartPath;
						}

						ChooseItem->Owner()->BeforeEdit<ChooseValue, shared_str>(edit_val);

						ChooseItemVec Items;
						if (!V->OnChooseFillEvent.empty())
						{
							V->m_Items = &Items;
							V->OnChooseFillEvent(V);
						}

						UIChooseForm::SelectItem(V->m_ChooseID, V->subitem, edit_val.c_str(), 0, V->m_FillParam, 0, !Items.empty() ? &Items : 0, V->m_ChooseFlags);
						PropertiesFrom->m_EditChooseValue = ChooseItem->Owner();
					}
					ImGui::PopID();
				}

				ImGui::EndTable();
			}
			ImGui::PopStyleColor();
			ImGui::PopStyleVar(4);
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
				if (XRay::ImGui::Button("(Mixed)", ImVec2(-1, 0)))
				{
					RemoveMixed();
				}
			}
			else
			{
				ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(ButtonPadding.y, CellPadding.y));
				if (PItem->m_Flags.test(PropItem::flDisabled))
				{
					//if (type == PROP_FLAG)
					//{
					//	FlagValueCustom* V = dynamic_cast<FlagValueCustom*>(PItem->GetFrontValue()); VERIFY(V);
					//	ImGui::TextDisabled(V->GetValueEx() ? "true" : "false");
					//}
					//else
					//{
						ImGui::BeginDisabled();
						DrawProp();
						ImGui::EndDisabled();
					//}
				}
				else
				{
					ImGui::PushItemWidth(-1);
					DrawProp();
				}
				ImGui::PopStyleVar();
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