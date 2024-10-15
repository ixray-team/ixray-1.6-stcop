#include "stdafx.h"
UIPropertiesForm::UIPropertiesForm():m_Root("",this)
{
	m_bModified = false;
	m_EditChooseValue = nullptr;
	m_EditShortcutValue = nullptr;
	m_EditTextureValue = nullptr;
	m_EditTextValueData = nullptr;
	m_Flags.zero();
}

UIPropertiesForm::~UIPropertiesForm()
{
	if (m_EditTextValueData)xr_delete(m_EditTextValueData);
	ClearProperties();
}

void UIPropertiesForm::Draw()
{
	if (!bAsyncUpdated)
	{
		return;
	}
	{
		if (m_EditChooseValue)
		{
			shared_str result;
			bool is_result;
			if (UIChooseForm::GetResult(is_result, result))
			{
				if (is_result)
				{
					if (m_EditChooseValue->AfterEdit<ChooseValue, shared_str>(result))
						if (m_EditChooseValue->ApplyValue<ChooseValue, shared_str>(result))
						{
							Modified();
						}
				}
				m_EditChooseValue = nullptr;
			}

			UIChooseForm::Update();
		}
		if (m_EditTextureValue)
		{
			shared_str result;
			bool is_result;
			if (UIChooseForm::GetResult(is_result, result))
			{
				if (is_result)
				{
					if (result.c_str() == nullptr)
					{
						xr_string result_as_str = "$null";
						if (m_EditTextureValue->AfterEdit<CTextValue, xr_string>(result_as_str))
							if (m_EditTextureValue->ApplyValue<CTextValue, LPCSTR>(result_as_str.c_str()))
							{
								Modified();
							}
					}
					else
					{
						xr_string result_as_str = result.c_str();
						if (m_EditTextureValue->AfterEdit<CTextValue, xr_string>(result_as_str))
							if (m_EditTextureValue->ApplyValue<CTextValue, LPCSTR>(result_as_str.c_str()))
							{
								Modified();
							}
					}

				}
				m_EditTextureValue = nullptr;
			}
			UIChooseForm::Update();
		}
		if (m_EditShortcutValue)
		{
			xr_shortcut result;
			bool ok;
			if (UIKeyPressForm::GetResult(ok, result))
			{
				if (ok)
				{
					if (m_EditShortcutValue->AfterEdit<ShortcutValue, xr_shortcut>(result))
						if (m_EditShortcutValue->ApplyValue<ShortcutValue, xr_shortcut>(result))
						{
							Modified();
						}
				}
				m_EditShortcutValue = nullptr;
			}
		}
	}

	static ImGuiTableFlags flags = ImGuiTableFlags_Borders | ImGuiTableFlags_BordersOuterH | ImGuiTableFlags_Resizable | ImGuiTableFlags_RowBg | ImGuiTableFlags_NoBordersInBody;
	if (ImGui::BeginTable("props", 2, flags))
	{
		ImGui::TableSetupColumn("Name", ImGuiTableColumnFlags_NoHide);
		ImGui::TableSetupColumn("Prop", ImGuiTableColumnFlags_WidthFixed);
		ImGui::TableHeadersRow();
		m_Root.DrawRoot();
		ImGui::EndTable();
	}
}

void UIPropertiesForm::AssignItemsAsync(PropItemVec items)
{
	bAsyncUpdated = false;

	m_Items = items;
	for (PropItem* item : items)
	{
		item->m_Owner = this;
		UIPropertiesItem* Item = static_cast<UIPropertiesItem*>(m_Root.AppendItem(item->Key()));
		VERIFY(Item);
		Item->PItem = item;
	}

	bAsyncUpdated = true;
}


void UIPropertiesForm::AssignItems(PropItemVec& items)
{
	if (!bAsyncUpdated)
		return;

	m_Items = items;
	for (PropItem* item : items)
	{
		item->m_Owner = this;
		UIPropertiesItem* Item = static_cast<UIPropertiesItem*>(m_Root.AppendItem(item->Key()));
		VERIFY(Item);
		Item->PItem = item;
	}
}

PropItem* UIPropertiesForm::FindItemOfName(shared_str name)
{
	for (PropItem* I : m_Items)
	{
		const char* key = I->Key();
		if (strrchr(key, '\\'))
		{
			key = strrchr(key, '\\') + 1;
		}
		if (name == key)
		{
			return I;
		}
	}
	return nullptr;
}

void UIPropertiesForm::ClearProperties()
{
	VERIFY(!m_EditChooseValue);
	for (PropItem* I : m_Items)
	{
		xr_delete(I);
	}
	m_Root = UIPropertiesItem("",this);
	m_Items.clear();
}

PropItem* UIPropertiesForm::FindItem(const char* name)
{
	UIPropertiesItem*Item = static_cast<UIPropertiesItem *>( m_Root.FindItem(name));
	if (Item)
	{
		return Item->PItem;
	}
	return nullptr;
}

void UIPropertiesForm::DrawEditText()
{

	if (ImGui::BeginPopupContextItem("EditText", 0))
	{
		R_ASSERT(m_EditTextValueData);

		ImGui::BeginGroup();
		if (ImGui::Button("Ok"))
		{
			CTextValue* V1 = dynamic_cast<CTextValue*>(m_EditTextValue->GetFrontValue());
			if (V1)
			{
				xr_string out = m_EditTextValueData;
				if (m_EditTextValue->AfterEdit<CTextValue, xr_string>(out))
				{
					if (m_EditTextValue->ApplyValue<CTextValue, LPCSTR>(out.c_str()))
					{
						xr_delete(m_EditTextValueData);
						Modified();
						ImGui::CloseCurrentPopup();
					}
				}
			}
			else
			{
				RTextValue* V2 = dynamic_cast<RTextValue*>(m_EditTextValue->GetFrontValue());
				if (V2)
				{
					shared_str out = m_EditTextValueData;
					if (m_EditTextValue->AfterEdit<RTextValue, shared_str>(out))
					{
						if (m_EditTextValue->ApplyValue<RTextValue, shared_str>(out))
						{
							xr_delete(m_EditTextValueData);
							Modified();
							ImGui::CloseCurrentPopup();
						}
						else
						{
							ImGui::CloseCurrentPopup();
						}
					}
				}
				else
				{
					STextValue* V3 = dynamic_cast<STextValue*>(m_EditTextValue->GetFrontValue());
					if (V3)
					{
						xr_string out = m_EditTextValueData;
						if (m_EditTextValue->AfterEdit<STextValue, xr_string>(out))
						{
							if (m_EditTextValue->ApplyValue<STextValue, xr_string>(out))
							{
								xr_delete(m_EditTextValueData);
								Modified();
								ImGui::CloseCurrentPopup();
							}
						}
					}
					else
					{
						R_ASSERT(false);
					}
				}
			}
		}ImGui::SameLine(0);
		if (ImGui::Button("Cancel"))
		{
			xr_delete(m_EditTextValueData);
			ImGui::CloseCurrentPopup();
		} ImGui::SameLine(0);
		if (ImGui::Button("Apply"))
		{
			CTextValue* V1 = dynamic_cast<CTextValue*>(m_EditTextValue->GetFrontValue());
			if (V1)
			{
				xr_string out = m_EditTextValueData;
				if (m_EditTextValue->AfterEdit<CTextValue, xr_string>(out))
				{
					if (m_EditTextValue->ApplyValue<CTextValue, LPCSTR>(out.c_str()))
					{
						Modified();
					}
				}
			}
			else
			{
				RTextValue* V2 = dynamic_cast<RTextValue*>(m_EditTextValue->GetFrontValue());
				if (V2)
				{
					shared_str out = m_EditTextValueData;
					if (m_EditTextValue->AfterEdit<RTextValue, shared_str>(out))
					{
						if (m_EditTextValue->ApplyValue<RTextValue, shared_str>(out))
						{
							Modified();
						}
					}
				}
				else
				{
					STextValue* V3 = dynamic_cast<STextValue*>(m_EditTextValue->GetFrontValue());
					if (V3)
					{
						xr_string out = m_EditTextValueData;
						if (m_EditTextValue->AfterEdit<STextValue, xr_string>(out))
						{
							if (m_EditTextValue->ApplyValue<STextValue, xr_string>(out))
							{
								Modified();
							}
						}
					}
					else
					{
						R_ASSERT(false);
					}
				}
			}
		}ImGui::SameLine(150);

		if (ImGui::Button("Load"))
		{
			xr_string fn;
			if (EFS.GetOpenName("$import$", fn, false, NULL, 2)) 
			{
				xr_string		buf;
				FS.TryLoad(fn);
				IReader* F = FS.r_open(fn.c_str());

				F->r_stringZ(buf);
				xr_delete(m_EditTextValueData);
				m_EditTextValueData = xr_strdup(buf.c_str());
				m_EditTextValueDataSize = xr_strlen(m_EditTextValueData)+1;
				FS.r_close(F);
			}
		}
		
		ImGui::SameLine(0);
		if (ImGui::Button("Save"))
		{
			xr_string fn;
			if (EFS.GetSaveName("$import$", fn, NULL, 2)) {
				CMemoryWriter F;
				F.w_stringZ(m_EditTextValueData);
				if (!F.save_to(fn.c_str()))
					Msg("!Can't save text file: %s", fn.c_str());
			}
		}
		
		ImGui::SameLine(0);
		if (ImGui::Button("Clear")) { m_EditTextValueData[0] = 0; }
		ImGui::EndGroup();

		if (m_EditTextValueData)
		{
			if (!IsUTF8(m_EditTextValueData))
			{
				xr_string CopyStr = m_EditTextValueData;
				xr_free(m_EditTextValueData);
				m_EditTextValueData = xr_strdup(Platform::ANSI_TO_UTF8(CopyStr).c_str());
			}

			ImGui::InputTextMultiline
			(
				"##text",
				m_EditTextValueData,
				m_EditTextValueDataSize,
				ImVec2(500, 200),
				ImGuiInputTextFlags_CallbackResize,
				[](ImGuiInputTextCallbackData* data)->int
				{
					return reinterpret_cast<UIPropertiesForm*>(data->UserData)->DrawEditText_Callback(data);
				},
				reinterpret_cast<void*>(this)
			);
		}
		
		ImGui::EndPopup();
	}
}

int UIPropertiesForm::DrawEditText_Callback(ImGuiInputTextCallbackData* data)
{
	if (IsUTF8(m_EditTextValueData))
	{
		xr_string CopyStr = m_EditTextValueData;
		xr_free(m_EditTextValueData);
		m_EditTextValueData = xr_strdup(Platform::UTF8_to_CP1251(CopyStr).c_str());
	}
	m_EditTextValueData =(char*) xr_realloc(m_EditTextValueData, data->BufSize);
	m_EditTextValueDataSize = data->BufSize;
	data->Buf = m_EditTextValueData;
	return 0;
}

void UIPropertiesForm::DrawEditGameType()
{
	if (ImGui::BeginPopupContextItem("EditGameType", 0))
	{
		R_ASSERT(m_EditGameTypeValue);

		bool test = false;
		{
			ImGui::BeginGroup();
			{
				bool cheked = m_EditGameTypeChooser.MatchType(eGameIDSingle);
				if (ImGui::Checkbox("Single", &cheked))
				{
					m_EditGameTypeChooser.m_GameType.set(eGameIDSingle, cheked);
				}
			}
			{
				bool cheked = m_EditGameTypeChooser.MatchType(eGameIDDeathmatch);
				if (ImGui::Checkbox("DM", &cheked))
				{
					m_EditGameTypeChooser.m_GameType.set(eGameIDDeathmatch, cheked);
				}
			}
			{
				bool cheked = m_EditGameTypeChooser.MatchType(eGameIDTeamDeathmatch);
				if (ImGui::Checkbox("TDM", &cheked))
				{
					m_EditGameTypeChooser.m_GameType.set(eGameIDTeamDeathmatch, cheked);
				}
			}
			{
				bool cheked = m_EditGameTypeChooser.MatchType(eGameIDArtefactHunt);
				if (ImGui::Checkbox("ArtefactHunt", &cheked))
				{
					m_EditGameTypeChooser.m_GameType.set(eGameIDArtefactHunt, cheked);
				}
			}
			{
				bool cheked = m_EditGameTypeChooser.MatchType(eGameIDCaptureTheArtefact);
				if (ImGui::Checkbox("CTA", &cheked))
				{
					m_EditGameTypeChooser.m_GameType.set(eGameIDCaptureTheArtefact, cheked);
				}
			}
			{
				bool cheked = m_EditGameTypeChooser.MatchType(eGameIDFreeMP);
				if (ImGui::Checkbox("FMP", &cheked))
				{
					m_EditGameTypeChooser.m_GameType.set(eGameIDFreeMP, cheked);
				}
			}
			ImGui::EndGroup(); ImGui::SameLine();
		}
		{
			ImGui::BeginGroup();
			if (ImGui::Button("Ok", ImVec2(ImGui::GetFrameHeight() * 6, 0)))
			{
				if (m_EditGameTypeValue->AfterEdit<GameTypeValue, GameTypeChooser>(m_EditGameTypeChooser))
					if (m_EditGameTypeValue->ApplyValue<GameTypeValue, GameTypeChooser>(m_EditGameTypeChooser))
					{
						Modified();
					}
				ImGui::CloseCurrentPopup();
			}
			if (ImGui::Button("Cancel", ImVec2(ImGui::GetFrameHeight() * 6, 0)))
			{
				m_EditGameTypeValue = nullptr;
				ImGui::CloseCurrentPopup();
			}
			ImGui::EndGroup();
		}
		ImGui::EndPopup();
	}
}

