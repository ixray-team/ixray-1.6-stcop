#include "stdafx.h"
#include "UIPropertiesForm.h"

UIPropertiesForm::UIPropertiesForm() :
	m_Root("", this), SearchRoot("", this)
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
	xr_delete(m_EditTextValueData);
	ClearProperties();
}

void UIPropertiesForm::Draw()
{
	if (!bAsyncUpdated)
	{
		return;
	}

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

	if (!IsSearchDisabled)
	{
		ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x - 45);

		string32 FindStr = {};
		xr_strcpy(FindStr, m_SearchText.c_str());

		if (ImGui::InputTextWithHint("##search", "Search...", FindStr, sizeof(FindStr)))
		{
			m_SearchText = FindStr;
			SearchRoot.Items.clear();
		}

		if (GUIManager->SearchIcon)
		{
			ImVec2 IconSize = { 12,12 };

			ImGui::SameLine();
			ImVec2 cursorPos = ImGui::GetCursorPos();
			ImGui::SetCursorPos(ImVec2(cursorPos.x - IconSize.x - 10.f, 1 + cursorPos.y + (IconSize.y / 4)));

			ImGui::Image(GUIManager->SearchIcon, IconSize);
		}

		IsSearchActive = !m_SearchText.empty();

		ImGui::SameLine();
		if (ImGui::Button("Clear"))
		{
			m_SearchText = "";
			IsSearchActive = false;
		}

		ImGui::Separator();
	}
	static constexpr ImGuiTableFlags DefFlags = ImGuiTableFlags_Borders | ImGuiTableFlags_BordersOuterH | ImGuiTableFlags_RowBg | ImGuiTableFlags_NoBordersInBodyUntilResize;
	ImGuiTableFlags Flags = DefFlags;
	if (IsFitMode)
	{
		Flags |= ImGuiTableFlags_SizingFixedFit;
	}
	else
	{
		Flags |= ImGuiTableFlags_Resizable;
	}

	ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(0, ImGui::GetStyle().CellPadding.y));
	if (ImGui::BeginTable("props", 2, Flags))
	{
		ImGui::TableSetupColumn(" Name", ImGuiTableColumnFlags_WidthFixed, 0.0f);
		ImGui::TableSetupColumn("Prop", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableHeadersRow();

		ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(0, 1));
		if (IsSearchActive)
		{
			DrawFilteredProperties();
		}
		else
		{
			m_Root.DrawRoot();
		}
		ImGui::PopStyleVar();

		ImGui::EndTable();
	}
	ImGui::PopStyleVar();
}

void UIPropertiesForm::ResetEnd()
{
}

void UIPropertiesForm::AssignItemsAsync(PropItemVec items)
{
	bAsyncUpdated = false;

	m_Items = items;
	for (PropItem* PItem : items)
	{
		PItem->m_Owner = this;
		UIPropertiesItem* Item = static_cast<UIPropertiesItem*>(m_Root.AppendItem(PItem->Key()));
		VERIFY(Item);
		Item->PItem = PItem;
		Item->IsTexture = PItem->IsTextureItem;
	}

	bAsyncUpdated = true;
}

void UIPropertiesForm::AssignItems(PropItemVec& items)
{
	if (!bAsyncUpdated)
		return;

	m_Items = items;

	m_SearchText = "";
	m_SearchText.clear();

	for (PropItem* PItem : items)
	{
		PItem->m_Owner = this;
		UIPropertiesItem* Item = static_cast<UIPropertiesItem*>(m_Root.AppendItem(PItem->Key()));
		VERIFY(Item);
		Item->PItem = PItem;
		Item->IsTexture = PItem->IsTextureItem;
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

	m_SearchText = "";
	m_SearchText.clear();
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

UIPropertiesItem* UIPropertiesForm::FindPropItem(const char* path)
{
	UIPropertiesItem* Item = static_cast<UIPropertiesItem*>(m_Root.FindItem(path));
	if (Item)
	{
		return Item;
	}
	return nullptr;
}

void UIPropertiesForm::DrawEditText()
{
	if (ImGui::BeginPopupContextItem("EditText", 0))
	{
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
					shared_str out = !IsUTF8(m_EditTextValueData) ? m_EditTextValueData : Platform::UTF8_to_CP1251(m_EditTextValueData).data();
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
						VERIFY(false);
					}
				}
			}
		}
		ImGui::SameLine(0);

		if (ImGui::Button("Cancel"))
		{
			xr_delete(m_EditTextValueData);
			ImGui::CloseCurrentPopup();
		}
		ImGui::SameLine(0);

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
						VERIFY(false);
					}
				}
			}
		}ImGui::SameLine(150);

		if (ImGui::Button("Load"))
		{
			xr_string fn;
			if (EFS.GetOpenName("$import$", fn, false, NULL, 2)) 
			{
				xr_string buf;
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
			if (EFS.GetSaveName("$import$", fn, NULL, 2))
			{
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

			string512 OutStr = {};
			xr_strcpy(OutStr, m_EditTextValueData);
			if (ImGui::InputTextMultiline("##text", OutStr, std::size(OutStr), ImVec2(500, 200)))
			{
				bool IsUTF8String = IsUTF8(m_EditTextValueData);
				xr_free(m_EditTextValueData);

				if (IsUTF8String)
				{
					m_EditTextValueData = xr_strdup(Platform::UTF8_to_CP1251(OutStr).c_str());
				}
				else
				{
					m_EditTextValueData = xr_strdup(OutStr);
				}
			}
		}
		
		ImGui::EndPopup();
	}
}

void UIPropertiesForm::DrawEditGameType()
{
	if (ImGui::BeginPopupContextItem("EditGameType", 0))
	{
		R_ASSERT(m_EditGameTypeValue);

		bool test = false;
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
		{
			bool cheked = m_EditGameTypeChooser.MatchType(eGameIDCapturePoints);
			if (ImGui::Checkbox("CP", &cheked))
			{
				m_EditGameTypeChooser.m_GameType.set(eGameIDCapturePoints, cheked);
			}

		}
		ImGui::EndGroup(); ImGui::SameLine();

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
		ImGui::EndPopup();
	}
}

void UIPropertiesForm::DrawFilteredProperties()
{
	if (SearchRoot.Items.empty())
	{
		for (PropItem* PItem : m_Items)
		{
			if (DoesItemMatchSearch(PItem->Key()))
			{
				UIPropertiesItem* Item = static_cast<UIPropertiesItem*>(SearchRoot.AppendItem(PItem->Key()));
				VERIFY(Item);
				Item->PItem = PItem;
				Item->IsTexture = PItem->IsTextureItem;
			}
		}
	}

	SearchRoot.DrawRoot();
}

bool UIPropertiesForm::DoesItemMatchSearch(shared_str ItemName)
{
	if (!ItemName || m_SearchText.empty())
	{
		return true;
	}

	const char* key = *ItemName;
	if (strrchr(key, '\\'))
	{
		key = strrchr(key, '\\') + 1;
	}

	xr_string itemName = key;
	xr_string searchLower = m_SearchText;

	xr_strlwr(itemName);
	xr_strlwr(searchLower);

	return itemName.Contains(searchLower);
}

int UIPropertiesForm::GetVisibleItemsCount()
{
	if (!IsSearchActive)
	{
		return m_Items.size();
	}

	int Count = 0;
	for (PropItem* item : m_Items)
	{
		if (DoesItemMatchSearch(item->key))
		{
			Count++;
		}
	}
	return Count;
}