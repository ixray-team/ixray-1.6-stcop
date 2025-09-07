#include "stdafx.h"

UIDOShuffle* UIDOShuffle::Form = nullptr;
UIDOShuffle::UIDOShuffle()
{
	bModif = false;
	m_ChooseObject = false;
	m_Texture = nullptr;
	m_TextureNull.create("ed\\ed_nodata");
	m_TextureNull->Load();
	m_Props = new UIPropertiesForm();
	m_RealTexture = nullptr;
}

UIDOShuffle::~UIDOShuffle()
{
	m_TextureNull.destroy();
	m_MaskTexture.destroy();

	ApplyChanges();
	xr_delete(m_Props);
	if (m_Texture)
		IM_TEXTURE_RELEASE(m_Texture);

	ClearIndexForms();

	if (m_RealTexture)
		IM_TEXTURE_RELEASE(m_RealTexture);
}

void UIDOShuffle::Draw()
{
	ImGui::Columns(3);
	if (ImGui::BeginChild("Preview"))
	{
		ImVec2 StartPos = ImGui::GetCursorPos();

		ImGui::Image(m_MaskTexture._get() ? m_MaskTexture->pSurface : m_TextureNull->pSurface, ImVec2(256, 256));
		if (ImGui::IsItemClicked(ImGuiMouseButton_Left))
		{
			ImVec2 ClickPos = ImGui::GetMousePos();
			ClickPos -= ImGui::GetWindowPos();
			//ClickPos -= ImGui::GetStyle().ItemSpacing;
			ClickPos -= StartPos;

			size_t PixelIndex = ClickPos.y * 256 * 4 + (ClickPos.x * 4);

			UIDOOneColor* NewItem = m_color_indices.emplace_back(new UIDOOneColor);
			NewItem->DOShuffle = this;

			if (!Pixels.empty())
			{
				NewItem->Color[2] = (float)Pixels[PixelIndex + 0] / 255.f;
				NewItem->Color[1] = (float)Pixels[PixelIndex + 1] / 255.f;
				NewItem->Color[0] = (float)Pixels[PixelIndex + 2] / 255.f;
			}
		}

		int selected = m_list_selected;
		ImGui::SetNextItemWidth(-1);
		if (ImGui::ListBox("##list", &selected, [](void* data, int ind, const char** out)->bool {auto item = reinterpret_cast<xr_vector<xr_string>*>(data)->at(ind).c_str();; *out = item; return true; }, reinterpret_cast<void*>(&m_list), m_list.size(), 14))
		{
			if (m_list_selected != selected)
			{
				m_list_selected = selected;
				OnItemFocused(m_list[selected].c_str());
			}
		}
	}
	ImGui::EndChild();
	ImGui::NextColumn();
	ImGui::BeginChild("Left");
	{
		if (m_RealTexture != m_Texture)
		{
			if (m_RealTexture)
				IM_TEXTURE_RELEASE(m_RealTexture);

			m_RealTexture = m_Texture;
		}
		ImGui::Image(m_RealTexture ? m_RealTexture :m_TextureNull->pSurface, ImVec2(256, 256));

		{
			if (ImGui::Button("+", ImVec2(0, ImGui::GetFrameHeight()))) { UIChooseForm::SelectItem(smObject, 8); m_ChooseObject = true; }; ImGui::SameLine();
			if (ImGui::Button("-", ImVec2(0, ImGui::GetFrameHeight()))) 
			{
				if (m_list_selected >= 0 && m_list.size() > m_list_selected)
				{
					DM->RemoveDO(m_list[m_list_selected].c_str());
					for (UIDOOneColor* one_color : m_color_indices) { one_color->RemoveObject(m_list[m_list_selected]); }
					m_list.erase(m_list.begin() + m_list_selected);
					OnItemFocused(nullptr);
					bModif = true;
				}
			}
			ImGui::SameLine();
			if (ImGui::Button("X", ImVec2(0, ImGui::GetFrameHeight())))
			{
				m_list.clear();
				DM->InvalidateSlots();
				DM->ClearColorIndices();
				ClearIndexForms();
				UI->RedrawScene();
				OnItemFocused(nullptr);
				bModif = true;
			}
			ImGui::SameLine();
			if (ImGui::Button("Load..", ImVec2(0, ImGui::GetFrameHeight())))
			{
				xr_string fname;
				if (EFS.GetOpenName(_detail_objects_, fname)) {
					LoadFromStream(fname);
				}
			}
			ImGui::SameLine();
			if (ImGui::Button("Save..", ImVec2(0, ImGui::GetFrameHeight())))
			{
				xr_string fname;
				if (EFS.GetSaveName(_detail_objects_, fname)) 
				{
					ApplyChanges(false);
					DM->ExportColorIndices(fname.c_str());
				}
				
			}
		}
		{
			ImGui::BeginChild("Props",  ImVec2(0, 0), false);
			m_Props->Draw();
			ImGui::EndChild();
		}
		
	}
	ImGui::EndChild();
	ImGui::NextColumn();
	ImGui::BeginChild("Right");
	{
		if (ImGui::Button("X")) { ClearIndexForms(); }
		ImGui::SameLine();
		if (ImGui::Button("Append Color Index", ImVec2(-1,0))) 
		{
			m_color_indices.push_back(new UIDOOneColor());
			m_color_indices.back()->DOShuffle = this;
		}

		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 0));
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
		{
			int index = 0;
			
			for (UIDOOneColor* one_color : m_color_indices)
			{
				ImGui::PushID(index);
				one_color->Draw();
				ImGui::PopID();
				index++;
			}
			for (auto b = m_color_indices.begin(), e = m_color_indices.end(); b != e; b++)
			{
				if ((*b)->IsClosed())
				{
					auto ptr = *b;
					xr_delete(ptr);
					m_color_indices.erase(b);
					b = m_color_indices.begin();
				}
			}
		}
		ImGui::PopStyleVar(2);
	}
	ImGui::EndChild();
	ImGui::Columns();
	if (m_ChooseObject)
	{
		bool ok = false;
		xr_vector<xr_string> list;
		if (UIChooseForm::GetResult(ok,list))
		{
			if (ok)
			{
				for (xr_string& l : list)
				{
					if (!FindItem(l.c_str())) 
					{
						DM->AppendDO(l.c_str());
						m_list_selected = m_list.size();
						m_list.push_back(l);
					}
				}
				if (!list.empty())
				{
					OnItemFocused(list.back().c_str());
					bModif = true;
				}
			}
			m_ChooseObject = false;
		}
		UIChooseForm::Update();
	}
}

void UIDOShuffle::LoadFromStream(xr_string& fname)
{
	if (Form->DM->ImportColorIndices(fname.c_str()))
	{
		Form->DM->InvalidateSlots();
		Form->FillData();
		Form->bModif = true;
	}
}

void UIDOShuffle::Update()
{
	if (Form && !Form->IsClosed())
	{
		ImGui::SetNextWindowSize(ImVec2(800, 600), ImGuiCond_::ImGuiCond_FirstUseEver);
		if (ImGui::BeginPopupModal("Detail Object List", &Form->bOpen,0,true))
		{
			Form->Draw();
			ImGui::EndPopup();
		}
	}

}

bool UIDOShuffle::GetResult()
{
	if (!Form->bOpen)
	{
		xr_delete(Form);
		return true;
	}
	return false;
}

void UIDOShuffle::Show(EDetailManager* DM)
{
	VERIFY(!Form);
	Form = new UIDOShuffle();
	Form->DM = DM;
	Form->FillData();
}

void UIDOShuffle::FillData()
{
	m_list.clear();
	OnItemFocused(nullptr);

	xr_string TextureMaskPath = DM->m_Base.GetName();
	m_MaskTexture.destroy();
	if (!TextureMaskPath.empty())
	{
		TextureMaskPath += ".dds";

		string_path FullPath;
		FS.update_path(FullPath, _game_textures_, TextureMaskPath.c_str());

		Pixels = DXTUtils::GitPixels(FullPath, 256, 256);

		// FX: Remove Alpha
		for (size_t Iter = 3; Iter < Pixels.size(); Iter += 4)
		{
			Pixels[Iter] = 255;
		}

		if (!Pixels.empty())
		{
			m_MaskTexture = new CTexture();
			ID3DTexture2D* pTexture = nullptr;
			R_CHK(REDevice->CreateTexture(256, 256, 1, 0, D3DFMT_A8R8G8B8, D3DPOOL_MANAGED, &pTexture, 0));
			{
				D3DLOCKED_RECT rect;
				R_CHK(pTexture->LockRect(0, &rect, 0, D3DLOCK_DISCARD));
				memcpy(rect.pBits, Pixels.data(), Pixels.size());
				R_CHK(pTexture->UnlockRect(0));

				m_MaskTexture->pSurface = pTexture;
			}
		}
	}

	for (CDetailManager::DetailIt d_it = DM->objects.begin(); d_it != DM->objects.end(); d_it++)
		m_list.push_back(((EDetail*)(*d_it))->GetName());
	ClearIndexForms();
	ColorIndexPairIt S = DM->m_ColorIndices.begin();
	ColorIndexPairIt E = DM->m_ColorIndices.end();
	ColorIndexPairIt it = S;
	for (; it != E; it++) {
		UIDOOneColor* OneColor = new UIDOOneColor();
		OneColor->DOShuffle = this;
		m_color_indices.push_back(OneColor);
		OneColor->Color[0] = color_get_R(it->first)/255.f;
		OneColor->Color[1] = color_get_G(it->first) / 255.f;
		OneColor->Color[2] = color_get_B(it->first) / 255.f;
		for (DOIt do_it = it->second.begin(); do_it != it->second.end(); do_it++) 
		{
			EDetail* dd = 0;
			for (CDetailManager::DetailIt d_it = DM->objects.begin(); d_it != DM->objects.end(); d_it++)
				if (0 == strcmp(((EDetail*)(*d_it))->GetName(), (*do_it)->GetName())) { dd = (EDetail*)*d_it; break; }
			VERIFY(dd);
			OneColor->list.push_back(dd->GetName());
		}
	}
}

void UIDOShuffle::OnItemFocused(const char* name)
{
	if (!name)
	{
		if (m_Texture)
		{
			IM_TEXTURE_RELEASE(m_Texture);
			m_Texture = nullptr;
		}

		m_Props->ClearProperties();
		m_list_selected = -1;
		return;
	}

	m_Thm = ImageLib.CreateThumbnail(name, EImageThumbnail::ETObject);
	if (m_Texture)
	{
		IM_TEXTURE_RELEASE(m_Texture);
		m_Texture = nullptr;
	}
	
	if (m_Thm)
		m_Thm->Update((ID3DBaseTexture*&)m_Texture);

	xr_delete(m_Thm);

	EDetail *dd= DM->FindDOByName(name);
	VERIFY(dd);
	PropItemVec items;

	//PHelper().CreateCaption(items, "Ref Name", dd->GetName());
	PHelper().CreateFloat(items, "Density", &dd->m_fDensityFactor, 0.1f, 1.0f);
	PHelper().CreateFloat(items, "Min Scale", &dd->m_fMinScale, 0.1f, 100.0f);
	PHelper().CreateFloat(items, "Max Scale", &dd->m_fMaxScale, 0.1f, 100.f);
	PHelper().CreateFlag32(items, "No Waving", &dd->m_Flags, DO_NO_WAVING);

	m_Props->AssignItems(items);
}

bool UIDOShuffle::FindItem(const char* name)
{
	for (xr_string& nm : m_list)
	{
		if (nm == name)return true;
	}
	return false;
}

void UIDOShuffle::ClearIndexForms()
{
	for (int i = 0; i < m_color_indices.size(); i++)xr_delete(m_color_indices[i]);
	m_color_indices.clear();
}

bool UIDOShuffle::ApplyChanges(bool msg )
{
	DM->RemoveColorIndices();
	for (u32 k = 0; k < m_color_indices.size(); k++) {
		UIDOOneColor* OneColor = m_color_indices[k];
		if (OneColor->list.size())
		{
			u32 clr = color_rgba_f(OneColor->Color[0], OneColor->Color[1], OneColor->Color[2], 1.f);
			for (xr_string&i:OneColor->list)
				DM->AppendIndexObject(clr, i.c_str(), false);
		}
	}
	if (/*bNeedUpdate||*/bModif&& msg)
	{
		ELog.DlgMsg(mtInformation, "Object or object list changed. Reinitialize needed!");
		DM->InvalidateSlots();
		return true;
	}
	return false;
}

