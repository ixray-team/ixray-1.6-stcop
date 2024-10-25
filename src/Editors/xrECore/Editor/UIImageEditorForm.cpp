//---------------------------------------------------------------------------
#include "stdafx.h"
#include "UIImageEditorForm.h"
#include "EThumbnail.h"
UIImageEditorForm* UIImageEditorForm::Form = nullptr;
UIImageEditorForm::UIImageEditorForm()
{
	m_ItemProps = new UIPropertiesForm();
	m_ItemList = new UIItemListForm();
	m_ItemList->SetOnItemFocusedEvent(TOnILItemFocused(this,&UIImageEditorForm::OnItemsFocused));
	m_ItemList->SetOnItemRemoveEvent(TOnItemRemove(&ImageLib, &CImageManager::RemoveTexture));
	m_Texture = nullptr;
	m_bFilterImage = true;
	m_bFilterCube = true;
	m_bFilterBump = true;
	m_bFilterNormal = true;
	m_bFilterTerrain = true;
	m_bUpdateProperties = false;
	m_TextureRemove = nullptr;
}

UIImageEditorForm::~UIImageEditorForm()
{
	if (m_Texture)m_Texture->Release();
	xr_delete(m_ItemList);
	xr_delete(m_ItemProps);
}

void UIImageEditorForm::Draw()
{
	IsDocked = ImGui::IsWindowDocked();
	IsFocused = ImGui::IsWindowFocused();

	if (m_bUpdateProperties)
	{
		UpdateProperties();
		m_bUpdateProperties = false;
	}

	if (m_TextureRemove)
	{
		m_TextureRemove->Release();
		m_TextureRemove = nullptr;
	}

	ImGui::Columns(2);
	{
		float MinWidth = ImGui::GetWindowWidth() * 0.35f;
		if (ImGui::GetColumnWidth() < MinWidth)
			ImGui::SetColumnOffset(1, MinWidth);

		ImGui::BeginGroup();
		ImGui::BeginChild("Left", ImVec2(0, -ImGui::GetFrameHeight() - (bImportMode ? 4 : 24)), true, ImGuiWindowFlags_HorizontalScrollbar);
		{
			m_ItemList->Draw();
			if (!IsDocked)
				IsDocked = ImGui::IsWindowDocked();
			if (!IsFocused)
				IsFocused = ImGui::IsWindowFocused();
		}
		ImGui::EndChild();

		if (!bImportMode)
		{
			if (ImGui::Checkbox("Image", &m_bFilterImage))FilterUpdate(); ImGui::SameLine();
			if (ImGui::Checkbox("Cube", &m_bFilterCube))FilterUpdate(); ImGui::SameLine();
			if (ImGui::Checkbox("Bump", &m_bFilterBump))FilterUpdate(); ImGui::SameLine();
			if (ImGui::Checkbox("Normal", &m_bFilterNormal))FilterUpdate(); ImGui::SameLine();
			if (ImGui::Checkbox("Terrain", &m_bFilterTerrain))FilterUpdate();
		}

		if (ImGui::Button("Close"))
		{
			HideLib();
		}

		ImGui::SameLine();
		if (ImGui::Button("Ok"))
		{
			UpdateLib();
			HideLib();
		}

		if (!bImportMode)
		{
			ImGui::SameLine();
			if (ImGui::Button("Remove Texture"))
			{
				m_ItemList->RemoveSelectItem();
			}
		}
		else {
			ImGui::SameLine();
			if(ImGui::Button("Selected")) {
				UpdateSelected();
				HideLib();
			}
		}
		ImGui::EndGroup();
	}

	ImGui::NextColumn();
	{
		ImGui::BeginChild("Right", ImVec2(0, 0));
		{
			if (m_Texture == nullptr)
			{
				u32 mem = 0;
				m_Texture = RImplementation.texture_load("ed\\ed_nodata", mem);
			}
			ImGui::Image(m_Texture, ImVec2(128, 128));
			m_ItemProps->Draw();

			if (!IsDocked)
				IsDocked = ImGui::IsWindowDocked();
			if (!IsFocused)
				IsFocused = ImGui::IsWindowFocused();
		}
		ImGui::EndChild();
	}
}

void UIImageEditorForm::Update()
{
	if (Form)
	{
		if (!Form->IsClosed())
		{
			Form->BeginDraw();
			ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(600, 400));
			if (ImGui::Begin("ImageEditor", nullptr, ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoScrollbar))
			{
				Form->Draw();
			}
			ImGui::PopStyleVar(1);
			ImGui::End();
			Form->EndDraw();
		}
		else
		{
			xr_delete(Form);
		}
	}
}

void UIImageEditorForm::Show(bool bImport)
{
	if(Form == nullptr) {
		Form = new UIImageEditorForm();
	}
	Form->bImportMode = bImport;
	//. form->ebRebuildAssociation->Enabled = !bImport;
	Form->bReadonlyMode = !FS.can_write_to_alias(_textures_);
	if (Form->bReadonlyMode)
	{
		Log("#!You don't have permisions to modify textures.");
		Form->m_ItemProps->SetReadOnly(TRUE);
	}
	Form->modif_map.clear();
	Form->InitItemList();
}

void UIImageEditorForm::FindInEditor(xr_string fn, bool bImport) {
	if(Form && (Form->bImportMode || bImport)) {
		Form->HideLib();
		Form = NULL;
		//	xr_delete(Form);
	}
	if(!Form) {
		if(bImport) {
			if(!Form) {
				Form = new UIImageEditorForm();
			}
			Form->texture_map.insert(fn);
		}

		Show(bImport);
	}
	if(Form) {
		Form->m_ItemList->SelectItem(fn.data());
	}
}

void UIImageEditorForm::ImportTextures()
{
	VERIFY(!Form);
	FS_FileSet TextureMap;
	int new_cnt = ImageLib.GetLocalNewTextures(TextureMap);
	if (new_cnt)
	{
		if (ELog.DlgMsg(mtInformation, "Found %d new texture(s)", new_cnt))
		{
			Form = new UIImageEditorForm();
			Form->texture_map.swap(TextureMap);
			Show(true);
		}
	}
	else
	{
		ELog.DlgMsg(mtInformation, "Can't find new textures.");
	}
}

ETextureThumbnail* UIImageEditorForm::FindUsedTHM(const shared_str& name)
{
	THMMapIt it = m_THM_Used.find(name);
	if (it != m_THM_Used.end())
		return it->second;

	ETextureThumbnail* thm = new ETextureThumbnail(name.c_str(), false);
	m_THM_Used[name] = thm;

	if (bImportMode)
	{
		xr_string fn = name.c_str();
		ImageLib.UpdateFileName(fn);

		if (!thm->Load(name.c_str(), _import_))
		{
			bool bLoad = thm->Load(fn.c_str(), _game_textures_);
			ImageLib.CreateTextureThumbnail(thm, name.c_str(), _import_, !bLoad);
		}
	}
	else
	{
		thm->Load();
	}
	return thm;
}

void UIImageEditorForm::RegisterModifiedTHM()
{
	if (m_ItemProps->IsModified() || bImportMode)
	{
		for (THMIt t_it = m_THM_Current.begin(); t_it != m_THM_Current.end(); ++t_it)
		{
			FS_FileSetIt it = texture_map.find(FS_File((*t_it)->SrcName()));
			R_ASSERT(it != texture_map.end());
			modif_map.insert(*it);
		}
	}
}

void UIImageEditorForm::OnCubeMapBtnClick(ButtonValue* value, bool& bModif, bool& bSafe)
{
	ButtonValue* B = dynamic_cast<ButtonValue*>(value); R_ASSERT(B);
	bModif = false;
	switch (B->btn_num) {
	case 0: 
	{
		RStringVec items;
		if (0 != m_ItemList->GetSelected(items))
		{
			for (RStringVecIt it = items.begin(); it != items.end(); it++)
			{
				xr_string new_name = xr_string(it->c_str()) + "#small";
				ImageLib.CreateSmallerCubeMap(it->c_str(), new_name.c_str());
			}
		}
	}
	break;
	}
}

void UIImageEditorForm::OnTypeChange(PropValue* prop)
{
	m_bUpdateProperties = true;
}

void UIImageEditorForm::UpdateProperties()
{
	ListItemsVec vec;
	m_ItemList->GetSelected(nullptr, vec, false);

	if (vec.size() == 1)
	{
		m_ItemProps->ClearProperties();
		OnItemsFocused(vec[0]);
	}
}

void UIImageEditorForm::InitItemList()
{
	R_ASSERT(m_THM_Used.empty());
	if (!bImportMode)
		ImageLib.GetTexturesRaw(texture_map);
	/*
		FS_FileSet				flist;
		FS.file_list			(flist,"$game_textures$",FS_ListFiles|FS_ClampExt,"*.thm");
		Msg						("TfrmImageLib::InitItemsList count of .thm files=%d", flist.size());
		FS_FileSetIt It			= flist.begin();
		FS_FileSetIt It_e		= flist.end();
		string256 				tex_name;
		for(;It!=It_e;++It)
		{
			shared_str sn 		=(*It).name.c_str();
			FindUsedTHM			(sn);
		}
	*/

	ListItemsVec 			items;
	// fill
	FS_FileSetIt it = texture_map.begin();
	FS_FileSetIt _E = texture_map.end();
	for (; it != _E; it++)
	{
		ListItem* I = LHelper().CreateItem(items, it->name.c_str(), 0);
		I->m_Object = (void*)(FindUsedTHM(it->name.c_str()));
		R_ASSERT2(I->m_Object, it->name.c_str());
	}
	m_ItemList->AssignItems(items);
}

void UIImageEditorForm::HideLib()
{
	bOpen = false;
	ImGui::CloseCurrentPopup();
}

void UIImageEditorForm::UpdateLib()
{
	VERIFY(!bReadonlyMode);
	RegisterModifiedTHM();
	SaveUsedTHM();
	if (bImportMode && !texture_map.empty())
	{
		AStringVec modif;
		ImageLib.SafeCopyLocalToServer(texture_map);
		// rename with folder
		FS_FileSet files = texture_map;
		texture_map.clear();
		xr_string               fn;
		FS_FileSetIt it = files.begin();
		FS_FileSetIt _E = files.end();

		for (; it != _E; it++)
		{
			fn = EFS.ChangeFileExt(it->name.c_str(), "");
			ImageLib.UpdateFileName(fn);
			FS_File				F(*it);
			F.name = fn;
			texture_map.insert(F);
		}
		// sync
		ImageLib.SynchronizeTextures(true, true, true, &texture_map, &modif);
		ImageLib.RefreshTextures(&modif);
	}
	else
	{
		// save game textures
		if (modif_map.size())
		{
			AStringVec modif;
			ImageLib.SynchronizeTextures(true, true, true, &modif_map, &modif);
			ImageLib.RefreshTextures(&modif);
		}
	}
}

void UIImageEditorForm::UpdateSelected() 
{
	texture_map.clear();
	RStringVec items; 
	string_path fn{};

	if(m_ItemList->GetSelected(items)) 
	{
		for(auto item : items) {
			if(auto file = FS.exist(fn, _import_, *item)) 
			{
				texture_map.insert(xr_string(item.c_str()));
			}
		}
	}
	UpdateLib();
}

void UIImageEditorForm::OnItemsFocused(ListItem* item)
{
	PropItemVec props;

	RegisterModifiedTHM();
	m_THM_Current.clear();
	m_TextureRemove = m_Texture;
	m_Texture = nullptr;

	if (ListItem* prop = item)
	{
		ETextureThumbnail* thm = FindUsedTHM(prop->Key());
		m_THM_Current.push_back(thm);
		m_ItemProps->ClearProperties();

		// fill prop
		thm->FillProp(props, PropValue::TOnChange(this, &UIImageEditorForm::OnTypeChange));

		if (thm->_Format().type == STextureParams::ttCubeMap)
		{
			ButtonValue* B = PHelper().CreateButton(props, "CubeMap\\Edit", "Make Small", 0);
			B->OnBtnClickEvent.bind(this, &UIImageEditorForm::OnCubeMapBtnClick);
		}

		thm->Update(m_Texture);
	}

	m_ItemProps->AssignItems(props);
}

void UIImageEditorForm::SaveUsedTHM()
{
	for (THMMapIt t_it = m_THM_Used.begin(); t_it != m_THM_Used.end(); ++t_it)
	{
		if (modif_map.find(FS_File(t_it->second->SrcName())) != modif_map.end())
			t_it->second->Save();
	}
}

void UIImageEditorForm::FilterUpdate()
{
	const ListItemsVec& items = m_ItemList->GetItems();

	u32 cnt = items.size();
	for (u32 k = 0; k < cnt; ++k)
	{
		ListItem* I = items[k];

		ETextureThumbnail* thm = (ETextureThumbnail*)I->m_Object;

		BOOL bVis = FALSE;
		int type = thm->_Format().type;
		if (STextureParams::ttImage == type&&m_bFilterImage)
			bVis = TRUE;
		else if (STextureParams::ttCubeMap == type && m_bFilterCube)
			bVis = TRUE;
		else if (STextureParams::ttBumpMap== type && m_bFilterBump)
			bVis = TRUE;
		else if (STextureParams::ttNormalMap == type && m_bFilterNormal)
			bVis = TRUE;
		else if (STextureParams::ttTerrain == type && m_bFilterTerrain)
			bVis = TRUE;

		I->Visible(bVis);
		
	}
	m_ItemList->ClearSelected();
}