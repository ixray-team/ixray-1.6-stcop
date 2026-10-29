//---------------------------------------------------------------------------
#include "stdafx.h"
#include "UIImageEditorForm.h"
#include "EThumbnail.h"
#include "ImageManager.h"

#include <RedImage/RedImage.hpp>

UIImageEditorForm* UIImageEditorForm::Form = nullptr;

UIImageEditorForm::UIImageEditorForm()
{
	m_ItemProps = new UIPropertiesForm();
	m_ItemList = new UIItemListForm();
	m_ItemList->m_Flags.set(UIItemListForm::fMultiSelect, true);

	m_ItemList->SetOnItemsFocusedEvent({this,&UIImageEditorForm::OnItemsFocused});
	m_ItemList->SetOnItemRemoveEvent({&ImageLib, &CImageManager::RemoveTexture});

	m_bFilterImage = true;
	m_bFilterCube = true;
	m_bFilterBump = true;
	m_bFilterNormal = true;
	m_bFilterTerrain = true;
	m_bUpdateProperties = false;
}

UIImageEditorForm::~UIImageEditorForm()
{
	m_Texture.destroy();;

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

	if (TextureRemove)
	{
		TextureRemove.destroy();
		TextureRemove = nullptr;
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
			ImGui::SameLine();
			ImGui::Checkbox("Delete Sources", &m_bDeleteSourceTextures);
		}
		ImGui::EndGroup();
	}

	ImGui::NextColumn();
	{
		ImGui::BeginChild("Right", ImVec2(0, 0));
		{
			DrawPreview();
			m_ItemProps->Draw();

			if (!IsDocked)
				IsDocked = ImGui::IsWindowDocked();
			if (!IsFocused)
				IsFocused = ImGui::IsWindowFocused();
		}
		ImGui::EndChild();
	}
}

void UIImageEditorForm::DrawPreview()
{
	static const char* ModeNames[] = {"Image", "Sphere", "Box", "Plane"};
	static constexpr float PreviewHeight = 220.f;

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0.f, ImGui::GetStyle().ItemSpacing.y));
	for (int Index = 0; Index < int(std::size(ModeNames)); ++Index)
	{
		const ImDrawFlags Rounding = Index == 0 ? ImDrawFlags_RoundCornersLeft : Index == int(std::size(ModeNames)) - 1 ? ImDrawFlags_RoundCornersRight : ImDrawFlags_RoundCornersNone;
		bool Active = int(PreviewMode) == Index;

		ImGui::PushID(Index);
		if (XRay::ImGui::ToolbarButton("##PreviewMode", ModeNames[Index], &Active, { 0.f, 0.f }, Rounding))
		{
			PreviewMode = EPreviewMode(Index);
		}
		ImGui::PopID();

		if (Index + 1 < int(std::size(ModeNames)))
		{
			ImGui::SameLine();
		}
	}
	ImGui::PopStyleVar();

	// The 3D preview shows the last focused texture with its edited, unsaved parameters
	ETextureThumbnail* Thm = m_THM_Current.empty() ? nullptr : m_THM_Current.back();
	if (PreviewMode != EPreviewMode::Image && Thm != nullptr)
	{
		switch (PreviewMode)
		{
		case EPreviewMode::Sphere:	Preview.SetShape(CMaterialPreview::EShape::Sphere); break;
		case EPreviewMode::Box:		Preview.SetShape(CMaterialPreview::EShape::Box); break;
		default:					Preview.SetShape(CMaterialPreview::EShape::Plane); break;
		}

		Preview.ShowShapeSelector = false;
		Preview.Draw(Thm->_Format(), ImVec2(0.f, PreviewHeight));
		return;
	}

	if (m_Texture == nullptr)
	{
		u32 mem = 0;

		auto* baseTexture = ::RImplementation.texture_load("ed\\ed_nodata", mem);
		if (baseTexture)
		{
			m_Texture = new CTexture;
			m_Texture->surface_set(baseTexture);
			baseTexture->Release();
		}
	}
	ImGui::Image(m_Texture && m_Texture->get_SRView() ? m_Texture->get_SRView()->GetRawSRV() : nullptr, ImVec2(128, 128));
}

void UIImageEditorForm::Update()
{
	if (Form)
	{
		if (!Form->IsClosed())
		{
			bool NeedShow = true;
			ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(600, 400));
			if (ImGui::Begin("Image Editor", &NeedShow, ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoScrollbar))
			{
				Form->Draw();
			}
			ImGui::PopStyleVar(1);
			ImGui::End();

			if (!NeedShow)
			{
				Form->HideLib();
			}
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
		Form->m_ItemProps->SetReadOnly(true);
	}
	Form->modif_map.clear();
	Form->InitItemList();
}

void UIImageEditorForm::FindInEditor(xr_string fn, bool bImport)
{
	if (Form && (Form->bImportMode || bImport))
	{
		Form->HideLib();
		Form = nullptr;
	}

	if (!Form)
	{
		if (bImport)
		{
			Form = new UIImageEditorForm();
			Form->texture_map.insert(fn);
		}

		Show(bImport);
	}

	if (Form)
	{
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
		if (ELog.DlgMsg(mtInformation, mbOK|mbCancel, "Found %d new texture(s)", new_cnt) == mrOK)
		{
			Form = new UIImageEditorForm();
			Form->texture_map.swap(TextureMap);
			Show(true);
		}
	}
	else
	{
		ELog.DlgMsg(mtInformation, mbOK, "Can't find new textures.");
	}
}

ETextureThumbnail* UIImageEditorForm::FindUsedTHM(const shared_str& name)
{
	static xrCriticalSection THMUsedCS;
	ETextureThumbnail* thm = nullptr;

	{
		xrCriticalSectionGuard guard(THMUsedCS);
		THMMapIt it = m_THM_Used.find(name);
		if (it != m_THM_Used.end())
		{
			return it->second;
		}

		thm = new ETextureThumbnail(name.c_str(), false);
		m_THM_Used[name] = thm;
	}
		
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

		if (!thm->Valid())
		{
			string_path RawFileName = {};
			FS.update_path(RawFileName, _textures_, name.c_str());
			RedImageTool::RedImage Image;

			xr_string FullFileName = RawFileName;
			FullFileName += ".tga";

			if (Image.LoadFromFile(FullFileName.c_str()))
			{
				Image.SwapRB();

				u8* SizedPixels = new u8[THUMB_SIZE * 4];
				RedImageTool::RedTextureUtils::Scale(SizedPixels, THUMB_WIDTH, THUMB_HEIGHT, (u8*)*Image, Image.GetWidth(), Image.GetHeight(), Image.GetFormat(), RedImageTool::RedResizeFilter::Box);

				thm->m_Pixels.resize(THUMB_SIZE * 4);

				const int RowSize = THUMB_WIDTH * 4;
				u8* InvertedPixels = new u8[THUMB_SIZE * 4];

				for (int y = 0; y < THUMB_HEIGHT; y++)
				{
					int SrcY = THUMB_HEIGHT - 1 - y;
					memcpy(InvertedPixels + y * RowSize, SizedPixels + SrcY * RowSize, RowSize);
				}

				memcpy(thm->m_Pixels.data(), InvertedPixels, THUMB_SIZE * 4);

				xr_delete(SizedPixels);
				xr_delete(InvertedPixels);

				thm->HasPreview = true;
			}
		}
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

	//if (vec.size() == 1)
	{
		m_ItemProps->ClearProperties();
		OnItemsFocused(vec);
	}
}

void UIImageEditorForm::InitItemList()
{
	R_ASSERT(m_THM_Used.empty());
	if (!bImportMode)
	{
		ImageLib.GetTexturesRaw(texture_map);
	}

	ListItemsVec items;
	xr_vector<xr_pair<ListItem*, str_c>> list_elems;
	list_elems.reserve(items.size());
	for (auto& elem : texture_map)
	{
		list_elems.emplace_back(LHelper().CreateItem(items, elem.name.c_str(), 0), elem.name.c_str());
	}
	xr_parallel_for(size_t(0), list_elems.size(), [&](size_t i)
	{
		auto& elem = list_elems[i];
		elem.first->m_Object = FindUsedTHM(elem.second);
		R_ASSERT2(elem.first->m_Object, elem.second);
		Msg("Load THM for %s", elem.second);
	});
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
	ImageLib.CommitTextureLib(bImportMode, texture_map, modif_map, m_bDeleteSourceTextures);
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

void UIImageEditorForm::OnItemsFocused(ListItemsVec& item)
{
	PropItemVec props;

	RegisterModifiedTHM();
	m_THM_Current.clear();
	TextureRemove = m_Texture;
	m_Texture = nullptr;

	m_ItemProps->ClearProperties();
	for (ListItem* prop : item)
	{
		ETextureThumbnail* thm = FindUsedTHM(prop->Key());
		m_THM_Current.push_back(thm);

		// fill prop
		thm->FillProp(props, PropValue::TOnChange(this, &UIImageEditorForm::OnTypeChange));

		if (thm->_Format().type == STextureParams::ttCubeMap)
		{
			ButtonValue* B = PHelper().CreateButton(props, "CubeMap\\Edit", "Make Small", 0);
			B->OnBtnClickEvent.bind(this, &UIImageEditorForm::OnCubeMapBtnClick);
		}

		IRHISurface* Surf = nullptr;
		thm->Update(Surf);

		m_Texture = new CTexture;
		m_Texture->surface_set(Surf);
		Surf->Release();

		Preview.SetTexture(prop->Key());
	}

	if (item.empty())
	{
		Preview.SetTexture("");
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

		bool bVis = false;
		int type = thm->_Format().type;
		if (STextureParams::ttImage == type&&m_bFilterImage)
			bVis = true;
		else if (STextureParams::ttCubeMap == type && m_bFilterCube)
			bVis = true;
		else if (STextureParams::ttBumpMap== type && m_bFilterBump)
			bVis = true;
		else if (STextureParams::ttNormalMap == type && m_bFilterNormal)
			bVis = true;
		else if (STextureParams::ttTerrain == type && m_bFilterTerrain)
			bVis = true;

		I->Visible(bVis);
		
	}
	m_ItemList->ClearSelected();
}