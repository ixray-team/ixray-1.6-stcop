#include "stdafx.h"
#include "UIEditLibrary.h"
#include "../../xrECore/Editor/Library.h"

static FS_FileSet modif_map;
UIEditLibrary* UIEditLibrary::Form = nullptr;

UIEditLibrary::UIEditLibrary()
{
	m_ObjectList = new UIItemListForm();
	InitObjects();
	m_ObjectList->SetOnItemFocusedEvent(TOnILItemFocused(this, &UIEditLibrary::OnItemFocused));
	m_ObjectList->m_Flags.set(UIItemListForm::fMultiSelect, true);
	m_Props = new UIPropertiesForm();
	m_PropsObjects = new UIPropertiesForm();
	m_Preview = false;
	m_SelectLods = false;
	m_RealTexture = nullptr;
}

void UIEditLibrary::OnItemFocused(ListItem* item)
{
	m_RealTexture = nullptr;
	m_Props->ClearProperties();
	m_Current = nullptr;
	if (item)
	{
		PropItemVec Info;

		m_Current = item->Key();
		EObjectThumbnail* m_Thm = (EObjectThumbnail*)ImageLib.CreateThumbnail(m_Current, EImageThumbnail::ETObject);

		if (m_Thm && m_Thm->_FaceCount() != 0 && m_Thm->_VertexCount() != 0)
		{
			m_Thm->Update(m_RealTexture);
			m_Thm->FillInfo(Info);
			m_Props->AssignItems(Info);
		}
		else
		{
			PHelper().CreateCaption(Info, g_pStringTable->translate("ed_st_face_count").c_str(), g_pStringTable->translate("ed_st_no_thm").c_str());
			PHelper().CreateCaption(Info, g_pStringTable->translate("ed_st_vertex_count").c_str(), g_pStringTable->translate("ed_st_no_thm").c_str());
			m_Props->AssignItems(Info);
		}

		if (m_Preview)
		{
			ListItemsVec vec;
			vec.push_back(item);
			SelectionToReference(&vec);
		}

		if (bShowProps)
			OnPropertiesClick();
	}
	else
	{
		bShowProps = false;
	}

	UI->RedrawScene();
}

UIEditLibrary::~UIEditLibrary() 
{
	xr_delete(m_PropsObjects);
	xr_delete(m_Props);
}

void UIEditLibrary::InitObjects()
{
	ListItemsVec items;
	FS_FileSet   lst;

	if (Lib.GetObjects(lst))
	{
		FS_FileSetIt it = lst.begin();
		FS_FileSetIt _E = lst.end();
		for (; it != _E; it++)
		{
			xr_string fn;
			ListItem* I = LHelper().CreateItem(items, it->name.c_str(), 0, ListItem::flDrawThumbnail, 0);
		}
	}

	m_ObjectList->AssignItems(items);
}

void UIEditLibrary::Update()
{
	if (!Form)
		return;

	if (!Form->IsClosed())
		Form->Draw();
	else
		Close();
}

void UIEditLibrary::Show()
{
	UI->BeginEState(esEditLibrary);

	if (!Form)
	{
		Form = new UIEditLibrary();
	}
	else
	{
		Form->bOpen = true;
	}
	UI->Push(Form, false);
	modif_map.clear();
}

void UIEditLibrary::Close()
{
	UI->EndEState(esEditLibrary);
	// TODO: возможно еще кого то надо грохнуть
	xr_delete(Form);
}

void UIEditLibrary::DrawObjects()
{
	ImGui::BeginChild(g_pStringTable->translate("ed_st_obj_list").c_str());
	ImGui::Separator();

	m_ObjectList->Draw();
	ImGui::Separator();
	ImGui::EndChild();

	if (ImGui::IsItemHovered())
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
}

void UIEditLibrary::DrawObject(CCustomObject* obj, const char* name)
{
}

void UIEditLibrary::GenerateLOD(RStringVec& props, bool bHighQuality)
{
	u32      lodsCnt = 0;
	SPBItem* pb = UI->ProgressStart(props.size(), g_pStringTable->translate("ed_st_making_lod").c_str());

	for (const shared_str& str : props)
	{
		RStringVec reference;
		reference.push_back(str);
		ChangeReference(reference);   // select item

		R_ASSERT(m_pEditObjects.size() == 1);
		CSceneObject* SO = m_pEditObjects[0];
		CEditableObject* O = SO->GetReference();

		if (O && O->IsMUStatic())
		{
			pb->Inc(O->GetName());
			BOOL bLod = O->m_objectFlags.is(CEditableObject::eoUsingLOD);
			O->m_objectFlags.set(CEditableObject::eoUsingLOD, FALSE);
			xr_string tex_name;
			tex_name = EFS.ChangeFileExt(O->GetName(), "");

			string_path tmp;
			strcpy(tmp, tex_name.c_str());
			_ChangeSymbol(tmp, '\\', '_');
			tex_name = xr_string("lod_") + tmp;
			tex_name = ImageLib.UpdateFileName(tex_name);
			ImageLib.CreateLODTexture(O, tex_name.c_str(), LOD_IMAGE_SIZE, LOD_IMAGE_SIZE, LOD_SAMPLE_COUNT, O->Version(), bHighQuality ? 4 /*7*/ : 1);
			O->OnDeviceDestroy();
			O->m_objectFlags.set(CEditableObject::eoUsingLOD, bLod);
			ELog.Msg(mtInformation, g_pStringTable->translate("ed_st_lod_create_success").c_str(), O->GetName());
			lodsCnt++;
		}
		else
			ELog.Msg(mtError, g_pStringTable->translate("ed_st_lod_non_multiple_usage").c_str(), SO->RefName());

		if (UI->NeedAbort())
			break;
	}

	UI->ProgressEnd(pb);

	if (lodsCnt)
		ELog.DlgMsg(mtInformation, g_pStringTable->translate("ed_st_lods_created_msg").c_str(), lodsCnt);
}

void UIEditLibrary::MakeLOD(bool bHighQuality)
{
	// if (ebSave->Enabled)
	// {
	//     ELog.DlgMsg(mtError, "& Save library changes before generating LOD.");
	//         return;
	// }

	int res = ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo | mbCancel, g_pStringTable->translate("ed_st_lod_multiple_obj").c_str());

	if (res == mrCancel)
		return;

	if (res == mrNo)
	{
		RStringVec sel_items;
		for (ListItem* ListItem : m_ObjectList->m_SelectedItems)
		{
			sel_items.push_back(ListItem->Key());
		}

		GenerateLOD(sel_items, bHighQuality);
		return;
	}

	R_ASSERT(res == mrYes);
	UIChooseForm::SelectItem(smObject, 512, 0);
	m_SelectLods = true;
	m_HighQualityLod = true;
	// answer is handled in DrawRightBar
}

void UIEditLibrary::OnMakeThmClick()
{
	U32Vec       pixels;

	if (m_ObjectList->m_SelectedItems.empty())
		return;

	// m_Items->GetSelected(NULL, sel_items, false);
	ListItemsIt it = m_ObjectList->m_SelectedItems.begin();
	ListItemsIt it_e = m_ObjectList->m_SelectedItems.end();

	for (; it != it_e; ++it)
	{
		ListItem* item = *it;
		CEditableObject* obj = Lib.CreateEditObject(item->Key());

		if (obj && m_Preview)
		{
			string_path fn;
			FS.update_path(fn, _objects_, ChangeFileExt(obj->GetName(), ".thm").c_str());

			// m_Items->SelectItem(item->Key(), true, false, true);
			if (ImageLib.CreateOBJThumbnail(fn, obj, obj->Version()))
				ELog.Msg(mtInformation, g_pStringTable->translate("ed_st_thumbnail_create_success").c_str());
		}
		else
			ELog.DlgMsg(mtError, g_pStringTable->translate("ed_st_thumbail_no_preview").c_str());

		Lib.RemoveEditObject(obj);
	}

	//ELog.DlgMsg(mtInformation, "+ Done.");
}

void UIEditLibrary::OnPropertiesClick()
{
	m_PropsObjects->ClearProperties();
	PropItemVec Info;
	bShowProps = true;

	const xr_string InitTex = "texture_";
	for (ListItem* ListItem : m_ObjectList->m_SelectedItems)
	{
		CSceneObject* SO = new CSceneObject(nullptr, nullptr);
		SO->SetReference(ListItem->Key());
		CEditableObject* NE = SO->GetReference();

		/////////////////////////////////////////////
		NE->FillBasicProps("", Info);
		for (auto Surf : NE->m_Surfaces)
		{
			NE->FillSurfaceProps(Surf, Surf->_GameMtlName(), Info);
		}

		xr_delete(SO);
	}

	m_PropsObjects->AssignItems(Info);
	m_PropsObjects->SetModifiedEvent(OnModified);
}

void UIEditLibrary::DrawRightBar()
{

	if (bShowProps)
	{
		ImGui::Begin(g_pStringTable->translate("ed_st_objects_properties").c_str(), &bShowProps);
		m_PropsObjects->Draw();
		ImGui::End();
	}

	if (ImGui::BeginChild(g_pStringTable->translate("ed_st_right").c_str(), ImVec2(0, 0)))
	{
		ImGui::Image(m_RealTexture ? m_RealTexture : EDevice->texture_null->pSurface, ImVec2(200, 200));

		m_Props->Draw();

		if (ImGui::Button(g_pStringTable->translate("ed_st_properties").c_str(), ImVec2(-1, 0)))
			OnPropertiesClick();
		if (ImGui::IsItemHovered())
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);

		// Make Thumbnail & Lod
		{
			bool enableMakeThumbnailAndLod = !m_ObjectList->m_SelectedItems.empty() && m_Preview;

			if (!enableMakeThumbnailAndLod)
			{
				ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
				ImGui::PushStyleVar(ImGuiStyleVar_Alpha, ImGui::GetStyle().Alpha * 0.5f);
			}

			if (ImGui::Button(g_pStringTable->translate("ed_st_make_thumbnail").c_str(), ImVec2(-1, 0)))
			{
				OnMakeThmClick();

				for (auto Item : m_ObjectList->m_SelectedItems)
				{
					OnItemFocused(Item);
				}
			}
			if (ImGui::IsItemHovered())
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);

			if (ImGui::Button(g_pStringTable->translate("ed_st_make_lod_hq").c_str(), ImVec2(-1, 0)))
				MakeLOD(true);
			if (ImGui::IsItemHovered())
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);

			if (ImGui::Button(g_pStringTable->translate("ed_st_make_lod_lq").c_str(), ImVec2(-1, 0)))
				MakeLOD(false);
			if (ImGui::IsItemHovered())
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);

			if (!enableMakeThumbnailAndLod)
			{
				ImGui::PopItemFlag();
				ImGui::PopStyleVar();
			}
		}

		if (ImGui::Checkbox(g_pStringTable->translate("ed_st_preview").c_str(), &m_Preview))
			OnPreviewClick();

		if (ImGui::IsItemHovered())
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		if (ImGui::Button(g_pStringTable->translate("ed_st_remove_obj").c_str(), ImVec2(-1, 0)))
		{
			m_ObjectList->RemoveSelectItem();
		}
		
		if (ImGui::IsItemHovered())
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		if (ImGui::Button(g_pStringTable->translate("ed_st_import_obj").c_str(), ImVec2(-1, 0)))
		{
			ImportClick();
		}

		if (ImGui::IsItemHovered())
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		if (ImGui::Button(g_pStringTable->translate("ed_st_export_obj").c_str(), ImVec2(-1, 0)))
		{
			ExportObj();
		}

		ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
		ImGui::PushStyleVar(ImGuiStyleVar_Alpha, ImGui::GetStyle().Alpha * 0.5f);
		if (ImGui::IsItemHovered())
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		if (ImGui::Button(g_pStringTable->translate("ed_st_rename_obj").c_str(), ImVec2(-1, 0)))
		{
			
		}
		if (ImGui::IsItemHovered())
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		if (ImGui::Button(g_pStringTable->translate("ed_st_export_lwo").c_str(), ImVec2(-1, 0)))
		{
		}

		if (ImGui::IsItemHovered())
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);

		ImGui::PopItemFlag();
		ImGui::PopStyleVar();

		if (!IsModify)
		{
			ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
			ImGui::PushStyleVar(ImGuiStyleVar_Alpha, ImGui::GetStyle().Alpha * 0.5f);
			RenderSaveButton();
			ImGui::PopItemFlag();
			ImGui::PopStyleVar();
		}
		else
		{
			RenderSaveButton();
		}

		if (ImGui::IsItemHovered())
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);

		if (ImGui::Button(g_pStringTable->translate("ed_st_close").c_str(), ImVec2(-1, 0)))
			Close();
		if (ImGui::IsItemHovered())
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
	}
	ImGui::EndChild();

	if (m_SelectLods)
	{
		bool       change = false;
		SStringVec lst;

		if (UIChooseForm::GetResult(change, lst))
		{
			if (change)
			{
				RStringVec selItems;

				for (const xr_string& xrStr : lst)
					selItems.push_back(xrStr.c_str());

				GenerateLOD(selItems, m_HighQualityLod);
			}

			m_SelectLods = false;
		}

		UIChooseForm::Update();
	}
}

void UIEditLibrary::RenderSaveButton()
{
	if (ImGui::Button(g_pStringTable->translate("ed_st_save").c_str(), ImVec2(-1, 0)))
	{
		RStringVec sel_strings;
		ChangeReference(sel_strings);
		Lib.Save(&modif_map);
		modif_map.clear();
		RefreshSelected();
		IsModify = false;
	}
}

void UIEditLibrary::OnPreviewClick()
{
	RefreshSelected();
}

void UIEditLibrary::RefreshSelected()
{
	bool mt = false;

	if (m_Preview)
	{
		if (m_ObjectList->m_SelectedItems.empty())
		{
			ListItemsVec vec;
			for (ListItem* ListItem : m_ObjectList->m_SelectedItems)
			{
				vec.push_back(ListItem);
			}
			mt = SelectionToReference(&vec);
		}
		else
			mt = SelectionToReference(nullptr);
	}

	UI->RedrawScene();
}

/// ---------------------------------------------------------------------------
bool UIEditLibrary::SelectionToReference(ListItemsVec* props)
{
	RStringVec   sel_strings;
	ListItemsVec sel_items;

	if (props)
		sel_items = *props;
	// else
	// m_Items->GetSelected(NULL, sel_items, false /*true*/);

	ListItemsIt it = sel_items.begin();
	ListItemsIt it_e = sel_items.end();

	for (; it != it_e; ++it)
	{
		ListItem* item = *it;
		sel_strings.push_back(item->Key());
	}
	ChangeReference(sel_strings);
	return sel_strings.size() > 0;
}

void UIEditLibrary::ShowProperty()
{
	
}

#include "../xrECore/Editor/ExportObjectOGF.h"

void UIEditLibrary::ExportOneOBJ(CEditableObject* EO)
{
	string_path			fn;
	FS.update_path(fn, _import_, EO->m_LibName.c_str());
	CExportObjectOGF 	E(EO);
	CMemoryWriter 		F;
	if (E.ExportAsWavefrontOBJ(F, fn))
	{
		strcat(fn, ".obj");
		F.save_to(fn);
	}
}

void UIEditLibrary::ExportObj()
{
	if (!m_Preview)
	{
		SPBItem* pb = UI->ProgressStart(m_pEditObjects.size(), g_pStringTable->translate("ed_st_exporting_obj").c_str());
		CSceneObject* SO = new CSceneObject((LPVOID)0, (LPSTR)0);

		for (ListItem* item : m_ObjectList->m_SelectedItems)
		{
			pb->Inc(item->Key());
			SO->SetReference(item->Key());
			CEditableObject* NE = SO->GetReference();
			SO->UpdateTransform();
			if (NE)
			{
				SO->FPosition = NE->t_vPosition;
				SO->FScale = NE->t_vScale;
				SO->FRotation = NE->t_vRotate;

				ExportOneOBJ(NE);
			}
		}

		if (UI->NeedAbort())
			xr_delete(SO);

		UI->ProgressEnd(pb);
	}
	else
	{
		xr_vector<CSceneObject*>::iterator it = m_pEditObjects.begin();
		xr_vector<CSceneObject*>::iterator it_e = m_pEditObjects.end();
		SPBItem* pb = UI->ProgressStart(m_pEditObjects.size(), g_pStringTable->translate("ed_st_exporting_obj").c_str());
		for (; it != it_e; ++it)
		{
			CSceneObject* SO = *it;
			CEditableObject* O = SO->GetReference();
			pb->Inc(O->GetName());

			if (O)
			{
				ExportOneOBJ(O);
			}
			if (UI->NeedAbort()) 	break;
		}
		UI->ProgressEnd(pb);
	}
	ELog.DlgMsg(mtInformation, g_pStringTable->translate("ed_st_done").c_str());
}

void UIEditLibrary::OnModified()
{
	if (!Form) 				return;
	Form->IsModify = true;

	auto it = Form->m_ObjectList->m_SelectedItems.begin();
	auto it_e = Form->m_ObjectList->m_SelectedItems.end();
	for (; it != it_e; ++it)
	{
		CSceneObject* SO = new CSceneObject(nullptr, nullptr);
		SO->SetReference((*it)->Key());
		CEditableObject* E = SO->GetReference();
		if (E)
		{
			modif_map.insert(FS_File(E->GetName()));
			E->Modified();
			SO->UpdateTransform();
		}
	}
	UI->RedrawScene();
}

void UIEditLibrary::ChangeReference(const RStringVec& items)
{
	xr_vector<CSceneObject*>::iterator it = m_pEditObjects.begin();
	xr_vector<CSceneObject*>::iterator it_e = m_pEditObjects.end();
	for (; it != it_e; ++it)
	{
		CSceneObject* SO = *it;
		xr_delete(SO);
	}

	m_pEditObjects.clear();

	RStringVec::const_iterator sit = items.begin();
	RStringVec::const_iterator sit_e = items.end();

	for (; sit != sit_e; ++sit)
	{
		CSceneObject* SO = new CSceneObject((LPVOID)0, (LPSTR)0);
		m_pEditObjects.push_back(SO);
		SO->SetReference((*sit).c_str());

		CEditableObject* NE = SO->GetReference();
		if (NE)
		{
			SO->FPosition = NE->t_vPosition;
			SO->FScale = NE->t_vScale;
			SO->FRotation = NE->t_vRotate;
		}
		// update transformation
		SO->UpdateTransform();
	}

	ExecCommand(COMMAND_EVICT_OBJECTS);
	ExecCommand(COMMAND_EVICT_TEXTURES);
}

void UIEditLibrary::OnRender()
{
	if (!Form || !Form->bOpen)
	{
		if (UI->GetEState() == EEditorState::esEditLibrary)
		{
			UI->EndEState(EEditorState::esEditLibrary);
		}
		return;
	}
	else if (UI->GetEState() != EEditorState::esEditLibrary)
	{
		UI->BeginEState(EEditorState::esEditLibrary);
	}

	if (!Form->m_Preview)
		return;

	for (auto& it : Form->m_pEditObjects)
	{
		CSceneObject* SO = it;
		CSceneObject* S = SO;

		CEditableObject* O = SO->GetReference();
		if (O)
		{
			S->m_RT_Flags.set(S->flRT_Visible, true);

			if (!S->FPosition.similar(O->t_vPosition))
				S->FPosition = O->t_vPosition;

			if (!S->FRotation.similar(O->t_vRotate))
				S->FRotation = O->t_vRotate;

			if (!S->FScale.similar(O->t_vScale))
				S->FScale = O->t_vScale;

			SO->OnFrame();
			SO->RenderSingle();
			// static bool strict = true;
			// SO->Render(0, strict);
		}
	}
}

void UIEditLibrary::Draw()
{
	ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(550, 650));

	if (!ImGui::Begin(g_pStringTable->translate("ed_st_obj_lib").c_str(), &bOpen))
	{
		ImGui::PopStyleVar(1);
		ImGui::End();
		return;
	}

	{
		ImGui::BeginGroup();

		if (ImGui::BeginChild(g_pStringTable->translate("ed_st_left").c_str(), ImVec2(-200, -ImGui::GetFrameHeight() - 4), true))
			DrawObjects();

		ImGui::EndChild();
		ImGui::SetNextItemWidth(-200);
		ImGui::Text(g_pStringTable->translate("ed_st_items_count").c_str(), m_ObjectList->m_Items.size());
		// ImGui::InputText("##value", m_Filter, sizeof(m_Filter));
		ImGui::EndGroup();
	}

	ImGui::SameLine();
	DrawRightBar();

	ImGui::PopStyleVar(1);
	ImGui::End();
}

void UIEditLibrary::ImportClick()
{
	xr_string open_nm, save_nm, nm;
	if (EFS.GetOpenName(_import_, open_nm, true))
	{
		// remove selected object
		// load
		AStringVec 				lst;
		_SequenceToList(lst, open_nm.c_str());
		bool bNeedUpdate = false;
		// folder name
		AnsiString 				folder;


		xr_string m_LastSelection;
		for (AStringIt it = lst.begin(); it != lst.end(); ++it)
		{
			nm = ChangeFileExt(EFS.ExtractFileName((*it).c_str()), "").c_str();
			CEditableObject* O = new CEditableObject(nm.c_str());
			FS.TryLoad(*it);
			if (O->Load(it->c_str()))
			{
				save_nm = xr_string(FS.get_path(_objects_)->m_Path) + folder.c_str() + EFS.ChangeFileExt(nm, ".object");

				if (FS.exist(save_nm.c_str()))
					if (mrNo == ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, g_pStringTable->translate("ed_st_obj_exist").c_str(), nm.c_str()))
					{
						xr_delete(O);
						break;
					}

				O->Save(save_nm.c_str());
				EFS.MarkFile(it->c_str(), true);
				bNeedUpdate = true;
			}
			else
				ELog.DlgMsg(mtError, g_pStringTable->translate("Can't load file '%s'.").c_str(), it->c_str());

			xr_delete(O);

			LPCSTR p = FS.get_path(_objects_)->m_Path;
			if (folder.Contains(p))
			{
				m_LastSelection = xr_string(folder.c_str() + strlen(p)) + nm;
				xr_strlwr(m_LastSelection);
			}
			else 
			{
				m_LastSelection = xr_string(folder.c_str()) + nm;
			}
		}
		if (bNeedUpdate)
		{
			Lib.CleanLibrary();
			InitObjects();
			//m_Items->SelectItem(m_LastSelection.c_str(), true, false, true);
		}
	}
}