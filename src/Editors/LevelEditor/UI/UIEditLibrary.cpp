#include "stdafx.h"
#include "UIEditLibrary.h"
#include "imgui_internal.h"

#include "../xrEUI/IconsFontAwesome6.h"

#include "../../xrECore/Editor/Library.h"
#include "../xrECore/Editor/ExportObjectOGF.h"

static FS_FileSet ModifyMap;
UIEditLibrary* UIEditLibrary::Form = nullptr;

static void ViewportFocusCallback()
{
	LUI->EndEState(esEditScene);
	LUI->BeginEState(esEditLibrary);
}

UIEditLibrary::UIEditLibrary()
{
	ObjectList = new UIItemListForm();
	InitObjects();
	ObjectList->SetOnItemFocusedEvent({this, &UIEditLibrary::OnItemFocused});
	ObjectList->SetOnItemUnfocusedEvent({this, &UIEditLibrary::OnItemUnfocused});
	ObjectList->m_Flags.set(UIItemListForm::fMultiSelect, true);

	InternalProps = new UIPropertiesForm();
	PreviewProps = new UIPropertiesForm();
	PreviewProps->DisableSearch(true);

	IsPreview = true;
	SelectLods = false;
	RealTexture = nullptr;

	View.OnFocusCallback = (xr_delegate<void()>)ViewportFocusCallback;
	xr_strcpy(View.ViewportName, "Render##ObjectLibrary");
	
	SearchList.SetOnItemFocusedEvent({this, &UIEditLibrary::OnItemFocused});
	SearchList.SetOnItemUnfocusedEvent({this, &UIEditLibrary::OnItemUnfocused});
}

void UIEditLibrary::OnItemFocused(ListItem* item)
{
	RealTexture = nullptr;
	PreviewProps->ClearProperties();
	CurrentKey = nullptr;

	if (item)
	{
		PropItemVec Info;

		CurrentKey = item->Key();
		EObjectThumbnail* m_Thm = (EObjectThumbnail*)ImageLib.CreateThumbnail(CurrentKey, EImageThumbnail::ETObject);

		if (m_Thm && m_Thm->_FaceCount() != 0 && m_Thm->_VertexCount() != 0)
		{
			IRHISurface* Surface = nullptr;
			m_Thm->Update(Surface);

			RealTexture = new CTexture();
			RealTexture->surface_set(Surface);
			Surface->Release();

			m_Thm->FillInfo(Info);
			PreviewProps->AssignItems(Info);
		}
		else
		{
			PHelper().CreateCaption(Info, "Faces", "THM not found");
			PHelper().CreateCaption(Info, "Vertexes", "THM not found");
			PreviewProps->AssignItems(Info);
		}

		if (IsPreview)
		{
			FocusedItems = ActualItemList().m_SelectedItems;
			SelectionToReference(&FocusedItems);
		}

		OnPropertiesClick();
	}

	UI->RedrawScene();
}

void UIEditLibrary::OnItemUnfocused(ListItem* item)
{
	if (!IsPreview)
		return;

	if (item != nullptr)
	{
		auto Iter = std::find(FocusedItems.begin(), FocusedItems.end(), item);
		
		if (Iter != FocusedItems.end())
		{
			FocusedItems.erase(Iter);
			SelectionToReference(&FocusedItems);
		}
	}
}

UIEditLibrary::~UIEditLibrary() 
{
	xr_delete(PreviewProps);
	xr_delete(InternalProps);
}

void UIEditLibrary::InitObjects()
{
	ListItemsVec Items;
	FS_FileSet List;

	if (Lib.GetObjects(List))
	{
		for (const FS_File& File : List)
		{
			LHelper().CreateItem(Items, File.name.c_str(), 0, ListItem::flDrawThumbnail, 0);
		}
	}

	ObjectList->AssignItems(Items);
}

void UIEditLibrary::Update()
{
	if (!Form)
	{
		return;
	}

	if (!Form->IsClosed())
	{
		Form->Draw();
	}
	else
	{
		Close();
	}
}

UIEditLibrary* UIEditLibrary::Init()
{
	if (!Form)
	{
		Form = new UIEditLibrary();
	}
	return Form;
}

void UIEditLibrary::Show()
{
	UI->BeginEState(esEditLibrary);

	if (!Form)
	{
		Init();
	}

	Form->bOpen = true;
	UI->ActiveTabIndex = Form->TabIndex;
}

void UIEditLibrary::Close()
{
	UI->EndEState(esEditLibrary);
	Form->bOpen = false;
}

void UIEditLibrary::DrawObjects()
{
	ImGui::BeginChild("Object List");

	// Поиск
	string256 TempBuff;
	xr_strcpy(TempBuff, SearchQuery.c_str());
	ImGui::SetNextItemWidth(-1);
	if (ImGui::InputTextWithHint("##value", "Search...", TempBuff, sizeof(TempBuff)))
	{
		SearchQuery = TempBuff;

		if (SearchQuery.empty())
		{
			SearchList.ClearList();
		}
		else
		{
			ListItemsVec Filtered;
			for (ListItem* Item : ObjectList->GetItems())
			{
				if (strstr(Item->Key(), SearchQuery.c_str()))
				{
					ListItem* CopyItem = new ListItem(Item->Type());
					*CopyItem = *Item;
					Filtered.push_back(CopyItem);
				}
			}
			SearchList.AssignItems(Filtered, nullptr, true, false);
		}
	}

	if (GUIManager->SearchIcon)
	{
		ImVec2 IconSize = { 12,12 };

		ImGui::SameLine();
		ImVec2 CursorPos = ImGui::GetCursorPos();
		ImGui::SetCursorPos(ImVec2(CursorPos.x - IconSize.x - 10.f, 1 + CursorPos.y + (IconSize.y / 4)));

		ImGui::Image(GUIManager->SearchIcon, IconSize);
	}
	ImGui::Separator();

	ActualItemList().Draw();

	ImGui::Separator();
	ImGui::EndChild();

	if (ImGui::IsItemHovered())
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
}

void UIEditLibrary::GenerateLOD(const RStringVec& props, bool bHighQuality)
{
	u32 LodsCnt = 0;
	SPBItem* ProgbarState = UI->ProgressStart(props.size(), "Making LOD");

	for (const shared_str& PropStr : props)
	{
		RStringVec Reference;
		Reference.push_back(PropStr);
		ChangeReference(Reference); // select item

		R_ASSERT(m_pEditObjects.size() == 1);
		CSceneObject* SO = m_pEditObjects[0];
		CEditableObject* O = SO->GetReference();

		if (O && O->IsMUStatic())
		{
			ProgbarState->Inc(O->GetName());
			bool HasLod = O->m_objectFlags.is(CEditableObject::eoUsingLOD);
			O->m_objectFlags.set(CEditableObject::eoUsingLOD, false);
			xr_string TexName;
			TexName = EFS.ChangeFileExt(O->GetName(), "");

			string_path TempPath;
			strcpy(TempPath, TexName.c_str());
			_ChangeSymbol(TempPath, '\\', '_');
			TexName = xr_string("lod_") + TempPath;
			TexName = ImageLib.UpdateFileName(TexName);
			ImageLib.CreateLODTexture(O, TexName.c_str(), LOD_IMAGE_SIZE, LOD_IMAGE_SIZE, LOD_SAMPLE_COUNT, xr_chrono_to_time_t(std::chrono::system_clock::now()), bHighQuality ? 4 /*7*/ : 1);
			O->OnDeviceDestroy();
			O->m_objectFlags.set(CEditableObject::eoUsingLOD, HasLod);
			ELog.Msg(mtInformation, "+ LOD for object '%s' successfully created.", O->GetName());
			LodsCnt++;
		}
		else
		{
			ELog.Msg(mtError, "! Can't create LOD texture from non 'Multiple Usage' object.", SO->RefName());
		}

		if (UI->NeedAbort())
		{
			break;
		}
	}

	UI->ProgressEnd(ProgbarState);

	if (LodsCnt)
	{
		ELog.DlgMsg(mtInformation, "+ '%u' LOD's succesfully created.", LodsCnt);
	}
}

static xr_task_group LODTask;
void UIEditLibrary::MakeLOD(bool bHighQuality)
{
	int Result = ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo | mbCancel, "Do you want to select multiple objects?");

	if (Result == mrCancel)
	{
		return;
	}

	if (Result == mrNo)
	{
		RStringVec SelItems;
		for (ListItem* ListItem : ActualItemList().m_SelectedItems)
		{
			SelItems.push_back(ListItem->Key());
		}

		// LODTask.wait();
		// LODTask.run
		//(
		//	[this, sel_items, bHighQuality]()
		{
			GenerateLOD(SelItems, bHighQuality);
		}
		//);

		return;
	}

	UIChooseForm::SelectItem(smObject, 512, 0);
	SelectLods = true;
	m_HighQualityLod = true;
}

void UIEditLibrary::OnMakeThmClick()
{
	for (ListItem* Item : ActualItemList().m_SelectedItems)
	{
		CEditableObject* Object = Lib.CreateEditObject(Item->Key());

		if (Object && IsPreview)
		{
			string_path Filename;
			FS.update_path(Filename, _objects_, ChangeFileExt(Object->GetName(), ".thm").c_str());

			if (ImageLib.CreateOBJThumbnail(Filename, Object, Object->Version()))
			{
				ELog.Msg(mtInformation, "+ Thumbnail successfully created.");
			}
		}
		else
		{
			ELog.DlgMsg(mtError, "& Can't create thumbnail. Set preview mode.");
		}

		Lib.RemoveEditObject(Object);
	}
}

void UIEditLibrary::OnPropertiesClick()
{
	InternalProps->ClearProperties();
	PropItemVec Info;

	const xr_string InitTex = "texture_";
	for (ListItem* ListItem : ActualItemList().m_SelectedItems)
	{
		CSceneObject* SO = new CSceneObject(nullptr, nullptr);
		SO->SetReference(ListItem->Key());
		CEditableObject* NE = SO->GetReference();

		NE->FillBasicProps("", Info);

		AnsiString pref_init = AnsiString("Surfaces");
		{
			auto BatchButton = PHelper().CreateButton(Info, PrepareKey(pref_init.c_str(), "Batch Material Convert"), "All unique,All shared", 0);
			BatchButton->OnBtnClickEvent.bind(NE, &CEditableObject::OnBatchProcessMaterial);
		}
		pref_init.append("\\");

		for (SurfaceIt Iter = NE->m_Surfaces.begin(); Iter != NE->m_Surfaces.end(); Iter++)
		{
			AnsiString	Pref = pref_init + (*Iter)->_Name();
			PropValue* V = PHelper().CreateCaption(Info, Pref.c_str(), "");
			V->tag = (int)*Iter;
			NE->FillSurfaceProps(*Iter, Pref.c_str(), Info);
		}

		xr_delete(SO);
	}

	InternalProps->AssignItems(Info);
	InternalProps->SetModifiedEvent(OnModified);
}

void UIEditLibrary::DrawRightBar()
{
	if (ImGui::BeginChild("Right", ImVec2(0, 0)))
	{
		ImGui::Image(RealTexture ? RealTexture->get_SRView()->GetRawSRV() : EDevice->texture_null->get_SRView()->GetRawSRV(), ImVec2(200, 200));

		PreviewProps->Draw();

		// Make Thumbnail & Lod
		{
			bool EnableMakeThumbnailAndLod = !ActualItemList().m_SelectedItems.empty() && IsPreview;

			if (!EnableMakeThumbnailAndLod)
			{
				ImGui::PushItemFlag(ImGuiItemFlags_Disabled, true);
				ImGui::PushStyleVar(ImGuiStyleVar_Alpha, ImGui::GetStyle().Alpha * 0.5f);
			}

			if (ImGui::Button("Make Thumbnail", ImVec2(-1, 0)))
			{
				UI->RedrawScene(false);

				UI->CommandList[TUI::ECommandListID::CurrentFrame].push_back
				(
					[this]()
					{
						UI->ViewID = View.ViewportID;
						View.OnFocusCallback(); 
					}
				);

				UI->CommandList[TUI::ECommandListID::NextFrame].push_back
				(
					[this]()
					{
						OnMakeThmClick();

						for (auto Item : ActualItemList().m_SelectedItems)
						{
							OnItemFocused(Item);
						} 
					}
				);
			}

			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			}

			ImGui::Text("Make LOD:");
			ImGui::SameLine();

			if (ImGui::Button("HQ", ImVec2(25, 0)))
			{
				MakeLOD(true);
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			}

			ImGui::SameLine();

			if (ImGui::Button("LQ", ImVec2(25, 0)))
			{
				MakeLOD(false);
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			}

			if (!EnableMakeThumbnailAndLod)
			{
				ImGui::PopItemFlag();
				ImGui::PopStyleVar();
			}
		}

		ImGui::SameLine();

		if (ImGui::Checkbox("Dropper", &m_Dropper))
		{
			PickSurface();
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		}
		if (ImGui::Button("Remove Object", ImVec2(-1, 0)))
		{
			ActualItemList().RemoveSelectItem();
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		}
		if (ImGui::Button("Import Object", ImVec2(-1, 0)))
		{
			ImportClick();
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		}
		if (ImGui::Button("Export OBJ", ImVec2(-1, 0)))
		{
			ExportObj();
		}

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
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		}

		if (ImGui::Button("Close", ImVec2(-1, 0)))
		{
			Close();
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		}
	}
	ImGui::EndChild();

	if (SelectLods)
	{
		bool Changed = false;
		SStringVec TempList;

		if (UIChooseForm::GetResult(Changed, TempList))
		{
			if (Changed)
			{
				RStringVec SelItems;

				for (const xr_string& xrStr : TempList)
				{
					SelItems.push_back(xrStr.c_str());
				}

				GenerateLOD(SelItems, m_HighQualityLod);
			}

			SelectLods = false;
		}

		UIChooseForm::Update();
	}
}

void UIEditLibrary::RenderSaveButton()
{
	if (ImGui::Button("Save", ImVec2(-1, 0)))
	{
		RStringVec SelStrings;
		ChangeReference(SelStrings);
		Lib.Save(&ModifyMap);
		ModifyMap.clear();
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

	if (IsPreview)
	{
		if (!ActualItemList().m_SelectedItems.empty())
		{
			ListItemsVec vec;
			for (ListItem* ListItem : ActualItemList().m_SelectedItems)
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

void UIEditLibrary::PickSurface()
{
	if (m_Dropper)
	{
		View.OnClickCallback.bind(this, &UIEditLibrary::PickCallback);
		return;
	}

	View.OnClickCallback.clear();
}

void UIEditLibrary::PickCallback()
{
	Fvector StartDir;
	Fvector StartPos;

	UIPropertiesItem* Itm = InternalProps->FindPropItem(PrevClick.c_str());
	if (Itm != nullptr)
	{
		Itm->SetUnselect();
	}

	UI->CurrentView().m_Camera.MouseRayFromPoint(StartPos, StartDir, View.GetMousePos());

	for (CSceneObject* Obj : m_pEditObjects)
	{
		float dis = UI->ZFar();
		SRayPickInfo pinf;
		pinf.IsForcePickup = true;

		Obj->RayPick(dis, StartPos, StartDir, &pinf);

		if (pinf.e_mesh == nullptr)
			continue;

		CSurface* surf = pinf.e_mesh->GetSurfaceByFaceID(pinf.inf.tris_id);
		PrevClick = AnsiString("Surfaces\\") + AnsiString(surf->_Name());

		UIPropertiesItem* Itm = InternalProps->FindPropItem(PrevClick.c_str());
		
		if (Itm == nullptr)
			continue;
		
		Itm->SetSelect();
	}
}

bool UIEditLibrary::SelectionToReference(ListItemsVec* props)
{
	RStringVec SelStrings;
	ListItemsVec SelItems;

	if (props)
	{
		SelItems = *props;
	}

	for (ListItem* Item : SelItems)
	{
		SelStrings.push_back(Item->Key());
	}
	ChangeReference(SelStrings);
	return SelStrings.size() > 0;
}

void UIEditLibrary::ExportOneOBJ(CEditableObject* EO)
{
	string_path Filename;
	FS.update_path(Filename, _import_, EO->m_LibName.c_str());
	CExportObjectOGF E(EO);
	CMemoryWriter F;

	if (E.ExportAsWavefrontOBJ(F, Filename))
	{
		strcat(Filename, ".obj");
		F.save_to(Filename);
	}
}

void UIEditLibrary::ExportObj()
{
	if (!IsPreview)
	{
		SPBItem* ProgbarState = UI->ProgressStart(m_pEditObjects.size(), "Expotring to OBJ");
		CSceneObject* SO = new CSceneObject((LPVOID)0, (LPSTR)0);

		for (ListItem* SelItem : ActualItemList().m_SelectedItems)
		{
			ProgbarState->Inc(SelItem->Key());
			SO->SetReference(SelItem->Key());
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

		UI->ProgressEnd(ProgbarState);
	}
	else
	{
		SPBItem* ProgbarState = UI->ProgressStart(m_pEditObjects.size(), "Expotring to OBJ");
		for (CSceneObject* SO : m_pEditObjects)
		{
			CEditableObject* O = SO->GetReference();
			ProgbarState->Inc(O->GetName());

			if (O)
			{
				ExportOneOBJ(O);
			}

			if (UI->NeedAbort())
				break;
		}
		UI->ProgressEnd(ProgbarState);
	}
	ELog.DlgMsg(mtInformation, "Done.");
}

void UIEditLibrary::OnModified()
{
	if (!Form)
		return;

	Form->IsModify = true;

	for (ListItem* Item : Form->ActualItemList().m_SelectedItems)
	{
		CSceneObject* SO = new CSceneObject(nullptr, nullptr);
		SO->SetReference(Item->Key());
		CEditableObject* E = SO->GetReference();
		if (E)
		{
			ModifyMap.insert(FS_File(E->GetName()));
			E->Modified();
			SO->UpdateTransform();
		}
	}
	UI->RedrawScene();
}

void UIEditLibrary::ChangeReference(const RStringVec& items)
{
	for (CSceneObject* Obj : m_pEditObjects)
	{
		xr_delete(Obj);
	}

	m_pEditObjects.clear();

	for (const shared_str& String : items)
	{
		CSceneObject* SO = new CSceneObject((LPVOID)0, (LPSTR)0);
		m_pEditObjects.push_back(SO);
		SO->SetReference(*String);

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

	if (!Form->IsPreview)
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
		}
	}
}

void UIEditLibrary::Draw()
{
	ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(550, 650));

	// Render child windows
	if (bOpen)
	{
		if (IsPreview)
		{
			View.Draw();
		}

		ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(400, 400));
		if (ImGui::Begin("Properties##LibraryEditor"))
		{
			InternalProps->Draw();
		}
		ImGui::End();
		ImGui::PopStyleVar(1);
	}

	if (!ImGui::Begin("Object Library", &bOpen))
	{
		ImGui::PopStyleVar(1);
		ImGui::End();
		return;
	}

	{
		ImGui::BeginGroup();

		if (ImGui::BeginChild("Left", ImVec2(-220, -ImGui::GetFrameHeight() - 4), true))
			DrawObjects();

		ImGui::EndChild();
		ImGui::SetNextItemWidth(-200);
		ImGui::Text(" Items count: %u", ActualItemList().m_Items.size());
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
		AStringVec lst;
		_SequenceToList(lst, open_nm.c_str());
		bool bNeedUpdate = false;

		AnsiString folder;
		xr_string LastSelection;

		for (AStringIt it = lst.begin(); it != lst.end(); ++it)
		{
			nm = ChangeFileExt(EFS.ExtractFileName((*it).c_str()), "").c_str();
			CEditableObject* O = new CEditableObject(nm.c_str());
			FS.TryLoad(*it);
			if (O->Load(it->c_str()))
			{
				save_nm = xr_string(FS.get_path(_objects_)->m_Path) + folder.c_str() + EFS.ChangeFileExt(nm, ".object");

				if (FS.exist(save_nm.c_str()))
				{
					if (mrNo == ELog.DlgMsg(mtConfirmation, (mbYes | mbNo), "Object '%s' already exist. Owerwrite it?", nm.c_str()))
					{
						xr_delete(O);
						break;
					}
				}

				O->Save(save_nm.c_str());
				EFS.MarkFile(it->c_str(), true);
				bNeedUpdate = true;
			}
			else
			{
				ELog.DlgMsg(mtError, "Can't load file '%s'.", it->c_str());
			}

			xr_delete(O);

			const char* p = FS.get_path(_objects_)->m_Path;
			if (folder.Contains(p))
			{
				LastSelection = xr_string(folder.c_str() + strlen(p)) + nm;
				xr_strlwr(LastSelection);
			}
			else
			{
				LastSelection = xr_string(folder.c_str()) + nm;
			}
		}
		if (bNeedUpdate)
		{
			Lib.CleanLibrary();
			InitObjects();
		}
	}
}