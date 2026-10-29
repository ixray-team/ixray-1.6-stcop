#include "stdafx.h"
#include "ActorEditor.h"
#include "UIActorLibrary.h"
#include "imgui_internal.h"

#include "IconsFontAwesome7.h"

#include "../../xrECore/Editor/Library.h"
#include "../../xrECore/Editor/ExportObjectOGF.h"
#include "../Editor/Utils/ContentView.h"

static FS_FileSet ModifyMap;
UIActorLibrary* UIActorLibrary::Form = nullptr;

static const char* LibraryWindow = "Library###ActorLibrary";

UIActorLibrary::UIActorLibrary(UIRenderForm* RenderView) :
	View(RenderView)
{
	R_ASSERT(Form == nullptr);
	Form = this;

	ObjectList = new UIItemListForm();
	InitObjects();
	ObjectList->SetOnItemFocusedEvent({this, &UIActorLibrary::OnItemFocused});
	ObjectList->SetOnItemUnfocusedEvent({this, &UIActorLibrary::OnItemUnfocused});
	ObjectList->m_Flags.set(UIItemListForm::fMultiSelect, true);

	Props = new UIPropertiesForm();

	IsPreview = true;
	SelectLods = false;

	SearchList.SetOnItemFocusedEvent({this, &UIActorLibrary::OnItemFocused});
	SearchList.SetOnItemUnfocusedEvent({this, &UIActorLibrary::OnItemUnfocused});
}

void UIActorLibrary::OnItemFocused(ListItem* item)
{
	CurrentKey = nullptr;

	if (item)
	{
		CurrentKey = item->Key();

		if (IsPreview)
		{
			FocusedItems = ActualItemList().m_SelectedItems;
			SelectionToReference(&FocusedItems);
		}

		// A single selected object is opened in the Actor Editor, several are edited together
		if (ActualItemList().m_SelectedItems.size() == 1)
		{
			if (!Syncing)
			{
				OpenSelected();
			}
		}
		else
		{
			FillProperties();
		}
	}

	EContext.UI->RedrawScene();
}

void UIActorLibrary::OnItemUnfocused(ListItem* item)
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

UIActorLibrary::~UIActorLibrary()
{
	View->OnClickCallback.clear();
	RStringVec Empty;
	ChangeReference(Empty);

	xr_delete(Props);
	xr_delete(ObjectList);
	Form = nullptr;
}

void UIActorLibrary::InitObjects()
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

void UIActorLibrary::DrawObjects()
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

void UIActorLibrary::GenerateLOD(const RStringVec& props, bool bHighQuality)
{
	u32 LodsCnt = 0;
	SPBItem* ProgbarState = EContext.UI->ProgressStart(props.size(), "Making LOD");
	ForcePreview = true;

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
			ProgbarState->Inc(O ? O->m_LibName.c_str() : "");
			bool HasLod = O->m_objectFlags.is(CEditableObject::eoUsingLOD);
			O->m_objectFlags.set(CEditableObject::eoUsingLOD, false);
			xr_string TexName;
			TexName = EFS.ChangeFileExt(O->m_LibName.c_str(), "");

			string_path TempPath;
			strcpy(TempPath, TexName.c_str());
			_ChangeSymbol(TempPath, '\\', '_');
			TexName = xr_string("lod_") + TempPath;
			TexName = ImageLib.UpdateFileName(TexName);
			ImageLib.CreateLODTexture(O, TexName.c_str(), LOD_IMAGE_SIZE, LOD_IMAGE_SIZE, LOD_SAMPLE_COUNT, xr_chrono_to_time_t(std::chrono::system_clock::now()), bHighQuality ? 4 /*7*/ : 1);
			O->OnDeviceDestroy();
			O->m_objectFlags.set(CEditableObject::eoUsingLOD, HasLod);
			ELog.Msg(mtInformation, "+ LOD for object '%s' successfully created.", O->m_LibName.c_str());
			LodsCnt++;
		}
		else
		{
			ELog.Msg(mtError, "! Can't create LOD texture from non 'Multiple Usage' object.", SO->RefName());
		}

		if (EContext.UI->NeedAbort())
		{
			break;
		}
	}

	ForcePreview = false;
	EContext.UI->ProgressEnd(ProgbarState);

	if (LodsCnt)
	{
		ELog.DlgMsg(mtInformation, "+ '%u' LOD's succesfully created.", LodsCnt);
	}
}

static xr_task_group LODTask;
void UIActorLibrary::MakeLOD(bool bHighQuality)
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

void UIActorLibrary::OnMakeThmClick()
{
	for (ListItem* Item : ActualItemList().m_SelectedItems)
	{
		CEditableObject* Object = Lib.CreateEditObject(Item->Key());

		if (Object && IsPreview)
		{
			string_path Filename;
			FS.update_path(Filename, _objects_, ChangeFileExt(Object->m_LibName.c_str(), ".thm").c_str());

			if (ImageLib.CreateOBJThumbnail(Filename, Object, Object->Version()))
			{
				ELog.Msg(mtInformation, "+ Thumbnail successfully created.");

				string_path ObjectFile;
				FS.update_path(ObjectFile, _objects_, Object->m_LibName.c_str());
				CContentView::InvalidateIcon(FS.fix_path(EFS.ChangeFileExt(ObjectFile, ".object")));
			}
		}
		else
		{
			ELog.DlgMsg(mtError, "& Can't create thumbnail. Set preview mode.");
		}

		Lib.RemoveEditObject(Object);
	}
}

void UIActorLibrary::UpdateLodChoose()
{
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

void UIActorLibrary::DrawActions()
{
	const float ButtonSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
	const bool HasSelection = !ActualItemList().m_SelectedItems.empty();

	auto Button = [&](const char* Id, const char* Icon, const char* Hint, bool* Toggle, ImDrawFlags Rounding) -> bool
	{
		const bool Pressed = XRay::ImGui::ToolbarButton(Id, Icon, Toggle, { ButtonSize, ButtonSize }, Rounding);
		if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled))
		{
			ImGui::SetTooltip("%s", Hint);
		}
		ImGui::SameLine();
		return Pressed;
	};
	auto Gap = [&]()
	{
		ImGui::SameLine(0, ImGui::GetStyle().ItemSpacing.x * 2);
	};

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));

	ImGui::BeginDisabled(!HasSelection || !IsPreview);
	if (Button("LibThumbnail", ICON_FA_IMAGE, "Make Thumbnail", nullptr, ImDrawFlags_RoundCornersLeft))
	{
		// The next frame shows only the object
		ImageLib.BeginThumbnailCapture();
		EContext.UI->CommandList[TUI::ECommandListID::NextFrame].push_back
		(
			[this]()
			{
				OnMakeThmClick();
				ImageLib.EndThumbnailCapture();

				for (auto Item : ActualItemList().m_SelectedItems)
				{
					OnItemFocused(Item);
				}
			}
		);
	}
	if (Button("LibLodHQ", ICON_FA_GEM, "Make LOD (High Quality)", nullptr, ImDrawFlags_RoundCornersNone))
	{
		MakeLOD(true);
	}
	if (Button("LibLodLQ", ICON_FA_FEATHER, "Make LOD (Low Quality)", nullptr, ImDrawFlags_RoundCornersRight))
	{
		MakeLOD(false);
	}
	ImGui::EndDisabled();
	Gap();

	bool DropperOn = Dropper;
	if (Button("LibDropper", ICON_FA_EYE_DROPPER, "Dropper: pick a surface in the viewport", &DropperOn, ImDrawFlags_RoundCornersAll))
	{
		Dropper = !Dropper;
		PickSurface();
	}
	Gap();

	if (Button("LibImport", ICON_FA_FILE_IMPORT, "Import Object", nullptr, ImDrawFlags_RoundCornersLeft))
	{
		ImportClick();
	}
	ImGui::BeginDisabled(!HasSelection);
	if (Button("LibExportObj", ICON_FA_FILE_EXPORT, "Export OBJ", nullptr, ImDrawFlags_RoundCornersNone))
	{
		ExportObj();
	}
	if (Button("LibRemove", ICON_FA_TRASH_CAN, "Remove Object", nullptr, ImDrawFlags_RoundCornersRight))
	{
		ActualItemList().RemoveSelectItem();
	}
	ImGui::EndDisabled();
	Gap();

	ImGui::BeginDisabled(!IsModify);
	if (Button("LibSave", ICON_FA_FLOPPY_DISK, "Save modified library objects", nullptr, ImDrawFlags_RoundCornersAll))
	{
		SaveModified();
	}
	ImGui::EndDisabled();

	ImGui::PopStyleVar();
	ImGui::NewLine();
}

void UIActorLibrary::OnPreviewClick()
{
	RefreshSelected();
}

void UIActorLibrary::RefreshSelected()
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

	EContext.UI->RedrawScene();
}

bool UIActorLibrary::SelectionToReference(ListItemsVec* props)
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

void UIActorLibrary::ExportOneOBJ(CEditableObject* EO)
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

void UIActorLibrary::ExportObj()
{
	if (!IsPreview)
	{
		SPBItem* ProgbarState = EContext.UI->ProgressStart(m_pEditObjects.size(), "Expotring to OBJ");
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

		if (EContext.UI->NeedAbort())
			xr_delete(SO);

		EContext.UI->ProgressEnd(ProgbarState);
	}
	else
	{
		SPBItem* ProgbarState = EContext.UI->ProgressStart(m_pEditObjects.size(), "Expotring to OBJ");
		for (CSceneObject* SO : m_pEditObjects)
		{
			CEditableObject* O = SO->GetReference();
			ProgbarState->Inc(O ? O->m_LibName.c_str() : "");

			if (O)
			{
				ExportOneOBJ(O);
			}

			if (EContext.UI->NeedAbort())
				break;
		}
		EContext.UI->ProgressEnd(ProgbarState);
	}
	ELog.DlgMsg(mtInformation, "Done.");
}

void UIActorLibrary::ChangeReference(const RStringVec& items)
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
}

void UIActorLibrary::OnRender()
{
	if (!Form || !Form->IsPreview)
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

void UIActorLibrary::Draw()
{
	if (ImGui::Begin(LibraryWindow))
	{
		if (ImGui::BeginChild("List", ImVec2(0, -ImGui::GetFrameHeightWithSpacing())))
		{
			DrawObjects();
		}
		ImGui::EndChild();

		ImGui::TextDisabled("%u objects", (u32)ActualItemList().m_Items.size());
	}
	ImGui::End();

	UpdateLodChoose();
}

void UIActorLibrary::OpenSelected()
{
	ListItemsVec& Selected = ActualItemList().m_SelectedItems;
	if (Selected.empty())
	{
		return;
	}

	string_path FileName;
	FS.update_path(FileName, _objects_, Selected.front()->Key());
	xr_strcat(FileName, ".object");
	ReleaseProperties();

	xr_string File = FS.fix_path(xr_string(FileName));
	xr_strlwr(File);
	if (ATools != nullptr && File == ATools->m_LastFileName.c_str())
	{
		return; // already edited
	}

	UIActorEditorForm::Open(File.c_str());
}

void UIActorLibrary::SyncWithActor(const char* FileName)
{
	if (Form == nullptr || FileName == nullptr || Form->Syncing)
	{
		return;
	}

	// Library key: the path relative to $objects$ without the extension
	xr_string Key = FS.fix_path(xr_string(FileName));
	xr_strlwr(Key);
	xr_string Root = FS.fix_path(xr_string(FS.get_path(_objects_)->m_Path));
	xr_strlwr(Root);
	if (!Key.starts_with(Root.c_str()))
	{
		return;
	}
	Key = EFS.ChangeFileExt(Key.substr(Root.length()), "");

	bool Found = false;
	for (ListItem* Item : Form->ObjectList->GetItems())
	{
		xr_string ItemKey = Item->Key();
		xr_strlwr(ItemKey);
		if (ItemKey == Key)
		{
			Key = Item->Key();
			Found = true;
			break;
		}
	}

	if (!Found)
	{
		return;
	}

	Form->SearchQuery.clear();
	Form->SearchList.ClearList();

	Form->Syncing = true;
	Form->ObjectList->SelectItem(Key.c_str());
	Form->Syncing = false;
}

void UIActorLibrary::FillProperties()
{
	Props->ClearProperties();
	PropItemVec Info;

	for (ListItem* Item : ActualItemList().m_SelectedItems)
	{
		CSceneObject* SO = new CSceneObject(nullptr, nullptr);
		SO->SetReference(Item->Key());
		CEditableObject* NE = SO->GetReference();

		if (NE != nullptr)
		{
			NE->FillBasicProps("", Info);

			for (SurfaceIt Iter = NE->m_Surfaces.begin(); Iter != NE->m_Surfaces.end(); Iter++)
			{
				AnsiString Pref = AnsiString("Surfaces\\") + (*Iter)->_Name();
				PropValue* V = PHelper().CreateCaption(Info, Pref.c_str(), "");
				V->tag = (int)*Iter;
				NE->FillSurfaceProps(*Iter, Pref.c_str(), Info);
			}
		}

		xr_delete(SO);
	}

	Props->AssignItems(Info);
	Props->SetModifiedEvent(OnModified);
	OwnsProperties = true;
}

void UIActorLibrary::ReleaseProperties()
{
	OwnsProperties = false;
}

void UIActorLibrary::DrawProperties()
{
	Props->Draw();
}

void UIActorLibrary::SaveModified()
{
	RStringVec SelStrings;
	ChangeReference(SelStrings);
	Lib.Save(&ModifyMap);
	ModifyMap.clear();
	RefreshSelected();
	IsModify = false;
}

void UIActorLibrary::OnModified()
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
			ModifyMap.insert(FS_File(E->m_LibName.c_str()));
			E->bOnModified = true;
			SO->UpdateTransform();
		}
		xr_delete(SO);
	}
	EContext.UI->RedrawScene();
}

void UIActorLibrary::PickSurface()
{
	if (Dropper)
	{
		View->OnClickCallback.bind(this, &UIActorLibrary::PickCallback);
		return;
	}

	View->OnClickCallback.clear();
}

void UIActorLibrary::PickCallback()
{
	Fvector StartDir;
	Fvector StartPos;

	UIPropertiesItem* Itm = Props->FindPropItem(PrevClick.c_str());
	if (Itm != nullptr)
	{
		Itm->SetUnselect();
	}

	EContext.UI->CurrentView().m_Camera.MouseRayFromPoint(StartPos, StartDir, View->GetMousePos());

	// The edited object: the picked surface is selected in the object items and shown in Properties
	CEditableObject* Edited = ATools != nullptr ? ATools->CurrentObject() : nullptr;
	if (!OwnsProperties && Edited != nullptr)
	{
		float Dist = EContext.UI->ZFar();
		SRayPickInfo Info;
		Info.IsForcePickup = true;
		if (Edited->RayPick(Dist, StartPos, StartDir, Fidentity, &Info) && Info.e_mesh != nullptr)
		{
			CSurface* Surf = Info.e_mesh->GetSurfaceByFaceID(Info.inf.tris_id);
			if (Surf != nullptr)
			{
				ATools->SelectListItem(SURFACES_PREFIX, Surf->_Name(), true, false, true);
			}
		}
		return;
	}

	// Several library objects edited together: the surface row is highlighted
	for (CSceneObject* Obj : m_pEditObjects)
	{
		float dis = EContext.UI->ZFar();
		SRayPickInfo pinf;
		pinf.IsForcePickup = true;

		Obj->RayPick(dis, StartPos, StartDir, &pinf);

		if (pinf.e_mesh == nullptr)
			continue;

		CSurface* surf = pinf.e_mesh->GetSurfaceByFaceID(pinf.inf.tris_id);
		PrevClick = AnsiString("Surfaces\\") + AnsiString(surf->_Name());

		Itm = Props->FindPropItem(PrevClick.c_str());
		if (Itm == nullptr)
			continue;

		Itm->SetSelect();
	}
}

void UIActorLibrary::ImportClick()
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