#include "stdafx.h"
#include "../../Editor/Utils/TerrainGarbageGenerator.h"
#include "IconsFontAwesome6.h"

static xr_atomic_bool RefreshInProgress;

UIObjectTool::UIObjectTool()
{
	m_selPercent = 0.f;
	m_MultiAppend = false;
	m_PropRandom = false;
	m_Current = nullptr;
	m_RandomAppend = false;
	m_Selection = false;
	m_RealTexture = nullptr;
	m_RemoveTexture = nullptr;
	m_ObjectList = new UIItemListForm();
	m_ObjectList->SetOnItemFocusedEvent({this, &UIObjectTool::OnItemFocused});
	m_TextureNull.create("ed\\ed_nodata");
	m_TextureNull->Load();

	m_Props = new UIPropertiesForm();
	m_Props->SetFitMode(true);
	m_Props->DisableSearch(true);
	RefreshList();

	PropsRandomAppend.DisableSearch(true);
}

UIObjectTool::~UIObjectTool()
{
	while (RefreshInProgress)
	{
		std::this_thread::yield();
	}

	m_RemoveTexture.destroy();
	m_RealTexture.destroy();

	xr_delete(m_Props);
	m_TextureNull.destroy();
	xr_delete(m_ObjectList);
}


void UIObjectTool::HandleDragDrop()
{
	const ImGuiPayload* payload = ImGui::GetDragDropPayload();

	if (payload && ImGui::IsMouseDragging(ImGuiMouseButton_Left) && GUIManager->DnDType == EDragDropType::RandomAppend)
	{
		ImDrawList* draw_list = ImGui::GetWindowDrawList();
		ImVec2 p_min = ImGui::GetItemRectMin();
		ImVec2 p_max = ImGui::GetItemRectMax();
		draw_list->AddRectFilled(p_min, p_max, IM_COL32(50, 50, 70, 100));
		draw_list->AddRect(p_min, p_max, IM_COL32(100, 180, 255, 255));
	}

	if (!ImGui::BeginDragDropTarget())
		return;

	auto ImData = ImGui::AcceptDragDropPayload("TEST#rai");

	if (ImData == nullptr)
	{
		ImGui::EndDragDropTarget();
		return;
	}
	struct DragDropData
	{
		xr_string FileName;
	} Data = *(DragDropData*)ImData->Data;


	if (Data.FileName.ends_with(".rai"))
	{
		LoadFromFile(Data.FileName);
	}

	ImGui::EndDragDropTarget();
}

void UIObjectTool::Draw()
{
	const float ButtonHeight		= XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
	const float	ItemSpacingX		= ImGui::GetStyle().ItemSpacing.x;
	const float TableRowHeight		= XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableRowHeight);

	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		if (XRay::ImGui::Button("Multiple Append in World Center", { -0.01, 0 }))
		{
			m_MultiAppend = true;
			UIChooseForm::SelectItem(smObject, 512, 0);
		}
		XRay::ImGui::Separator();

		if (XRay::ImGui::ToggleButton("Random Append", m_RandomAppend, { -(ButtonHeight + ItemSpacingX) * 2, ButtonHeight }))
		{
			ParentTools->ActivateAppendRandom(m_RandomAppend);
		}
		ImGui::SameLine(0, ItemSpacingX);
		if (XRay::ImGui::Button(ICON_FA_FILE_IMPORT, { ButtonHeight, ButtonHeight }))
		{
			xr_string Outfile;

			if (EFS.GetOpenName("$server_data_root$", Outfile, false, 0, -1, "*.rai"))
			{
				LoadFromFile(Outfile);
			}
		}
		ImGui::SameLine(0, ItemSpacingX);
		if (XRay::ImGui::Button(ICON_FA_FLOPPY_DISK, { ButtonHeight, ButtonHeight }))
		{
			xr_string Outfile;

			if (EFS.GetSaveName("$server_data_root$", Outfile, 0, -1, "*.rai"))
			{
				if (!Outfile.ends_with(".rai"))
				{
					Outfile += ".rai";
				}

				IWriter* Stream = FS.w_open(Outfile.data());
				Stream->w_u8(1);

				Stream->w_fvector3(ParentTools->m_AppendRandomMinScale);
				Stream->w_fvector3(ParentTools->m_AppendRandomMaxScale);
				Stream->w_fvector3(ParentTools->m_AppendRandomMinRotation);
				Stream->w_fvector3(ParentTools->m_AppendRandomMaxRotation);
				Stream->w_u32(ParentTools->m_Flags.get());

				Stream->w_stringZ(ParentTools->m_AppendRandomObjectsStr);

				Stream->w_u32((u32)ParentTools->m_AppendRandomObjects.size());

				for (const shared_str& str : ParentTools->m_AppendRandomObjects)
				{
					Stream->w_stringZ(str);
				}

				FS.w_close(Stream);

				xr_path File = Outfile;
				RAIFile = File.xfilename();
			}
		}

		if (m_RandomAppend)
		{
			ParentTools->FillAppendRandomPropertiesBegin(PropsRandomAppend);

			PropsRandomAppend.Draw();

			if (XRay::ImGui::Button("Random Append in Selected Object", { -0.01, 0 }))
			{
				GenerateGarbage();
			}
			XRay::ImGui::Separator();
		}

		m_RemoveTexture.destroy();

		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		if (XRay::ImGui::BeginExpand("Reference Select"))
		{
			if (XRay::ImGui::BeginTable("##objecttools_refselect", 4, ImGuiTableFlags_BordersInner | ImGuiTableFlags_RowBg))
			{
												ImGui::TableSetupColumn("Key", ImGuiTableColumnFlags_WidthFixed);					ImGui::TableSetupColumn("-", ImGuiTableColumnFlags_WidthFixed);													ImGui::TableSetupColumn("--", ImGuiTableColumnFlags_WidthFixed);												ImGui::TableSetupColumn("--", ImGuiTableColumnFlags_WidthStretch);
				XRay::ImGui::TableNextRow();	XRay::ImGui::TableNextColumn();	XRay::ImGui::TextFramed("Select by Current: ");		XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button(" +", { GUIManager->ScaleByDpi(24.f), 0})) { SelByRefObject(true); }		XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button(" -", { GUIManager->ScaleByDpi(24.f), 0})) { SelByRefObject(false); }
				XRay::ImGui::TableNextRow();	XRay::ImGui::TableNextColumn();	XRay::ImGui::TextFramed("Select by Selected: ");	XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button("=%", { GUIManager->ScaleByDpi(24.f), 0})) { MultiSelByRefObject(true); }	XRay::ImGui::TableNextColumn();		if (XRay::ImGui::Button("+%", { GUIManager->ScaleByDpi(24.f), 0})) { MultiSelByRefObject(false); }	XRay::ImGui::TableNextColumn();		ImGui::SetNextItemWidth(-TableRowHeight); ImGui::DragFloat("%", &m_selPercent, 1, 0, 100, "%.1f");
				XRay::ImGui::EndTable();
			}

			XRay::ImGui::EndExpand();
		}

		static bool ShowSurf = false;
		if (XRay::ImGui::BeginExpand("Surface"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;

			if (XRay::ImGui::Button("Clear Select", { SizeX, 0 }))
			{
				Scene->UndoSave();
				ClearSurface(true);
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Clear Level", { SizeX, 0 }))
			{
				if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to reset surface in level?") == mrYes)
				{
					Scene->UndoSave();
					ClearSurface(false);
				}
			}

			XRay::ImGui::EndExpand();
		}

		if (XRay::ImGui::BeginExpand("Current Object"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;

			if (XRay::ImGui::Button("Select", { SizeX, 0 }))
			{
				UIChooseForm::SelectItem(smObject, 1, m_Current, 0, 0, 0, 0, 0);
				m_Selection = true;
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Refresh", { SizeX, 0 }))
			{
				RefreshList();
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
	}
}

void UIObjectTool::GenerateGarbage()
{
	static EGarbageGenerator Generator;
	ESceneObjectTool* ObjectToolPtr = static_cast<ESceneObjectTool*>(Scene->GetTool(OBJCLASS_SCENEOBJECT));
	auto ObjectList = ObjectToolPtr->GetObjects();

	bool Placed = false;

	for (CCustomObject* Object : ObjectList)
	{
		if (!Object->Selected())
			continue;

		Generator.Generate((CSceneObject*)Object);
		Placed = true;
	}

	if (!Placed)
	{
		ELog.DlgMsg(mtInformation, mbOK, "An object or terrain must be selected!");
	}
}

void UIObjectTool::DrawRandomAppend()
{
	if (ImGui::BeginChild("##objecttools_randomappend", { 0, 0 }, ImGuiChildFlags_AutoResizeY | ImGuiChildFlags_AlwaysAutoResize))
	{
		if (!RAIFile.empty())
		{
			ImVec4 TextColor = { 1.f, 1.f, 0.7f, 1.f };
			ImGui::TextColored(TextColor, RAIFile.data());
		}

		float ButtonSize = ImGui::GetWindowSize().x / 2 - 10;
		if (ImGui::Button("Multiple Append", { ButtonSize , 0 }))
		{
			UIChooseForm::SelectItem(smObject, 512, 0);
			m_MultiAppend = true;
		}

		ImGui::SameLine();
		ImGui::BeginDisabled(ParentTools->m_AppendRandomObjects.empty());
		if (ImGui::Button("Generate Garbage", { ButtonSize , 0 }))
		{
			static EGarbageGenerator Generator;
			ESceneObjectTool* ObjectToolPtr = static_cast<ESceneObjectTool*>(Scene->GetTool(OBJCLASS_SCENEOBJECT));
			auto ObjectList = ObjectToolPtr->GetObjects();

			bool Placed = false;

			for (CCustomObject* Object : ObjectList)
			{
				if (!Object->Selected())
					continue;

				Generator.Generate((CSceneObject*)Object);
				Placed = true;
			}

			if (!Placed)
			{
				ELog.DlgMsg(mtInformation, mbOK, "An object or terrain must be selected!");
			}
		}
		ImGui::EndDisabled();

		if (ImGui::Checkbox("Random Append", &m_RandomAppend))
		{
			ParentTools->ActivateAppendRandom(m_RandomAppend);
		}
		ImGui::SameLine();

		if (ImGui::Button("Load"))
		{
			xr_string Outfile;

			if (EFS.GetOpenName("$server_data_root$", Outfile, false, 0, -1, "*.rai"))
			{
				LoadFromFile(Outfile);
			}
		}
		ImGui::SameLine();

		ImGui::BeginDisabled(ParentTools->m_AppendRandomObjects.empty());
		if (ImGui::Button("Save"))
		{
			xr_string Outfile;

			if (EFS.GetSaveName("$server_data_root$", Outfile, 0, -1, "*.rai"))
			{
				if (!Outfile.ends_with(".rai"))
				{
					Outfile += ".rai";
				}

				IWriter* Stream = FS.w_open(Outfile.data());
				Stream->w_u8(1);

				Stream->w_fvector3(ParentTools->m_AppendRandomMinScale);
				Stream->w_fvector3(ParentTools->m_AppendRandomMaxScale);
				Stream->w_fvector3(ParentTools->m_AppendRandomMinRotation);
				Stream->w_fvector3(ParentTools->m_AppendRandomMaxRotation);
				Stream->w_u32(ParentTools->m_Flags.get());

				Stream->w_stringZ(ParentTools->m_AppendRandomObjectsStr);

				Stream->w_u32((u32)ParentTools->m_AppendRandomObjects.size());

				for (const shared_str& str : ParentTools->m_AppendRandomObjects)
				{
					Stream->w_stringZ(str);
				}

				FS.w_close(Stream);

				xr_path File = Outfile;
				RAIFile = File.xfilename();
			}
		}
		ImGui::SameLine();
		ImGui::EndDisabled();

		if (ImGui::Button("Custom.."))
		{
			m_PropRandom = true;
			ParentTools->FillAppendRandomPropertiesBegin(PropsRandomAppend);
		}

		ImGui::EndChild();
		HandleDragDrop();
	}
}

void UIObjectTool::LoadFromFile(xr_string& Outfile)
{
	FS.TryLoad(Outfile);
	IReader* Stream = FS.r_open(Outfile.data());
	u8 Ver = Stream->r_u8();

	if (Ver != 1)
	{
		Msg("! Unsupported *.rai file!");
		FS.r_close(Stream);
		return;
	}

	Stream->r_fvector3(ParentTools->m_AppendRandomMinScale);
	Stream->r_fvector3(ParentTools->m_AppendRandomMaxScale);
	Stream->r_fvector3(ParentTools->m_AppendRandomMinRotation);
	Stream->r_fvector3(ParentTools->m_AppendRandomMaxRotation);
	ParentTools->m_Flags.flags = Stream->r_u32();

	Stream->r_stringZ(ParentTools->m_AppendRandomObjectsStr);

	u32 Size = Stream->r_u32();

	ParentTools->m_AppendRandomObjects.resize(Size);
	for (shared_str& str : ParentTools->m_AppendRandomObjects)
	{
		Stream->r_stringZ(str);
	}

	FS.r_close(Stream);

	xr_path File = Outfile;
	RAIFile = File.xfilename();
}

void UIObjectTool::DrawObjectsList()
{
	if (!bDrawList)
		return;

	if (ImGui::Begin("Edit Group Items", &bDrawList))
	{
		if (!RefreshInProgress)
		{
			//if (ImGui::BeginChild("Props"))
			{
				ImGui::Image(m_RealTexture ? m_RealTexture->get_SRView()->GetRawSRV() : (m_TextureNull->get_SRView()->GetRawSRV()), ImVec2(128, 128));
				ImGui::SameLine();
				ImGui::BeginChild("##EGIProps", { 0, 128 });
				m_Props->Draw();
				ImGui::EndChild();

				ImGui::Separator();
			}
			//ImGui::EndChild();
			if (ImGui::BeginChild("##objectslist"))
			{
				m_ObjectList->Draw();
			}
			ImGui::EndChild();
		}
		else
			ImGui::Text("Loading...");
	}
	ImGui::End();
}

void UIObjectTool::RefreshList()
{
	if (RefreshInProgress)
		return;

	string_path ObjectPath = {};

	FS.update_path(ObjectPath, _objects_, "");
	FS.rescan_path(ObjectPath, true);

	std::thread refreshThread(&UIObjectTool::RefreshListInternal, this);
	refreshThread.detach();
}

void UIObjectTool::RefreshListInternal()
{
	RefreshInProgress = true;

	ListItemsVec items;
	FS_FileSet lst;
	
	if (Lib.GetObjects(lst)) 
	{
		FS_FileSetIt	it = lst.begin();
		FS_FileSetIt	_E = lst.end();
		for (; it != _E; it++) {
			xr_string fn;
			ListItem* I = LHelper().CreateItem(items, it->name.c_str(), 0, ListItem::flDrawThumbnail, 0);
		}
	}
	if (m_RealTexture)
		m_RemoveTexture = m_RealTexture;

	m_RealTexture = nullptr;
	m_Props->ClearProperties();
	m_ObjectList->AssignItems(items);

	RefreshInProgress = false;
}

void UIObjectTool::OnDrawUI()
{
	if (m_Selection)
	{
		bool change = false;
		xr_string lst;
		if (UIChooseForm::GetResult(change, lst))
		{
			if (change)
			{
				m_ObjectList->SelectItem(lst.c_str());
			}
			m_Selection = false;
		}

		UIChooseForm::Update();
	}
	if (m_MultiAppend)
	{
		bool change = false;
		SStringVec lst;
		if (UIChooseForm::GetResult(change, lst))
		{
			if (change)
			{
				Fvector pos = { 0.f,0.f,0.f };
				Fvector up = { 0.f,1.f,0.f };
				Scene->SelectObjects(false, OBJCLASS_SCENEOBJECT);

				SPBItem* pb = UI->ProgressStart(lst.size(), "Append object: ");
				for (AStringIt it = lst.begin(); it != lst.end(); it++)
				{
					string256 namebuffer;
					Scene->GenObjectName(OBJCLASS_SCENEOBJECT, namebuffer, it->c_str());
					CSceneObject* obj = new CSceneObject((LPVOID)0, namebuffer);
					CEditableObject* ref = obj->SetReference(it->c_str());
					if (!ref)
					{
						ELog.DlgMsg(mtError, "TfraObject:: Can't load reference object.");
						xr_delete(obj);
						return;
					}
					obj->MoveTo(pos, up);
					Scene->AppendObject(obj);
				}
				UI->ProgressEnd(pb);
			}
			m_MultiAppend = false;
		}
		UIChooseForm::Update();
	}
	if (m_PropRandom)
	{
		if (ParentTools->FillAppendRandomPropertiesEnd())
		{
			m_PropRandom = false;
		}
		UIPropertiesModal::Update();
	}
}
void UIObjectTool::OnItemFocused(ListItem* item)
{
	if (m_RealTexture)m_RemoveTexture = m_RealTexture;
	m_RealTexture = nullptr;

	m_Props->ClearProperties();
	m_Current = nullptr;
	if (item)
	{
		m_Current = item->Key();
		auto * m_Thm = ImageLib.CreateThumbnail(m_Current, EImageThumbnail::ETObject);
		if (m_Thm)
		{
			IRHISurface* Surface = nullptr;
			m_Thm->Update(Surface);

			if (Surface != nullptr)
			{
				m_RealTexture = new CTexture;
				m_RealTexture->surface_set(Surface);

				Surface->Release();
			}

			PropItemVec Info;
			m_Thm->FillInfo(Info);
			m_Props->AssignItems(Info);
		}
	}
}

void UIObjectTool::SelByRefObject(bool flag)
{
	const char* N = Current();
	if (N) {
		ObjectIt _F = Scene->FirstObj(OBJCLASS_SCENEOBJECT);
		ObjectIt _E = Scene->LastObj(OBJCLASS_SCENEOBJECT);
		for (; _F != _E; _F++) {
			if ((*_F)->Visible()) {
				CSceneObject* _O = (CSceneObject*)(*_F);
				if (_O->RefCompare(N)) _O->Select(flag);
			}
		}
	}
}

void UIObjectTool::MultiSelByRefObject(bool clear_prev)
{
	ObjectList 	objlist;
	LPU32Vec 	sellist;
	if (Scene->GetQueryObjects(objlist, OBJCLASS_SCENEOBJECT, 1, 1, -1)) {
		for (ObjectIt it = objlist.begin(); it != objlist.end(); it++) {
			const char* N = ((CSceneObject*)*it)->RefName();
			ObjectIt _F = Scene->FirstObj(OBJCLASS_SCENEOBJECT);
			ObjectIt _E = Scene->LastObj(OBJCLASS_SCENEOBJECT);
			for (; _F != _E; _F++) {
				CSceneObject* _O = (CSceneObject*)(*_F);
				if ((*_F)->Visible() && _O->RefCompare(N)) {
					if (clear_prev) {
						_O->Select(false);
						sellist.push_back((u32*)_O);
					}
					else {
						if (!_O->Selected())
							sellist.push_back((u32*)_O);
					}
				}
			}
		}
		std::sort(sellist.begin(), sellist.end());
		sellist.erase(std::unique(sellist.begin(), sellist.end()), sellist.end());
		random_shuffle(sellist.begin(), sellist.end());
		int max_k = iFloor(float(sellist.size()) / 100.f * float(m_selPercent) + 0.5f);
		int k = 0;
		for (LPU32It o_it = sellist.begin(); k < max_k; o_it++, k++) {
			CSceneObject* _O = (CSceneObject*)(*o_it);
			_O->Select(true);
		}
	}
}

void UIObjectTool::ClearSurface(bool selected)
{
	{
		ObjectIt _F = Scene->FirstObj(OBJCLASS_SCENEOBJECT);
		ObjectIt _E = Scene->LastObj(OBJCLASS_SCENEOBJECT);
		for (; _F != _E; _F++) {
			if ((*_F)->Visible()) {
				CSceneObject* _O = (CSceneObject*)(*_F);
				if ((_O->Selected() && _O->Visible())||!selected)
				{
					_O->ClearSurface();
				}
			}
		}
	}
}
