#include "stdafx.h"
#include "../../Editor/Utils/TerrainGarbageGenerator.h"

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
	m_ObjectList->SetOnItemFocusedEvent(TOnILItemFocused(this, &UIObjectTool::OnItemFocused));
	m_TextureNull.create("ed\\ed_nodata");
	m_TextureNull->Load();

	m_Props = new UIPropertiesForm();
	RefreshList();
}

UIObjectTool::~UIObjectTool()
{
	while (RefreshInProgress)
		std::this_thread::yield();

	if (m_RemoveTexture)m_RemoveTexture->Release();
	if (m_RealTexture)m_RealTexture->Release();
	xr_delete(m_Props);
	m_TextureNull.destroy();
	xr_delete(m_ObjectList);
}

void UIObjectTool::Draw()
{
	ImGui::Checkbox("Show lists", &bDrawList);

	if (m_RemoveTexture)m_RemoveTexture->Release();
	m_RemoveTexture = nullptr;

	ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
	if (ImGui::TreeNode("Commands"))
	{
		ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
		{
			if (ImGui::Button("Multiple Append", ImVec2(-1, 0)))
			{
				UIChooseForm::SelectItem(smObject, 512, 0);
				m_MultiAppend = true;
			}
		}
		ImGui::Separator();
		{
			if (!RAIFile.empty())
			{
				ImVec4 TextColor = { 1.f, 1.f, 0.7f, 1.f };
				ImGui::TextColored(TextColor, RAIFile.data());
			}

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
			}
			ImGui::SameLine();

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

			if (ImGui::Button("Custom..."))
			{
				m_PropRandom = true;
				ParentTools->FillAppendRandomPropertiesBegin();
			}

			if (ImGui::Button("Generate random garbage"))
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
		}
		ImGui::Separator();
		ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
		ImGui::TreePop();
	}
	ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
	if (ImGui::TreeNode("Reference Select"))
	{
		ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
		{
			ImGui::Text("Select by Current: "); ImGui::SameLine(); if (ImGui::Button(" +")) { SelByRefObject(true); } ImGui::SameLine(); if (ImGui::Button(" -")) { SelByRefObject(false); }
			ImGui::Text("Select by Selected:"); ImGui::SameLine(); if (ImGui::Button("=%")) { MultiSelByRefObject(true); } ImGui::SameLine(); if (ImGui::Button("+%")) { MultiSelByRefObject(false); } ImGui::SameLine(); ImGui::SetNextItemWidth(-ImGui::GetTextLineHeight() - 8); ImGui::DragFloat("%", &m_selPercent, 1, 0, 100, "%.1f");
		}
		ImGui::Separator();
		ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
		ImGui::TreePop();
	}
	ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
	if (ImGui::TreeNode("Surface"))
	{
		ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
		{
			if (ImGui::Button("Clear Surface in select", ImVec2(-1, 0)))
			{
				Scene->UndoSave();
				ClearSurface(true);
			}
			if (ImGui::Button("Clear Surface in level", ImVec2(-1, 0)))
			{
				if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure to reset surface in level?") == mrYes) 
				{
					Scene->UndoSave();
					ClearSurface(false);
				}
			  
			}
		}
		ImGui::Separator();
		ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
		ImGui::TreePop();
	}
	ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
	if (ImGui::TreeNode("Current Object"))
	{

		ImGui::Unindent(ImGui::GetTreeNodeToLabelSpacing());
		{
			if (ImGui::Button("Select ...", ImVec2(-1, 0)))
			{
				UIChooseForm::SelectItem(smObject,1, m_Current,0,0,0,0,0);
				m_Selection = true;
			}
			if (ImGui::Button("Refresh List", ImVec2(-1, 0)))
			{
				RefreshList();
			}
		}
		ImGui::Separator();
		ImGui::Indent(ImGui::GetTreeNodeToLabelSpacing());
		ImGui::TreePop();
	}
}

void UIObjectTool::DrawObjectsList()
{
	if (!bDrawList)
		return;

	if (ImGui::Begin("Edit group items", &bDrawList))
	{
		if (!RefreshInProgress)
		{
			//if (ImGui::BeginChild("Props"))
			{
				ImGui::Image(m_RealTexture ? m_RealTexture : (m_TextureNull->pSurface), ImVec2(128, 128));
				ImGui::SameLine();
				m_Props->Draw();
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
			m_Thm->Update(m_RealTexture);
			PropItemVec Info;
			m_Thm->FillInfo(Info);
			m_Props->AssignItems(Info);
		}
	}
}

void UIObjectTool::SelByRefObject(bool flag)
{
	LPCSTR N = Current();
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
			LPCSTR N = ((CSceneObject*)*it)->RefName();
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
