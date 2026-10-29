#include "stdafx.h"
#include "ActorEditor.h"
#include "ActorManipulator.h"
#include "CustomTools/UIBatchTools.h"
#include "CustomTools/UIPostProcess.h"

#include "../../xrECore/Editor/EditMesh.h"
#include "../../xrEUI/ImGuizmo.h"
#include "../../xrEUI/imgui_IconMenuItem.h"
#include "IconsFontAwesome7.h"
#include "../Editor/Utils/ContentView.h"
#include "imgui_internal.h"

extern ECORE_API bool g_force16BitTransformQuant;
extern ECORE_API bool g_force32BitTransformQuant;

UIActorEditorForm* UIActorEditorForm::Form = nullptr;
CContentView* UIActorEditorForm::ContentBrowser = nullptr;

static const char* ToolWindow = "Object Tool###ActorObjectTool";
static const char* ItemsWindow = "Object Items###ActorObjectItems";
static const char* PreviewWindow = "Preview###ActorPreview";
static const char* LibraryWindow = "Library###ActorLibrary";
static const char* ReferencesWindow = "Reference View###ActorReferences";
// Same window as the Scene View properties, so it keeps the shared dock slot
static const char* PropertiesWindow = "Properties";

static void ActorViewportDrop(const xr_string& File, int)
{
	UIActorEditorForm::DropFile(File);
}

static void ActorEditorFocusCallback()
{
	LUI->EndEState(esEditLibrary);
	LUI->EndEState(esEditScene);
	LUI->BeginEState(esEditCustom);
}

UIActorEditorForm::UIActorEditorForm()
{
	View.OnFocusCallback = (xr_delegate<void()>)ActorEditorFocusCallback;
	View.SetToolBarEvent(TOnRenderToolBar(this, &UIActorEditorForm::DrawRenderToolBar));
	View.DragFunctor = ActorViewportDrop;
	ViewName = "Actor Editor";

	if (g_force16BitTransformQuant)
	{
		AnimMode = e16bit;
	}
	else if (g_force32BitTransformQuant)
	{
		AnimMode = e32bit;
	}

	Tool = new CActorTools();
	ATools = Tool;
	Tool->OnCreate();

	KeyForm = new UIKeyForm();
	Library = new UIActorLibrary(&View);
}

UIActorEditorForm::~UIActorEditorForm()
{
	if (Tools == Tool)
	{
		Tools = LTools;
	}

	ReferenceTask.wait();

	xr_delete(Library);
	xr_delete(KeyForm);
	Tool->OnDestroy();
	ATools = nullptr;
	xr_delete(Tool);
}

void UIActorEditorForm::Create(int Tab)
{
	R_ASSERT(Form == nullptr);
	Form = new UIActorEditorForm();
	Form->TabIndex = Tab;
	Form->Tool->BoneView->TabIndex = Tab;
	Form->Tool->UVView->TabIndex = Tab;
	EContext.UI->Push(Form, false);

	ContentBrowser = new CContentView("Content Browser##AE");
	ContentBrowser->Init();
	ContentBrowser->TabIndex = Tab;
	ContentBrowser->SetStateName("ActorContentBrowser");
	EContext.UI->Push(ContentBrowser, false);
}

void UIActorEditorForm::Open(const char* FileName)
{
	if (Form == nullptr)
	{
		return;
	}

	EContext.UI->RequestedTabIndex = Form->TabIndex;

	if (FileName != nullptr)
	{
		Form->Library->ReleaseProperties();
		Form->Tool->CommandLoad(xr_string(FileName), CCommandVar());
	}
}

void UIActorEditorForm::Destroy()
{
	xr_delete(Form);
}

bool UIActorEditorForm::IsActive()
{
	return Form != nullptr && EContext.UI->ActiveTabIndex == Form->TabIndex;
}

CActorTools* UIActorEditorForm::GetActiveTool()
{
	return IsActive() ? Form->Tool : nullptr;
}

void UIActorEditorForm::Update()
{
	if (Form == nullptr)
	{
		return;
	}

	const bool Active = EContext.UI->ActiveTabIndex == Form->TabIndex;
	if (Active == Form->WasActive)
	{
		return;
	}

	Form->WasActive = Active;
	if (Active)
	{
		Form->PrevViewID = EContext.UI->ViewID;
		EContext.UI->ViewID = Form->View.ViewportID;
		ActorEditorFocusCallback();
		Tools = Form->Tool;
	}
	else
	{
		Tools = LTools;
		if (EContext.UI->ViewID == (size_t)Form->View.ViewportID)
		{
			EContext.UI->ViewID = Form->PrevViewID;
		}
	}

	EContext.UI->RedrawScene();
}

UIActorEditorForm::RenderMode UIActorEditorForm::GetRenderMode()
{
	return Form != nullptr ? Form->Mode : Render_Editor;
}

void UIActorEditorForm::SetRenderMode(bool EngineMode)
{
	if (Form == nullptr)
	{
		return;
	}

	Form->Mode = (ATools->IsVisualPresent() && EngineMode) ? Render_Engine : Render_Editor;
	ATools->PlayMotion();
}

int UIActorEditorForm::GetPickMode()
{
	return Form != nullptr ? Form->PickMode : 0;
}

UIKeyForm* UIActorEditorForm::GetKeyForm()
{
	return Form != nullptr ? Form->KeyForm : nullptr;
}

void UIActorEditorForm::DropFile(const xr_string& FileName)
{
	if (Form == nullptr)
	{
		return;
	}

	xr_string File = FileName;
	if (IsUTF8(File.c_str()))
	{
		File = Platform::UTF8_to_CP1251(File);
	}
	xr_strlwr(File);

	// Models open in the editor
	if (File.ends_with(".object") || File.ends_with(".ogf"))
	{
		Open(File.c_str());
		return;
	}

	const bool IsMotion = File.ends_with(".skl") || File.ends_with(".skls");
	const bool IsMotionRef = File.ends_with(".omf");
	const bool IsBones = File.ends_with(".bones");
	if (!IsMotion && !IsMotionRef && !IsBones)
	{
		return;
	}

	EContext.UI->RequestedTabIndex = Form->TabIndex;

	CActorTools* Actor = Form->Tool;
	CEditableObject* Object = Actor->CurrentObject();
	if (Object == nullptr)
	{
		ELog.DlgMsg(mtError, "Load object first.");
		return;
	}

	if (IsBones)
	{
		FS.TryLoad(File);
		IReader* Reader = FS.r_open(File.c_str());
		if (Reader != nullptr && Object->LoadBoneData(*Reader))
		{
			ELog.Msg(mtInformation, "Bone data '%s' successfully loaded.", File.c_str());
			Actor->OnBoneModified();
		}
		else
		{
			ELog.DlgMsg(mtError, "Failed to load bone data '%s'.", File.c_str());
		}
		if (Reader != nullptr)
		{
			FS.r_close(Reader);
		}
		Actor->UpdateProperties();
		return;
	}

	if (!Object->IsSkeleton())
	{
		ELog.DlgMsg(mtError, "Object has no skeleton.");
		return;
	}

	if (IsMotion)
	{
		FS.TryLoad(File);
		if (Actor->AppendMotion(File.c_str()))
		{
			Actor->OnMotionKeysModified();
		}
		else
		{
			ELog.DlgMsg(mtError, "Can't append motions from '%s'.", File.c_str());
		}
		Actor->UpdateProperties();
		return;
	}

	// .omf: a motion reference relative to $game_meshes$
	string_path CurrentWD = {};
	string_path CurrentGM = {};
	FS.update_path(CurrentGM, "$game_meshes$", "");
	FS.update_path(CurrentWD, "$fs_root$", CurrentGM);
	xr_strlwr(CurrentWD);

	if (File.starts_with(CurrentWD))
	{
		Object->m_SMotionRefs.emplace_back(EFS.ChangeFileExt(File.substr(xr_strlen(CurrentWD)), "").c_str());
	}
	else
	{
		xr_path FilePath = File;
		xr_string FileNameOnly = FilePath.xfilename();
		xr_string OutPath = CurrentWD + FileNameOnly;

		// Copies the motions into the game meshes
		std::filesystem::copy_file(File.c_str(), OutPath.c_str(), std::filesystem::copy_options::overwrite_existing);
		FS.TryLoad((CurrentGM + FileNameOnly).c_str());
		Object->m_SMotionRefs.emplace_back(FileNameOnly.substr(0, FileNameOnly.length() - 4).c_str());
	}

	Actor->OnMotionDefsModified();
	Actor->UpdateProperties();
}

// The workspace windows get dock nodes of their own, split off the scene viewport node.
// On the other tabs these nodes have no visible window, so the viewport takes their space
// and the Scene View layout keeps its sizes.
static bool HasOwnDockNode(const char* Name, ImGuiWindow* SceneWindow)
{
	ImGuiWindow* Window = ImGui::FindWindowByName(Name);
	if (Window == nullptr)
	{
		return true; // not created yet, check again next frame
	}
	return Window->DockId != 0 && (SceneWindow == nullptr || Window->DockId != SceneWindow->DockId);
}

static void BuildWorkspaceLayout()
{
	// Checked once per session, so the user can still undock the windows afterwards
	static bool Checked = false;
	if (Checked)
	{
		return;
	}

	ImGuiWindow* Viewport = ImGui::FindWindowByName("Render##0");
	if (Viewport == nullptr || Viewport->DockNode == nullptr)
	{
		return;
	}

	if (ImGui::FindWindowByName(ToolWindow) == nullptr || ImGui::FindWindowByName(ItemsWindow) == nullptr || ImGui::FindWindowByName(PreviewWindow) == nullptr)
	{
		return;
	}
	Checked = true;

	ImGuiWindow* EditMode = ImGui::FindWindowByName("Edit Mode");
	if (HasOwnDockNode(ToolWindow, EditMode) && HasOwnDockNode(ItemsWindow, EditMode) && HasOwnDockNode(PreviewWindow, nullptr))
	{
		return;
	}

	ImGuiID Center = Viewport->DockId;
	ImGuiID Left = ImGui::DockBuilderSplitNode(Center, ImGuiDir_Left, 0.22f, nullptr, &Center);
	ImGuiID LeftBottom = ImGui::DockBuilderSplitNode(Left, ImGuiDir_Down, 0.6f, nullptr, &Left);

	ImGui::DockBuilderDockWindow(ToolWindow, Left);
	ImGui::DockBuilderDockWindow(ItemsWindow, LeftBottom);
	ImGui::DockBuilderDockWindow(LibraryWindow, LeftBottom);
	ImGui::DockBuilderDockWindow(ReferencesWindow, LeftBottom);
	ImGui::DockBuilderDockWindow(PreviewWindow, Center);
	ImGui::DockBuilderFinish(ImGui::DockNodeGetRootNode(Viewport->DockNode)->ID);
}

void UIActorEditorForm::Draw()
{
	BuildWorkspaceLayout();

	DrawObjectTool();
	DrawObjectItems();

	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
	if (ImGui::Begin(PreviewWindow))
	{
		View.DrawVP();
	}
	ImGui::End();
	ImGui::PopStyleVar();

	DrawProperties();
	Library->Draw();
	DrawReferences();

	KeyForm->Draw();
	UIBoneForm::Update();
	Tool->OnDrawUI();
}

void UIActorEditorForm::Render()
{
	// The actor tool is the global Tools while the tab is active: the editor renders it directly
}

void UIActorEditorForm::SetSmooth(ESmoothGroup Smooth)
{
	EPrefs->SmoothGroup = Smooth;

	if (!Tool->CurrentObject())
	{
		return;
	}

	for (CEditableMesh* Mesh : Tool->CurrentObject()->m_Meshes)
	{
		u32 Count = Mesh->m_SVertInfl;
		Mesh->UnloadSVertices();
		Mesh->GenerateSVertices(Count);
	}
}

void UIActorEditorForm::SetAnim(EAnimMode Anim)
{
	AnimMode = Anim;

	g_force16BitTransformQuant = (Anim == e16bit);
	g_force32BitTransformQuant = (Anim == e32bit);
}

// One labelled row of the Object Tool tables
static void ToolRow(const char* Label)
{
	XRay::ImGui::TableNextRow();
	XRay::ImGui::TableNextColumn();
	XRay::ImGui::TextFramed(Label);
	XRay::ImGui::TableNextColumn();
}

// Mutually exclusive toolbar buttons that share the row width; returns the clicked index or -1
static int SegmentedButtons(const char* Id, const char* const* Labels, int Count, int Current)
{
	int Clicked = -1;
	const float Width = ImGui::GetContentRegionAvail().x / Count;

	ImGui::PushID(Id);
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
	for (int Index = 0; Index < Count; ++Index)
	{
		ImDrawFlags Rounding = ImDrawFlags_RoundCornersNone;
		if (Count == 1)
		{
			Rounding = ImDrawFlags_RoundCornersAll;
		}
		else if (Index == 0)
		{
			Rounding = ImDrawFlags_RoundCornersLeft;
		}
		else if (Index == Count - 1)
		{
			Rounding = ImDrawFlags_RoundCornersRight;
		}

		bool Selected = Index == Current;
		ImGui::PushID(Index);
		if (XRay::ImGui::ToolbarButton("##segment", Labels[Index], &Selected, { Width, 0 }, Rounding))
		{
			Clicked = Index;
		}
		ImGui::PopID();

		if (Index != Count - 1)
		{
			ImGui::SameLine();
		}
	}
	ImGui::PopStyleVar();
	ImGui::PopID();

	return Clicked;
}

void UIActorEditorForm::DrawObjectTool()
{
	auto BeginToolTable = [](const char* Id) -> bool
	{
		if (!XRay::ImGui::BeginTable(Id, 2, ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_RowBg))
		{
			return false;
		}
		ImGui::TableSetupColumn("##Key", ImGuiTableColumnFlags_WidthFixed, ImGui::CalcTextSize("Format:").x + ImGui::GetStyle().FramePadding.x * 4);
		ImGui::TableSetupColumn("##Values", ImGuiTableColumnFlags_WidthStretch);
		return true;
	};

	ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);
	if (ImGui::Begin(ToolWindow))
	{
		ImGui::SetNextItemOpen(true, ImGuiCond_Once);
		if (XRay::ImGui::BeginExpand(ICON_FA_EYE " Display"))
		{
			if (BeginToolTable("DisplaySettings"))
			{
				ToolRow("Render:");
				static const char* RenderLabels[] = { "Editor", "Engine" };
				switch (SegmentedButtons("Render", RenderLabels, 2, Mode == Render_Editor ? 0 : 1))
				{
				case 0:
					Tool->PhysicsStopSimulate();
					Mode = Render_Editor;
					Tool->UpdateProperties();
					EContext.UI->RedrawScene();
					break;
				case 1:
					Tool->PhysicsStopSimulate();
					if (!Tool->IsVisualPresent())
					{
						Tool->MakePreview();
					}
					SetRenderMode(Tool->IsVisualPresent());
					Tool->UpdateProperties();
					EContext.UI->RedrawScene();
					break;
				}

				ToolRow("Smooth:");
				static const char* SmoothLabels[] = { "Edges", "Normals", "Legacy" };
				const int Smooth = EPrefs->SmoothGroup == ESmoothGroup::Edges ? 0 : EPrefs->SmoothGroup == ESmoothGroup::Normals ? 1 : 2;
				switch (SegmentedButtons("Smooth", SmoothLabels, 3, Smooth))
				{
				case 0: SetSmooth(ESmoothGroup::Edges); break;
				case 1: SetSmooth(ESmoothGroup::Normals); break;
				case 2: SetSmooth(ESmoothGroup::Other); break;
				}

				ToolRow("Pick:");
				static const char* PickLabels[] = { "None", "Surface", "Bone" };
				const int NewPick = SegmentedButtons("Pick", PickLabels, 3, PickMode);
				if (NewPick >= 0)
				{
					PickMode = NewPick;
				}

				ToolRow("Format:");
				static const char* AnimLabels[] = { "8-bit", "16-bit", "32-bit" };
				const int NewAnim = SegmentedButtons("AnimFormat", AnimLabels, 3, (int)AnimMode);
				if (NewAnim >= 0)
				{
					SetAnim((EAnimMode)NewAnim);
				}

				XRay::ImGui::EndTable();
			}
			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_Once);
		if (XRay::ImGui::BeginExpand(ICON_FA_BONE " Skeleton"))
		{
			static const char* ToolLabels[] = { ICON_FA_SITEMAP " Bones", ICON_FA_LAYER_GROUP " Parts", ICON_FA_TABLE_CELLS " UV" };
			switch (SegmentedButtons("SkeletonTools", ToolLabels, 3, -1))
			{
			case 0: Tool->BoneView->Show(true); break;
			case 1: UIBoneForm::Show(); break;
			case 2: Tool->UVView->Show(true); break;
			}
			XRay::ImGui::EndExpand();
		}

		ImGui::SetNextItemOpen(true, ImGuiCond_Once);
		if (XRay::ImGui::BeginExpand(ICON_FA_CUBES " Library"))
		{
			Library->DrawActions();
			XRay::ImGui::EndExpand();
		}
	}
	ImGui::End();
	ImGui::PopStyleVar();
}

void UIActorEditorForm::DrawObjectItems()
{
	if (ImGui::Begin(ItemsWindow))
	{
		// Working with the object items brings its properties back
		if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows))
		{
			Library->ReleaseProperties();
		}

		ImGui::BeginGroup();

		Tool->m_ObjectItems->SetOnDrawItemExtraEvent(+[](UIItemListForm::Node& Node)
		{
			if (Node.Object && Node.Object->Type() == emSurface && Node.Object->m_Object)
			{
				CSurface* Surf = (CSurface*)Node.Object->m_Object;
				const float ButtonWidth = ImGui::GetFrameHeight();
				const float AvailWidth = ImGui::GetContentRegionAvail().x;
				ImGui::SameLine();

				ImGui::SetCursorPosX(AvailWidth - ButtonWidth * 2.f - ImGui::GetStyle().ItemSpacing.x);
				ImGui::PushID(Surf);
				if (ImGui::SmallButton(ICON_FA_ARROWS_ROTATE))
				{
					Surf->Reload();
					EContext.UI->RedrawScene();
				}
				if (ImGui::IsItemHovered())
				{
					ImGui::SetTooltip("Reload surface");
				}

				ImGui::SameLine();
				const char* Icon = Surf->m_bEditorVisible ? ICON_FA_EYE : ICON_FA_EYE_SLASH;
				if (ImGui::SmallButton(Icon))
				{
					Surf->m_bEditorVisible = !Surf->m_bEditorVisible;
					EContext.UI->RedrawScene();
				}
				ImGui::PopID();
			}
		});
		Tool->m_ObjectItems->Draw();
		Tool->m_ObjectItems->SetOnDrawItemExtraEvent(nullptr);

		ImGui::EndGroup();

		CEditableObject* Object = Tool->CurrentObject();
		if (Object != nullptr && !Object->m_objectFlags.test(CEditableObject::eoDynamic))
		{
			ImGui::Separator();

			if (ImGui::Button("Make dynamic", { -1, 0 }))
			{
				if (Object->CreateBone("idle"))
				{
					Tool->OnObjectModified();
				}

				Tool->RealUpdateProperties();
			}
		}
	}
	ImGui::End();
}

void UIActorEditorForm::DrawProperties()
{
	ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);
	if (ImGui::Begin(PropertiesWindow))
	{
		if (XRay::ImGui::BeginDarkChild("ActorPropertiesBorder"))
		{
			// The selected library objects or the edited object
			if (Library->HasProperties())
			{
				Library->DrawProperties();
			}
			else
			{
				Tool->m_Props->Draw();
			}
			XRay::ImGui::EndDarkChild();
		}
	}
	ImGui::End();
	ImGui::PopStyleVar();
}

static xr_string GetLibraryName(const xr_string& FileName)
{
	if (!FileName.ends_with(".object"))
	{
		return {};
	}

	string_path Root = {};
	FS.update_path(Root, _objects_, "");

	xr_string Name = FileName;
	xr_string RootName = Root;
	xr_strlwr(Name);
	xr_strlwr(RootName);
	if (!Name.starts_with(RootName))
	{
		return {};
	}

	Name = Name.substr(RootName.size());
	return Name.substr(0, Name.size() - xr_strlen(".object"));
}

void UIActorEditorForm::StartReferenceSearch(const xr_string& Name)
{
	ReferenceTask.wait();
	ReferenceRunning = false;

	ReferenceTarget = Name;
	References.clear();
	ReferenceMatches.clear();
	ReferenceSnapshot.clear();

	if (Name.empty() || Scene == nullptr)
	{
		return;
	}

	ObjectList& Objects = Scene->ListObj(OBJCLASS_SCENEOBJECT);
	ReferenceSnapshot.reserve(Objects.size());
	for (CCustomObject* Object : Objects)
	{
		CSceneObject* SceneObject = static_cast<CSceneObject*>(Object);
		ReferenceSnapshot.push_back({ SceneObject->GetName(), SceneObject->m_ReferenceName });
	}

	ReferenceDone = false;
	ReferenceRunning = true;
	ReferenceTask.run([this]()
	{
		for (u32 Index = 0; Index < (u32)ReferenceSnapshot.size(); ++Index)
		{
			const char* RefName = ReferenceSnapshot[Index].RefName.c_str();
			if (RefName != nullptr && xr_stricmp(RefName, ReferenceTarget.c_str()) == 0)
			{
				ReferenceMatches.push_back(Index);
			}
		}
		ReferenceDone = true;
	});
}

void UIActorEditorForm::SyncReferences()
{
	if (Tool->IsModified())
	{
		const int Result = ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo | mbCancel, "The object has unsaved changes. Save it before sync?");
		if (Result == mrCancel)
		{
			return;
		}
		if (Result == mrYes && !Tool->Save(Tool->GetEditFileName().c_str()))
		{
			ELog.DlgMsg(mtError, "Can't save object '%s'.", Tool->GetEditFileName().c_str());
			return;
		}
	}

	Lib.ReloadObject(ReferenceTarget.c_str());

	for (ObjectIt It = Scene->FirstObj(OBJCLASS_SECTOR); It != Scene->LastObj(OBJCLASS_SECTOR); ++It)
	{
		static_cast<CSector*>(*It)->ReloadObjectsReferences();
	}

	u32 Synced = 0;
	for (const shared_str& Name : References)
	{
		CCustomObject* Object = Scene->FindObjectByName(Name.c_str(), OBJCLASS_SCENEOBJECT);
		if (Object != nullptr)
		{
			static_cast<CSceneObject*>(Object)->ReloadReferences();
			++Synced;
		}
	}

	ELog.Msg(mtInformation, "Synced %u scene objects of '%s'.", Synced, ReferenceTarget.c_str());
	EContext.UI->RedrawScene();
}

void UIActorEditorForm::DrawReferences()
{
	const xr_string Target = GetLibraryName(Tool->CurrentObject() != nullptr ? Tool->GetEditFileName() : xr_string());
	if (Target != ReferenceTarget)
	{
		StartReferenceSearch(Target);
	}

	if (ReferenceRunning && ReferenceDone)
	{
		ReferenceTask.wait();
		ReferenceRunning = false;
		for (u32 Index : ReferenceMatches)
		{
			References.push_back(ReferenceSnapshot[Index].ObjectName);
		}
		ReferenceMatches.clear();
		ReferenceSnapshot.clear();
	}

	if (ImGui::Begin(ReferencesWindow))
	{
		if (ReferenceTarget.empty())
		{
			ImGui::TextDisabled("Load a library object to find its scene objects.");
		}
		else
		{
			ImGui::BeginDisabled(ReferenceRunning);
			if (ImGui::Button(ICON_FA_MAGNIFYING_GLASS " Find"))
			{
				StartReferenceSearch(ReferenceTarget);
			}
			if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled))
			{
				ImGui::SetTooltip("Search the scene again");
			}
			ImGui::SameLine();
			ImGui::BeginDisabled(References.empty());
			if (ImGui::Button(ICON_FA_ARROWS_ROTATE " Sync"))
			{
				SyncReferences();
			}
			if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled))
			{
				ImGui::SetTooltip("Reload the object from disk and refresh its scene objects");
			}
			ImGui::EndDisabled();
			ImGui::EndDisabled();

			ImGui::SameLine();
			if (ReferenceRunning)
			{
				ImGui::TextDisabled("Searching...");
			}
			else
			{
				ImGui::Text("%u on the level", (u32)References.size());
			}

			ImGui::Separator();
			if (ImGui::BeginChild("##ReferenceList"))
			{
				for (const shared_str& Name : References)
				{
					if (ImGui::Selectable(Name.c_str(), false, ImGuiSelectableFlags_AllowDoubleClick))
					{
						CCustomObject* Object = Scene->FindObjectByName(Name.c_str(), OBJCLASS_SCENEOBJECT);
						if (Object != nullptr)
						{
							Scene->SelectObjects(false, OBJCLASS_SCENEOBJECT);
							Object->Select(true);
							if (ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
							{
								Scene->ZoomExtents(OBJCLASS_SCENEOBJECT, true);
							}
							EContext.UI->RedrawScene();
						}
					}
					if (ImGui::IsItemHovered())
					{
						ImGui::SetTooltip("Click to select, double-click to zoom in the scene");
					}
				}
			}
			ImGui::EndChild();
		}
	}
	ImGui::End();
}

void UIActorEditorForm::DrawSkeletonFlag(const char* Id, const char* Icon, const char* Hint, u32 Flag, ImDrawFlags Rounding)
{
	const float ButtonSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);

	bool IsEnabled = EPrefs->object_flags.is(Flag);
	if (XRay::ImGui::ToolbarButton(Id, Icon, &IsEnabled, { ButtonSize, ButtonSize }, Rounding))
	{
		EPrefs->object_flags.set(Flag, !IsEnabled);
		EContext.UI->RedrawScene();
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetTooltip(Hint);
	}

	ImGui::SameLine();
}

void UIActorEditorForm::DrawRenderToolBar(ImVec2 Pos, ImVec2 Size)
{
	const float ButtonSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
	const float ToolbarPadding = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ToolbarPadding);
	ImGui::PushStyleColor(ImGuiCol_ChildBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelBorderTint).Value);
	ImGui::BeginChild("##ActorToolbar", { 0, ButtonSize + ToolbarPadding * 2 }, 0, ImGuiWindowFlags_NoScrollbar);
	ImGui::PopStyleColor();

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
	ImGui::SetCursorPos({ ToolbarPadding, ToolbarPadding });

	auto Button = [&](const char* Label, const char* Hint, bool Selected, ImDrawFlags Rounding) -> bool
	{
		const float Width = ImGui::CalcTextSize(Label, nullptr, true).x + ToolbarPadding * 4;
		const bool Pressed = XRay::ImGui::ToolbarButton(Label, Label, &Selected, { Width, ButtonSize }, Rounding);
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip(Hint);
		}
		ImGui::SameLine();
		return Pressed;
	};
	auto Gap = [&]()
	{
		ImGui::SameLine(0, ToolbarPadding * 3);
	};

	// Action
	const ETAction Action = Tool->GetAction();
	if (Button(ICON_FA_ARROW_POINTER, "Select", Action == etaSelect, ImDrawFlags_RoundCornersLeft))
	{
		Tool->SetAction(etaSelect);
	}
	if (Button(ICON_FA_ARROWS_UP_DOWN_LEFT_RIGHT, "Move", Action == etaMove, ImDrawFlags_RoundCornersNone))
	{
		Tool->SetAction(etaMove);
	}
	if (Button(ICON_FA_UP_RIGHT_AND_DOWN_LEFT_FROM_CENTER, "Scale", Action == etaScale, ImDrawFlags_RoundCornersNone))
	{
		Tool->SetAction(etaScale);
	}
	if (Button(ICON_FA_ROTATE, "Rotate", Action == etaRotate, ImDrawFlags_RoundCornersRight))
	{
		Tool->SetAction(etaRotate);
	}
	Gap();

	// Local/World
	{
		const bool IsLocal = g_ActorManipulator.MatrixMode == ImGuizmo::LOCAL;
		ImGui::BeginDisabled(Action == etaScale || Action == etaSelect);
		if (Button(IsLocal ? ICON_FA_CUBE : ICON_FA_GLOBE, IsLocal ? "Local transform" : "World transform", false, ImDrawFlags_RoundCornersAll))
		{
			g_ActorManipulator.MatrixMode = IsLocal ? ImGuizmo::WORLD : ImGuizmo::LOCAL;
		}
		ImGui::EndDisabled();
	}
	Gap();

	// Skeleton
	DrawSkeletonFlag("DrawJoints", ICON_FA_CIRCLE_DOT, "Draw Joints", epoDrawJoints, ImDrawFlags_RoundCornersLeft);
	DrawSkeletonFlag("DrawBoneAxis", ICON_FA_CROSSHAIRS, "Draw Bone Axis", epoDrawBoneAxis, ImDrawFlags_RoundCornersNone);
	DrawSkeletonFlag("DrawBoneNames", ICON_FA_FONT, "Draw Bone Names", epoDrawBoneNames, ImDrawFlags_RoundCornersNone);
	DrawSkeletonFlag("DrawBoneShapes", ICON_FA_SHAPES, "Draw Bone Shapes", epoDrawBoneShapes, ImDrawFlags_RoundCornersRight);
	Gap();

	if (Button(ICON_FA_EXPAND " Zoom", "Zoom to the object", false, ImDrawFlags_RoundCornersAll))
	{
		Tool->ZoomObject(false);
	}

	ImGui::PopStyleVar();
	ImGui::EndChild();

	g_ActorManipulator.Render(Pos.x, Pos.y, Size.x, Size.y);
}

void UIActorEditorForm::DrawMainMenu()
{
	if (!IsActive())
	{
		return;
	}

	CActorTools* Actor = Form->Tool;
	if (ImGui::BeginMenu("Actor"))
	{
		if (ImGui::MenuItemI("Import...", ICON_FA_FILE_IMPORT, ""))
		{
			Actor->CommandImport(CCommandVar(), CCommandVar());
		}
		if (ImGui::MenuItemI("Import OMF...", ICON_FA_FILE_IMPORT, ""))
		{
			Actor->CommandImportOMF(CCommandVar(), CCommandVar());
		}
		if (ImGui::BeginMenuI("Export", ICON_FA_FILE_EXPORT))
		{
			if (ImGui::MenuItemI("Export OGF...", ICON_FA_FILE_EXPORT, ""))
			{
				Actor->CommandExportOGF(CCommandVar(), CCommandVar());
			}
			if (ImGui::MenuItemI("Export OMF...", ICON_FA_FILE_EXPORT, ""))
			{
				Actor->CommandExportOMF(CCommandVar(), CCommandVar());
			}
			if (ImGui::MenuItemI("Export OBJ...", ICON_FA_FILE_EXPORT, ""))
			{
				Actor->CommandExportOBJ(CCommandVar(), CCommandVar());
			}
			if (ImGui::MenuItemI("Export DM...", ICON_FA_FILE_EXPORT, ""))
			{
				Actor->CommandExportDM(CCommandVar(), CCommandVar());
			}
			if (ImGui::MenuItemI("Export C++...", ICON_FA_FILE_EXPORT, ""))
			{
				Actor->CommandExportCPP(CCommandVar(), CCommandVar());
			}
			ImGui::EndMenu();
		}
		ImGui::Separator();

		if (ImGui::MenuItem("Optimize Motions"))
		{
			Actor->CommandOptimizeMotions(CCommandVar(), CCommandVar());
		}
		if (ImGui::MenuItem("Make Thumbnail"))
		{
			Actor->CommandMakeThumbnail(CCommandVar(), CCommandVar());
		}
		if (ImGui::MenuItem("Batch Convert..."))
		{
			Actor->CommandBatchConvert(CCommandVar(), CCommandVar());
		}
		ImGui::Separator();

		if (ImGui::BeginMenu("Preview Object"))
		{
			if (ImGui::MenuItem("Custom..."))
			{
				Actor->SelectPreviewObject(false);
			}
			if (ImGui::MenuItem("Clear"))
			{
				Actor->SelectPreviewObject(true);
			}
			ImGui::Separator();
			if (ImGui::MenuItem("Preferences"))
			{
				Actor->SetPreviewObjectPrefs();
			}
			ImGui::EndMenu();
		}
		ImGui::Separator();

		if (ImGui::MenuItem("Batch Tools"))
		{
			CUIBatchTools::Instance().Open();
		}
		if (ImGui::MenuItem("Post Process Editor"))
		{
			CMainPPE::Instance().OpenState() = true;
		}
		ImGui::EndMenu();
	}
}
