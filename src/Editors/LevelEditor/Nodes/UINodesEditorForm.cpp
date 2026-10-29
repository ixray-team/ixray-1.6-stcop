#include "stdafx.h"
#include "UINodesEditorForm.h"
#include "../../xrEUI/IconsFontAwesome6.h"
#include "imgui_internal.h"
#include "../Editor/Utils/ContentView.h"
#include "Logic/UILogicEditorForm.h"
#include "UIDialogsView.h"
#include "UIMacroView.h"

UINodesEditorForm* UINodesEditorForm::Form = nullptr;

static const char* ContentWindow = "Content Browser##LG";

void CNodesDock::BeginFrame()
{
	// The windows were hidden: the selection of the docks is restored for a few frames
	if (ImGui::GetFrameCount() > LastDrawFrame + 1)
	{
		RestoreFrames = 3;
	}
	else if (RestoreFrames > 0)
	{
		--RestoreFrames;
	}
	LastDrawFrame = ImGui::GetFrameCount();
}

bool CNodesDock::Begin(const char* Name)
{
	const bool Visible = ImGui::Begin(Name);
	ImGuiWindow* Window = ImGui::GetCurrentWindow();
	ImGuiDockNode* Node = Window->DockNode;
	if (Node == nullptr || Node->TabBar == nullptr)
	{
		return Visible;
	}

	if (RestoreFrames > 0)
	{
		auto Iter = Selection.find(Node->ID);
		if (Iter != Selection.end() && Iter->second == Window->TabId && Node->TabBar->SelectedTabId != Window->TabId)
		{
			if (ImGuiTabItem* Tab = ImGui::TabBarFindTabByID(Node->TabBar, Window->TabId))
			{
				ImGui::TabBarQueueFocus(Node->TabBar, Tab);
			}
		}
	}
	else if (Window->DockTabIsVisible)
	{
		Selection[Node->ID] = Window->TabId;
	}
	return Visible;
}

UINodesEditorForm::UINodesEditorForm()
{
}

UINodesEditorForm::~UINodesEditorForm()
{
}

void UINodesEditorForm::Create(int Tab)
{
	R_ASSERT(Form == nullptr);
	Form = new UINodesEditorForm();
	Form->TabIndex = Tab;

	CContentView* Browser = new CContentView(ContentWindow);
	Browser->Init();
	Browser->TabIndex = Tab;
	Browser->SetStateName("LogicContentBrowser");
	EContext.UI->Push(Browser, false);
	Form->Browser = Browser;

	Form->Modes[(size_t)ENodesMode::Logic].reset(xr_make_unique<UILogicEditorForm>().release());
	Form->Modes[(size_t)ENodesMode::Dialogs].reset(xr_make_unique<DialogEditor>().release());
	Form->Modes[(size_t)ENodesMode::Macros].reset(xr_make_unique<CUIMacroView>().release());

	EContext.UI->Push(Form, false);
	Form->SetStateName("NodesEditorForm");
}

void UINodesEditorForm::Destroy()
{
	xr_delete(Form);
}

void UINodesEditorForm::Serialize(nlohmann::json& Data, bool IsSaving)
{
	IEditorWnd::Serialize(Data, IsSaving);

	static const char* Keys[] = { "logic", "dialogs", "macros" };
	static_assert(std::size(Keys) == (size_t)ENodesMode::Count);

	if (IsSaving)
	{
		Data["mode"] = (int)Mode;
		nlohmann::json DockIds = nlohmann::json::array();
		for (ImGuiID Id : Docks)
		{
			DockIds.push_back(Id);
		}
		Data["docks"] = DockIds;

		for (size_t Index = 0; Index < (size_t)ENodesMode::Count; ++Index)
		{
			nlohmann::json ModeData = nlohmann::json::object();
			Modes[Index]->SerializeMode(ModeData, true);
			Data[Keys[Index]] = ModeData;
		}
		return;
	}

	int SavedMode = 0;
	if (ReadState(Data, "mode", SavedMode) && SavedMode >= 0 && SavedMode < (int)ENodesMode::Count)
	{
		Mode = (ENodesMode)SavedMode;
	}

	auto DockIter = Data.find("docks");
	if (DockIter != Data.end() && DockIter->is_array() && DockIter->size() == (size_t)ENodesDock::Count)
	{
		for (size_t Index = 0; Index < (size_t)ENodesDock::Count; ++Index)
		{
			if ((*DockIter)[Index].is_number_unsigned())
			{
				Docks[Index] = (*DockIter)[Index].get<ImGuiID>();
			}
		}
	}

	// Reopening the files must not switch the editor to this tab
	const int RequestedTab = EContext.UI->RequestedTabIndex;
	const int ActiveTab = EContext.UI->ActiveTabIndex;
	const ENodesMode SavedModeValue = Mode;
	for (size_t Index = 0; Index < (size_t)ENodesMode::Count; ++Index)
	{
		auto Iter = Data.find(Keys[Index]);
		if (Iter != Data.end() && Iter->is_object())
		{
			Modes[Index]->SerializeMode(*Iter, false);
		}
	}
	Mode = SavedModeValue;
	EContext.UI->RequestedTabIndex = RequestedTab;
	EContext.UI->ActiveTabIndex = ActiveTab;
}

INodesMode* UINodesEditorForm::Current() const
{
	return Modes[(size_t)Mode].get();
}

bool UINodesEditorForm::IsActive()
{
	return Form != nullptr && EContext.UI->ActiveTabIndex == Form->TabIndex;
}

bool UINodesEditorForm::IsActive(ENodesMode Mode)
{
	return IsActive() && Form->Mode == Mode;
}

bool UINodesEditorForm::IsModified()
{
	if (Form == nullptr)
	{
		return false;
	}
	return std::any_of(std::begin(Form->Modes), std::end(Form->Modes), [](const xr_unique_ptr<INodesMode>& Item) { return Item != nullptr && Item->IsModeModified(); });
}

void UINodesEditorForm::Activate(ENodesMode Mode)
{
	if (Form == nullptr)
	{
		return;
	}
	Form->Mode = Mode;
	EContext.UI->RequestedTabIndex = Form->TabIndex;
	EContext.UI->ActiveTabIndex = Form->TabIndex;
}

int UINodesEditorForm::GetTab()
{
	return Form != nullptr ? Form->TabIndex : -1;
}

CContentView* UINodesEditorForm::GetBrowser()
{
	return Form != nullptr ? Form->Browser : nullptr;
}

const char* UINodesEditorForm::GetBrowserWindow()
{
	return ContentWindow;
}

bool UINodesEditorForm::Save()
{
	return Form != nullptr && Form->Current()->SaveMode();
}

bool UINodesEditorForm::Reload()
{
	return Form != nullptr && Form->Current()->ReloadMode();
}

bool UINodesEditorForm::Undo(bool Redo)
{
	return Form != nullptr && Form->Current()->UndoMode(Redo);
}

void UINodesEditorForm::RevealInBrowser()
{
	if (Form == nullptr || Form->Browser == nullptr)
	{
		return;
	}
	const xr_string Path = Form->Current()->GetModePath();
	if (!Path.empty())
	{
		Form->Browser->Reveal(Path);
		ImGui::SetWindowFocus(ContentWindow);
	}
}

void UINodesEditorForm::DrawModeSwitch(float ButtonSize)
{
	if (Form == nullptr)
	{
		return;
	}

	for (size_t Index = 0; Index < (size_t)ENodesMode::Count; ++Index)
	{
		INodesMode& Item = *Form->Modes[Index];
		const bool Selected = (size_t)Form->Mode == Index;
		const ImDrawFlags Corners = Index == 0 ? ImDrawFlags_RoundCornersLeft : Index + 1 == (size_t)ENodesMode::Count ? ImDrawFlags_RoundCornersRight : ImDrawFlags_RoundCornersNone;

		xr_string Label = xr_string(Item.GetIcon()) + " " + Item.GetName();
		if (Item.IsModeModified())
		{
			Label += " *";
		}
		const xr_string Id = "##nodes_mode" + xr_string::ToString((int)Index);

		bool Toggle = Selected;
		if (XRay::ImGui::ToolbarButton(Id.c_str(), Label.c_str(), &Toggle, { 0.0f, ButtonSize }, Corners))
		{
			Form->Mode = (ENodesMode)Index;
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			ImGui::SetTooltip("%s node editor", Item.GetName());
		}
		if (Index + 1 < (size_t)ENodesMode::Count)
		{
			ImGui::SameLine();
		}
	}
}

void UINodesEditorForm::DrawTopBar(float ButtonSize)
{
	if (Form != nullptr)
	{
		Form->Current()->DrawModeTopBar(ButtonSize);
	}
}

void UINodesEditorForm::BuildLayout(INodesMode& Item)
{
	bool& Checked = LayoutChecked[(size_t)Mode];
	if (Checked)
	{
		return;
	}

	xr_vector<SNodesWindow> Windows;
	Item.GetWindows(Windows);
	Windows.push_back({ ContentWindow, ENodesDock::Browser });

	// The windows are made by the first draw of the mode
	xr_vector<ImGuiWindow*> Found;
	for (const SNodesWindow& Window : Windows)
	{
		ImGuiWindow* Value = ImGui::FindWindowByName(Window.Name);
		if (Value == nullptr)
		{
			return;
		}
		Found.push_back(Value);
	}
	ImGuiWindow* Viewport = ImGui::FindWindowByName("Render##0");
	if (Viewport == nullptr || Viewport->DockNode == nullptr)
	{
		return;
	}
	Checked = true;

	// The docks of the windows that were placed before: by the user, by another mode
	for (size_t Index = 0; Index < Windows.size(); ++Index)
	{
		if (Found[Index]->DockId != 0)
		{
			Docks[(size_t)Windows[Index].Dock] = Found[Index]->DockId;
		}
	}

	bool AllDocked = true;
	bool DocksValid = true;
	for (size_t Index = 0; Index < Windows.size(); ++Index)
	{
		AllDocked &= Found[Index]->DockId != 0;
		const ImGuiID Dock = Docks[(size_t)Windows[Index].Dock];
		DocksValid &= Dock != 0 && ImGui::DockBuilderGetNode(Dock) != nullptr;
	}
	if (AllDocked)
	{
		return;
	}

	if (!DocksValid)
	{
		ImGuiID Center = Viewport->DockId;
		ImGuiID Left = ImGui::DockBuilderSplitNode(Center, ImGuiDir_Left, 0.2f, nullptr, &Center);
		const ImGuiID Right = ImGui::DockBuilderSplitNode(Center, ImGuiDir_Right, 0.27f, nullptr, &Center);
		const ImGuiID Bottom = ImGui::DockBuilderSplitNode(Center, ImGuiDir_Down, 0.2f, nullptr, &Center);
		const ImGuiID LeftBottom = ImGui::DockBuilderSplitNode(Left, ImGuiDir_Down, 0.45f, nullptr, &Left);
		Docks[(size_t)ENodesDock::Graph] = Center;
		Docks[(size_t)ENodesDock::Browser] = Left;
		Docks[(size_t)ENodesDock::Outliner] = LeftBottom;
		Docks[(size_t)ENodesDock::Details] = Right;
		Docks[(size_t)ENodesDock::Bottom] = Bottom;
	}

	for (size_t Index = 0; Index < Windows.size(); ++Index)
	{
		if (Found[Index]->DockId == 0 || !DocksValid)
		{
			ImGui::DockBuilderDockWindow(Windows[Index].Name, Docks[(size_t)Windows[Index].Dock]);
		}
	}
	ImGui::DockBuilderFinish(ImGui::DockNodeGetRootNode(Viewport->DockNode)->ID);
}

void UINodesEditorForm::Draw()
{
	INodesMode* Item = Current();
	BuildLayout(*Item);
	Item->DrawMode();
}
