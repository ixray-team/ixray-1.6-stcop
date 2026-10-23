#include "stdafx.h"
#include "UIParticleEditorForm.h"
#include "UI_ParticleTools.h"
#include "../Editor/Utils/ContentView.h"
#include "IconsFontAwesome6.h"
#include "imgui_internal.h"
#include "../../xrEUI/imgui_IconMenuItem.h"

extern xr_token2* actions_token;
extern xr_token2 actions_token_impl[];

UIParticleEditorForm* UIParticleEditorForm::Form = nullptr;
CContentView* UIParticleEditorForm::ContentBrowser = nullptr;

static const char* ItemsWindow = "Particles###ParticleItems";
static const char* PreviewWindow = "Preview###ParticlePreview";
// Same window as the Scene View properties, so it keeps the shared dock slot
static const char* PropertiesWindow = "Properties";
static const char* ReferencesWindow = ICON_FA_DIAGRAM_PROJECT " References###ParticleReferences";

static void ParticleEditorFocusCallback()
{
	LUI->EndEState(esEditLibrary);
	LUI->EndEState(esEditScene);
	LUI->BeginEState(esEditCustom);
}

UIParticleEditorForm::UIParticleEditorForm()
{
	View.OnFocusCallback = (xr_delegate<void()>)ParticleEditorFocusCallback;
	View.SetToolBarEvent(TOnRenderToolBar(this, &UIParticleEditorForm::DrawRenderToolBar));
	ViewName = "Particle Editor";

	actions_token = actions_token_impl;
	Tool = new CParticleTool();
	PTools = Tool;
	Tool->OnCreate();
}

UIParticleEditorForm::~UIParticleEditorForm()
{
	Tool->OnDestroy();
	PTools = nullptr;
	xr_delete(Tool);
}

void UIParticleEditorForm::Create(int Tab)
{
	R_ASSERT(Form == nullptr);
	Form = new UIParticleEditorForm();
	Form->TabIndex = Tab;
	EContext.UI->Push(Form, false);

	ContentBrowser = new CContentView("Content Browser##PE");
	ContentBrowser->Init();
	ContentBrowser->TabIndex = Tab;
	static_cast<CLevelPreferences*>(EPrefs)->LoadParticleBrowser(ContentBrowser);
	EContext.UI->Push(ContentBrowser, false);
}

void UIParticleEditorForm::Open(const char* ItemName)
{
	if (Form == nullptr)
	{
		return;
	}

	EContext.UI->RequestedTabIndex = Form->TabIndex;

	if (ItemName != nullptr)
	{
		Form->Tool->SelectEffect(ItemName);
	}
}

void UIParticleEditorForm::Destroy()
{
	xr_delete(Form);
}

bool UIParticleEditorForm::IsActive()
{
	return Form != nullptr && EContext.UI->ActiveTabIndex == Form->TabIndex;
}

bool UIParticleEditorForm::SaveCurrent()
{
	return Form != nullptr && Form->Tool->SaveCurrent();
}

CParticleTool* UIParticleEditorForm::GetActiveTool()
{
	return IsActive() ? Form->Tool : nullptr;
}

void UIParticleEditorForm::Update()
{
	if (Form == nullptr)
	{
		return;
	}

	const bool Active = EContext.UI->ActiveTabIndex == Form->TabIndex;
	if (Active != Form->WasActive)
	{
		Form->WasActive = Active;
		if (Active)
		{
			Form->PrevViewID = EContext.UI->ViewID;
			EContext.UI->ViewID = Form->View.ViewportID;
			ParticleEditorFocusCallback();
		}
		else if (EContext.UI->ViewID == Form->View.ViewportID)
		{
			EContext.UI->ViewID = Form->PrevViewID;
		}
	}

	if (Active)
	{
		Form->Tool->OnFrame();
	}
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

	ImGuiWindow* EditMode = ImGui::FindWindowByName("Edit Mode");
	if (ImGui::FindWindowByName(ItemsWindow) == nullptr || ImGui::FindWindowByName(ReferencesWindow) == nullptr || ImGui::FindWindowByName(PreviewWindow) == nullptr)
	{
		return;
	}
	Checked = true;

	if (HasOwnDockNode(ItemsWindow, EditMode) && HasOwnDockNode(ReferencesWindow, EditMode) && HasOwnDockNode(PreviewWindow, nullptr))
	{
		return;
	}

	ImGuiID Center = Viewport->DockId;
	ImGuiID Left = ImGui::DockBuilderSplitNode(Center, ImGuiDir_Left, 0.22f, nullptr, &Center);
	ImGuiID LeftBottom = ImGui::DockBuilderSplitNode(Left, ImGuiDir_Down, 0.35f, nullptr, &Left);

	ImGui::DockBuilderDockWindow(ItemsWindow, Left);
	ImGui::DockBuilderDockWindow(ReferencesWindow, LeftBottom);
	ImGui::DockBuilderDockWindow(PreviewWindow, Center);
	ImGui::DockBuilderFinish(ImGui::DockNodeGetRootNode(Viewport->DockNode)->ID);
}

void UIParticleEditorForm::Draw()
{
	BuildWorkspaceLayout();

	if (ImGui::Begin(ItemsWindow, nullptr, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse))
	{
		DrawItemList();
	}
	ImGui::End();

	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
	if (ImGui::Begin(PreviewWindow))
	{
		View.DrawVP();
	}
	ImGui::End();
	ImGui::PopStyleVar();

	if (ImGui::Begin(ReferencesWindow))
	{
		Tool->DrawReferenceList();
	}
	ImGui::End();

	if (ImGui::Begin(PropertiesWindow))
	{
		if (XRay::ImGui::BeginDarkChild("ParticlePropertiesBorder"))
		{
			DrawProperties();
			XRay::ImGui::EndDarkChild();
		}
	}
	ImGui::End();

	Tool->OnDrawUI();
}

void UIParticleEditorForm::Render()
{
	if (EContext.UI->ViewID != View.ViewportID)
	{
		return;
	}

	Tool->Render();
}

void UIParticleEditorForm::DrawRenderToolBar(ImVec2 Pos, ImVec2 Size)
{
	const float ButtonSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
	const float ToolbarPadding = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ToolbarPadding);
	ImGui::PushStyleColor(ImGuiCol_ChildBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelBorderTint).Value);
	ImGui::BeginChild("##ParticleToolbar", { 0, ButtonSize + ToolbarPadding * 2 }, 0, ImGuiWindowFlags_NoScrollbar);
	ImGui::PopStyleColor();

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
	ImGui::SetCursorPos({ ToolbarPadding, ToolbarPadding });

	auto Button = [&](const char* Label, ImDrawFlags Rounding, bool* Toggle = nullptr) -> bool
	{
		const float Width = ImGui::CalcTextSize(Label, nullptr, true).x + ToolbarPadding * 4;
		const bool Pressed = XRay::ImGui::ToolbarButton(Label, Label, Toggle, { Width, ButtonSize }, Rounding);
		ImGui::SameLine();
		return Pressed;
	};
	auto Gap = [&]()
	{
		ImGui::SameLine(0, ToolbarPadding * 3);
	};

	if (Button(ICON_FA_LAYER_GROUP " Add Group", ImDrawFlags_RoundCornersLeft))
	{
		Tool->AppendPG(0, "pg");
		Tool->Modified();
	}
	if (Button(ICON_FA_FIRE " Add Particle", ImDrawFlags_RoundCornersNone))
	{
		Tool->AppendPE(0, "pe");
		Tool->Modified();
	}
	if (Button(ICON_FA_BEZIER_CURVE " Add PAC", ImDrawFlags_RoundCornersRight))
	{
		Tool->AppendPAC(0, "pac");
		Tool->Modified();
	}
	Gap();

	if (Button(ICON_FA_CLONE " Clone", ImDrawFlags_RoundCornersLeft))
	{
		Tool->CloneCurrent();
	}
	if (Button(ICON_FA_TRASH_CAN " Remove", ImDrawFlags_RoundCornersNone))
	{
		Tool->RemoveCurrent();
	}
	if (Button(ICON_FA_FILE_IMPORT " Import Effect", ImDrawFlags_RoundCornersRight))
	{
		Tool->ImportPE();
	}
	Gap();

	if (Button(ICON_FA_EXPAND " Zoom", ImDrawFlags_RoundCornersLeft))
	{
		Tool->ZoomObject(false);
	}
	if (Button(ICON_FA_ERASER " Clear", ImDrawFlags_RoundCornersRight))
	{
		EContext.UI->CurrentView().m_Camera.Reset();
		Tool->ResetPreviewObject();
	}
	ImGui::PopStyleVar();
	ImGui::EndChild();
}

void UIParticleEditorForm::DrawItemList()
{
	auto FilterButton = [&](const char* Label, PEd::LisType Type)
	{
		const bool Selected = Tool->m_SelectedTypes == PEd::ListTypeBase(Type);
		if (Selected)
		{
			ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_ButtonActive));
		}
		if (ImGui::Button(Label))
		{
			Tool->m_SelectedTypes = PEd::ListTypeBase(Type);
		}
		if (Selected)
		{
			ImGui::PopStyleColor();
		}
	};
	FilterButton(ICON_FA_LIST " All", PEd::LisType::All);
	ImGui::SameLine();
	FilterButton(ICON_FA_LAYER_GROUP " Groups", PEd::LisType::Groups);
	ImGui::SameLine();
	FilterButton(ICON_FA_FIRE " Effects", PEd::LisType::Effects);
	ImGui::SameLine();
	FilterButton(ICON_FA_BEZIER_CURVE " Anim Curves", PEd::LisType::AnimCurve);

	ImGui::Separator();

	ImGui::PushItemWidth(-1);
	ImGui::InputTextWithHint("##SearchFilter", ICON_FA_MAGNIFYING_GLASS " Search...", SearchFilter, sizeof(SearchFilter));
	ImGui::PopItemWidth();

	UIItemListForm* CurrentList = Tool->GetCurrentList();
	if (CurrentList == nullptr)
	{
		return;
	}

	CurrentList->SetFilter(SearchFilter);

	ImGui::Separator();

	if (!ImGui::BeginChild("##ParticleTree"))
	{
		ImGui::EndChild();
		return;
	}

	ImGui::SetNextItemOpen(true, ImGuiCond_Once);
	if (ImGui::TreeNode("Items"))
	{
		ImGui::BeginGroup();
		CurrentList->Draw();
		ImGui::EndGroup();
		ImGui::TreePop();
	}

	ImGui::EndChild();
}

void UIParticleEditorForm::DrawProperties()
{
	if (ImGui::TreeNode("Custom Object"))
	{
		ImGui::BeginGroup();
		Tool->m_ObjectProps->Draw();
		ImGui::EndGroup();
		ImGui::TreePop();
	}

	ImGui::SetNextItemOpen(true, ImGuiCond_Once);
	if (ImGui::TreeNode("Item Properties"))
	{
		ImGui::BeginGroup();
		Tool->m_ItemProps->Draw();
		ImGui::EndGroup();
		ImGui::TreePop();
	}
}
