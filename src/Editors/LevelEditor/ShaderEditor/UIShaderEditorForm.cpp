#include "stdafx.h"
#include "UIShaderEditorForm.h"
#include "UI_shadertools.h"
#include "SHEngineTools.h"
#include "IconsFontAwesome7.h"
#include "imgui_internal.h"

UIShaderEditorForm* UIShaderEditorForm::Form = nullptr;

static const char* ItemsWindow = "Shaders###ShaderItems";
static const char* PreviewWindow = "Preview###ShaderPreview";
// Same window as the Scene View properties, so it keeps the shared dock slot
static const char* PropertiesWindow = "Properties";

static void ShaderEditorFocusCallback()
{
	LUI->EndEState(esEditLibrary);
	LUI->EndEState(esEditScene);
	LUI->BeginEState(esEditCustom);
}

UIShaderEditorForm::UIShaderEditorForm()
{
	View.OnFocusCallback = (xr_delegate<void()>)ShaderEditorFocusCallback;
	View.SetToolBarEvent(TOnRenderToolBar(this, &UIShaderEditorForm::DrawRenderToolBar));
	ViewName = "Shader Editor";

	Tool = new CShaderTool();
	STools = Tool;
	Tool->OnCreate();
}

UIShaderEditorForm::~UIShaderEditorForm()
{
	if (Tools == Tool)
	{
		Tools = LTools;
	}

	Tool->OnDestroy();
	STools = nullptr;
	xr_delete(Tool);
}

void UIShaderEditorForm::Create(int Tab)
{
	R_ASSERT(Form == nullptr);
	Form = new UIShaderEditorForm();
	Form->TabIndex = Tab;
	EContext.UI->Push(Form, false);
}

void UIShaderEditorForm::Open()
{
	if (Form == nullptr)
	{
		return;
	}

	EContext.UI->RequestedTabIndex = Form->TabIndex;
}

void UIShaderEditorForm::Destroy()
{
	xr_delete(Form);
}

bool UIShaderEditorForm::IsActive()
{
	return Form != nullptr && EContext.UI->ActiveTabIndex == Form->TabIndex;
}

CShaderTool* UIShaderEditorForm::GetActiveTool()
{
	return IsActive() ? Form->Tool : nullptr;
}

bool UIShaderEditorForm::SaveAll()
{
	return Form != nullptr && Form->Tool->Save(nullptr);
}

bool UIShaderEditorForm::ReloadCurrent()
{
	if (Form == nullptr)
	{
		return false;
	}

	Form->Tool->Reload();
	return true;
}

void UIShaderEditorForm::Update()
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
		ShaderEditorFocusCallback();
		Tools = Form->Tool;

		if (Form->Tool->m_Current != nullptr && Form->Tool->m_Current->ID() == aeEngine)
		{
			static_cast<CSHEngineTools*>(Form->Tool->m_Current)->RestoreForceBlender();
		}
	}
	else
	{
		Tools = LTools;

		// The forced blender is for the preview only: the scene must compile its shaders as usual,
		// and the ones built while it was set are rebuilt
		DEV->SetForceBlenderShader(nullptr);
		DEV->RecompileShaders();
		if (EContext.UI->ViewID == (size_t)Form->View.ViewportID)
		{
			EContext.UI->ViewID = Form->PrevViewID;
		}
	}

	EContext.UI->RedrawScene();
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

	if (ImGui::FindWindowByName(ItemsWindow) == nullptr || ImGui::FindWindowByName(PreviewWindow) == nullptr)
	{
		return;
	}
	Checked = true;

	ImGuiWindow* EditMode = ImGui::FindWindowByName("Edit Mode");
	if (HasOwnDockNode(ItemsWindow, EditMode) && HasOwnDockNode(PreviewWindow, nullptr))
	{
		return;
	}

	ImGuiID Center = Viewport->DockId;
	ImGuiID Left = ImGui::DockBuilderSplitNode(Center, ImGuiDir_Left, 0.22f, nullptr, &Center);

	ImGui::DockBuilderDockWindow(ItemsWindow, Left);
	ImGui::DockBuilderDockWindow(PreviewWindow, Center);
	ImGui::DockBuilderFinish(ImGui::DockNodeGetRootNode(Viewport->DockNode)->ID);
}

void UIShaderEditorForm::Draw()
{
	BuildWorkspaceLayout();

	if (ImGui::Begin(ItemsWindow))
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

	if (ImGui::Begin(PropertiesWindow))
	{
		if (XRay::ImGui::BeginDarkChild("ShaderPropertiesBorder"))
		{
			Tool->m_ItemProps->Draw();
			XRay::ImGui::EndDarkChild();
		}
	}
	ImGui::End();

	Tool->OnDrawUI();
}

void UIShaderEditorForm::Render()
{
	// The shader tool is the global Tools while the tab is active: the editor renders it directly
}

static const char* GetToolIcon(EToolsID ID)
{
	switch (ID)
	{
	case aeEngine:		return ICON_FA_CUBE;
	case aeCompiler:	return ICON_FA_GEARS;
	case aeMtl:			return ICON_FA_LAYER_GROUP;
	case aeMtlPair:		return ICON_FA_LINK;
	case aeSoundEnv:	return ICON_FA_VOLUME_HIGH;
	default:			return ICON_FA_CODE;
	}
}

void UIShaderEditorForm::DrawRenderToolBar(ImVec2 Pos, ImVec2 Size)
{
	const float ButtonSize = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ButtonSize);
	const float ToolbarPadding = XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::ToolbarPadding);
	const ImGuiStyle& Style = ImGui::GetStyle();

	ImGui::PushStyleColor(ImGuiCol_ChildBg, XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::PanelBorderTint).Value);
	ImGui::BeginChild("##ShaderToolbar", { 0, ButtonSize + ToolbarPadding * 2 }, 0, ImGuiWindowFlags_NoScrollbar);
	ImGui::PopStyleColor();

	auto& ToolsMap = Tool->m_Tools;

	string256 Labels[aeMaxTools];
	float FullWidth = 0.0f;
	int Count = 0;
	for (auto& [ID, SHTool] : ToolsMap)
	{
		if (Count >= (int)std::size(Labels))
		{
			break;
		}

		xr_sprintf(Labels[Count], "%s  %s", GetToolIcon(ID), SHTool->ToolsName());
		FullWidth += ImGui::CalcTextSize(Labels[Count]).x + Style.FramePadding.x * 2.0f;
		++Count;
	}

	const bool IconsOnly = FullWidth + ToolbarPadding * 2.0f > ImGui::GetContentRegionAvail().x;
	const bool Shortcuts = !ImGui::GetIO().WantTextInput && ImGui::GetIO().KeyCtrl;

	ImGui::SetCursorPos({ ToolbarPadding, ToolbarPadding });
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));

	int Index = 0;
	for (auto& [ID, SHTool] : ToolsMap)
	{
		if (Index >= Count)
		{
			break;
		}

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

		if (Index > 0)
		{
			ImGui::SameLine();
		}

		ImGui::PushID(Index);

		bool Active = Tool->m_Current == SHTool;
		const char* Label = IconsOnly ? GetToolIcon(ID) : Labels[Index];
		const bool Shortcut = Shortcuts && Index < 9 && ImGui::IsKeyPressed((ImGuiKey)(ImGuiKey_1 + Index), false);

		if ((XRay::ImGui::ToolbarButton("##tool", Label, &Active, { 0, ButtonSize }, Rounding) || Shortcut) && Tool->m_Current != SHTool)
		{
			Tool->OnChangeEditor(SHTool);
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			if (Index < 9)
			{
				ImGui::SetTooltip("%s (Ctrl+%d)", SHTool->ToolsName(), Index + 1);
			}
			else
			{
				ImGui::SetTooltip("%s", SHTool->ToolsName());
			}
		}

		ImGui::PopID();
		++Index;
	}

	ImGui::PopStyleVar();
	ImGui::EndChild();
}

void UIShaderEditorForm::DrawItemList()
{
	ISHTools* Current = Tool->m_Current;
	if (Current == nullptr)
	{
		return;
	}

	if (ImGui::Button(ICON_FA_PLUS " Create"))
	{
		Current->OnCreateItem("new_item");
		Current->Modified();
	}
	ImGui::SameLine();

	if (ImGui::Button(ICON_FA_CLONE " Clone"))
	{
		if (Current->m_CurrentItem != nullptr)
		{
			xr_string CloneName = Current->m_CurrentItem->Key();
			CloneName += "_clone";
			Current->OnCloneItem(Current->m_CurrentItem->Key(), CloneName.c_str());
		}
	}
	ImGui::SameLine();

	if (ImGui::Button(ICON_FA_TRASH_CAN " Remove"))
	{
		Current->RemoveCurrent();
	}

	if (!Tool->m_PreviewProps->Empty())
	{
		ImGui::SetNextItemOpen(true, ImGuiCond_Once);
		if (XRay::ImGui::BeginExpand("Preview"))
		{
			if (ImGui::BeginChild("##ShaderPreviewProps", { 0, 68 }))
			{
				Tool->m_PreviewProps->Draw();
			}
			ImGui::EndChild();
			XRay::ImGui::EndExpand();
		}
	}

	ImGui::SetNextItemOpen(true, ImGuiCond_Once);
	if (XRay::ImGui::BeginExpand("Items"))
	{
		ImGui::BeginGroup();
		Tool->m_Items->Draw();
		ImGui::EndGroup();
		XRay::ImGui::EndExpand();
	}
}
