#include "stdafx.h"

UITopBarForm::UITopBarForm()
{

#define ADD_BUTTON_IMAGE_T1(Class,Name)
#define ADD_BUTTON_IMAGE_T2(Class,Name)
#define ADD_BUTTON_IMAGE_S(Name)	m_t##Name = EDevice->Resources->_CreateTexture("ed\\bar\\"#Name);m_t##Name->Load();m_time##Name = 0;
#define ADD_BUTTON_IMAGE_D(Name) 	m_t##Name = EDevice->Resources->_CreateTexture("ed\\bar\\"#Name);m_t##Name->Load();m_b##Name = false;
#include "UITopBarForm_ButtonList.h"
	RefreshBar();
}

UITopBarForm::~UITopBarForm()
{
	
}

void UITopBarForm::Draw()
{
	ImGuiViewport* viewport = ImGui::GetMainViewport();
	ImGui::SetNextWindowPos(ImVec2(viewport->Pos.x, viewport->Pos.y + UI->GetMenuBarHeight()));
	ImGui::SetNextWindowSize(ImVec2(viewport->Size.x, UIToolBarSize));
	ImGui::SetNextWindowViewport(viewport->ID);

	ImGuiWindowFlags window_flags = 0
		| ImGuiWindowFlags_NoDocking
		| ImGuiWindowFlags_NoTitleBar
		| ImGuiWindowFlags_NoResize
		| ImGuiWindowFlags_NoMove
		| ImGuiWindowFlags_NoScrollbar
		| ImGuiWindowFlags_NoSavedSettings
		;
	ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding,ImVec2( 2,2));
	ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(2, 2));
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(4, 2));
	ImGui::Begin("TOOLBAR", NULL, window_flags);
	{
#define ADD_BUTTON_IMAGE_S(Name)\
		if (ImGui::ImageButton(m_t##Name->surface_get(), ImVec2(20, 20), ImVec2(m_time##Name>EDevice->TimerAsync() ? 0.5 : 0, 0), ImVec2(m_time##Name>EDevice->TimerAsync() ? 1 : 0.5, 1), 0))\
		{\
			m_time##Name = EDevice->TimerAsync() + 130;\
			Click##Name();\
		}ImGui::SameLine();
#include "UITopBarForm_ButtonList.h"
		bool Simulate = ATools->IsPhysics();

		if (ImGui::Checkbox("Simulate", &Simulate))
		{
			if(Simulate)
				ATools->PhysicsSimulate();
			else
				ATools->PhysicsStopSimulate();
		}
	}
	ImGui::End();
	ImGui::PopStyleVar(5);
}
void UITopBarForm::RefreshBar()
{
}

void UITopBarForm::ClickUndo()
{
	ExecCommand(COMMAND_UNDO);
}

void UITopBarForm::ClickRedo()
{
	ExecCommand(COMMAND_REDO);
}