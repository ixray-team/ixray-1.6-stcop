#include "stdafx.h"
#include "UI_PACEditorForm.h"
#include <imgui_internal.h>
#include "implot.h"

#include "../../Layers/xrRender/ParticleAnimCurve.h"

UIPACEditorForm* UIPACEditorForm::Form = nullptr;

UIPACEditorForm::UIPACEditorForm()
{
	ImPlot::CreateContext();
}

UIPACEditorForm::~UIPACEditorForm()
{
	ImPlot::DestroyContext();
}

void UIPACEditorForm::Draw()
{
    //if (ImGui::Begin("PAC Editor", 0))
    //{
	    DrawCurves();
    //}
	
	//ImGui::End();
}

void UIPACEditorForm::Open(PS::CPACDef* EditedPAC)
{
	VERIFY(!Form);

	Form = new UIPACEditorForm();
	Form->EditedPAC = EditedPAC;
	Form->EditedPAC->SplitKeysForPlot(Form->R_keys_y, Form->G_keys_y, Form->B_keys_y, Form->A_keys_y, Form->keys_x);
	Form->keys_y_dummy.resize(Form->keys_x.size(), 0.0f);
}

void UIPACEditorForm::Update()
{
	if (Form && !Form->IsClosed())
	{
		ImGui::OpenPopup("Particle Animation Curve Editor");
		ImGui::SetNextWindowSize(ImVec2(400, 500), ImGuiCond_::ImGuiCond_FirstUseEver);
		if (ImGui::BeginPopupModal("Particle Animation Curve Editor", nullptr,0))
		{
			Form->Draw();
			ImGui::EndPopup();
		}
	}
}

void UIPACEditorForm::DrawCurves()
{
	static bool ShowTest = false;
	if (ImGui::Button("demo_test"))
	{
		ShowTest = !ShowTest;
	}
	if (ShowTest)
	{
		ImPlot::ShowDemoWindow();
	}
	static ImPlotSubplotFlags flags = ImPlotSubplotFlags_ShareItems|ImPlotSubplotFlags_NoLegend;
	static float rratios[] = {5,1};
	static float cratios[] = {1};
	float XSpace = EditedPAC->GetMaxTime()*0.05f;
	float YSpace = std::fabs(EditedPAC->GetMinValue()*0.1f)+std::fabs(EditedPAC->GetMaxValue()*0.1f)/2;
	if (ImPlot::BeginSubplots("Animation Curves", 2, 1, ImVec2(-1,400), flags, rratios, cratios))
	{
		if (ImPlot::BeginPlot("Curves", ImVec2(-1,0), ImPlotFlags_NoTitle|ImPlotFlags_NoLegend))
		{
			ImPlot::SetupAxes(nullptr, "value", ImPlotAxisFlags_NoDecorations);
			ImPlot::SetupAxisLinks(ImAxis_X1, &LinkXMin, &LinkXMax);
			ImPlot::SetupAxesLimits(-XSpace,EditedPAC->GetMaxTime()+XSpace,EditedPAC->GetMinValue()-YSpace,EditedPAC->GetMaxValue()+YSpace);
			ImPlot::SetNextLineStyle(ImColor(255,0,0));
			ImPlot::PlotLine("R", keys_x.data(), R_keys_y.data(), R_keys_y.size());
			ImPlot::SetNextLineStyle(ImColor(0,255,0));
			ImPlot::PlotLine("G", keys_x.data(), G_keys_y.data(), G_keys_y.size());
			ImPlot::SetNextLineStyle(ImColor(0,0,255));
			ImPlot::PlotLine("B", keys_x.data(), B_keys_y.data(), B_keys_y.size());
			ImPlot::SetNextLineStyle(ImColor(255,255,255));
			ImPlot::PlotLine("A", keys_x.data(), A_keys_y.data(), A_keys_y.size());
			ImPlot::EndPlot();
		}
		if (ImPlot::BeginPlot("Times", ImVec2(-1,0), ImPlotFlags_NoTitle|ImPlotFlags_NoLegend))
		{
			ImPlot::SetupAxes("t (msec)", nullptr, 0,ImPlotAxisFlags_NoDecorations|ImPlotAxisFlags_Lock);
			ImPlot::SetupAxisLinks(ImAxis_X1, &LinkXMin, &LinkXMax);
			ImPlot::SetupAxesLimits(-XSpace,EditedPAC->GetMaxTime()+XSpace,-0.01,0.01);
			ImPlot::PlotScatter("Points", keys_x.data(), keys_y_dummy.data(), keys_x.size());
			ImPlot::EndPlot();
		}
        ImPlot::EndSubplots();
	}
}
