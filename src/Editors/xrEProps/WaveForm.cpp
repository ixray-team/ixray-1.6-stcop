#include "stdafx.h"
#include "WaveForm.h"

CWaveForm* CWaveForm::form;
void CWaveForm::DrawGraph(ImVec2 Size)
{
	int Width = Size.x - 4;
	int Height = Size.y - 4;

	if (Width <= 0 || Height <= 0)
	{
		return;
	}

	ImGui::BeginChild("Graph", ImVec2(Width + 4, Height + 4), true);

	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	ImVec2 P0 = ImGui::GetCursorScreenPos();

	// Фон и рамка
	DrawList->AddRectFilled(P0, ImVec2(P0.x + Width + 4, P0.y + Height + 4), IM_COL32(0, 0, 0, 255));
	DrawList->AddRect(P0, ImVec2(P0.x + Width + 4, P0.y + Height + 4), IM_COL32(0, 102, 0, 255));

	float Delta = m_CurFunc.arg[1] * 2;
	Delta = Delta ? (Height / Delta) : 0;
	float AxisX = Height - (Delta * (-m_CurFunc.arg[0]) + Height / 2);

	DrawList->AddLine
	(
		ImVec2(P0.x + 2, P0.y + AxisX + 2),
		ImVec2(P0.x + Width + 2, P0.y + AxisX + 2),
		IM_COL32(0, 255, 0, 255)
	);

	// Точки графика
	float TCost = 1.f / Width;
	xr_vector<ImVec2> Points;
	Points.reserve(Width);

	float Tm = 0;
	float Y = m_CurFunc.Calculate(Tm) - m_CurFunc.arg[0];
	float YY = Height - (Delta * Y + Height / 2);
	ImVec2 StartPoint = ImVec2(P0.x + 2, P0.y + YY + 2);
	Points.push_back(StartPoint);

	for (int t = 1; t < Width; t++)
	{
		Tm = scale * t * TCost / (fis_zero(m_CurFunc.arg[3]) ? 1.f : m_CurFunc.arg[3]);
		Y = m_CurFunc.Calculate(Tm) - m_CurFunc.arg[0];
		YY = Height - (Delta * Y + Height / 2);
		Points.push_back(ImVec2(P0.x + t + 2, P0.y + YY + 2));
	}

	if (!Points.empty())
	{
		DrawList->AddPolyline(Points.data(), Points.size(), IM_COL32(255, 255, 0, 255), false, 1.0f);
	}

	ImGui::EndChild();
}

void CWaveForm::Draw()
{
	if (!IsOpen) return;

	ImGui::SetNextWindowSizeConstraints
	(
		ImVec2(422, 380),
		ImVec2(FLT_MAX, FLT_MAX)
	);

	if (ImGui::Begin("Wave Form", &IsOpen))
	{
		ImGui::Text("Function:"); ImGui::SameLine();
		xr_vector<const char*> FuncNames;
		for (auto& F : function_token)
		{
			FuncNames.push_back(F.name);
		}
		
		ImGui::SetNextItemWidth(200);
		ImGui::Combo("##Function", &selectedFunction, FuncNames.data(), (int)FuncNames.size());

		ImGui::SetNextItemWidth(200);
		ImGui::InputFloat("Offset (arg1)", &arg[0], 0.01f, 1.0f, "%.3f");
		ImGui::SetNextItemWidth(200);
		ImGui::InputFloat("Amplitude (arg2)", &arg[1], 0.01f, 1.0f, "%.3f");
		ImGui::SetNextItemWidth(200);
		ImGui::InputFloat("Phase (arg3)", &arg[2], 0.01f, 1.0f, "%.3f");
		ImGui::SetNextItemWidth(200);
		ImGui::InputFloat("Rate (arg4)", &arg[3], 0.01f, 1.0f, "%.3f");
		ImGui::SetNextItemWidth(200);
		ImGui::InputFloat("Scale", &scale, 0.01f, 0.1f);

		UpdateFuncData();

		ImGui::Text("Waveform Preview:");
		ImGui::SameLine();
		ImGui::Text("Min: %s  Center: %s  Max: %s", lbMin, lbCenter, lbMax);
		ImGui::Dummy(ImVec2(0, 4));
		DrawGraph(ImVec2(400, 150));

		if (ImGui::Button("Ok"))
		{
			IsOpen = false;
			ResultStatus = true;
		}
		ImGui::SameLine();

		if (ImGui::Button("Cancel"))
		{
			IsOpen = false;
		}
	}
	ImGui::End();
}

void CWaveForm::UpdateFuncData()
{
	if (bLoadMode)
		return;

	m_CurFunc.F = (WaveForm::EFunction)selectedFunction;
	for (int i = 0; i < 4; i++) m_CurFunc.arg[i] = arg[i];

	// Labels
	snprintf(lbMax, sizeof(lbMax), "%.2f", arg[0] + arg[1]);
	snprintf(lbMin, sizeof(lbMin), "%.2f", arg[0] - arg[1]);
	snprintf(lbCenter, sizeof(lbCenter), "%.2f", arg[0]);

	float v = scale * 1000 / ((arg[3] == 0) ? 1.f : arg[3]);
	if (v <= 1000) snprintf(lbEnd, sizeof(lbEnd), "%4.0f ms", v);
	else snprintf(lbEnd, sizeof(lbEnd), "%.2f s", v);
}