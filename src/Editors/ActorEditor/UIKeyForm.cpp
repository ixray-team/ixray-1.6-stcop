#include "stdafx.h"

namespace detail
{
	float RoundToTwoDecimals(float value)
	{
		return std::round(value * 100.0f) / 100.0f;
	}

	bool compareFloat(float a, float b, int precision = 2)
	{
		float scale = std::pow(10, precision);
		return std::round(a * scale) == std::round(b * scale);
	}
}

UIKeyForm::UIKeyForm():m_AutoChange(true), m_TimeFactor(1), m_Position(0), m_currentEditMotion(nullptr)
{
}

UIKeyForm::~UIKeyForm()
{
}

void UIKeyForm::Draw()
{
	m_currentEditMotion = ATools->GetCurrentMotion();

	bool bMarksPresent12 = (m_currentEditMotion && m_currentEditMotion->marks.size() >= 2);
	bool bMarksPresent34 = (m_currentEditMotion && m_currentEditMotion->marks.size() == 4);

	bool Mark1 = bMarksPresent12 || ((CAEPreferences*)EPrefs)->bAlwaysShowKeyBar12 || ((CAEPreferences*)EPrefs)->bAlwaysShowKeyBar34;
	bool Mark2 = bMarksPresent12 || ((CAEPreferences*)EPrefs)->bAlwaysShowKeyBar12 || ((CAEPreferences*)EPrefs)->bAlwaysShowKeyBar34;
	bool Mark3 = bMarksPresent34 || ((CAEPreferences*)EPrefs)->bAlwaysShowKeyBar34;
	bool Mark4 = bMarksPresent34 || ((CAEPreferences*)EPrefs)->bAlwaysShowKeyBar34;

	ImGui::Begin("KeyForm");
	{

		float a, b, c;
		ATools->GetStatTime(a, b, c);

		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0, 0));
		ImGui::BeginChild("Left", ImVec2(60, 0));
		{
		
			ImGui::Checkbox("Auto", &m_AutoChange);
			ImGui::Text("Left1");
			ImGui::Text("Right1");
			ImGui::Text("Left2");
			ImGui::Text("Right2");
			ImGui::Text("Notify");
			ImGui::Separator();
			ImGui::EndChild();
		}ImGui::SameLine();
		ImGui::BeginChild("Midle", ImVec2(-120, 0));
		{
			ImGui::SetNextItemWidth(-1);
			if (AutoChange())m_Position = c ;
			ImGui::SliderFloat("##key1", &m_Position, a , b , "%.4f");
			ImGui::SetNextItemWidth(-1);
			ImVec2 size = ImGui::GetItemRectSize();
			static float Zero = 0;
			if (size.x!= m_TempForPlotHistogram.size())
			{
				m_TempForPlotHistogram.resize(size.x);
			}
			if (!Mark1) ImGui::BeginDisabled();
			if (Mark1)
				DrawMark(0);
			ImGui::PlotHistogram("##left1", Mark1?m_TempForPlotHistogram .data():&Zero, Mark1 ? m_TempForPlotHistogram.size() : 1, 0, NULL, 0.0f, 1.0f, size);
			if (!Mark1) ImGui::EndDisabled();

			if (!Mark2) ImGui::BeginDisabled();
			if (Mark2)
				DrawMark(1);
			ImGui::PlotHistogram("##right1", Mark2 ? m_TempForPlotHistogram.data() : &Zero, Mark2 ? m_TempForPlotHistogram.size() : 1, 0, NULL, 0.0f, 1.0f, size);
			if (!Mark2) ImGui::EndDisabled();

			if (!Mark3) ImGui::BeginDisabled();
			if (Mark3)
				DrawMark(2);
			ImGui::PlotHistogram("##left2", Mark3 ? m_TempForPlotHistogram.data() : &Zero, Mark3 ? m_TempForPlotHistogram.size() : 1, 0, NULL, 0.0f, 1.0f, size);
			if (!Mark3) ImGui::EndDisabled();

			if (!Mark4) ImGui::BeginDisabled();
			if (Mark4)
				DrawMark(3);
			ImGui::PlotHistogram("##right2", Mark4 ? m_TempForPlotHistogram.data() : &Zero, Mark4 ? m_TempForPlotHistogram.size() : 1, 0, NULL, 0.0f, 1.0f, size);
			if (!Mark4) ImGui::EndDisabled();

			DrawNotify();
			ImGui::PlotHistogram("##animnotify", m_TempForPlotHistogram.data(), m_TempForPlotHistogram.size(), 0, NULL, 0.0f, 1.0f, size);

			if (ImGui::IsItemClicked(ImGuiMouseButton_Left))
			{
				float a, b, c;
				ATools->GetStatTime(a, b, c);
				float motion_length = b - a;

				ImVec2 MousePos = ImGui::GetMousePos();
				ImVec2 ItemPos = ImGui::GetItemRectMin();
				ImVec2 ItemSize = ImGui::GetItemRectSize();
				float LocalPos = (MousePos.x - ItemPos.x) / ItemSize.x;

				float TimeOffset = detail::RoundToTwoDecimals(LocalPos * b);

				m_currentEditMotion->notify[TimeOffset] = {};
			}
			else if (ImGui::IsItemClicked(ImGuiMouseButton_Right))
			{
				float a, b, c;
				ATools->GetStatTime(a, b, c);
				float motion_length = b - a;

				ImVec2 MousePos = ImGui::GetMousePos();
				ImVec2 ItemPos = ImGui::GetItemRectMin();
				ImVec2 ItemSize = ImGui::GetItemRectSize();
				float LocalPos = (MousePos.x - ItemPos.x) / ItemSize.x;

				float TimeOffset = detail::RoundToTwoDecimals(LocalPos * b);
				float Step = detail::RoundToTwoDecimals(motion_length / 100);


				for (auto& [Time, _] : m_currentEditMotion->notify)
				{
					for (float StartKey = TimeOffset - Step; StartKey < (TimeOffset + Step * 2); StartKey += Step)
					{
						if (detail::compareFloat(Time, StartKey))
						{
							m_currentEditMotion->notify.erase(Time);
						}
					}
				}
			}

			ImGui::EndChild();
		}ImGui::SameLine();
		ImGui::BeginChild("Back", ImVec2(120, 0));
		{

			ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(2, 0));
			{
				ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
				ImGui::Text("LOD:"); ImGui::SameLine();
				ImGui::PopStyleVar();

			}
			ImGui::SetNextItemWidth(-1);
			float LOD_TimeFactor[2] = { ATools->m_RenderObject.m_fLOD,m_TimeFactor };
			ImGui::SliderFloat2("##lod", LOD_TimeFactor, 0, 1);
			ATools->m_RenderObject.m_fLOD = LOD_TimeFactor[0];
			if (m_TimeFactor != LOD_TimeFactor[1])
			{
				m_TimeFactor = LOD_TimeFactor[1];
				EDevice->time_factor(m_TimeFactor);
			}

			if (!Mark1) ImGui::BeginDisabled();
			ImGui::PushID("left1");
			if (ImGui::Button("Del") && Mark1) { SetMark(0, 3); } ImGui::SameLine(); 	if (ImGui::Button("Up") && Mark1) { SetMark(0, 2); } ImGui::SameLine(); 	if (ImGui::Button("Down", ImVec2(-1, 0)) && Mark1) { SetMark(0, 1); }
			ImGui::PopID();
			if (!Mark1) ImGui::EndDisabled();

			if (!Mark2) ImGui::BeginDisabled();
			ImGui::PushID("right1");
			if (ImGui::Button("Del") && Mark2) { SetMark(1, 3); } ImGui::SameLine(); 	if (ImGui::Button("Up") && Mark2) { SetMark(1, 2); } ImGui::SameLine(); 	if (ImGui::Button("Down", ImVec2(-1, 0)) && Mark2) { SetMark(1, 1); }
			ImGui::PopID();
			if (!Mark2) ImGui::EndDisabled();

			if (!Mark3) ImGui::BeginDisabled();
			ImGui::PushID("left2");
			if (ImGui::Button("Del") && Mark3) { SetMark(2, 3); } ImGui::SameLine(); 	if (ImGui::Button("Up") && Mark3) { SetMark(2, 2); } ImGui::SameLine(); 	if (ImGui::Button("Down", ImVec2(-1, 0)) && Mark3) { SetMark(2, 1); }
			ImGui::PopID();
			if (!Mark3) ImGui::EndDisabled();

			if (!Mark4) ImGui::BeginDisabled();
			ImGui::PushID("right2");
			if (ImGui::Button("Del") && Mark4) { SetMark(3, 3); } ImGui::SameLine(); 	if (ImGui::Button("Up") && Mark4) { SetMark(3, 2); } ImGui::SameLine(); 	if (ImGui::Button("Down", ImVec2(-1, 0)) && Mark4) { SetMark(3, 1); }
			ImGui::PopID();
			if (!Mark4) ImGui::EndDisabled();

			ImGui::PopStyleVar();
			ImGui::EndChild();
		}
		ImGui::PopStyleVar();
	}
	ImGui::End();
}
inline bool interval_comparer(const motion_marks::interval& i1, const motion_marks::interval& i2)
{
	return (i1.first < i2.first);
}
void UIKeyForm::SetMark(int id, int action)
{
	if (!m_currentEditMotion)
		return;

	if (m_currentEditMotion->marks.size() == 0)
		return;

	motion_marks& M = m_currentEditMotion->marks[id];
	float a, b, c;
	ATools->GetStatTime(a, b, c);
	float cur_time = c - a;

	motion_marks::ITERATOR it = M.intervals.begin();
	motion_marks::ITERATOR it_e = M.intervals.end();

	if (action == 3)
	{ //del current

		for (; it != it_e; ++it)
		{
			motion_marks::interval& iv = *it;
			if (iv.first<cur_time && iv.second>cur_time)
			{
				M.intervals.erase(it);
				break;
			}
		}
	}
	else if (action == 2)
	{//up
		for (; it != it_e; ++it)
		{
			motion_marks::interval& iv = *it;
			if (iv.first<cur_time && iv.second>cur_time)
			{
				iv.second = cur_time;
				break;
			}
		}
	}
	else if (action == 1)
	{//down
		for (; it != it_e; ++it)
		{
			motion_marks::interval& iv = *it;
			if (iv.first<cur_time && iv.second>cur_time)
			{
				iv.first = cur_time;
				break;
			}
		}
		if (it == it_e)
		{//insert new
			M.intervals.push_back(motion_marks::interval(cur_time, b - a));
		}
	}

	std::sort(M.intervals.begin(), M.intervals.end(), interval_comparer);
}

void UIKeyForm::DrawNotify()
{
	std::memset(m_TempForPlotHistogram.data(), 0, sizeof(float) * m_TempForPlotHistogram.size());

	float a, b, c;
	ATools->GetStatTime(a, b, c);
	float motion_length = b - a;

	if (motion_length == 0)
		return;

	float k_len = m_TempForPlotHistogram.size() / motion_length;
	for (auto [Time, Notify] : m_currentEditMotion->notify)
	{
		float Key = Time * k_len;

		for (int KeyStart = int(Key) - 5; KeyStart < int(Key + 5); KeyStart++)
		{
			if (KeyStart < 0)
				continue;

			m_TempForPlotHistogram[KeyStart] = 1;
		}
	}
}

void UIKeyForm::DrawMark(int id)
{
	std::memset(m_TempForPlotHistogram.data(), 0, sizeof(float) * m_TempForPlotHistogram.size());

	if (!m_currentEditMotion)
		return;

	motion_marks& M = m_currentEditMotion->marks[id];

	float a, b, c;
	ATools->GetStatTime(a, b, c);
	float motion_length = b - a;

	if (motion_length == 0)
		return;

	float k_len = m_TempForPlotHistogram.size() / motion_length;

	motion_marks::C_ITERATOR it = M.intervals.begin();
	motion_marks::C_ITERATOR it_e = M.intervals.end();

	for (; it != it_e; ++it)
	{
		const motion_marks::interval& iv = *it;
		Ivector2	posLT, posRB;
		for (int i = iv.first * k_len; i < iv.second * k_len; i++)
		{
			m_TempForPlotHistogram[i] = 1;
		}
	}
}
