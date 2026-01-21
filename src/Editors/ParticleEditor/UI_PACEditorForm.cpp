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
	Form->dkeys_x.resize(Form->keys_x.size());
	for (size_t i = 0; i < Form->keys_x.size(); i++)
	{
		Form->dkeys_x[i].Value = Form->keys_x[i];
		Form->dkeys_x[i].Index = i;
	}
	Form->keys_y_ddummy.resize(Form->keys_x.size(), 0.0);
}

void UIPACEditorForm::Update()
{
	if (Form)
	{
		if(!Form->IsClosed())
		{
			string256 buff;
			xr_sprintf(buff, sizeof(buff), "%s (%s)", "PAC Editor", Form->EditedPAC->getName());
			ImGui::OpenPopup(buff);
			ImGui::SetNextWindowSize(ImVec2(1000, 0), ImGuiCond_::ImGuiCond_Always);
			if (ImGui::BeginPopupModal(buff, &Form->bOpen,0))
			{
				Form->Draw();
				ImGui::EndPopup();
			}
		} else
		{
			xr_delete(Form);
		}
	}
}

void UIPACEditorForm::DrawCurves()
{
	static bool ShowTest = false;
	constexpr bool ShowDemo = false;
	if constexpr (ShowDemo)
	{
		if (ImGui::Button("demo_test"))
		{
			ShowTest = !ShowTest;
		}
		if (ShowTest)
		{
			ImPlot::ShowDemoWindow();
		}
		ImGui::SameLine();
	}
	ImGui::Text("Ctrl + click on time bar to add key");
	static ImPlotSubplotFlags flags = ImPlotSubplotFlags_ShareItems|ImPlotSubplotFlags_NoLegend;
	static float rratios[] = {5,1};
	static float cratios[] = {1};
	static struct DrawAxisData
	{
		bool R = true;
		bool G = true;
		bool B = true;
		bool A = true;
	} DrawData;
	float MaxTime = EditedPAC->GetMaxTime();
	if (fis_zero(MaxTime))
	{
		MaxTime = 100;
	}
	float XSpace = MaxTime*0.05f;
	float YSpace = std::fabs(EditedPAC->GetMinValue()*0.1f)+std::fabs(EditedPAC->GetMaxValue()*0.1f)/2;
	auto cmp = [&](double key_a, double key_b)
	{
		return key_a < key_b;
	};
	auto proj = [&](const DoubleKey& key)
	{
		return key.Value;
	};
	auto AfterChangeTimeFunc = [&](size_t i)
	{
		auto temp = dkeys_x[i];
		dkeys_x.erase(dkeys_x.begin()+i);
		auto FoundIt = std::ranges::upper_bound(dkeys_x, temp.Value, cmp, proj);
		size_t NewIndex = FoundIt-dkeys_x.begin();
		if (FoundIt == dkeys_x.end())
		{
			if (dsimilar((FoundIt-1)->Value, temp.Value))
			{
				temp.Value += 1;
			}
			dkeys_x.push_back(temp);
		} else
		{
			if (FoundIt != dkeys_x.begin() && dsimilar((FoundIt-1)->Value, temp.Value))
			{
				temp.Value += 1;
			}
			dkeys_x.emplace(FoundIt, temp);
		}
		for (size_t i = 0; i < dkeys_x.size(); i++)
		{
			dkeys_x[i].Index = i;
		}
		auto Func = [&]<typename T>(xr_vector<T>& vec){
			auto temp = vec[i];
			vec.erase(vec.begin()+i);
			vec.emplace(vec.begin()+NewIndex, temp);
		};
		Func(R_keys_y);
		Func(G_keys_y);
		Func(B_keys_y);
		Func(A_keys_y);
		Func(keys_x);
		keys_x[NewIndex] = dkeys_x[NewIndex].Value;
		SelectedKeyframeIndex = NewIndex;
	};
	{
		ImGui::PushID("AxisSwitch");
#define DrawAxisSwitch(X, sameline) \
		ImGui::Checkbox(#X, &DrawData.##X); \
		if constexpr(sameline){ ImGui::SameLine(); }
		DrawAxisSwitch(R, true);
		DrawAxisSwitch(G, true);
		DrawAxisSwitch(B, true);
		DrawAxisSwitch(A, false);
#undef DrawAxisSwitch
		ImGui::PopID();
	}
	if (ImPlot::BeginSubplots("Animation Curves", 2, 1, ImVec2(-1,400), flags, rratios, cratios))
	{
		if (ImPlot::BeginPlot("Curves", ImVec2(-1,0), ImPlotFlags_NoTitle|ImPlotFlags_NoLegend|ImPlotFlags_NoMenus))
		{
			ImPlot::SetupAxes(nullptr, "value", ImPlotAxisFlags_NoDecorations);
			ImPlot::SetupAxisLinks(ImAxis_X1, &LinkXMin, &LinkXMax);
			ImPlot::SetupAxesLimits(-XSpace,MaxTime+XSpace,EditedPAC->GetMinValue()-YSpace,EditedPAC->GetMaxValue()+YSpace);
			if (DrawData.R)
			{
				ImPlot::SetNextLineStyle(ImColor(255,0,0));
				ImPlot::PlotLine("R", keys_x.data(), R_keys_y.data(), R_keys_y.size());
			}
			if (DrawData.G)
			{
				ImPlot::SetNextLineStyle(ImColor(0,255,0));
				ImPlot::PlotLine("G", keys_x.data(), G_keys_y.data(), G_keys_y.size());
			}
			if (DrawData.B)
			{
				ImPlot::SetNextLineStyle(ImColor(0,0,255));
				ImPlot::PlotLine("B", keys_x.data(), B_keys_y.data(), B_keys_y.size());
			}
			if (DrawData.A)
			{
				ImPlot::SetNextLineStyle(ImColor(255,255,255));
				ImPlot::PlotLine("A", keys_x.data(), A_keys_y.data(), A_keys_y.size());
			}
			ImPlot::EndPlot();
		}
		if (ImPlot::BeginPlot("Times", ImVec2(-1,0), ImPlotFlags_NoTitle|ImPlotFlags_NoLegend|ImPlotFlags_NoMenus))
		{
			ImPlot::SetupAxes("t (msec)", nullptr, 0,ImPlotAxisFlags_NoDecorations|ImPlotAxisFlags_Lock);
			ImPlot::SetupAxisLinks(ImAxis_X1, &LinkXMin, &LinkXMax);
			ImPlot::SetupAxesLimits(-XSpace,MaxTime+XSpace,-0.01,0.01);
			struct
			{
				bool NeedInsert = false;
				ImPlotPoint pt;
			} InsertData;
			if (ImPlot::IsPlotHovered() && ImGui::IsMouseClicked(0) && ImGui::GetIO().KeyCtrl) {
				InsertData.pt = ImPlot::GetPlotMousePos();
				InsertData.pt.y = 0.0;
				InsertData.NeedInsert = true;
			}
			struct UpdatedData
			{
				size_t Index;
				bool HasData = false;
			} updated_data;
			for (size_t i = 0; i < keys_x.size(); i++)
			{
				ImPlot::DragPoint(
					i,
					&(dkeys_x[i].Value),
					&(keys_y_ddummy[i]),
					ImColor(10,10,255),
					4,
					0,
					&dkeys_x[i].clicked,
					&dkeys_x[i].hovered,
					&dkeys_x[i].hold);
				keys_y_ddummy[i] = 0.0;
				if (dkeys_x[i].clicked)
				{
					SelectedKeyframeIndex = i;
				}
				if (dkeys_x[i].hold)
				{
					dkeys_x[i].held = true;
					keys_x[dkeys_x[i].Index] = dkeys_x[i].Value;
				} else if (dkeys_x[i].held)
				{
					dkeys_x[i].held = false;
					updated_data.HasData = true;
					updated_data.Index = i;
				}
			}
			if (updated_data.HasData)
			{
				updated_data.HasData = false;
				auto i = updated_data.Index;
				AfterChangeTimeFunc(i);
			}
			if (InsertData.NeedInsert)
			{
				InsertData.NeedInsert = false;
				auto& pt = InsertData.pt;
				if (dkeys_x.empty())
				{
					DoubleKey NewKey;
					NewKey.Index = 0;
					NewKey.Value = pt.x;
					dkeys_x.push_back(NewKey);
					keys_x.push_back(pt.x);
					keys_y_ddummy.push_back(0);
					R_keys_y.push_back(1);
					G_keys_y.push_back(1);
					B_keys_y.push_back(1);
					A_keys_y.push_back(1);
				} else
				{
					auto FoundIt = std::ranges::upper_bound(dkeys_x, pt.x, cmp, proj);
					size_t NewIndex = std::distance(dkeys_x.begin(), FoundIt);
					DoubleKey NewKey;
					NewKey.Value = pt.x;
					NewKey.Index = NewIndex;
					if (FoundIt == dkeys_x.end())
					{
						dkeys_x.push_back(NewKey);
						keys_x.push_back(pt.x);
						keys_y_ddummy.push_back(0);
						R_keys_y.push_back(R_keys_y.back());
						G_keys_y.push_back(G_keys_y.back());
						B_keys_y.push_back(B_keys_y.back());
						A_keys_y.push_back(A_keys_y.back());
					} else if (FoundIt == dkeys_x.begin())
					{
						dkeys_x.insert(dkeys_x.begin(), NewKey);
						keys_x.insert(keys_x.begin(), pt.x);
						keys_y_ddummy.insert(keys_y_ddummy.begin(), 0);
						R_keys_y.insert(R_keys_y.begin(), R_keys_y[0]);
						G_keys_y.insert(G_keys_y.begin(), G_keys_y[0]);
						B_keys_y.insert(B_keys_y.begin(), B_keys_y[0]);
						A_keys_y.insert(A_keys_y.begin(), A_keys_y[0]);
					} else
					{
						dkeys_x.insert(dkeys_x.begin()+NewIndex, NewKey);
						auto InsertInterpolated = [&](xr_vector<float>& vec)
						{
							float Alpha = (keys_x[NewIndex] - pt.x)/(keys_x[NewIndex]-keys_x[NewIndex-1]);
							vec.insert(vec.begin()+NewIndex, vec[NewIndex-1]+(vec[NewIndex]-vec[NewIndex-1])*Alpha);
						};
						InsertInterpolated(R_keys_y);
						InsertInterpolated(G_keys_y);
						InsertInterpolated(B_keys_y);
						InsertInterpolated(A_keys_y);
						keys_x.insert(keys_x.begin()+NewIndex, pt.x);
						keys_y_ddummy.insert(keys_y_ddummy.begin()+NewIndex, 0);
					}
					SelectedKeyframeIndex = NewIndex;
				}
			}
			ImPlot::EndPlot();
		}
        ImPlot::EndSubplots();
	}

	if (SelectedKeyframeIndex < dkeys_x.size())
	{
		bool NeedUpdate = false;
		bool NeedDelete = ImGui::Button("Delete Key");
		ImGui::Text("Time");
		ImGui::SameLine();
		NeedUpdate |= ImGui::InputDouble("Time Input", &dkeys_x[SelectedKeyframeIndex].Value, 0, 0, "%.0f");
		static xr_vector<int> Buttons = {-100, -10, -1, 1, 10, 100};
		for (auto elem : Buttons)
		{
			ImGui::SameLine();
			if (ImGui::Button(std::to_string(elem).c_str()))
			{
				dkeys_x[SelectedKeyframeIndex].Value += elem;
				NeedUpdate |= true;
			}
		}
		if (NeedUpdate)
		{
			if (dkeys_x[SelectedKeyframeIndex].Value <= 0)
			{
				auto ValidateFirstElems = [&](size_t index)
				{
					do
					{
						dkeys_x[index++].Value++;
					} while (index < dkeys_x.size()-1 && index < SelectedKeyframeIndex && dkeys_x[index-1].Value == dkeys_x[index].Value);
				};
				ValidateFirstElems(0);
				dkeys_x[SelectedKeyframeIndex].Value = 0;
			}
			AfterChangeTimeFunc(SelectedKeyframeIndex);
		}
		ImGui::Separator();
		auto ChannelButtonsFunc = [&](LPCSTR Channel, xr_vector<float>& vec)
		{
			ImGui::Text(Channel);
			ImGui::SameLine();
			string16 buf;
			ImGui::InputFloat(xr_strconcat(buf, Channel, " Input"), &vec[SelectedKeyframeIndex], 0, 0, "%.2f");
			static xr_vector<float> Buttons = {-1.0f, -0.1f, -0.01f, 0.01f, 0.1f, 1.0f};
			ImGui::PushID(Channel);
			for (auto elem : Buttons)
			{
				ImGui::SameLine();
				xr_sprintf(buf, sizeof(buf), "%.2f", elem);
				if (ImGui::Button(buf))
				{
					vec[SelectedKeyframeIndex] += elem;
				}
			}
			ImGui::PopID();
		};
		ChannelButtonsFunc("R", R_keys_y);
		ChannelButtonsFunc("G", G_keys_y);
		ChannelButtonsFunc("B", B_keys_y);
		ChannelButtonsFunc("A", A_keys_y);

		if (NeedDelete)
		{
			dkeys_x.erase(dkeys_x.begin()+SelectedKeyframeIndex);
			keys_y_ddummy.erase(keys_y_ddummy.begin()+SelectedKeyframeIndex);
			keys_x.erase(keys_x.begin()+SelectedKeyframeIndex);
			R_keys_y.erase(R_keys_y.begin()+SelectedKeyframeIndex);
			G_keys_y.erase(G_keys_y.begin()+SelectedKeyframeIndex);
			B_keys_y.erase(B_keys_y.begin()+SelectedKeyframeIndex);
			A_keys_y.erase(A_keys_y.begin()+SelectedKeyframeIndex);
		}
	}

	bool NeedClose = false;
	if (ImGui::Button("OK"))
	{
		EditedPAC->UpdateCurveFromKeys(R_keys_y, G_keys_y, B_keys_y, A_keys_y, keys_x);
		NeedClose = true;
	}
	ImGui::SameLine();
	if (ImGui::Button("Cancel"))
	{
		NeedClose = true;
	}
	if (NeedClose)
	{
		ImGui::CloseCurrentPopup();
		xr_delete(Form);
	}
}
