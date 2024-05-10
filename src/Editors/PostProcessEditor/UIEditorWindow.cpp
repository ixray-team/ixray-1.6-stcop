#include "stdafx.h"
#include "UIEditorWindow.h"
#include "../EditorProps/ImGuiFileDialog.h"

static int BackWnd = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoNav | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoSavedSettings;

void CMainPPE::AddKey(float Value, bool OnlyValue)
{
	bool Found = false;

	for (const auto& Item : ListData)
	{
		if (Item.Value == Value)
		{
			Found = true;
			break;
		}
	}

	if (!Found)
	{
		PointItem Data;
		Data.Name = xr_string::ToString(Value);
		Data.Name = Data.Name.substr(0, Data.Name.find('.') + 3);
		Data.Value = Value;
		Data.IsActive = ListData.empty();

		ListData.push_back(std::move(Data));
	}

	if (OnlyValue)
		return;

	if (ModeIter < 3)
	{
		CPostProcessParam* ParamColor = mAnimator.GetParam((pp_params)ModeIter);
		ParamColor->add_value(Value, 0, 0);
		ParamColor->add_value(Value, 0, 1);
		ParamColor->add_value(Value, 0, 2);

		if (ModeIter == 2)
		{
			mAnimator.GetParam(pp_params::pp_gray_value)->add_value(Value, Itensity, 0);
		}
	}
	else if (ModeIter == 3)
	{
		mAnimator.GetParam(pp_params::pp_dual_h)->add_value(Value, 0, 0);
		mAnimator.GetParam(pp_params::pp_dual_v)->add_value(Value, 0, 0);
	}
	else if (ModeIter == 4)
	{
		mAnimator.GetParam(pp_params::pp_noise_i)->add_value(Value, 0, 0);
		mAnimator.GetParam(pp_params::pp_noise_g)->add_value(Value, 0, 0);
		mAnimator.GetParam(pp_params::pp_noise_f)->add_value(Value, 0, 0);
	}
	else if (ModeIter == 5)
	{
		CPostProcessParam* ParamColor = mAnimator.GetParam(pp_params::pp_blur);
		ParamColor->add_value(Value, 0, 0);
		ParamColor->add_value(Value, 0, 1);
		ParamColor->add_value(Value, 0, 2);
		ParamColor->add_value(Value, 0, 3);
	}
	else if (ModeIter == 6)
	{
		mAnimator.GetParam(pp_params::pp_cm_influence)->add_value(Value, 0, 0);
	}
}

CPostProcessParam* CMainPPE::GetCurrentParam()
{
	switch (ModeIter)
	{
		case 0: return mAnimator.GetParam(pp_params::pp_base_color);
		case 1: return mAnimator.GetParam(pp_params::pp_add_color);
		case 2: return mAnimator.GetParam(pp_params::pp_gray_color);
		case 3: return mAnimator.GetParam(pp_params::pp_dual_h);
		case 4: return mAnimator.GetParam(pp_params::pp_noise_i);
		case 5: return mAnimator.GetParam(pp_params::pp_blur);
		case 6: return mAnimator.GetParam(pp_params::pp_cm_influence);
	}

	return nullptr;
}

size_t CMainPPE::GetSelectedItemID() const
{
	size_t Iter = 0;

	for (const PointItem& SecondItem : ListData)
	{
		if (SecondItem.IsActive)
		{
			return Iter;
		}

		Iter++;
	}

	if (ListData.size() >= Iter)
		Iter = 0;

	return Iter;
}

void CMainPPE::Apply()
{
	if (ImGuiFileDialog::Instance()->Display("ChooseFileDlgKey"))
	{
		if (ImGuiFileDialog::Instance()->IsOk())
		{
			// action if OK
			std::string filePathName = ImGuiFileDialog::Instance()->GetFilePathName();
			std::string filePath = ImGuiFileDialog::Instance()->GetCurrentPath();
			FileName = ImGuiFileDialog::Instance()->GetCurrentFileName().c_str();
			// action

			ImGuiFileDialog::Instance()->Close();

			if (DrawDialogType == DialogType::Save)
			{
				mAnimator.Save(filePathName.c_str());
			}
			else if (DrawDialogType == DialogType::Load)
			{
				if (!FS.exist(filePathName.c_str()))
				{
					string_path Root;
					FS.update_path(Root, "$fs_root$", "");
					filePathName = filePathName.substr(strlen(Root));
				}

				mAnimator.Load(filePathName.c_str());
				LoadData();
			}

			DrawDialogType = DialogType::None;
		}
		else
		{
			ImGuiFileDialog::Instance()->Close();
		}
	}

	// Update props 
	ApplyData();
}

void CMainPPE::ClickHandle()
{
	if (LoadClick)
	{
		IGFD::FileDialogConfig config;
		string_path AnimDir = {};
		FS.update_path(AnimDir, "$game_anims$", "");

		config.path = AnimDir;
		ImGuiFileDialog::Instance()->OpenDialog("ChooseFileDlgKey", "Choose File", ".ppe", config);
		LoadClick = false;

		DrawDialogType = DialogType::Load;
	}
	else if (SaveClick)
	{
		IGFD::FileDialogConfig config;
		string_path AnimDir = {};
		FS.update_path(AnimDir, "$game_anims$", "");

		config.path = AnimDir;
		ImGuiFileDialog::Instance()->OpenDialog("ChooseFileDlgKey", "Choose File", ".ppe", config);
		SaveClick = false;

		DrawDialogType = DialogType::Save;
	}
	else if (NewClick)
	{
		ListData.clear();
		mAnimator.Clear();
		mAnimator.Create();
		NewClick = false;
	}

}

void CMainPPE::ApplyData()
{
	if (ListData.empty())
		return;

	float CurrentTime = ListData[GetSelectedItemID()].Value;

	ImVec4 ConvColor = Color;

	ConvColor.x = ((ConvColor.x * 255) -127.5f) / 127.5f;
	ConvColor.y = ((ConvColor.y * 255) -127.5f) / 127.5f;
	ConvColor.z = ((ConvColor.z * 255) -127.5f) / 127.5f;

	if (ModeIter < 3)
	{
		auto* ParamColor = mAnimator.GetParam((pp_params)ModeIter);

		ParamColor->update_value(CurrentTime, ConvColor.x, 0);
		ParamColor->update_value(CurrentTime, ConvColor.y, 1);
		ParamColor->update_value(CurrentTime, ConvColor.z, 2);

		if (ModeIter == 2)
		{
			mAnimator.GetParam(pp_params::pp_gray_value)->update_value(CurrentTime, Itensity, 0);
		}
	}
	else if (ModeIter == 3)
	{
		mAnimator.GetParam(pp_params::pp_dual_h)->update_value(CurrentTime, Duality.x);
		mAnimator.GetParam(pp_params::pp_dual_v)->update_value(CurrentTime, Duality.y);
	}
	else if (ModeIter == 4)
	{
		mAnimator.GetParam(pp_params::pp_noise_i)->update_value(CurrentTime, Noise.x);
		mAnimator.GetParam(pp_params::pp_noise_g)->update_value(CurrentTime, Noise.y);
		mAnimator.GetParam(pp_params::pp_noise_f)->update_value(CurrentTime, Noise.z);
	}
	else if (ModeIter == 5)
	{
		auto* ParamColor = mAnimator.GetParam(pp_params::pp_blur);
		ParamColor->update_value(CurrentTime, ConvColor.x, 0);
		ParamColor->update_value(CurrentTime, ConvColor.y, 1);
		ParamColor->update_value(CurrentTime, ConvColor.z, 2);
		ParamColor->update_value(CurrentTime, Itensity, 3);
	}
	else if (ModeIter == 6)
	{
		mAnimator.PPinfo().cm_tex1 = Texture;
		mAnimator.PPinfo().cm_influence = Influence;
	}
}

void CMainPPE::LoadData()
{
	ListData.clear();

	auto PushKeys = [&](CPostProcessParam* Params)
	{
		for (size_t Iter = 0; Iter < Params->get_keys_count(); Iter++)
		{
			AddKey(Params->get_key_time(Iter), true);
		}
	};

	if (ModeIter < 3)
	{
		CPostProcessParam* ParamColor = mAnimator.GetParam((pp_params)ModeIter);
		PushKeys(ParamColor);
	}
	else if (ModeIter == 3)
	{
		CPostProcessParam* ParamColor = mAnimator.GetParam(pp_params::pp_dual_h);
		PushKeys(ParamColor);
	}
	else if (ModeIter == 4)
	{
		CPostProcessParam* ParamColor = mAnimator.GetParam(pp_params::pp_noise_i);
		PushKeys(ParamColor);
	}
	else if (ModeIter == 5)
	{
		CPostProcessParam* ParamColor = mAnimator.GetParam(pp_params::pp_blur);
		PushKeys(ParamColor);
	}
	else if (ModeIter == 6)
	{
		CPostProcessParam* ParamColor = mAnimator.GetParam(pp_params::pp_cm_influence);
		PushKeys(ParamColor);
	}

	// Setup values
	UpdateData();
}

void CMainPPE::UpdateData()
{
	if (ListData.empty())
		return;

	float CurrentTime = ListData[GetSelectedItemID()].Value;

	if (ModeIter < 3)
	{
		CPostProcessColor* ParamColor = (CPostProcessColor*)mAnimator.GetParam((pp_params)ModeIter);

		ParamColor->get_value(CurrentTime, Color.x, 0);
		ParamColor->get_value(CurrentTime, Color.y, 1);
		ParamColor->get_value(CurrentTime, Color.z, 2);

		if (ModeIter == 2)
		{
			mAnimator.GetParam(pp_params::pp_gray_value)->get_value(CurrentTime, Itensity, 0);
		}

		Color.x = Color.x * 127.5f + 127.5f;
		Color.y = Color.y * 127.5f + 127.5f;
		Color.z = Color.z * 127.5f + 127.5f;

		Color.x /= 255;
		Color.y /= 255;
		Color.z /= 255;
	}
	else if (ModeIter == 3)
	{
		mAnimator.GetParam(pp_params::pp_dual_h)->get_value(CurrentTime, Duality.x);
		mAnimator.GetParam(pp_params::pp_dual_v)->get_value(CurrentTime, Duality.y);
	}
	else if (ModeIter == 4)
	{
		mAnimator.GetParam(pp_params::pp_noise_i)->get_value(CurrentTime, Noise.x);
		mAnimator.GetParam(pp_params::pp_noise_g)->get_value(CurrentTime, Noise.y);
		mAnimator.GetParam(pp_params::pp_noise_f)->get_value(CurrentTime, Noise.z);
	}
	else if (ModeIter == 5)
	{
		CPostProcessColor* ParamColor = (CPostProcessColor*)mAnimator.GetParam(pp_params::pp_blur);
		ParamColor->get_value(CurrentTime, Color.x, 0);
		ParamColor->get_value(CurrentTime, Color.y, 1);
		ParamColor->get_value(CurrentTime, Color.z, 2);
		ParamColor->get_value(CurrentTime, Itensity, 3);
	}
	else if (ModeIter == 6)
	{
		strcpy(Texture, mAnimator.PPinfo().cm_tex1.c_str());
		Influence = mAnimator.PPinfo().cm_influence;
	}
}

void CMainPPE::DrawChart()
{
	ImGui::SetNextWindowPos({ 0, HeaderSize });

	ImGui::Begin("PostProcessChart", 0, BackWnd | ImGuiWindowFlags_NoInputs);
	ImVec2 WndSize = { (float)EDevice.TargetWidth, (float)EDevice.TargetHeight / 2 };
	ImGui::SetWindowSize("PostProcessChart", WndSize);
	ImGui::GetWindowDrawList()->AddLine({ 0, HeaderSize + WndSize.y / 2 }, { WndSize.x, HeaderSize + WndSize.y / 2 }, ImColor(255, 255, 255));
	ImGui::GetWindowDrawList()->AddText({ 5, HeaderSize + WndSize.y / 2 - 15 }, ImColor(255, 255, 255), "127.5");
	ImGui::GetWindowDrawList()->AddText({ 5, HeaderSize }, ImColor(255, 255, 255), "255");
	ImGui::GetWindowDrawList()->AddText({ 5, HeaderSize + WndSize.y - 15 }, ImColor(255, 255, 255), "0");

	if (!ListData.empty())
	{
		if (ModeIter < 3 || ModeIter == 5)
		{
			float LineStepWidth = WndSize.x / (ListData.size() - 1) - 1;
			size_t Iter = 0;

			pp_params CurParam = (pp_params)ModeIter;
			if (ModeIter == 5)
				CurParam = pp_params::pp_blur;

			Fvector PrevPos = { 0, 0, 0 };

			for (auto& Item : ListData)
			{
				Fvector RGB;
				mAnimator.GetParam(CurParam)->get_value(Item.Value, RGB.x, 0);
				mAnimator.GetParam(CurParam)->get_value(Item.Value, RGB.y, 1);
				mAnimator.GetParam(CurParam)->get_value(Item.Value, RGB.z, 2);

				RGB.mul(127.5f);
				RGB.add(127.5f);

				float HeightOnePercent = WndSize.y / 100.f;

				Fvector Pos =
				{
					HeaderSize + std::min(WndSize.y - (HeightOnePercent * (RGB.x / 255.f * 100)), WndSize.y),
					HeaderSize + std::min(WndSize.y - (HeightOnePercent * (RGB.y / 255.f * 100)), WndSize.y),
					HeaderSize + std::min(WndSize.y - (HeightOnePercent * (RGB.z / 255.f * 100)), WndSize.y)
				};

				if (Iter != 0)
				{
					ImGui::GetWindowDrawList()->AddLine({ LineStepWidth * (Iter - 1), PrevPos.x }, { LineStepWidth * Iter, Pos.x }, ImColor(255, 0, 0), 2.f);
					ImGui::GetWindowDrawList()->AddLine({ LineStepWidth * (Iter - 1), PrevPos.y }, { LineStepWidth * Iter, Pos.y }, ImColor(0, 255, 0), 2.f);
					ImGui::GetWindowDrawList()->AddLine({ LineStepWidth * (Iter - 1), PrevPos.z }, { LineStepWidth * Iter, Pos.z }, ImColor(0, 0, 255), 2.f);
				}

				PrevPos.set(Pos);

				if (Item.IsActive)
				{
					ImGui::GetWindowDrawList()->AddLine({ LineStepWidth * Iter, HeaderSize }, { LineStepWidth * Iter, WndSize.y + HeaderSize }, ImColor(127, 127, 127));
					ImGui::GetWindowDrawList()->AddText({ (LineStepWidth * Iter) - 15 - (Item.Name.length() * 4), HeaderSize + WndSize.y - 15}, ImColor(255, 255, 255), Item.Name.c_str());
				}
				Iter++;
			}
		}
	}
	
	ImGui::End();
}

void CMainPPE::DrawTool()
{
	ImGui::SetNextWindowPos({ 0, (float)EDevice.TargetHeight / 2 + HeaderSize });
	ImGui::SetNextWindowSize({ (float)EDevice.TargetWidth, (float)EDevice.TargetHeight / 2 - HeaderSize });
	ImGui::Begin("PostProcessTools", 0, BackWnd | ImGuiWindowFlags_NoInputs);
	const char* AllModes[] = { "Base Color", "Add Color", "Gray Color", "Duality", "Noise", "Blur", "ColorMapper" };
	// ImGui::SetNextWindowSize({ 100, 30 });

	if (ImGui::Begin("ModeWnd", 0, BackWnd | ImGuiWindowFlags_NoBackground))
	{
		ImGui::SetWindowPos({ 5,  (float)EDevice.TargetHeight / 2 + 5 + HeaderSize });
		ImGui::SetWindowSize({ 300, 50 });
		if (ImGui::Combo("Mode", &ModeIter, AllModes, IM_ARRAYSIZE(AllModes)))
		{
			LoadData();
		}
		ImGui::End();
	}

	ImGui::SetNextWindowPos({ 5, (float)EDevice.TargetHeight / 2 + 30 + HeaderSize });
	ImGui::SetNextWindowSize({ 220, 500 });
	if (ImGui::Begin("HelperInput", 0, BackWnd | ImGuiWindowFlags_NoBackground))
	{
		if (ImGui::BeginListBox(" ", { 195, 200 }))
		{
			for (PointItem& Item : ListData)
			{
				if (ImGui::Selectable(Item.Name.c_str(), &Item.IsActive))
				{
					for (PointItem& SecondItem : ListData)
					{
						if (Item.Value != SecondItem.Value)
						{
							SecondItem.IsActive = false;
						}
					}
					UpdateData();
				}
			}
			ImGui::EndListBox();
		}

		static float InputItemName;

		ImGui::SetCursorPosX(8);
		float OldYPos = ImGui::GetCursorPosY();
		ImGui::SetNextWindowBgAlpha(0.f);
		if (ImGui::BeginChild("HelperInputFloat", { 125, 40 }))
		{
			ImGui::InputFloat("  ", &InputItemName, 0.01f, 0.5f, "%.2f");
		}
		ImGui::EndChild();

		ImGui::SetNextWindowBgAlpha(0.f);
		ImGui::SetCursorPos({ 92, OldYPos });
		if (ImGui::BeginChild("HelperPointButtons", { 130, 40 }))
		{
			if (ImGui::Button("Add", { 32, 19 }))
			{
				AddKey(InputItemName);
			}

			ImGui::SetCursorPos({ 35, 0 });
			if (ImGui::Button("Del", { 33, 19 }))
			{
				size_t Iter = 0;
				for (const PointItem& Item : ListData)
				{
					if (Item.IsActive)
						break;

					Iter++;
				}

				auto PP = GetCurrentParam();
				PP->delete_value(ListData[Iter].Value);

				if (Iter < ListData.size())
				{
					ListData.erase(ListData.begin() + Iter);
				}

			}

			ImGui::SetCursorPos({ 71, 0 });
			if (ImGui::Button("Clear", { 40, 19 }))
			{
				auto PP = GetCurrentParam();

				for (const PointItem& Item : ListData)
				{
					PP->delete_value(Item.Value);
				}
				ListData.clear();
			}
		}
		ImGui::EndChild();

		ImGui::End();
	}

	ImGui::SetNextWindowPos({ 210, (float)EDevice.TargetHeight / 2 + 32 + HeaderSize });
	ImGui::SetNextWindowSize({ 400, 300 });
	if (ImGui::Begin("EditHelper", 0, BackWnd | ImGuiWindowFlags_NoBackground))
	{
		if (ListData.empty())
		{
			float Temp = 0;
			ImGui::InputFloat("Key Time ", &Temp, 0.01f, 0.5f, "%.2f");
		}
		else
		{
			float& Value = ListData[GetSelectedItemID()].Value;
			ImGui::InputFloat("Key Time ", &Value, 0.01f, 0.5f, "%.2f");

			if (Value < 0)
				Value = 0;
		}

		if (ModeIter == 3)
		{
			ImGui::InputFloat("Duality [H]", &Duality.x, 0.01f, 0.5f);
			ImGui::InputFloat("Duality [V]", &Duality.y, 0.01f, 0.5f);
		}
		else if (ModeIter == 6)
		{
			ImGui::InputFloat("Influence", &Influence, 0.01f, 0.5f);
			ImGui::InputText("Texture", Texture, sizeof(Texture));
		}
		else if (ModeIter == 4)
		{
			ImGui::InputFloat("Noise [Intensity]", &Noise.x, 0.01f, 0.5f);
			ImGui::InputFloat("Noise [Grain]", &Noise.y, 0.01f, 0.5f);
			ImGui::InputFloat("Noise [FPS]", &Noise.z, 0.01f, 0.5f);
		}
		else
		{
			if (ModeIter == 2 || ModeIter == 5)
			{
				ImGui::InputFloat("Itensity", &Itensity, 0.01f, 0.5f, "%.2f");

				if (Itensity < 0)
					Itensity = 0;
			}

			DrawPicker();
		}
		ImGui::End();
	}

	ImGui::End();
}

void CMainPPE::DrawPicker()
{
	ImGui::ColorEdit4("Color", (float*)&Color);

	if (UsePicker && ImGui::Begin("EditHelper"))
	{
		static float Color[4] = { 1, 1, 1, 1 };
		ImGui::ColorPicker4("Color", Color);
		ImGui::End();
	}
}

void CMainPPE::DrawUI()
{
	ImGui::BeginMainMenuBar();
	ImGui::MenuItem("New", "",  &NewClick);
	ImGui::MenuItem("Load", "", &LoadClick);
	ImGui::MenuItem("Save", "", &SaveClick);
	bool Temp = false;
	ImGui::MenuItem(FileName.c_str(), "", Temp, false);

	HeaderSize = ImGui::GetWindowHeight();
	ImGui::EndMainMenuBar();

	DrawChart();
	DrawTool();

	ClickHandle();
	Apply();
}
