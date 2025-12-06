#include "StdAfx.h"
#include "CarConfig.h"

static xr_vector<shared_str> s_AvailableCars;
static int s_SelectedCar = 0;

// Collect sections
static void CollectCars()
{
	s_AvailableCars.clear();

	for (const auto& S : pSettings->sections())
	{
		if (pSettings->line_exist(S->Name, "class"))
		{
			shared_str Class = pSettings->r_string(S->Name, "class");
			if (Class == "C_NIVA")
			{
				s_AvailableCars.emplace_back(S->Name);
			}
		}
	}
}

void DrawVector3(const char* label, Fvector& v)
{
	ImGui::DragFloat3(label, (float*)&v, 0.01f);
}

void DrawString(const char* label, shared_str& s)
{
	char buf[256]{};
	xr_strcpy(buf, s.c_str());
	if (ImGui::InputText(label, buf, sizeof(buf)))
		s = buf;
}

void DrawList(const char* label, xr_vector<shared_str>& v)
{
	if (ImGui::TreeNode(label))
	{
		for (int i = 0; i < (int)v.size(); i++)
		{
			char buf[256]{};
			xr_strcpy(buf, v[i].c_str());
			ImGui::InputText(std::format("##{}", i).c_str(), buf, sizeof(buf));
			v[i] = buf;
		}

		if (ImGui::Button("Add"))
			v.emplace_back("new_item");

		ImGui::TreePop();
	}
}

void DrawGearMap(xr_map<shared_str, float>& m)
{
	if (!ImGui::TreeNode("Transmission gear ratios"))
		return;

	for (auto& it : m)
	{
		float v = it.second;
		if (ImGui::DragFloat(it.first.c_str(), &v, 0.01f))
			it.second = v;
	}

	ImGui::TreePop();
}


// ------------------------------------------------------------
// Main Editor
// ------------------------------------------------------------
void RenderCarConfigEditor()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_CarEditor)])
	{
		return;
	}

	if (!ImGui::Begin("Cars Editor", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_CarEditor)]))
	{
		ImGui::End();
	}

	if (s_AvailableCars.empty())
	{
		CollectCars();
	}

	ImGui::Text("Car Config Editor");
	ImGui::Separator();

	// Select car
	if (ImGui::BeginCombo("Car section", s_AvailableCars[s_SelectedCar].c_str()))
	{
		for (int i = 0; i < (int)s_AvailableCars.size(); i++)
		{
			bool selected = (i == s_SelectedCar);
			if (ImGui::Selectable(s_AvailableCars[i].c_str(), selected))
			{
				s_SelectedCar = i;
				g_CarConfig.Load(s_AvailableCars[i]);
			}
			if (selected) ImGui::SetItemDefaultFocus();
		}
		ImGui::EndCombo();
	}

	ImGui::SameLine();
	if (ImGui::Button("Reload"))
		g_CarConfig.Load(g_CarConfig.Name);

	ImGui::SameLine();
	if (ImGui::Button("Save"))
		g_CarConfig.Save(g_CarConfig.Name);

	// ----------- SECTIONS (яоникепш) ------------------------------------

	if (ImGui::CollapsingHeader("Geometry"))
	{
		DrawVector3("Size", g_CarConfig.Size);
		DrawVector3("Center of mass", g_CarConfig.CenterOfMass);
	}

	if (ImGui::CollapsingHeader("Physics"))
	{
		ImGui::DragFloat("Mass", &g_CarConfig.Mass);
		ImGui::DragFloat("Engine Power", &g_CarConfig.EnginePower);
		ImGui::DragFloat("Fuel Tank", &g_CarConfig.FuelTank);
		ImGui::DragFloat("Fuel Consumption", &g_CarConfig.FuelConsumption);
	}

	if (ImGui::CollapsingHeader("Camera"))
	{
		DrawVector3("Cam Pos", g_CarConfig.CamPos);
		DrawVector3("Cam First eye", g_CarConfig.CamPosFirst);
		DrawVector3("Cam LookAt", g_CarConfig.CamPosLookAt);
		DrawVector3("Cam Free", g_CarConfig.CamPosFree);
	}

	if (ImGui::CollapsingHeader("Exit Points"))
	{
		DrawVector3("Exit Pos", g_CarConfig.ExitPos);
		DrawVector3("Exit Pos Driver", g_CarConfig.ExitPosDriver);
	}

	if (ImGui::CollapsingHeader("Bones"))
	{
		DrawList("Driving wheels", g_CarConfig.DrivingWheels);
		DrawList("Steering wheels", g_CarConfig.SteeringWheels);
		DrawList("Breaking wheels", g_CarConfig.BreakingWheels);
		DrawList("Doors", g_CarConfig.Doors);

		DrawString("Steer Bone", g_CarConfig.SteerBone);
		DrawString("Driver Place", g_CarConfig.DriverPlace);
		DrawString("Exhaust Bone", g_CarConfig.ExhaustBone);
		DrawString("Trunk Bone", g_CarConfig.TrunkBone);
	}

	if (ImGui::CollapsingHeader("Engine Params"))
	{
		ImGui::DragFloat("Power Inc", &g_CarConfig.PowerIncFactor);
		ImGui::DragFloat("Power Dec", &g_CarConfig.PowerDecFactor);
		ImGui::DragFloat("RPM Inc", &g_CarConfig.RpmIncFactor);
		ImGui::DragFloat("RPM Dec", &g_CarConfig.RpmDecFactor);
		ImGui::DragFloat("Neutral Power Factor", &g_CarConfig.NeutralPowerFactor);

		ImGui::DragFloat("Max Engine RPM", &g_CarConfig.MaxEngineRpm);
		ImGui::DragFloat("Max Power RPM", &g_CarConfig.MaxPowerRpm);
		ImGui::DragFloat("Max Torque RPM", &g_CarConfig.MaxTorqueRpm);
		ImGui::DragFloat("Idling RPM", &g_CarConfig.IdlingRpm);
		ImGui::DragFloat("Limiter RPM", &g_CarConfig.LimiterRpm);

		ImGui::Separator();

		ImGui::DragFloat("Axle Friction", &g_CarConfig.AxleFriction);
		ImGui::DragFloat("Steering Speed", &g_CarConfig.SteeringSpeed);
		ImGui::DragFloat("Steering Torque", &g_CarConfig.SteeringTorque);
		ImGui::DragFloat("Brake Torque", &g_CarConfig.BrakeTorque);
		ImGui::DragFloat("Brake Time", &g_CarConfig.BrakeTime);
		ImGui::DragFloat("Hand Brake Torque", &g_CarConfig.HandBrakeTorque);

		ImGui::DragFloat("Main Gear Ratio", &g_CarConfig.MainGearRatio);
		ImGui::Checkbox("Auto Transmission", &g_CarConfig.AutoTransmission);

		DrawGearMap(g_CarConfig.GearRatios);
	}

	if (ImGui::CollapsingHeader("Sounds"))
	{
		DrawString("Engine", g_CarConfig.EngineSound);
	}
	ImGui::End();
}
