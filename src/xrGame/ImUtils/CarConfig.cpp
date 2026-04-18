#include "StdAfx.h"
#include "CarConfig.h"

#include "../../Include/xrRender/RenderVisual.h"
#include "../../Include/xrRender/Kinematics.h"

CarConfig g_CarConfig;

// ------------------------------------------------------------
// Helpers
// ------------------------------------------------------------
static xr_vector<shared_str> ReadList(CInifile* ini, const char* section, const char* name)
{
	xr_vector<shared_str> v;

	if (!ini->line_exist(section, name))
		return v;

	const char* S = ini->r_string(section, name);
	string256 token;
	xr_strcpy(token, S);

	// Windows-safe strtok_s
	LPSTR context = nullptr;
	LPSTR item = strtok_s(token, ", \t", &context);

	while (item)
	{
		v.emplace_back(item);
		item = strtok_s(nullptr, ", \t", &context);
	}

	return v;
}


static void ReadFvector(CInifile* ini, const char* sec, const char* key, Fvector& out)
{
	if (ini->line_exist(sec, key))
		out = ini->r_fvector3(sec, key);
}

// ------------------------------------------------------------
// Load config
// ------------------------------------------------------------
void CarConfig::Load(const shared_str& Section)
{
	Name = Section;

	shared_str VisualName = pSettings->r_string(Section, "visual");

	// Грузим визуал чтобы достать user data
	auto Visual = ::Render->model_Create(VisualName.c_str());
	auto& ini = *Visual->dcast_PKinematics()->LL_UserData();
	const char* section = "car_definition";

	// General
	Mass = ini.line_exist(section, "ph_mass") ? ini.r_float(section, "ph_mass") : 0;

	// Geometry
	ReadFvector(&ini, section, "size", Size);
	ReadFvector(&ini, section, "center_of_mass", CenterOfMass);

	// Physics
	EnginePower = ini.r_float(section, "engine_power");
	FuelTank = ini.r_float(section, "fuel_tank");
	FuelConsumption = ini.r_float(section, "fuel_consumption");

	// Cameras
	ReadFvector(&ini, section, "camera_pos", CamPos);
	ReadFvector(&ini, section, "camera_pos_firsteye", CamPosFirst);
	ReadFvector(&ini, section, "camera_pos_lookat", CamPosLookAt);
	ReadFvector(&ini, section, "camera_pos_free", CamPosFree);

	// Exits
	ReadFvector(&ini, section, "exit_position", ExitPos);
	ReadFvector(&ini, section, "exit_position_driver", ExitPosDriver);

	// Lists
	DrivingWheels = ReadList(&ini, section, "driving_wheels");
	SteeringWheels = ReadList(&ini, section, "steering_wheels");
	BreakingWheels = ReadList(&ini, section, "breaking_wheels");
	Doors = ReadList(&ini, section, "doors");

	// Single bones
	SteerBone = ini.r_string_wb(section, "steer");
	DriverPlace = ini.r_string_wb(section, "driver_place");
	ExhaustBone = ini.r_string_wb(section, "exhausts");
	TrunkBone = ini.r_string_wb(section, "trunk_bone");

	// Engine params
	PowerIncFactor = ini.r_float(section, "power_increment_factor");
	PowerDecFactor = ini.r_float(section, "power_decrement_factor");
	RpmIncFactor = ini.r_float(section, "rpm_increment_factor");
	RpmDecFactor = ini.r_float(section, "rpm_decrement_factor");
	NeutralPowerFactor = ini.r_float(section, "m_power_neutral_factor");

	MaxEngineRpm = ini.r_float(section, "max_engine_rpm");
	MaxPowerRpm = ini.r_float(section, "max_power_rpm");
	MaxTorqueRpm = ini.r_float(section, "max_torque_rpm");
	IdlingRpm = ini.r_float(section, "idling_engine_rpm");
	LimiterRpm = ini.line_exist(section, "limiter_engine_rpm") ? ini.r_float(section, "limiter_engine_rpm") : 0.f;

	AxleFriction = ini.r_float(section, "axle_friction");
	SteeringSpeed = ini.r_float(section, "steering_speed");
	SteeringTorque = ini.r_float(section, "steering_torque");
	BrakeTorque = ini.r_float(section, "break_torque");
	BrakeTime = ini.r_float(section, "break_time");
	HandBrakeTorque = ini.r_float(section, "hand_break_torque");

	MainGearRatio = ini.r_float(section, "main_gear_ratio");
	AutoTransmission = ini.r_bool(section, "auto_transmission");

	// Transmission
	GearRatios.clear();
	if (ini.section_exist("transmission_gear_ratio"))
	{
		const auto& S = ini.r_section("transmission_gear_ratio");
		for (const auto& it : S.Data)
		{
			GearRatios[it.first] = atof(*it.second);
		}
	}

	// Sounds
	EngineSound = ini.r_string_wb("car_sound", "snd_name");
	StartSound = ini.r_string_wb("car_sound", "engine_start");
	StopSound = ini.r_string_wb("car_sound", "engine_stop");
	TansmissionSwitchSound = ini.r_string_wb("car_sound", "transmission_switch");
	ExplosionSound = ini.r_string_wb("car_sound", "explosion_sound");

	// Lights
	Lights.clear();
	if (ini.section_exist("lights"))
	{
		auto& S = ini.r_section("lights");
		for (const auto& L : S.Data)
		{
			LightDef R;
			R.Bone = L.first;
			// Format: color,r,g,b,range,cone,spot,glow,glow_radius,is_point
			sscanf(*L.second, "%f,%f,%f,%f,%f,%f",
				&R.Color.r, &R.Color.g, &R.Color.b, &R.Color.a,
				&R.Range, &R.ConeAngle);

			Lights[L.first] = R;
		}
	}
}

// ------------------------------------------------------------
// Save back to LTX
// ------------------------------------------------------------
void CarConfig::Save(const shared_str& Section)
{
	auto& ini = *pSettings;
	const char* section = *Section;

	ini.w_float(section, "ph_mass", Mass);

	ini.w_fvector3(section, "size", Size);
	ini.w_fvector3(section, "center_of_mass", CenterOfMass);

	ini.w_float(section, "engine_power", EnginePower);
	ini.w_float(section, "fuel_tank", FuelTank);
	ini.w_float(section, "fuel_consumption", FuelConsumption);

	ini.w_fvector3(section, "camera_pos", CamPos);
	ini.w_fvector3(section, "camera_pos_firsteye", CamPosFirst);
	ini.w_fvector3(section, "camera_pos_lookat", CamPosLookAt);
	ini.w_fvector3(section, "camera_pos_free", CamPosFree);

	ini.w_fvector3(section, "exit_position", ExitPos);
	ini.w_fvector3(section, "exit_position_driver", ExitPosDriver);

	auto WriteList = [&](const char* key, const xr_vector<shared_str>& v)
	{
		string1024 out{};
		for (size_t i = 0; i < v.size(); i++)
		{
			xr_strcat(out, v[i].c_str());
			if (i + 1 < v.size()) xr_strcat(out, ",");
		}
		ini.w_string(section, key, out);
	};

	WriteList("driving_wheels", DrivingWheels);
	WriteList("steering_wheels", SteeringWheels);
	WriteList("breaking_wheels", BreakingWheels);
	WriteList("doors", Doors);

	ini.w_string(section, "steer", SteerBone.c_str());
	ini.w_string(section, "driver_place", DriverPlace.c_str());
	ini.w_string(section, "exhausts", ExhaustBone.c_str());
	ini.w_string(section, "trunk_bone", TrunkBone.c_str());

	ini.w_float(section, "power_increment_factor", PowerIncFactor);
	ini.w_float(section, "power_decrement_factor", PowerDecFactor);
	ini.w_float(section, "rpm_increment_factor", RpmIncFactor);
	ini.w_float(section, "rpm_decrement_factor", RpmDecFactor);
	ini.w_float(section, "m_power_neutral_factor", NeutralPowerFactor);

	ini.w_float(section, "max_engine_rpm", MaxEngineRpm);
	ini.w_float(section, "max_power_rpm", MaxPowerRpm);
	ini.w_float(section, "max_torque_rpm", MaxTorqueRpm);
	ini.w_float(section, "idling_engine_rpm", IdlingRpm);
	ini.w_float(section, "limiter_engine_rpm", LimiterRpm);

	ini.w_float(section, "axle_friction", AxleFriction);
	ini.w_float(section, "steering_speed", SteeringSpeed);
	ini.w_float(section, "steering_torque", SteeringTorque);
	ini.w_float(section, "break_torque", BrakeTorque);
	ini.w_float(section, "break_time", BrakeTime);
	ini.w_float(section, "hand_break_torque", HandBrakeTorque);

	ini.w_float(section, "main_gear_ratio", MainGearRatio);
	ini.w_bool(section, "auto_transmission", AutoTransmission);

	// Transmission
	for (auto& G : GearRatios)
	{
		ini.w_float("transmission_gear_ratio", G.first.c_str(), G.second);
	}

	// Sounds
	ini.w_string("car_sound", "snd_name", EngineSound.c_str());
	ini.w_string("car_sound", "engine_start", StartSound.c_str());
	ini.w_string("car_sound", "engine_stop", StopSound.c_str());
	ini.w_string("car_sound", "transmission_switch", TansmissionSwitchSound.c_str());
	ini.w_string("car_sound", "explosion_sound", ExplosionSound.c_str());

	// Lights
	for (auto& L : Lights)
	{
		string1024 s;
		sprintf(s, "%f,%f,%f,%f,%f,%f",
			L.second.Color.r, L.second.Color.g, L.second.Color.b, L.second.Color.a,
			L.second.Range, L.second.ConeAngle);

		ini.w_string("lights", L.first.c_str(), s);
	}
}
