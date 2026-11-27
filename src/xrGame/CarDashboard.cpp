#include "StdAfx.h"
#include "CarDashboard.h"
#include "Level.h"
#include "../Include/xrRender/Kinematics.h"

struct SGaugeKey
{
	const char* Key;
	u8 Type;
};
void CCarDashboard::BindBone(IKinematics& Kinematics, CInifile& Ini, const char* Section, u16 BoneId)
{
	static constexpr SGaugeKey Keys[] =
		{
			{"speedometer", u8(EGauge::Speedometer)},
			{"tachometer", u8(EGauge::Tachometer)},
			{"fuel", u8(EGauge::Fuel)},
			{"temp", u8(EGauge::Temperature)},
			{"gauge", u8(EGauge::Generic)},
			{"fan", u8(EGauge::Fan)},
			{"pressure", u8(EGauge::Pressure)},
			{"battery", u8(EGauge::Battery)},
			{"brakes_pressure", u8(EGauge::BrakesPressure)},
			{"brakes_pressure_apply", u8(EGauge::BrakesPressureApply)},
			{"blower", u8(EGauge::Blower)},
			{"engine", u8(EGauge::Engine)},
			{"gear", u8(EGauge::Gear)},
			{"clock_seconds", u8(EGauge::ClockSeconds)},
			{"clock_minutes", u8(EGauge::ClockMinutes)},
			{"clock_hours", u8(EGauge::ClockHours)},
		};

	for (const SGaugeKey& Key : Keys)
	{
		if (!Ini.line_exist(Section, Key.Key))
		{
			continue;
		}

		const EGauge Type = EGauge(Key.Type);
		Bindings.push_back(xr_make_unique<SBinding>(SBinding{this, Type}));
		Bones.push_back(BoneId);
		Kinematics.LL_GetBoneInstance(BoneId).set_callback(bctCustom, GaugeCallback, Bindings.back().get());
		ReadConfig(Type, Ini, Section);
	}
}

void CCarDashboard::Unbind(IKinematics* Kinematics)
{
	if (Kinematics != nullptr)
	{
		for (u16 BoneId : Bones)
		{
			if (BoneId != BI_NONE)
			{
				Kinematics->LL_GetBoneInstance(BoneId).reset_callback();
			}
		}
	}

	Bindings.clear();
	Bones.clear();
}

void CCarDashboard::ReadConfig(EGauge Type, CInifile& Ini, const char* Section)
{
	const auto Read = [&Ini, Section](const char* Key, auto& Value)
	{
		Value = Ini.read_if_exists<std::remove_reference_t<decltype(Value)>>(Section, Key, Value);
	};

	switch (Type)
	{
		case EGauge::Speedometer:
			Read("speedometer_factor", Speedometer.Factor);
			Read("speedometer_max_speed", Speedometer.MaxSpeed);
			Read("speedometer_min_speed", Speedometer.MinSpeed);
			Read("speedometer_offset", Speedometer.Offset);
			break;
		case EGauge::Tachometer:
			Read("tachometer_factor", Tachometer.Factor);
			Read("tachometer_min_rpm", Tachometer.MinRpm);
			break;
		case EGauge::Fuel:
			Read("fuel_angle", Fuel.Angle);
			Read("fuel_offset", Fuel.Offset);
			break;
		case EGauge::Temperature:
			Read("temp_angle", Temperature.Angle);
			Read("temp_offset", Temperature.Offset);
			break;
		case EGauge::Generic:
			Read("gauge_angle", Generic.Angle);
			break;
		case EGauge::Fan:
			Read("fan_speed", Fan.Speed);
			Read("fan_engine", Fan.FollowEngine);
			break;
		case EGauge::Pressure:
			Read("pressure_max_angle", Pressure.MaxAngle);
			Read("pressure_idle_angle", Pressure.IdleAngle);
			Read("pressure_factor", Pressure.Factor);
			Read("pressure_offset", Pressure.Offset);
			break;
		case EGauge::Battery:
			Read("battery_factor", Battery.Factor);
			Read("battery_angle", Battery.Angle);
			break;
		case EGauge::BrakesPressure:
			Read("brakes_pressure_factor", Brakes.PressureFactor);
			Read("brakes_pressure_max_angle", Brakes.PressureMaxAngle);
			break;
		case EGauge::BrakesPressureApply:
			Read("brakes_pressure_apply_angle", Brakes.ApplyAngle);
			break;
		case EGauge::Blower:
			Read("blower_angle", Blower.Angle);
			break;
		case EGauge::Engine:
			Read("engine_angle", Engine.Angle);
			Read("engine_speed", Engine.Speed);
			break;
		case EGauge::Gear:
			Read("gear_manual", GearLever.Manual);
			Read("gear_speed", GearLever.Speed);
			Read("gear_delay", GearLever.Delay);
			clamp(GearLever.Delay, 0.f, 2.f);
			Read("gear_park_angle_x", GearLever.ParkAngleX);
			Read("gear_drive_angle_x", GearLever.DriveAngleX);
			Read("gear_reverse_angle_x", GearLever.ReverseAngleX);
			Read("gear_left_angle", GearLever.LeftAngle);
			Read("gear_middle_angle", GearLever.MiddleAngle);
			Read("gear_right_angle", GearLever.RightAngle);
			break;
		case EGauge::ClockSeconds:
			Read("clock_seconds_offset", Clock.SecondsOffset);
			Read("clock_seconds_real", Clock.RealSeconds);
			break;
		case EGauge::ClockMinutes:
			Read("clock_minutes_offset", Clock.MinutesOffset);
			break;
		case EGauge::ClockHours:
			Read("clock_hours_offset", Clock.HoursOffset);
			break;
	}
}

void __stdcall CCarDashboard::GaugeCallback(CBoneInstance* Bone)
{
	const SBinding* Binding = static_cast<const SBinding*>(Bone->callback_param());
	if (Binding == nullptr || Binding->Owner == nullptr)
	{
		return;
	}

	Fmatrix Result;
	if (Binding->Owner->Evaluate(Binding->Type, *Bone, Result))
	{
		Bone->mTransform.mulB_43(Result);
	}
}

bool CCarDashboard::Evaluate(EGauge Type, CBoneInstance& Bone, Fmatrix& Result)
{
	switch (Type)
	{
		case EGauge::Speedometer: return UpdateSpeedometer(Result);
		case EGauge::Tachometer: return UpdateTachometer(Result);
		case EGauge::Fuel: return UpdateFuel(Result);
		case EGauge::Temperature: return UpdateTemperature(Result);
		case EGauge::Battery: return UpdateBattery(Result);
		case EGauge::Generic: return UpdateGeneric(Result);
		case EGauge::Fan: return UpdateFan(Result);
		case EGauge::Blower: return UpdateBlower(Result);
		case EGauge::Pressure: return UpdatePressure(Result);
		case EGauge::BrakesPressure: return UpdateBrakesPressure(Result);
		case EGauge::BrakesPressureApply: return UpdateBrakesPressureApply(Result);
		case EGauge::Engine: return UpdateEngine(Result);
		case EGauge::Gear: return UpdateGear(Bone, Result);
		case EGauge::ClockSeconds: return UpdateClockSeconds(Result);
		case EGauge::ClockMinutes: return UpdateClockMinutes(Result);
		case EGauge::ClockHours: return UpdateClockHours(Result);
	}

	return false;
}

bool CCarDashboard::UpdateSpeedometer(Fmatrix& Result)
{
	if (!Telemetry.HasDriveWheels)
	{
		return false;
	}

	const float Factor = fabsf(Speedometer.Factor);
	float Speed = -Speedometer.MinSpeed * Factor + Telemetry.DriveWheelsSpeed * 3.6f * Factor;
	clamp(Speed, 0.f, Speedometer.MaxSpeed * Factor);

	Speedometer.Current = (1.f - 6.f * Device.fTimeDelta) * Speedometer.Current + 6.f * Device.fTimeDelta * Speed;

	if (Speedometer.Factor < 0.f)
	{
		Result.rotateZ(deg2rad(Speedometer.Offset) + Speedometer.Current / 30.f);
	}
	else
	{
		Result.rotateZ(-deg2rad(Speedometer.Offset) - Speedometer.Current / 30.f);
	}

	return true;
}

bool CCarDashboard::UpdateTachometer(Fmatrix& Result)
{
	float CurrentRpm = Telemetry.Rpm - (Tachometer.MinRpm * 0.105f);
	clamp(CurrentRpm, 0.f, CurrentRpm);
	float Rpm = (CurrentRpm / Telemetry.MaxRpm) * 3.f * fabsf(Tachometer.Factor);
	if (!Telemetry.EngineOn)
	{
		Rpm = 0.f;
	}

	Tachometer.Current = (1.f - 15.f * Device.fTimeDelta) * Tachometer.Current + 15.f * Device.fTimeDelta * Rpm;

	if (Tachometer.Factor < 0.f)
	{
		Result.rotateZ(Tachometer.Current);
	}
	else
	{
		Result.rotateZ(-Tachometer.Current);
	}

	return true;
}

bool CCarDashboard::UpdateFuel(Fmatrix& Result)
{
	float Target = Telemetry.FuelRatio + deg2rad(Fuel.Offset);
	if (!Telemetry.EngineOn)
	{
		Target = 0.f;
	}

	if (Fuel.Current < Target)
	{
		Fuel.Current = (1.f - 0.9f * Device.fTimeDelta) * Fuel.Current + 0.9f * Device.fTimeDelta * Target;
	}
	else
	{
		Fuel.Current = (1.f - 3.f * Device.fTimeDelta) * Fuel.Current + 3.f * Device.fTimeDelta * Target;
	}

	Result.rotateZ(-Fuel.Current * deg2rad(Fuel.Angle));

	return true;
}

bool CCarDashboard::UpdateTemperature(Fmatrix& Result)
{
	float Target = 0.f;
	float Speed = 1.f;
	if (Telemetry.EngineOn)
	{
		Target = 1.f;
		if (Telemetry.Drive)
		{
			Target = 1.2f;
			Speed = 1.2f;
		}
		float CarSpeed = Telemetry.SpeedKmh;
		clamp(CarSpeed, 0.f, 80.f);
		Target = Target - (CarSpeed / 260.f);

		Temperature.OffsetCurrent = (1.f - 0.9f * Device.fTimeDelta) * Temperature.OffsetCurrent + 0.9f * Device.fTimeDelta * deg2rad(Temperature.Offset);
		Temperature.Current = (1.f - 0.9f * Device.fTimeDelta) * Temperature.Current + 0.9f * Device.fTimeDelta;
	}
	else
	{
		Temperature.OffsetCurrent = (1.f - 3.f * Device.fTimeDelta) * Temperature.OffsetCurrent;
		Temperature.Current = (1.f - 3.f * Device.fTimeDelta) * Temperature.Current;
	}

	Temperature.Value = (1.f - 0.006f * Speed * Device.fTimeDelta) * Temperature.Value + 0.006f * Speed * Device.fTimeDelta * deg2rad(Temperature.Angle) * Target;

	Result.rotateZ(-Temperature.Value * Temperature.Current - Temperature.OffsetCurrent);

	return true;
}

bool CCarDashboard::UpdateBattery(Fmatrix& Result)
{
	const float Rpm = (1.f + (Telemetry.Rpm - Telemetry.MinRpm) / (Telemetry.MaxRpm - Telemetry.MinRpm) * Battery.Factor);
	float Angle = fabsf(Battery.Angle) * Rpm;
	if (!Telemetry.EngineOn)
	{
		Angle = 0.f;
	}

	if (Battery.Current < Angle)
	{
		Battery.Current = (1.f - 0.9f * Device.fTimeDelta) * Battery.Current + 0.9f * Device.fTimeDelta * deg2rad(Angle);
	}
	else
	{
		Battery.Current = (1.f - 3.f * Device.fTimeDelta) * Battery.Current + 3.f * Device.fTimeDelta * deg2rad(Angle);
	}

	if (Battery.Angle < 0.f)
	{
		Result.rotateZ(Battery.Current);
	}
	else
	{
		Result.rotateZ(-Battery.Current);
	}

	return true;
}

bool CCarDashboard::UpdateGeneric(Fmatrix& Result)
{
	float Angle = fabsf(Generic.Angle);
	if (!Telemetry.EngineOn)
	{
		Angle = 0.f;
	}

	if (Generic.Current < Angle)
	{
		Generic.Current = (1.f - 0.9f * Device.fTimeDelta) * Generic.Current + 0.9f * Device.fTimeDelta * deg2rad(Angle);
	}
	else
	{
		Generic.Current = (1.f - 3.f * Device.fTimeDelta) * Generic.Current + 3.f * Device.fTimeDelta * deg2rad(Angle);
	}

	if (Generic.Angle < 0.f)
	{
		Result.rotateZ(Generic.Current);
	}
	else
	{
		Result.rotateZ(-Generic.Current);
	}

	return true;
}

bool CCarDashboard::UpdateFan(Fmatrix& Result)
{
	float Rpm = Telemetry.Rpm;
	clamp(Rpm, Telemetry.MinRpm, Rpm);

	if (Telemetry.EngineOn)
	{
		Fan.SpeedCurrent += 0.6f * fabsf(Fan.Speed) * Device.fTimeDelta;
	}
	else
	{
		Fan.SpeedCurrent -= 0.6f * fabsf(Fan.Speed) * Device.fTimeDelta;
	}
	clamp(Fan.SpeedCurrent, 0.f, fabsf(Fan.Speed));

	if (!Fan.FollowEngine)
	{
		Fan.Rotation = Fan.Rotation + Fan.SpeedCurrent * 3.f * Device.fTimeDelta;
	}
	else
	{
		Fan.Rotation = Fan.Rotation + Fan.SpeedCurrent * 3.f * Device.fTimeDelta * (Rpm / Telemetry.MaxRpm);
	}

	if (Fan.Speed < 0.f)
	{
		Result.rotateZ(-Fan.Rotation);
	}
	else
	{
		Result.rotateZ(Fan.Rotation);
	}

	return true;
}

bool CCarDashboard::UpdateBlower(Fmatrix& Result)
{
	float Angle = fabsf(Blower.Angle);
	if (!Telemetry.Drive)
	{
		Angle = 0.f;
	}
	else if (Telemetry.TransmissionSwitching)
	{
		Angle = fabsf(Blower.Angle) * 0.3f;
	}

	float Speed = 0.3f;
	if (Telemetry.TransmissionSwitching)
	{
		Speed *= 0.5f;
	}

	if (Blower.Current < Angle)
	{
		Blower.Current = (1.f - Speed * Device.fTimeDelta * 30.f) * Blower.Current + Speed * Device.fTimeDelta * 30.f * deg2rad(Angle);
	}
	else
	{
		Blower.Current = (1.f - 1.5f * Speed * Device.fTimeDelta * 30.f) * Blower.Current + 1.5f * Speed * Device.fTimeDelta * 30.f * deg2rad(Angle);
	}

	if (Blower.Angle < 0.f)
	{
		Result.rotateX(Blower.Current);
	}
	else
	{
		Result.rotateX(-Blower.Current);
	}

	return true;
}

bool CCarDashboard::UpdatePressure(Fmatrix& Result)
{
	const float RpmRange = Telemetry.MaxRpm - Telemetry.MinRpm;
	float Rpm = Telemetry.Rpm - Telemetry.MinRpm;
	clamp(Rpm, 0.f, RpmRange);
	Rpm = (Rpm / RpmRange) * fabsf(Pressure.Factor) + deg2rad(Pressure.IdleAngle + Pressure.Offset);

	if (Telemetry.EngineOn)
	{
		Pressure.Current = (1.f - 0.9f * Device.fTimeDelta) * Pressure.Current + 0.9f * Device.fTimeDelta;
	}
	else
	{
		Pressure.Current = (1.f - 3.f * Device.fTimeDelta) * Pressure.Current;
	}

	if (Telemetry.Drive)
	{
		Rpm += 0.1f * fabsf(Pressure.Factor);
	}

	Pressure.Value = (1.f - 1.5f * Device.fTimeDelta) * Pressure.Value + 1.5f * Device.fTimeDelta * Rpm;
	clamp(Pressure.Value, Pressure.Value, deg2rad(Pressure.MaxAngle + Pressure.Offset));

	if (Pressure.Factor < 0.f)
	{
		Result.rotateZ(Pressure.Current * Pressure.Value);
	}
	else
	{
		Result.rotateZ(-Pressure.Current * Pressure.Value);
	}

	return true;
}

bool CCarDashboard::UpdateBrakesPressure(Fmatrix& Result)
{
	float Angle = fabsf(Brakes.PressureMaxAngle);
	if (!Telemetry.EngineOn)
	{
		Angle = 0.f;
	}

	if (Brakes.PressureCurrent < Angle)
	{
		Brakes.PressureCurrent = (1.f - 0.9f * Device.fTimeDelta) * Brakes.PressureCurrent + 0.9f * Device.fTimeDelta * deg2rad(Angle);
	}
	else
	{
		Brakes.PressureCurrent = (1.f - 3.f * Device.fTimeDelta) * Brakes.PressureCurrent + 3.f * Device.fTimeDelta * deg2rad(Angle);
	}

	if (Telemetry.EngineOn)
	{
		if (Telemetry.Brakes && Telemetry.BrakeElapsed < Telemetry.BrakeTime && Telemetry.BrakeTime > 0.f)
		{
			const float Rate = 0.3f * Device.fTimeDelta * (1.f / Telemetry.BrakeTime);
			Brakes.Pressure = (1.f - Rate) * Brakes.Pressure + Rate * Brakes.PressureFactor;
		}
		else
		{
			Brakes.Pressure = (1.f - 0.09f * Device.fTimeDelta) * Brakes.Pressure + (0.09f * Device.fTimeDelta);
		}
	}
	else
	{
		Brakes.Pressure = (1.f - 0.05f * Device.fTimeDelta) * Brakes.Pressure;
	}

	if (Brakes.PressureMaxAngle < 0.f)
	{
		Result.rotateZ(Brakes.PressureCurrent * Brakes.Pressure);
	}
	else
	{
		Result.rotateZ(-Brakes.PressureCurrent * Brakes.Pressure);
	}

	return true;
}

bool CCarDashboard::UpdateBrakesPressureApply(Fmatrix& Result)
{
	const float Angle = fabsf(Brakes.ApplyAngle);
	float Press = 1.f;
	if (!Telemetry.EngineOn || !Telemetry.Brakes)
	{
		Press = 0.f;
	}
	else if (Telemetry.BrakeTime > 0.f)
	{
		Press = Telemetry.BrakeElapsed / Telemetry.BrakeTime;
	}

	clamp(Press, 0.f, 1.f);
	Brakes.ApplyCurrent = (1.f - 3.f * Device.fTimeDelta) * Brakes.ApplyCurrent + 3.f * Device.fTimeDelta * Press;

	if (Brakes.ApplyAngle < 0.f)
	{
		Result.rotateZ(deg2rad(Angle) * Brakes.Pressure * Brakes.ApplyCurrent);
	}
	else
	{
		Result.rotateZ(-deg2rad(Angle) * Brakes.Pressure * Brakes.ApplyCurrent);
	}

	return true;
}

bool CCarDashboard::UpdateEngine(Fmatrix& Result)
{
	float Rpm = 0.f;
	float Twist = 0.f;
	float Gas = 1.25f;
	if (!Telemetry.Drive || Telemetry.TransmissionSwitching)
	{
		Gas = 1.f;
	}

	if (!Telemetry.EngineOn)
	{
		Engine.Current = 0.f;
	}
	else
	{
		Rpm = 1.2f * Gas * ((Telemetry.Rpm - Telemetry.MinRpm) / (Telemetry.MaxRpm - Telemetry.MinRpm));
		Twist = Rpm;
		clamp(Twist, 0.f, 1.f);
		clamp(Rpm, 0.f, 1.f);

		// The needle shakes between -1 and 1; the shake gets faster with rpm.
		const float Step = Engine.Speed * (10.f + 50.f * Rpm) * Gas * Device.fTimeDelta;
		if (!Engine.Stage)
		{
			Engine.Current += Step;
			if (Engine.Current >= 1.f)
			{
				Engine.Current = 1.f;
				Engine.Stage = true;
			}
		}
		else
		{
			Engine.Current -= Step;
			if (Engine.Current <= -1.f)
			{
				Engine.Current = -1.f;
				Engine.Stage = false;
			}
		}
	}

	Result.rotateZ(-(Engine.Current * (1.f - Rpm) * deg2rad(Engine.Angle) * Gas + (Twist * deg2rad(Engine.Angle)) * Gas));

	return true;
}

bool CCarDashboard::UpdateGear(CBoneInstance& Bone, Fmatrix& Result)
{
	float AngleX = GearLever.ParkAngleX;
	float AngleZ = GearLever.MiddleAngle;
	if (Telemetry.HasDriver)
	{
		const size_t Gear = Telemetry.Gear;

		// The lever starts moving GearDelay seconds after the gear change.
		// The hand code moves to the lever immediately; the lever itself
		// stays still during this window.
		if (GearLever.LastGear != static_cast<size_t>(-1) &&
			(GearLever.LastGear != Gear || (Telemetry.TransmissionSwitching && !GearLever.WasSwitching)))
		{
			GearLever.ChangeTime = Device.fTimeGlobal;
		}
		GearLever.LastGear = Gear;
		GearLever.WasSwitching = Telemetry.TransmissionSwitching;

		const bool WaitDelay =
			GearLever.Delay > 0.f &&
			GearLever.ChangeTime >= 0.f &&
			Device.fTimeGlobal < GearLever.ChangeTime + GearLever.Delay;

		if (!GearLever.Manual)
		{
			if (Telemetry.EngineOn || Telemetry.SpeedKmh > 1.f)
			{
				AngleX = (Gear == 0) ? GearLever.ReverseAngleX : GearLever.DriveAngleX;
			}
		}
		else
		{
			float FinalAngleX;
			if (Gear == 0)
			{
				FinalAngleX = GearLever.ReverseAngleX;
			}
			else
			{
				FinalAngleX = (Gear % 2 == 1) ? GearLever.DriveAngleX : GearLever.ReverseAngleX;
			}

			float FinalAngleZ;
			if (Gear == 0 || Gear > 4)
			{
				FinalAngleZ = GearLever.RightAngle;
			}
			else if (Gear < 3)
			{
				FinalAngleZ = GearLever.LeftAngle;
			}
			else
			{
				FinalAngleZ = GearLever.MiddleAngle;
			}

			if (Telemetry.TransmissionSwitching)
			{
				// Out of the current gate, through the neutral center, then into
				// the target gate. A same-gate shift keeps its side.
				AngleX = GearLever.ParkAngleX;
				AngleZ = (fabsf(GearLever.CurrentAngleZ - FinalAngleZ) > 0.01f)
					? GearLever.MiddleAngle
					: FinalAngleZ;
			}
			else
			{
				AngleX = FinalAngleX;
				AngleZ = FinalAngleZ;
			}
		}

		if (WaitDelay)
		{
			AngleX = GearLever.CurrentAngleX;
			AngleZ = GearLever.CurrentAngleZ;
		}
		else
		{
			float Delay = Telemetry.TransmissionSwitchDelay;
			if (Delay > 0.f)
			{
				// The lever first travels along X (out of the gate), then along Z.
				clamp(Delay, 0.f, 1.f);
				const float Speed = (50.f + 150.f * (1.f - Delay)) * GearLever.Speed * Device.fTimeDelta;
				if (GearLever.CurrentAngleX != AngleX)
				{
					if (GearLever.CurrentAngleX > AngleX)
					{
						GearLever.CurrentAngleX -= Speed;
						clamp(GearLever.CurrentAngleX, AngleX, GearLever.CurrentAngleX);
					}
					else
					{
						GearLever.CurrentAngleX += Speed;
						clamp(GearLever.CurrentAngleX, GearLever.CurrentAngleX, AngleX);
					}
				}
				else
				{
					if (GearLever.CurrentAngleZ > AngleZ)
					{
						GearLever.CurrentAngleZ -= Speed * 0.5f;
						clamp(GearLever.CurrentAngleZ, AngleZ, GearLever.CurrentAngleZ);
					}
					if (GearLever.CurrentAngleZ < AngleZ)
					{
						GearLever.CurrentAngleZ += Speed * 0.5f;
						clamp(GearLever.CurrentAngleZ, GearLever.CurrentAngleZ, AngleZ);
					}
				}
				AngleX = GearLever.CurrentAngleX;
				AngleZ = GearLever.CurrentAngleZ;
			}
		}
	}

	if (GearLever.Manual)
	{
		Fmatrix Side;
		Side.rotateZ(deg2rad(AngleZ));
		Bone.mTransform.mulB_43(Side);
	}
	Result.rotateX(deg2rad(AngleX));

	return true;
}

bool CCarDashboard::UpdateClockSeconds(Fmatrix& Result)
{
	float Seconds = Level().GetGameDayTimeSec();
	if (Clock.RealSeconds)
	{
		Seconds /= Level().GetGameTimeFactor();
	}

	Result.rotateZ(deg2rad(Clock.SecondsOffset * 6.f) - deg2rad(Seconds * 6.f));

	return true;
}

bool CCarDashboard::UpdateClockMinutes(Fmatrix& Result)
{
	Result.rotateZ(deg2rad(Clock.MinutesOffset * 6.f) - deg2rad(Level().GetGameDayTimeSec() / 10.f));

	return true;
}

bool CCarDashboard::UpdateClockHours(Fmatrix& Result)
{
	Result.rotateZ(deg2rad(Clock.HoursOffset * 30.f) - deg2rad(Level().GetGameDayTimeSec() / 120.f));

	return true;
}
