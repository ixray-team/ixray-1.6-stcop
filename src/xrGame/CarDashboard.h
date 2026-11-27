#pragma once

class CBoneInstance;
class CInifile;
class IKinematics;

struct SCarTelemetry
{
	float DriveWheelsSpeed = 0.f; // m/s
	bool HasDriveWheels = false;
	float SpeedKmh = 0.f;
	float Rpm = 0.f;
	float MinRpm = 0.f;
	float MaxRpm = 1.f;
	float FuelRatio = 0.f;
	float BrakeElapsed = 0.f;
	float BrakeTime = 0.f;
	float TransmissionSwitchDelay = 0.f;
	size_t Gear = 0;
	bool EngineOn = false;
	bool Drive = false;
	bool TransmissionSwitching = false;
	bool Brakes = false;
	bool HasDriver = false;
};

// Dashboard instruments of a car: needles, clocks, fan, blower and the gear lever. 
// Each instrument is a bone of the car visual whose ltx section in the visual's user data names the instrument type. 
// Instruments of the same type share one config and one state, so several bones of one type move together.
class CCarDashboard
{
public:
	CCarDashboard() = default;
	CCarDashboard(const CCarDashboard&) = delete;
	CCarDashboard& operator=(const CCarDashboard&) = delete;

	void BindBone(IKinematics& Kinematics, CInifile& Ini, const char* Section, u16 BoneId);
	void Unbind(IKinematics* Kinematics);

	void SetTelemetry(const SCarTelemetry& Value)
	{
		Telemetry = Value;
	}

	bool Empty() const
	{
		return Bones.empty();
	}

	float GearDelay() const
	{
		return GearLever.Delay;
	}

private:
	enum class EGauge : u8
	{
		Speedometer,
		Tachometer,
		Fuel,
		Temperature,
		Battery,
		Generic,
		Engine,
		Blower,
		Pressure,
		BrakesPressure,
		BrakesPressureApply,
		Fan,
		Gear,
		ClockSeconds,
		ClockMinutes,
		ClockHours,
	};

	// callback_param of a gauge bone.
	struct SBinding
	{
		CCarDashboard* Owner = nullptr;
		EGauge Type = EGauge::Speedometer;
	};

	struct SSpeedometer
	{
		float Factor = 1.f;
		float MaxSpeed = flt_max;
		float MinSpeed = 0.f;
		float Offset = 0.f;
		float Current = 0.f;
	};

	struct STachometer
	{
		float Factor = 1.f;
		float MinRpm = 0.f;
		float Current = 0.f;
	};

	struct SFuel
	{
		float Angle = 0.f;
		float Offset = 0.f;
		float Current = 0.f;
	};

	struct STemperature
	{
		float Angle = 0.f;
		float Offset = 0.f;
		float Current = 0.f;
		float OffsetCurrent = 0.f;
		float Value = 0.f;
	};

	struct SAngleGauge
	{
		float Angle = 0.f;
		float Current = 0.f;
	};

	struct SBattery
	{
		float Factor = 0.f;
		float Angle = 0.f;
		float Current = 0.f;
	};

	struct SFan
	{
		float Speed = 1.f;
		bool FollowEngine = false;
		float SpeedCurrent = 0.f;
		float Rotation = 0.f;
	};

	struct SPressure
	{
		float MaxAngle = 0.f;
		float IdleAngle = 0.f;
		float Factor = 1.f;
		float Offset = 0.f;
		float Current = 0.f;
		float Value = 0.f;
	};

	struct SEngine
	{
		float Angle = 0.f;
		float Speed = 0.f;
		float Current = 0.f;
		bool Stage = false;
	};

	struct SBrakes
	{
		float PressureFactor = 0.f;
		float PressureMaxAngle = 0.f;
		float PressureCurrent = 0.f;
		float Pressure = 0.f;
		float ApplyAngle = 0.f;
		float ApplyCurrent = 0.f;
	};

	struct SGearLever
	{
		float ParkAngleX = 0.f;
		float DriveAngleX = -15.f;
		float ReverseAngleX = -5.f;
		float LeftAngle = 10.f;
		float MiddleAngle = 0.f;
		float RightAngle = -10.f;
		float Speed = 1.f;
		// Delay before the gear lever starts moving after a gear change, seconds.
		float Delay = 0.f;
		bool Manual = false;
		float CurrentAngleX = 0.f;
		float CurrentAngleZ = 0.f;
		float ChangeTime = -1.f;
		size_t LastGear = static_cast<size_t>(-1);
		bool WasSwitching = false;
	};

	struct SClock
	{
		float SecondsOffset = 0.f;
		float MinutesOffset = 0.f;
		float HoursOffset = 0.f;
		bool RealSeconds = true;
	};

	static void __stdcall GaugeCallback(CBoneInstance* Bone);

	void ReadConfig(EGauge Type, CInifile& Ini, const char* Section);
	bool Evaluate(EGauge Type, CBoneInstance& Bone, Fmatrix& Result);

	bool UpdateSpeedometer(Fmatrix& Result);
	bool UpdateTachometer(Fmatrix& Result);
	bool UpdateFuel(Fmatrix& Result);
	bool UpdateTemperature(Fmatrix& Result);
	bool UpdateBattery(Fmatrix& Result);
	bool UpdateGeneric(Fmatrix& Result);
	bool UpdateFan(Fmatrix& Result);
	bool UpdateBlower(Fmatrix& Result);
	bool UpdatePressure(Fmatrix& Result);
	bool UpdateBrakesPressure(Fmatrix& Result);
	bool UpdateBrakesPressureApply(Fmatrix& Result);
	bool UpdateEngine(Fmatrix& Result);
	bool UpdateGear(CBoneInstance& Bone, Fmatrix& Result);
	bool UpdateClockSeconds(Fmatrix& Result);
	bool UpdateClockMinutes(Fmatrix& Result);
	bool UpdateClockHours(Fmatrix& Result);

	SCarTelemetry Telemetry;
	SSpeedometer Speedometer;
	STachometer Tachometer;
	SFuel Fuel;
	STemperature Temperature;
	SAngleGauge Generic;
	SAngleGauge Blower;
	SBattery Battery;
	SFan Fan;
	SPressure Pressure;
	SEngine Engine;
	SBrakes Brakes;
	SGearLever GearLever;
	SClock Clock;

	xr_vector<xr_unique_ptr<SBinding>> Bindings;
	xr_vector<u16> Bones;
};
