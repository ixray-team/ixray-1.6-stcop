#pragma once

// ------------------------------------------------------------
// Lights
// ------------------------------------------------------------
struct LightDef
{
    shared_str Bone;
    Fcolor Color{ 1,1,1,1 };
    float Range{ 10.f };
    float ConeAngle{ 60.f };
    shared_str SpotTexture;
    shared_str GlowTexture;
    float GlowRadius{ 0.1f };
    bool IsPoint{ false };
};

// ------------------------------------------------------------
// Полная конфигурация автомобиля
// ------------------------------------------------------------
struct CarConfig
{
    // General
    shared_str Name;
    float Mass{ 1200.f };

    // Geometry
    Fvector Size{ 1,1,1 };
    Fvector CenterOfMass{ 0,0,0 };

    // Physics
    float EnginePower{ 100.f };
    float FuelTank{ 50.f };
    float FuelConsumption{ 10.f };

    // Camera points
    Fvector CamPos{ 0,2,0 };
    Fvector CamPosFirst{ 0,1.5f,0 };
    Fvector CamPosLookAt{ 0,1,5 };
    Fvector CamPosFree{ 0,3,5 };

    // Exit points
    Fvector ExitPos{ 1,0,0 };
    Fvector ExitPosDriver{ -1,0,0 };

    // Bones (lists)
    xr_vector<shared_str> DrivingWheels;
    xr_vector<shared_str> SteeringWheels;
    xr_vector<shared_str> BreakingWheels;
    xr_vector<shared_str> Doors;

    // Bones (single)
    shared_str SteerBone;
    shared_str DriverPlace;
    shared_str ExhaustBone;
    shared_str TrunkBone;

    // Engine params
    float PowerIncFactor{ 1.f };
    float PowerDecFactor{ 1.f };
    float RpmIncFactor{ 1.f };
    float RpmDecFactor{ 1.f };
    float NeutralPowerFactor{ 1.f };
    float MaxEngineRpm{ 6000.f };
    float MaxPowerRpm{ 4500.f };
    float MaxTorqueRpm{ 3500.f };
    float IdlingRpm{ 800.f };
    float LimiterRpm{ 6500.f };

    float AxleFriction{ 1.f };
    float SteeringSpeed{ 3.f };
    float SteeringTorque{ 100.f };
    float BrakeTorque{ 1000.f };
    float BrakeTime{ 0.5f };
    float HandBrakeTorque{ 3000.f };

    float MainGearRatio{ 4.1f };
    bool AutoTransmission{ false };

    // Transmission gears
    xr_map<shared_str, float> GearRatios;

    // Sounds
    shared_str EngineSound;
    shared_str StartSound;
    shared_str StopSound;
    shared_str TansmissionSwitchSound;
    shared_str ExplosionSound;

    // Lights
    xr_map<shared_str, LightDef> Lights;

    // I/O
    void Load(const shared_str& section);
    void Save(const shared_str& section);
};

extern CarConfig g_CarConfig;
