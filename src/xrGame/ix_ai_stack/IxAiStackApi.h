#pragma once

#include "../../xrCore/_types.h"
#include "IxAiTypes.h"

class IxAiManager;

class IxAiStackApi final
{
public:
    static void Initialize();
    static void Shutdown();
    static void Update(f32 deltaTime);
    static bool IsActive();
    static IxAiManager* Manager();

    static void ReloadRuntimeConfig();
    static void ResetRuntimeOverrides();

    static IxAiControlMode GetControlMode();
    static void SetControlMode(IxAiControlMode mode);

    static bool IsLegacyOutputAllowed();
    static bool IsFeatureEnabled(IxAiFeatureGate feature);
    static void SetFeatureEnabled(IxAiFeatureGate feature, bool enabled);

private:
    IxAiStackApi() = delete;
};
