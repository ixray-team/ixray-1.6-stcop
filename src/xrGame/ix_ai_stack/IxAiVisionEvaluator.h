#pragma once

class CAI_Stalker;
class CEntityAlive;

// FOV / ray LOS probe for IX visual events.
class IxAiVisionEvaluator final
{
public:
    static bool StalkerHasClearSightToTarget(CAI_Stalker& stalker, CEntityAlive& targetAlive);

private:
    IxAiVisionEvaluator() = delete;
    ~IxAiVisionEvaluator() = delete;
};
