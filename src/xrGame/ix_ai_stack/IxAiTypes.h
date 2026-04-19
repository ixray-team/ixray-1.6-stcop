#pragma once

#include "../../xrCore/_types.h"
#include "../../xrCore/_vector3d.h"

enum class IxAiSquadFanoutFlags : u8
{
    None = 0,
    StealthClassified = 1,
};

enum class IxAiPerceptionEventType : u8
{
    SoundGunshot,
    SoundSilenced,
    SoundFootstep,
    SoundCry,
    SoundWeaponHandling,
    SoundWeaponSurface,
    SoundPhysics,
    SoundExplosion,
    SoundItemFumble,
    SoundBoltImpact,
    VisualPlayer,
    VisualCorpse,
    VisualBlood,
    LightSource,
    SquadAllyWounded,
    SquadCombatEngaged,
    Other,
    None
};

struct IxAiPerceptionEvent final
{
    Fvector _position{};
    u16 _sourceObjectId{};
    IxAiPerceptionEventType _type{IxAiPerceptionEventType::None};
    f32 _intensity{};
    f32 _radius{};
    f32 _timestamp{};
    u8 _squadFanoutFlags{};

    IxAiPerceptionEvent();
    ~IxAiPerceptionEvent();
};

enum class IxAiAlertLevel : u8
{
    Vigilant,
    Suspicious,
    Search,
    Combat,
    None
};

enum class IxAiBehaviourKind : u8
{
    GuardBasic,
    FlankerLite,
};

struct IxAiMemorySlot final
{
    IxAiPerceptionEventType _type{IxAiPerceptionEventType::None};
    Fvector _position{};
    f32 _timeStamp{};
    f32 _strength{};
};

enum class IxAiBeliefLayer : u8
{
    Sensory,
    Working
};

struct IxAiBeliefGrain final
{
    IxAiPerceptionEventType _type{IxAiPerceptionEventType::None};
    Fvector _position{};
    f32 _timeStamp{};
    f32 _confidence{};
};

struct IxAiBehaviourProfile final
{
    f32 _suspicionDecayRate{};
    f32 _alertRadius{};
    f32 _silencedGunHearingMultiplier{};
    bool _alwaysLooksForCover{};
    f32 _flankRange{};
    IxAiBehaviourKind _behaviourKind{IxAiBehaviourKind::GuardBasic};
    bool _useBehaviourTreeNodePool{};

    IxAiBehaviourProfile();
    ~IxAiBehaviourProfile();
};
