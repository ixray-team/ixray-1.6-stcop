#include "StdAfx.h"

#include "../../xrSound/ai_sounds.h"
#include "IxAiSoundClassification.h"
#include "IxAiStackTuning.h"

IxAiPerceptionEventType IxAiSoundClassification::MapEngineSoundType(int soundType, float power)
{
    const u32 mask = (u32)soundType;

    if ((mask & (u32)SOUND_TYPE_WEAPON_SHOOTING) == (u32)SOUND_TYPE_WEAPON_SHOOTING)
    {
        if (power < g_ixAiRuntimeTuning.silencedShotPowerCutoff)
        {
            return IxAiPerceptionEventType::SoundSilenced;
        }

        return IxAiPerceptionEventType::SoundGunshot;
    }

    if ((mask & (u32)SOUND_TYPE_WEAPON_BULLET_HIT) == (u32)SOUND_TYPE_WEAPON_BULLET_HIT)
    {
        return IxAiPerceptionEventType::SoundWeaponSurface;
    }

    if ((mask & (u32)SOUND_TYPE_WEAPON_RECHARGING) == (u32)SOUND_TYPE_WEAPON_RECHARGING)
    {
        return IxAiPerceptionEventType::SoundWeaponHandling;
    }

    if ((mask & (u32)SOUND_TYPE_WEAPON_EMPTY_CLICKING) == (u32)SOUND_TYPE_WEAPON_EMPTY_CLICKING)
    {
        return IxAiPerceptionEventType::SoundWeaponHandling;
    }

    if ((mask & (u32)SOUND_TYPE_WORLD_OBJECT_EXPLODING) == (u32)SOUND_TYPE_WORLD_OBJECT_EXPLODING)
    {
        return IxAiPerceptionEventType::SoundExplosion;
    }

    if ((mask & (u32)SOUND_TYPE_WORLD_OBJECT_BREAKING) == (u32)SOUND_TYPE_WORLD_OBJECT_BREAKING)
    {
        return IxAiPerceptionEventType::SoundPhysics;
    }

    if ((mask & (u32)SOUND_TYPE_WORLD_OBJECT_COLLIDING) == (u32)SOUND_TYPE_WORLD_OBJECT_COLLIDING)
    {
        return IxAiPerceptionEventType::SoundPhysics;
    }

    if ((mask & (u32)SOUND_TYPE_ITEM_DROPPING) == (u32)SOUND_TYPE_ITEM_DROPPING)
    {
        return IxAiPerceptionEventType::SoundItemFumble;
    }

    if ((mask & (u32)SOUND_TYPE_ITEM_PICKING_UP) == (u32)SOUND_TYPE_ITEM_PICKING_UP)
    {
        return IxAiPerceptionEventType::SoundItemFumble;
    }

    if ((mask & (u32)SOUND_TYPE_ITEM_HIDING) == (u32)SOUND_TYPE_ITEM_HIDING)
    {
        return IxAiPerceptionEventType::SoundItemFumble;
    }

    if ((mask & (u32)SOUND_TYPE_ITEM_TAKING) == (u32)SOUND_TYPE_ITEM_TAKING)
    {
        return IxAiPerceptionEventType::SoundItemFumble;
    }

    if ((mask & (u32)SOUND_TYPE_ITEM_USING) == (u32)SOUND_TYPE_ITEM_USING)
    {
        return IxAiPerceptionEventType::SoundItemFumble;
    }

    if ((mask & (u32)SOUND_TYPE_MONSTER_INJURING) == (u32)SOUND_TYPE_MONSTER_INJURING)
    {
        return IxAiPerceptionEventType::SoundCry;
    }

    if ((mask & (u32)SOUND_TYPE_MONSTER_STEP) == (u32)SOUND_TYPE_MONSTER_STEP)
    {
        return IxAiPerceptionEventType::SoundFootstep;
    }

    if ((mask & (u32)SOUND_TYPE_MONSTER_TALKING) == (u32)SOUND_TYPE_MONSTER_TALKING)
    {
        return IxAiPerceptionEventType::SoundCry;
    }

    if ((mask & (u32)SOUND_TYPE_MONSTER_DYING) == (u32)SOUND_TYPE_MONSTER_DYING)
    {
        return IxAiPerceptionEventType::SoundCry;
    }

    if ((mask & (u32)SOUND_TYPE_MONSTER_ATTACKING) == (u32)SOUND_TYPE_MONSTER_ATTACKING)
    {
        return IxAiPerceptionEventType::Other;
    }

    if ((mask & (u32)SOUND_TYPE_MONSTER_EATING) == (u32)SOUND_TYPE_MONSTER_EATING)
    {
        return IxAiPerceptionEventType::Other;
    }

    if ((mask & (u32)SOUND_TYPE_WORLD_AMBIENT) == (u32)SOUND_TYPE_WORLD_AMBIENT)
    {
        return IxAiPerceptionEventType::None;
    }

    (void)power;
    return IxAiPerceptionEventType::Other;
}
