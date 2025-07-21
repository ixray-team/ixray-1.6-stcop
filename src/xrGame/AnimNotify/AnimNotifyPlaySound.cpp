#include "stdafx.h"
#include "AnimNotifyPlaySound.h"

#include "../Include/xrRender/Kinematics.h"
#include "../Include/xrRender/RenderVisual.h"

constexpr u32 m_sounds_num = 20;

void CAnimNotifyPlaySound::Construct(const CInifile& ini, LPCSTR sect)
{
    m_sound_to_play = ini.r_string(sect, "sound");
    for (u32 i = 0; i < m_sounds_num; i++)
    {
        m_sounds_buffer.emplace(new CScriptSound(m_sound_to_play.c_str()));
    }
}

void CAnimNotifyPlaySound::Execute(IRenderVisual* visual, u16 bone_id)
{
    auto Kinematics = visual->dcast_PKinematics();
    {
        xr_set<CScriptSound*> SoundsToRemove = {};
        for (auto& sound : m_playing_sounds)
        {
            if (!sound->IsPlaying())
            {
                SoundsToRemove.insert(sound);
            }
        }
        for (auto& sound : SoundsToRemove)
        {
            m_sounds_buffer.push(sound);
            m_playing_sounds.erase(sound);
        }
    }
    if (m_sounds_buffer.empty())
    {
        R_ASSERT3(false, "Too much calls of play sound notify", m_sound_to_play.c_str());
        return;
    }
    auto NewSound = new CScriptSound(m_sound_to_play.c_str());
    NewSound->PlayAtPos(nullptr, Kinematics->LL_GetBoneInstance(bone_id).mTransform.c);
    m_playing_sounds.emplace(NewSound);
}
