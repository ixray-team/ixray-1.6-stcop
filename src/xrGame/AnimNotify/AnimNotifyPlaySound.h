#pragma once
#include "script_sound.h"
#include "../xrCore/AnimNotify/AnimNotifyTypes.h"

class CAnimNotifyPlaySound : public IAnimNotify
{
    shared_str m_sound_to_play;
    xr_queue<CScriptSound*> m_sounds_buffer;
    xr_set<CScriptSound*> m_playing_sounds = {};
    
public:
    void Construct(const CInifile& ini, LPCSTR sect) override;
    void Execute(IRenderVisual* visual, u16 bone_id) override;
    
};
