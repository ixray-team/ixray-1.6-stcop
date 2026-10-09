#pragma once

#ifdef DISABLE_RESONANCE_AUDIO
inline void Resonance_RegisterEffects() {}
inline void Resonance_Shutdown() {}
#else
void Resonance_RegisterEffects();
void Resonance_Shutdown();
#endif
