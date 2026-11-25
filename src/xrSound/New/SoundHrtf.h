#pragma once
#include "SoundMeta.h"

// "spatialize" voice effect: binaural rendering of a mono voice with the SADIE 3rd order spherical-harmonic HRIRs.
// The SH encoding of the voice direction is folded into one HRIR pair per voice and applied with an FFT convolution.
// Falls back to the "spatial" stereo panner when HRTF is off (ss_HRTF) or no renderer is free.
bool Snd_HrtfProc(void* State, SoundEffectOp Op, void* Arg);
void Snd_ShutdownHrtf();
