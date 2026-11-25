#pragma once
#include "SoundMeta.h"

// "reverb" bus effect: algorithmic room reverb used by the generated zone buses.
// A 16-channel diffuser feeds a 16-line feedback delay network with slowly modulated line lengths and
// separate RT60 below and above 1.4 kHz, which renders a dense tail without metallic ringing;
// one low-passed tap renders the shoebox early reflections. The tail level follows the same
// curve over RT60 as the Resonance Audio spectral reverb it replaces, so the zone gains keep their meaning.
bool Snd_RoomReverbProc(void* State, SoundEffectOp Op, void* Arg);
