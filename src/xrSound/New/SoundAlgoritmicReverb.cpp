/**************************************************************************************
 * Copyright (C) 2026 Anton Kovalev (vertver)
 * New Sound Engine
 ***************************************************************************************
 * Source code is licensed under the following terms:
 *
 * 1. IX-Ray Team License
 *    Non-exclusive, royalty-free, perpetual license is hereby granted to:
 *      - ForserX   (https://github.com/ForserX)
 *      - Drombeys  (https://github.com/Drombeys)
 *      - v2v3v4    (https://github.com/v2v3v4)
 *
 *    Permitted rights:
 *      - Copy, modify, merge, publish and distribute this Software
 *        and its documentation.
 *
 * 2. Public Access License
 *    Non-exclusive, "access-view-study" rights granted to everyone else.
 *
 *    Permitted rights:
 *      - Private copying is allowed, provided that no distribution occurs.
 *      - Public cloning (i.e. "forking") is allowed, but any source code
 *        modification or binary redistribution is prohibited.
 *
 * Usage of this Software beyond the rights granted above is strictly prohibited.
 *
 * The above copyright notice and this license text must be included in all
 * copies or substantial portions of the Software.
 **************************************************************************************/
#include "SoundAlgoritmicReverb.h"
#include "SoundSource.h"
#include "SoundDSP.h"
#include "ReverbInterface.h"

#define SND_REVERB_ZONE_IDLE_MS (3000)
#define SND_REVERB_RAY_RANGE (1000.0f)

// Algorithmic (Resonance / Steam Audio) reverb is attenuated by -6 dB (x0.5) relative to its configured level
#define SND_REVERB_ZONE_GAIN (0.010f * 0.5f)

struct SoundReverbState
{
	xrSRWLock ZoneLock;
	xr_vector<sound_zone_params> Zones;
	bool IsEditorZone = false;
};

IReverInterface* GReverInterface = nullptr;
static SoundReverbState GReverb = {};

static void Snd_GetZoneBuffer(sound_zone_params& Zone, float** OutBuffer)
{
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		OutBuffer[Channel] = Zone.data[Channel];
	}
}

void Snd_ShutdownReverb()
{
	if (GReverInterface != nullptr)
	{
		delete GReverInterface;
		GReverInterface = nullptr;
	}
}

u32 Snd_FindReverbZone(const Fvector& Position)
{
	CDB::MODEL* EnvModel = ::Sound->get_geometry_env();
	CDB::COLLIDER* Collider = ::Sound->get_geometry_db();
	if (EnvModel == nullptr)
	{
		return 0;
	}

	Fvector Dir = {0.0f, -1.0f, 0.0f};
	Collider->ray_options(CDB::OPT_ONLYNEAREST);
	Collider->ray_query(EnvModel, Position, Dir, SND_REVERB_RAY_RANGE);
	if (Collider->r_count() == 0)
	{
		return 0;
	}

	const CDB::TRI& Tri = EnvModel->get_tris()[Collider->r_begin()->id];

	xrSRWLockGuard Guard(GReverb.ZoneLock, true);
	R_ASSERT(Tri.dummy < GReverb.Zones.size());
	return Tri.dummy + 1;
}

void Snd_BeginReverbBlock()
{
	xrSRWLockGuard Guard(GReverb.ZoneLock, true);
	for (sound_zone_params& Zone : GReverb.Zones)
	{
		Zone.use_count = 0;
		memset(Zone.data, 0, sizeof(Zone.data));
	}
}

void Snd_SendToReverbZone(u32 ZoneIdx, float** Data, float BeginFactor, float EndFactor, float Left, float Right)
{
	xrSRWLockGuard Guard(GReverb.ZoneLock, true);
	if (GReverb.IsEditorZone)
	{
		ZoneIdx = 1;
	}

	if (ZoneIdx == 0 || ZoneIdx > GReverb.Zones.size())
	{
		return;
	}

	sound_zone_params& Zone = GReverb.Zones[ZoneIdx - 1];
	Zone.use_count++;
	Zone.last_use_ms = Snd_Milliseconds();

	float* ReverbBuffer[SND_CHANNEL_COUNT];
	Snd_GetZoneBuffer(Zone, ReverbBuffer);
	DSP_MixBufferPanning(ReverbBuffer, Data, BeginFactor, EndFactor, Left, Right, SND_BLOCKSIZE);
}

void Snd_RenderReverbZones(float** ScratchBuffer, float** BusBuffer)
{
	if (!psSoundFlags.is(ss_EFX))
	{
		return;
	}

	xrSRWLockGuard Guard(GReverb.ZoneLock, true);
	for (sound_zone_params& Zone : GReverb.Zones)
	{
		if (Zone.use_count == 0 && (Zone.last_use_ms + SND_REVERB_ZONE_IDLE_MS) < Snd_Milliseconds())
		{
			continue;
		}

		PROF_EVENT("Reverb rendering");
		float* ReverbBuffer[SND_CHANNEL_COUNT];
		Snd_GetZoneBuffer(Zone, ReverbBuffer);

		if (GReverInterface != nullptr)
		{
			GReverInterface->ProcessReverb(Zone, ReverbBuffer, ScratchBuffer, BusBuffer);
		}

		float Gain = std::clamp(Zone.settings.reverb, 0.0f, 1.0f) * SND_REVERB_ZONE_GAIN;
		DSP_MixBuffer(BusBuffer, ScratchBuffer, Gain, Gain, SND_BLOCKSIZE);
	}
}

void XRay::Sound::Mixer::AddEditorZone(sound_zone_params& Params)
{
	ResetZones();
	AddZone(Params);

	xrSRWLockGuard Guard(GReverb.ZoneLock);
	GReverb.IsEditorZone = true;
}

void XRay::Sound::Mixer::AddZone(sound_zone_params& Params)
{
	xrSRWLockGuard Guard(GReverb.ZoneLock);
	if (GReverInterface != nullptr)
	{
		GReverInterface->InitZone(Params);
	}

	GReverb.Zones.emplace_back(std::move(Params));
}

void XRay::Sound::Mixer::ResetZones()
{
	xrSRWLockGuard Guard(GReverb.ZoneLock);
	for (sound_zone_params& Zone : GReverb.Zones)
	{
		if (GReverInterface != nullptr)
		{
			GReverInterface->ReleaseZone(Zone);
		}
	}

	GReverb.Zones.clear();
}

const xr_vector<sound_zone_params>& XRay::Sound::Mixer::GetZones()
{
	return GReverb.Zones;
}
