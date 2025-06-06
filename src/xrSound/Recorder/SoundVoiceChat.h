#pragma once
#include "ISoundVoiceChat.h"
#include "SoundRecorderA.h"
#include "StreamPlayerA.h"
#include "VoicePacketsPacker.h"

struct ALCcontext;

#define VOICE_SAMPLE_RATE 24000
#define VOICE_SAMPLES_PER_BUFFER 960

class SoundVoiceChat : 
	public ISoundVoiceChat
{
public:
	SoundVoiceChat();
	~SoundVoiceChat();

	ISoundRecorder* CreateRecorder(IVoicePacketSender* sender);

	IStreamPlayer* CreateStreamPlayer();
	void DestroySoundPlayer(IStreamPlayer* player);

	void Update(const Fvector& P, const Fvector& D, const Fvector& N);

private:
	void Destroy();

private:
	CSoundRecorderA* m_pRecorder = nullptr;
	CVoicePacketsPacker* m_pVoicePacker = nullptr;

	xr_vector<IStreamPlayer*> m_players;
};