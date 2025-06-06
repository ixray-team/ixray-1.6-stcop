#pragma once
#include "ISoundRecorder.h"

struct ALCdevice;
class CSpeexPreprocess;
class CVoicePacketsPacker;

class CSoundRecorderA : public ISoundRecorder
{
public:
	CSoundRecorderA(int sampleRate, int samplesPerBuffer);
	~CSoundRecorderA();

	bool Init(CVoicePacketsPacker* packetsPacker);
	void Destroy();

	virtual bool IsStarted() { return m_started; }

	virtual void Start();
	virtual void Stop();

	void Update();

private:
	void ChangeGain(float* buffer, size_t length);

private:
	u32 m_sampleRate;
	int m_samplesPerBuffer;

	u32 m_bytesPerSample;

	bool m_started = false;

	CSpeexPreprocess* m_speexPreprocess = nullptr;
	CVoicePacketsPacker* m_packetsPacker = nullptr;
	SDL_AudioStream* m_captureStream = 0;
	SDL_AudioSpec m_captureSpec = {};
	float* m_buffer = nullptr;
	xr_vector<u8> m_accumBuffer;
};