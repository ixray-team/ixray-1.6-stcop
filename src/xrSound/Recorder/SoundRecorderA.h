#pragma once
#include "ISoundRecorder.h"

struct ALCdevice;
class CSpeexPreprocess;
class CVoicePacketsPacker;

class CSoundRecorderA : public ISoundRecorder
{
public:
	CSoundRecorderA(u32 sampleRate, int format, u32 samplesPerBuffer);
	~CSoundRecorderA();

	bool Init(CVoicePacketsPacker* packetsPacker);
	void Destroy();

	virtual bool IsStarted() { return m_started; }

	virtual void Start();
	virtual void Stop();

	void Update();

private:
	void ChangeGain(s8* buffer, int length);

private:
	u32 m_sampleRate;
	int m_format;
	int m_samplesPerBuffer;

	u32 m_bytesPerSample;

	s8* m_buffer;
	bool m_started = false;

	CSpeexPreprocess* m_speexPreprocess = nullptr;
	CVoicePacketsPacker* m_packetsPacker = nullptr;

	ALCdevice* m_pCaptureDevice = nullptr;
};