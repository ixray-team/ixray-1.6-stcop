#pragma once
#include "../xrCore/Containers/RingBufferEx.h"
#include "IStreamPlayer.h"

struct SDL_AudioSpec;
struct SDL_AudioDeviceStream;
struct OpusDecoder;

class CStreamPlayerA : public IStreamPlayer
{
public:
	CStreamPlayerA(int sampleRate);
	~CStreamPlayerA();

	virtual void PushToPlay(const void* data, int count) override;
	virtual bool IsPlaying() override;
	virtual void Update() override;

	virtual void SetDistance(float value) override;
	virtual void SetPosition(const Fvector& pos) override;

private:
	void UpdateVolume(float* buffer, int samples);

private:
	static constexpr int RING_BUFFER_SIZE = 262144;
	CRingBuffer<float, RING_BUFFER_SIZE> m_ringBuffer;

	SDL_AudioStream* m_audioStream = nullptr;
	SDL_AudioSpec m_spec{};

	int m_sampleRate = 48000;

	OpusDecoder* m_pOpusDecoder = nullptr;

	Fvector m_position{ 0, 0, 0 };
	float m_distance = 0.f;
	bool m_isRelative = false;
};