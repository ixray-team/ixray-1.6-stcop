#pragma once
#include "../xrCore/RingBufferEx.h"
#include "IStreamPlayer.h"

struct ALCcontext;
struct OpusDecoder;

class CStreamPlayerA : public IStreamPlayer
{
public:
	CStreamPlayerA(u32 sampleRate, int format, ALCcontext* context);
	~CStreamPlayerA();

	virtual void PushToPlay(const void* data, int count);
	virtual bool IsPlaying();

	virtual void Update();

	virtual void SetDistance(float value);
	virtual void SetPosition(const Fvector& pos);
private:
	void UpdateVolume();

private:

	static constexpr int RING_BUFFER_SIZE = 262144;
	CRingBuffer<s16, RING_BUFFER_SIZE> m_ringBuffer;

	static constexpr int NUM_BUFFERS = 16;
	u32 m_buffers[NUM_BUFFERS];

	xr_deque<u32> m_freeBuffers;

	u32 m_sampleRate;
	int m_format;

	ALCcontext* m_pContext;
	u32 m_source;

	OpusDecoder* m_pOpusDecoder;

	Fvector m_position{ 0, 0, 0 };

	float m_distance = 0;
	bool m_isRelative = false;
};