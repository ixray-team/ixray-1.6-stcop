#include "stdafx.h"
#include "StreamPlayerA.h"
#include "SoundVoiceChat.h"
#include "SoundRender_Core.h"

#include <opus.h>

CStreamPlayerA::CStreamPlayerA(int sampleRate)
	: m_sampleRate(sampleRate)
{
	SDL_zero(m_spec);
	m_spec.freq = sampleRate;
	m_spec.format = SDL_AUDIO_F32;
	m_spec.channels = 1;

	m_audioStream = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &m_spec, nullptr, 0);
	if (!m_audioStream)
	{
		Msg("SDL error: %s", SDL_GetError());
		return;
	}

	SDL_ResumeAudioStreamDevice(m_audioStream);

	int error;
	m_pOpusDecoder = opus_decoder_create(sampleRate, 1, &error);
	R_ASSERT2(error == OPUS_OK, "Opus decoder creation failed");
}

CStreamPlayerA::~CStreamPlayerA()
{
	if (m_audioStream)
	{
		SDL_DestroyAudioStream(m_audioStream);
		m_audioStream = nullptr;
	}
	if (m_pOpusDecoder)
	{
		opus_decoder_destroy(m_pOpusDecoder);
		m_pOpusDecoder = nullptr;
	}
}

void CStreamPlayerA::SetDistance(float value)
{
	m_isRelative = (value == 0);
	m_distance = std::clamp(value, 0.f, 1000.f);
}

void CStreamPlayerA::SetPosition(const Fvector& pos)
{
	m_position = pos;
}

void CStreamPlayerA::PushToPlay(const void* data, int count)
{
	float decoded_buf[1024];
	int decoded_len = opus_decode_float(
		m_pOpusDecoder,
		reinterpret_cast<const unsigned char*>(data),
		count,
		decoded_buf,
		VOICE_SAMPLES_PER_BUFFER,
		0
	);

	if (decoded_len <= 0)
		return;

	m_ringBuffer.Write(decoded_buf, decoded_len);
}

void CStreamPlayerA::UpdateVolume(float* buffer, int samples)
{
	float volume = 1.0f;

	if (!m_isRelative)
	{
		float distance = SoundRender->listener_position().distance_to(m_position);
		float max_dist = m_distance;
		float min_dist = m_distance / 3.0f;

		if (distance <= min_dist)
			volume = 1.0f;
		else if (distance >= max_dist)
			volume = 0.0f;
		else
			volume = (max_dist - distance) / (max_dist - min_dist);

		volume *= psSoundVPlayers;
		volume = std::clamp(volume, 0.01f, 1.0f);
	}

	for (int i = 0; i < samples; ++i)
	{
		buffer[i] *= volume;
	}
}

void CStreamPlayerA::Update()
{
	if (!m_audioStream)
		return;

	if (!m_ringBuffer.BytesToRead())
		return;

	float tempBuffer[4096];
	int readed = m_ringBuffer.Read(tempBuffer, 4096);

	if (readed <= 0)
		return;

	static float floatBuffer[4096];
	for (int i = 0; i < readed; ++i)
	{
		floatBuffer[i] = tempBuffer[i];
	}

	UpdateVolume(floatBuffer, readed);

	SDL_PutAudioStreamData(m_audioStream, floatBuffer, readed * sizeof(float));
}

bool CStreamPlayerA::IsPlaying()
{
	// SDL3 �� ��� ������ ���������� � "is playing",
	// �� ���� ����� ��� ������� � ������ ��������������� ���
	return SDL_GetAudioStreamQueued(m_audioStream) > 0;
}