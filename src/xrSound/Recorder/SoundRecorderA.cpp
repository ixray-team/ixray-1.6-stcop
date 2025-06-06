#include "stdafx.h"
#include "Sound.h"
#include "SoundRecorderA.h"
#include "SpeexPreprocess.h"
#include "VoicePacketsPacker.h"
#include "ISoundRecorder.h"

namespace
{
	union ShortByteUnion 
	{
		s16 asShort;
		u8 asBytes[2];
	};
}

CSoundRecorderA::CSoundRecorderA(int sampleRate, int samplesPerBuffer)
	: m_sampleRate(sampleRate), m_samplesPerBuffer(samplesPerBuffer)
{
	m_bytesPerSample = sizeof(float);

	m_buffer = new float[samplesPerBuffer];
	m_speexPreprocess = new CSpeexPreprocess(sampleRate, samplesPerBuffer);
	m_speexPreprocess->EnableAGC(psSoundRecorderMode);
	m_speexPreprocess->EnableDenoise(psSoundRecorderDenoise);
	m_accumBuffer.reserve(m_samplesPerBuffer * sizeof(float));
}


CSoundRecorderA::~CSoundRecorderA()
{
	Destroy();

	xr_delete(m_buffer);
	xr_delete(m_speexPreprocess);
}

bool CSoundRecorderA::Init(CVoicePacketsPacker* packetsPacker)
{
	m_packetsPacker = packetsPacker;

	SDL_zero(m_captureSpec);
	m_captureSpec.freq = m_sampleRate;
	m_captureSpec.format = SDL_AUDIO_F32;
	m_captureSpec.channels = 1;

	m_captureStream = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_RECORDING, &m_captureSpec, nullptr, 0);
	if (!m_captureStream)
	{
		Msg("Failed to open capture stream: %s", SDL_GetError());
		return false;
	}

	SDL_ResumeAudioDevice(SDL_GetAudioStreamDevice(m_captureStream));
	return true;
}

void CSoundRecorderA::Destroy()
{
	if (m_captureStream)
	{
		SDL_DestroyAudioStream(m_captureStream);
		m_captureStream = nullptr;
	}
}

void CSoundRecorderA::Start()
{
	m_started = true;
}

void CSoundRecorderA::Stop()
{
	m_started = false;
}

void CSoundRecorderA::Update()
{
	PROF_EVENT("Sound: Recorder Core");

	if (!m_captureStream)
		return;

	u8 tempBuffer[4096];

	int bytesRead = SDL_GetAudioStreamData(m_captureStream, tempBuffer, sizeof(tempBuffer));
	if (bytesRead <= 0)
		return;

	m_accumBuffer.insert(m_accumBuffer.end(), tempBuffer, tempBuffer + bytesRead);

	const int requiredBytes = m_samplesPerBuffer * sizeof(float);
	if ((int)m_accumBuffer.size() < requiredBytes)
		return;

	memcpy(m_buffer, m_accumBuffer.data(), requiredBytes);
	m_accumBuffer.erase(m_accumBuffer.begin(), m_accumBuffer.begin() + requiredBytes);

	if (m_started && m_packetsPacker)
	{
		if (psSoundRecorderMode)
		{
			if (!m_speexPreprocess->IsAGCEnabled())
				m_speexPreprocess->EnableAGC(true);
		}
		else
		{
			if (m_speexPreprocess->IsAGCEnabled())
				m_speexPreprocess->EnableAGC(false);
		}

		if (psSoundRecorderDenoise)
		{
			if (!m_speexPreprocess->IsDenoiseEnabled())
				m_speexPreprocess->EnableDenoise(true);
		}
		else
		{
			if (m_speexPreprocess->IsDenoiseEnabled())
				m_speexPreprocess->EnableDenoise(false);
		}

		if (psSoundRecorderMode == 0)
		{
			ChangeGain(m_buffer, m_samplesPerBuffer);
		}

		if (psSoundRecorderMode || psSoundRecorderDenoise)
		{
			// Speex всё ещё short*, поэтому нужен конверт
			static xr_vector<short> tempShort(m_samplesPerBuffer);
			for (int i = 0; i < m_samplesPerBuffer; ++i)
				tempShort[i] = (short)(std::clamp(m_buffer[i], -1.0f, 1.0f) * 32767.0f);

			m_speexPreprocess->RunPreprocess(tempShort.data());

			for (int i = 0; i < m_samplesPerBuffer; ++i)
				m_buffer[i] = tempShort[i] / 32767.0f;
		}

		m_packetsPacker->AddPacket(m_buffer, requiredBytes);
	}
}

void CSoundRecorderA::ChangeGain(float* buffer, size_t length)
{
	const float modifier = psSoundVRecorder;

	for (int i = 0; i < length; ++i)
	{
		buffer[i] *= modifier;

		if (buffer[i] > 1.0f)
			buffer[i] = 1.0f;
		else if (buffer[i] < -1.0f)
			buffer[i] = -1.0f;
	}
}
