#pragma once
//#include <mmreg.h>

#include "SoundRender_Cache.h"

#include <vorbis\vorbisfile.h>

#ifndef IXR_WINDOWS
struct WAVEFORMATEX
{
    uint16_t wFormatTag;       // формат (например, 1 = PCM)
    uint16_t nChannels;        // количество каналов
    uint32_t nSamplesPerSec;   // частота дискретизации
    uint32_t nAvgBytesPerSec;  // средняя скорость передачи данных
    uint16_t nBlockAlign;      // размер блока данных
    uint16_t wBitsPerSample;   // биты на сэмпл
    uint16_t cbSize;           // доп. размер структуры, обычно 0
};

constexpr uint16_t WAVE_FORMAT_PCM = 1;
#endif

class CSoundRender_Source
	: public CSound_source
{
public:
	shared_str				pname;
	shared_str				fname;
	cache_cat				CAT;

	float					fTimeTotal;
	u32						dwBytesTotal;

	WAVEFORMATEX			m_wformat;

	float					m_fBaseVolume;
	float					m_fMinDist;
	float					m_fMaxDist;
	float					m_fMaxAIDist;
	u32						m_uGameType;
private:
	OggVorbis_File			m_ovf;
	IReader*				m_wave;

	void 					i_decompress_fr			(OggVorbis_File* ovf, char* dest, u32 size);    
	void					LoadWave 				(LPCSTR name);
public:
							CSoundRender_Source		();
							~CSoundRender_Source	();

	void					load					(LPCSTR name);
    void					unload					();
	void					decompress				(u32 line, OggVorbis_File* ovf);
	
	virtual	float			length_sec				() const	{return fTimeTotal;}
	virtual u32				game_type				() const	{return m_uGameType;}
	virtual LPCSTR			file_name				() const	{return *fname;}
	virtual float			base_volume				() const	{return m_fBaseVolume;}
	virtual u16				channels_num			() const	{return m_wformat.nChannels;}
	virtual u32				bytes_total				() const	{return dwBytesTotal;}
};