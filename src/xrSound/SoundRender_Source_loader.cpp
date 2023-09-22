#include "stdafx.h"
#pragma hdrstop

#include <msacm.h>

#include "SoundRender_Core.h"
#include "SoundRender_Source.h"
#include "ogg_utils.h"

void CSoundRender_Source::decompress(u32 line, OggVorbis_File* ovf)
{
	VERIFY	(ovf);
	// decompression of one cache-line
	u32		line_size		= SoundRender->cache.get_linesize();
	char*	dest			= (char*)SoundRender->cache.get_dataptr	(CAT,line);
	u32		buf_offs		= (line*line_size) / 2 / m_wformat.nChannels;
	u32		left_file		= dwBytesTotal - buf_offs;
	u32		left			= (u32)_min	(left_file,line_size);

	// seek
	u32	cur_pos				= u32(ov_pcm_tell(ovf));
	if (cur_pos!=buf_offs)
		ov_pcm_seek			(ovf,buf_offs);

	// decompress
	i_decompress_fr(ovf,dest,left);
}

void CSoundRender_Source::LoadWave	(LPCSTR pName)
{
	pname					= pName;
	ZeroMemory				(&m_wformat, sizeof(WAVEFORMATEX));

	// Load file into memory and parse WAV-format
	m_wave					= FS.r_open(pname.c_str());
	R_ASSERT3				(m_wave && m_wave->length(),"Can't open wave file:",pname.c_str());

	ov_callbacks			ovc;
	ovc.read_func			= ov_read_func;
	ovc.seek_func			= ov_seek_func;
	ovc.close_func			= ov_close_func;
	ovc.tell_func			= ov_tell_func;
	ov_open_callbacks		(m_wave, &m_ovf, NULL, 0, ovc);

	vorbis_info* ovi		= ov_info(&m_ovf, -1);
	// verify
	R_ASSERT3				(ovi, "Invalid source info:", pname.c_str());
	R_ASSERT3				(ovi->rate==44100, "Invalid source rate:", pname.c_str());

#ifdef DEBUG
	if(ovi->channels==2)
	{
		Msg("stereo sound source [%s]", pname.c_str());
	}
#endif // #ifdef DEBUG

	m_wformat.nSamplesPerSec	= (ovi->rate); //44100;
	m_wformat.wFormatTag		= WAVE_FORMAT_PCM;
	m_wformat.nChannels			= u16(ovi->channels);
	m_wformat.wBitsPerSample	= 16;

	m_wformat.nBlockAlign		= (m_wformat.nChannels * m_wformat.wBitsPerSample) / 8;
	m_wformat.nAvgBytesPerSec	= m_wformat.nSamplesPerSec * m_wformat.nBlockAlign;

	s64 pcm_total				= ov_pcm_total(&m_ovf,-1);
	dwBytesTotal				= u32(pcm_total*m_wformat.nBlockAlign); 
	fTimeTotal					= s_f_def_source_footer + dwBytesTotal/float(m_wformat.nAvgBytesPerSec);

	vorbis_comment*	ovm		= ov_comment(&m_ovf,-1);
	if (ovm->comments)
	{
		IReader F			(ovm->user_comments[0],ovm->comment_lengths[0]);
		u32 vers			= F.r_u32	();
        if (vers==0x0001){
			m_fMinDist		= F.r_float	();
			m_fMaxDist		= F.r_float	();
	        m_fBaseVolume	= 1.0f;
			m_uGameType		= F.r_u32	();
			m_fMaxAIDist	= m_fMaxDist;
		}else if (vers==0x0002){
			m_fMinDist		= F.r_float	();
			m_fMaxDist		= F.r_float	();
			m_fBaseVolume	= F.r_float	();
			m_uGameType		= F.r_u32	();
			m_fMaxAIDist	= m_fMaxDist;
		}else if (vers==OGG_COMMENT_VERSION){
			m_fMinDist		= F.r_float	();
			m_fMaxDist		= F.r_float	();
            m_fBaseVolume	= F.r_float	();
			m_uGameType		= F.r_u32	();
			m_fMaxAIDist	= F.r_float	();
		}else{
			Log				("! Invalid ogg-comment version, file: ", pname.c_str());
		}
	}else{
		Log					("! Missing ogg-comment, file: ", pname.c_str());
	}
	R_ASSERT3((m_fMaxAIDist>=0.1f)&&(m_fMaxDist>=0.1f),"Invalid max distance.", pname.c_str());
}

void CSoundRender_Source::load(LPCSTR name)
{
	string_path			fn,N;
	xr_strcpy				(N,name);
	_strlwr				(N);
	if (strext(N))		*strext(N) = 0;

	fname				= N;

	strconcat			(sizeof(fn),fn,N,".ogg");
	if (!FS.exist("$level$",fn))	FS.update_path	(fn,"$game_sounds$",fn);

	if (!FS.exist(fn)) {
		FS.update_path(fn,"$game_sounds$", "$no_sound.ogg");
		Msg("! Can't find sound '%s'", fname.c_str());
	}

	LoadWave			(fn);
	SoundRender->cache.cat_create	(CAT, dwBytesTotal);
}

void CSoundRender_Source::unload()
{
	SoundRender->cache.cat_destroy	(CAT);
    fTimeTotal						= 0.0f;
    dwBytesTotal					= 0;

	if (m_wave) {
		FS.r_close(m_wave);
	}
}

