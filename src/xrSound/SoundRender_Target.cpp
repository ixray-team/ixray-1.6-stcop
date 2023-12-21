#include "stdafx.h"
#pragma hdrstop

#include "SoundRender_Target.h"
#include "SoundRender_Core.h"
#include "SoundRender_Emitter.h"
#include "SoundRender_Source.h"
#include "ogg_utils.h"

CSoundRender_Target::CSoundRender_Target()
{
	m_pEmitter		= 0;
	rendering		= FALSE;
	m_wave			= 0;
}

CSoundRender_Target::~CSoundRender_Target()
{
	VERIFY				(m_wave == 0);
}

BOOL CSoundRender_Target::_initialize()
{
    return					TRUE;
}

void	CSoundRender_Target::start(CSoundRender_Emitter* E)
{
	R_ASSERT		(E);

	// *** Initial buffer startup ***
	// 1. Fill parameters
	// 4. Load 2 blocks of data (as much as possible)
	// 5. Deferred-play-signal (emitter-exist, rendering-false)
	m_pEmitter		= E;
	rendering		= FALSE;
	attach			();
}

void	CSoundRender_Target::render			()
{
	rendering		= TRUE;
}

void	CSoundRender_Target::stop			()
{
	dettach			();
	m_pEmitter		= NULL;
	rendering		= FALSE;
}

void	CSoundRender_Target::rewind			()
{
	R_ASSERT		(rendering);
}

void	CSoundRender_Target::update			()
{
	R_ASSERT		(m_pEmitter);
}

void	CSoundRender_Target::fill_parameters()
{
	VERIFY			(m_pEmitter);
}

void	CSoundRender_Target::attach()
{
	VERIFY			(m_pEmitter);
	m_wave			= FS.r_open		(m_pEmitter->source()->pname.c_str()); 
	R_ASSERT3		(m_wave && m_wave->length(),"Can't open wave file:", m_pEmitter->source()->pname.c_str());

	ov_callbacks		ovc;
	ovc.read_func		= ov_read_func;
	ovc.seek_func		= ov_seek_func;
	ovc.close_func		= ov_close_func;
	ovc.tell_func		= ov_tell_func;
	ov_open_callbacks	(m_wave, &m_ovf, NULL, 0, ovc);
}

void	CSoundRender_Target::dettach()
{
	if (m_wave)		{
		ov_clear		(&m_ovf);
		FS.r_close		(m_wave);
	}
}

