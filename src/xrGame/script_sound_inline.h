////////////////////////////////////////////////////////////////////////////
//	Module 		: script_sound_inline.h
//	Created 	: 06.02.2004
//  Modified 	: 06.02.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script sound class inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

IC	u32	CScriptSound::Length				()
{
	return					iFloor(m_sound.get_length_sec()*1000.0f);
}

IC	void CScriptSound::Play					(CScriptGameObject *object)
{
	Play					(object,0.f,0);
}

IC	void CScriptSound::Play					(CScriptGameObject *object, float delay)
{
	Play					(object,delay,0);
}
		
IC	void CScriptSound::PlayAtPos			(CScriptGameObject *object, const Fvector &position)
{
	PlayAtPos				(object,position,0.f,0);
}

IC	void CScriptSound::PlayAtPos			(CScriptGameObject *object, const Fvector &position, float delay)
{
	PlayAtPos				(object,position,delay,0);
}

IC	void CScriptSound::SetMinDistance		(const float fMinDistance)
{
	m_sound.set_range(fMinDistance,GetMaxDistance());
}

IC	void CScriptSound::SetMaxDistance		(const float fMaxDistance)
{
	m_sound.set_range(GetMinDistance(),fMaxDistance);
}

IC	const float	CScriptSound::GetFrequency	() const
{
	return				(m_sound.get_params().freq);
}

IC	const float CScriptSound::GetMinDistance() const
{
	return				(m_sound.get_params().min_distance);
}

IC	const float CScriptSound::GetMaxDistance() const
{
	return				(m_sound.get_params().max_distance);
}

IC	const float	CScriptSound::GetVolume		() const
{
	return				(m_sound.get_params().volume);
}

IC	bool CScriptSound::IsPlaying			() const
{
	return				(m_sound.is_playing());
}

IC void CScriptSound::AttachTail(LPCSTR caSoundName)
{
	m_sound.attach_tail		(caSoundName);
}

IC	void CScriptSound::Stop					()
{
	m_sound.stop		();
}

IC	void CScriptSound::StopDeffered			()
{
	m_sound.stop_deffered();
}

IC	void CScriptSound::SetPosition			(const Fvector &position)
{
	m_sound.set_position(position);
}

IC	void CScriptSound::SetFrequency			(float frequency)
{
	m_sound.set_frequency(frequency);
}

IC	void CScriptSound::SetVolume			(float volume)
{
	m_sound.set_volume	(volume);
}

IC  CSound_params CScriptSound::GetParams	()
{
	return				(m_sound.get_params());
}

IC	void CScriptSound::SetParams			(CSound_params *sound_params)
{
	m_sound.set_params	(sound_params);
}
