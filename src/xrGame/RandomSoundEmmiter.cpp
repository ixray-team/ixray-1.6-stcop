#include "stdafx.h"
#include "RandomSoundEmmiter.h"


CRandomSoundEmmiter::CRandomSoundEmmiter(const char* section, const char* soundParameter, esound_type _sound_type, int _game_type)
{
	sound_type = _sound_type;
	game_type = _game_type;
	Load(section, soundParameter);
}

void CRandomSoundEmmiter::Load(const char* section, const char* soundParameter)
{
	if (pSettings->line_exist(section, soundParameter))
	{
		xr_string unsplittedPaths = pSettings->r_string(section, soundParameter);
		if (!unsplittedPaths.empty())
		{
			xr_vector<xr_string> paths = unsplittedPaths.RemoveWhitespaces().Split();
			for (xr_string& sound_path : paths)
			{
				soundsArray.emplace_back().create(sound_path.c_str(), sound_type, game_type);
			}
		}
	}
}

void CRandomSoundEmmiter::Stop()
{
	for (ref_sound& sound : soundsArray)
	{
		if (sound.handle() && sound.is_playing())
		{
			sound.stop();
		}
	}
}

bool CRandomSoundEmmiter::IsPlaying()
{
	for (ref_sound& sound : soundsArray)
	{
		if (sound.handle() && sound.is_playing())
		{
			return true;
		}
	}

	return false;
}

void CRandomSoundEmmiter::UpdatePosition(const Fvector& pos)
{
	for (ref_sound& sound : soundsArray)
	{
		if (sound.handle() &&sound.is_playing() && sound.slot()) {
			sound.set_position(pos);
		}
	}
}

void CRandomSoundEmmiter::UpdateVolume(float volume)
{
	for (ref_sound& sound : soundsArray)
	{
		if (sound.handle())
		{
			sound.set_volume(volume);
		}
	}
}

void CRandomSoundEmmiter::PlayRandomSound(CObject* O, const Fvector& pos, u32 flags, float delay, float volume)
{
	if (!soundsArray.empty())
	{
		ref_sound snd = soundsArray[::Random.randI(soundsArray.size() -1)];
		if (snd.handle())
		{
			snd.play_at_pos(O, pos, flags, delay);
			snd.set_volume(volume);
		}
	}
}