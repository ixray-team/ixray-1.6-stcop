#pragma once

class CRandomSoundEmmiter
{
private:
	xr_vector<ref_sound> soundsArray;
	void Load(const char* section, const char* soundParameter);
public:
	esound_type sound_type = st_Effect;
	int	game_type = sg_SourceType;

	CRandomSoundEmmiter(const char* section, const char* soundParameter, esound_type _sound_type = st_Effect, int _game_type = sg_SourceType);
	~CRandomSoundEmmiter() = default;

	void Stop();
	bool IsPlaying();
	void UpdatePosition(const Fvector& pos);
	void UpdateVolume(float volume = 1.0f);
	void PlayRandomSound(CObject* O, const Fvector& pos, u32 flags, float delay, float volume = 1.0f);
};