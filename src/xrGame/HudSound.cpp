#include "StdAfx.h"
#include "HudSound.h"

float HUD_SOUND_ITEM::g_fHudSndFrequency = 1.0f;
float HUD_SOUND_ITEM::g_fHudSndVolumeFactor = 1.0f;
float psHUDSoundVolume = 1.0f;
float psHUDStepSoundVolume = 1.0f;

void InitHudSoundSettings()
{
	psHUDSoundVolume = pSettings->r_float("hud_sound", "hud_sound_vol_k");
	psHUDStepSoundVolume = READ_IF_EXISTS(pSettings, r_float, "hud_sound", "hud_step_sound_vol_k", 1.0f);
}

void HUD_SOUND_ITEM::LoadSound(LPCSTR section, LPCSTR line, HUD_SOUND_ITEM& hud_snd, int type)
{
	hud_snd.m_activeSnd = nullptr;
	hud_snd.sounds.clear();

	string256 sound_line;
	xr_strcpy(sound_line, line);
	int k = 0;

	while (pSettings->line_exist(section, sound_line))
	{
		hud_snd.sounds.push_back(SSnd());
		SSnd& s = hud_snd.sounds.back();

		LoadSound(section, sound_line, s.snd, type, &s.volume, &s.delay);
		xr_sprintf(sound_line, "%s%d", line, ++k);
	}//while
}

void HUD_SOUND_ITEM::LoadSound(LPCSTR section, LPCSTR line, ref_sound& snd, int type, float* volume, float* delay)
{
	LPCSTR str = pSettings->r_string(section, line);
	string256 buf_str;

	int	count = _GetItemCount(str);
	R_ASSERT(count);

	_GetItem(str, 0, buf_str);
	snd.create(buf_str, st_Effect, type);

	if (volume != nullptr)
	{
		*volume = 1.f;
		if (count > 1)
		{
			_GetItem(str, 1, buf_str);
			if (xr_strlen(buf_str) > 0)
			{
				*volume = (float)atof(buf_str);
			}
		}
	}

	if (delay != nullptr)
	{
		*delay = 0;
		if (count > 2)
		{
			_GetItem(str, 2, buf_str);
			if (xr_strlen(buf_str) > 0)
			{
				*delay = (float)atof(buf_str);
			}
		}
	}
}

void HUD_SOUND_ITEM::DestroySound(HUD_SOUND_ITEM& hud_snd)
{
	for (SSnd& it : hud_snd.sounds)
	{
		it.snd.destroy();
	}

	hud_snd.sounds.clear();
	hud_snd.m_activeSnd = nullptr;
}

void HUD_SOUND_ITEM::PlaySound(HUD_SOUND_ITEM& hud_snd, const Fvector& position, const CObject* parent, bool b_hud_mode, bool looped, bool allowOverlap, u8 index)
{
	if (hud_snd.sounds.empty())
	{
		return;
	}

    if (hud_snd.m_b_exclusive)
	{
		StopSound(hud_snd);
	}

	u32 flags = b_hud_mode ? sm_2D : 0;

	if (looped)
	{
		flags |= sm_Looped;
	}

    hud_snd.m_activeSnd = &hud_snd.sounds[Random.randI(hud_snd.sounds.size())];

    float vol = hud_snd.m_activeSnd->volume * (b_hud_mode ? psHUDSoundVolume : 1.0f) * g_fHudSndVolumeFactor;

    Fvector pos = (flags & sm_2D) ? Fvector{} : position;

    if (!allowOverlap)
    {
		hud_snd.m_activeSnd->snd.play_at_pos(const_cast<CObject*>(parent), pos, flags, hud_snd.m_activeSnd->delay);
        hud_snd.m_activeSnd->snd.set_volume(vol);
        hud_snd.m_activeSnd->snd.set_frequency(g_fHudSndFrequency);
    }
	else
    {
        hud_snd.m_activeSnd->snd.play_no_feedback(const_cast<CObject*>(parent), flags, hud_snd.m_activeSnd->delay, &pos, &vol, &g_fHudSndFrequency);
    }
}

void HUD_SOUND_ITEM::StopSound(HUD_SOUND_ITEM& hud_snd)
{
	for (SSnd& it : hud_snd.sounds)
	{
		it.snd.stop();
	}

	hud_snd.m_activeSnd = nullptr;
}

HUD_SOUND_COLLECTION::HUD_SOUND_COLLECTION() : m_alias(nullptr)
{
	m_sound_items.clear();

	m_sound_items.resize(m_sound_items.size() + 1);
	HUD_SOUND_ITEM& snd_item = m_sound_items.back();
	snd_item.m_alias = "sndByMotion";
	snd_item.m_b_exclusive = false;
}

//----------------------------------------------------------
HUD_SOUND_COLLECTION::~HUD_SOUND_COLLECTION()
{
	for (HUD_SOUND_ITEM& it : m_sound_items)
	{
		HUD_SOUND_ITEM::StopSound(it);
		HUD_SOUND_ITEM::DestroySound(it);
	}

	m_sound_items.clear();
}

HUD_SOUND_ITEM* HUD_SOUND_COLLECTION::FindSoundItem(LPCSTR alias, bool b_assert)
{
	xr_vector<HUD_SOUND_ITEM>::iterator it = std::find(m_sound_items.begin(),m_sound_items.end(),alias);
	
	if (it != m_sound_items.end())
	{
		return &*it;
	}
    else
    {
		R_ASSERT3(!b_assert,"sound item not found in collection", alias);
        return nullptr;
	}
}

void HUD_SOUND_COLLECTION::PlaySound(HUD_SOUND_ITEM* snd_item, const Fvector& position, const CObject* parent, bool hud_mode, bool looped, bool allowOverlap, u8 index)
{
	for (HUD_SOUND_ITEM& Item : m_sound_items)
	{
		if (Item.m_b_exclusive)
		{
			HUD_SOUND_ITEM::StopSound(Item);
		}
	}

	HUD_SOUND_ITEM::PlaySound(*snd_item, position, parent, hud_mode, looped, allowOverlap, index);
}

void HUD_SOUND_COLLECTION::PlaySound(const char* alias, const Fvector& position, const CObject* parent, bool hud_mode, bool looped, bool allowOverlap, u8 index)
{
	HUD_SOUND_ITEM* snd_item = FindSoundItem(alias, false);
	if (snd_item != nullptr)
	{
		HUD_SOUND_ITEM::PlaySound(*snd_item, position, parent, hud_mode, looped, allowOverlap, index);
	}
}

void HUD_SOUND_COLLECTION::StopSound(LPCSTR alias)
{
	HUD_SOUND_ITEM* snd_item = FindSoundItem(alias, true);
	HUD_SOUND_ITEM::StopSound(*snd_item);
}

void HUD_SOUND_COLLECTION::SetPosition(LPCSTR alias, const Fvector& pos)
{
    HUD_SOUND_ITEM* snd_item = FindSoundItem(alias, false);
    if (snd_item != nullptr && snd_item->playing())
	{
		snd_item->set_position(pos);
	}
}

void HUD_SOUND_COLLECTION::StopAllSounds()
{
	for (HUD_SOUND_ITEM& it : m_sound_items)
	{
		HUD_SOUND_ITEM::StopSound(it);
	}
}

void HUD_SOUND_COLLECTION::LoadSound(LPCSTR section, LPCSTR line, LPCSTR alias, bool exclusive, int type)
{
	HUD_SOUND_ITEM& snd_item = *FindSoundItem(alias, false);
	if (&snd_item)
	{
		HUD_SOUND_ITEM::LoadSound(section, line, snd_item, type);
		snd_item.m_alias = alias;
		snd_item.m_b_exclusive = exclusive;
	}
	else
	{
		m_sound_items.resize(m_sound_items.size() + 1);
		HUD_SOUND_ITEM& snd_item_new = m_sound_items.back();
		HUD_SOUND_ITEM::LoadSound(section, line, snd_item_new, type);
		snd_item_new.m_alias = alias;
		snd_item_new.m_b_exclusive = exclusive;
	}
}

//Alundaio:
/*
It's usage is to play a group of sounds HUD_SOUND_ITEMs as if they were a single layered entity. This is a achieved by
wrapping the class around HUD_SOUND_COLLECTION and tagging them with the same alias. This way, when one for example
sndShot is played, it will play all the sound items with the same alias.
*/
//----------------------------------------------------------
HUD_SOUND_COLLECTION_LAYERED::~HUD_SOUND_COLLECTION_LAYERED()
{
	for (HUD_SOUND_COLLECTION& it : m_sound_items)
	{
		it.~HUD_SOUND_COLLECTION();
	}

	m_sound_items.clear();
}

void HUD_SOUND_COLLECTION_LAYERED::StopAllSounds()
{
	for (HUD_SOUND_COLLECTION& it : m_sound_items)
	{
		it.StopAllSounds();
	}
}

void HUD_SOUND_COLLECTION_LAYERED::StopSound(LPCSTR alias)
{
	for (HUD_SOUND_COLLECTION& it : m_sound_items)
	{
		if (it.m_alias == alias)
		{
			it.StopSound(alias);
		}
	}
}

void HUD_SOUND_COLLECTION_LAYERED::SetPosition(LPCSTR alias, const Fvector& pos)
{
	for (HUD_SOUND_COLLECTION& it : m_sound_items)
	{
		if (it.m_alias == alias)
		{
			it.SetPosition(alias, pos);
		}
	}
}

void HUD_SOUND_COLLECTION_LAYERED::PlaySound(LPCSTR alias, const Fvector& position, const CObject* parent, bool hud_mode, bool looped, bool allowOverlap, u8 index)
{
	for (HUD_SOUND_COLLECTION& it : m_sound_items)
	{
		if (it.m_alias == alias)
		{
			it.PlaySound(alias, position, parent, hud_mode, looped, allowOverlap, index);
		}
	}
}

HUD_SOUND_ITEM* HUD_SOUND_COLLECTION_LAYERED::FindSoundItem(LPCSTR alias, bool b_assert)
{
	for (HUD_SOUND_COLLECTION& it : m_sound_items)
	{
		if (it.m_alias == alias)
		{
			return it.FindSoundItem(alias, b_assert);
		}
	}

    return nullptr;
}

void HUD_SOUND_COLLECTION_LAYERED::LoadSound(LPCSTR section, LPCSTR line, LPCSTR alias, bool exclusive, int type)
{
	LPCSTR str = pSettings->r_string(section, line);
	string256 buf_str;

	int	count = _GetItemCount(str);
	R_ASSERT(count);

	_GetItem(str, 0, buf_str);
	
	if (pSettings->section_exist(buf_str))
	{
		string256 sound_line;
		xr_strcpy(sound_line, "snd_1_layer");
		int k = 1;

		while (pSettings->line_exist(buf_str, sound_line))
		{
			m_sound_items.resize(m_sound_items.size() + 1);
			HUD_SOUND_COLLECTION& snd_item = m_sound_items.back();
			snd_item.LoadSound(buf_str, sound_line, alias, exclusive, type);
			snd_item.m_alias = alias;
			xr_sprintf(sound_line, "snd_%d_layer", ++k);
		}
	}
	else //For compatibility with normal HUD_SOUND_COLLECTION sounds
	{
		m_sound_items.resize(m_sound_items.size() + 1);
		HUD_SOUND_COLLECTION& snd_item = m_sound_items.back();
		snd_item.LoadSound(section, line, alias, exclusive, type);
		snd_item.m_alias = alias;
	}
}

void HUD_SOUND_COLLECTION_LAYERED::LoadSound(CInifile const* ini, LPCSTR section, LPCSTR line, LPCSTR alias, bool exclusive, int type)
{
	LPCSTR str = ini->r_string(section, line);
	string256 buf_str;

	int	count = _GetItemCount(str);
	R_ASSERT(count);

	_GetItem(str, 0, buf_str);

	if (ini->section_exist(buf_str))
	{
		string256 sound_line;
		xr_strcpy(sound_line, "snd_1_layer");
		int k = 1;
		while (ini->line_exist(buf_str, sound_line))
		{
			m_sound_items.resize(m_sound_items.size() + 1);
			HUD_SOUND_COLLECTION& snd_item = m_sound_items.back();
			snd_item.LoadSound(buf_str, sound_line, alias, exclusive, type);
			snd_item.m_alias = alias;
			xr_sprintf(sound_line, "snd_%d_layer", ++k);
		}
	}
	else //For compatibility with normal HUD_SOUND_COLLECTION sounds
	{
		m_sound_items.resize(m_sound_items.size() + 1);
		HUD_SOUND_COLLECTION& snd_item = m_sound_items.back();
		snd_item.LoadSound(section, line, alias, exclusive, type);
		snd_item.m_alias = alias;
	}
}
//-Alundaio