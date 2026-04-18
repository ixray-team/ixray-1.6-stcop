#pragma once


struct HUD_SOUND_ITEM
{
    HUD_SOUND_ITEM() : m_activeSnd(NULL), m_b_exclusive(false)
    {
        m_alias = "";
    }

    static void LoadSound(const char* section, const char* line, ref_sound& hud_snd, int type = sg_SourceType, float* volume = nullptr, float* delay = nullptr);

    static void LoadSound(const char* section, const char* line, HUD_SOUND_ITEM& hud_snd, int type = sg_SourceType);

	static void DestroySound(HUD_SOUND_ITEM& hud_snd);

    static void PlaySound(HUD_SOUND_ITEM& snd, const Fvector& position, const CObject* parent, bool hud_mode, bool looped = false, bool allowOverlap = false, u8 index = u8(-1));

	static void	StopSound(HUD_SOUND_ITEM& snd);

	ICF bool playing()
	{
        if (m_activeSnd)
            return m_activeSnd->snd.is_playing();
		else
            return false;
	}

    ICF void set_position(const Fvector& pos)
    {
        if (m_activeSnd)
        {
            if (m_activeSnd->snd._feedback() && !m_activeSnd->snd._feedback()->is_2D())
                m_activeSnd->snd.set_position(pos);
            else
                m_activeSnd = nullptr;
        }
    }

    static float g_fHudSndFrequency;
    ICF static void SetHudSndGlobalFrequency(const float& fFreq)
    {
        // SM_TODO: Bad for parallelization
        g_fHudSndFrequency = fFreq;
		}

    static float g_fHudSndVolumeFactor;
    ICF static void SetHudSndGlobalVolumeFactor(const float& fVolume)
    {
        // SM_TODO: Bad for parallelization
        g_fHudSndVolumeFactor = fVolume;
	}

    struct SSnd
    {
		ref_sound	snd;
		float		delay;		//задержка перед проигрыванием
		float		volume;		//громкость
	};
	xr_string		m_alias;
	SSnd*			m_activeSnd;
	bool			m_b_exclusive;
	xr_vector<SSnd> sounds;

	bool operator == (const char* alias) const{return 0==_stricmp(m_alias.c_str(),alias);}
};

class HUD_SOUND_COLLECTION
{
    // xr_vector<HUD_SOUND_ITEM>	m_sound_items;
    // HUD_SOUND_ITEM*				FindSoundItem	(	const char* alias, bool b_assert);
public:
	xr_string m_alias; // Alundaio: For use when it's part of a layered Collection
	~HUD_SOUND_COLLECTION();

    void Clear();
	
    HUD_SOUND_COLLECTION();

    xr_vector<HUD_SOUND_ITEM> m_sound_items; // Alundaio: made public

    HUD_SOUND_ITEM* FindSoundItem(const char* alias, bool b_assert); // AVO: made public to check if sound is loaded
    void PlaySound(HUD_SOUND_ITEM* snd_item, const Fvector& position, const CObject* parent, bool hud_mode, bool looped, bool allowOverlap, u8 index);
    void PlaySound(const char* alias, const Fvector& position, const CObject* parent, bool hud_mode, bool looped = false, bool allowOverlap = false, u8 index = u8(-1));

	void						StopSound		(	const char* alias);

    void LoadSound(const char* section, const char* line, const char* alias, bool exclusive = false, int type = sg_SourceType);

	void						SetPosition		(	const char* alias, 	const Fvector& pos);
	void						StopAllSounds	();
};

//Alundaio:
class HUD_SOUND_COLLECTION_LAYERED
{
	xr_vector<HUD_SOUND_COLLECTION>	m_sound_items;
public:
	~HUD_SOUND_COLLECTION_LAYERED();
    HUD_SOUND_ITEM* FindSoundItem(const char* alias, bool b_assert);
    void PlaySound(const char* alias, const Fvector& position, const CObject* parent, bool hud_mode, bool looped = false, bool allowOverlap = false, u8 index = u8(-1));
    void StopSound(const char* alias);
    void StopAllSounds();
    void LoadSound(const char* section, const char* line, const char* alias, bool exclusive = false, int type = sg_SourceType);
    void LoadSound(CInifile const* ini, const char* section, const char* line, const char* alias, bool exclusive = false, int type = sg_SourceType);
    void SetPosition(const char* alias, const Fvector& pos);
};
//-Alundaio