#pragma once
#include "../xrEngine/Feel_Touch.h"
#include "HudSound.h"
#include "../xrSound/ai_sounds.h"
#include "Artefact.h"
#include "AnomalyZone.h"

struct ITEM_TYPE
{
	//min,max
	Fvector2 freq;
	HUD_SOUND_ITEM detect_snds;
};

//описание зоны, обнаруженной детектором
struct ITEM_INFO
{
	ITEM_TYPE* curr_ref = nullptr;
	float snd_time = 0.0f;
	//текущая частота работы датчика
	float cur_period = 0.0f;

	ITEM_INFO() = default;
	~ITEM_INFO() = default;
};

template <typename K>
class CDetectList : public Feel::Touch
{
protected:
	using TypesMap = xr_map<shared_str, ITEM_TYPE>;
	using TypesMapIt = typename TypesMap::iterator;
	TypesMap m_TypesMap;

public:
	using ItemsMap = xr_map<K*, ITEM_INFO>;
	using ItemsMapIt = typename ItemsMap::iterator;
	ItemsMap m_ItemInfos;

protected:
	void feel_touch_new(CObject* O) override
	{
		K* pK = smart_cast<K*>(O);
		R_ASSERT(pK);
		TypesMapIt it = m_TypesMap.find(O->cNameSect());
		R_ASSERT(it != m_TypesMap.end());
		m_ItemInfos[pK].snd_time = 0.0f;
		m_ItemInfos[pK].curr_ref = &(it->second);
	}

	void feel_touch_delete(CObject* O) override
	{
		K* pK = smart_cast<K*>(O);
		R_ASSERT(pK);
		m_ItemInfos.erase(pK);
	}
public:
	void destroy()
	{
		for (auto& it : m_TypesMap)
		{
			HUD_SOUND_ITEM::DestroySound(it.second.detect_snds);
		}
	}

	void clear()
	{
		m_ItemInfos.clear();
		Feel::Touch::feel_touch.clear();
	}

	void load(const char* sect, const char* prefix)
	{
		u32 i = 1;
		string256 temp = {};
		do {
			xr_sprintf(temp, "%s_class_%d", prefix, i);
			if (pSettings->line_exist(sect, temp))
			{
				shared_str item_sect = pSettings->r_string(sect, temp);

				m_TypesMap.insert(std::make_pair(item_sect, ITEM_TYPE()));
				ITEM_TYPE& item_type = m_TypesMap[item_sect];

				xr_sprintf(temp, "%s_freq_%d", prefix, i);
				item_type.freq = pSettings->r_fvector2(sect, temp);

				xr_sprintf(temp, "%s_sound_%d_", prefix, i);
				HUD_SOUND_ITEM::LoadSound(sect, temp, item_type.detect_snds, SOUND_TYPE_ITEM);

				++i;
			}
			else
				break;

		} while (true);
	}
};

class CAnomalyZone;

class CAfList final : public CDetectList<CArtefact>
{
protected:
	bool feel_touch_contact(CObject* O) override;
public:
	CAfList() = default;
	int m_af_rank = 0;
};

class CZoneList final : public CDetectList<CAnomalyZone>
{
protected:
	bool feel_touch_contact(CObject* O) override;
public:
	CZoneList() = default;
	virtual	~CZoneList();
};
