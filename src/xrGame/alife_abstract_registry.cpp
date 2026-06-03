#include "stdafx.h"
#include "alife_abstract_registry.h"

#include "game_news.h"
#include "encyclopedia_article_defs.h"
#include "relation_registry_defs.h"
#include "InfoPortionDefs.h"
#include "../xrCore/_stl_extensions.h"
#include "map_location_defs.h"
#include "map_location.h"
#include "GameTaskDefs.h"
#include "actor_statistic_defs.h"
#include "../xrCore/fastdelegate.h"
#include "../xrCore/Save/SaveObject.h"

namespace SaveSystemDefined {

	template<typename K, typename V>
	void Serialize2(ISaveObject& Object, std::pair<K, V>& Value);

	template<>
	void Serialize2<ALife::_OBJECT_ID, KNOWN_INFO_CONTAINER>(ISaveObject& Object, std::pair<ALife::_OBJECT_ID, KNOWN_INFO_CONTAINER>& Value)
	{
		BEGIN_CHUNK(Object,"Registry::u16+KNOWN_INFO_CONTAINER")
		{
			Object << Value.first << Value.second;
		}
	}

	template<>
	void Serialize2<ALife::_OBJECT_ID, RELATION_DATA>(ISaveObject& Object, std::pair<ALife::_OBJECT_ID, RELATION_DATA>& Value)
	{
		BEGIN_CHUNK(Object,"Registry::u16+RELATION_DATA")
		{
			Object << Value.first << Value.second;
		}
	}

	template<>
	void Serialize2<u16, ARTICLE_VECTOR>(ISaveObject& Object, std::pair<u16, ARTICLE_VECTOR>& Value)
	{
		BEGIN_CHUNK(Object,"Registry::u16+ARTICLE_VECTOR")
		{
			Object << Value.first << Value.second;
		}
	}

	template<>
	void Serialize2<u16, GAME_NEWS_VECTOR>(ISaveObject& Object, std::pair<u16, GAME_NEWS_VECTOR>& Value)
	{
		BEGIN_CHUNK(Object,"Registry::u16+GAME_NEWS_VECTOR")
		{
			Object << Value.first << Value.second;
		}
	}

	template<>
	void Serialize2<shared_str, int>(ISaveObject& Object, std::pair<shared_str, int>& Value)
	{
		BEGIN_CHUNK(Object,"Registry::shared_str+int")
		{
			Object << Value.first << Value.second;
		}
	}

	template<>
	void Serialize2<u16, Locations>(ISaveObject& Object, std::pair<u16, Locations>& Value)
	{
		BEGIN_CHUNK(Object,"Registry::u16+Locations")
		{
			Object << Value.first;
			u32 Size = 0;
			if (Object.IsSave())
			{
				for (auto& elem : Value.second)
				{
					if (elem.location->Serializable())
					{
						++Size;
					}
				}
			}
			Object << Size;
			BEGIN_ARRAY(Object)
			{
				if (Object.IsSave())
				{
					for (auto& elem : Value.second)
					{
						if (elem.location->Serializable())
						{
							Object << elem;
						}
					}
				} else
				{
					Value.second.resize(Size);
					for (u32 i = 0; i < Size; ++i)
					{
						Object << Value.second[i];
					}
				}
			}
		}
	}

	template<>
	void Serialize2<u16, vGameTasks>(ISaveObject& Object, std::pair<u16, vGameTasks>& Value)
	{
		BEGIN_CHUNK(Object,"Registry::u16+vGameTasks")
		{
			Object << Value.first << Value.second;
		}
	}

	template<>
	void Serialize2<u16, vStatSectionData>(ISaveObject& Object, std::pair<u16, vStatSectionData>& Value)
	{
		BEGIN_CHUNK(Object,"Registry::u16+vStatSectionData")
		{
			Object << Value.first << Value.second;
		}
	}

	template<>
	void Serialize<ALife::_OBJECT_ID, KNOWN_INFO_CONTAINER>(ISaveObject& Object, xr_map<ALife::_OBJECT_ID, KNOWN_INFO_CONTAINER>& Value)
	{
		((CSaveObject&)Object).Serialize(Value, fastdelegate::MakeDelegate(&SaveSystemDefined::Serialize2<ALife::_OBJECT_ID, KNOWN_INFO_CONTAINER>));
	}

	template<>
	void Serialize<ALife::_OBJECT_ID, RELATION_DATA>(ISaveObject& Object, xr_map<ALife::_OBJECT_ID, RELATION_DATA>& Value)
	{
		((CSaveObject&)Object).Serialize(Value, fastdelegate::MakeDelegate(&SaveSystemDefined::Serialize2<ALife::_OBJECT_ID, RELATION_DATA>));
	}

	template<>
	void Serialize<u16, ARTICLE_VECTOR>(ISaveObject& Object, xr_map<u16, ARTICLE_VECTOR>& Value)
	{
		((CSaveObject&)Object).Serialize(Value, fastdelegate::MakeDelegate(&SaveSystemDefined::Serialize2<u16, ARTICLE_VECTOR>));
	}

	template<>
	void Serialize<u16, GAME_NEWS_VECTOR>(ISaveObject& Object, xr_map<u16, GAME_NEWS_VECTOR>& Value)
	{
		((CSaveObject&)Object).Serialize(Value, fastdelegate::MakeDelegate(&SaveSystemDefined::Serialize2<u16, GAME_NEWS_VECTOR>));
	}

	template<>
	void Serialize<shared_str, int>(ISaveObject& Object, xr_map<shared_str, int>& Value)
	{
		((CSaveObject&)Object).Serialize(Value, fastdelegate::MakeDelegate(&SaveSystemDefined::Serialize2<shared_str, int>));
	}

	template<>
	void Serialize<u16, Locations>(ISaveObject& Object, xr_map<u16, Locations>& Value)
	{
		((CSaveObject&)Object).Serialize(Value, fastdelegate::MakeDelegate(&SaveSystemDefined::Serialize2<u16, Locations>));
	}

	template<>
	void Serialize<u16, vGameTasks>(ISaveObject& Object, xr_map<u16, vGameTasks>& Value)
	{
		((CSaveObject&)Object).Serialize(Value, fastdelegate::MakeDelegate(&SaveSystemDefined::Serialize2<u16, vGameTasks>));
	}

	template<>
	void Serialize<u16, vStatSectionData>(ISaveObject& Object, xr_map<u16, vStatSectionData>& Value)
	{
		((CSaveObject&)Object).Serialize(Value, fastdelegate::MakeDelegate(&SaveSystemDefined::Serialize2<u16, vStatSectionData>));
	}

};