////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_registry_container.h
//	Created 	: 01.07.2004
//  Modified 	: 01.07.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife registry container class
////////////////////////////////////////////////////////////////////////////
#pragma once
#include "alife_abstract_registry.h"

#include "InfoPortionDefs.h"
#include "PdaMsg.h"
#include "encyclopedia_article_defs.h"
#include "alife_abstract_registry.h"
#include "relation_registry_defs.h"


#include "GameTaskDefs.h"
#include "game_news.h"
#include "map_location_defs.h"

#include "actor_statistic_defs.h"

using CInfoPortionRegistry = CALifeAbstractRegistry<ALife::_OBJECT_ID, KNOWN_INFO_CONTAINER>;
using CRelationRegistry = CALifeAbstractRegistry<ALife::_OBJECT_ID, RELATION_DATA>;
using CGameNewsRegistry = CALifeAbstractRegistry<u16, GAME_NEWS_VECTOR>;
using CSpecificCharacterRegistry = CALifeAbstractRegistry<shared_str, int>;
using CEncyclopediaRegistry = CALifeAbstractRegistry<u16, ARTICLE_VECTOR>;

class CALifeRegistryContainer
{
public:
    CInfoPortionRegistry       m_info_portions;
    CRelationRegistry          m_character_relations;
    CGameNewsRegistry          m_game_news;
    CSpecificCharacterRegistry m_specific_characters;
    CMapLocationRegistry       m_map_locations;
    CGameTaskRegistry          m_game_tasks;
    CActorStatisticRegistry    m_actor_statistics;
    CEncyclopediaRegistry      m_encyclopedia_registry;

    template <typename T>
    T& get();

    template <typename T>
    const T& get() const;

    void load(IReader& file_stream);
    void save(IWriter& memory_stream);
    void Serialize(ISaveObject& Object);
};

#include "alife_registry_container_inline.h"