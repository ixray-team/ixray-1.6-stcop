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
#include "character_info_defs.h"
#include "relation_registry_defs.h"


#include "GameTaskDefs.h"
#include "game_news.h"
#include "map_location_defs.h"

#include "actor_statistic_defs.h"

using CInfoPortionRegistry = CALifeAbstractRegistry<u16, KNOWN_INFO_VECTOR>;
using CRelationRegistry = CALifeAbstractRegistry<u16, RELATION_DATA>;
using CGameNewsRegistry = CALifeAbstractRegistry<u16, GAME_NEWS_VECTOR>;
using CSpecificCharacterRegistry = CALifeAbstractRegistry<shared_str, int>;

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

    template <typename T>
    T& get();

    template <typename T>
    const T& get() const;

    void load(IReader& file_stream);
    void save(IWriter& memory_stream);
};

#include "alife_registry_container_inline.h"