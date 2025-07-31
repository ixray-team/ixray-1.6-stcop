////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_registry_container_inline.h
//	Created 	: 01.07.2004
//  Modified 	: 01.07.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife registry container class inline functions
////////////////////////////////////////////////////////////////////////////
#pragma once

template <typename T>
T& CALifeRegistryContainer::get()
{
    if constexpr (std::is_same_v<T, CInfoPortionRegistry>)
        return m_info_portions;
    else if constexpr (std::is_same_v<T, CRelationRegistry>)
        return m_character_relations;
    else if constexpr (std::is_same_v<T, CGameNewsRegistry>)
        return m_game_news;
    else if constexpr (std::is_same_v<T, CSpecificCharacterRegistry>)
        return m_specific_characters;
    else if constexpr (std::is_same_v<T, CMapLocationRegistry>)
        return m_map_locations;
    else if constexpr (std::is_same_v<T, CGameTaskRegistry>)
        return m_game_tasks;
    else if constexpr (std::is_same_v<T, CActorStatisticRegistry>)
        return m_actor_statistics;
    else
    {
        VERIFY(!"no entry");
        return {};//static_assert(false, "Unsupported registry type");
    }
}

template <typename T>
const T& CALifeRegistryContainer::get() const
{
    if constexpr (std::is_same_v<T, CInfoPortionRegistry>)
        return m_info_portions;
    else if constexpr (std::is_same_v<T, CRelationRegistry>)
        return m_character_relations;
    else if constexpr (std::is_same_v<T, CGameNewsRegistry>)
        return m_game_news;
    else if constexpr (std::is_same_v<T, CSpecificCharacterRegistry>)
        return m_specific_characters;
    else if constexpr (std::is_same_v<T, CMapLocationRegistry>)
        return m_map_locations;
    else if constexpr (std::is_same_v<T, CGameTaskRegistry>)
        return m_game_tasks;
    else if constexpr (std::is_same_v<T, CActorStatisticRegistry>)
        return m_actor_statistics;
    else
    {
        VERIFY(!"no entry");
        return {};//static_assert(false, "Unsupported registry type");
    }
}
