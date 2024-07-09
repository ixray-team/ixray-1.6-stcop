////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_monster_base.cpp
//	Created 	: 07.02.2007
//  Modified 	: 07.02.2007
//	Author		: Dmitriy Iassenev
//	Description : ALife mnster base class
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "alife_simulator.h"
#include "xrServer.h"
#include "alife_monster_brain.h"

void CSE_ALifeMonsterBase::on_spawn				()
{
    inherited1::on_spawn();

    if (!pSettings->line_exist(s_name, "Spawn_Inventory_Item_Section"))
        return;

    LPCSTR item_sections = pSettings->r_string(s_name, "Spawn_Inventory_Item_Section");
    LPCSTR item_probabilities = pSettings->r_string(s_name, "Spawn_Inventory_Item_Probability");

    xr_vector<float> probabilities;
    xr_vector<shared_str> sections;

    string128 buf;
    int count = _GetItemCount(item_sections);

    for (int i = 0; i < count; ++i) 
    {
        sections.push_back(_GetItem(item_sections, i, buf));
        probabilities.push_back(static_cast<float>(atof(_GetItem(item_probabilities, i, buf))));
    }

    for (size_t i = 0; i < sections.size(); ++i) 
    {
        float probability = randF();

        if ((probability >= probabilities[i]) && !fsimilar(probabilities[i], 1.f))
            continue;

        alife().spawn_item(*sections[i], o_Position, m_tNodeID, m_tGraphID, ID)->ID_Parent = ID;
    }
}

extern void add_online_impl		(CSE_ALifeDynamicObject *object, const bool &update_registries);

void CSE_ALifeMonsterBase::add_online			(const bool &update_registries)
{
	add_online_impl				(this,update_registries);
	brain().on_switch_online	();
}

extern void add_offline_impl	(CSE_ALifeDynamicObject *object, const xr_vector<ALife::_OBJECT_ID> &saved_children, const bool &update_registries);

void CSE_ALifeMonsterBase::add_offline			(const xr_vector<ALife::_OBJECT_ID> &saved_children, const bool &update_registries)
{
	add_offline_impl			(this,saved_children,update_registries);
	brain().on_switch_offline	();
}
