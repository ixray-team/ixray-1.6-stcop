////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_monster_base.cpp
//	Created 	: 07.02.2007
//  Modified 	: 07.02.2007
//	Author		: Dmitriy Iassenev
//	Description : ALife mnster base class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "alife_simulator.h"
#include "xrServer.h"
#include "alife_monster_brain.h"
#include "ai_space.h"
#include "alife_object_registry.h"
#include "alife_graph_registry.h"
#include "specific_character.h"
#include "alife_schedule_registry.h"

void CSE_ALifeMonsterBase::on_spawn				()
{
    inherited1::on_spawn();

    if (!pSettings->line_exist(s_name, "Spawn_Inventory_Item_Section"))
        return;

    const char* item_sections = pSettings->r_string(s_name, "Spawn_Inventory_Item_Section");
    const char* item_probabilities = pSettings->r_string(s_name, "Spawn_Inventory_Item_Probability");

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

void CSE_ALifeMonsterBase::add_online			(const bool &update_registries)
{
	CSE_ALifeDynamicObject* object = smart_cast<CSE_ALifeDynamicObject*>(this);
	VERIFY(object);

	NET_Packet tNetPacket;
	ClientID clientID;
	clientID.set(object->alife().server().GetServerClient() ? object->alife().server().GetServerClient()->ID.value() : 0);

	for (auto ID : object->children)
	{
		// Alundaio:
		if (ID == ai().alife().graph().actor()->ID)
		{
			continue;
		}
		//-Alundaio

		CSE_ALifeDynamicObject* l_tpALifeDynamicObject = ai().alife().objects().object(ID);
		if (!l_tpALifeDynamicObject)
		{
			continue;
		}

		CSE_ALifeInventoryItem* l_tpALifeInventoryItem = smart_cast<CSE_ALifeInventoryItem*>(l_tpALifeDynamicObject);
		if (!l_tpALifeInventoryItem)
		{
			continue;
		}

		// R_ASSERT2				(l_tpALifeInventoryItem,"Non inventory item object has parent?!");

		l_tpALifeInventoryItem->base()->s_flags.bor(M_SPAWN_UPDATE);
		CSE_Abstract* l_tpAbstract = smart_cast<CSE_Abstract*>(l_tpALifeInventoryItem);
		object->alife().server().entity_Destroy(l_tpAbstract);

#ifdef DEBUG
		//		if (psAI_Flags.test(aiALife))
		//			Msg					("[LSS] Spawning item [%s][%s][%d]",l_tpALifeInventoryItem->base()->name_replace(),*l_tpALifeInventoryItem->base()->s_name,l_tpALifeDynamicObject->ID);
		Msg(
			"[LSS][%d] Going online [%d][%s][%d] with parent [%d][%s] on '%s'",
			Device.dwFrame,
			Device.dwTimeGlobal,
			l_tpALifeInventoryItem->base()->name_replace(),
			l_tpALifeInventoryItem->base()->ID,
			object->ID,
			object->name_replace(),
			"*SERVER*"
		);
#endif

		//		R_ASSERT3								(ai().level_graph().valid_vertex_id(l_tpALifeDynamicObject->m_tNodeID),"Invalid vertex for object ",l_tpALifeInventoryItem->name_replace());
		l_tpALifeDynamicObject->o_Position = object->o_Position;
		l_tpALifeDynamicObject->m_tNodeID = object->m_tNodeID;
		object->alife().server().Process_spawn(tNetPacket, clientID, false, l_tpALifeInventoryItem->base());
		l_tpALifeDynamicObject->s_flags.band(u16(-1) ^ M_SPAWN_UPDATE);
		l_tpALifeDynamicObject->m_bOnline = true;
		if (!l_tpALifeDynamicObject->children.empty())
		{
			l_tpALifeDynamicObject->add_online(update_registries); // thx hozar2002
		}
	}

	if (!update_registries)
	{
		return;
	}

	object->alife().scheduled().remove(object);
	object->alife().graph().remove(object, object->m_tGraphID, false);
	brain().on_switch_online	();
}

void CSE_ALifeMonsterBase::add_offline			(const xr_vector<ALife::_OBJECT_ID> &saved_children, const bool &update_registries)
{
	CSE_ALifeDynamicObject* object = smart_cast<CSE_ALifeDynamicObject*>(this);
	VERIFY(object);
	for (u32 i = 0, n = (u32)saved_children.size(); i < n; ++i)
	{
		CSE_ALifeDynamicObject* child = smart_cast<CSE_ALifeDynamicObject*>(ai().alife().objects().object(saved_children[i], true));
		R_ASSERT(child);
		child->m_bOnline = false;

		CSE_ALifeInventoryItem* inventory_item = smart_cast<CSE_ALifeInventoryItem*>(child);
		VERIFY2(inventory_item, "Non inventory item object has parent?!");
		if (!child->children.empty())
		{
			child->add_offline(child->children, update_registries); // thx hozar2002
		}
#ifdef DEBUG
		//		if (psAI_Flags.test(aiALife))
		//			Msg					("[LSS] Destroying item [%s][%s][%d]",inventory_item->base()->name_replace(),*inventory_item->base()->s_name,inventory_item->base()->ID);
		Msg(
			"[LSS][%d] Going offline [%d][%s][%d] with parent [%d][%s] on '%s'",
			Device.dwFrame,
			Device.dwTimeGlobal,
			inventory_item->base()->name_replace(),
			inventory_item->base()->ID,
			object->ID,
			object->name_replace(),
			"*SERVER*"
		);
#endif

		ALife::_OBJECT_ID item_id = inventory_item->base()->ID;
		inventory_item->base()->ID = object->alife().server().PerformIDgen(item_id);

		if (!child->can_save())
		{
			object->alife().release(child);
			--i;
			--n;
			continue;
		}

		child->clear_client_data();
		object->alife().graph().add(child, child->m_tGraphID, false);
		object->alife().graph().attach(*object, inventory_item, child->m_tGraphID, true);
	}

	if (!update_registries)
	{
		return;
	}

	object->alife().scheduled().add(object);
	object->alife().graph().add(object, object->m_tGraphID, false);
	brain().on_switch_offline	();
}
