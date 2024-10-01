#include "stdafx.h"
#include "game_sv_freemp.h"
#include "clsid_game.h"
#include "actor_mp_client.h"
#include "Inventory.h"

void game_sv_freemp::FillDeathActorRejectItems(CSE_ActorMP* actor, xr_vector<CSE_Abstract*>& to_reject)
{
	R_ASSERT(actor);
	CActor* pActor = smart_cast<CActor*>(Level().Objects.net_Find(actor->ID));

	VERIFY2(pActor, make_string<const char*>("Actor not found. actor_id = [%d]", actor->ID));
	if (!pActor) {
		Msg("! ERROR: Actor not found. actor_id = [%d]", actor->ID);
		return;
	}

	u16	active_slot = pActor->inventory().GetActiveSlot();
	if (active_slot == KNIFE_SLOT || active_slot == BOLT_SLOT || active_slot == BINOCULAR_SLOT)
	{
		active_slot = NO_ACTIVE_SLOT;
	}

	if (active_slot != NO_ACTIVE_SLOT)
	{
		PIItem item = pActor->inventory().ItemFromSlot(active_slot);
		if (!item)
		{
			return;
		}
		CSE_Abstract* server_item = m_server->ID_to_entity(item->object_id());
		if (!server_item)
		{
			return;
		}

		to_reject.push_back(server_item);
	}
}

BOOL game_sv_freemp::OnTouchPlayersBag(CSE_ActorMP* actor, CSE_Abstract* item)
{
	R_ASSERT(actor);
	R_ASSERT(item);

	if (item->ID_Parent != 0xffff)
	{
		return TRUE;
	}

	//move all items from rukzak to player

	if (!item->children.empty())
	{
		NET_Packet	EventPack;
		NET_Packet	PacketReject;
		NET_Packet	PacketTake;

		EventPack.w_begin(M_EVENT_PACK);

		while (!item->children.empty())
		{
			CSE_Abstract* e_child_item = get_entity_from_eid(item->children.back());
			if (e_child_item)
			{
				if (!OnTouch(actor->ID, e_child_item->ID, FALSE))
				{
					NET_Packet P;
					u_EventGen(P, GE_OWNERSHIP_REJECT, item->ID);
					P.w_u16(e_child_item->ID);

					m_server->Process_event_reject(P, m_server->GetServerClient()->ID, 0, item->ID, e_child_item->ID);
					continue;
				}
			}

			m_server->Perform_transfer(PacketReject, PacketTake, e_child_item, item, actor);

			EventPack.w_u8(u8(PacketReject.B.count));
			EventPack.w(&PacketReject.B.data, PacketReject.B.count);
			EventPack.w_u8(u8(PacketTake.B.count));
			EventPack.w(&PacketTake.B.data, PacketTake.B.count);
		}
		if (EventPack.B.count > 2)
			u_EventSend(EventPack);
	}

	//destroy the BAG
	DestroyGameItem(item);

	return FALSE;
}

void game_sv_freemp::OnDetachPlayersBag(CSE_ActorMP* actor, CSE_Abstract* item)
{
	R_ASSERT(actor);
	R_ASSERT(item);

	//move all items from player to rukzak
	xr_vector<u16>::const_iterator it_e = actor->children.end();

	xr_vector<CSE_Abstract*>			to_transfer;
	xr_vector<CSE_Abstract*>			to_destroy;
	xr_vector<CSE_Abstract*>			to_reject;

	// may be there is a sense to move next invokation into the ProcessDeath method...
	FillDeathActorRejectItems(actor, to_reject);

	for (auto it = actor->children.cbegin(); it != it_e; ++it)
	{
		u16 ItemID = *it;
		CSE_Abstract* e_item = get_entity_from_eid(ItemID);

		R_ASSERT(e_item->ID_Parent == actor->ID);

		if (std::find(to_reject.begin(), to_reject.end(), e_item) != to_reject.end())
			continue;

		if ((e_item->m_tClassID == CLSID_OBJECT_W_KNIFE) ||
			(e_item->m_tClassID == CLSID_DEVICE_TORCH) ||
			(e_item->m_tClassID == CLSID_IITEM_BOLT))
		{
			to_destroy.push_back(e_item);
		}
		else
		{
			if (!smart_cast<CSE_ALifeItemCustomOutfit*>(e_item) && e_item->m_tClassID != CLSID_OBJECT_PLAYERS_BAG)
			{
				to_transfer.push_back(e_item);
			}
		}
	}

	NET_Packet EventPack;
	NET_Packet PacketReject;
	NET_Packet PacketTake;
	EventPack.w_begin(M_EVENT_PACK);

	for (auto it = to_transfer.cbegin(); it != to_transfer.cend(); ++it)
	{
		m_server->Perform_transfer(PacketReject, PacketTake, *it, actor, item);
		EventPack.w_u8(u8(PacketReject.B.count));
		EventPack.w(&PacketReject.B.data, PacketReject.B.count);
		EventPack.w_u8(u8(PacketTake.B.count));
		EventPack.w(&PacketTake.B.data, PacketTake.B.count);
	}

	if (EventPack.B.count > 2)
		u_EventSend(EventPack);

	for (const auto& el : to_destroy)
		DestroyGameItem(el);

	for (const auto& el : to_reject)
		RejectGameItem(el);
}