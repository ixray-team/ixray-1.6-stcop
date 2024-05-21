#include "xr_entity_la.h"
#include "xr_packet.h"

using namespace xray_re;

se_shelter::se_shelter() {}

void se_shelter::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_space_restrictor::state_read(packet, size);
}

void se_shelter::state_write(xr_packet& packet)
{
	cse_alife_space_restrictor::state_write(packet);
}

se_safe::se_safe(): m_init_items_spawned(false), m_safe_locked(false), m_count(0) {}

void se_safe::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_object_physic::state_read(packet, size);
	m_init_items_spawned = packet.r_bool();
	m_safe_locked = packet.r_bool();
	m_count = packet.r_u16();
	xr_assert(m_count == 0);
}

void se_safe::state_write(xr_packet& packet)
{
	cse_alife_object_physic::state_write(packet);
	packet.w_bool(m_init_items_spawned);
	packet.w_bool(m_safe_locked);
	packet.w_u16(m_count);
}

cse_turret_mgun::cse_turret_mgun() {}

void cse_turret_mgun::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
}

void cse_turret_mgun::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
}