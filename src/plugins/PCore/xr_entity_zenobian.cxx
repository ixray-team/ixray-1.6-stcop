#include "xr_entity_zenobian.h"
#include "xr_packet.h"

using namespace xray_re;

se_turret_mgun::se_turret_mgun(): m_health(0) {}

void se_turret_mgun::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_helicopter::state_read(packet, size);
	packet.r_float(m_health);
}

void se_turret_mgun::state_write(xr_packet& packet)
{
	cse_alife_helicopter::state_write(packet);
	packet.w_float(m_health);
}

////////////////////////////////////////////////////////////////////////////////

se_anomaly_field::se_anomaly_field(): m_initialized(false) {}

void se_anomaly_field::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_space_restrictor::state_read(packet, size);
	packet.r_bool(m_initialized);
	xr_assert(!m_initialized);
}

void se_anomaly_field::state_write(xr_packet& packet)
{
	cse_alife_space_restrictor::state_write(packet);
	packet.w_bool(m_initialized);
}
