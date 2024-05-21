#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_ENTITY_ZENOBIAN_H__
#define __XR_ENTITY_ZENOBIAN_H__

#include "xr_entity.h"

namespace xray_re {

class se_turret_mgun: public cse_alife_helicopter {
public:
			se_turret_mgun();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
protected:
	float		m_health;
};

class se_anomaly_field: public cse_alife_space_restrictor {
public:
			se_anomaly_field();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
protected:
	bool		m_initialized;
};

} // end of namespace xray_re

#endif
