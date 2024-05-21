#pragma once

#include "xr_entity.h"

namespace xray_re {

class se_shelter: public cse_alife_space_restrictor {
public:
	se_shelter();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
};

class se_safe : public cse_alife_object_physic {
public:
	se_safe();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);

private:
	bool m_init_items_spawned;
	bool m_safe_locked;
	uint16_t m_count;
};

class cse_turret_mgun : public cse_alife_dynamic_object_visual {
public:
	cse_turret_mgun();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
};

} // end of namespace xray_re