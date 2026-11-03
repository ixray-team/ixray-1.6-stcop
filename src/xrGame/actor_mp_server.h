#pragma once

#include "xrServer_Objects_ALife_Monsters.h"
#include "actor_mp_state.h"

class CSE_ActorMP :
	public CSE_ALifeCreatureActor 
{
private:
	typedef CSE_ALifeCreatureActor	inherited;

private:
	actor_mp_state_holder	m_state_holder;
	bool					m_ready_to_update;

private:
			void			fill_state		(actor_mp_state &state);

public:
							CSE_ActorMP		(const char*		section);
	virtual void 			UPDATE_Read		(NET_Packet &packet) override;
	virtual void 			UPDATE_Write	(NET_Packet &packet) override;
	virtual void 			STATE_Read		(NET_Packet &packet, u16 size) override;
	virtual void 			STATE_Write		(NET_Packet &packet) override;
	virtual bool			Net_Relevant	() override;

#ifdef XRGAME_EXPORTS
	virtual void			on_death				(CSE_Abstract *killer) override;	
#endif
};