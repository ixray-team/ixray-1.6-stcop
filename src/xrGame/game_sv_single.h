#pragma once

#include "game_sv_base.h"

class xrServer;

class game_sv_Single final : public game_sv_GameState
{
private:
	using inherited = game_sv_GameState;

public:
									game_sv_Single			();
	virtual							~game_sv_Single			();

	virtual		const char*				type_name				() const { return "single";};
	virtual		void				Create					(shared_str& options);
//	virtual		CSE_Abstract*		get_entity_from_eid		(u16 id);


	virtual		void				OnCreate				(u16 id_who);
	virtual		bool				OnTouch					(u16 eid_who, u16 eid_what, bool bForced = FALSE);
	virtual		void				OnDetach				(u16 eid_who, u16 eid_what);

	// Main
	virtual		void				Update					();
	virtual		void				SetGameTimeFactor		(const float fTimeFactor);

	virtual		ALife::_TIME_ID		GetEnvironmentGameTime	();
	virtual		float				GetEnvironmentGameTimeFactor		();
	virtual		void				SetEnvironmentGameTimeFactor		(const float fTimeFactor);

	virtual		bool				change_level			(NET_Packet &net_packet, ClientID sender);
	virtual		void				save_game				(NET_Packet &net_packet, ClientID sender);
	virtual		bool				load_game				(NET_Packet &net_packet, ClientID sender);
	virtual		void				reload_game				(NET_Packet &net_packet, ClientID sender);
	virtual		void				switch_distance			(NET_Packet &net_packet, ClientID sender);
	virtual		bool				CanHaveFriendlyFire		()	{return FALSE;}
	virtual		void				teleport_object			(NET_Packet &packet, u16 id);
	virtual		void				add_restriction			(NET_Packet &packet, u16 id);
	virtual		void				remove_restriction		(NET_Packet &packet, u16 id);
	virtual		void				remove_all_restrictions	(NET_Packet &packet, u16 id);
	virtual		bool				custom_sls_default		() {return !!m_alife_simulator;};
	virtual		void				sls_default				();
	virtual		shared_str			level_name				(const shared_str &server_options) const;
	virtual		void				on_death				(CSE_Abstract *e_dest, CSE_Abstract *e_src);
				void				restart_simulator		(const char* saved_game_name);

	IC			xrServer			&server					() const
	{
		VERIFY						(m_server);
		return						(*m_server);
	}

	virtual game_sv_Single* cast_game_sv_single() override { return this; }
};
