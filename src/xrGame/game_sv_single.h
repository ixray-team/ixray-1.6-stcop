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

	const char*				type_name				() const override { return "single";};
	void				Create					(shared_str& options) override;
//	virtual		CSE_Abstract*		get_entity_from_eid		(u16 id);


	void				OnCreate				(ALife::_OBJECT_ID id_who) override;
	bool				OnTouch					(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_what, bool bForced = false) override;
	void				OnDetach				(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_what) override;

	// Main
	void				Update					() override;
	void				SetGameTimeFactor		(const float fTimeFactor) override;

	ALife::_TIME_ID		GetEnvironmentGameTime	() override;
	float				GetEnvironmentGameTimeFactor		() override;
	void				SetEnvironmentGameTimeFactor		(const float fTimeFactor) override;

	bool				change_level			(NET_Packet &net_packet, ClientID sender) override;
	void				save_game				(NET_Packet &net_packet, ClientID sender) override;
	bool				load_game				(NET_Packet &net_packet, ClientID sender) override;
	void				reload_game				(NET_Packet &net_packet, ClientID sender) override;
	void				switch_distance			(NET_Packet &net_packet, ClientID sender) override;
	bool				CanHaveFriendlyFire		() override {return false;}
	void				teleport_object			(NET_Packet &packet, ALife::_OBJECT_ID id) override;
	void				add_restriction			(NET_Packet &packet, ALife::_OBJECT_ID id) override;
	void				remove_restriction		(NET_Packet &packet, ALife::_OBJECT_ID id) override;
	void				remove_all_restrictions	(NET_Packet &packet, ALife::_OBJECT_ID id) override;
	bool				custom_sls_default		() override {return !!m_alife_simulator;};
	void				sls_default				() override;
	shared_str			level_name				(const shared_str &server_options) const override;
	void				on_death				(CSE_Abstract *e_dest, CSE_Abstract *e_src) override;
				void				restart_simulator		(const char* saved_game_name);

	IC			xrServer			&server					() const
	{
		VERIFY						(m_server);
		return						(*m_server);
	}

	virtual game_sv_Single* cast_game_sv_single() override { return this; }
};
