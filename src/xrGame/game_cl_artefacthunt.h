#pragma once
#include "game_cl_teamdeathmatch.h"

class CUIGameAHunt;

class game_cl_ArtefactHunt final : public game_cl_TeamDeathmatch
{
	friend class CUIMessagesWindow;
	CUIGameAHunt*						m_game_ui;	
	shared_str							m_Eff_Af_Spawn;
	shared_str							m_Eff_Af_Disappear;
	using inherited = game_cl_TeamDeathmatch;

protected:
	virtual const shared_str			GetBaseCostSect			() override {return "artefacthunt_base_cost";}
	virtual void				TranslateGameMessage	(u32 msg, NET_Packet& P) override;
	virtual void				shedule_Update			(u32 dt) override;

	virtual bool				CanCallBuyMenu			() override;
	virtual bool				CanBeReady				() override;
	virtual void				UpdateMapLocations		() override;

	virtual bool				NeedToSendReady_Spectator		(int key, game_PlayerState* ps) override;
	virtual void				LoadSndMessages				() override;

	virtual void				OnBuySpawnMenu_Ok		() override;	
public:
	u8									artefactsNum;//ah
	ALife::_OBJECT_ID					artefactBearerID;//ah,ZoneMap
	ALife::_OBJECT_ID					old_artefactBearerID;
	u8									teamInPossession;//ah,ZoneMap
	u8									old_teamInPossession;
	ALife::_OBJECT_ID artefactID;
	ALife::_OBJECT_ID old_artefactID;
	s32									iReinforcementTime;
	s32									dReinforcementTime;

	int									m_iSpawn_Cost;

public :
										game_cl_ArtefactHunt	();
	virtual ~game_cl_ArtefactHunt	() override;
	virtual void				Init					() override;
	virtual CUIGameCustom*		createGameUI			() override;
	virtual void				SetGameUI				(CUIGameCustom*) override;
	virtual void				net_import_state		(NET_Packet& P) override;
	virtual void				GetMapEntities(xr_vector<SZoneMapEntityData>& dst) override;
	virtual char*				getTeamSection			(int Team) override;
	virtual bool				PlayerCanSprint			(CActor* pActor) override;

	virtual void						SetScore				() override;
	virtual void				OnSellItemsFromRuck		() override;

	virtual void				OnSpawn					(CObject* pObj) override;
	virtual void				OnDestroy				(CObject* pObj) override;
	virtual void				SendPickUpEvent			(ALife::_OBJECT_ID ID_who, ALife::_OBJECT_ID ID_what) override;
	virtual void					OnConnected				() override;

	virtual game_cl_mp* cast_game_cl_mp() override { return this; }
	virtual game_cl_Deathmatch* cast_game_cl_deathmatch() override { return this; }
	virtual game_cl_ArtefactHunt* cast_game_cl_artefacthunt() override { return this; }
};
