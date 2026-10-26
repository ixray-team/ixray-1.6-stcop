#pragma once
#include "game_cl_mp.h"

#include "ui/UIBuyWndShared.h"
#include "ui/UIBuyWndBase.h"


class CUISkinSelectorWnd;
class CUIDialogWnd;
class CUIGameDM;
class CUIPdaWnd;
class CUIInventoryWnd;
class CUIMapDesc;
class CWeaponMagazinedWGrenade;

class game_cl_Deathmatch : public game_cl_mp
{
	using inherited = game_cl_mp;
	CUIGameDM*						m_game_ui;
	shared_str						Actor_Spawn_Effect;
public :
									game_cl_Deathmatch();
	virtual ~game_cl_Deathmatch() override;
	s32								m_s32FragLimit; //dm,tdm,ah
	s32								m_s32TimeLimit; //dm
	bool							m_bDamageBlockIndicators;
	xr_vector<game_TeamState>		teams;//dm,tdm,ah
	u32								m_u32ForceRespawn;

	u32								m_cl_dwWarmUp_Time;
	string64						WinnerName;

	virtual CUIGameCustom*		createGameUI			() override;
	virtual void				SetGameUI				(CUIGameCustom*) override;
	virtual void				net_import_state		(NET_Packet& P) override;
	virtual void				net_import_update		(NET_Packet& P) override;
	virtual void				Init					() override;

	virtual void				LoadSndMessages			() override;
	protected:
	struct PresetItem
	{
		u8	SlotID;
		u8	ItemID;
		s16	BigID;
		PresetItem (u8 Slot, u8 Item) { set(Slot, Item); };
		PresetItem (s16 Big) { set(Big); };
		bool	operator ==		(const s16& ID) 
		{ 
			return (BigID) == (ID);
		}
		void		set(s16 Big) { SlotID = u8((Big>>0x08) & 0x00ff); ItemID = u8(Big & 0x00ff); BigID = Big;}
		void		set(u8 Slot, u8 Item) { SlotID = Slot; ItemID = Item; BigID = (s16(SlotID) << 0x08) | s16(ItemID); };
	};

	using PRESET_ITEMS = xr_vector<PresetItem>;
	using PRESET_ITEMS_it = PRESET_ITEMS::iterator;

	PRESET_ITEMS					PresetItemsTeam0;
	PRESET_ITEMS					AdditionalPresetItems;
	PRESET_ITEMS*					pCurPresetItems;
	PRESET_ITEMS					PlayerDefItems;
	IBuyWnd*						pCurBuyMenu;

	CUISkinSelectorWnd*				pCurSkinMenu;
	bool							m_bFirstRun;
	bool							m_bMenuCalledFromReady;
	bool							m_bSkinSelected;
	
	bool							m_bBuyEnabled;
	s32								m_iCurrentPlayersMoney;

	u32								m_dwVoteEndTime;

	virtual const shared_str		GetBaseCostSect			() {return "deathmatch_base_cost";}
			void					CheckItem				(PIItem pItem, PRESET_ITEMS* pPresetItems,  bool OnlyPreset);

			void					ClearBuyMenu			();
			IBuyWnd*				InitBuyMenu				(const shared_str& BasePriceSection, s16 Team);
			CUISkinSelectorWnd*		InitSkinMenu			(s16 Team = -1);
			void					SetBuyMenuItems			(PRESET_ITEMS* pItems, bool OnlyPreset = false);
	virtual bool					CanBeReady				() override;
	virtual bool					CanCallBuyMenu			();
	virtual bool					CanCallSkinMenu			();
	virtual	bool					CanCallInventoryMenu	();

	virtual bool					NeedToSendReady_Actor	(int key, game_PlayerState* ps) override;
	virtual bool					NeedToSendReady_Spectator(int key, game_PlayerState* ps) override;

			void					Check_Invincible_Players ();

	virtual void				shedule_Update			(u32 dt) override;
	virtual bool				OnKeyboardPress			(int key) override;
	virtual bool				OnKeyboardRelease		(int key) override;

	virtual		const shared_str	GetTeamMenu				(s16 team);
	virtual		void				LoadTeamDefaultPresetItems	(const shared_str& caSection, IBuyWnd* pBuyMenu, PRESET_ITEMS* pPresetItems);
	virtual		void				LoadPlayerDefItems			(char* TeamName, IBuyWnd* pBuyMenu);
	virtual		void				LoadDefItemsForRank			(IBuyWnd* pBuyMenu);
	virtual		void				ChangeItemsCosts			(IBuyWnd* pBuyMenu);
///	virtual		s16					GetBuyMenuItemIndex			(u8 SlotID, u8 ItemID);
				s16					GetBuyMenuItemIndex			(u8 Addons, u8 ItemID);

	virtual		void				ConvertTime2String		(string64* str, u32 Time);
	virtual		int					GetPlayersPlace			(game_PlayerState* ps);

	virtual		void				PlayParticleEffect		(const char* EffName, Fvector& pos);

	virtual		void				ShowBuyMenu				();
	virtual		void				HideBuyMenu				();

public:
	virtual s16						ModifyTeam				(s16 Team) override;

	virtual char*				getTeamSection			(int Team) override;
	virtual	void					SetCurrentBuyMenu		();
	virtual	void					SetCurrentSkinMenu		();//	{pCurSkinMenu = pSkinMenuTeam0; };

	virtual void					OnSpectatorSelect		() override;

	virtual void					OnMapInfoAccept			() override;
	virtual void				OnSkinMenuBack			() override;
	virtual void					OnBuyMenu_Ok			() override;
	virtual	void					OnBuyMenu_DefaultItems	();
	virtual void					OnSkinMenu_Ok			() override;
	virtual void				OnSkinMenu_Cancel		() override;

	virtual void				OnGameMenuRespond_ChangeSkin	(NET_Packet& P) override;

	virtual CUIDialogWnd*			GetBuyWnd				()	{ return (CUIDialogWnd*) pCurBuyMenu; };
	virtual CUIDialogWnd*			GetSkinWnd				()	{ return (CUIDialogWnd*) pCurSkinMenu; };

	virtual void				OnVoteStart				(NET_Packet& P) override;
	virtual void				OnVoteStop				(NET_Packet& P) override;
	virtual void				OnVoteEnd				(NET_Packet& P) override;

	virtual void				GetMapEntities			(xr_vector<SZoneMapEntityData>& dst) override;

	virtual void				OnRender				() override;
	virtual bool				IsEnemy					(game_PlayerState* ps) override;
	virtual bool				IsEnemy					(CEntityAlive* ea1, CEntityAlive* ea2) override;

	virtual void				OnSpawn					(CObject* pObj) override;
	virtual void				OnSwitchPhase			(u32 old_phase, u32 new_phase) override;
	virtual void				OnRankChanged			(u8 OldRank) override;
	virtual		void				PlayRankChangesSndMessage ();
	virtual void				OnTeamChanged			() override;
	virtual void				OnMoneyChanged			() override;

	virtual		void				SetScore				();

	virtual void				OnSwitchPhase_InProgress() override;

	virtual u8					GetTeamCount			() override { return 1; };

	virtual void				OnPlayerFlagsChanged	(game_PlayerState* ps) override;
	virtual void				SendPickUpEvent			(ALife::_OBJECT_ID ID_who, ALife::_OBJECT_ID ID_what) override;

	virtual void				OnGameRoundStarted				() override;
	virtual void				UpdateMapLocations		() override;
	virtual bool				IsPlayerInTeam			(game_PlayerState* ps, ETeam team) override;
	virtual		bool				LocalPlayerCanBuyItem	(shared_str const & name_sect);
	virtual const char*				GetGameScore			(string32&	score_dest) override;
	virtual void				OnConnected				() override;

	virtual game_cl_mp* cast_game_cl_mp() override { return this; }
	virtual game_cl_Deathmatch* cast_game_cl_deathmatch() override { return this; }

private:
	//next methods for alife players in buy menu (artefacthunt)
	typedef buffer_vector<shared_str> aditional_ammo_t;
			void		TryToDefuseAllWeapons		(aditional_ammo_t & dest_ammo);
			void		AdditionalAmmoInserter		(aditional_ammo_t::value_type const & sect_name);
};

bool	DM_Compare_Players		(game_PlayerState* p1, game_PlayerState* p2);