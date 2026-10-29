#ifndef GamePersistentH
#define GamePersistentH
#pragma once

#include "../xrEngine/IGame_Persistent.h"
#include "DofManager.h"
class CMainMenu;
class CUICursor;
class CParticlesObject;
class CUISequencer;
class ui_core;

class CGamePersistent: 
	public IGame_Persistent, 
	public IEventReceiver
{
	using inherited = IGame_Persistent;

	// ambient particles
	xr_shared_ptr<CParticlesObject>	ambient_particles; 
    xr_vector<u32>		ambient_sound_next_time; // max snd channels
	u32					ambient_effect_next_time;
	u32					ambient_effect_stop_time;

	float				ambient_effect_wind_start;
	float				ambient_effect_wind_in_time;
	float				ambient_effect_wind_end;
	float				ambient_effect_wind_out_time;
	bool				ambient_effect_wind_on;

	CDofManager Dof;

	CUISequencer*		m_intro;
	EVENT				eQuickLoad;

	xr_delegate<void()> m_intro_event;

	void 		start_logo_intro		();
	void 		update_logo_intro		();

	void 		game_loaded				();
	void 		update_game_loaded		();

	void 		start_game_intro		();
	void 		update_game_intro		();

#ifdef DEBUG
	u32					m_frame_counter;
	u32					m_last_stats_frame;
#endif

	void				WeathersUpdate			();

public:
	xr_vector<xr_shared_ptr<CParticlesObject>> ps_active;
	xr_vector<xr_shared_ptr<CParticlesObject>> ps_active_deffer;
	xr_vector<xr_shared_ptr<CParticlesObject>> ps_needtoplay;

	IReader*			pDemoFile;
	u32					uTime2Change;
	EVENT				eDemoStart;

						CGamePersistent			();
	virtual ~CGamePersistent		() override;

	virtual void PreStart(const char* op) override;
	virtual void		Start					(const char* op) override;
	virtual void		Disconnect				() override;
	virtual bool		IsIntroActive			() const override { return m_intro != nullptr; }

	virtual void		OnAppActivate			() override;
	virtual void		OnAppDeactivate			() override;

	virtual void		OnAppStart				() override;
	virtual void		OnAppEnd				() override;
	virtual void		OnGameStart				() override;
	virtual void		OnGameEnd				() override;

	virtual void	_BCL	OnFrame					() override;
	virtual void			OnEvent					(EVENT E, u64 P1, u64 P2) override;

	virtual void		UpdateGameType			() override;

	virtual void		destroy_particles		(bool all_particles) override;
	virtual void		UpdatePlayDestroyParticles() override;
	virtual void		UpdateParticles			() override;

	virtual void		RegisterModel			(IRenderVisual* V) override;
	virtual float		MtlTransparent			(u32 mtl_idx) override;
	virtual void		Statistics				(CGameFont* F) override;

	virtual bool		OnRenderPPUI_query		() override;
	virtual void		OnRenderPPUI_main		() override;
	virtual void		OnRenderPPUI_PP			() override;
	virtual void		LoadTitle				(bool change_tip = false, shared_str map_name = "") override;
    void				SetLoadStageTitle		(const char* ls_title = nullptr) override;

	virtual bool		CanBePaused				() override;

			void		SetPickableEffectorDOF	(bool bSet);
			void		SetEffectorDOF			(const Fvector& needed_dof);
			void		RestoreEffectorDOF		();
	CDofManager& DofManager() { return Dof; }

	virtual void		GetCurrentDof			(Fvector3& dof) override;
	virtual void		SetBaseDof				(const Fvector3& dof) override;
	virtual void		OnSectorChanged			(int sector) override;
	virtual void		OnAssetsChanged			() override;
	void				SetDiscordStatus() const;
	virtual void		GetTextureParams		(shared_str tex, Frect& out_rect, shared_str& file_name) override;
	virtual void		ChangeCursorPosition	(Fvector2 value) override;
};

IC CGamePersistent&		GamePersistent()		{ return *((CGamePersistent*) g_pGamePersistent);			}

#endif //GamePersistentH

