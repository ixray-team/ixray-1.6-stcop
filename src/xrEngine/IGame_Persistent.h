#pragma once
#include "../xrServerEntities/gametype_chooser.h"
#include "GameFont.h"
#include "Environment.h"
#include "IGame_ObjectPool.h"
#include "Render.h"
#include "VisorRainDrops.h"
#include "../Include/xrRender/VisorGlassType.h"
#include <luabind/class_info.hpp>

#include "alife_space.h"

class IRenderVisual;
class IMainMenu;
extern ENGINE_API bool g_dedicated_server;
extern ENGINE_API u32 g_NewDofMode;

enum class EDofMode : u32
{
	Autofocus = 0,
	Static = 1,
	Manual = 2
};

// Physical camera targets shared by the game and renderer. Distances are in meters, focal length in millimeters.
struct SDofCameraSettings
{
	EDofMode Mode = EDofMode::Autofocus;
	float FNumber = 4.0f;
	bool ManualFocalLength = false;
	float FocalLength = 10.0f;
	float FocusTime = 0.1f;
	Fvector2 FocusPoint = {0.5f, 0.495f};
	float FocusDistance = 1.4f;
	float TransitionTime = 0.1f;
	bool IsInstant = false;
	float MaxRadius = 0.0f;
	float Squeeze = 1.0f;
	float Swirl = 0.0f;
};

class ENGINE_API IGame_Persistent : 
	public DLL_Pure,
	public pureAppStart, 
	public pureAppEnd,
	public pureAppActivate, 
	public pureAppDeactivate,
	public pureFrame
{
public:
	union params
	{
		struct
		{
			string256	m_game_or_spawn;
			string256	m_game_type;
			string256	m_alife;
			string256	m_new_or_load;
			EGameIDs	m_e_game_type;
		};
		string256		m_params[4];

		params() { reset(); }

		void reset()
		{
			for (int i = 0; i < 4; ++i)
				xr_strcpy(m_params[i], "");
		}

		void parse_cmd_line(const char* cmd_line)
		{
			reset();
			int n = std::min(4, _GetItemCount(cmd_line, '/'));
			for (int i = 0; i < n; ++i)
			{
				_GetItem(cmd_line, i, m_params[i], '/');
				_strlwr(m_params[i]);
			}
		}
	};

	struct ShaderParamsData
	{
		float HelmetCondition = -1.f;
		bool ItemCfgHudGasMaskAvailable = false;
		bool ItemCfgHudRainDropsAvailable = false;
		VisorGlassType GlassType = VisorGlassType::Flat;
		ALife::_OBJECT_ID VisorItemId = ALife::INVALID_OBJECT_ID;
		Fvector4 NightVisionPostprocess{};
	};

	params m_game_params;
	ShaderParamsData ShaderParams;
	VisorRainDrops VisorRain;
	luabind::functor<void> ixr_framework_onframe;
public:
	virtual void					PreStart			(const char* op);
	virtual void					Start				(const char* op);
	virtual void					Disconnect			();
	IGame_ObjectPool				ObjectPool;
	class CEnvironment*				pEnvironment;
	CEnvironment&					Environment()	{return *pEnvironment;};
	void							Prefetch			( );
	xr_string						GetPrefetchSection	(const char* Prefix) const;
	void							RequestLearnedModels(const char* LevelName);
	void							SaveLearnedModels	(const char* LevelName);
	IMainMenu*						m_pMainMenu;

	virtual	void					UpdatePlayDestroyParticles() {};
	virtual	void					UpdateParticles		() {};
	virtual	void					destroy_particles	(bool all_particles) {};

	virtual bool					OnRenderPPUI_query	() { return false; };	// should return true if we want to have second function called
	virtual void					OnRenderPPUI_main	() {};
	virtual void					OnRenderPPUI_PP		() {};

	virtual void					OnAppStart			() override;
	virtual void					OnAppEnd			() override;
	virtual void					OnAppActivate		() override;
	virtual void					OnAppDeactivate		() override;
	virtual void		_BCL		OnFrame				() override;
	virtual void					OnFrameMT			();

	// вызывается только когда изменяется тип игры
	virtual	void					OnGameStart			(); 
	virtual void					OnGameEnd			();

	virtual void					UpdateGameType		() {};
	virtual void					GetCurrentDof		(Fvector3& dof){dof.set(-1.4f, 0.0f, 250.f);};
	virtual float GetDofOverrideWeight() const { return 0.0f; }
	virtual void GetDofCameraSettings(SDofCameraSettings& /*Settings*/) const {}
	virtual void					SetBaseDof			(const Fvector3& dof){};
	virtual float					GetDofKernel		(float Default) { return Default; }
	virtual bool					IsDofForced			() const { return false; }
	virtual bool IsDofCameraForced() const { return false; }
	virtual void					OnSectorChanged		(int sector){};
	virtual void					OnAssetsChanged		();

	virtual void					RegisterModel		(IRenderVisual* V) {}
	virtual	float					MtlTransparent		(u32 mtl_idx) { return 1;}

	IGame_Persistent				();
	~IGame_Persistent		() override;

	ICF		u32						GameType			() {return m_game_params.m_e_game_type;};
	virtual void					Statistics			(CGameFont* F) {}
	virtual	void					LoadTitle			(bool change_tip=false, shared_str map_name=""){}
	virtual void					SetLoadStageTitle	(const char* /*ls_title*/) {}
	virtual bool					CanBePaused			()		{ return true;}
	virtual bool					IsIntroActive		() const { return false; }
	virtual void					GetTextureParams	(shared_str tex, Frect& out_rect, shared_str& file_name) {}
	virtual void					ChangeCursorPosition(Fvector2 value) {}
};

extern ENGINE_API IGame_Persistent* g_pGamePersistent;

#include "IGame_Menu.h"
