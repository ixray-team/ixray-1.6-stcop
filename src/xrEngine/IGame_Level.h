#pragma once

#include "IInputReceiver.h"
#include "xr_object_list.h"
#include "../xrCore/Collision/xr_area.h"
#include "IGame_Patrol.h"

// refs
class ENGINE_API CCameraManager;
class ENGINE_API CCursor;
class ENGINE_API CCustomHUD;
class XRCORE_API ISpatial;
namespace Feel { class ENGINE_API Sound; }

class ENGINE_API CServerInfo
{
private:
	struct SItem_ServerInfo
	{
		string128	name;
		u32			color;
	};
	enum { max_item = 15 };
	svector<SItem_ServerInfo,max_item>	data;

public:
	u32		Size()			{ return data.size(); }
	void	ResetData()		{ data.clear(); }

	void	AddItem( LPCSTR name_,		LPCSTR value_, u32 color_ = RGB(255,255,255) );
	void	AddItem( shared_str& name_,	LPCSTR value_, u32 color_ = RGB(255,255,255) );

	IC SItem_ServerInfo&	operator[] ( u32 id ) { VERIFY( id < max_item ); return data[id]; }

	CServerInfo() {};
	~CServerInfo() {};
};

//-----------------------------------------------------------------------------------------------------------
class ENGINE_API	IGame_Level	:
	public DLL_Pure,
	public IInputReceiver,
	public pureRender,
	public pureFrame,
	public IEventReceiver
{
	friend class CCameraManager;

protected:
	// Network interface
	CObject*					pCurrentEntity;
	CObject*					pCurrentViewEntity;
   
	// Static sounds
	xr_vector<ref_sound>		Sounds_Random;
	u32							Sounds_Random_dwNextTime;
	BOOL						Sounds_Random_Enabled;
	CCameraManager*				m_pCameras;
	CObject*					pCurrentControlEntity;

	// temporary
	xr_vector<ISpatialShared>	snd_ER;
public:
	CObjectList					Objects; 
	CObjectSpace				ObjectSpace;
	CCameraManager&				Cameras			()				{return *m_pCameras;};

	BOOL						bReady;
	bool						UseSnowmask = true;

	CInifile*					pLevel;
public:	// deferred sound events
	struct	_esound_delegate	{
		Feel::Sound*			dest	;
		ref_sound_data_ptr		source	;
		float					power	;
	};
	xr_vector<_esound_delegate>	snd_Events;
public:
	// Main, global functions
	IGame_Level					();
	virtual ~IGame_Level		();

	virtual shared_str			name					() const = 0;
	virtual void				GetLevelInfo			( CServerInfo* si ) = 0;

	virtual u64					GetGameTime				() { return 0; };
	virtual BOOL				net_Start				( LPCSTR op_server, LPCSTR op_client)	= 0;
	virtual void				net_Load				( LPCSTR name )							= 0;
	virtual void				net_Save				( LPCSTR name )							= 0;
	virtual void				net_Stop				( );
	virtual void				net_Update				( )										= 0;

	virtual BOOL				Load					( u32 dwNum );
	virtual BOOL				Load_GameSpecific_Before( )										{ return TRUE; };		// before object loading
	virtual BOOL				Load_GameSpecific_After	( )										{ return TRUE; };		// after object loading
	void						Load_GameSpecific_CFORM	( CDB::TRI* T, size_t count );

	virtual void	_BCL		OnFrame					( void );
	virtual void				OnRender				( void );

	virtual	shared_str			OpenDemoFile			(LPCSTR demo_file_name) = 0;
	virtual void				net_StartPlayDemo		() = 0;

	// Main interface
	CObject*					CurrentEntity			( void ) const							{ return pCurrentEntity;				}
	CObject*					CurrentViewEntity		( void ) const							{ return pCurrentViewEntity;			}
	void						SetEntity				( CObject* O  );//							{ pCurrentEntity=pCurrentViewEntity=O;	}
	void						SetViewEntity			( CObject* O  );//							{ pCurrentViewEntity=O;					}
	

	CObject*					CurrentControlEntity	( void ) const		{ return pCurrentControlEntity; }
	void						SetControlEntity		( CObject* O  )		{ pCurrentControlEntity=O; }

	void						SoundEvent_Register		( ref_sound_data_ptr S, float range );
	void						SoundEvent_Dispatch		( );
	void                        SoundEvent_OnDestDestroy (Feel::Sound*);
	void                        SoundEvent_net_Relcase	(CObject* obj);
	// Loader interface
    virtual float               GetEnvironmentGameDayTimeSec() const = 0;
    virtual void                SetEnvironmentGameTimeFactor(u64 const& GameTime, float const& fTimeFactor) = 0;
    virtual float               GetEnvironmentTimeFactor() const = 0;
    virtual void                SetEnvironmentTimeFactor(float fTimeFactor) = 0;
    virtual u64                 GetEnvironmentGameTime() const = 0;

	virtual	void				SpawnItem(LPCSTR section, const Fvector& position, u32 level_vertex_id, u16 parent_id) = 0;
	virtual IGame_Patrol*		CreatePatrol(const char* patrol) = 0;
	virtual void				LoadEditor(shared_str LevelName) {};

	ICF void dbg_text_renderer(const Fvector& pos, u32 color = color_rgba(0, 255, 100, 255), shared_str str = "+")
	{
		Fvector4		v_res;
		Device.mFullTransform.transform(v_res, pos);

		float x = (1.f + v_res.x) / 2.f * (Device.Width);
		float y = (1.f - v_res.y) / 2.f * (Device.Height);

		if (v_res.z < 0 || v_res.w < 0)
			return;

		if (v_res.x < -1.f || v_res.x > 1.f || v_res.y < -1.f || v_res.y>1.f)
			return;

		g_FontManager->pFontSystem->SetAligment(CGameFont::alCenter);
		g_FontManager->pFontSystem->SetColor(color);
		g_FontManager->pFontSystem->Out(x, y, "%s", str.c_str());
	}
};

extern ENGINE_API IGame_Level* g_pGameLevel;