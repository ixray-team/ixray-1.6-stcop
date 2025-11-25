#include "StdAfx.h"
#include "pch_script.h"
#include "UIGameTutorial.h"
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/UIXmlInit.h"
#include "object_broker.h"
#include "../../xrEngine/xr_input.h"

#include "../Include/xrRender/UISequenceVideoItem.h"
#include "../Include/xrRender/UIShader.h"
#include "../Include/xrRender/UIRender.h"
#include "../../xrUI/UICursor.h"

extern ENGINE_API bool bShowPauseString;

//-----------------------------------------------------------------------------
// Tutorial Item
//-----------------------------------------------------------------------------
CUISequenceVideoItem::CUISequenceVideoItem(CUISequencer* owner):CUISequenceItem(owner)
{
	//m_texture				= nullptr;
	m_flags.set				(etiPlaying|etiNeedStart|etiDelayed|etiBackVisible,false);
	m_delay					= 0.f;
	m_wnd					= nullptr;
	m_wnd_bg				= nullptr;
	m_delay					= 0.f;
	m_time_start			= 0;
	m_sync_time				= 0;
}

CUISequenceVideoItem::~CUISequenceVideoItem()
{
	m_sound.stop			();
	m_sound_mono[0].stop	();
	m_sound_mono[1].stop	();
	delete_data				(m_wnd);
	delete_data				(m_wnd_bg);
}

bool CUISequenceVideoItem::IsPlaying()
{
	return					(!!m_flags.test(etiPlaying));
}

void CUISequenceVideoItem::Load(CUIXml* xml, int idx)
{
	CUISequenceItem::Load	(xml,idx);

	XML_NODE* _stored_root	= xml->GetLocalRoot();
	xml->SetLocalRoot		(xml->NavigateToNode("item",idx));
	
	const char* str				= xml->Read				("pause_state", 0, "ignore");
	m_flags.set										(etiNeedPauseOn,	0==_stricmp(str, "on"));
	m_flags.set										(etiNeedPauseOff,	0==_stricmp(str, "off"));
	m_flags.set										(etiNeedPauseSound, 0==_stricmp(str, "on"));

	str						= xml->Read				("can_be_stopped",0,"on");
	m_flags.set										(etiCanBeStopped,	0==_stricmp(str, "on"));

	str						= xml->Read				("back_show",0,"on");
	m_flags.set										(etiBackVisible,	0==_stricmp(str, "on"));

	m_flags.set										(etiGrabInput,		true);

	m_delay					= std::max(xml->ReadFlt		("delay",0,0.f),0.f);

	//ui-components
	if(xml->NavigateToNode("background",0))
	{
		m_wnd_bg									= new CUIStatic();
		m_wnd_bg->SetAutoDelete						(false);
		CUIXmlInit::InitStatic						(*xml, "background", 0, m_wnd_bg);
	}
	m_wnd											= new CUIStatic();
	m_wnd->SetAutoDelete							(false);
	CUIXmlInit::InitStatic							(*xml, "video_wnd", 0, m_wnd);
	bool bFullScreen								= (1==xml->ReadAttribInt("video_wnd", 0, "fullscreen", 0));
	if(!bFullScreen)
	{
		m_wnd->SetWndPos								(Fvector2().set(512.0f,384.0f));
		m_wnd->SetAlignment								(waCenter);
		Frect texture_coords							= m_wnd->GetUIStaticItem().GetTextureRect();

		bool is_16_9									= UI().is_widescreen();
		float kw_image									= UI_BASE_WIDTH / texture_coords.width();

		Fvector2										wnd_size;

		wnd_size.x										= UI_BASE_WIDTH;
		wnd_size.y										= texture_coords.height()*kw_image;
		if(is_16_9)
			wnd_size.y									*= m_compatibility_mode == eCompatibilitySoC ? 1.328f : 1.2f;

		m_wnd->SetWndSize								(wnd_size);
	}
	const char* snd_name										= xml->Read("sound",0,"");

	if (snd_name && snd_name[0])
	{
		string_path _fn;
		if (FS.exist(_fn, "$game_sounds$", snd_name, ".ogg"))
		{
			m_sound.create		(snd_name,st_Effect,sg_Undefined);	
		}
		else
		{
			string_path			_l, _r;
			xr_strconcat(_l, snd_name, "_l");
			xr_strconcat(_r, snd_name, "_r");
			m_sound_mono[0].create(_l, st_Effect, sg_Undefined);
			m_sound_mono[1].create(_r, st_Effect, sg_Undefined);
		}
	}
	xml->SetLocalRoot		(_stored_root);
}

void CUISequenceVideoItem::Update()
{
	inherited::Update();
	if(GetUICursor().IsVisible())
	{
		m_flags.set			(etiStoredCursorState, true);
		GetUICursor().Hide	();
	}
	// deferred start
	if (Device.dwTimeContinual>=m_time_start)
	{
		if (m_flags.test(etiDelayed))
		{
			if(m_wnd_bg)
			{
				m_owner->MainWnd()->AttachChild	(m_wnd_bg);
				m_wnd_bg->Show				(true);
			}
			m_owner->MainWnd()->AttachChild	(m_wnd);
			m_wnd->Show						(true);
			m_flags.set						(etiDelayed, false);
		}
	}else return;

	ref_sound snd			= m_sound.handle() ? m_sound : m_sound_mono[0];
	u32 sync_tm				= Device.dwTimeContinual;
	m_sync_time				= sync_tm;
	// processing A&V

	if (m_texture->HasTexture())
	{
		bool is_playing		= snd.handle() ? snd.is_playing() : m_texture->video_IsPlaying();
		if (is_playing)
		{
			m_texture->video_Sync(m_sync_time);
		}else
		{
			// sync start
			if (m_flags.test(etiNeedStart))
			{
				if (m_sound.handle())
					m_sound.play		(nullptr, sm_Intro);
				else
				{
					m_sound_mono[0].play(nullptr, sm_Intro);
					m_sound_mono[0].set_panning(1.0f, 0.f);
					m_sound_mono[1].play(nullptr, sm_Intro);
					m_sound_mono[1].set_panning(0.f, 1.0f);
				}
				m_texture->video_Play	(false, m_sync_time);
				m_flags.set				(etiNeedStart,false);
				CUIWindow* w			= m_owner->MainWnd()->FindChild("back");
				if (w)					
					w->Show(!!m_flags.test(etiBackVisible));
			}else{
				m_flags.set				(etiPlaying,false);
			}
		}
	}
}

void CUISequenceVideoItem::OnRender()
{
	if (!m_texture->HasTexture() && m_wnd->GetShader() && m_wnd->GetShader()->inited())
	{
		UIRender->SetShader(*m_wnd->GetShader());
		m_texture->CaptureTexture();
		m_texture->video_Stop();
	}
}

void CUISequenceVideoItem::Start()
{
	inherited::Start			();
	m_flags.set					(etiStoredPauseState, Device.Paused());

	if(m_flags.test(etiNeedPauseOn) && !m_flags.test(etiStoredPauseState))
	{
		Device.Pause			(true, true, true, "videoitem_start");
		bShowPauseString		= false;
	}

	if(m_flags.test(etiNeedPauseOff) && m_flags.test(etiStoredPauseState))
		Device.Pause			(false, true, true, "videoitem_start");

	if(m_flags.test(etiNeedPauseSound))
		Device.Pause			(true, false, true, "videoitem_start");

	m_flags.set					(etiPlaying,true);
	m_flags.set					(etiNeedStart,true);

	m_sync_time					= 0;
	m_time_start				= Device.dwTimeContinual+iFloor(m_delay*1000.f);
	m_flags.set					(etiDelayed,true);

	if (m_flags.test(etiBackVisible)){
		CUIWindow* w			= m_owner->MainWnd()->FindChild("back");
		if (w)					w->Show(true);
	}
}

bool CUISequenceVideoItem::Stop	(bool bForce)
{
	if(m_flags.test(etiStoredCursorState) )
		GetUICursor().Show			();

	if(!m_flags.test(etiCanBeStopped) && !bForce && IsPlaying()) 
		return false;

	m_flags.set					(etiPlaying,false);

	m_wnd->Show					(false);
	if(Device.dwTimeContinual>=m_time_start && m_wnd->GetParent()==m_owner->MainWnd())
		m_owner->MainWnd()->DetachChild(m_wnd);

	m_sound.stop				();
	m_texture->ResetTexture		();

	if(m_flags.test(etiNeedPauseOn) && !m_flags.test(etiStoredPauseState))
		Device.Pause			(false, true, true, "videoitem_stop");

	if(m_flags.test(etiNeedPauseOff) && m_flags.test(etiStoredPauseState))
		Device.Pause			(true, true, true, "videoitem_stop");

	if(m_flags.test(etiNeedPauseSound))
		Device.Pause			(false, false, true, "videoitem_stop");

	inherited::Stop				();
	return true;
}
