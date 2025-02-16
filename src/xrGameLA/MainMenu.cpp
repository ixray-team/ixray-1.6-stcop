#include "stdafx.h"
#include "MainMenu.h"
#include "UI/UIDialogWnd.h"
#include "ui/UIMessageBoxEx.h"
#include "../xr_IOConsole.h"
#include "../xrEngine/igame_level.h"
#include "../CameraManager.h"
#include "xr_Level_controller.h"
#include "ui\UITextureMaster.h"
#include "ui\UIXmlInit.h"
#include <dinput.h>
#include "ui\UIBtnHint.h"
#include "UICursor.h"
#include "string_table.h"

#include "object_broker.h"

//#define DEMO_BUILD

string128	ErrMsgBoxTemplate	[]	= {
		"message_box_invalid_pass",
		"message_box_invalid_host",
		"message_box_session_full",
		"message_box_server_reject",
		"message_box_cdkey_in_use",
		"message_box_cdkey_disabled",
		"message_box_cdkey_invalid",
		"message_box_different_version",
		"message_box_gs_service_not_available",
		"message_box_sb_master_server_connect_failed",
		"msg_box_no_new_patch",
		"msg_box_new_patch",
		"msg_box_patch_download_error",		
		"msg_box_patch_download_success",
		"msg_box_connect_to_master_server",
		"msg_box_kicked_by_server",
		"msg_box_error_loading"
};

extern bool b_shniaganeed_pp;

CMainMenu*	MainMenu()	{return (CMainMenu*)g_pGamePersistent->m_pMainMenu; };
//----------------------------------------------------------------------------------
#define INIT_MSGBOX(_box, _template)	{ _box = new CUIMessageBoxEx(); _box->InitMessageBox(_template);}
//----------------------------------------------------------------------------------

CMainMenu::CMainMenu	()
{
	m_Flags.zero					();
	m_startDialog					= NULL;
	m_screenshotFrame				= u32(-1);
	g_pGamePersistent->m_pMainMenu	= this;
	if (Device.b_is_Ready)			OnDeviceCreate();  	
	ReadTextureInfo					();
	CUIXmlInit::InitColorDefs		();
	g_btnHint						= NULL;
	g_statHint						= NULL;
	m_deactivated_frame				= 0;	
	
	m_sPatchURL						= "";

	m_sPDProgress.IsInProgress		= false;

	//---------------------------------------------------------------
	m_NeedErrDialog					= ErrNoError;
	m_start_time					= 0;

	if(!g_dedicated_server)
	{
		g_btnHint						= new CUIButtonHint();
		g_statHint						= new CUIButtonHint();
		
		for (u32 i=0; i<u32(ErrMax); i++)
		{
			CUIMessageBoxEx*			pNewErrDlg;
			INIT_MSGBOX					(pNewErrDlg, ErrMsgBoxTemplate[i]);
			m_pMB_ErrDlgs.push_back		(pNewErrDlg);
		}

		m_pMB_ErrDlgs[PatchDownloadSuccess]->AddCallbackStr("button_yes", MESSAGE_BOX_YES_CLICKED, CUIWndCallback::void_function(this, &CMainMenu::OnRunDownloadedPatch));
		m_pMB_ErrDlgs[PatchDownloadSuccess]->AddCallbackStr("button_yes", MESSAGE_BOX_OK_CLICKED, CUIWndCallback::void_function(this, &CMainMenu::OnConnectToMasterServerOkClicked));
	}
	
	Device.seqFrame.Add		(this,REG_PRIORITY_LOW-1000);

	if (g_uCommonFlags.test(CF_Prefetch_UI)){
		Msg("*Start prefetching UI textures");
		Device.m_pRender->RenderPrefetchUITextures();
	}
}

CMainMenu::~CMainMenu	()
{
	if (g_uCommonFlags.test(CF_Prefetch_UI)){
		ReportTxrsForPrefetching();
	}
	Device.seqFrame.Remove			(this);
	xr_delete						(g_btnHint);
	xr_delete						(g_statHint);
	xr_delete						(m_startDialog);
	g_pGamePersistent->m_pMainMenu	= NULL;
	delete_data						(m_pMB_ErrDlgs);	
}

void CMainMenu::ReadTextureInfo()
{
	if (pSettings->section_exist("texture_desc"))
	{
		xr_string itemsList; 
		string256 single_item;

		itemsList = pSettings->r_string("texture_desc", "files");
		int itemsCount	= _GetItemCount(itemsList.c_str());

		for (int i = 0; i < itemsCount; i++)
		{
			_GetItem(itemsList.c_str(), i, single_item);
			xr_strcat(single_item,".xml");
			CUITextureMaster::ParseShTexInfo(single_item);
		}		
	}
}

extern ENGINE_API BOOL	bShowPauseString;
extern bool				IsGameTypeSingle();

void CMainMenu::Activate	(bool bActivate)
{
	if (	!!m_Flags.test(flActive) == bActivate)		return;
	if (	m_Flags.test(flGameSaveScreenshot)	)		return;
	if (	(m_screenshotFrame == Device.dwFrame)	||
			(m_screenshotFrame == Device.dwFrame-1) ||
			(m_screenshotFrame == Device.dwFrame+1))	return;

	bool b_is_single		= IsGameTypeSingle();

	if(g_dedicated_server && bActivate) return;

	if(bActivate)
	{
		b_shniaganeed_pp			= true;
		Device.Pause				(TRUE, FALSE, TRUE, "mm_activate1");
			m_Flags.set				(flActive|flNeedChangeCapture,TRUE);

		m_Flags.set					(flRestoreCursor,GetUICursor().IsVisible());

		if(!ReloadUI())				return;

		m_Flags.set					(flRestoreConsole,Console->bVisible);

		if(b_is_single)	m_Flags.set	(flRestorePause,Device.Paused());

		Console->Hide				();


		if(b_is_single)
		{
			m_Flags.set					(flRestorePauseStr, bShowPauseString);
			bShowPauseString			= FALSE;
			if(!m_Flags.test(flRestorePause))
				Device.Pause			(TRUE, TRUE, FALSE, "mm_activate2");
		}

		if(g_pGameLevel)
		{
			if(b_is_single){
				Device.seqFrame.Remove		(g_pGameLevel);
			}
			Device.seqRender.Remove			(g_pGameLevel);
			CCameraManager::ResetPP			();
		};
		Device.seqRender.Add				(this, 4); // 1-console 2-cursor 3-tutorial

		Console->Execute					("stat_memory");
	}else{
		m_deactivated_frame					= Device.dwFrame;
		m_Flags.set							(flActive,				FALSE);
		m_Flags.set							(flNeedChangeCapture,	TRUE);

		Device.seqRender.Remove				(this);
		
		bool b = !!Console->bVisible;
		if(b){
			Console->Hide					();
		}

		IR_Release							();
		if(b){
			Console->Show					();
		}

		if(m_startDialog->IsShown())
			m_startDialog->HideDialog		();

		CleanInternals						();
		if(g_pGameLevel)
		{
			if(b_is_single){
				Device.seqFrame.Add			(g_pGameLevel);

			}
			Device.seqRender.Add			(g_pGameLevel);
		};
		if(m_Flags.test(flRestoreConsole))
			Console->Show			();

		if(b_is_single)
		{
			if(!m_Flags.test(flRestorePause))
				Device.Pause			(FALSE, TRUE, FALSE, "mm_deactivate1");

			bShowPauseString			= m_Flags.test(flRestorePauseStr);
		}	
	
		if(m_Flags.test(flRestoreCursor))
			GetUICursor().Show			();

		Device.Pause					(FALSE, TRUE, TRUE, "mm_deactivate2");

		if(m_Flags.test(flNeedVidRestart))
		{
			m_Flags.set			(flNeedVidRestart, FALSE);
			Console->Execute	("vid_restart");
		}
	}
}

bool CMainMenu::ReloadUI()
{
	if(m_startDialog)
	{
		if(m_startDialog->IsShown())
			m_startDialog->HideDialog		();
		CleanInternals						();
	}
	DLL_Pure* dlg = NEW_INSTANCE(TEXT2CLSID("MAIN_MNU"));
	if(!dlg) 
	{
		m_Flags.set				(flActive|flNeedChangeCapture,FALSE);
		return false;
	}
	xr_delete					(m_startDialog);
	m_startDialog				= smart_cast<CUIDialogWnd*>(dlg);
	VERIFY						(m_startDialog);
	m_startDialog->m_bWorkInPause= true;
	m_startDialog->ShowDialog	(true);

	m_activatedScreenRatio		= (float)Device.dwWidth/(float)Device.dwHeight > (UI_BASE_WIDTH/UI_BASE_HEIGHT+0.01f);
	return true;
}

bool CMainMenu::IsActive()
{
	return !!m_Flags.test(flActive);
}

bool CMainMenu::CanSkipSceneRendering()
{
	return IsActive() && !m_Flags.test(flGameSaveScreenshot);
}

//IInputReceiver
static int mouse_button_2_key []	=	{MOUSE_1,MOUSE_2,MOUSE_3};
void	CMainMenu::IR_OnMousePress				(int btn)	
{	
	if(!IsActive()) return;

	IR_OnKeyboardPress(mouse_button_2_key[btn]);
};

void	CMainMenu::IR_OnMouseRelease(int btn)	
{
	if(!IsActive()) return;

	IR_OnKeyboardRelease(mouse_button_2_key[btn]);
};

void	CMainMenu::IR_OnMouseHold(int btn)	
{
	if(!IsActive()) return;

	IR_OnKeyboardHold(mouse_button_2_key[btn]);

};

void	CMainMenu::IR_OnMouseMove(int x, int y)
{
	if(!IsActive()) return;
	CDialogHolder::IR_UIOnMouseMove(x, y);
};

void	CMainMenu::IR_OnMouseStop(int x, int y)
{
};

void	CMainMenu::IR_OnKeyboardPress(int dik)
{
	if(!IsActive()) return;

	if( is_binded(kCONSOLE, dik) )
	{
		Console->Show();
		return;
	}
	if (DIK_F12 == dik){
		Render->Screenshot();
		return;
	}

	CDialogHolder::IR_UIOnKeyboardPress(dik);
};

void	CMainMenu::IR_OnKeyboardRelease			(int dik)
{
	if(!IsActive()) return;
	
	CDialogHolder::IR_UIOnKeyboardRelease(dik);
};

void	CMainMenu::IR_OnKeyboardHold(int dik)	
{
	if(!IsActive()) return;
	
	CDialogHolder::IR_UIOnKeyboardHold(dik);
};

void CMainMenu::IR_OnMouseWheel(int direction)
{
	if(!IsActive()) return;
	
	CDialogHolder::IR_UIOnMouseWheel(direction);
}


bool CMainMenu::OnRenderPPUI_query()
{
	return IsActive() && !m_Flags.test(flGameSaveScreenshot) && b_shniaganeed_pp;
}


extern void draw_wnds_rects();
void CMainMenu::OnRender	()
{
	if(m_Flags.test(flGameSaveScreenshot))
		return;

	if(g_pGameLevel)
		Render->Calculate			();

	Render->Render				();
	if(!OnRenderPPUI_query())
	{
		DoRenderDialogs();
		UI().RenderFont();
		draw_wnds_rects();
	}
}

void CMainMenu::OnRenderPPUI_main	()
{
	if(!IsActive()) return;

	if(m_Flags.test(flGameSaveScreenshot))
		return;

	UI().pp_start();

	if(OnRenderPPUI_query())
	{
		DoRenderDialogs();
		UI().RenderFont();
	}

	UI().pp_stop();
}

void CMainMenu::OnRenderPPUI_PP	()
{
	if ( !IsActive() ) return;

	if(m_Flags.test(flGameSaveScreenshot))	return;

	UI().pp_start();
	
	xr_vector<CUIWindow*>::iterator it = m_pp_draw_wnds.begin();
	for(; it!=m_pp_draw_wnds.end();++it)
	{
		(*it)->Draw();
	}
	UI().pp_stop();
}
/*
void CMainMenu::StartStopMenu(CUIDialogWnd* pDialog, bool bDoHideIndicators)
{
	pDialog->m_bWorkInPause = true;
	CDialogHolder::StartStopMenu(pDialog, bDoHideIndicators);
}*/

//pureFrame
void CMainMenu::OnFrame()
{
	if (m_Flags.test(flNeedChangeCapture))
	{
		m_Flags.set					(flNeedChangeCapture,FALSE);
		if (m_Flags.test(flActive))	IR_Capture();
		else						IR_Release();
	}
	CDialogHolder::OnFrame		();


	//screenshot stuff
	if(m_Flags.test(flGameSaveScreenshot) && Device.dwFrame > m_screenshotFrame  )
	{
		m_Flags.set					(flGameSaveScreenshot,FALSE);
		::Render->Screenshot		(IRender_interface::SM_FOR_GAMESAVE, m_screenshot_name);
		
		if(g_pGameLevel && m_Flags.test(flActive))
		{
			Device.seqFrame.Remove	(g_pGameLevel);
			Device.seqRender.Remove	(g_pGameLevel);
		};

		if(m_Flags.test(flRestoreConsole))
			Console->Show			();
	}

	if(IsActive())
	{
		CheckForErrorDlg();
		bool b_is_16_9	= (float)Device.dwWidth/(float)Device.dwHeight > (UI_BASE_WIDTH/UI_BASE_HEIGHT+0.01f);
		if(b_is_16_9 !=m_activatedScreenRatio)
		{
			ReloadUI();
			m_startDialog->SendMessage(m_startDialog, MAIN_MENU_RELOADED, NULL);
		}
	}
}

void CMainMenu::OnDeviceCreate()
{
}


void CMainMenu::Screenshot(IRender_interface::ScreenshotMode mode, LPCSTR name)
{
	if(mode != IRender_interface::SM_FOR_GAMESAVE)
	{
		::Render->Screenshot		(mode,name);
	}else{
		m_Flags.set					(flGameSaveScreenshot, TRUE);
		xr_strcpy(m_screenshot_name,name);
		if(g_pGameLevel && m_Flags.test(flActive)){
			Device.seqFrame.Add		(g_pGameLevel);
			Device.seqRender.Add	(g_pGameLevel);
		};
		m_screenshotFrame			= Device.dwFrame+1;
		m_Flags.set					(flRestoreConsole,		Console->bVisible);
		Console->Hide				();
	}
}

void CMainMenu::RegisterPPDraw(CUIWindow* w)
{
	UnregisterPPDraw				(w);
	m_pp_draw_wnds.push_back		(w);
}

void CMainMenu::UnregisterPPDraw				(CUIWindow* w)
{
	m_pp_draw_wnds.erase(
		std::remove(
			m_pp_draw_wnds.begin(),
			m_pp_draw_wnds.end(),
			w
		),
		m_pp_draw_wnds.end()
	);
}

void CMainMenu::SetErrorDialog					(EErrorDlg ErrDlg)	
{ 
	m_NeedErrDialog = ErrDlg;
};

void CMainMenu::CheckForErrorDlg()
{
	if (m_NeedErrDialog == ErrNoError)	return;
	m_pMB_ErrDlgs[m_NeedErrDialog]->ShowDialog(false);
	m_NeedErrDialog						= ErrNoError;
};

void CMainMenu::SwitchToMultiplayerMenu()
{

};

void CMainMenu::DestroyInternal(bool bForce)
{
	if(m_startDialog && ((m_deactivated_frame < Device.dwFrame+4)||bForce) )
		xr_delete		(m_startDialog);
}

void CMainMenu::OnNewPatchFound(LPCSTR VersionName, LPCSTR URL)
{

};

void CMainMenu::OnNoNewPatchFound				()
{

}

void CMainMenu::OnDownloadPatch(CUIWindow*, void*)
{


}

void	CMainMenu::OnDownloadPatchError()
{

};

void	CMainMenu::OnDownloadPatchSuccess			()
{

}

void CMainMenu::OnSessionTerminate(LPCSTR reason)
{
	if ( m_NeedErrDialog == SessionTerminate && (Device.dwTimeGlobal - m_start_time) < 8000 )
		return;

	m_start_time = Device.dwTimeGlobal;
	CStringTable	st;
	LPCSTR str = st.translate("ui_st_kicked_by_server").c_str();
	LPSTR		text;

	if ( reason && xr_strlen(reason) && reason[0] == '@' )
	{
		STRCONCAT( text, reason + 1 );
	}
	else
	{
		STRCONCAT( text, str, " ", reason );
	}
	
	m_pMB_ErrDlgs[SessionTerminate]->SetText(st.translate(text).c_str());
	SetErrorDialog(CMainMenu::SessionTerminate);
}

void	CMainMenu::OnLoadError(LPCSTR module)
{
	LPCSTR str=CStringTable().translate("ui_st_error_loading").c_str();
	string1024 Text;
	strconcat(sizeof(Text),Text,str," ");
	xr_strcat(Text,sizeof(Text),module);
	m_pMB_ErrDlgs[LoadingError]->SetText(Text);
	SetErrorDialog(CMainMenu::LoadingError);
}

void	CMainMenu::OnDownloadPatchProgress			(u64 bytesReceived, u64 totalSize)
{
	m_sPDProgress.Progress = (float(bytesReceived)/float(totalSize))*100.0f;
};

extern ENGINE_API string512  g_sLaunchOnExit_app;
extern ENGINE_API string512  g_sLaunchOnExit_params;
void	CMainMenu::OnRunDownloadedPatch			(CUIWindow*, void*)
{
	xr_strcpy					(g_sLaunchOnExit_app,*m_sPatchFileName);
	xr_strcpy					(g_sLaunchOnExit_params,"");
	Console->Execute		("quit");
}

void CMainMenu::CancelDownload()
{

}

void CMainMenu::SetNeedVidRestart()
{
	m_Flags.set(flNeedVidRestart,TRUE);
}

void CMainMenu::OnDeviceReset()
{
	if(IsActive() && g_pGameLevel)
		SetNeedVidRestart();
}

extern	void	GetCDKey(char* CDKeyStr);
//extern	int VerifyClientCheck(const char *key, unsigned short cskey);

bool CMainMenu::IsCDKeyIsValid()
{

	return true;
}

bool		CMainMenu::ValidateCDKey					()
{
	if (IsCDKeyIsValid()) return true;
	SetErrorDialog(CMainMenu::ErrCDKeyInvalid);
	return false;
}

void		CMainMenu::Show_CTMS_Dialog				()
{
	if (!m_pMB_ErrDlgs[ConnectToMasterServer]) return;
	if (m_pMB_ErrDlgs[ConnectToMasterServer]->IsShown()) return;
	m_pMB_ErrDlgs[ConnectToMasterServer]->ShowDialog(false);
}

void		CMainMenu::Hide_CTMS_Dialog()
{
	if (!m_pMB_ErrDlgs[ConnectToMasterServer]) return;
	if (!m_pMB_ErrDlgs[ConnectToMasterServer]->IsShown()) return;
	m_pMB_ErrDlgs[ConnectToMasterServer]->HideDialog();
}

void CMainMenu::OnConnectToMasterServerOkClicked(CUIWindow*, void*)
{
	Hide_CTMS_Dialog();
}

LPCSTR CMainMenu::GetGSVer()
{
	// Empty string, because we have no GameSpy.
	return "";
}

void CMainMenu::ReportTxrsForPrefetching()
{
	if (SuggestedForPrefetching.size() > 0){
		Msg("---These UI textures are suggested to be prefetched since they caused stutterings when some UI window was loading");
		Msg("---Add this list to prefetch_ui_textures.ltx (wisely)");
		for (u32 i = 0; i < SuggestedForPrefetching.size(); i++){
			Msg("%s", SuggestedForPrefetching[i].c_str());
		}
	}
}
