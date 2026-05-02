#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../encyclopedia_article_defs.h"
#include "UIPdaAux.h"
#include "PdaUiSound.h"

class CInventoryOwner;
class CUIFrameLineWnd;
class CUI3tButton;
class CUITabControl;
class CUIStatic;
class CUIXml;
class CUIFrameWindow;
class UIHint;

class CUITaskWnd;
class CUIFactionWarWnd;
class CUIRankingWnd;
class CUILogsWnd;
class CUIAnimatedStatic;
class UIHint;
class CMapSpot;
class CUIPdaContactsWnd;
class CUIEventsWnd;
class CUIStalkersRankingWnd;
class CUIEncyclopediaWnd;
class CUIActorInfoWnd;
class CUIDiaryWnd;
class CUIMapWnd;
class CUIPdaSpot;

class CUIPdaWnd final : public CUIDialogWnd
{
	typedef CUIDialogWnd	inherited;
protected:
	CUITabControl*			UITabControl;
	CUI3tButton*			m_btn_close;

	CUIStatic*				UIMainPdaFrame;
	CUIStatic*				m_updatedSectionImage;
	CUIStatic*				m_oldSectionImage;
	CUIStatic*				UINoice;
	
	CUIStatic*				m_caption;
	CUIStatic*				m_captionDate;
	CUIStatic*				m_captionLocation;
	shared_str				m_caption_const;
	shared_str				m_prevDateTimeValue;
	bool					m_captionGameDateTime = false;
	bool					m_captionShowLocationName = false;
	CUIAnimatedStatic*		m_anim_static;
	CUIStatic*				m_clock;
	CUIWindow*				m_pTabBgLayer;
	xr_map<shared_str, CUIStatic*> m_tabBackgrounds;
	CUIStatic*				m_pCurrentTabBackground;

	// Currently visible PDA page (native subdialog or script-provided window).
	CUIWindow*				m_pActiveDialog;
	shared_str				m_sActiveSection;
	xr_vector<Fvector2>		m_sign_places_main;
	xr_vector<pda_section::part> m_updateBadgeSections;

	UIHint*					m_hint_wnd;

	CUIFrameLineWnd*		UIMainButtonsBackground;
	CUIFrameLineWnd*		UITimerBackground;
	CPdaUiSounds			m_uiSounds;

	void					UpdateDateTime					(bool force = false);
	void					UpdateLocationName				();
	void					SetCaptionWithOptionalLocation	(const char* baseText);
	void					DrawUpdatedSections				();
	void					BuildUpdateBadgeSections		();
	void					InitTabBackgrounds				(CUIXml& xml);
	void					SetActiveTabBackground			(const shared_str& sectionId);
	CUIWindow*				ResolveNativeSubdialog			(const shared_str& resolvedSection);
	void					ApplyActiveSubdialog			(const shared_str& tabButtonSection, const shared_str& resolvedSection);
private:
	bool m_isSetActiveSubdialog = false;
	const char* m_onSetActiveSubdialog = {};

public:
	CUITaskWnd*				pUITaskWnd;
	CUIFactionWarWnd*		pUIFactionWarWnd;
	CUIRankingWnd*			pUIRankingWnd;
	CUILogsWnd*				pUILogsWnd;
	CUIPdaContactsWnd*		UIPdaContactsWnd;
	CUIEventsWnd*			pUIEventsWnd;
	CUIStalkersRankingWnd*	pUIStalkersRankingWnd;
	CUIEncyclopediaWnd*		pUIEncyclopediaWnd;
	CUIActorInfoWnd*		pUIActorInfoWnd;
	CUIDiaryWnd*			pUIDiaryWnd;
	CUIMapWnd*				pUIMapWnd;

	CMapSpot*				pSelectedMapSpot;
	Fvector2				last_cursor_pos;

	virtual void			Reset				();
	virtual CUIWindow* ui_cast_window() { return this; }

public:
							CUIPdaWnd			();
	virtual					~CUIPdaWnd			();

	void ResetCursor();

	virtual void 			Init				();

	virtual void 			SendMessage			(CUIWindow* pWnd, s16 msg, void* pData = nullptr);

	virtual void 			Draw				();
	virtual void 			Update				();
	virtual void 			Show				(bool status);
	virtual bool			OnMouseAction		(float x, float y, EUIMessages mouse_action) override;
	virtual bool			OnKeyboardAction	(int dik, EUIMessages keyboard_action);
	virtual bool			OnGamepadKeyAction	(int key, EUIMessages gamepad_action);
	virtual bool			OnGamepadKeyHold	(int key);
			UIHint*			get_hint_wnd		() const { return m_hint_wnd; }
			CPdaUiSounds&	UiSounds			() { return m_uiSounds; }
			const CPdaUiSounds& UiSounds		() const { return m_uiSounds; }
			void			DrawHint			();

			void			SetActiveCaption	();
			void			SetCaption			(const char* text);
			void			Show_SecondTaskWnd	(bool status);
			void			Show_MapLegendWnd	(bool status);

			void 			SetActiveDialog		(CUIWindow* pUI) 	{ m_pActiveDialog = pUI; };
			CUIWindow*		GetActiveDialog		() 					{return m_pActiveDialog;};
			const char*			GetActiveSection	()					{return m_sActiveSection.c_str();};
			CUITabControl*	GetTabControl		()					{return UITabControl;};
	
			virtual void HideDialog() override;

			void			SetActiveSubdialog	(const shared_str& section);
			void			SetActiveSubdialog_script(const char* section)				{ SetActiveSubdialog((const shared_str&)section); };
	virtual bool			StopAnyMove			();
			void			PdaContentsChanged	(pda_section::part type);

			void			UpdatePda			();
			void			UpdateRankingWnd	();
			void			ReloadGamepadLegends();
			CUIPdaSpot*		GetActiveUserSpotWnd();
public:

DECLARE_SCRIPT_REGISTER_FUNCTION
};
