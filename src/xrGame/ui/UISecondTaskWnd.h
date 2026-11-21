////////////////////////////////////////////////////////////////////////////
//	Module 		: UISecondTaskWnd.h
//	Created 	: 30.05.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Secondary Task Wnd class
////////////////////////////////////////////////////////////////////////////

#ifndef UI_SECOND_TASK_WND_H_INCLUDED
#define UI_SECOND_TASK_WND_H_INCLUDED

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIWndCallback.h"

#define PDA_TASK_XML	"pda_tasks.xml"

class CUIXml;
class CUIFrameWindow;
class CUIScrollView;
class CUIStatic;
class CUI3tButton;
class CUITabControl;
class CUICheckButton;
class CUIFrameLineWnd;
class CGameTask;
class CUITaskItem;
class UIHint;
class CPdaUiSounds;
class CUIHotkeysHint;

enum class ETaskListFilter : u8
{
	All = 0,
	Story,
	Side,
	Active,
	Completed,
	Failed,
	Archive,
	Count
};

class UITaskListWnd final : public CUIWindow, public CUIWndCallback
{
private:
	typedef CUIWindow	inherited;

public:
					UITaskListWnd		();
	virtual ~UITaskListWnd		() override;

			void	init_from_xml		( CUIXml& xml, const char* path );

	virtual bool	OnMouseAction		( float x, float y, EUIMessages mouse_action ) override;
	virtual void 	OnMouseScroll		(float iDirection) override;
	virtual bool	OnGamepadKeyAction	(int id, EUIMessages gamepad_action) override;
	virtual bool	OnGamepadKeyHold	(int id) override;
	virtual void	Show				( bool status ) override;
	virtual void 	OnFocusReceive		() override;
	virtual void	OnFocusLost			() override;
	virtual void	Update				() override;
	virtual void	SendMessage			( CUIWindow* pWnd, s16 msg, void* pData ) override;
			void	SetFilterMode		(ETaskListFilter mode);
			void	SetUiSounds			(CPdaUiSounds* uiSounds) { m_pUiSounds = uiSounds; }
			ETaskListFilter GetFilterMode() const { return m_filter; }
			bool	HasFilterTabs		() const { return m_filter_tabs != nullptr; }
			void	UpdateStorylineTask	(CGameTask* task);
			CUITaskItem* GetStorylineTaskItem() const { return _storylineTaskItem; }

			void	UpdateList			();
			bool	SwitchFilterTab		(bool Next);

	virtual CUIWindow* ui_cast_window() override { return this; }

protected:
	void 			OnBtnClose			( CUIWindow* w, void* d);
	void			OnStorylineTaskFocus(CUIWindow* w, void* d);
	bool 			SortingLessFunction	( CUIWindow* left, CUIWindow* right );
	bool			SelectNextToSelected( bool bNext );

//			void	UpdateCounter		();
public:
	UIHint*				hint_wnd;

	static ETaskListFilter	FilterFromTabId		(const shared_str& TabId);
	static const char*		TabIdFromFilter		(ETaskListFilter Filter);
	static bool				IsFinishedFilter	(ETaskListFilter Filter);
	static bool				TaskMatchesFilter	(const CGameTask* Task, ETaskListFilter Filter);

private:
	CGameTask*			StorylineTask		() const;
	void				UpdateStorylineTaskFocus();

	u32						CountTasks			(ETaskListFilter Filter) const;
	void					UpdateFilterTabCounters();
	void					UpdateEmptyText		(bool Empty);
	CUIHotkeysHint*			CreateTabHint		(CUIXml& Xml, const char* Path);
	void					RefreshTabHints		();

private: // m_
	CUIFrameWindow*		m_background;
	CUIScrollView*		m_list;
	
	CUIStatic*			m_caption;
//	CUIStatic*			m_counter;
	CUI3tButton*		m_bt_close;
	CUITabControl*		m_filter_tabs;
	CUITaskItem*		_storylineTaskItem;
	CUI3tButton*		_btnStorylineTaskFocus;

//	u32					m_activ_task_count;
	float				m_orig_h;
	ETaskListFilter		m_filter;
	CPdaUiSounds*		m_pUiSounds = nullptr;

	struct SFilterTabText
	{
		shared_str Id;
		xr_string BaseText;
	};
	xr_vector<SFilterTabText> FilterTabTexts;
	CUIStatic*			EmptyText = nullptr;
	CUIHotkeysHint*		TabHintPrev = nullptr;
	CUIHotkeysHint*		TabHintNext = nullptr;
	bool				TabHintsControllerMode = false;
	shared_str			EmptyTextDefault;
	shared_str			EmptyTexts[static_cast<u32>(ETaskListFilter::Count)];

}; // class UITaskListWnd

// -------------------------------------------------------------------------------------------------

class UITaskListWndItem final : public CUIWindow
{
private:
	typedef CUIWindow	inherited;

public:
					UITaskListWndItem	();
	virtual ~UITaskListWndItem	() override = default;

			bool	init_task			( CGameTask* task, UITaskListWnd* parent );
		u32		get_priority_task	() const;
	CGameTask*		get_task			() { return m_task; }

	virtual void 	OnFocusReceive		() override;
	virtual void	OnFocusLost			() override;
	virtual void	Update				() override;
	virtual void	SendMessage			( CUIWindow* pWnd, s16 msg, void* pData ) override;
	virtual bool	OnMouseAction				( float x, float y, EUIMessages mouse_action ) override;

	virtual CUIWindow* ui_cast_window() override { return this; }

			void	showHint			();
			void	hide_hint			();
private:
			void	update_view			();
			void	update_visible_map_spot();

public:
			bool	show_hint_can;
			bool	show_hint;

private: // m_
	CGameTask*		m_task;
	CUI3tButton*	m_name;
	CUICheckButton*	m_bt_view;
	CUIStatic*		m_st_story;
	CUIStatic*		m_task_icon;
	CUI3tButton*	m_bt_focus;
	CUI3tButton*	m_btn_task_focus;
	bool			Finished = false;
	bool			Failed = false;
	CUIStatic*		m_st_status = nullptr;
	shared_str		StatusTextCompleted;
	shared_str		StatusTextFailed;

	enum
	{
		stt_activ = 0,
		stt_unread,
		stt_read,
		stt_completed,
		stt_failed,
		stt_count
	};
	u32				m_color_states[stt_count];

}; // class UITaskListWndItem

#endif // UI_SECOND_TASK_WND_H_INCLUDED
