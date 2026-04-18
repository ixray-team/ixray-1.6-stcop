#pragma once

#include "UIWindow.h"
#include "UIOptionsItem.h"

class CUITabButton;

using TABS_VECTOR = xr_vector<CUITabButton*>;
using TABS_VECTOR_it = TABS_VECTOR::iterator;

class UI_API CUITabControl: 
	public CUIWindow , 
	public CUIOptionsItem
{
	typedef				CUIWindow inherited;
public:
						CUITabControl				();
	virtual				~CUITabControl				();

	// options item
	virtual void		SetCurrentOptValue			();	// opt->current
	virtual void		SaveBackUpOptValue			();	// current->backup
	virtual void		SaveOptValue				();	// current->opt
	virtual void		UndoOptValue				();	// backup->current
	virtual bool		IsChangedOptValue			() const;	// backup!=current

	virtual bool		OnKeyboardAction			(int dik, EUIMessages keyboard_action);
	virtual void		OnTabChange					(const shared_str& sCur, const shared_str& sPrev);
	virtual void		OnStaticFocusReceive		(CUIWindow* pWnd);
	virtual void		OnStaticFocusLost			(CUIWindow* pWnd);

	// Добавление кнопки-закладки в список закладок контрола
	bool				AddItem						(const char* pItemName, const char* pTexName, Fvector2 pos, Fvector2 size);
	bool				AddItem						(CUITabButton *pButton);
	void				RemoveItemById				(const shared_str& id);
	void				RemoveItemById_script		(const char* id)						{ RemoveItemById(id); }
	void				RemoveItemByIndex			(u32 index);

	void				RemoveAll					();

	virtual void		SendMessage					(CUIWindow *pWnd, s16 msg, void *pData);
	virtual void		Enable						(bool status);

	const shared_str&	GetActiveId					()	const						{ return m_sPushedId; }
	const char*				GetActiveId_script			();
			int			GetActiveIndex				() const;
	const shared_str&	GetPrevActiveId				()								{ return m_sPrevPushedId; }
			void		SetActiveTab				(const shared_str& sNewTab);
			void		SetActiveTab_script			(const char* sNewTab)				{SetActiveTab(sNewTab);};
			void		SetActiveTabByIndex			(u32 index);
	const	u32			GetTabsCount				() const						{ return (u32)m_TabsArr.size(); }

			void		NextTab						(bool bAllowLoop = false);
			void		PrevTab						(bool bAllowLoop = false);

	// Режим клавилатурных акселераторов (вкл/выкл)
	IC bool				GetAcceleratorsMode			() const						{ return m_bAcceleratorsEnable; }
	void				SetAcceleratorsMode			(bool bEnable)					{ m_bAcceleratorsEnable = bEnable; }


	TABS_VECTOR *		GetButtonsVector			()								{ return &m_TabsArr; }
	CUITabButton*		GetButtonById				(const shared_str& id);
	CUITabButton*		GetButtonById_script		(const char* s)						{ return GetButtonById(s);}
	CUITabButton*		GetButtonByIndex			(u32 index) const;

	void		ResetTab					();

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	// Список кнопок - переключателей закладок
	TABS_VECTOR			m_TabsArr;

	shared_str			m_sPushedId;
	shared_str			m_sPrevPushedId;

	// Цвет неактивных элементов
	u32					m_cGlobalTextColor;
	u32					m_cGlobalButtonColor;

	// Цвет надписи на активном элементе
	u32					m_cActiveTextColor;
	u32					m_cActiveButtonColor;

	bool				m_bAcceleratorsEnable;
	shared_str			m_opt_backup_value;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
