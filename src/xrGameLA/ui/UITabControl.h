#pragma once

#include "uiwindow.h"
#include "../../xrScripts/script_export_space.h"
#include "UIOptionsItem.h"

class CUITabButton;
class CUIButton;

using TABS_VECTOR = xr_vector<CUITabButton*>;
using TABS_VECTOR_it = TABS_VECTOR::iterator;

class CUITabControl: public CUIWindow, public CUIOptionsItem
{
	typedef				CUIWindow inherited;
public:
						CUITabControl				();
	virtual				~CUITabControl				();

	// options item
	virtual void		SetCurrentOptValue				();
	virtual void		SaveOptValue					();
	virtual bool		IsChangedOptValue					() const;
	virtual void		SaveBackUpOptValue			();
	virtual void		UndoOptValue				();

	virtual bool		OnKeyboardAction					(int dik, EUIMessages keyboard_action);
	virtual void		OnTabChange					(int iCur, int iPrev);
	virtual void		OnStaticFocusReceive		(CUIWindow* pWnd);
	virtual void		OnStaticFocusLost			(CUIWindow* pWnd);

	// Добавление кнопки-закладки в список закладок контрола
	bool				AddItem						(LPCSTR pItemName, LPCSTR pTexName, float x, float y, float width, float height);
	bool				AddItem						(CUITabButton *pButton);

	void				RemoveItem					(const u32 Index);
	void				RemoveAll					();

	virtual void		SendMessage					(CUIWindow *pWnd, s16 msg, void *pData);

			int			GetActiveIndex				()	const								{ return m_iPushedIndex; }
			int			GetPrevActiveIndex			()	const								{ return m_iPrevPushedIndex; }
			void		SetNewActiveTab				(const int iNewTab);	
	const	int			GetTabsCount				() const						{ return m_TabsArr.size(); }
	
	// Режим клавилатурных акселераторов (вкл/выкл)
	IC bool				GetAcceleratorsMode			() const						{ return m_bAcceleratorsEnable; }
	void				SetAcceleratorsMode			(bool bEnable)					{ m_bAcceleratorsEnable = bEnable; }


	TABS_VECTOR *		GetButtonsVector			()								{ return &m_TabsArr; }
	CUIButton*			GetButtonByIndex			(int i);
	const shared_str	GetCommandName				(int i);
	CUIButton*			GetButtonByCommand			(const shared_str& n);
			void		ResetTab					();
protected:
	// Список кнопок - переключателей закладок
	TABS_VECTOR			m_TabsArr;

	// Текущая нажатая кнопка. -1 - ни одна, 0 - первая, 1 - вторая, и т.д.
	int					m_iPushedIndex;
	int					m_iPrevPushedIndex;

	// Цвет неактивных элементов
	u32					m_cGlobalTextColor;
	u32					m_cGlobalButtonColor;

	// Цвет надписи на активном элементе
	u32					m_cActiveTextColor;
	u32					m_cActiveButtonColor;

	// Разрешаем/запрещаем клавиатурные акселераторы
	bool				m_bAcceleratorsEnable;
	int					m_opt_backup_value;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
