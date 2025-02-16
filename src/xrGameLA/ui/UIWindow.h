#pragma once
#include "../../xrEngine/xr_level_controller.h"
class CUIWindow;

struct _12b	{ DWORD _[3]; };
extern poolSS< _12b, 128>	ui_allocator;


template <class T>
class	uialloc	{
public:
	typedef	size_t		size_type;
	typedef ptrdiff_t	difference_type;
	typedef T*			pointer;
	typedef const T*	const_pointer;
	typedef T&			reference;
	typedef const T&	const_reference;
	typedef T			value_type;

public:
	template<class _Other>	
	struct rebind			{	typedef uialloc<_Other> other;	};
public:
							pointer					address			(reference _Val) const					{	return (&_Val);	}
							const_pointer			address			(const_reference _Val) const			{	return (&_Val);	}
													uialloc			()										{	}
													uialloc			(const uialloc<T>&)						{	}
	template<class _Other>							uialloc			(const uialloc<_Other>&)					{	}
	template<class _Other>	uialloc<T>&				operator=		(const uialloc<_Other>&)					{	return (*this);	}
							pointer					allocate		(size_type n, const void* p=0) const	
							{	VERIFY(1==n);
								return (pointer) ui_allocator.create();	
							};
							char*			__charalloc		(size_type n)							
							{	VERIFY	(1==n);
								return	(char*) ui_allocator.create();	
							};
							void					deallocate		(pointer p, size_type n) const			
							{	
								VERIFY(1==n);
								_12b* p_ = (_12b*)p;
								ui_allocator.destroy	(p_);				
							}
							void					deallocate		(void* p, size_type n) const		
							{	
								VERIFY(1==n);
								_12b* p_ = (_12b*)p;
								ui_allocator.destroy	(p_);				
							}
							void					construct		(pointer p, const T& _Val)				{	::new(p) T(_Val);			}
							void					destroy			(pointer p)								{	p->~T();					}
							size_type				max_size		() const								{	size_type _Count = (size_type)(-1) / sizeof (T);	return (0 < _Count ? _Count : 1);	}
};
template<class _Ty,	class _Other>	inline	bool operator==(const uialloc<_Ty>&, const uialloc<_Other>&)		{	return (true);							}
template<class _Ty, class _Other>	inline	bool operator!=(const uialloc<_Ty>&, const uialloc<_Other>&)		{	return (false);							}

//. template<typename T>	
//. class	ui_list 		: public std::list<T,uialloc<T> >{ public: u32 size() const {return (u32)__super::size(); } };


#define	 ui_list xr_vector

#define DEF_UILIST(N,T)		typedef ui_list< T > N;			typedef N::iterator N##_it;

//////////////////////////////////////////////////////////////////////////

#include "UIMessages.h"
#include "../../xrScripts/script_export_space.h"
#include "uiabstract.h"


class CUIWindow  : public CUISimpleWindow, public CUIIndexableWindow
{
public:
				CUIWindow						();
	virtual		~CUIWindow						();


	////////////////////////////////////
	//работа с дочерними и родительскими окнами
	virtual u32				AttachChild			(CUIWindow* pChild, int pos = -1);
	virtual void			DetachChild			(CUIWindow* pChild);
	virtual bool			IsChild				(CUIWindow* pChild) const;
	virtual void			DetachAll			();
	int						GetChildNum			()								{return m_ChildWndList.size();} 

	void					SetParent			(CUIWindow* pNewParent);
	CUIWindow*				GetParent			()	const							{return m_pParentWnd;}
	
	//получить окно самого верхнего уровня
	CUIWindow*				GetTop				()								{if(m_pParentWnd == nullptr) return  this; 
																				else return  m_pParentWnd->GetTop();}
	CUIWindow*				GetCurrentMouseHandler();
	CUIWindow*				GetChildMouseHandler();


	virtual bool			OnKeyboardAction			(int dik, EUIMessages keyboard_action);
	virtual bool			OnKeyboardHold		(int dik);

	virtual bool 			OnMouseAction				(float x, float y, EUIMessages mouse_action);
	virtual void 			OnMouseMove			();
	virtual void 			OnMouseScroll		(float iDirection);
	virtual bool 			OnDbClick			();
	virtual bool 			OnMouseDown			(int mouse_btn);
	virtual void 			OnMouseUp			(int mouse_btn);

	virtual void 			OnFocusReceive		();
	virtual void 			OnFocusLost			();
		bool 			HasChildMouseHandler		();

	
	//захватить/освободить мышь окном
	//сообщение посылается дочерним окном родительскому
	void					SetCapture			(CUIWindow* pChildWindow, bool capture_status);
	CUIWindow*				GetMouseCapturer	()													{return m_pMouseCapturer;}

	//окошко, которому пересылаются сообщения,
	//если nullptr, то шлем на GetParent()
	void					SetMessageTarget	(CUIWindow* pWindow)								{m_pMessageTarget = pWindow;}
	CUIWindow*				GetMessageTarget	();

			void			SetKeyboardCapture	(CUIWindow* pChildWindow, bool capture_status);

	
	
	//обработка сообщений не предусмотреных стандартными обработчиками
	//ф-ция должна переопределяться
	//pWnd - указатель на окно, которое послало сообщение
	//pData - указатель на дополнительные данные, которые могут понадобиться
	virtual void			SendMessage			(CUIWindow* pWnd, s16 msg, void* pData = nullptr);
	
	

	//запрещение/разрешение на ввод с клавиатуры
	virtual void			Enable				(bool status)									{m_bIsEnabled=status;}
			bool			IsEnabled			()												{return m_bIsEnabled;}

	//убрать/показать окно и его дочерние окна
	virtual void			Show				(bool status)									{SetVisible(status); Enable(status); }
	IC		bool			IsShown				()												{return GetVisible();}
			void			ShowChildren		(bool show);
	
	//абсолютные координаты
	IC void					GetAbsoluteRect		(Frect& r) ;
	IC void					GetAbsolutePos		(Fvector2& p) 	{Frect abs; GetAbsoluteRect(abs); p.set(abs.x1,abs.y1);}


			void			Init_script		(float x, float y, float w, float h)	{CUISimpleWindow::SetWndPos(Fvector2().set(x, y));
														CUISimpleWindow::SetWndSize(Fvector2().set(w, h));}

			void			SetWndRect_script	(float x, float y, float w, float h)			{CUISimpleWindow::SetWndPos(Fvector2().set(x, y));
																									CUISimpleWindow::SetWndSize(Fvector2().set(w, h));}
			void			SetWndRect_script	(Frect rect)									{CUISimpleWindow::SetWndRect(rect);}
			void			SetWndPos_script	(float x, float y)								{CUISimpleWindow::SetWndPos(Fvector2().set(x, y));}
			void			SetWndSize_script	(float w, float h)								{CUISimpleWindow::SetWndSize(Fvector2().set(w, h));}
#ifdef DEBUG
	virtual void			SetDebugColor		(u32 color, u32 hcolor, u32 dcolor = 0xffffffff){m_debug_color[0] = color; m_debug_color[1] = hcolor; m_debug_color[2] = dcolor;}
protected:
	u32 m_debug_color[3];
public:
#endif
	//прорисовка окна
	virtual void			Draw				();
	virtual void			Draw				(float x, float y);
	//обновление окна передпрорисовкой
	virtual void			Update				();

	virtual void			BringAllToTop		();
	virtual	bool			BringToTop			(CUIWindow* pChild);

			void			SetPPMode			();
			void			ResetPPMode			();
	IC		bool			GetPPMode			()		{return m_bPP;};
	//для перевода окна и потомков в исходное состояние
	virtual void			Reset				();
			void			ResetAll			();

	virtual void			SetFont				(CGameFont* pFont)			{ m_pFont = pFont;}
	CGameFont*				GetFont				()							{if(m_pFont) return m_pFont;
																				if(m_pParentWnd== nullptr)	
																					return  m_pFont;
																				else
																					return  m_pParentWnd->GetFont();}


	DEF_UILIST				(WINDOW_LIST, CUIWindow*);
	WINDOW_LIST&			GetChildWndList		()							{return m_ChildWndList; }


	IC bool					IsAutoDelete		()							{return m_bAutoDelete;}
	IC void					SetAutoDelete		(bool auto_delete)			{m_bAutoDelete = auto_delete;}

	// Name of the window
	const shared_str		WindowName			() const					{ return m_windowName; }
	void					SetWindowName		(LPCSTR wn)					{ m_windowName = wn; }
	LPCSTR					WindowName_script	()							{return m_windowName.c_str();}
	CUIWindow*				FindChild			(const shared_str name);

	IC bool					CursorOverWindow	() const					{ return m_bCursorOverWindow; }
	IC u32					FocusReceiveTime	() const					{ return m_dwFocusReceiveTime; }
	
	IC bool					GetCustomDraw		() const					{return m_bCustomDraw;}
	IC void					SetCustomDraw		(bool b) 					{m_bCustomDraw = b;}

protected:
	IC void					SafeRemoveChild(CUIWindow* child)				{WINDOW_LIST_it it = std::find(m_ChildWndList.begin(),m_ChildWndList.end(),child); if(it!=m_ChildWndList.end())m_ChildWndList.erase(it);};

	shared_str				m_windowName;
	//список дочерних окон
	WINDOW_LIST				m_ChildWndList;
	
	//указатель на родительское окно
	CUIWindow*				m_pParentWnd;

	//дочернее окно которое, захватило ввод мыши
	CUIWindow*				m_pMouseCapturer;

	//кто изначально иницировал
	//захват фокуса, только он теперь
	//может весь фокус и освободить
	CUIWindow*				m_pOrignMouseCapturer;
	
	//дочернее окно которое, захватило ввод клавиатуры
	CUIWindow*				m_pKeyboardCapturer;

	//кому шлем сообщения
	CUIWindow*				m_pMessageTarget;

	CGameFont*				m_pFont;

	// Последняя позиция мышки
	Fvector2 cursor_pos;

	//время прошлого клика мышки
	//для определения DoubleClick
	u32						m_dwLastClickTime;
	u32						m_dwFocusReceiveTime;

	//флаг автоматического удаления во время вызова деструктора
	bool					m_bAutoDelete;

	bool					m_bPP;
	bool					m_bIsEnabled;

	// Если курсор над окном
	bool					m_bCursorOverWindow;
	bool					m_bCustomDraw;
	bool					m_bClickable;

#ifdef DEBUG
	int m_dbg_id;
#endif

public:
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

bool fit_in_rect(CUIWindow* w, Frect const& vis_rect, float border = 0.0f, float dx16pos = 0.0f );
#ifdef DEBUG
void draw_debug_rect(CUIWindow* w, u32 color = 0xffff0000);
#endif