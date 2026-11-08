#pragma once
#include "../xrEngine/xr_level_controller.h"
#include "../xrCore/FormatParsers/XML/Expression.h"
class CUIWindow;

#define	 ui_list xr_vector
#define DEF_UILIST(N,T)		typedef ui_list< T > N;			typedef N::iterator N##_it;

//////////////////////////////////////////////////////////////////////////

#include "UIMessages.h"
#include "../../xrScripts/script_export_space.h"
#include "../uiabstract.h"
#include "UIActionRepeaters.h"
#include "../../Include/xrRender/UITransform.h"

class CUIStatic;
class CUICellItem;
class CUIListBoxItem;
class CUIListItem;
class CUIListWnd;
class ITextureOwner;
class CUILightAnimColorConroller;
class CUITreeViewItem;
class CUIScrollView;
class CUISelectable;
class CUIListItemServer;
class CUIProgressShape;
class CUIGamepadLegend;
class CDialogHolder;

namespace luabind
{
	template <typename TResult> class functor;
}

class UI_API CUIWindow  : 
	public CUISimpleWindow,
	public CUIActionRepeatersOwner
{
private:
	friend class CDialogHolder;
	xr_vector<CDialogHolder*> DialogHolders;
	void RegisterDialogHolder(CDialogHolder* Holder);
	void UnregisterDialogHolder(CDialogHolder* Holder);
	struct SelfCallbackState;
	xr_shared_ptr<SelfCallbackState> SelfCallbacks;
	bool DispatchSelfCallbacks(s16 Event);
	float TreeHeading = 0.0f;
	float TargetTreeHeading = 0.0f;
	double TreeHeadingProgress = 0.0;
	float SmoothRotateFactor = 1.0f;
	bool SmoothRotateEnabled = true;
	bool TreeHeadingPending = false;
	bool RotationSmoothingPending = false;
	u32 LastSmoothRotateFrame = u32(-1);
	void UpdateSmoothRotation();
	Fvector2 TreePivot = {0.0f, 0.0f};
	Fvector2 TreeOffset = {0.0f, 0.0f};
	bool RotationEnabled = true;
	bool UseAspectRatioCorrection = false;
	UITransform2D GetLocalRotationTransform();
	UITransform2D BuildTreeTransform(const UITransform2D& Parent);
	UITransform2D GetLocalTreeTransform();
	CUIWindow* PickWindowAt(const Fvector2& Position, bool& Reliable);
	xr_vector<CUIWindow*> ChildDrawOrder;
	bool MoveChildInDrawOrder(CUIWindow* Child, bool Front);
	void RemoveChildDrawOrder(CUIWindow* Child);

public:
	void AddSelfCallback(s16 Event, const luabind::functor<void>& Handler);
	bool SendMessageToTarget(s16 Event, void* Data = nullptr);
	void DetachFromDialogHolders();
	bool ToFront();
	bool ToBack();
	void SetTreeHeading(float Angle, const Fvector2& Pivot, const Fvector2& Offset);
	void SetSmothRotate(bool Enabled, float Factor);
	void SetUseAspectRatioCorrection(bool Enabled) { UseAspectRatioCorrection = Enabled; }
	bool IsAspectRatioCorrectionActive() const;
	virtual void EnableHeading(bool Enabled) { RotationEnabled = Enabled; }
	bool IsHeadingEnabled() const { return RotationEnabled; }
	UITransform2D GetTreeTransform();
	bool HitTest(const Fvector2& Position);
	void DrawWithTransform();
	bool DispatchMouseAction(float X, float Y, EUIMessages Action);
				CUIWindow						();
	virtual		~CUIWindow						();


	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUIStatic* ui_cast_static() { return nullptr; }
	virtual CUICellItem* ui_cast_cell_item() { return nullptr; }
	virtual CUIListBoxItem* ui_cast_list_box_item() { return nullptr; }
	virtual CUIListItem* ui_cast_list_item() { return nullptr; }
	virtual CUIListWnd* ui_cast_list() { return nullptr; }
	virtual ITextureOwner* ui_cast_texture_owner() { return nullptr; }
	virtual CUILightAnimColorConroller* ui_cast_light_anim_color_controller() { return nullptr; }
	virtual CUITreeViewItem* ui_cast_tree_view_item() { return nullptr; }
	virtual CUIScrollView* ui_cast_scroll_view() { return nullptr; }
	virtual CUISelectable* ui_cast_selectable() { return nullptr; }
	virtual CUIListItemServer* ui_cast_list_item_server() { return nullptr; }
	virtual CUIProgressShape* ui_cast_progress_shape() { return nullptr; }
	virtual CUIGamepadLegend* ui_cast_gamepad_legend() { return nullptr; }

	////////////////////////////////////
	//работа с дочерними и родительскими окнами
	virtual void			AttachChild			(CUIWindow* pChild);
	virtual void			DetachChild			(CUIWindow* pChild);
	virtual bool			TryDetachChild		(CUIWindow* pChild);
	virtual bool			IsChild				(CUIWindow* pChild) const;
	virtual void			DetachAll			();
	int						GetChildNum			() 
	{
		xrCriticalSectionGuard guard(csUi);
		return (int)m_ChildWndList.size();
	}

	void					SetParent			(CUIWindow* pNewParent);
	CUIWindow*				GetParent			()	const							{return m_pParentWnd;}
	
	//получить окно самого верхнего уровня
	CUIWindow*				GetTop				()								{if(m_pParentWnd == NULL) return  this; 
																				else return  m_pParentWnd->GetTop();}
	CUIWindow*				GetCurrentMouseHandler();
	CUIWindow*				GetChildMouseHandler();


	virtual bool			OnKeyboardAction	(int dik, EUIMessages keyboard_action);
	virtual bool			OnKeyboardHold		(int dik);

	virtual bool 			OnMouseAction				(float x, float y, EUIMessages mouse_action);
	virtual void 			OnMouseMove			();
	virtual void 			OnMouseScroll		(float iDirection);
	virtual bool 			OnDbClick			();
	virtual bool 			OnMouseDown			(int mouse_btn);
	virtual void 			OnMouseUp			(int mouse_btn);

	virtual bool			OnGamepadKeyAction	(int key, EUIMessages gamepad_action);
	virtual bool			OnGamepadStickAction(int key, Fvector2 value, EUIMessages gamepad_action);
	virtual bool			OnGamepadKeyHold	(int key);
	virtual bool			OnGyroscopeAction	(Fvector3 value);
	virtual bool			OnTouchpadAction	(Fvector2 value);

	virtual void 			OnFocusReceive		();
	virtual void 			OnFocusLost			();
	
	//захватить/освободить мышь окном
	//сообщение посылается дочерним окном родительскому
	void					SetCapture			(CUIWindow* pChildWindow, bool capture_status);
	CUIWindow*				GetMouseCapturer	()													{return m_pMouseCapturer;}

	//окошко, которому пересылаются сообщения,
	//если NULL, то шлем на GetParent()
	void					SetMessageTarget	(CUIWindow* pWindow)								{m_pMessageTarget = pWindow;}
	CUIWindow*				GetMessageTarget	();

			void			SetKeyboardCapture	(CUIWindow* pChildWindow, bool capture_status);

	//обработка сообщений не предусмотреных стандартными обработчиками
	//ф-ция должна переопределяться
	//pWnd - указатель на окно, которое послало сообщение
	//pData - указатель на дополнительные данные, которые могут понадобиться
	virtual void			SendMessage			(CUIWindow* pWnd, s16 msg, void* pData = NULL);
	
	

	virtual void			Enable				(bool status)									{m_bIsEnabled=status;}
			bool			IsEnabled			()												{return m_bIsEnabled;}

	//убрать/показать окно и его дочерние окна
	virtual void			Show				(bool status)									{SetVisible(status); Enable(status); }
	IC		bool			IsShown				()												{return GetVisible();}
			void			ShowChildren		(bool show);
	
	//абсолютные координаты
	void					GetAbsoluteRect		(Frect& r) ;
	IC void					GetAbsolutePos		(Fvector2& p) 	{Frect abs; GetAbsoluteRect(abs); p.set(abs.x1,abs.y1);}


			void			SetWndRect_script	(Frect rect)										{CUISimpleWindow::SetWndRect(rect);}
			void			SetWndPos_script	(Fvector2 pos)										{CUISimpleWindow::SetWndPos(pos);}
			void			SetWndSize_script	(Fvector2 size)										{CUISimpleWindow::SetWndSize(size);}

	//прорисовка окна
	virtual void			Draw				();
	virtual void			Draw				(float x, float y);
	//обновление окна передпрорисовкой
	virtual void			Update				();


			void			SetPPMode			();
			void			ResetPPMode			();
	IC		bool			GetPPMode			()		{return m_bPP;};
	//для перевода окна и потомков в исходное состояние
	virtual void			Reset				();
			void			ResetAll			();


	virtual void			SetFont				(CGameFont* pFont)			{ m_pFont = pFont;}
	CGameFont*				GetFont				()							{if(m_pFont) return m_pFont;
																				if(m_pParentWnd== NULL)	
																					return  m_pFont;
																				else
																					return  m_pParentWnd->GetFont();}

	using WINDOW_LIST = xr_vector<CUIWindow*>;
	using WINDOW_LIST_it = WINDOW_LIST::iterator;
	const WINDOW_LIST& GetChildDrawOrder() const { return ChildDrawOrder.empty() ? m_ChildWndList : ChildDrawOrder; }
	bool HasCustomChildDrawOrder() const { return !ChildDrawOrder.empty(); }

	WINDOW_LIST&			GetChildWndList		()							{return m_ChildWndList; }


	IC bool					IsAutoDelete		()							{return m_bAutoDelete;}
	IC void					SetAutoDelete		(bool auto_delete)			{m_bAutoDelete = auto_delete;}

	// Name of the window
	const shared_str		WindowName			() const					{ return m_windowName; }
	void					SetWindowName		(const char* wn)					{ m_windowName = wn; }
	const char*					WindowName_script	()							{return m_windowName.c_str();}
	CUIWindow*				FindChild			(const shared_str name);
	template <typename T>
	T* FindChild_script(const char* name)
	{
		shared_str n = name;
		CUIWindow* pWnd = FindChild(n);
		if (pWnd == nullptr)
		{
			return nullptr;
		}

		return smart_cast<T*>(pWnd);
	}
	// Name of the window by node from XML
	const shared_str		WindowNodeName		() const					{ return m_windowNodeName; }
	void					SetWindowNodeName	(const char* wn)					{ m_windowNodeName = wn; }

	IC bool					CursorOverWindow	() const					{ return m_bCursorOverWindow; }
	bool CursorOverWindow(bool Precise);
	IC u32					FocusReceiveTime	() const					{ return m_dwFocusReceiveTime; }
	
	IC bool					GetCustomDraw		() const					{return m_bCustomDraw;}
	IC void					SetCustomDraw		(bool b) 					{m_bCustomDraw = b;}

public:
	// Expression that provide data for this widget
	CExpression             m_expression;
	xrCriticalSection csUi;
protected:
	bool ProcessMouseAction(float X, float Y, EUIMessages& MouseAction);
	virtual bool HitTestUntransformed(const Fvector2& Position);
	Fvector2 ToUntransformedPoint(const Fvector2& Position);
	bool ShouldSmoothRotate() const { return SmoothRotateEnabled && SmoothRotateFactor < 1.0f; }
	void QueueSmoothRotation() { RotationSmoothingPending = true; }
	static bool AdvanceSmoothHeading(double& Current, float Target, float Blend);
	IC void SafeRemoveChild(CUIWindow* child)
	{
		xrCriticalSectionGuard guard(csUi);
		WINDOW_LIST_it it = std::find(m_ChildWndList.begin(), m_ChildWndList.end(), child);
		if (it != m_ChildWndList.end())
		{
			m_ChildWndList.erase(it);
		}
		RemoveChildDrawOrder(child);
	}

	shared_str				m_windowName;
	shared_str				m_windowNodeName;
	//список дочерних окон
	WINDOW_LIST				m_ChildWndList;
	
	//указатель на родительское окно
	CUIWindow*				m_pParentWnd;

	//дочернее окно которое, захватило ввод мыши
	CUIWindow*				m_pMouseCapturer;
	
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

#ifdef DEBUG
	int m_dbg_id;
#endif

public:
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

UI_API bool fit_in_rect(CUIWindow* w, Frect const& vis_rect, float border = 0.0f, float dx16pos = 0.0f );
UI_API bool fit_infownd_in_rect(CUIWindow* wInfo, Frect & stick_to_rect, Frect fit_in_rect, float border, float dx16pos = 0.0f);
