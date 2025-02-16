#pragma once
#include "UIFrameLineWnd.h"

class CUITextWnd;
class CUIStatic;

class CUIListBoxItem : public CUIFrameLineWnd, public CUISelectable
{
	friend	class		CUITreeViewBoxItem;
	typedef				CUIFrameLineWnd inherited;
public:
						CUIListBoxItem			(void);

	virtual void		Draw					();
	virtual bool		OnMouseDown				(int mouse_btn);
	virtual void		OnFocusReceive			();
	virtual void		SetSelected				(bool b);

			void		InitDefault				();
			void		SetTAG					(u32 value);
			u32			GetTAG					();

			void		SetData					(void* data);
			void*		GetData					();

		CUITextWnd*		AddTextField			(LPCSTR txt, float width);
		CUIStatic*		AddIconField			(float width);

		CUITextWnd*		GetTextItem				()								{return m_text;}

		//TextControl
		void			SetText					(LPCSTR txt);
		LPCSTR			GetText					();
		void			SetTextColor			(u32 color, u32 color_s);
		void			SetTextColor			(u32 color);
		u32				GetTextColor			();
		void			SetFont					(CGameFont* F);
		CGameFont*		GetFont					();
		void			SetTextAlignment		(ETextAlignment alignment);
		ETextAlignment	GetTextAlignment		();

protected:
		CUITextWnd*		m_text;
		u32				txt_color;
		u32				txt_color_s;

		u32				tag;

		//skyloader: время и фрейм для отсчета даблклика
		u32				m_dwLastClickTime;
		u32				m_dwLastClickFrame;

		void*			pData;
		float			FieldsLength			() const;
};

