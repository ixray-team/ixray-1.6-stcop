#pragma once
#include "../xrUI/Widgets/UIButton.h"

class CUIListItem :	public CUIButton
{
private:
	typedef CUIButton inherited;
public:
	CUIListItem(void);
	virtual ~CUIListItem(void);

	virtual void Init(float x, float y, float width, float height);
	virtual void Init(const char* str, float x, float y, float width, float height);
	virtual void InitTexture(LPCSTR tex_name);

	virtual bool	OnMouseAction				(float x, float y, EUIMessages mouse_action);
	
			void* GetData() {return m_pData;}
			void SetData(void* pData) { m_pData = pData;}

			int GetIndex() {return m_iIndex;}
			void SetIndex(int index) {m_iIndex = index; m_iGroupID = index;}

			int GetValue() {return m_iValue;}
			void SetValue(int value) {m_iValue = value;}

			int	GetGroupID() { return m_iGroupID; }
			void SetGroupID(int ID) { m_iGroupID = ID; }

	virtual void	MarkSelected				(bool b){};
	// ïåðåîïðåäåëÿåì êðèòåðèé ïîäñâå÷èâàíèÿ òåêñòà
	virtual bool IsHighlightText();
	virtual void SetHighlightText(bool Highlight)		{ m_bHighlightText = Highlight; }

protected:
	//óêàçàòåëü íà ïðîèçâîëüíûå äàííûå, êîòîðûå ìîãóò
	//ïðèñîåäåíèåíû ê ýëåìåíòó
	void* m_pData;
	
	//ïðîèçâîëüíîå ÷èñëî, ïðèïèñàííîå îáúåêòó
	int m_iValue;
	
	//èíäåêñ â ñïèñêå
	int m_iIndex;

	// èäåíòèôèêàòîð ãðóïïû
	int m_iGroupID;

	// ïîäñâå÷èâàåòñÿ êíîïêà èëè íåò?
	bool m_bHighlightText;

};
