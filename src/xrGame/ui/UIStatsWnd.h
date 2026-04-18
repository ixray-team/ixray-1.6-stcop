#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIListWnd.h"
#include "../../xrUI/Widgets/UIListItem.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UIMultiTextStatic.h"

using FIELDS_VECTOR = xr_vector<CUI3tButton*>;
using FIELDS_VECTOR_it = FIELDS_VECTOR::iterator;

// Класс для определения нового члена списка
class CUIStatsListItem: public CUIListItem
{
	typedef CUIListItem inherited;
public:
	virtual ~CUIStatsListItem() {};
	void XmlInit(const char *path, CUIXml &uiXml);
	void Highlight(bool bHighlight);
	void SetSubItemColor(u32 uItemIndex, u32 uColor);

	// поля записи
	FIELDS_VECTOR FieldsVector;
};

class CUIStatsWnd: public CUIDialogWnd
{
private:
	typedef CUIDialogWnd inherited;

	string1024	XML_NAME;
public:
	CUIStatsWnd(const char* XML = nullptr);
	virtual ~CUIStatsWnd();

	virtual void Init(const char* XML = nullptr);
//	virtual void SendMessage(CUIWindow* pWnd, s16 msg, void* pData);

	// Добавить 1 элемент. Заполнить поля необходимо самостоятельно. Возвращает указатель 
	// на добавленный элемент
	CUIStatsListItem * AddItem();
	// Получить элемент, при п		омощи поиска в полях по строке. Можно искать
	// элемент начиная с заданного номера
	CUIStatsListItem * FindFrom(u32 beg_pos, const char *strCaption);
	// Удалить элемент в котором есть статик с текстом strCaption. В каждом Item'е поиск 
	// начать с позиции beg_pos
	void RemoveItemFrom(u32 beg_pos, const char *strCaption);
	// Подсветить нужный элемент
	void HighlightItem(u32 uItem);
	// Получить номер подсвеченого эл-та
	u32	GetHighlightedItem() { return m_uHighlightedItem; }
	// Выделить нужный элемент
	void SelectItem(u32 uItem);
	// Установить текст заголовка нужной колонки
	void SetHeaderColumnText(u32 headerItem, const shared_str &text);
	
	Frect GetFrameRect () { return UIFrameWnd.GetWndRect();};
	void RemoveItem (const u32 Index) {UIStatsList.RemoveItem(Index);};

	CUIFrameWindow*		GetFrameWindow	()	{return &UIFrameWnd;};
protected:
//	CUIButton			UIBtn;
	// Фрейм - оболочка
	CUIFrameWindow		UIFrameWnd;
	// Лист для отображения списка статичтики игроков
	CUIListWnd			UIStatsList;
	// Подсвеченый элемент
	u32					m_uHighlightedItem;
	// Заголовок
	CUIMultiTextStatic	UIHeader;
};
