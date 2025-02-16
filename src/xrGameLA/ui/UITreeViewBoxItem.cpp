//=============================================================================
//  Filename:   UITreeViewItem.cpp
//	Created by Roman E. Marchenko, vortex@gsc-game.kiev.ua
//	Copyright 2004. GSC Game World
//	---------------------------------------------------------------------------
//  TreeView Item class
//=============================================================================

#include "stdafx.h"
#include "UITreeViewBoxItem.h"
#include "UIListBox.h"
#include "../../xrEngine/string_table.h"


#define UNREAD_COLOR	0xff00ff00
#define READ_COLOR		0xffffffff

//////////////////////////////////////////////////////////////////////////

// Смещение относительно родителя
const int				_subShift = 1;
const char* const		treeItemBackgroundTexture = "ui\\ui_pda_over_list";
// Цвет непрочитанного элемента
static const u32		unreadColor = 0xff00ff00;

//////////////////////////////////////////////////////////////////////////

CUITreeViewBoxItem::CUITreeViewBoxItem()
	:	isRoot				(false),
		isOpened			(false),
		iTextShift			(0),
		pOwner				(NULL),
		m_uUnreadedColor	(UNREAD_COLOR),
		m_uReadedColor		(READ_COLOR),
		m_value				(-1),
		m_real_parent		(NULL)
{
	AttachChild(&UIBkg);
	UIBkg.InitTexture(treeItemBackgroundTexture);
	UIBkg.TextureOff();
	UIBkg.SetTextureOffset(-20, 0);
	m_bManualSetColor = false;
}

//////////////////////////////////////////////////////////////////////////

CUITreeViewBoxItem::~CUITreeViewBoxItem()
{
	Close();
	DeleteAllSubItems();
}

//////////////////////////////////////////////////////////////////////////

void CUITreeViewBoxItem::OnRootChanged()
{
	xr_string str;
	if (isRoot)
	{
		// Вставляем после последнего пробела перед текстом знак + или -
		str = GetText();

		xr_string::size_type pos = str.find_first_not_of(" ");
		if (xr_string::npos == pos) pos = 0;

		if (pos == 0)
		{
			++iTextShift;
			str.insert(0, " ");
		}
		else
			--pos;

		if (isOpened)
			// Add minus sign
			str.replace(pos, 1, "-");
		else
			// Add plus sign
			str.replace(pos, 1, "+");

		inherited::SetText(str.c_str());
	}
	else
	{
		str = GetText();
		// Remove "+/-" sign
		xr_string::size_type pos = str.find_first_of("+-");

		if (pos == 0)
		{
			for (int i = 0; i < iTextShift; ++i)
				str.insert(pos, " ");
		}
		else
			str.replace(pos, 1, " ");

		inherited::SetText(str.c_str());
		SetItemColor();
	}
}

//////////////////////////////////////////////////////////////////////////

void CUITreeViewBoxItem::OnOpenClose()
{
	// Если мы не являемся узлом дерева, значит ничего не делаем
	if (!isRoot) return;

	xr_string str;

	str = GetText();
	xr_string::size_type pos = str.find_first_of("+-");

	if (xr_string::npos != pos)
	{
		if (isOpened)
			// Change minus sign to plus
			str.replace(pos, 1, "-");
		else
			// Change plus sign to minus
			str.replace(pos, 1, "+");
	}

	inherited::SetText(str.c_str());
	SetItemColor();
}

//////////////////////////////////////////////////////////////////////////

void CUITreeViewBoxItem::Open()
{
	// Если не рут или уже открыты, то ничего не делаем
	if (!isRoot || isOpened) return;
	isOpened = true;

	// Изменяем состояние
	OnOpenClose();
	
	// Аттачим все подэлементы к родтельскому листбоксу
	CUIListBox *pList = smart_cast<CUIListBox*>(m_real_parent);
	
	R_ASSERT(pList);

	int pos = GetIndex();

	for (SubItems_it it = vSubItems.begin(); it != vSubItems.end(); ++it)
	{
		pList->AddExistingItem(*it, ++pos);
		(*it)->m_bAutoDelete = true;
	}
}

//////////////////////////////////////////////////////////////////////////

void CUITreeViewBoxItem::Close()
{
	// Если не рут или уже открыты, то ничего не делаем
	if (!isRoot || !isOpened) return;
	isOpened = false;

	// Изменяем состояние
	OnOpenClose();

	// Детачим все подэлементы
	CUIListBox *pList = smart_cast<CUIListBox*>(m_real_parent);

	R_ASSERT(pList);
	
	// Сначала все закрыть
	for (SubItems_it it = vSubItems.begin(); it != vSubItems.end(); ++it)
	{
		(*it)->Close();
	}

	// Затем все датачим
	for (SubItems_it it = vSubItems.begin(); it != vSubItems.end(); ++it)
	{
		(*it)->m_bAutoDelete = false;
		pList->Remove(*it);
	}
}

//////////////////////////////////////////////////////////////////////////

void CUITreeViewBoxItem::AddSubItem(CUITreeViewBoxItem *pItem)
{
	R_ASSERT(pItem);
	if (!pItem) return;

	pItem->SetTextShift(_subShift + iTextShift);

	vSubItems.push_back(pItem);
	pItem->SetOwner(this);
	pItem->SetText(pItem->GetText());
	pItem->SetItemColor();
}

//////////////////////////////////////////////////////////////////////////

void CUITreeViewBoxItem::DeleteAllSubItems()
{
	for (SubItems_it it = vSubItems.begin(); it != vSubItems.end(); ++it)
	{
		VERIFY(*it);
		CUIListBox *list = (*it)->m_real_parent;
		if (list)
			list->Remove(*it);

		//xr_delete(*it); // auto delete 
	}

	vSubItems.clear();
}

//////////////////////////////////////////////////////////////////////////

void CUITreeViewBoxItem::SetRoot(bool set)
{
	if (isRoot) return;
	
	isRoot = set;
	OnRootChanged();
}

//////////////////////////////////////////////////////////////////////////

void CUITreeViewBoxItem::SetText(LPCSTR str)
{
	xr_string s = str;
	xr_string::size_type pos = s.find_first_not_of(" +-");

	if (pos < static_cast<xr_string::size_type>(iTextShift))
	{
		for (u32 i = 0; i < iTextShift - pos; ++i)
			s.insert(0, " ");
	}
	else if (pos > static_cast<xr_string::size_type>(iTextShift))
	{
		s.erase(0, pos - iTextShift);
	}

	inherited::SetText(s.c_str());
	SetItemColor();
}

//////////////////////////////////////////////////////////////////////////

CUITreeViewBoxItem* CUITreeViewBoxItem::Find(LPCSTR text) const
{
	// Пробегаемся по списку подчиненных элементов, и ищем элемент с заданным текстом
	// Если среди подч. эл-тов есть root'ы, то ищем рекурсивно в них
	CUITreeViewBoxItem *pResult = NULL;
	xr_string caption;

	for (SubItems::const_iterator it = vSubItems.begin(); it != vSubItems.end(); ++it)
	{
		caption = (*it)->GetText();
		xr_string::size_type pos = caption.find_first_not_of(" +-");
		if (pos != xr_string::npos)
		{
			caption.erase(0, pos);
		}

		if (xr_strcmp(caption.c_str(), text) == 0)
			pResult = *it;

		if ((*it)->IsRoot() && !pResult)
			pResult = (*it)->Find(text);

		if (pResult) break;
	}

	return pResult;
}

//////////////////////////////////////////////////////////////////////////

CUITreeViewBoxItem * CUITreeViewBoxItem::Find(int value) const
{
	CUITreeViewBoxItem *pResult = NULL;

	for (SubItems::const_iterator it = vSubItems.begin(); it != vSubItems.end(); ++it)
	{
		if ((*it)->GetArticleValue() == value) pResult = *it;

		if ((*it)->IsRoot() && !pResult)
			pResult = (*it)->Find(value);

		if (pResult) break;
	}

	return pResult;
}

//////////////////////////////////////////////////////////////////////////

CUITreeViewBoxItem * CUITreeViewBoxItem::Find(CUITreeViewBoxItem * pItem) const
{
	CUITreeViewBoxItem *pResult = NULL;

	for (SubItems::const_iterator it = vSubItems.begin(); it != vSubItems.end(); ++it)
	{
		if ((*it)->IsRoot() && !pResult)
			pResult = (*it)->Find(pItem);
		else
			if (pItem == *it) pResult = *it;

		if (pResult) break;
	}

	return pResult;
}

//////////////////////////////////////////////////////////////////////////

xr_string CUITreeViewBoxItem::GetHierarchyAsText()
{
	xr_string name;

	if (GetOwner())
	{
		name = GetOwner()->GetHierarchyAsText();
	}

	xr_string::size_type prevPos = name.size() + 1;
	name += static_cast<xr_string>("/") + static_cast<xr_string>(GetText());

	// Удаляем мусор: [ +-]
	xr_string::size_type pos = name.find_first_not_of("/ +-", prevPos);
	if (xr_string::npos != pos)
	{
		name.erase(prevPos, pos - prevPos);
	}

	return name;
}

//////////////////////////////////////////////////////////////////////////

void CUITreeViewBoxItem::MarkArticleAsRead(bool value)
{
	// Если элемент рутовый, то мы его маркаем его, и все чилды
	if (IsRoot())
	{
		m_bArticleRead = value;
		if(!m_bManualSetColor)
			SetItemColor();

		for (SubItems_it it = vSubItems.begin(); it != vSubItems.end(); ++it)
		{
			(*it)->m_bArticleRead = value;
			(*it)->SetItemColor();
			if ((*it)->IsRoot())
				(*it)->MarkArticleAsRead(value);
		}
	}
	else
	{
		// Если же нет, то маркаем себя и говорим проверить свой парентовый элемент
		m_bArticleRead	= value;
		if(!m_bManualSetColor)
			SetItemColor();
		CheckParentMark(GetOwner());
	}
}

//////////////////////////////////////////////////////////////////////////

void CUITreeViewBoxItem::CheckParentMark(CUITreeViewBoxItem *pOwner)
{
	// Берем рута, смотрим на его чилдов, и если среди них есть хоть 1
	// непрочитанный, то маркаем себя как непрочитанный, и  говорим провериться выше.
	bool f = false;
	if (pOwner && pOwner->IsRoot())
	{
		for (SubItems_it it = pOwner->vSubItems.begin(); it != pOwner->vSubItems.end(); ++it)
		{
			if (!(*it)->IsArticleReaded())
			{
				pOwner->m_bArticleRead = false;
				pOwner->SetItemColor();
				f = true;
			}
		}

		if (!f)
		{
			// Если мы тут, то все артиклы прочитанны, и можно маркнуть себя как прочитанная ветвь
			pOwner->m_bArticleRead = true;
			pOwner->SetItemColor();
		}

		pOwner->CheckParentMark(pOwner->GetOwner());
	}
}


bool CUITreeViewBoxItem::OnMouseDown(int mouse_btn)
{
	if (inherited::OnMouseDown(mouse_btn))
	{
		if (IsRoot())
		{
			IsOpened() ? Close() : Open();
		}
		else
		{
			MarkArticleAsRead(true);
		}
		return true;
	}
	return false;
}

static CUITreeViewBoxItem *pPrevFocusedItem = NULL;

void CUITreeViewBoxItem::OnFocusReceive()
{
	inherited::OnFocusReceive();
	UIBkg.TextureOn();
	if (pPrevFocusedItem)
	{
		pPrevFocusedItem->UIBkg.TextureOff();
	}
	pPrevFocusedItem = this;
}

void CUITreeViewBoxItem::OnFocusLost()
{
	CUIWindow::OnFocusLost();
	UIBkg.TextureOff();
	pPrevFocusedItem = NULL;		
}



//////////////////////////////////////////////////////////////////////////
// Standalone function for tree hierarchy creation
//////////////////////////////////////////////////////////////////////////

void CreateTreeBranch(shared_str nesting, shared_str leafName, CUIListBox *pListToAdd, int leafProperty,
					  CGameFont *pRootFont, u32 rootColor, CGameFont *pLeafFont, u32 leafColor, bool markRead)
{
	// Nested function emulation
	class AddTreeTail_
	{
	private:
		CGameFont	*pRootFnt;
		u32			rootItemColor;
	public:
		AddTreeTail_(CGameFont *f, u32 cl)
			:	pRootFnt		(f),
				rootItemColor	(cl)
		{}

		CUITreeViewBoxItem * operator () (GroupStringTree_it it, GroupStringTree &cont, CUITreeViewBoxItem *pItemToIns)
		{
			// Вставляем иерархию разделов в энциклопедию
			CUITreeViewBoxItem *pNewItem = NULL;

			for (GroupStringTree_it it2 = it; it2 != cont.end(); ++it2)
			{
				pNewItem = new CUITreeViewBoxItem();
				pItemToIns->AddSubItem(pNewItem);
				pNewItem->SetFont(pRootFnt);
				pNewItem->SetText(*(*it2));
				pNewItem->SetReadedColor(rootItemColor);
				pNewItem->SetRoot(true);
				pItemToIns = pNewItem;
			}

			return pNewItem;
		}
	} AddTreeTail(pRootFont, rootColor);

	//-----------------------------------------------------------------------------
	//  Function body
	//-----------------------------------------------------------------------------

	// Начинаем алгоритм определения группы вещи в иерархии энциклопедии
	R_ASSERT(*nesting);
	R_ASSERT(pListToAdd);
	R_ASSERT(pLeafFont);
	R_ASSERT(pRootFont);
	xr_string group = *nesting;

	// Парсим строку группы для определения вложенности
	GroupStringTree				groupTree;

	xr_string::size_type		pos;
	xr_string					oneLevel;

	while (true)
	{
		pos = group.find('/');
		if (pos != xr_string::npos)
		{
			oneLevel.assign(group, 0, pos);
			shared_str str(oneLevel.c_str());
			groupTree.push_back(CStringTable().translate(str));
			group.erase(0, pos + 1);
		}
		else
		{
			groupTree.push_back(CStringTable().translate(group.c_str()));
			break;
		}
	}

	// Теперь ищем нет ли затребованных групп уже в наличии
	CUITreeViewBoxItem *pTVItem = NULL, *pTVItemChilds = NULL;
	bool status = false;

	// Для всех рутовых элементов
	for (int i = 0; i < pListToAdd->GetSize(); ++i)
	{
		pTVItem = smart_cast<CUITreeViewBoxItem*>(pListToAdd->GetItem(i));
		R_ASSERT(pTVItem);

		pTVItem->Close();

		xr_string	caption = pTVItem->GetText();
		// Remove "+" sign
		caption.erase(0, 1);

		// Ищем не содержит ли он данной иерархии и добавляем новые элементы если не найдено
		if (0 == xr_strcmp(caption.c_str(), *groupTree.front()))
		{
			// Уже содержит. Надо искать глубже
			pTVItemChilds = pTVItem;
			for (GroupStringTree_it it = groupTree.begin() + 1; it != groupTree.end(); ++it)
			{
				pTVItem = pTVItemChilds->Find(*(*it));
				// Не нашли, надо вставлять хвост списка вложенности
				if (!pTVItem)
				{
					pTVItemChilds = AddTreeTail(it, groupTree, pTVItemChilds);
					status = true;
					break;
				}
				pTVItemChilds = pTVItem;
			}
		}

		if (status) break;
	}

	// Прошли все существующее дерево, и не нашли? Тогда добавляем новую иерархию
	if (!pTVItemChilds)
	{
		pTVItemChilds = new CUITreeViewBoxItem();
		pTVItemChilds->SetListParent(pListToAdd);
		pTVItemChilds->SetFont(pRootFont);
		pTVItemChilds->SetText(*groupTree.front());
		pTVItemChilds->SetReadedColor(rootColor);
		pTVItemChilds->SetRoot(true);
		pListToAdd->AddExistingItem(pTVItemChilds);
	
		// Если в списке вложенности 1 элемент, то хвоста нет, и соответственно ничего не добавляем
		if (groupTree.size() > 1)
			pTVItemChilds = AddTreeTail(groupTree.begin() + 1, groupTree, pTVItemChilds);
	}

	// К этому моменту pTVItemChilds обязательно должна быть не NULL
	R_ASSERT(pTVItemChilds);

	// Cначала проверяем нет ли записи с таким названием, и добавляем если нет
	//	if (!pTVItemChilds->Find(*name))
	//	{
	pTVItem		= new CUITreeViewBoxItem();
	pTVItem->SetFont(pLeafFont);
	pTVItem->SetReadedColor(leafColor);
	pTVItem->SetText(*CStringTable().translate(*leafName));
	pTVItem->SetArticleValue(leafProperty);
	pTVItemChilds->AddSubItem(pTVItem);
	pTVItem->MarkArticleAsRead(markRead);
	pTVItem->SetListParent(pListToAdd);
	//	}
}