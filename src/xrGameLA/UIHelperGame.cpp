////////////////////////////////////////////////////////////////////////////
//	Module 		: UIHelper.cpp
//	Created 	: 17.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Helper class implementation
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "UIHelperGame.h"
#include "../xrUI/UIXmlInit.h"

#include "ui/UIDragDropReferenceList.h"
#include "ui/uilistwnd.h"
#include "ui/UILabel.h"

CUIDragDropListEx* UIHelperGame::CreateDragDropListEx(CUIXml& xml, LPCSTR ui_path, CUIWindow* parent)
{
	CUIDragDropListEx* ui = new CUIDragDropListEx();
	parent->AttachChild(ui);
	ui->SetAutoDelete(true);
	CUIXmlInitGame::InitDragDropListEx(xml, ui_path, 0, ui);
	return ui;
}

CUIDragDropReferenceList* UIHelperGame::CreateDragDropReferenceList(CUIXml& xml, LPCSTR ui_path, CUIWindow* parent)
{
	CUIDragDropReferenceList* ui = new CUIDragDropReferenceList();
	parent->AttachChild(ui);
	ui->SetAutoDelete(true);
	CUIXmlInitGame::InitDragDropListEx(xml, ui_path, 0, ui);
	return ui;
}

bool CUIXmlInitGame::InitDragDropListEx(CUIXml& xml_doc, LPCSTR path, int index, CUIDragDropListEx* pWnd)
{
	bool ValidNode = xml_doc.NavigateToNode(path, index);
	R_ASSERT4(ValidNode, "XML node not found", path, xml_doc.m_xml_file_name);

	Fvector2 pos, size;
	pos.x = xml_doc.ReadAttribFlt(path, index, "x");
	pos.y = xml_doc.ReadAttribFlt(path, index, "y");
	size.x = xml_doc.ReadAttribFlt(path, index, "width");
	size.y = xml_doc.ReadAttribFlt(path, index, "height");

	CUIXmlInit::InitAlignment(xml_doc, path, index, pos.x, pos.y, pWnd);

	pWnd->InitDragDropList(pos, size);

	Ivector2 w_cell_sz, w_cells, w_cell_sp;

	w_cell_sz.x = xml_doc.ReadAttribInt(path, index, "cell_width");
	w_cell_sz.y = xml_doc.ReadAttribInt(path, index, "cell_height");
	w_cells.y = xml_doc.ReadAttribInt(path, index, "rows_num");
	w_cells.x = xml_doc.ReadAttribInt(path, index, "cols_num");

	w_cell_sp.x = xml_doc.ReadAttribInt(path, index, "cell_sp_x");
	w_cell_sp.y = xml_doc.ReadAttribInt(path, index, "cell_sp_y");

	pWnd->SetCellSize(w_cell_sz);
	pWnd->SetCellsSpacing(w_cell_sp);
	pWnd->SetStartCellsCapacity(w_cells);

	int tmp = xml_doc.ReadAttribInt(path, index, "unlimited", 0);
	pWnd->SetAutoGrow(tmp != 0);
	tmp = xml_doc.ReadAttribInt(path, index, "group_similar", 0);
	pWnd->SetGrouping(tmp != 0);
	tmp = xml_doc.ReadAttribInt(path, index, "custom_placement", 1);
	pWnd->SetCustomPlacement(tmp != 0);

	tmp = xml_doc.ReadAttribInt(path, index, "vertical_placement", 0);
	pWnd->SetVerticalPlacement(tmp != 0);

	tmp = xml_doc.ReadAttribInt(path, index, "always_show_scroll", 0);
	pWnd->SetAlwaysShowScroll(tmp != 0);

	tmp = xml_doc.ReadAttribInt(path, index, "condition_progress_bar", 0);
	pWnd->SetConditionProgBarVisibility(tmp != 0);

	tmp = xml_doc.ReadAttribInt(path, index, "virtual_cells", 0);
	pWnd->SetVirtualCells(tmp != 0);

	if (tmp != 0)
	{
		xr_string vc_vert_align = xml_doc.ReadAttrib(path, index, "vc_vert_align", "");
		pWnd->SetCellsVertAlignment(vc_vert_align);
		xr_string vc_horiz_align = xml_doc.ReadAttrib(path, index, "vc_horiz_align", "");
		pWnd->SetCellsHorizAlignment(vc_horiz_align);
	}


	pWnd->back_color = CUIXmlInit::GetColor(xml_doc, path, index, 0xFFFFFFFF);
	pWnd->SetWindowNodeName(path);

	return true;
}

bool CUIXmlInitGame::InitListWnd(CUIXml& xml_doc, LPCSTR path, int index, CUIListWnd* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path, index), "XML node not found", path, xml_doc.m_xml_file_name);

	float x = xml_doc.ReadAttribFlt(path, index, "x");
	float y = xml_doc.ReadAttribFlt(path, index, "y");

	InitAlignment(xml_doc, path, index, x, y, pWnd);

	float width = xml_doc.ReadAttribFlt(path, index, "width");
	float height = xml_doc.ReadAttribFlt(path, index, "height");
	float item_height = xml_doc.ReadAttribFlt(path, index, "item_height");
	int active_background = xml_doc.ReadAttribInt(path, index, "active_bg");

	// Init font from xml config file
	string256							buf;
	CGameFont* LocalFont = NULL;
	u32 cl;

	shared_str text_path = xr_strconcat(buf, path, ":font");
	InitFont(xml_doc, *text_path, index, cl, LocalFont);
	if (LocalFont)
	{
		pWnd->SetFont(LocalFont);
		pWnd->SetTextColor(cl);
	}

	pWnd->SetScrollBarProfile(xml_doc.ReadAttrib(path, index, "scroll_profile", "default"));
	pWnd->InitListWnd(x, y, width, height, item_height);
	pWnd->EnableActiveBackground(!!active_background);

	if (xml_doc.ReadAttribInt(path, index, "always_show_scroll"))
	{
		pWnd->SetAlwaysShowScroll(true);
		pWnd->EnableAlwaysShowScroll(true);
		pWnd->EnableScrollBar(true);
	}

	if (xml_doc.ReadAttribInt(path, index, "always_hide_scroll"))
	{
		pWnd->SetAlwaysShowScroll(false);
		pWnd->EnableAlwaysShowScroll(true);
	}


	bool bVertFlip = (1 == xml_doc.ReadAttribInt(path, index, "flip_vert", 0));
	pWnd->SetVertFlip(bVertFlip);

	return true;
}

bool CUIXmlInitGame::InitLabel(CUIXml& xml_doc, LPCSTR path, int index, CUILabel* pWnd)
{
	InitFrameLine(xml_doc, path, index, pWnd);

	string256 buf;
	xr_strconcat(buf, path, ":text");
	InitText(xml_doc, buf, index, &pWnd->m_text);

	return true;
}
