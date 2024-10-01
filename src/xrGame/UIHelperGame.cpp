////////////////////////////////////////////////////////////////////////////
//	Module 		: UIHelper.cpp
//	Created 	: 17.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Helper class implementation
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "UIHelperGame.h"
#include "../xrUI/UIXmlInit.h"

#include "UI/UIDragDropReferenceList.h"

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
