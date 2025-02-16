#include "stdafx.h"
#include "UIXmlInit.h"
#include "../level.h"
#include "../../xrEngine/string_table.h"
#include "UIFrameWindow.h"
#include "UIStatic.h"
#include "UICheckButton.h"
#include "UICustomSpin.h"
#include "UIRadioButton.h"
#include "UIProgressBar.h"
#include "UIProgressShape.h"
#include "UITabControl.h"
#include "UILabel.h"
#include "UITextBanner.h"
#include "UIMultiTextStatic.h"
#include "UIAnimatedStatic.h"
#include "uixmlinit.h"
#include "UIListBox.h"
#include "UIListWnd.h"
#include "UIComboBox.h"
#include "UITrackBar.h"
#include "UITrackBarVariable.h"
#include "UIHint.h"
#include "../game_base_space.h"
#include "../UIFontDefines.h"

#include "UITextureMaster.h"
#include "UIDragDropListEx.h"
#include "UIDragDropReferenceList.h"
#include "UItabButtonMP.h"
#include "UILines.h"

extern int keyname_to_dik(LPCSTR);

const char * const		COLOR_DEFINITIONS					= "color_defs.xml";
CUIXmlInit::ColorDefs		*CUIXmlInit::m_pColorDefs			= NULL;

shared_str CUIXmlInit::m_Graffiti19RussiaFontName = "ui_font_graffiti19_russian";
shared_str CUIXmlInit::m_Graffiti22RussiaFontName = "ui_font_graffiti22_russian";
shared_str CUIXmlInit::m_Graff32FontName = "ui_font_graff_32";
shared_str CUIXmlInit::m_Graff50FontName = "ui_font_graff_50";
shared_str CUIXmlInit::m_Arial14FontName = "ui_font_arial_14";
shared_str CUIXmlInit::m_Arial21FontName = "ui_font_arial_21";
shared_str CUIXmlInit::m_Letterica16FontName = "ui_font_letterica16_russian";
shared_str CUIXmlInit::m_Letterica18FontName = "ui_font_letterica18_russian";
shared_str CUIXmlInit::m_Letter25FontName = "ui_font_letter_25";

//////////////////////////////////////////////////////////////////////////

CUIXmlInit::CUIXmlInit()
{
	InitColorDefs();
}

//////////////////////////////////////////////////////////////////////////

CUIXmlInit::~CUIXmlInit()
{
}

//////////////////////////////////////////////////////////////////////////

Frect CUIXmlInit::GetFRect(CUIXml& xml_doc, LPCSTR path, int index){
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);
	Frect rect;
	rect.set(0,0,0,0);
	rect.x1 = xml_doc.ReadAttribFlt(path, index, "x");
	rect.y1	= xml_doc.ReadAttribFlt(path, index, "y");
	rect.x2 = rect.x1 + xml_doc.ReadAttribFlt(path, index, "width");
	rect.y2 = rect.y1 + xml_doc.ReadAttribFlt(path, index, "height");

	return rect;
}

bool CUIXmlInit::InitWindow(CUIXml& xml_doc, LPCSTR path, 	
							int index, CUIWindow* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);
	
	Fvector2 pos, size;
	pos.x = xml_doc.ReadAttribFlt(path, index, "x");
	pos.y = xml_doc.ReadAttribFlt(path, index, "y");
	InitAlignment(xml_doc, path, index, pos.x, pos.y, pWnd);
	size.x = xml_doc.ReadAttribFlt(path, index, "width");
	size.y = xml_doc.ReadAttribFlt(path, index, "height");
	pWnd->SetWndPos(pos);
	pWnd->SetWndSize(size);

   	string512 buf;

	xr_strconcat(buf,path,":window_name");
	if(xml_doc.NavigateToNode(buf,index))
		pWnd->SetWindowName		( xml_doc.Read(buf, index, NULL) );

	InitAutoStaticGroup			(xml_doc, path, index, pWnd);
//.	InitAutoFrameLineGroup		(xml_doc, path, index, pWnd);

	return true;
}

//////////////////////////////////////////////////////////////////////////

bool CUIXmlInit::InitFrameWindow(CUIXml& xml_doc, LPCSTR path, 
									int index, CUIFrameWindow* pWnd)
{
	R_ASSERT4		(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	InitTexture		(xml_doc, path, index, pWnd);
	InitWindow		(xml_doc, path, index, pWnd);

   	string512 buf;

	//инициализировать заголовок окна
	xr_strconcat(buf,path,":title");
	if(xml_doc.NavigateToNode(buf,index)) InitStatic(xml_doc, buf, index, pWnd->UITitleText);
	
	pWnd->BringToTop	(pWnd->UITitleText);

	return			true;
}


bool CUIXmlInit::InitOptionsItem(CUIXml& xml_doc, LPCSTR path, int index, CUIOptionsItem* pWnd){
	string256		buf;
	xr_strconcat	(buf,path,":options_item");

	if (xml_doc.NavigateToNode(buf,index))
	{
        shared_str entry		= xml_doc.ReadAttrib(buf, index, "entry");
        shared_str group		= xml_doc.ReadAttrib(buf, index, "group");
		pWnd->AssignProps		(entry, group);
		
		LPCSTR depends			= xml_doc.ReadAttrib(buf, index,"depend", NULL);
		if(depends)
		{
			CUIOptionsItem::ESystemDepends d = CUIOptionsItem::sdNothing;

			if(0==stricmp(depends,"vid"))
				d = CUIOptionsItem::sdVidRestart;
			else
			if(0==stricmp(depends,"snd"))
				d = CUIOptionsItem::sdSndRestart;
			else
			if(0==stricmp(depends,"restart"))
				d = CUIOptionsItem::sdSystemRestart;
			else
			if(0==stricmp(depends,"runtime"))
				d = CUIOptionsItem::sdApplyOnChange;
			else
			if (0 == stricmp(depends, "level_restart"))
				d = CUIOptionsItem::sdLevelRestart;
			else
				Msg("! unknown param [%s] in optionsItem [%s]", depends, entry.c_str());

			pWnd->SetSystemDepends		(d);
		}
		return true;
	}
	else return false;	
}


bool CUIXmlInit::InitStatic(CUIXml& xml_doc, LPCSTR path, 
									int index, CUIStatic* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	InitWindow			(xml_doc, path, index, pWnd);

	string256			buf;
	InitText			(xml_doc, xr_strconcat(buf,path,":text"), index, pWnd);
	InitTexture			(xml_doc, path, index, pWnd);
	InitTextureOffset	(xml_doc,path,index,pWnd);

	int flag = xml_doc.ReadAttribInt(path, index, "heading", 0);
	pWnd->EnableHeading( (flag)?true:false);

	float heading_angle = xml_doc.ReadAttribFlt(path, index, "heading_angle", 0.0f);
	if ( !fis_zero( heading_angle ) )
	{
		pWnd->EnableHeading( true );
		pWnd->SetConstHeading( true );
		pWnd->SetHeading( deg2rad( heading_angle ) );
	}

	LPCSTR str_flag				= xml_doc.ReadAttrib	(path, index, "light_anim",	"");
	int flag_cyclic				= xml_doc.ReadAttribInt	(path, index, "la_cyclic",	1);
	int flag_text				= xml_doc.ReadAttribInt	(path, index, "la_text",	1);
	int flag_texture			= xml_doc.ReadAttribInt	(path, index, "la_texture",	1);
	int flag_alpha				= xml_doc.ReadAttribInt	(path, index, "la_alpha",	0);
		
	u8 flags					= 0;
	if(flag_cyclic)				flags |= LA_CYCLIC;
	if(flag_alpha)				flags |= LA_ONLYALPHA;
	if(flag_text)				flags |= LA_TEXTCOLOR;
	if(flag_texture)			flags |= LA_TEXTURECOLOR;

	pWnd->SetColorAnimation		(str_flag, flags);


	str_flag					= xml_doc.ReadAttrib(path, index, "xform_anim",				"");
	flag_cyclic					= xml_doc.ReadAttribInt(path, index, "xform_anim_cyclic",	1);
		
	pWnd->SetXformLightAnim		(str_flag, (flag_cyclic)?true:false);

	int flag_highlight_txt		= xml_doc.ReadAttribInt(path, index, "highlight_text", 0);
	if (flag_highlight_txt)
	{
		pWnd->HighlightText(true);

		u32 hA = static_cast<u32>(xml_doc.ReadAttribInt(path, index, "hA", 255));
		u32 hR = static_cast<u32>(xml_doc.ReadAttribInt(path, index, "hR", 255));
		u32 hG = static_cast<u32>(xml_doc.ReadAttribInt(path, index, "hG", 255));
		u32 hB = static_cast<u32>(xml_doc.ReadAttribInt(path, index, "hB", 255));
		pWnd->SetHighlightColor		(color_argb(hA, hR, hG, hB));
	}

	bool bComplexMode = xml_doc.ReadAttribInt(path, index, "complex_mode",0)?true:false;
	if(bComplexMode)
		pWnd->TextItemControl()->SetTextComplexMode(bComplexMode);
	
	LPCSTR text_hint			= xml_doc.ReadAttrib(path, index, "hint", NULL);
	if (text_hint)
		pWnd->m_stat_hint_text	= CStringTable().translate(text_hint);

	return true;
}

bool CUIXmlInit::InitTextWnd(CUIXml& xml_doc, LPCSTR path, int index, CUITextWnd* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	InitWindow			(xml_doc, path, index, pWnd);

	string256			buf;
	InitText			(xml_doc, xr_strconcat(buf,path,":text"), index, &pWnd->TextItemControl());

	LPCSTR str_flag			= xml_doc.ReadAttrib(path, index, "light_anim",		"");
	int flag_cyclic			= xml_doc.ReadAttribInt(path, index, "la_cyclic",	1);
	int flag_alpha			= xml_doc.ReadAttribInt(path, index, "la_alpha",	0);
	
	u8 flags				= LA_TEXTCOLOR ;
	if(flag_cyclic)			flags |= LA_CYCLIC;
	if(flag_alpha)			flags |= LA_ONLYALPHA;
	pWnd->SetColorAnimation	(str_flag, flags);	


	bool bComplexMode = xml_doc.ReadAttribInt(path, index, "complex_mode",0)?true:false;
	if(bComplexMode)
		pWnd->SetTextComplexMode(bComplexMode);

	R_ASSERT(pWnd->GetChildWndList().size()==0);
	return true;
}

bool CUIXmlInit::InitCheck(CUIXml& xml_doc, LPCSTR path, int index, CUICheckButton* pWnd)
{
	InitStatic(xml_doc, path, index, pWnd);

	string256 buf;	
	xr_strconcat(buf, path, ":texture");
	LPCSTR texture = xml_doc.Read(buf, index, "ui_checker");

	pWnd->InitCheckButton(pWnd->GetWndPos(),pWnd->GetWndSize(),texture);

	u32 color;
	bool color_flag = false;

	xr_strconcat(buf,path,":text_color:e");
	if (xml_doc.NavigateToNode(buf,index))
	{
		color			= GetColor(xml_doc, buf, index, 0x00);
		pWnd->SetStateTextColor(color, S_Enabled);
		color_flag = true;
	}

	xr_strconcat(buf,path,":text_color:d");
	if (xml_doc.NavigateToNode(buf,index))
	{
		color			= GetColor(xml_doc, buf, index, 0x00);
		pWnd->SetStateTextColor(color,S_Disabled);
		color_flag = true;
	}

	xr_strconcat(buf,path,":text_color:t");
	if (xml_doc.NavigateToNode(buf, index))
	{
		color			= GetColor(xml_doc, buf, index, 0x00);
		pWnd->SetStateTextColor(color,S_Touched);
		color_flag = true;
	}

	xr_strconcat(buf,path,":text_color:h");
	if (xml_doc.NavigateToNode(buf,index))
	{
		color			= GetColor(xml_doc, buf, index, 0x00);
		pWnd->SetStateTextColor(color,S_Highlighted);
		color_flag = true;
	}

	if (!color_flag)
	{
		xr_strconcat(buf,path,":text");
		if (xml_doc.NavigateToNode(buf,index))
		{
			color			= GetColor(xml_doc, buf, index, 0x00);
			pWnd->SetStateTextColor(color,S_Enabled);
		}
	}

	InitOptionsItem(xml_doc, path, index, pWnd);

	return true;
}

bool CUIXmlInit::InitSpin(CUIXml& xml_doc, LPCSTR path, int index, CUICustomSpin* pWnd)
{
	InitWindow(xml_doc, path, index, pWnd);
	InitOptionsItem(xml_doc, path, index, pWnd);
	pWnd->InitSpin(pWnd->GetWndPos(), pWnd->GetWndSize());

	string256				foo;
	u32						color;
	xr_strconcat			(foo,path,":text_color:e");
	if (xml_doc.NavigateToNode(foo,index)){
		color				= GetColor(xml_doc,foo,index,0x00);
		pWnd->SetTextColor	(color);	
	}
	xr_strconcat			(foo,path,":text_color:d");
	if (xml_doc.NavigateToNode(foo,index)){
		color				= GetColor(xml_doc,foo,index,0x00);
		pWnd->SetTextColorD	(color);
	}

	return true;
}

bool CUIXmlInit::InitText(CUIXml& xml_doc, LPCSTR path, int index, CUIStatic* pWnd)
{
	if(!xml_doc.NavigateToNode(path,index))
		return false;

	return	InitText		(xml_doc,path,index, pWnd->TextItemControl());
}


bool CUIXmlInit::InitText(CUIXml& xml_doc, LPCSTR path, int index, CUILines* pLines)
{
	if(!xml_doc.NavigateToNode(path,index))
		return false;

	u32	color;
	CGameFont *pTmpFont		= NULL;
	InitFont				(xml_doc, path, index, color, pTmpFont);
	pLines->SetTextColor	(color);
	if (pTmpFont)
		pLines->SetFont(pTmpFont);	

	// Load font alignment
	shared_str al = xml_doc.ReadAttrib(path, index, "align");
	if (0 == xr_strcmp(al, "c"))
		pLines->SetTextAlignment(CGameFont::alCenter);
	else if (0 == xr_strcmp(al, "r"))
		pLines->SetTextAlignment(CGameFont::alRight);
	else if (0 == xr_strcmp(al, "l"))
		pLines->SetTextAlignment(CGameFont::alLeft);

	al	= xml_doc.ReadAttrib(path, index, "vert_align", "");

	if (0 == xr_strcmp(al, "c"))
		pLines->SetVTextAlignment(valCenter);
	else if (0 == xr_strcmp(al, "b"))
		pLines->SetVTextAlignment(valBotton);
	else if (0 == xr_strcmp(al, "t"))
		pLines->SetVTextAlignment(valTop);

	pLines->SetTextComplexMode(xml_doc.ReadAttribInt(path, index, "complex_mode",0)?true:false);

	// Text coordinates
	float text_x		= xml_doc.ReadAttribFlt(path, index, "x", 0);
	float text_y		= xml_doc.ReadAttribFlt(path, index, "y", 0);

	pLines->m_TextOffset.set(text_x, text_y);

	shared_str text = xml_doc.Read(path, index, NULL);
	if (text.size())
		pLines->SetText(CStringTable().translate(text).c_str());

	return true;
}
////////////////////////////////////////////////////////////////////////////////////////////
extern int keyname_to_dik(LPCSTR);

bool CUIXmlInit::Init3tButton(CUIXml& xml_doc, LPCSTR path, int index, CUI3tButton* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	pWnd->m_frameline_mode = (xml_doc.ReadAttribInt(path, index, "frame_mode", 0) == 1)? true : false;

	pWnd->vertical = (xml_doc.ReadAttribInt(path, index, "vertical", 0) == 1)? true : false;

	InitWindow			(xml_doc, path, index, pWnd);				
	pWnd->InitButton	(pWnd->GetWndPos(), pWnd->GetWndSize());

	string256			buf;
	InitText			(xml_doc, xr_strconcat(buf,path,":text"), index, pWnd);

	u32	color;
	bool 	color_flag = false;

	xr_strconcat(buf,path,":text_color:e");
	if (xml_doc.NavigateToNode(buf,index))
	{
		color			= GetColor(xml_doc, buf, index, 0x00);
		pWnd->SetStateTextColor(color, S_Enabled);
		color_flag = true;
	}

	xr_strconcat(buf,path,":text_color:d");
	if (xml_doc.NavigateToNode(buf,index))
	{
		color			= GetColor(xml_doc, buf, index, 0x00);
		pWnd->SetStateTextColor(color,S_Disabled);
		color_flag = true;
	}

	xr_strconcat(buf,path,":text_color:t");
	if (xml_doc.NavigateToNode(buf, index))
	{
		color			= GetColor(xml_doc, buf, index, 0x00);
		pWnd->SetStateTextColor(color,S_Touched);
		color_flag = true;
	}

	xr_strconcat(buf,path,":text_color:h");
	if (xml_doc.NavigateToNode(buf,index))
	{
		color			= GetColor(xml_doc, buf, index, 0x00);
		pWnd->SetStateTextColor(color,S_Highlighted);
		color_flag = true;
	}

	if (!color_flag)
	{
		xr_strconcat(buf,path,":text");
		if (xml_doc.NavigateToNode(buf,index))
		{
			color			= GetColor(xml_doc, buf, index, 0x00);
			pWnd->SetStateTextColor(color,S_Enabled);
		}
	}

	InitMultiTexture	(xml_doc, path, index, pWnd);
	InitTextureOffset	(xml_doc, path, index, pWnd);
	InitSound			(xml_doc, path, index, pWnd);

	LPCSTR accel		= xml_doc.ReadAttrib(path, index, "accel", NULL);
	if(accel)
	{
		int acc				= keyname_to_dik(accel);
		pWnd->SetAccelerator(acc, 0);
	}
	accel				= xml_doc.ReadAttrib(path, index, "accel_ext", NULL);
	if(accel)
	{
		int acc				= keyname_to_dik(accel);
		pWnd->SetAccelerator(acc, 1);
	}

	bool stretch_flag = xml_doc.ReadAttribInt(path, index, "stretch") ? true : false;
	pWnd->SetStretch(stretch_flag);

	LPCSTR text_hint		= xml_doc.ReadAttrib	(path, index, "hint", NULL);
	if(text_hint)
		pWnd->m_hint_text	= CStringTable().translate(text_hint);

	return true;
}

bool CUIXmlInit::InitButton(CUIXml& xml_doc, LPCSTR path, 
						int index, CUIButton* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	InitStatic(xml_doc, path, index, pWnd);

	LPCSTR accel		= xml_doc.ReadAttrib(path, index, "accel", NULL);
	if(accel)
	{
		int acc				= keyname_to_dik(accel);
		pWnd->SetAccelerator(acc, 0);
	}
	accel				= xml_doc.ReadAttrib(path, index, "accel_ext", NULL);
	if(accel)
	{
		int acc				= keyname_to_dik(accel);
		pWnd->SetAccelerator(acc, 1);
	}

	LPCSTR text_hint		= xml_doc.ReadAttrib	(path, index, "hint", NULL);
	if(text_hint)
		pWnd->m_hint_text	= CStringTable().translate(text_hint);

	float shadowOffsetX	= xml_doc.ReadAttribFlt(path, index, "shadow_offset_x", 0);
	float shadowOffsetY	= xml_doc.ReadAttribFlt(path, index, "shadow_offset_y", 0);

	pWnd->SetShadowOffset(Fvector2().set(shadowOffsetX, shadowOffsetY));

	float pushOffsetX		= xml_doc.ReadAttribFlt	(path, index, "push_off_x", 2);
	float pushOffsetY		= xml_doc.ReadAttribFlt	(path, index, "push_off_y", 3);

	pWnd->SetPushOffset		(Fvector2().set(pushOffsetX,pushOffsetY) );

	return true;
}


bool CUIXmlInit::InitTabButtonMP(CUIXml& xml_doc, LPCSTR path,	int index, CUITabButtonMP *pWnd)
{
	Init3tButton(xml_doc, path,	index, pWnd);

	string256							buff;
	xr_strconcat						(buff, path, ":idention");
	
	if (xml_doc.NavigateToNode			(buff, index))
	{
		pWnd->m_text_ident_cursor_over.x = xml_doc.ReadAttribFlt(buff, index, "over_x", 0);
		pWnd->m_text_ident_cursor_over.y = xml_doc.ReadAttribFlt(buff, index, "over_y", 0);

		pWnd->m_text_ident_normal.x		= xml_doc.ReadAttribFlt(buff, index, "normal_x", 0);
		pWnd->m_text_ident_normal.y		= xml_doc.ReadAttribFlt(buff, index, "normal_y", 0);
	}

	xr_strconcat(buff, path, ":hint");

	if (xml_doc.NavigateToNode(buff, index))
	{
		pWnd->CreateHint();
        InitStatic(xml_doc, buff, index, pWnd->m_hint);
	}

	return true;
}


bool CUIXmlInit::InitSound(CUIXml& xml_doc, LPCSTR path, int index, CUI3tButton* pWnd){
	string256 sound_h;
	string256 sound_t;
	xr_strconcat(sound_h, path, ":sound_h");
	xr_strconcat(sound_t, path, ":sound_t");

	shared_str sound_h_result = xml_doc.Read(sound_h, index, "");
	shared_str sound_t_result = xml_doc.Read(sound_t, index, "");

	if (xr_strlen(sound_h_result) != 0)
		pWnd->InitSoundH(*sound_h_result);

	if (xr_strlen(sound_t_result) != 0)
		pWnd->InitSoundT(*sound_t_result);

	return true;
}

bool CUIXmlInit::InitDragDropListEx(CUIXml& xml_doc, LPCSTR path, int index, CUIDragDropListEx* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	Fvector2 pos, size;
	pos.x			= xml_doc.ReadAttribFlt(path, index, "x");
	pos.y			= xml_doc.ReadAttribFlt(path, index, "y");
	size.x			= xml_doc.ReadAttribFlt(path, index, "width");
	size.y			= xml_doc.ReadAttribFlt(path, index, "height");

	InitAlignment	(xml_doc, path, index, pos.x, pos.y, pWnd);



	pWnd->InitDragDropList		(pos, size);

	Ivector2 w_cell_sz, w_cells, w_cell_sp;

	w_cell_sz.x				= xml_doc.ReadAttribInt(path, index, "cell_width");
	w_cell_sz.y				= xml_doc.ReadAttribInt(path, index, "cell_height");
	w_cells.y				= xml_doc.ReadAttribInt(path, index, "rows_num");
	w_cells.x				= xml_doc.ReadAttribInt(path, index, "cols_num");

	w_cell_sp.x				= xml_doc.ReadAttribInt(path, index, "cell_sp_x");
	w_cell_sp.y				= xml_doc.ReadAttribInt(path, index, "cell_sp_y");

	pWnd->SetCellSize		(w_cell_sz);
	pWnd->SetCellsSpacing	(w_cell_sp);	
	pWnd->SetStartCellsCapacity	(w_cells);	

	int tmp					= xml_doc.ReadAttribInt(path, index, "unlimited", 0);
	pWnd->SetAutoGrow		(tmp!=0);
	tmp						= xml_doc.ReadAttribInt(path, index, "group_similar", 0);
	pWnd->SetGrouping		(tmp!=0);
	tmp						= xml_doc.ReadAttribInt(path, index, "custom_placement", 1);
	pWnd->SetCustomPlacement(tmp!=0);

	tmp						= xml_doc.ReadAttribInt(path, index, "vertical_placement", 0);
	pWnd->SetVerticalPlacement(tmp!=0);

	tmp						= xml_doc.ReadAttribInt(path, index, "always_show_scroll", 0);
	pWnd->SetAlwaysShowScroll(tmp!=0);

	tmp						= xml_doc.ReadAttribInt(path, index, "condition_progress_bar", 0);
	pWnd->SetConditionProgBarVisibility(tmp!=0);

	tmp						= xml_doc.ReadAttribInt(path, index, "virtual_cells", 0);
	pWnd->SetVirtualCells(tmp!=0);

	if(tmp!=0)
	{
		xr_string vc_vert_align = xml_doc.ReadAttrib(path, index, "vc_vert_align", "");
		pWnd->SetCellsVertAlignment(vc_vert_align);
		xr_string vc_horiz_align = xml_doc.ReadAttrib(path, index, "vc_horiz_align", "");
		pWnd->SetCellsHorizAlignment(vc_horiz_align);
	}


	pWnd->back_color		= GetColor( xml_doc, path, index, 0xFFFFFFFF );

	return true;
}

bool CUIXmlInit::InitProgressBar(CUIXml& xml_doc, LPCSTR path, 
						int index, CUIProgressBar* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	InitAutoStaticGroup			(xml_doc, path, index, pWnd);

	string256 buf;
	Fvector2 pos, size;
	pos.x 				= xml_doc.ReadAttribFlt(path, index, "x");
	pos.y 				= xml_doc.ReadAttribFlt(path, index, "y");

	InitAlignment		(xml_doc, path, index, pos.x, pos.y, pWnd);

	size.x				= xml_doc.ReadAttribFlt(path, index, "width");
	size.y				= xml_doc.ReadAttribFlt(path, index, "height");
	
	CUIProgressBar::EOrientMode mode = CUIProgressBar::om_vert;
	int mode_horz     = xml_doc.ReadAttribInt(path, index, "horz", 0);
	LPCSTR mode_str = xml_doc.ReadAttrib(path, index, "mode");
	if ( mode_horz == 1 ) // om_horz
	{
		mode = CUIProgressBar::om_horz;
	}
	else if ( stricmp( mode_str, "horz" ) == 0 )	{	mode = CUIProgressBar::om_horz;	}
	else if ( stricmp( mode_str, "vert" ) == 0 )	{	mode = CUIProgressBar::om_vert;	}
	else if ( stricmp( mode_str, "back" ) == 0 )	{	mode = CUIProgressBar::om_back;	}
	else if ( stricmp( mode_str, "down" ) == 0 )	{	mode = CUIProgressBar::om_down;	}
	
	pWnd->InitProgressBar(pos, size, mode);

	float min = xml_doc.ReadAttribFlt(path, index, "min");
	float max = xml_doc.ReadAttribFlt(path, index, "max");
	float ppos = xml_doc.ReadAttribFlt(path, index, "pos");
	
	pWnd->SetRange			(min, max);
	pWnd->SetProgressPos	(ppos);
	pWnd->m_inertion		= xml_doc.ReadAttribFlt(path, index, "inertion", 0.0f);
	// progress
	xr_strconcat(buf,path,":progress");

	if (!xml_doc.NavigateToNode(buf, index))
		return false;

	InitStatic								(xml_doc, buf, index, &pWnd->m_UIProgressItem);
	
	pWnd->m_UIProgressItem.SetWndSize		(pWnd->GetWndSize());
	
	// background
	xr_strconcat							(buf,path,":background");
	
	if (xml_doc.NavigateToNode(buf, index))
	{
        InitStatic							(xml_doc, buf, index, &pWnd->m_UIBackgroundItem);
		pWnd->m_bBackgroundPresent			= true;
		pWnd->m_UIBackgroundItem.SetWndSize(pWnd->GetWndSize());
	}

	xr_strconcat(buf,path,":min_color");
	
	if( xml_doc.NavigateToNode(buf,index) ){
		pWnd->m_bUseColor			= true;
	
		u32 color = GetColor	(xml_doc, buf, index, 0xff);
		pWnd->m_minColor.set(color);

		xr_strconcat(buf,path,":max_color");
	
		color = GetColor	(xml_doc, buf, index, 0xff);
		pWnd->m_maxColor.set(color);
	}

	return true;
}

bool CUIXmlInit::InitProgressShape(CUIXml& xml_doc, LPCSTR path, int index, CUIProgressShape* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	InitWindow						(xml_doc, path, index, pWnd);

	if (xml_doc.ReadAttribInt(path, index, "text"))
		pWnd->SetTextVisible(true);

	string256 _path;

	if (xml_doc.NavigateToNode(xr_strconcat(_path, path, ":back"),index))
		InitStatic(xml_doc, _path, index, pWnd->m_pBackground);

	InitStatic(xml_doc, xr_strconcat(_path, path, ":front"), index, pWnd->m_pTexture);

	pWnd->m_sectorCount	= xml_doc.ReadAttribInt(path, index, "sector_count", 8);
	pWnd->m_bClockwise	= xml_doc.ReadAttribInt(path, index, "clockwise") ? true : false;

	return true;
}

void CUIXmlInit::InitAutoStaticGroup(CUIXml& xml_doc, LPCSTR path, int index, CUIWindow* pParentWnd)
{
	XML_NODE* _stored_root				= xml_doc.GetLocalRoot();
	xml_doc.SetLocalRoot				(xml_doc.NavigateToNode(path,index));

	XML_NODE* curr_root					= xml_doc.GetLocalRoot();
	if(!curr_root)
			curr_root					= xml_doc.GetRoot();
	
	XML_NODE* node						= curr_root->FirstChild();
	int cnt_static						= 0;
	int cnt_frameline					= 0;
	int cnt_text						= 0;
	string512							buff;

	while(node)
	{
		LPCSTR node_name			= node->Value();
		if(0==stricmp(node_name,"auto_static"))
		{
			CUIStatic* pUIStatic		= new CUIStatic();
			InitStatic					(xml_doc, "auto_static", cnt_static, pUIStatic);
			xr_sprintf					(buff,"auto_static_%d", cnt_static);
			pUIStatic->SetWindowName	(buff);
			pUIStatic->SetAutoDelete	(true);
			pParentWnd->AttachChild		(pUIStatic);

			++cnt_static;
		}else
		if(0==stricmp(node_name,"auto_frameline"))
		{
			CUIFrameLineWnd* pUIFrameline = new CUIFrameLineWnd();
			InitFrameLine				(xml_doc, "auto_frameline", cnt_frameline, pUIFrameline);
			xr_sprintf					(buff,"auto_frameline_%d", cnt_frameline);
			pUIFrameline->SetWindowName	(buff);
			pUIFrameline->SetAutoDelete	(true);
			pParentWnd->AttachChild	(pUIFrameline);

			++cnt_frameline;
		}else
		if(0==stricmp(node_name,"auto_text"))
		{
			++cnt_text;
		}
		node						= curr_root->NextSibling();
	}
/*
	CUIStatic* pUIStatic				= NULL;
	string64							sname;
	for(int i=0; i<items_num; i++)
	{
		pUIStatic						= new CUIStatic();
		InitStatic						(xml_doc, "auto_static", i, pUIStatic);
		xr_sprintf						(sname,"auto_static_%d", i);
		pUIStatic->SetWindowName		(sname);
		pUIStatic->SetAutoDelete		(true);
		pParentWnd->AttachChild			(pUIStatic);
		pUIStatic						= NULL;
	}
*/
	xml_doc.SetLocalRoot				(_stored_root);
}

void CUIXmlInit::InitAutoFrameLineGroup(CUIXml& xml_doc, LPCSTR path, int index, CUIWindow* pParentWnd)
{
	int items_num						= xml_doc.GetNodesNum(path, index, "auto_frameline");
	if ( items_num == 0 )
	{
		return;
	}
	XML_NODE* _stored_root				= xml_doc.GetLocalRoot();
	xml_doc.SetLocalRoot				(xml_doc.NavigateToNode(path,index));

	CUIFrameLineWnd* pUIFL				= NULL;
	string64							sname;
	for(int i=0; i<items_num; ++i)
	{
		pUIFL							= new CUIFrameLineWnd();
		InitFrameLine					(xml_doc, "auto_frameline", i, pUIFL);
		xr_sprintf						(sname,"auto_frameline_%d", i);
		pUIFL->SetWindowName			(sname);
		pUIFL->SetAutoDelete			(true);
		pParentWnd->AttachChild			(pUIFL);
		pUIFL							= NULL;
	}

	xml_doc.SetLocalRoot				(_stored_root);
}

bool CUIXmlInit::InitFont(CUIXml &xml_doc, LPCSTR path, int index, u32 &color, CGameFont *&pFnt)
{
	color = GetColor	(xml_doc, path, index, 0xff);

	LPCSTR font_name = xml_doc.ReadAttrib(path, index, "font", nullptr);
	if(!font_name)
	{
		pFnt = nullptr;
		return false;
	}else
	{
		CFontManager& fontManager = UI().Font();
		if (!xr_strcmp(font_name, GRAFFITI19_FONT_NAME))
		{
			pFnt = fontManager.GetFont(m_Graffiti19RussiaFontName);
		}
		else if (!xr_strcmp(font_name, GRAFFITI22_FONT_NAME))
		{
			pFnt = fontManager.GetFont(m_Graffiti22RussiaFontName);
		}
		else if (!xr_strcmp(font_name, GRAFFITI32_FONT_NAME))
		{
			pFnt = fontManager.GetFont(m_Graff32FontName);
		}
		else if (!xr_strcmp(font_name, GRAFFITI50_FONT_NAME))
		{
			pFnt = fontManager.GetFont(m_Graff50FontName);
		}
		else if (!xr_strcmp(font_name, ARIAL14_FONT_NAME))
		{
			pFnt = fontManager.GetFont(m_Arial14FontName);
		}
		else if (!xr_strcmp(font_name, ARIAL21_FONT_NAME))
		{
			pFnt = fontManager.GetFont(m_Arial21FontName);
		}
		else if (!xr_strcmp(font_name, MEDIUM_FONT_NAME))
		{
			pFnt = fontManager.pFontMedium;
		}
		else if (!xr_strcmp(font_name, SMALL_FONT_NAME))
		{
			pFnt = fontManager.pFontStat;
		}
		else if (!xr_strcmp(font_name, LETTERICA16_FONT_NAME))
		{
			pFnt = fontManager.GetFont(m_Letterica16FontName);
		}
		else if (!xr_strcmp(font_name, LETTERICA18_FONT_NAME))
		{
			pFnt = fontManager.GetFont(m_Letterica18FontName);
		}
		else if (!xr_strcmp(font_name, LETTERICA25_FONT_NAME))
		{
			pFnt = fontManager.GetFont(m_Letter25FontName);
		}
		else if (!xr_strcmp(font_name, DI_FONT_NAME))
		{
			pFnt = fontManager.pFontDI;
		}
		else
		{
			shared_str FontNameStr = font_name;
			pFnt = fontManager.GetFont(FontNameStr);
		}
	}
	return true;
}

bool CUIXmlInit::InitTabControl(CUIXml &xml_doc, LPCSTR path, int index, CUITabControl *pWnd)
{
	R_ASSERT4				(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);
	
	bool status				= true;

	status &= InitWindow	(xml_doc, path, index, pWnd);
	InitOptionsItem			(xml_doc, path, index, pWnd);
	int tabsCount			= xml_doc.GetNodesNum(path, index, "button");
	int radio				= xml_doc.ReadAttribInt(path, index, "radio");

	XML_NODE* tab_node		= xml_doc.NavigateToNode(path,index);
	xml_doc.SetLocalRoot	(tab_node);

	CUITabButton* newButton;

	for (int i = 0; i < tabsCount; ++i)
	{
		newButton = radio ? new CUIRadioButton() : new CUITabButton();
		status &= Init3tButton(xml_doc, "button", i, newButton);
		pWnd->AddItem(newButton);
	}
	
	xml_doc.SetLocalRoot(xml_doc.GetRoot());

	return status;
}

//////////////////////////////////////////////////////////////////////////

bool CUIXmlInit::InitFrameLine(CUIXml& xml_doc, LPCSTR path, int index, CUIFrameLineWnd* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	string256 buf;

	Fvector2 pos, size;
	pos.x			= xml_doc.ReadAttribFlt(path, index, "x");
	pos.y			= xml_doc.ReadAttribFlt(path, index, "y");

	InitAlignment	(xml_doc, path, index, pos.x, pos.y, pWnd);

	size.x			= xml_doc.ReadAttribFlt(path, index, "width");
	size.y			= xml_doc.ReadAttribFlt(path, index, "height");
	bool vertical	= !!xml_doc.ReadAttribInt(path, index, "vertical");
	
	xr_strconcat	(buf,path,":texture");
	shared_str base_name = xml_doc.Read(buf, index, NULL);

	VERIFY2			(base_name, path);

	u32 color		= GetColor	(xml_doc,buf,index,0xff);
	pWnd->SetTextureColor	(color);

	InitWindow		(xml_doc, path, index, pWnd);
	pWnd->InitFrameLineWnd(*base_name, pos, size, !vertical);

	bool stretch_BE_flag	= !!xml_doc.ReadAttribInt(path, index, "stretch_b_e_textures", 0);
	pWnd->SetStretchBETextures(stretch_BE_flag);

	xr_strconcat(buf,path,":title");
	if(xml_doc.NavigateToNode(buf,index)) InitStatic(xml_doc, buf, index, &pWnd->UITitleText);

	return true;
}

bool CUIXmlInit::InitLabel(CUIXml& xml_doc, LPCSTR path, int index, CUILabel* pWnd)
{
	InitFrameLine(xml_doc, path, index, pWnd);

	string256 buf;
	xr_strconcat(buf,path,":text");
	InitText(xml_doc, buf, index, &pWnd->m_text);

	return true;
}


bool CUIXmlInit::InitCustomEdit(CUIXml& xml_doc, LPCSTR path, int index, CUICustomEdit* pWnd)
{
	InitStatic				(xml_doc, path, index, pWnd);
	pWnd->InitCustomEdit	(pWnd->GetWndPos(),pWnd->GetWndSize());

	string256				foo;	
	u32						color;
	xr_strconcat			(foo,path,":text_color:e");
	if (xml_doc.NavigateToNode(foo,index))
	{
		color				= GetColor	(xml_doc, foo, index, 0x00);
		pWnd->SetTextColor	(color);
	}
	xr_strconcat			(foo,path,":text_color:d");
	if (xml_doc.NavigateToNode(foo,index))
	{
		color				= GetColor	(xml_doc, foo, index, 0x00);
		pWnd->SetTextColorD	(color);
	}

	if (xml_doc.ReadAttribInt(path,index,"db_click",0))
		pWnd->SetDbClickMode();

	int max_count		= xml_doc.ReadAttribInt(path, index, "max_symb_count", 0);
	bool num_only       = (xml_doc.ReadAttribInt(path, index, "numonly", 0) == 1);
	bool read_only      = (xml_doc.ReadAttribInt(path, index, "read_only", 0) == 1);
	bool file_name_mode = (xml_doc.ReadAttribInt(path, index, "file_name_mode", 0) == 1);
	bool translate = (xml_doc.ReadAttribInt(path, index, "translate", 0) == 1);

	if ( file_name_mode || read_only || num_only || 0 < max_count )
	{
		if ( max_count <= 0 )
		{
			max_count = 32;
		}
		pWnd->Init( max_count, num_only, read_only, file_name_mode, translate );
	}

	if (xml_doc.ReadAttribInt(path, index, "password",0))
	{
		pWnd->SetPasswordMode();
	}
	return true;
}

bool CUIXmlInit::InitEditBox(CUIXml& xml_doc, LPCSTR path, int index, CUIEditBox* pWnd)
{
	InitCustomEdit		(xml_doc, path, index, pWnd);

	InitTexture			(xml_doc, path, index, pWnd);
	InitOptionsItem		(xml_doc, path, index, pWnd);

	return true;
}

//////////////////////////////////////////////////////////////////////////

bool CUIXmlInit::InitTextBanner(CUIXml& xml_doc, LPCSTR path, int index, CUITextBanner *pBnr)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	xr_map<shared_str, CUITextBanner::TextBannerStyles> conformityTable;
	conformityTable["none"]		= CUITextBanner::tbsNone;
	conformityTable["fade"]		= CUITextBanner::tbsFade;
	conformityTable["flicker"]	= CUITextBanner::tbsFlicker;

	int animationsCount = xml_doc.GetNodesNum(path, index, "animation");

	XML_NODE *tab_node = xml_doc.NavigateToNode(path, index);
	XML_NODE *old_node = xml_doc.GetLocalRoot();
	xml_doc.SetLocalRoot(tab_node);

	shared_str a;

	for (int i = 0; i < animationsCount; ++i)
	{
		a						= xml_doc.ReadAttrib("animation", i, "anim", "none");
		EffectParams * param	= pBnr->SetStyleParams(conformityTable[a]);

		param->bCyclic			= !!xml_doc.ReadAttribInt("animation", i, "cyclic", 1);
		param->bOn				= !!xml_doc.ReadAttribInt("animation", i, "on", 1);
		param->fPeriod			= static_cast<float>(atof(xml_doc.ReadAttrib("animation", i, "period", "1")));
		param->iEffectStage		= xml_doc.ReadAttribInt("animation", i, "stage", 0);
	}

	xml_doc.SetLocalRoot(old_node);

	return true;
}

//////////////////////////////////////////////////////////////////////////

bool CUIXmlInit::InitMultiTextStatic(CUIXml& xml_doc, LPCSTR path, int index, CUIMultiTextStatic *pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	bool status = true;
	string128	buf;

	status &= InitStatic(xml_doc, path, index, pWnd);
	int phrasesCount = xml_doc.GetNodesNum(path, index, "phrase");

	xr_strconcat(buf, path, ":phrase");
	XML_NODE* tab_node = xml_doc.NavigateToNode(path,index);
	xml_doc.SetLocalRoot(tab_node);

	CUIMultiTextStatic::SinglePhrase * p;
	u32	argb = 0;
	const char * const ph = "phrase";

	for (int i = 0; i < phrasesCount; ++i)
	{
		p = pWnd->AddPhrase();

		status			&= InitTextBanner(xml_doc, ph, i, &p->effect);
		p->outX			= (xml_doc.ReadAttribFlt(ph, i, "x", 0));
		p->outY			= (xml_doc.ReadAttribFlt(ph, i, "y", 0));
		p->maxWidth		= xml_doc.ReadAttribFlt(ph, i, "width", -1);

		CGameFont *pFont;
		InitFont(xml_doc, ph, i, argb, pFont);
        p->effect.SetFont(pFont);
		p->effect.SetTextColor(argb);

		CStringTable st;

		p->str =  st.translate(xml_doc.Read(ph, i, ""));
	}

	xml_doc.SetLocalRoot(xml_doc.GetRoot());

	return status;
}

//////////////////////////////////////////////////////////////////////////

bool CUIXmlInit::InitAnimatedStatic(CUIXml &xml_doc, const char *path, int index, CUIAnimatedStatic *pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	InitStatic(xml_doc, path, index, pWnd);
    
	float x				= xml_doc.ReadAttribFlt(path, index, "x_offset", 0);
	float y				= xml_doc.ReadAttribFlt(path, index, "y_offset", 0);
	u32 framesCount		= static_cast<u32>(xml_doc.ReadAttribInt(path, index, "frames", 0));
	u32 animDuration	= static_cast<u32>(xml_doc.ReadAttribInt(path, index, "duration", 0));
	u32 animCols		= static_cast<u32>(xml_doc.ReadAttribInt(path, index, "columns", 0));
	float frameWidth	= xml_doc.ReadAttribFlt(path, index, "frame_width", 0);
	float frameHeight	= xml_doc.ReadAttribFlt(path, index, "frame_height", 0);
	bool cyclic			= !!xml_doc.ReadAttribInt(path, index, "cyclic", 0);
	bool play			= !!xml_doc.ReadAttribInt(path, index, "autoplay", 0);

	pWnd->SetFrameDimentions(frameWidth, frameHeight);
	pWnd->SetFramesCount(framesCount);
	pWnd->m_bCyclic = cyclic;
	pWnd->SetAnimCols(animCols);
	pWnd->SetAnimationDuration(animDuration);
	pWnd->SetOffset(x,y);
	pWnd->SetAnimPos(0.0f);
	if (play) pWnd->Play();

	return true;
}

bool CUIXmlInit::InitSleepStatic(CUIXml &xml_doc, const char *path, int index, CUISleepStatic *pWnd)
{
//	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

//	InitStatic(xml_doc, path, index, pWnd);
    
	return true;
}

bool CUIXmlInit::InitTexture(CUIXml& xml_doc, LPCSTR path, int index, ITextureOwner* pWnd)
{
	string256 buf;
	LPCSTR texture	= NULL;
	LPCSTR shader	= NULL;
	xr_strconcat	(buf, path, ":texture");
	if (xml_doc.NavigateToNode(buf))
	{
		texture		= xml_doc.Read(buf, index, NULL);
		shader		= xml_doc.ReadAttrib(buf, index, "shader", NULL);
	}
	if (texture)
	{
		if(shader)
			pWnd->InitTextureEx(texture, shader);
		else
	       pWnd->InitTexture(texture);
	}
//--------------------
	Frect			rect;
	rect.x1			= xml_doc.ReadAttribFlt(buf, index, "x", 0);
	rect.y1			= xml_doc.ReadAttribFlt(buf, index, "y", 0);
	rect.x2			= rect.x1 + xml_doc.ReadAttribFlt(buf, index, "width", 0);	
	rect.y2			= rect.y1 + xml_doc.ReadAttribFlt(buf, index, "height", 0);

	bool stretch_flag = xml_doc.ReadAttribInt(path, index, "stretch") ? true : false;
	pWnd->SetStretchTexture(stretch_flag);

	u32 color = GetColor(xml_doc, buf, index, 0xff);
	pWnd->SetTextureColor(color);

	if (rect.width() != 0 && rect.height() != 0)
		pWnd->SetTextureRect(rect);

	return true;
}

bool CUIXmlInit::InitTextureOffset(CUIXml &xml_doc, LPCSTR path, int index, CUIStatic* pWnd){
    string256 textureOffset;
	if (0 == xr_strcmp(path, ""))
		xr_strcpy(textureOffset, "texture_offset");
	else
		xr_strconcat(textureOffset, path, ":texture_offset");

	float x = xml_doc.ReadAttribFlt(textureOffset, index, "x");
	float y = xml_doc.ReadAttribFlt(textureOffset, index, "y");

	pWnd->SetTextureOffset(x, y);

	return true;
}

bool CUIXmlInit::InitMultiTexture(CUIXml &xml_doc, LPCSTR path, int index, CUI3tButton* pWnd){
	string256 buff;
	bool success = false;

	xr_strconcat(buff, path, ":texture");
	shared_str texture = xml_doc.Read(buff, index, NULL);

	if (texture.size() > 0)
	{
		pWnd->InitTexture(*texture);
		return true;
	}

	xr_strconcat(buff, path, ":texture_e");
	texture = xml_doc.Read(buff, index, NULL);
	if (texture.size())
	{
		if ( pWnd->m_background )			
		{ 
			pWnd->m_background->InitState(S_Enabled, texture.c_str()); 
		}
		else if ( pWnd->m_back_frameline )	
		{ 
			pWnd->m_back_frameline->InitState(S_Enabled, texture.c_str()); 
			pWnd->m_back_frameline->Get(S_Enabled)->SetHorizontal(!(pWnd->vertical));
		}
		success = true;
	}

	xr_strconcat(buff, path, ":texture_t");
	texture = xml_doc.Read(buff, index, NULL);
	if (texture.size())
	{
		if ( pWnd->m_background )			
		{ 
			pWnd->m_background->InitState(S_Touched, texture.c_str()); 
		}
		else if ( pWnd->m_back_frameline )	
		{ 
			pWnd->m_back_frameline->InitState(S_Touched, texture.c_str()); 
			pWnd->m_back_frameline->Get(S_Touched)->SetHorizontal(!(pWnd->vertical));
		}
		success = true;
	}

	xr_strconcat(buff, path, ":texture_d");
	texture = xml_doc.Read(buff, index, NULL);
	if (texture.size())
	{
		if ( pWnd->m_background )			
		{ 
			pWnd->m_background->InitState(S_Disabled, texture.c_str()); 
		}
		else if ( pWnd->m_back_frameline )	
		{ 
			pWnd->m_back_frameline->InitState(S_Disabled, texture.c_str()); 
			pWnd->m_back_frameline->Get(S_Disabled)->SetHorizontal(!(pWnd->vertical));
		}
		success = true;
	}

	xr_strconcat(buff, path, ":texture_h");
	texture = xml_doc.Read(buff, index, NULL);   
	if (texture.size())
	{
		if ( pWnd->m_background )			
		{ 
			pWnd->m_background->InitState(S_Highlighted, texture.c_str()); 
		}
		else if ( pWnd->m_back_frameline )	
		{ 
			pWnd->m_back_frameline->InitState(S_Highlighted, texture.c_str()); 
			pWnd->m_back_frameline->Get(S_Highlighted)->SetHorizontal(!(pWnd->vertical));
		}
		success = true;
	}

	if (success)
        pWnd->TextureOn();

	return success;
}


float CUIXmlInit::ApplyAlignX(float coord, u32 align)
{
	return coord;

}

//////////////////////////////////////////////////////////////////////////

float CUIXmlInit::ApplyAlignY(float coord, u32 align)
{
	return coord;

}

//////////////////////////////////////////////////////////////////////////

void CUIXmlInit::ApplyAlign(float &x, float &y, u32 align)
{
	x = ApplyAlignX(x, align);
	y = ApplyAlignY(y, align);
}

//////////////////////////////////////////////////////////////////////////

bool CUIXmlInit::InitAlignment(CUIXml &xml_doc, const char *path,
							   int index, float &x, float &y,CUIWindow* pWnd)
{
	xr_string wnd_alignment = xml_doc.ReadAttrib(path, index, "alignment", "");
	
	if ( strchr(wnd_alignment.c_str(), 'c') )
		pWnd->SetAlignment(waCenter);


	// Alignment: right: "r", bottom: "b". Top, left - useless
	shared_str	alignStr = xml_doc.ReadAttrib(path, index, "align", "");

    bool result = false;

	if (strchr(*alignStr, 'r'))
	{
		x = ApplyAlignX(x, alRight);
		result = true;
	}
	if (strchr(*alignStr, 'b'))
	{
		y = ApplyAlignY(y, alBottom);
		result = true;
	}
	if (strchr(*alignStr, 'c'))
	{
		ApplyAlign(x, y, alCenter);
		result = true;
	}

	return result;
}

//////////////////////////////////////////////////////////////////////////

void CUIXmlInit::InitColorDefs()
{
	if (NULL != m_pColorDefs) return;

	m_pColorDefs = new ColorDefs();

	CUIXml					uiXml;
	uiXml.Load				(CONFIG_PATH, UI_PATH, COLOR_DEFINITIONS);

	int num = uiXml.GetNodesNum("colors", 0, "color");

	shared_str name;
	int r, b, g, a;

	for (int i = 0; i < num; ++i)
	{
		name	= uiXml.ReadAttrib("color", i, "name", "");
		r		= uiXml.ReadAttribInt("color", i, "r", 0);
		g		= uiXml.ReadAttribInt("color", i, "g", 0);
		b		= uiXml.ReadAttribInt("color", i, "b", 0);
		a		= uiXml.ReadAttribInt("color", i, "a", 255);
		
		(*m_pColorDefs)[name] = color_argb(a,r,g,b);
	}
}

bool CUIXmlInit::InitScrollView	(CUIXml& xml_doc, LPCSTR path, int index, CUIScrollView* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	InitWindow							(xml_doc, path, index, pWnd);
	pWnd->SetRightIndention				(xml_doc.ReadAttribFlt	(path, index, "right_ident",	0.0f));
	pWnd->SetLeftIndention				(xml_doc.ReadAttribFlt	(path, index, "left_ident",		0.0f));
	pWnd->SetUpIndention				(xml_doc.ReadAttribFlt	(path, index, "top_indent",		0.0f));
	pWnd->SetDownIndention				(xml_doc.ReadAttribFlt	(path, index, "bottom_indent",	0.0f));

	float vi							= xml_doc.ReadAttribFlt	(path, index, "vert_interval", 0.0f);
	pWnd->m_vertInterval				= (vi);

	bool bInverseDir					= (1==xml_doc.ReadAttribInt	(path, index, "inverse_dir", 0));
	pWnd->m_flags.set					(CUIScrollView::eInverseDir,bInverseDir);
	
	pWnd->SetScrollBarProfile			(xml_doc.ReadAttrib(path, index, "scroll_profile", "default"));

	pWnd->InitScrollView				();

	bool bVertFlip						= (1==xml_doc.ReadAttribInt	(path, index, "flip_vert", 0));
	pWnd->SetVertFlip					(bVertFlip);

	bool b = (1==xml_doc.ReadAttribInt(path, index, "always_show_scroll",1));

	pWnd->SetFixedScrollBar(b);

	b = (1==xml_doc.ReadAttribInt(path, index, "can_select",1));

	pWnd->m_flags.set					(CUIScrollView::eItemsSelectabe, b);

/////////////////////////////////////////////////////////////////////
	int tabsCount	= xml_doc.GetNodesNum(path, index, "text");

	XML_NODE* _stored_root = xml_doc.GetLocalRoot();
	xml_doc.SetLocalRoot(xml_doc.NavigateToNode(path,index));

	for (int i = 0; i < tabsCount; ++i)
	{
		CUITextWnd* newText				= new CUITextWnd();
		InitText						(xml_doc, "text", i, &newText->TextItemControl());
		newText->SetTextComplexMode		(true);
		newText->SetWidth				(pWnd->GetDesiredChildWidth());
		newText->AdjustHeightToText		();
		pWnd->AddWindow					(newText, true);
	}
	xml_doc.SetLocalRoot(_stored_root);
	return								true;
}

bool CUIXmlInit::InitListBox(CUIXml& xml_doc, LPCSTR path, int index, CUIListBox* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);

	InitScrollView(xml_doc, path, index, pWnd);

	string512		_path;
	u32				t_color;
	CGameFont*		pFnt;
	xr_strconcat	(_path, path, ":font");
	InitFont		(xml_doc, _path, index, t_color, pFnt);
	pWnd->SetTextColor	(t_color);
	pWnd->SetFont		(pFnt);

	xr_strconcat		(_path, path, ":font_s");	
	t_color = GetColor	(xml_doc, _path, index, 0x00);
	pWnd->SetTextColorS	(t_color);

	float h					= xml_doc.ReadAttribFlt(path, index, "item_height", 20.0f);
	pWnd->SetItemHeight		(h);

	shared_str selection_texture = xml_doc.ReadAttrib(_path, index, "selection_texture", "ui_listline");
	pWnd->SetSelectionTexture(*selection_texture);

	// Load font alignment
	shared_str al = xml_doc.ReadAttrib(_path, index, "align");
	if (0 == xr_strcmp(al, "c"))
		pWnd->SetTextAlignment(CGameFont::alCenter);

	else if (0 == xr_strcmp(al, "r"))
		pWnd->SetTextAlignment(CGameFont::alRight);

	else if (0 == xr_strcmp(al, "l"))
		pWnd->SetTextAlignment(CGameFont::alLeft);

	return true;
}

bool CUIXmlInit::InitListWnd(CUIXml& xml_doc, LPCSTR path, int index, CUIListWnd* pWnd)
{
	R_ASSERT4(xml_doc.NavigateToNode(path,index), "XML node not found", path, xml_doc.m_xml_file_name);
	
	float x								= xml_doc.ReadAttribFlt(path, index, "x");
	float y								= xml_doc.ReadAttribFlt(path, index, "y");

	InitAlignment						(xml_doc, path, index, x, y, pWnd);

	float width							= xml_doc.ReadAttribFlt(path, index, "width");
	float height						= xml_doc.ReadAttribFlt(path, index, "height");
	float item_height					= xml_doc.ReadAttribFlt(path, index, "item_height");
	int active_background				= xml_doc.ReadAttribInt(path, index, "active_bg");

	// Init font from xml config file
	string256							buf;
	CGameFont *LocalFont				= NULL;
	u32 cl;

	shared_str text_path				= xr_strconcat(buf,path,":font");
	InitFont							(xml_doc, *text_path, index, cl, LocalFont);
	if (LocalFont)
	{
		pWnd->SetFont(LocalFont);
		pWnd->SetTextColor(cl);
	}

	pWnd->SetScrollBarProfile			(xml_doc.ReadAttrib(path, index, "scroll_profile", "default"));
	pWnd->InitListWnd							(x,y, width,height,item_height);
	pWnd->EnableActiveBackground		(!!active_background);

	if (xml_doc.ReadAttribInt(path, index, "always_show_scroll"))
	{
		pWnd->SetAlwaysShowScroll		(true);
		pWnd->EnableAlwaysShowScroll	(true);
		pWnd->EnableScrollBar			(true);
	}

	if (xml_doc.ReadAttribInt(path, index, "always_hide_scroll"))
	{
		pWnd->SetAlwaysShowScroll		(false);
		pWnd->EnableAlwaysShowScroll	(true);		
	}


	bool bVertFlip						= (1==xml_doc.ReadAttribInt	(path, index, "flip_vert", 0));
	pWnd->SetVertFlip					(bVertFlip);

	return true;
}

bool CUIXmlInit::InitTrackBar(CUIXml& xml_doc, LPCSTR path, int index, CUITrackBar* pWnd)
{
	InitWindow			(xml_doc, path, 0, pWnd);
	pWnd->InitTrackBar	(pWnd->GetWndPos(),pWnd->GetWndSize());
	int is_integer		= xml_doc.ReadAttribInt(path, index, "is_integer", 0);
	pWnd->SetType		(!is_integer);
	InitOptionsItem		(xml_doc, path, 0, pWnd);

	int invert			= xml_doc.ReadAttribInt(path, index, "invert", 0);
	pWnd->SetInvert		(!!invert);
	float step			= xml_doc.ReadAttribFlt(path, index, "step", 0.1f);
	pWnd->SetStep		(step);
	
	bool stretch_BE_flag	= !!xml_doc.ReadAttribInt(path, index, "stretch_b_e_textures", 1);
	pWnd->SetStretchBETextures(stretch_BE_flag);

	return				true;
}

bool CUIXmlInit::InitTrackBarVariable(CUIXml& xml_doc, LPCSTR path, int index, CUITrackBarVariable* pWnd)
{
	InitWindow(xml_doc, path, 0, pWnd);
	pWnd->InitTrackBar(pWnd->GetWndPos(), pWnd->GetWndSize());

	int is_integer = xml_doc.ReadAttribInt(path, index, "is_integer", 0);
	pWnd->SetType(!is_integer);

	int invert = xml_doc.ReadAttribInt(path, index, "invert", 0);
	pWnd->SetInvert(!!invert);

	float step = xml_doc.ReadAttribFlt(path, index, "step", 0.1f);
	pWnd->SetStep(step);

	if (is_integer){
		int i_min = xml_doc.ReadAttribInt(path, index, "int_min", 0);
		int i_max = xml_doc.ReadAttribInt(path, index, "int_max", 10);
		pWnd->SetOptIBounds(i_min, i_max);
	}
	else{
		float float_min = xml_doc.ReadAttribFlt(path, index, "float_min", 0.1f);
		float float_max = xml_doc.ReadAttribFlt(path, index, "float_max", 0.1f);
		pWnd->SetOptFBounds(float_min, float_max);
	}

	bool stretch_BE_flag = !!xml_doc.ReadAttribInt(path, index, "stretch_b_e_textures", 1);
	pWnd->SetStretchBETextures(stretch_BE_flag);

	return				true;
}

bool CUIXmlInit::InitComboBox(CUIXml& xml_doc, LPCSTR path, int index, CUIComboBox* pWnd)
{
	u32							color;
	CGameFont*					pFont;

	pWnd->SetListLength			(xml_doc.ReadAttribInt(path, index, "list_length", 4));

	InitWindow					(xml_doc, path, index, pWnd);
	pWnd->InitComboBox			(pWnd->GetWndPos(),pWnd->GetWidth());
	InitOptionsItem				(xml_doc, path, index, pWnd);

	bool b = (1==xml_doc.ReadAttribInt(path, index, "always_show_scroll",1));

	pWnd->m_list_box.SetFixedScrollBar(b);

	string512					_path;
	xr_strconcat				(_path, path, ":list_font");
	InitFont					(xml_doc, _path, index, color, pFont);
	pWnd->SetFont				(pFont);
	pWnd->m_list_box.SetFont		(pFont);
	pWnd->m_list_box.SetTextColor	(color);
	xr_strconcat				(_path, path, ":list_font_s");	
	InitFont					(xml_doc, _path, index, color, pFont);
	pWnd->m_list_box.SetTextColorS	(color);
	
	xr_strconcat				(_path, path, ":text_color:e");
	if (xml_doc.NavigateToNode(_path, index)){
		color					= GetColor(xml_doc, _path, index, 0x00);
		pWnd->SetTextColor		(color);	
	}

	xr_strconcat				(_path, path, ":text_color:d");
	if (xml_doc.NavigateToNode(_path, index)){
		color					= GetColor(xml_doc, _path, index, 0x00);
		pWnd->SetTextColorD		(color);
	}

	bool stretch_BE_flag	= !!xml_doc.ReadAttribInt(path, index, "stretch_b_e_textures", 1);
	pWnd->m_frameLine.SetStretchBETextures(stretch_BE_flag);

	return true;
}

void CUIXmlInit::AssignColor(LPCSTR name, u32 clr)
{
	(*m_pColorDefs)[name] = clr;
}

u32	CUIXmlInit::GetColor(CUIXml& xml_doc, LPCSTR path, int index, u32 def_clr)
{
	LPCSTR clr_def = xml_doc.ReadAttrib(path, index, "color", NULL);
	if(clr_def){
		VERIFY(GetColorDefs()->find(clr_def) != GetColorDefs()->end());
		return 	(*m_pColorDefs)[clr_def];
	}else	{
		int r = xml_doc.ReadAttribInt(path, index, "r", def_clr);
		int g = xml_doc.ReadAttribInt(path, index, "g", def_clr);
		int b = xml_doc.ReadAttribInt(path, index, "b", def_clr);
		int a = xml_doc.ReadAttribInt(path, index, "a", 0xff);
		return color_argb(a,r,g,b);
	}

}

bool CUIXmlInit::InitHintWindow(CUIXml& xml_doc, LPCSTR path, int index, UIHintWindow* pWnd)
{
	VERIFY( pWnd );
	InitWindow( xml_doc, path, index, pWnd );
	LPCSTR hint_text = xml_doc.Read( path, index, "no hint" );
	pWnd->set_hint_text_ST( hint_text );

	pWnd->set_hint_delay( (u32)xml_doc.ReadAttribInt( path, index, "delay" ) );
	return true;
}

xr_vector<CUIStatic*> CUIXmlInit::InitAutoStatic(CUIXml& xml_doc, LPCSTR tag_name, CUIWindow* pParentWnd)
{
	int items_num = xml_doc.GetNodesNum(xml_doc.GetRoot(), tag_name);
	// tmp statics vector
	xr_vector<CUIStatic*>	tmpVec;

	CUIStatic* pUIStatic = NULL;
	for(int i=0; i<items_num; i++)
	{
		pUIStatic = new CUIStatic();
		InitStatic(xml_doc, tag_name, i, pUIStatic);
		pUIStatic->SetAutoDelete(true);
		pParentWnd->AttachChild(pUIStatic);
		tmpVec.push_back(pUIStatic);
		pUIStatic = NULL;
	}

	return tmpVec;
}
