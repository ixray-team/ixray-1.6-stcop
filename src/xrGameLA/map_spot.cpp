#include "stdafx.h"
#include "map_spot.h"
#include "map_location.h"

#include "ui/UIXmlInit.h"
#include "ui/UIMApWnd.h"
#include "level.h"
#include "../xrEngine/xr_object.h"
#include "object_broker.h"
#include "ui/UITextureMaster.h"

CMapSpot::CMapSpot(CMapLocation* ml)
:m_map_location(ml)
{
	m_bScale			= false;
}

CMapSpot::~CMapSpot()
{
}

void CMapSpot::Load(CUIXml* xml, LPCSTR path)
{
	CUIXmlInit::InitStatic(*xml,path,0,this);
	int i = xml->ReadAttribInt(path, 0, "scale", 0);
	m_bScale			= (i==1);

	m_originSize		= GetWndSize();
}

LPCSTR CMapSpot::GetHint() 
{
	return MapLocation()->GetHint();
};

void CMapSpot::Update()
{
	inherited::Update();
	if(m_bCursorOverWindow){
		VERIFY(m_dwFocusReceiveTime>=0);
		if( Device.dwTimeGlobal>(m_dwFocusReceiveTime+500) ){
			GetMessageTarget()->SendMessage(this, MAP_SHOW_HINT, NULL);
		}
	}
}

bool CMapSpot::OnMouseDown		(int mouse_btn)
{
		return false;
}


void CMapSpot::OnFocusLost		()
{
	inherited::OnFocusLost		();
	GetMessageTarget()->SendMessage(this, MAP_HIDE_HINT, NULL);
}


CMapSpotPointer::CMapSpotPointer(CMapLocation* ml)
:inherited(ml)
{
}

CMapSpotPointer::~CMapSpotPointer()
{
}

LPCSTR CMapSpotPointer::GetHint()
{
	return NULL;
}

//////////////////////////////////////////////////
CMiniMapSpot::CMiniMapSpot(CMapLocation* ml)
:inherited(ml)
{
}

CMiniMapSpot::~CMiniMapSpot()
{
}

void CMiniMapSpot::Load(CUIXml* xml, LPCSTR path)
{
	inherited::Load(xml,path);

	string256 buf;
	XML_NODE* n = NULL;
	
	Frect base_rect;
	base_rect.x1 = 0;
	base_rect.y1 = 0;
	base_rect.x2 = xml->ReadAttribFlt(path, 0, "width", 0);
	base_rect.y2 = xml->ReadAttribFlt(path, 0, "height", 0);

	Frect _stored_rect = m_UIStaticItem.GetTextureRect();

	strconcat(sizeof(buf), buf, path, ":texture_above");
	n = xml->NavigateToNode(buf,0);
	if(n){
		LPCSTR texture  = xml->Read(buf, 0, NULL);
		CUITextureMaster::InitTexture	(texture, &m_UIStaticItem);
		if(strchr(texture,'\\'))
		{
			float x					= xml->ReadAttribFlt(buf, 0, "x", base_rect.x1);
			float y					= xml->ReadAttribFlt(buf, 0, "y", base_rect.y1);
			float width				= xml->ReadAttribFlt(buf, 0, "width", base_rect.width());
			float height			= xml->ReadAttribFlt(buf, 0, "height", base_rect.height());
			m_tex_rect_above.set	(x,y,x+width,y+height);
		}else
			m_tex_rect_above		= m_UIStaticItem.GetTextureRect();

		m_icon_above				= m_UIStaticItem.GetShader		();
	}

	strconcat(sizeof(buf),buf, path, ":texture_below");
	n = xml->NavigateToNode(buf,0);
	if(n){
		LPCSTR texture  = xml->Read(buf, 0, NULL);
		CUITextureMaster::InitTexture	(texture, &m_UIStaticItem);
		if(strchr(texture,'\\'))
		{
			float x					= xml->ReadAttribFlt(buf, 0, "x", base_rect.x1);
			float y					= xml->ReadAttribFlt(buf, 0, "y", base_rect.y1);
			float width				= xml->ReadAttribFlt(buf, 0, "width", base_rect.width());
			float height			= xml->ReadAttribFlt(buf, 0, "height", base_rect.height());
			m_tex_rect_below.set	(x,y,x+width,y+height);
		}else
			m_tex_rect_below		= m_UIStaticItem.GetTextureRect();

		m_icon_below				= m_UIStaticItem.GetShader		();
	}
	strconcat(sizeof(buf),buf, path, ":texture");
	n = xml->NavigateToNode(buf,0);
	if(n){
		LPCSTR texture  = xml->Read(buf, 0, NULL);
		CUITextureMaster::InitTexture	(texture, &m_UIStaticItem);
		if(strchr(texture,'\\'))
		{
			float x					= xml->ReadAttribFlt(buf, 0, "x", base_rect.x1);
			float y					= xml->ReadAttribFlt(buf, 0, "y", base_rect.y1);
			float width				= xml->ReadAttribFlt(buf, 0, "width", base_rect.width());
			float height			= xml->ReadAttribFlt(buf, 0, "height", base_rect.height());
			m_tex_rect_normal.set	(x,y,x+width,y+height);
		}else
			m_tex_rect_normal		= m_UIStaticItem.GetTextureRect();

		m_icon_normal				= m_UIStaticItem.GetShader		();
	}

	m_UIStaticItem.SetTextureRect	(_stored_rect);
}

void CMiniMapSpot::Draw()
{
	CObject* O = Level().CurrentViewEntity();
	if(O&&m_icon_above->inited()&&m_icon_below->inited()){
		float ml_y = MapLocation()->GetLastPosition().y;
		float d = O->Position().y-ml_y;

		if(d>1.8f){
			GetUIStaticItem().SetShader			(m_icon_below);
			GetUIStaticItem().SetTextureRect	(m_tex_rect_below);
		}else
		if(d<-1.8f){
			GetUIStaticItem().SetShader			(m_icon_above);
			GetUIStaticItem().SetTextureRect	(m_tex_rect_above);
		}else{
			GetUIStaticItem().SetShader			(m_icon_normal);
			GetUIStaticItem().SetTextureRect	(m_tex_rect_normal);
		}
	};

	inherited::Draw();
}