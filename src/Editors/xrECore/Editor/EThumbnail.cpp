#include "stdafx.h"


#include "EThumbnail.h"

//------------------------------------------------------------------------------
// Custom Thumbnail
//------------------------------------------------------------------------------
ECustomThumbnail::ECustomThumbnail(const char* src_name, THMType type)
{
	m_Type		= type;
    m_SrcName   = src_name;
	m_Name 		= ChangeFileExt(xr_string(src_name),".thm");
    m_Age		= 0;
}

//------------------------------------------------------------------------------
ECustomThumbnail::~ECustomThumbnail()
{
}

//------------------------------------------------------------------------------
// Image Thumbnail
//------------------------------------------------------------------------------
EImageThumbnail::~EImageThumbnail()
{
	m_Pixels.clear();
}

void EImageThumbnail::VFlip()
{
	R_ASSERT(!m_Pixels.empty());
	u32 line[THUMB_WIDTH];
    u32 sz_ln=sizeof(u32)*THUMB_WIDTH;
    u32 y2 = THUMB_WIDTH-1;
    for (int y=0; y<THUMB_HEIGHT/2; y++,y2--){
    	CopyMemory(line,m_Pixels.data()+y2*THUMB_WIDTH,sz_ln);
    	CopyMemory(m_Pixels.data()+y2*THUMB_WIDTH,m_Pixels.data()+y*THUMB_WIDTH,sz_ln);
    	CopyMemory(m_Pixels.data()+y*THUMB_WIDTH,line,sz_ln);
    }
}

void EImageThumbnail::CreatePixels(u32* p, u32 w, u32 h)
{
    R_ASSERT(p && (w > 0) && (h > 0));
    m_Pixels.resize(THUMB_SIZE);
    DXTUtils::Filter::Process(m_Pixels.data(), THUMB_WIDTH, THUMB_HEIGHT, p, w, h, DXTUtils::Filter::imf_mitchell);
}

ECORE_API EImageThumbnail* CreateThumbnail(const char* src_name, ECustomThumbnail::THMType type, bool bLoad)
{
    switch (type){
    case ECustomThumbnail::ETObject: 	return new EObjectThumbnail	(src_name,bLoad);
    case ECustomThumbnail::ETTexture:	return new ETextureThumbnail(src_name,bLoad);
    case ECustomThumbnail::ETGroup:		return new EGroupThumbnail	(src_name,bLoad);
    default: NODEFAULT;
    }
    return nullptr;              
}


