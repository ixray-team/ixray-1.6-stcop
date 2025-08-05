#include "stdafx.h"


#include "EThumbnail.h"

//------------------------------------------------------------------------------
// Custom Thumbnail
//------------------------------------------------------------------------------
ECustomThumbnail::ECustomThumbnail(LPCSTR src_name, THMType type)
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

void EImageThumbnail::Update(ID3DBaseTexture*& Texture)
{
    if (m_Pixels.size() == 0)
    {
        if (Texture)
            IM_TEXTURE_RELEASE(Texture);

        Texture = nullptr;

        return;
    }

    ID3DTexture2D* pTexture = nullptr;
    if (Texture != nullptr)
    {
        R_CHK(((IDirect3DBaseTexture9*)(Texture))->QueryInterface(__uuidof(ID3DTexture2D), (void**)&pTexture));
    }
    else
    {
        R_CHK(REDevice->CreateTexture(THUMB_WIDTH, THUMB_HEIGHT, 1, 0, D3DFMT_A8R8G8B8, D3DPOOL_MANAGED, &pTexture, 0));
        Texture = pTexture;
    }

    D3DLOCKED_RECT rect;
    R_CHK(pTexture->LockRect(0, &rect, 0, D3DLOCK_DISCARD));
    for (int i = 0; i < THUMB_HEIGHT; i++)
    {

        unsigned char* dest = static_cast<unsigned char*>(rect.pBits) + (rect.Pitch * i);
        memcpy(dest, Pixels() + (THUMB_WIDTH * (THUMB_HEIGHT - i - 1)), sizeof(unsigned char) * THUMB_WIDTH * 4);
    }
    R_CHK(pTexture->UnlockRect(0));
}

ECORE_API EImageThumbnail* CreateThumbnail(LPCSTR src_name, ECustomThumbnail::THMType type, bool bLoad)
{
    switch (type){
    case ECustomThumbnail::ETObject: 	return new EObjectThumbnail	(src_name,bLoad);
    case ECustomThumbnail::ETTexture:	return new ETextureThumbnail(src_name,bLoad);
    case ECustomThumbnail::ETGroup:		return new EGroupThumbnail	(src_name,bLoad);
    default: NODEFAULT;
    }
    return 0;              
}


