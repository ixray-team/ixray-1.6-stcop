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

void EImageThumbnail::Update(IRHISurface*& Texture)
{
	if (m_Pixels.empty())
	{
		if (Texture)
		{
			Texture->Release();
			Texture = nullptr;
		}
		return;
	}

	RHITextureDesc Desc;
	Desc.Width = THUMB_WIDTH;
	Desc.Height = THUMB_HEIGHT;
	Desc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
	Desc.MipLevels = 1;
	Desc.ArraySize = 1;
	Desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
	Desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;

	if (Texture)
	{
		if (Texture->GetWidth() != Desc.Width ||
			Texture->GetHeight() != Desc.Height ||
			Texture->GetFormat() != Desc.Format)
		{
			Texture->Release();
			Texture = nullptr;
		}
	}

	RHISubResource SubResource{};
	SubResource.Width = THUMB_WIDTH;
	SubResource.Height = THUMB_HEIGHT;
	SubResource.TextureFormat = Desc.Format;
	SubResource.RowPitch = THUMB_WIDTH * 4;
	SubResource.DepthPitch = 0;

	xr_vector<u8> FlippedData(THUMB_WIDTH * THUMB_HEIGHT * 4);
	for (int Y = 0; Y < THUMB_HEIGHT; ++Y)
	{
		const u8* Src = (u8*)Pixels() + (THUMB_WIDTH * (THUMB_HEIGHT - Y - 1)) * 4;
		u8* Dst = FlippedData.data() + Y * THUMB_WIDTH * 4;
		memcpy(Dst, Src, THUMB_WIDTH * 4);
	}
	SubResource.Data = FlippedData.data();

	if (!Texture)
	{
		Texture = GRHI->CreateTexture2D(Desc, SubResource);
	}
	else
	{
		RHIBox box;
		box.left = 0;
		box.top = 0;
		box.front = 0;
		box.right = THUMB_WIDTH;
		box.bottom = THUMB_HEIGHT;
		box.back = 1;

		Texture->UpdateData(0, 0, &SubResource, box);
	}
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


