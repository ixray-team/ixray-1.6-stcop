#include "stdafx.h"
#include "FontAtlas.h"

u32 CFontAtlas::AtlasTextureCounter = 0;


CFontAtlas::CFontAtlas()
{
	string128 textureName;
	xr_sprintf(textureName, "$user$fontatlas_%u", AtlasTextureCounter++); //#TODO optimize
	Name = textureName;
}

CFontAtlas::CFontAtlas(CFontAtlas&& Other)
{
	operator=(std::move(Other));
}

CFontAtlas::CFontAtlas(const CFontAtlas& Other)
{
	operator=(Other);
}

CFontAtlas& CFontAtlas::operator=(CFontAtlas&& Other)
{
	FontRendersPerShader = std::move(Other.FontRendersPerShader);
	UsedHeight = Other.UsedHeight;
	Name = Other.Name;
	Users = std::move(Other.Users);
	bTextureCreated = Other.bTextureCreated;
	return *this;
}

CFontAtlas::~CFontAtlas()
{
	for (auto& [ShaderName, FontRenderer] : FontRendersPerShader)
	{
		::RenderFactory->DestroyFontRender(FontRenderer);
	}
	FontRendersPerShader.clear();
}

CFontAtlas& CFontAtlas::operator=(const CFontAtlas& Other)
{
	R_ASSERT(false && "CFontAtlas should not be copied");

	return *this;
}

u32 CFontAtlas::GetFreeHeight() const
{
	return TextureDimension - UsedHeight;
}

u32 CFontAtlas::GetUsedHeight() const
{
	return UsedHeight;
}

IFontRender* CFontAtlas::AddFontUser(CGameFont * const User, shared_str ShaderName, void* GreyscaleBitmap, u32 HeightRequired)
{
	VERIFY(std::find(Users.begin(), Users.end(), User) == Users.end());

	Users.push_back(User);

	IFontRender* FontRender = nullptr;
	auto FontRenderIter = FontRendersPerShader.find(ShaderName);
	if (FontRenderIter == FontRendersPerShader.end())
	{
		FontRender = ::RenderFactory->CreateFontRender();
		if (!bTextureCreated)
		{
			FontRender->CreateFontAtlas(TextureDimension, TextureDimension, Name.c_str(), GreyscaleBitmap);
			bTextureCreated = true;
		}
		FontRender->Initialize(ShaderName.c_str(), Name.c_str());
		FontRendersPerShader.emplace(std::make_pair(ShaderName, FontRender));
	}
	else
	{
		FontRender = FontRenderIter->second;
		FontRender->UpdatePartOfFontAtlas(UsedHeight, HeightRequired, Name.c_str(), GreyscaleBitmap);
	}
	
	UsedHeight += HeightRequired;
	return FontRender;
}

bool CFontAtlas::RemoveFontUser(CGameFont* User)
{
	Users.remove(User);
	return !HasUsers();
}

bool CFontAtlas::HasUsers() const
{
	return !Users.empty();
}

