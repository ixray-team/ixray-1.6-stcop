#pragma once

#include "TiramisuRenderTypes.h"
class TiramisuRenderTextureSequence;

// Загружает и кэширует runtime textures по нормализованному имени.
class TiramisuRenderTexturesManager
{
public:
					TiramisuRenderTexturesManager    ();
					~TiramisuRenderTexturesManager   ();
	
	TiramisuRenderTexture*		GetTexture				(const shared_str& InName, bool bSrgb = false);
	void				Free					(TiramisuRenderTexture* InTexture);
	void				FlushNextFrame			();
	void				Copy					(TiramisuRenderTexture* texture);

private:
	xr_map<shared_str, TiramisuRenderTexture*>		TexturesDynamic;
	xr_map<shared_str, TiramisuRenderTexture*>		Textures;
	xr_map<shared_str, TiramisuRenderTexture*>		FreeTexturesNextFrame;
	xr_set<shared_str>						ErrorTextures;
};
