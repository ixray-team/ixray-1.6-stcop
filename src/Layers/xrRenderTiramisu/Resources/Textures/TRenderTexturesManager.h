#pragma once
class TRenderTextureSeq;

class TRenderTexturesManager
{
public:
					TRenderTexturesManager    ();
					~TRenderTexturesManager   ();
	
	TRenderTexture*    GetTexture             (const shared_str& InName, bool bSrgb = false);
	void            Free                   (TRenderTexture* InTexture);
	void            FlushNextFrame         ();
	void            Copy                   (TRenderTexture* texture);

private:
	xr_map<shared_str, TRenderTexture*>	TexturesDynamic;
	xr_map<shared_str, TRenderTexture*>    Textures;
	xr_map<shared_str, TRenderTexture*>    FreeTexturesNextFrame;
	xr_set<shared_str>                  ErrorTextures;
};
