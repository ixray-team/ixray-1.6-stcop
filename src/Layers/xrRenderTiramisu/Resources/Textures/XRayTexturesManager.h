#pragma once
class XRayTextureSeq;

class XRayTexturesManager
{
public:
					XRayTexturesManager    ();
					~XRayTexturesManager   ();
	
	XRayTexture*    GetTexture             (const shared_str& InName, bool bSrgb = false);
	void            Free                   (XRayTexture* InTexture);
	void            FlushNextFrame         ();
	void            Copy                   (XRayTexture* texture);

private:
	xr_map<shared_str, XRayTextureSeq*> TexturesSeq;
	xr_map<shared_str, XRayTexture*>    Textures;
	xr_map<shared_str, XRayTexture*>    FreeTexturesNextFrame;
	xr_set<shared_str>                  ErrorTextures;
};
