#pragma once
class XRayFontRender:public IFontRender
{
public:
	XRayFontRender();
	~XRayFontRender();
	virtual void Initialize(LPCSTR cShader, LPCSTR cTexture) ;
	virtual void OnRender(CGameFont &owner) ;
	void CreateFontAtlas(u32 width, u32 height, const char* name, void* bitmap) override;

private:
};
