#pragma once
class CDS0_FontRender:public IFontRender
{
public:
	CDS0_FontRender();
	virtual ~CDS0_FontRender() override;
	virtual void Initialize(LPCSTR cShader, LPCSTR cTexture) override;
	virtual void OnRender(CGameFont &owner) override;
	void CreateFontAtlas(u32 width, u32 height, const char* name, void* bitmap) override;

private:
};
