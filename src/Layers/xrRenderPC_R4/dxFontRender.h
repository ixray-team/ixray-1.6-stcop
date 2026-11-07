#pragma once

#include "../../Include/xrRender/FontRender.h"
#include "../../Include/xrRender/UITransform.h"

class dxFontRender : public IFontRender
{
	struct GamepadIcon
	{
		shared_str texture_name;
		shared_str file_name;
		Frect rect;
		Fvector2 pos;
		Fvector2 sz;
		ref_shader shader;
		u8 alpha;
		UITransform2D Transform;
	};
public:
	dxFontRender();
	virtual ~dxFontRender() override;

	virtual void Initialize(const char* cShader, const char* cTexture) override;
	virtual void OnRender(CGameFont& owner) override;

	virtual void CreateFontAtlas(u32 width, u32 height, const char* name, void* bitmap) override;
	virtual bool GetAtlasTexSize(u32& outW, u32& outH) const override;

	void RenderBase(CGameFont& owner);
	void RenderIcon(GamepadIcon icon);

private:
	ref_shader				pShader;
	ref_geom				pGeom;
	ref_texture				pTexture;
	xr_vector<GamepadIcon> IconsToRender;
};