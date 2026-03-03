#include "StdAfx.h"
#include "player_hud.h"

dbg_render_obb::dbg_render_obb()
{
	Device.seqRender.Add(this, REG_PRIORITY_LOW - 1000);
	Primitives_Shader->create("hud\\crosshair");
}

dbg_render_obb::~dbg_render_obb()
{
	Primitives_Shader->destroy();
	Device.seqRender.Remove(this);
}

void dbg_render_obb::PushPoint_to_render(const Fvector& coords, const u32& color)
{
	Fvector p;
	float w = coords.x * Device.mFullTransform._14 + coords.y * Device.mFullTransform._24 + coords.z * Device.mFullTransform._34 + Device.mFullTransform._44;
	if (w >= 0)
	{
		Device.mFullTransform.transform(p, coords);

		p.x = (float)iFloor((p.x + 1) * Device.TargetWidth * 0.5f);
		p.y = (float)iFloor((-p.y + 1) * Device.TargetHeight * 0.5f);
		UIRender->PushPoint(p.x, p.y, 0, color, 0, 0);
	}
}

void dbg_render_obb::append_obb(const Fobb& obb) { obbs.push_back(obb); }

void dbg_render_obb::OnRender()
{
	draw_obbs();
}

void dbg_render_obb::draw_obbs()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_HudAdjustManager)])
	{
		return;
	}

	if (obbs.empty())
	{
		return;
	}

	u16 aabb_id[24] = { 0,1,  1,2,  2,3,  3,0,  4,5,  5,6,  6,7,  7,4,  1,5,  2,6,  3,7,  0,4 };

	UIRender->StartPrimitive(obbs.size() * 24, IUIRender::ptLineList, IUIRender::ePointType::pttTL);

	Fvector aabb[8];
	Fmatrix matrix;
	for (Fobb& obb : obbs)
	{
		obb.xform_full(matrix);
		matrix.transform_tiny(aabb[0], Fvector().set(-1, -1, -1));
		matrix.transform_tiny(aabb[1], Fvector().set(-1, +1, -1));
		matrix.transform_tiny(aabb[2], Fvector().set(+1, +1, -1));
		matrix.transform_tiny(aabb[3], Fvector().set(+1, -1, -1));
		matrix.transform_tiny(aabb[4], Fvector().set(-1, -1, +1));
		matrix.transform_tiny(aabb[5], Fvector().set(-1, +1, +1));
		matrix.transform_tiny(aabb[6], Fvector().set(+1, +1, +1));
		matrix.transform_tiny(aabb[7], Fvector().set(+1, -1, +1));

		for (u8 i = 0; i < 24; ++i)
		{
			PushPoint_to_render(aabb[aabb_id[i]], color);
		}
	}

	UIRender->CacheSetXformWorld(Fidentity);
	UIRender->SetShader(*Primitives_Shader);
	UIRender->FlushPrimitive();

	obbs.clear();
}