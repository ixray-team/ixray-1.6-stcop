#include "stdafx.h"

#include "uv_tri.h"
#include "xrFace.h"
#include "uv_grid.h"

bool	UVtri::similar	( const UVtri &uv, float eps/*eps = EPS*/ ) const
{
	return uv.owner == owner && _TCF::similar( uv, eps );
}

void UVtri::computeAABB(const Fbox2& bounds)
{
    R_ASSERT(bounds.min.x != bounds.max.x);
    R_ASSERT(bounds.min.y != bounds.max.y);

    // 🔥 AABB в UV
    Fvector2 uv_min;
    Fvector2 uv_max;

    uv_min.x = std::min({ uv[0].x, uv[1].x, uv[2].x });
    uv_min.y = std::min({ uv[0].y, uv[1].y, uv[2].y });

    uv_max.x = std::max({ uv[0].x, uv[1].x, uv[2].x });
    uv_max.y = std::max({ uv[0].y, uv[1].y, uv[2].y });

    const float inv_w = 1.f / (bounds.max.x - bounds.min.x);
    const float inv_h = 1.f / (bounds.max.y - bounds.min.y);

    uv_min_n.x = (uv_min.x - bounds.min.x) * inv_w;
    uv_min_n.y = (uv_min.y - bounds.min.y) * inv_h;

    uv_max_n.x = (uv_max.x - bounds.min.x) * inv_w;
    uv_max_n.y = (uv_max.y - bounds.min.y) * inv_h;
}

bool UVtri::overlapsCell(u32 cx, u32 cy) const
{
    constexpr float CELL_W = 1.0f / float(UV_GRID_X);
    constexpr float CELL_H = 1.0f / float(UV_GRID_Y);

    const float cell_min_x = float(cx) * CELL_W;
    const float cell_max_x = cell_min_x + CELL_W;
    const float cell_min_y = float(cy) * CELL_H;
    const float cell_max_y = cell_min_y + CELL_H;

    // AABB vs AABB
    if (uv_max_n.x < cell_min_x) return false;
    if (uv_min_n.x > cell_max_x) return false;
    if (uv_max_n.y < cell_min_y) return false;
    if (uv_min_n.y > cell_max_y) return false;

    return true;
}