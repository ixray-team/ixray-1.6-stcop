#pragma once
// ================== НАСТРОЙКИ ==================

constexpr u32 UV_GRID_X = 64;
constexpr u32 UV_GRID_Y = 64;
constexpr u32 UV_GRID_SIZE = UV_GRID_X * UV_GRID_Y;

// ================== ЯЧЕЙКА ==================

template <class TRI>
struct UVGridCell
{
    xr_vector<TRI*> tris;
    bool built = false;
};

// ================== GRID ==================

template <class TRI>
class UVGridLazy
{
public:
    UVGridLazy() = default;

    // Сброс между объектами / лайтмапами
    inline void reset()
    {
        if (!used_any) return;

        for (u32 i = 0; i < UV_GRID_SIZE; ++i)
        {
            if (cells[i].built)
            {
                cells[i].tris.clear();
                cells[i].built = false;
            }
        }
        used_any = false;
    }

    // Получить список треугольников для UV точки [0..1]
    inline xr_vector<TRI*>& query(
        float u, float v,
        const xr_vector<TRI>& all_tris
    )
    {
        const u32 idx = index(u, v);
        UVGridCell<TRI>& c = cells[idx];

        if (!c.built)
            build_cell(idx, all_tris, c);

        return c.tris;
    }

private:
    // ---------------- internals ----------------

    inline u32 index(float u, float v) const
    {
        // предполагается, что u,v уже в [0..1]
        u32 ix = u32(u * float(UV_GRID_X));
        u32 iy = u32(v * float(UV_GRID_Y));

        if (ix >= UV_GRID_X) ix = UV_GRID_X - 1;
        if (iy >= UV_GRID_Y) iy = UV_GRID_Y - 1;

        return iy * UV_GRID_X + ix;
    }

    inline void build_cell(
        u32 idx,
        const xr_vector<TRI>& all_tris,
        UVGridCell<TRI>& c
    )
    {
        c.built = true;
        used_any = true;

        // типичное число треугольников на ячейку
        c.tris.reserve(8);

        const u32 cx = idx % UV_GRID_X;
        const u32 cy = idx / UV_GRID_X;

        for (auto& T : all_tris)
        {
            // 🔥 ultra-fast broad phase
            if (T.overlapsCell(cx, cy))
                c.tris.push_back(const_cast<TRI*>(&T));
        }
    }

private:
    UVGridCell<TRI> cells[UV_GRID_SIZE];
    bool            used_any = false;
};