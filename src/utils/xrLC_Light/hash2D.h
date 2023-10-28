#pragma once
#include "serialize.h"
template <class T, u32 TableSizeX, u32 TableSizeY>
class hash2D
{
    xr_vector<T>	table[TableSizeY][TableSizeX];
    Fbox2			bounds;
    Fvector2		size;
public:
    hash2D() {
        bounds.invalidate();
        size.set(0.f, 0.f);
    }

    ~hash2D() {
    }

    void initialize(Fbox2& R, u32 faces) {
        bounds.set(R);
        size.set(R.max.x - R.min.x, R.max.y - R.min.y);

        u32		size = TableSizeY * TableSizeX;
        u32		apx = faces / size;

        for (u32 y = 0; y < TableSizeY; y++)
            for (u32 x = 0; x < TableSizeX; x++)
            {
                table[y][x].clear();
                table[y][x].reserve(apx);
            }
    };
    void add(Fbox2& R, T& value) {
        int x1 = iFloor(float(TableSizeX) * (R.min.x - bounds.min.x) / size.x); clamp(x1, 0, int(TableSizeX - 1));
        int x2 = iCeil(float(TableSizeX) * (R.max.x - bounds.min.x) / size.x); clamp(x2, 0, int(TableSizeX - 1));
        int y1 = iFloor(float(TableSizeY) * (R.min.y - bounds.min.y) / size.y); clamp(y1, 0, int(TableSizeY - 1));
        int y2 = iCeil(float(TableSizeY) * (R.max.y - bounds.min.y) / size.y); clamp(y2, 0, int(TableSizeY - 1));

        for (int y = y1; y <= y2; y++)
            for (int x = x1; x <= x2; x++)
                table[y][x].push_back(value);
    };
    xr_vector<T>& query(float x, float y) {
        int _x = iFloor(float(TableSizeX) * (x - bounds.min.x) / size.x); clamp(_x, 0, int(TableSizeX - 1));
        int _y = iFloor(float(TableSizeY) * (y - bounds.min.y) / size.y); clamp(_y, 0, int(TableSizeY - 1));
        return table[_y][_x];
    };

    // vector_serialize< t_read<Face> >	
    // vector_serialize< t_write<Face> >	
    template<typename TP>
    struct get_type
    {
        typedef	TP	type;
    };

    template<typename TP>
    struct get_type<TP*>
    {
        typedef	TP	type;
    };

    typedef	 typename get_type<T>::type		type;

    void	read(INetReader& r, vector_serialize< t_read<type, get_id_self_index<type> > >& rd) {
        r_pod(r, bounds);

        r.r_fvector2(size);
        for (u32 y = 0; y < TableSizeY; y++)
            for (u32 x = 0; x < TableSizeY; x++)
                rd.read_ref(r, table[y][x]);
    }
    void	write(IWriter& w, vector_serialize< t_write<type, get_id_self_index<type> > >& wt) const {
        w_pod(w, bounds);

        w.w_fvector2(size);
        for (u32 y = 0; y < TableSizeY; y++)
            for (u32 x = 0; x < TableSizeY; x++)
                wt.write_ref(w, table[y][x]);
    }
};