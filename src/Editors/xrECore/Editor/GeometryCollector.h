#pragma once
//---------------------------------------------------------------------------

struct ECORE_API GCVertex
{
    Fvector pos;
    u32     refs;
    GCVertex(const Fvector& p)
    {
        pos  = p;
        refs = 1;
    }
    bool similar(const GCVertex& v, float eps = EPS)
    {
        return pos.similar(v.pos);
    }
};

struct ECORE_API GCFace
{
    u32  verts[3];
    bool valid;
    u32  dummy;
};

class ECORE_API VCPacked
{
protected:
    using GCHash = xr_vector<U32Vec>;
    using GCHashIt = GCHash::iterator;

    xr_vector<GCVertex> verts;

    GCHash              VM;
    Fvector             VMmin, VMscale;
    Fvector             VMeps;
    float               eps;
    u32                 sx, sy, sz;

    IC U32Vec&          get_element(u32 ix, u32 iy, u32 iz)
    {
        VERIFY((ix < sx) && (iy < sy) && (iz < sz));
        return VM[iz * sy * sx + iy * sx + ix];
    }

public:
    VCPacked(const Fbox& bb, float eps = EPS, u32 clpSX = 24, u32 clpSY = 16, u32 clpSZ = 24, int apx_vertices = 5000);
    virtual ~VCPacked()
    {
        clear();
    }
    virtual void clear();

    u32          add_vert(const Fvector& V);

    GCVertex*    getV()
    {
        return &*verts.begin();
    }
    size_t getVS()
    {
        return verts.size();
    }

    void getHASH_size(u32& x, u32& y, u32& z)
    {
        x = sx;
        y = sy;
        z = sz;
    }
    U32Vec& getHASH_elem(u32 ix, u32 iy, u32 iz)
    {
        return get_element(ix, iy, iz);
    }

    xr_vector<GCVertex>& Vertices()
    {
        return verts;
    }
};