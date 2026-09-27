#pragma once
//---------------------------------------------------------------------------

struct ECORE_API GCVertex
{
    Fvector pos;
    u32     refs;
    GCVertex(const Fvector& P)
    {
        pos  = P;
        refs = 1;
    }
    bool similar(const GCVertex& V, float /*Eps*/ = EPS)
    {
        return pos.similar(V.pos);
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

    xr_vector<GCVertex> verts;

    GCHash              VM;
    Fvector             VMmin, VMscale;
    Fvector             VMeps;
    float               Eps;
    u32                 Sx, Sy, Sz;

    IC U32Vec& GetElement(u32 Ix, u32 Iy, u32 Iz)
    {
        VERIFY((Ix < Sx) && (Iy < Sy) && (Iz < Sz));
        return VM[Iz * Sy * Sx + Iy * Sx + Ix];
    }

public:
    VCPacked(const Fbox& Bb, float PackEps = EPS, u32 ClpSX = 24, u32 ClpSY = 16, u32 ClpSZ = 24, int ApxVertices = 5000);
    virtual ~VCPacked()
    {
        Clear();
    }
    virtual void Clear();

    u32 AddVert(const Fvector& V);

    xr_vector<GCVertex>& Vertices()
    {
        return verts;
    }
};
