//---------------------------------------------------------------------------
#include "stdafx.h"


#include "GeometryCollector.h"
//---------------------------------------------------------------------------


//------------------------------------------------------------------------------
// VCPacked
//------------------------------------------------------------------------------
VCPacked::VCPacked(const Fbox& Bb, float PackEps, u32 ClpSX, u32 ClpSY, u32 ClpSZ, int ApxVertices)
{
    Eps = PackEps;
    Sx  = std::max(ClpSX, 1u);
    Sy  = std::max(ClpSY, 1u);
    Sz  = std::max(ClpSZ, 1u);
    VM.resize(Sx * Sy * Sz);

    VMscale.set(Bb.max.x - Bb.min.x, Bb.max.y - Bb.min.y, Bb.max.z - Bb.min.z);
    VMmin.set(Bb.min);
    VMeps.set(VMscale.x / (Sx - 1) / 2, VMscale.y / (Sy - 1) / 2, VMscale.z / (Sz - 1) / 2);
    VMeps.x = (VMeps.x < EPS_L) ? VMeps.x : EPS_L;
    VMeps.y = (VMeps.y < EPS_L) ? VMeps.y : EPS_L;
    VMeps.z = (VMeps.z < EPS_L) ? VMeps.z : EPS_L;

    verts.reserve(ApxVertices);

    const int Average = (ApxVertices / (int)VM.size()) / 2;
    for (U32Vec& Cell : VM)
        Cell.reserve(Average);
}

u32 VCPacked::AddVert(const Fvector& V)
{
    u32 P    = 0xffffffff;

    u32 ClpX = Sx - 1, ClpY = Sy - 1, ClpZ = Sz - 1;
    u32 Ix = iFloor(float(V.x - VMmin.x) / VMscale.x * ClpX);
    u32 Iy = iFloor(float(V.y - VMmin.y) / VMscale.y * ClpY);
    u32 Iz = iFloor(float(V.z - VMmin.z) / VMscale.z * ClpZ);

    clamp(Ix, (u32)0, ClpX);
    clamp(Iy, (u32)0, ClpY);
    clamp(Iz, (u32)0, ClpZ);

    U32Vec& Cell = GetElement(Ix, Iy, Iz);
    for (u32 Idx : Cell)
        if (verts[Idx].similar(V, Eps))
        {
            P = Idx;
            verts[Idx].refs++;
            break;
        }

    if (0xffffffff == P)
    {
        P = verts.size();
        verts.push_back(GCVertex(V));

        GetElement(Ix, Iy, Iz).push_back(P);

        u32 IxE = iFloor(float(V.x + VMeps.x - VMmin.x) / VMscale.x * ClpX);
        u32 IyE = iFloor(float(V.y + VMeps.y - VMmin.y) / VMscale.y * ClpY);
        u32 IzE = iFloor(float(V.z + VMeps.z - VMmin.z) / VMscale.z * ClpZ);

        clamp(IxE, (u32)0, ClpX);
        clamp(IyE, (u32)0, ClpY);
        clamp(IzE, (u32)0, ClpZ);

        if (IxE != Ix)
            GetElement(IxE, Iy, Iz).push_back(P);
        if (IyE != Iy)
            GetElement(Ix, IyE, Iz).push_back(P);
        if (IzE != Iz)
            GetElement(Ix, Iy, IzE).push_back(P);
        if ((IxE != Ix) && (IyE != Iy))
            GetElement(IxE, IyE, Iz).push_back(P);
        if ((IxE != Ix) && (IzE != Iz))
            GetElement(IxE, Iy, IzE).push_back(P);
        if ((IyE != Iy) && (IzE != Iz))
            GetElement(Ix, IyE, IzE).push_back(P);
        if ((IxE != Ix) && (IyE != Iy) && (IzE != Iz))
            GetElement(IxE, IyE, IzE).push_back(P);
    }
    return P;
}

void VCPacked::Clear()
{
    verts.clear();
    for (U32Vec& Cell : VM)
        Cell.clear();
}
