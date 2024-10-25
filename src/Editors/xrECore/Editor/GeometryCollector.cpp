//---------------------------------------------------------------------------
#include "stdafx.h"
#pragma hdrstop

#include "GeometryCollector.h"
//---------------------------------------------------------------------------


//------------------------------------------------------------------------------
// VCPacked
//------------------------------------------------------------------------------
VCPacked::VCPacked(const Fbox& bb, float _eps, u32 _sx, u32 _sy, u32 _sz, int apx_vertices)
{
    eps = _eps;
    sx  = _max(_sx, 1);
    sy  = _max(_sy, 1);
    sz  = _max(_sz, 1);
    // prepare hash table
    VM.resize(sx * sy * sz);

    // Params
    VMscale.set(bb.max.x - bb.min.x, bb.max.y - bb.min.y, bb.max.z - bb.min.z);
    VMmin.set(bb.min);
    VMeps.set(VMscale.x / (sx - 1) / 2, VMscale.y / (sy - 1) / 2, VMscale.z / (sz - 1) / 2);
    VMeps.x = (VMeps.x < EPS_L) ? VMeps.x : EPS_L;
    VMeps.y = (VMeps.y < EPS_L) ? VMeps.y : EPS_L;
    VMeps.z = (VMeps.z < EPS_L) ? VMeps.z : EPS_L;

    // Preallocate memory
    verts.reserve(apx_vertices);

    int _size    = VM.size();
    int _average = (apx_vertices / _size) / 2;
    for (GCHashIt it = VM.begin(); it != VM.end(); it++)
        it->reserve(_average);
}

u32 VCPacked::add_vert(const Fvector& V)
{
    u32 P    = 0xffffffff;

    u32 clpX = sx - 1, clpY = sy - 1, clpZ = sz - 1;
    u32 ix, iy, iz;

    ix = iFloor(float(V.x - VMmin.x) / VMscale.x * clpX);
    iy = iFloor(float(V.y - VMmin.y) / VMscale.y * clpY);
    iz = iFloor(float(V.z - VMmin.z) / VMscale.z * clpZ);

    clamp(ix, (u32)0, clpX);
    clamp(iy, (u32)0, clpY);
    clamp(iz, (u32)0, clpZ);

    U32Vec& vl = get_element(ix, iy, iz);
    for (U32It it = vl.begin(); it != vl.end(); it++)
        if (verts[*it].similar(V, eps))
        {
            P = *it;
            verts[*it].refs++;
            break;
        }

    if (0xffffffff == P)
    {
        P = verts.size();
        verts.push_back(GCVertex(V));

        get_element(ix, iy, iz).push_back(P);

        u32 ixE, iyE, izE;
        ixE = iFloor(float(V.x + VMeps.x - VMmin.x) / VMscale.x * clpX);
        iyE = iFloor(float(V.y + VMeps.y - VMmin.y) / VMscale.y * clpY);
        izE = iFloor(float(V.z + VMeps.z - VMmin.z) / VMscale.z * clpZ);

        // R_ASSERT(ixE<=clpMX && iyE<=clpMY && izE<=clpMZ);
        clamp(ixE, (u32)0, clpX);
        clamp(iyE, (u32)0, clpY);
        clamp(izE, (u32)0, clpZ);

        if (ixE != ix)
            get_element(ixE, iy, iz).push_back(P);
        if (iyE != iy)
            get_element(ix, iyE, iz).push_back(P);
        if (izE != iz)
            get_element(ix, iy, izE).push_back(P);
        if ((ixE != ix) && (iyE != iy))
            get_element(ixE, iyE, iz).push_back(P);
        if ((ixE != ix) && (izE != iz))
            get_element(ixE, iy, izE).push_back(P);
        if ((iyE != iy) && (izE != iz))
            get_element(ix, iyE, izE).push_back(P);
        if ((ixE != ix) && (iyE != iy) && (izE != iz))
            get_element(ixE, iyE, izE).push_back(P);
    }
    return P;
}

void VCPacked::clear()
{
    verts.clear();
    for (GCHashIt it = VM.begin(); it != VM.end(); it++)
        it->clear();
}