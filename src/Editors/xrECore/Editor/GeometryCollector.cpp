//---------------------------------------------------------------------------
#include "stdafx.h"
#pragma hdrstop

#include "GeometryCollector.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

//------------------------------------------------------------------------------
// VCPacked
//------------------------------------------------------------------------------
VCPacked::VCPacked(const Fbox &bb, float _eps, u32 _sx, u32 _sy, u32 _sz, int apx_vertices)
{
            
    // Params

    // Preallocate memory
    verts.reserve	(apx_vertices);
}

u32		VCPacked::add_vert(const Fvector& V)
{
    u32 P = 0xffffffff;
    if (0xffffffff==P)
    {
        P = verts.size();
        verts.push_back(GCVertex(V));
    }
    return P;
}

void	VCPacked::clear()
{
    verts.clear();
}

//------------------------------------------------------------------------------
// GCPacked
//------------------------------------------------------------------------------
void	GCPacked::add_face(const Fvector& v0, const Fvector& v1, const Fvector& v2, u32 dummy)
{
	GCFace T;
    T.verts	[0] 	= add_vert(v0);
    T.verts	[1] 	= add_vert(v1);
    T.verts	[2] 	= add_vert(v2);
    T.dummy			= dummy;
    faces.push_back	(T);
    validate		(T);
}

void	GCPacked::clear()
{
	GCPacked::clear	();
    faces.clear();
}

void	GCPacked::calc_adjacency	(U32Vec& dest)
{
    dest.assign		(faces.size()*3,0xffffffff);
    // Dumb algorithm O(N^2) :)
    for (u32 f=0; f<faces.size(); f++)
    {
        for (u32 t=0; t<faces.size(); t++)
        {
            if (t==f)	continue;

            for (u32 f_e=0; f_e<3; f_e++)
            {
                u32 f1	= faces[f].verts[(f_e+0)%3];
                u32 f2	= faces[f].verts[(f_e+1)%3];
                if (f1>f2)	std::swap(f1,f2);

                for (u32 t_e=0; t_e<3; t_e++)
                {
                    u32 t1	= faces[t].verts[(t_e+0)%3];
                    u32 t2	= faces[t].verts[(t_e+1)%3];
                    if (t1>t2)	std::swap(t1,t2);

                    if (f1==t1 && f2==t2)
                    {
                        // f.edge[f_e] linked to t.edge[t_e]
                        dest[f*3+f_e]	= t;
                        break;
                    }
                }
            }
        }
    }
}

