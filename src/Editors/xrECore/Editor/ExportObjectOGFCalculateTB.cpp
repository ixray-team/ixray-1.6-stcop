#include "stdafx.h"
#pragma hdrstop

#include "ExportObjectOGF.h"
#include	"../../common/NvMender2003/nvMeshMender.h"
IC void set_vertex(MeshMender::Vertex& out_vertex, const SOGFVert& in_vertex);

#include	"../../common/NvMender2003/mender_input_output.h"
#include	"../../common/NvMender2003/remove_isolated_verts.h"

//--------------------------------------------------------------------------------
IC void set_vertex(MeshMender::Vertex& out_vertex, const SOGFVert& in_vertex)
{
    cv_vector(out_vertex.pos, in_vertex.P);
    cv_vector(out_vertex.normal, in_vertex.N);
    out_vertex.s = in_vertex.UV.x;
    out_vertex.t = in_vertex.UV.y;
    // out_vertex.tangent;
    // out_vertex.binormal;
}

IC void set_vertex(SOGFVert& out_vertex, const SOGFVert& in_old_vertex, const MeshMender::Vertex& in_vertex)
{
    out_vertex = in_old_vertex;

    cv_vector(out_vertex.P, in_vertex.pos);      //?
    cv_vector(out_vertex.N, in_vertex.normal);   //?

    out_vertex.UV.x = in_vertex.s;
    out_vertex.UV.y = in_vertex.t;
    Fvector tangent;
    Fvector binormal;
    out_vertex.T.set(cv_vector(tangent, in_vertex.tangent));
    out_vertex.B.set(cv_vector(binormal, in_vertex.binormal));
}

IC WORD& face_vertex(SOGFFace& F, u32 vertex_index)
{
    VERIFY(vertex_index < 3);
    return F.v[vertex_index];
}

IC const WORD& face_vertex(const SOGFFace& F, u32 vertex_index)
{
    VERIFY(vertex_index < 3);
    return F.v[vertex_index];
}

//--------------------------------------------------------------------------------------------

void CObjectOGFCollectorPacked::CalculateTB()
{
    xr_vector<MeshMender::Vertex> mender_in_out_verts;
    xr_vector<unsigned int>       mender_in_out_indices;
    xr_vector<unsigned int>       mender_mapping_out_to_in_vert;

    fill_mender_input(m_Verts, m_Faces, mender_in_out_verts, mender_in_out_indices);

    MeshMender mender;
    if (!mender.Mend(
        mender_in_out_verts, mender_in_out_indices, mender_mapping_out_to_in_vert, 1, 0.5, 0.5, 0.0f,
        MeshMender::DONT_CALCULATE_NORMALS, MeshMender::RESPECT_SPLITS, MeshMender::DONT_FIX_CYLINDRICAL))
    {
        Debug.fatal(DEBUG_INFO, "NVMeshMender failed ");
        // Debug.fatal(DEBUG_INFO,"NVMeshMender failed (%s)",mender.GetLastError().c_str());
    }

    retrive_data_from_mender_otput(
        m_Verts, m_Faces, mender_in_out_verts, mender_in_out_indices, mender_mapping_out_to_in_vert);
    // t_remove_isolated_verts(m_Verts, m_Faces);

    mender_in_out_verts.clear();
    mender_in_out_indices.clear();
    mender_mapping_out_to_in_vert.clear();

    // OptimizeTextureCoordinates();
}