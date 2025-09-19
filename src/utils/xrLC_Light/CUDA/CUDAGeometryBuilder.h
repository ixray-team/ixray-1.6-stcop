#pragma once
#include "../../xrCore/Collision/xrCDB.h"
#include "xrFace.h"
#include "CUDARayCast.h"
#include "CUDAContext.h"

class OptixGeometryBuilder
{
private:
    xr_vector<Fvector> vertices;
    xr_vector<CDB::TRI> triangles;
    xr_vector<Face*> facePointers;

public:
    IC void Clear()
    {
        vertices.clear();
        triangles.clear();
        facePointers.clear();
    }

    void AddFace(Face* F, const Fvector& v1, const Fvector& v2, const Fvector& v3)
    {
        // Добавляем вершины и получаем их индексы
        u32 idx1 = AddVertex(v1);
        u32 idx2 = AddVertex(v2);
        u32 idx3 = AddVertex(v3);

        // Создаем треугольник
        CDB::TRI tri;
        tri.verts[0] = idx1;
        tri.verts[1] = idx2;
        tri.verts[2] = idx3;
        tri.dummy = 0;

        triangles.push_back(tri);
        facePointers.push_back(F);
    }

    u32 AddVertex(const Fvector& v);

    bool BuildBLAS(OptixDeviceContext context, XRay::RayTrace::CUDA::OptixMeshBuffers& outBuffers);
};