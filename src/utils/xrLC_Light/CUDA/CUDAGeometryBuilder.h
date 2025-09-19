#pragma once
#include "../../xrCore/Collision/xrCDB.h"
#include "xrFace.h"
#include "CUDARayCast.h"
#include "CUDAContext.h"
#include "../../xrLC/Build.h"
#include <execution>

struct GridKeyVertexies 
{
    int x, y, z;

    bool operator==(const GridKeyVertexies& other) const noexcept {
        return x == other.x && y == other.y && z == other.z;
    }
};

struct GridKeyHasher {
    
 

    std::size_t operator()(const GridKeyVertexies& k) const noexcept 
    {
        // Сдвигаем в положительную область
        uint64_t X = uint64_t(int64_t(k.x) + 4096);
        uint64_t Y = uint64_t(int64_t(k.y) + 4096);
        uint64_t Z = uint64_t(int64_t(k.z) + 4096);

        // Простые числа (лучше > диапазона)
        const uint64_t p1 = 73856093ull;
        const uint64_t p2 = 19349663ull;
        const uint64_t p3 = 83492791ull;

        return X * p1 ^ Y * p2 ^ Z * p3;
    }

    // std::size_t operator()(const GridKeyVertexies& k) const noexcept
    // {
    //     const uint64_t fnv_prime = 1099511628211ull;
    //     uint64_t hash = 1469598103934665603ull; // offset basis
    // 
    //     hash ^= uint64_t(k.x); hash *= fnv_prime;
    //     hash ^= uint64_t(k.y); hash *= fnv_prime;
    //     hash ^= uint64_t(k.z); hash *= fnv_prime;
    // 
    //     return size_t(hash);
    // }

};

class OptixGeometryBuilder
{
private:
    struct VertexData
    {
        Fvector Vertex;
        u32 verID;
    };

    std::unordered_map<GridKeyVertexies, xr_vector<VertexData>, GridKeyHasher>       hash_vertices;
 
public:
    xr_vector<Fvector>   vertices;
    xr_vector<CDB::TRI>  triangles;
    xr_vector<Face*>     facePointers;

    IC void Clear()
    {
        vertices.clear();
        triangles.clear();
        facePointers.clear();
        hash_vertices.clear();
    }

    void MemoryDealoc()
    {
         vertices.shrink_to_fit();
        triangles.shrink_to_fit();
        facePointers.shrink_to_fit();
    }


    u32 AddVertex(const Fvector& v)
    {
        // const float cell_size = 1.0f;
        // 
        // // Квантизация координат
        // GridKeyVertexies key(v.x, v.y, v.z);
        // 
        // // Generate hash key
        // auto itHash = hash_vertices.find(key);
        // if (itHash != hash_vertices.end())
        // {
        //     Vertex* parsed = nullptr;
        //     for (auto& vertex : itHash->second)
        //     {
        //         if (vertex.Vertex.similar(v, 0.001f))
        //             return vertex.verID; // Нашли похожую вершину
        //     }
        // }
        // 
        // vertices.push_back(v);
        // 
        // u32 VertexID = vertices.size() - 1;
        // 
        // VertexData new_vertex;
        // new_vertex.Vertex = vertices.back();
        // new_vertex.verID = VertexID;
        // hash_vertices[key].push_back(new_vertex);
        // return VertexID;

        vertices.push_back(v);
        return vertices.size() - 1;
    }

    // Face Raw
    struct FaceRaw
    {
        Fvector v[3];
        Face* F;
    };
    std::vector<FaceRaw> raw_faces;
    void AddFaceRaw(Face* F, const Fvector& v1, const Fvector& v2, const Fvector& v3)
    {
        raw_faces.push_back({ {v1, v2, v3}, F });
    };


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

        AddFaceRaw(F, v1, v2, v3);
    }

  

    struct IndexedVertex
    {
        Fvector v;
        uint32_t originalIndex;
    };

    void RemoveDublicates()
    {
        size_t VertexStart = vertices.size();

        CTimer tStats;
        tStats.Start();
        //----------------------
        // 1. Собираем все вершины
        //----------------------
        size_t totalVerts = raw_faces.size() * 3;
        xr_vector<IndexedVertex> temp;
        temp.reserve(totalVerts);

        for (size_t i = 0; i < raw_faces.size(); ++i)
        {
            for (int j = 0; j < 3; ++j)
            {
                temp.push_back({ raw_faces[i].v[j], static_cast<uint32_t>(i * 3 + j) });
            }
        }
        //----------------------
        // 2. Сортируем вершины
        //----------------------
        std::sort(std::execution::par, temp.begin(), temp.end(), [](const IndexedVertex& a, const IndexedVertex& b)
            {
                if (a.v.x != b.v.x) return a.v.x < b.v.x;
                if (a.v.y != b.v.y) return a.v.y < b.v.y;
                return a.v.z < b.v.z;
            });
        //----------------------
        // 3. Убираем дубликаты
        //----------------------
        xr_vector<uint32_t> remap(totalVerts);
        xr_vector<Fvector> unique_vertices;
        unique_vertices.reserve(totalVerts / 3);

        uint32_t newIndex = 0;
        unique_vertices.push_back(temp[0].v);
        remap[temp[0].originalIndex] = 0;

        for (size_t i = 1; i < temp.size(); ++i)
        {
            if (!temp[i].v.similar(temp[i - 1].v, 0.001f))
            {
                ++newIndex;
                unique_vertices.push_back(temp[i].v);
            }
            remap[temp[i].originalIndex] = newIndex;
        }

        // Сожмать до реального размера
        unique_vertices.shrink_to_fit();

        //----------------------
        // 4. Перестраиваем треугольники
        //----------------------
        vertices.swap(unique_vertices);

        triangles.clear();                       facePointers.clear();
        triangles.reserve(raw_faces.size());     facePointers.reserve(raw_faces.size());
 
        for (size_t i = 0; i < raw_faces.size(); ++i)
        {
            CDB::TRI tri;
            tri.verts[0] = remap[i * 3 + 0];
            tri.verts[1] = remap[i * 3 + 1];
            tri.verts[2] = remap[i * 3 + 2];
            tri.dummy = 0;
            triangles.push_back(tri);
            facePointers.push_back(raw_faces[i].F);
        }

        //----------------------
        // 5. Чистим временные данные
        //----------------------
        raw_faces.clear();
        raw_faces.shrink_to_fit();

        clMsg("$ Remove Dublicates: %u ms | Vertex Now: %u | Vertex Pre : %u", 
            tStats.GetElapsed_ms(),
            vertices.size(), 
            VertexStart);
    };

    bool BuildBLAS(OptixDeviceContext context, XRay::RayTrace::CUDA::OptixMeshBuffers& outBuffers);
    bool BuildTLAS(OptixDeviceContext context, XRay::RayTrace::CUDA::OptixMeshBuffers& outScene, CUstream stream);
};