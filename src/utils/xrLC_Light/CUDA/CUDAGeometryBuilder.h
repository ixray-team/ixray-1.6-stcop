#pragma once
#include <execution>

#include "../../xrCore/Collision/xrCDB.h"
#include "../../xrLC/Build.h"

#include "xrFace.h"
#include "CUDARayCast.h"
#include "CUDAContext.h"
 
struct OptixMeshBuffers;

struct GridKeyVertexies 
{
    int x, y, z;

    bool operator==(const GridKeyVertexies& other) const noexcept
    {
        return x == other.x && y == other.y && z == other.z;
    }
};

struct GridKeyHasher
{
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
    // Remove Dublicates Private:
    struct IndexFaces
    {
        uint32_t i1, i2, i3;
        uint32_t originalIndex;

        IndexFaces(const CDB::TRI& tri, uint32_t idx) : originalIndex(idx)
        {
            // нормализуем пор€док вершин (сортировка трЄх чисел)
            i1 = tri.verts[0];
            i2 = tri.verts[1];
            i3 = tri.verts[2];

            if (i1 > i2) std::swap(i1, i2);
            if (i2 > i3) std::swap(i2, i3);
            if (i1 > i2) std::swap(i1, i2);
        }

        bool operator<(const IndexFaces& other) const
        {
            if (i1 != other.i1) return i1 < other.i1;
            if (i2 != other.i2) return i2 < other.i2;
            return i3 < other.i3;
        }

        bool similar(const IndexFaces& other) const
        {
            return i1 == other.i1 && i2 == other.i2 && i3 == other.i3;
        }
    };
 
    struct IndexedVertex
    {
        Fvector v;
        uint32_t originalIndex;
    }; 
     
    struct FaceRaw
    {
        Fvector v[3];
        Face* F;
    };
    xr_vector<FaceRaw> raw_faces;
 

public:
    xr_vector<Fvector>        vertices;
    xr_vector<CDB::TRI>       triangles;
    xr_vector<Face*>          facePointers;

    size_t RawFacesSize() { return raw_faces.size(); }
     
    IC void Clear()
    {
        vertices.clear();
        triangles.clear();
        facePointers.clear();
     }

    void MemoryDealoc()
    {
        vertices.shrink_to_fit();
        triangles.shrink_to_fit();
        facePointers.shrink_to_fit();
    }

    void AddFace(Face* F, const Fvector& v1, const Fvector& v2, const Fvector& v3)
    {
        // Добавляем вершины и получаем их индексы
        raw_faces.push_back({ {v1, v2, v3}, F });
    }
    
    // Remove Dublicates
    void RemoveDublicates()
    {
        size_t totalVerts = raw_faces.size() * 3;
        xr_vector<IndexedVertex> temp;
        temp.reserve(totalVerts);

        xr_vector<uint32_t> remap(totalVerts);
        xr_vector<Fvector> unique_vertices;
        unique_vertices.reserve(totalVerts / 3);

        //----------------------
        // 1. Собираем все вершины
        //----------------------
        for (size_t i = 0; i < raw_faces.size(); ++i)
        {
            size_t IndexVertex = i * 3;
            temp.push_back({ raw_faces[i].v[0], static_cast<uint32_t>(IndexVertex + 0) });    // 1
            temp.push_back({ raw_faces[i].v[1], static_cast<uint32_t>(IndexVertex + 1) });    // 2
            temp.push_back({ raw_faces[i].v[2], static_cast<uint32_t>(IndexVertex + 2) });    // 3
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
    };

    // Remove Dublicate Faces
    void RemoveDublicateFaces()
    {
        if (triangles.empty())        return;

        // 1. Убираем дубликаты треугольников через сортировку
        xr_vector<IndexFaces> temp;
        temp.reserve(triangles.size());

        for (size_t i = 0; i < triangles.size(); ++i)
        {
            temp.emplace_back(triangles[i], static_cast<uint32_t>(i));
        }

        std::sort(std::execution::par, temp.begin(), temp.end());

        // создаём новые массивы
        xr_vector<CDB::TRI> new_faces;
        xr_vector<Face*> new_dummy;
        new_faces.reserve(triangles.size());
        new_dummy.reserve(facePointers.size());

        // первый всегда берём
        new_faces.push_back(triangles[temp[0].originalIndex]);
        new_dummy.push_back(facePointers[temp[0].originalIndex]);

        for (size_t i = 1; i < temp.size(); ++i)
        {
            if (!temp[i].similar(temp[i - 1]))
            {
                new_faces.push_back(triangles[temp[i].originalIndex]);
                new_dummy.push_back(facePointers[temp[i].originalIndex]);
            }
        }

        new_faces.shrink_to_fit();
        new_dummy.shrink_to_fit();

        // меняем местами
        triangles.swap(new_faces);
        facePointers.swap(new_dummy);
    }

    bool BuildBLAS(OptixDeviceContext context, OptixMeshBuffers& outBuffers);
    bool BuildTLAS(OptixDeviceContext context, OptixMeshBuffers& outScene, CUstream stream);

    // Initialize Model
    void InitializeModel()
    {
        vertices.clear(); 
        triangles.clear();
        facePointers.clear(); 

        for (auto& Face : raw_faces)
        {
            CDB::TRI tri;
            tri.dummy = 0;

            for (auto k = 0; k < 3; k++)
            {
                vertices.push_back(Face.v[k]);
                tri.verts[k] = vertices.size() - 1;
            }
          

            triangles.push_back(tri);
            facePointers.push_back(Face.F);
        }
        raw_faces.clear();
   
    }

    // Quantized Dedup 
    
    struct CellIndex
    {
        xr_vector<uint32_t> indices;
    };
     
    // Метод удалиления дубликатов без высокого потребления памяти
    void RemoveDublicates_Batched()
    {
        constexpr float EPS  = 0.001f;
        constexpr float CELL = 0.01f;

        vertices.clear();
        triangles.clear();
        facePointers.clear();

        vertices.reserve(raw_faces.size() / 2);
        triangles.reserve(raw_faces.size());
        facePointers.reserve(raw_faces.size());

        constexpr size_t BATCH = 250'000;

        xr_hash_map<size_t, CellIndex> grid;
        grid.reserve(1000);

        for (size_t base = 0; base < raw_faces.size(); base += BATCH)
        {
            AditionalData("Processing Dublicate Vertex: %u / %u", base, raw_faces.size());

            size_t end = std::min(base + BATCH, raw_faces.size());
            grid.clear();
             
            for (size_t i = base; i < end; ++i)
            {
                CDB::TRI tri;
                tri.dummy = 0;

                for (int k = 0; k < 3; ++k)
                {
                    const Fvector& v = raw_faces[i].v[k];
                    int xnew = int(v.x / CELL);
                    int ynew = int(v.y / CELL);
                    int znew = int(v.z / CELL);

                    size_t hash = std::hash<int>()(xnew) ^ (std::hash<int>()(ynew) << 1) ^ (std::hash<int>()(znew) << 1);

                    auto& cell = grid[hash];
                    uint32_t found = UINT32_MAX;

                    for (uint32_t idx : cell.indices)
                    {
                        if (vertices[idx].similar(v, EPS))
                        {
                            found = idx;
                            break;
                        }
                    }

                    if (found == UINT32_MAX)
                    {
                        found = (uint32_t)vertices.size();
                        vertices.push_back(v);
                        cell.indices.push_back(found);
                    }

                    tri.verts[k] = found;
                }

                triangles.push_back(tri);
                facePointers.push_back(raw_faces[i].F);
            }

            // 💣 освобождаем батч
            for (auto& [k, c] : grid)
                c.indices.clear();
        }

        raw_faces.clear();
        raw_faces.shrink_to_fit();

        vertices.shrink_to_fit();
        triangles.shrink_to_fit();
        facePointers.shrink_to_fit();
    }
};