#pragma once
#include <execution>

#include "../../xrCore/Collision/xrCDB.h"
#include "../../xrLC/Build.h"

#include "xrFace.h"
#include "CUDARayCast.h"
#include "CUDAContext.h"
 
struct OptixMeshBuffers;
 
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
        void* F;
        u8 Kind;
        void* Extra;
    };
    xr_vector<FaceRaw> raw_faces;
 

public:
    xr_vector<Fvector>        vertices;
    xr_vector<CDB::TRI>       triangles;
    xr_vector<void*>          facePointers;
    xr_vector<u8>             FaceKinds;
    xr_vector<void*>          FaceExtras;

    size_t RawFacesSize() { return raw_faces.size(); }
     
    IC void Clear()
    {
        vertices.clear();
        triangles.clear();
        facePointers.clear();
        FaceKinds.clear();
        FaceExtras.clear();
     }

    void MemoryDealoc()
    {
        vertices.shrink_to_fit();
        triangles.shrink_to_fit();
        facePointers.shrink_to_fit();
        FaceKinds.shrink_to_fit();
        FaceExtras.shrink_to_fit();
    }

    void AddFace(void* F, const Fvector& V1, const Fvector& V2, const Fvector& V3, u8 Kind, void* Extra = nullptr)
    {
        raw_faces.push_back({ {V1, V2, V3}, F, Kind, Extra });
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
            if (a.v.x != b.v.x)
            {
                return a.v.x < b.v.x;
            }
            if (a.v.y != b.v.y)
            {
                return a.v.y < b.v.y;
            }
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
        vertices.clear(); vertices.shrink_to_fit();
        vertices.swap(unique_vertices);
         
        triangles.clear();
        triangles.shrink_to_fit();
        facePointers.clear();
        FaceKinds.clear();
        FaceExtras.clear();
        triangles.reserve(raw_faces.size());
        facePointers.reserve(raw_faces.size());
        FaceKinds.reserve(raw_faces.size());
        FaceExtras.reserve(raw_faces.size());
        for (size_t i = 0; i < raw_faces.size(); ++i)
        {
            CDB::TRI tri;
            tri.verts[0] = remap[i * 3 + 0];
            tri.verts[1] = remap[i * 3 + 1];
            tri.verts[2] = remap[i * 3 + 2];
            tri.dummy = 0;
            triangles.push_back(tri);
            facePointers.push_back(raw_faces[i].F);
            FaceKinds.push_back(raw_faces[i].Kind);
            FaceExtras.push_back(raw_faces[i].Extra);
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
        if (triangles.empty())
        {
            return;
        }

        // 1. Убираем дубликаты треугольников через сортировку
        xr_vector<IndexFaces> Temp;
        Temp.reserve(triangles.size());

        for (size_t Index = 0; Index < triangles.size(); ++Index)
        {
            Temp.emplace_back(triangles[Index], static_cast<uint32_t>(Index));
        }

        std::sort(std::execution::par, Temp.begin(), Temp.end());

        xr_vector<CDB::TRI> NewFaces;
        xr_vector<void*> NewDummies;
        xr_vector<u8> NewKinds;
        xr_vector<void*> NewExtras;
        NewFaces.reserve(triangles.size());
        NewDummies.reserve(facePointers.size());
        NewKinds.reserve(FaceKinds.size());
        NewExtras.reserve(facePointers.size());

        const auto Take = [&](uint32_t Idx)
        {
            NewFaces.push_back(triangles[Idx]);
            NewDummies.push_back(facePointers[Idx]);
            if (!FaceKinds.empty())
            {
                NewKinds.push_back(FaceKinds[Idx]);
            }
            NewExtras.push_back(Idx < FaceExtras.size() ? FaceExtras[Idx] : nullptr);
        };

        Take(Temp[0].originalIndex);

        for (size_t Index = 1; Index < Temp.size(); ++Index)
        {
            if (!Temp[Index].similar(Temp[Index - 1]))
            {
                Take(Temp[Index].originalIndex);
            }
            else
            {
                const uint32_t Idx = Temp[Index].originalIndex;
                const u8 CandKind = (Idx < FaceKinds.size()) ? FaceKinds[Idx] : u8(0);
                const bool CandLM = CandKind == 1 && facePointers[Idx] && ((Face*)facePointers[Idx])->pDeflector;
                const bool KeptLM = !NewKinds.empty() && NewKinds.back() == 1
                    && NewDummies.back() && ((Face*)NewDummies.back())->pDeflector;
                if (CandLM && !KeptLM)
                {
                    NewFaces.back() = triangles[Idx];
                    NewDummies.back() = facePointers[Idx];
                    if (!NewKinds.empty())
                    {
                        NewKinds.back() = 1;
                    }
                    if (!NewExtras.empty())
                    {
                        NewExtras.back() = Idx < FaceExtras.size() ? FaceExtras[Idx] : nullptr;
                    }
                }
            }
        }

        NewFaces.shrink_to_fit();
        NewDummies.shrink_to_fit();

        triangles.clear();
        triangles.shrink_to_fit();
        triangles.swap(NewFaces);

        facePointers.clear();
        facePointers.shrink_to_fit();
        facePointers.swap(NewDummies);
        FaceKinds.clear();
        FaceKinds.shrink_to_fit();
        FaceKinds.swap(NewKinds);
        FaceExtras.clear();
        FaceExtras.shrink_to_fit();
        FaceExtras.swap(NewExtras);
    }

    bool BuildBLAS(OptixDeviceContext context, OptixMeshBuffers& outBuffers);
    bool BuildTLAS(OptixDeviceContext context, OptixMeshBuffers& outScene );

    // Initialize Model
    void InitializeModelNEW()
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
};