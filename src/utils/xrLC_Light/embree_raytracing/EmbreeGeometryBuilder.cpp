#include "stdafx.h"
#include "EmbreeGeometryBuilder.h"
#include "../../../xrCore/Collision/xrCDB.h"
#include "xrFace.h"

#include <execution>
#include <array>

void TriangleContainer::RemoveDublicates()
{
    size_t VertexStart = verts_v.size();

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
        if (!temp[i].v.similar(temp[i - 1].v, EPS_L))
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
    verts_v.swap(unique_vertices);

    faces_v.clear();                         dummy.clear();
    faces_v.reserve(raw_faces.size());       dummy.reserve(raw_faces.size());

    for (size_t i = 0; i < raw_faces.size(); ++i)
    {
        Triangle tri;
        tri.point1 = remap[i * 3 + 0];
        tri.point2 = remap[i * 3 + 1];
        tri.point3 = remap[i * 3 + 2];
        faces_v.push_back(tri);
        dummy.push_back(raw_faces[i].F);
    }

    //----------------------
    // 5. Чистим временные данные
    //----------------------
    raw_faces.clear();
    raw_faces.shrink_to_fit();

    clMsg("$ Remove Dublicates: %u ms | Vertex Now: CAP: %u| SIZE: %u | Vertex Pre : %u",
        tStats.GetElapsed_ms(),
        verts_v.capacity(), verts_v.size(),
        VertexStart);
}


void TriangleContainer::RemoveDublicatesFaces()
{
    /*
    
    // 6. Убираем дубликаты треугольников
    struct TriKey
    {
        std::array<uint32_t, 3> idx;

        TriKey(uint32_t a, uint32_t b, uint32_t c) {
            idx = { a, b, c };
            std::sort(idx.begin(), idx.end()); // нормализация порядка
        }

        bool operator==(const TriKey& other) const {
            return idx == other.idx;
        }
    };

    struct TriHash {
        size_t operator()(const TriKey& t) const {
            return std::hash<uint32_t>()(t.idx[0]) ^
                (std::hash<uint32_t>()(t.idx[1]) << 1) ^
                (std::hash<uint32_t>()(t.idx[2]) << 2);
        }
    };

    std::unordered_map<TriKey, size_t, TriHash> seen;
    xr_vector<TriEmbree> new_faces;
    xr_vector<Face*> new_dummy;

    new_faces.reserve(faces_v.size());
    new_dummy.reserve(dummy.size());

    for (size_t i = 0; i < faces_v.size(); ++i)
    {
        TriEmbree& tri = faces_v[i];
        TriKey key(tri.point1, tri.point2, tri.point3);

        if (seen.find(key) == seen.end()) {
            seen[key] = i;
            new_faces.push_back(tri);
            new_dummy.push_back(dummy[i]);
        }
    }
    
    u32 pFaces = faces_v.size();

    faces_v.swap(new_faces);
    dummy.swap(new_dummy);

    clMsg("$ Triangles : Compacted From %u to %u", pFaces, faces_v.size());
    */

    CTimer t; 
    t.Start();

    // 1. Убираем дубликаты треугольников через сортировку
    xr_vector<IndexedTri> temp;
    temp.reserve(faces_v.size());

    for (size_t i = 0; i < faces_v.size(); ++i)
    {
        temp.emplace_back(faces_v[i], static_cast<uint32_t>(i));
    }

    std::sort(std::execution::par, temp.begin(), temp.end());

    // создаём новые массивы
    xr_vector<Triangle> new_faces;
    xr_vector<decltype(dummy)::value_type> new_dummy;
    new_faces.reserve(faces_v.size());
    new_dummy.reserve(dummy.size());

    // первый всегда берём
    new_faces.push_back(faces_v[temp[0].originalIndex]);
    new_dummy.push_back(dummy[temp[0].originalIndex]);

    for (size_t i = 1; i < temp.size(); ++i)
    {
        if (!temp[i].similar(temp[i - 1]))
        {
            new_faces.push_back(faces_v[temp[i].originalIndex]);
            new_dummy.push_back(dummy[temp[i].originalIndex]);
        }
    }
 
    u32 pFaces = faces_v.size();

    new_faces.shrink_to_fit();
    new_dummy.shrink_to_fit();

    // меняем местами
    faces_v.swap(new_faces);
    dummy.swap(new_dummy);


    clMsg("$ Triangles : Compacted From %u to (CAP: %u | SIZE: %u) | %u ms", pFaces, faces_v.capacity(), faces_v.size(), t.GetElapsed_ms());
}


size_t TriangleContainer::AddVertex(Fvector& V)
{
    verts_v.push_back(V);
    return verts_v.size();
}

void TriangleContainer::AddFace(void* F, Fvector& v1, Fvector& v2, Fvector& v3)
{
    Triangle triangle;
    triangle.point1 = AddVertex(v1);
    triangle.point2 = AddVertex(v2);
    triangle.point3 = AddVertex(v3);
    faces().push_back(triangle);
    dummy.push_back((Face*)F);

    AddFaceRaw( (Face*) F, v1, v2, v3);
}
   
void TriangleContainer::ClearAll()
{
    dummy.clear();
    faces_v.clear();
    verts_v.clear();

    faces_v.shrink_to_fit();
    verts_v.shrink_to_fit();
    dummy.shrink_to_fit();
}

