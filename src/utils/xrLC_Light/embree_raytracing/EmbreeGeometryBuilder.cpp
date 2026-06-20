#include "stdafx.h"
#include "EmbreeGeometryBuilder.h"
#include "../../../xrCore/Collision/xrCDB.h"
 
#include <execution>
#include <array>
 
void TriangleContainer::RemoveDublicatesVertexs( )
{
    CTimer tStats;
    tStats.Start();
    //----------------------
    // 1. Собираем все вершины
    //----------------------
    size_t totalVerts = raw_faces.size() * 3;
 
    if (raw_faces.empty())   return;

    xr_vector<IndexedVertex> temp;
    temp.reserve(totalVerts);

    for (size_t i = 0; i < raw_faces.size(); ++i)
    {
        for (int j = 0; j < 3; ++j)
        {
            temp.push_back({ raw_faces[i].v[j], static_cast<uint32_t>(i * 3 + j) });
        }
    }
    size_t VertexStart = temp.size();

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

        auto Face = raw_faces[i].Face;
        dummy.push_back(Face);
    }

    //----------------------
    // 5. Чистим временные данные
    //----------------------
    raw_faces.clear();
    raw_faces.shrink_to_fit();

    if (useMsg)
        Msg("$ GeometryBuffer Remove Dublicate Vertex : from %u to %u", VertexStart, verts_v.size());
}
 
void TriangleContainer::RemoveDublicatesFaces( )
{
    if (faces_v.empty())        return;

    CTimer t; t.Start();

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
		if (!temp[i].similar(temp[i - 1]) && !temp[i].isDegenerated())
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
 
    if (useMsg)
        Msg("$ GeometryBuffer Remove Dublicate Triangles : from %u to %u", pFaces, faces_v.size());
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


void TriangleContainer::ClearFaces()
{
    faces_v.clear();
    verts_v.clear();
    faces_v.shrink_to_fit();
    verts_v.shrink_to_fit();
}

