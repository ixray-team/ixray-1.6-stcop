#include "stdafx.h"
#include "HeightMap.h"

using XRay::Editor::HeightmapUtils::SHeightMap;
using XRay::Editor::HeightmapUtils::SHeightMapRenderData;

bool SHeightMap::LoadRAW(const char* filename)
{
	if (!FS.TryLoad(filename))
	{
		return false;
	}

	IReader* F = FS.r_open(filename);
	LoadSteam(F);
	FS.r_close(F);
	return true;
}

bool SHeightMap::LoadSteam(IReader* Reader)
{
	if (!Reader)
		return false;

	Width = Height = (u32)sqrt(Reader->length() / sizeof(u16));
	if (Width * Height * sizeof(u16) != Reader->length())
	{
		return false;
	}

	Data = (float*)xr_malloc(Width * Height * sizeof(float));

	u16* raw_data = (u16*)Reader->pointer();
	for (u32 i = 0; i < Width * Height; ++i)
	{
		Data[i] = float(raw_data[i]) / 65535.f;
	}

	MinH = Data[0];
	MaxH = Data[0];

	for (u32 i = 1; i < Width * Height; ++i)
	{
		if (Data[i] < MinH) MinH = Data[i];
		if (Data[i] > MaxH) MaxH = Data[i];
	}

	return true;
}
void SHeightMap::PrecacheRenderData(float scaleY, float cellSize, u32 baseColor)
{
    if (!RenderData.IsDirty)
        return;

    RenderData.Clear();

    float global_min_h = FLT_MAX;
    float global_max_h = -FLT_MAX;

    const u32 ChunkSize = SHeightMapRenderData::CHUNK_SIZE;
    const float FlatThreshold = SHeightMapRenderData::FLAT_THRESHOLD;

    // Calculate center offset with scale applied
    float centerX = (Width * cellSize * Size.x) * 0.5f;
    float centerZ = (Height * cellSize * Size.z) * 0.5f;

    // Calculate scaled dimensions
    u32 scaledWidth = u32(Width * Size.x);
    u32 scaledHeight = u32(Height * Size.z);

    u32 chunkCountX = (scaledWidth - 1) / ChunkSize + 1;
    u32 chunkCountZ = (scaledHeight - 1) / ChunkSize + 1;

    struct ChunkInfo
    {
        float min_h, max_h;
        bool is_flat;
        bool has_holes;
    };

    xr_vector<ChunkInfo> ChunkInfos(chunkCountX * chunkCountZ);

    for (u32 cz = 0; cz < chunkCountZ; ++cz)
    {
        for (u32 cx = 0; cx < chunkCountX; ++cx)
        {
            float minH = FLT_MAX, maxH = -FLT_MAX;
            bool has_holes = false;

            u32 x0 = cx * ChunkSize;
            u32 z0 = cz * ChunkSize;
            u32 x_end = std::min(x0 + ChunkSize, scaledWidth - 1);
            u32 z_end = std::min(z0 + ChunkSize, scaledHeight - 1);

            for (u32 z = z0; z <= z_end; ++z)
            {
                for (u32 x = x0; x <= x_end; ++x)
                {
                    // Инвертируем ось X
                    u32 orig_x = u32((Width - 1 - (x - Pos.x)) / Size.x);
                    u32 orig_z = u32((z - Pos.z) / Size.z);

                    if (orig_x >= Width || orig_z >= Height)
                    {
                        has_holes = true;
                        continue;
                    }

                    float h = (GetHeight(orig_x, orig_z) + Pos.y) * scaleY * Size.y;
                    if (h == 0.0f)
                        has_holes = true;
                    minH = std::min(minH, h);
                    maxH = std::max(maxH, h);

                    global_min_h = std::min(global_min_h, h);
                    global_max_h = std::max(global_max_h, h);
                }
            }

            bool is_flat = (maxH - minH < FlatThreshold) && !has_holes;
            ChunkInfos[cz * chunkCountX + cx] = { minH, maxH, is_flat, has_holes };
        }
    }

    // Height to color conversion
    auto Height2Color = [&](float h) -> u32
        {
            float t = (h - global_min_h) / std::max(global_max_h - global_min_h, EPS_S);
            t = std::clamp(t, 0.0f, 0.9f); // Убрал нижний clamp 0.2f

            // Нелинейное преобразование для лучшего восприятия глубины
            t = pow(t, 0.7f); // Можно регулировать степень (0.5-0.8)

            // Коррекция яркости для темных участков
            float brightness = 0.2f + 0.8f * t; // Минимальная яркость 20%

            return color_rgba(
                u8(((baseColor >> 16) & 0xFF) * brightness),
                u8(((baseColor >> 8) & 0xFF) * brightness),
                u8((baseColor & 0xFF) * brightness),
                255
            );
        };

    for (u32 cz = 0; cz < chunkCountZ; ++cz)
    {
        for (u32 cx = 0; cx < chunkCountX; ++cx)
        {
            const ChunkInfo& info = ChunkInfos[cz * chunkCountX + cx];

            // Check neighbors
            bool neighbor_flat = true;
            for (int dz = -1; dz <= 1 && neighbor_flat; ++dz)
            {
                for (int dx = -1; dx <= 1 && neighbor_flat; ++dx)
                {
                    if (dx == 0 && dz == 0)
                        continue;

                    int nx = int(cx) + dx;
                    int nz = int(cz) + dz;

                    if (nx >= 0 && nz >= 0 && nx < int(chunkCountX) && nz < int(chunkCountZ))
                    {
                        const ChunkInfo& neighbor = ChunkInfos[nz * chunkCountX + nx];
                        if (!neighbor.is_flat)
                            neighbor_flat = false;
                    }
                }
            }

            bool use_flat = info.is_flat && neighbor_flat;

            SHeightMapChunk chunk;
            chunk.BBox.invalidate();

            u32 x0 = cx * ChunkSize;
            u32 z0 = cz * ChunkSize;
            u32 x_end = std::min(x0 + ChunkSize, scaledWidth - 1);
            u32 z_end = std::min(z0 + ChunkSize, scaledHeight - 1);

            for (u32 z = z0; z < z_end; ++z)
            {
                for (u32 x = x0; x < x_end; ++x)
                {
                    // Инвертируем ось X при получении оригинальных координат
                    u32 orig_x = u32((Width - 1 - (x - Pos.x)) / Size.x);  // Инвертируем ось X
                    u32 orig_z = u32((z - Pos.z) / Size.z);
                    u32 orig_x1 = u32((Width - 1 - ((x + 1) - Pos.x)) / Size.x);  // Инвертируем ось X
                    u32 orig_z1 = u32(((z + 1) - Pos.z) / Size.z);

                    // Проверяем границы
                    if (orig_x >= Width || orig_z >= Height ||
                        orig_x1 >= Width || orig_z1 >= Height)
                        continue;

                    // Применяем масштаб и оффсет по Y
                    float h0 = (GetHeight(orig_x, orig_z) + Pos.y) * scaleY * Size.y;
                    float h1 = (GetHeight(orig_x1, orig_z) + Pos.y) * scaleY * Size.y;
                    float h2 = (GetHeight(orig_x1, orig_z1) + Pos.y) * scaleY * Size.y;
                    float h3 = (GetHeight(orig_x, orig_z1) + Pos.y) * scaleY * Size.y;

                    bool same_height = std::abs(h0 - h1) < FlatThreshold &&
                        std::abs(h1 - h2) < FlatThreshold &&
                        std::abs(h2 - h3) < FlatThreshold;

                    if (same_height)
                    {
                        if (h0 == 0.0f)
                            continue;

                        // Применяем масштаб и центрирование по XZ
                        Fvector v0 = { x * cellSize - centerX, h0, z * cellSize - centerZ };
                        Fvector v1 = { (x + 1) * cellSize - centerX, h1, z * cellSize - centerZ };
                        Fvector v2 = { (x + 1) * cellSize - centerX, h2, (z + 1) * cellSize - centerZ };
                        Fvector v3 = { x * cellSize - centerX, h3, (z + 1) * cellSize - centerZ };

                        chunk.BBox.modify(v0);
                        chunk.BBox.modify(v2);

                        chunk.Vertices.push_back(v0);
                        chunk.Vertices.push_back(v1);
                        chunk.Vertices.push_back(v2);
                        chunk.Vertices.push_back(v0);
                        chunk.Vertices.push_back(v2);
                        chunk.Vertices.push_back(v3);

                        float h_avg = (h0 + h1 + h2 + h3) * 0.25f;
                        u32 col = Height2Color(h_avg);
                        chunk.Colors.insert(chunk.Colors.end(), 6, col);
                    }
                    else
                    {
                        // Применяем масштаб и центрирование по XZ
                        Fvector v0 = { x * cellSize - centerX, h0, z * cellSize - centerZ };
                        Fvector v1 = { (x + 1) * cellSize - centerX, h1, z * cellSize - centerZ };
                        Fvector v2 = { (x + 1) * cellSize - centerX, h2, (z + 1) * cellSize - centerZ };
                        Fvector v3 = { x * cellSize - centerX, h3, (z + 1) * cellSize - centerZ };

                        if (h0 == 0.0f && h1 == 0.0f && h2 == 0.0f && h3 == 0.0f)
                            continue;

                        chunk.BBox.modify(v0);
                        chunk.BBox.modify(v2);

                        chunk.Vertices.push_back(v0);
                        chunk.Vertices.push_back(v1);
                        chunk.Vertices.push_back(v2);
                        chunk.Vertices.push_back(v0);
                        chunk.Vertices.push_back(v2);
                        chunk.Vertices.push_back(v3);

                        float h_avg = (h0 + h1 + h2 + h3) * 0.25f;
                        u32 col = Height2Color(h_avg);
                        chunk.Colors.insert(chunk.Colors.end(), 6, col);
                    }
                }
            }

            chunk.IsFlat = use_flat;
            chunk.IsValid = !chunk.Vertices.empty();

            if (chunk.IsValid)
                RenderData.Chunks.push_back(chunk);
        }
    }

    RenderData.IsDirty = false;
}


void SHeightMap::Draw(float scaleY, float cellSize, u32 baseColor)
{
	PrecacheRenderData(scaleY, cellSize, baseColor);

	if (RenderData.Chunks.empty())
		return;

	DU_impl.DD_DrawFace_begin(false);
	RCache.set_CullMode(CULL_NONE);
	EDevice->SetShader(EDevice->m_WireShader);

	CFrustum& frustum = ::Render->ViewBase;

	for (const auto& chunk : RenderData.Chunks)
	{
		float aabb[6] =
		{
			chunk.BBox.min.x, chunk.BBox.min.y, chunk.BBox.min.z,
			chunk.BBox.max.x, chunk.BBox.max.y, chunk.BBox.max.z
		};

		u32 mask = 0xFF;
		if (frustum.testAABB(aabb, mask) == fcvNone)
			continue;

		for (size_t i = 0; i < chunk.Vertices.size(); i += 3)
		{
			DU_impl.DD_DrawFace_push
			(
				chunk.Vertices[i],
				chunk.Vertices[i + 1],
				chunk.Vertices[i + 2],
				chunk.Colors[i]
			);
		}
	}

	DU_impl.DD_DrawFace_end();
}

void SHeightMap::MarkDirty()
{
	RenderData.IsDirty = true;
}

void SHeightMapRenderData::BuildFromHeightmap(const float* Heightmap, int width, int Height)
{
	Chunks.clear();

	for (int z = 0; z < Height; z += SHeightMapRenderData::CHUNK_SIZE)
	{
		for (int x = 0; x < width; x += SHeightMapRenderData::CHUNK_SIZE)
		{
			SHeightMapChunk chunk;
			chunk.BBox.invalidate();

			// Проверяем, можно ли чанк упростить (все высоты почти одинаковы)
			float min_h = FLT_MAX, max_h = -FLT_MAX;
			for (int dz = 0; dz < SHeightMapRenderData::CHUNK_SIZE && (z + dz) < Height; ++dz)
			{
				for (int dx = 0; dx < SHeightMapRenderData::CHUNK_SIZE && (x + dx) < width; ++dx)
				{
					float h = Heightmap[(z + dz) * width + (x + dx)];
					min_h = std::min(min_h, h);
					max_h = std::max(max_h, h);
					chunk.BBox.modify(Fvector(x + dx, h, z + dz));
				}
			}

			// Если разница высот маленькая — чанк "плоский", можно упростить
			chunk.IsFlat = (max_h - min_h < SHeightMapRenderData::FLAT_THRESHOLD);

			// Генерация вершин для чанка
			if (chunk.IsFlat)
			{
				// Упрощённый вариант (4 вершины = 1 quad)
				float avg_h = (min_h + max_h) * 0.5f;
				chunk.Vertices.push_back(Fvector(x, avg_h, z));
				chunk.Vertices.push_back(Fvector(x + SHeightMapRenderData::CHUNK_SIZE, avg_h, z));
				chunk.Vertices.push_back(Fvector(x, avg_h, z + SHeightMapRenderData::CHUNK_SIZE));
				chunk.Vertices.push_back(Fvector(x + SHeightMapRenderData::CHUNK_SIZE, avg_h, z + SHeightMapRenderData::CHUNK_SIZE));
			}
			else
			{
				// Полная детализация (все вершины чанка)
				for (int dz = 0; dz <= SHeightMapRenderData::CHUNK_SIZE && (z + dz) < Height; ++dz)
				{
					for (int dx = 0; dx <= SHeightMapRenderData::CHUNK_SIZE && (x + dx) < width; ++dx)
					{
						float h = Heightmap[(z + dz) * width + (x + dx)];
						chunk.Vertices.push_back(Fvector(x + dx, h, z + dz));
					}
				}
			}

			Chunks.push_back(chunk);
		}
	}
}

bool SHeightMap::RayPick(float& Distance, const Fvector& Start, const Fvector& Direction, const Fmatrix& InvParent, SRayPickInfo* PInf) const
{
	if (!Data || Width == 0 || Height == 0) return false;

	// Преобразование в локальные координаты
	Fvector LocalStart, LocalDir;
	InvParent.transform_tiny(LocalStart, Start);
	InvParent.transform_dir(LocalDir, Direction);
	LocalDir.normalize();

	// Bounding box карты высот
	Fbox HeightMapBox;

	Fvector BBoxMin = Fvector().set(
		Pos.x - Size.x * (Width - 1) * 0.5f, 
		Pos.y,                               
		Pos.z - Size.z * (Height - 1) * 0.5f 
	);

	Fvector BBoxMax = Fvector().set(
		Pos.x + Size.x * (Width - 1) * 0.5f, 
		Pos.y + Size.y * (MaxH - MinH),      
		Pos.z + Size.z * (Height - 1) * 0.5f 
	);

	HeightMapBox.min = BBoxMin;
	HeightMapBox.max = BBoxMax;

	// Быстрая проверка пересечения с AABB
	Fvector EntryPoint;
	if (HeightMapBox.Pick2(LocalStart, LocalDir, EntryPoint) == Fbox::rpNone)
		return false;

	// Параметры DDA-алгоритма
	const float CellSizeX = Size.x;
	const float CellSizeZ = Size.z;

	float X = (EntryPoint.x - BBoxMin.x) / Size.x;
	float Z = (EntryPoint.z - BBoxMin.z) / Size.z;

	const int StepX = (LocalDir.x > 0) ? 1 : -1;
	const int StepZ = (LocalDir.z > 0) ? 1 : -1;

	const float DeltaDistX = (LocalDir.x != 0) ? abs(1.0f / LocalDir.x) : FLT_MAX;
	const float DeltaDistZ = (LocalDir.z != 0) ? abs(1.0f / LocalDir.z) : FLT_MAX;

	float SideDistX = ((LocalDir.x > 0) ? (ceilf(X) - X) : (X - floorf(X))) * DeltaDistX;
	float SideDistZ = ((LocalDir.z > 0) ? (ceilf(Z) - Z) : (Z - floorf(Z))) * DeltaDistZ;

	// Основной цикл DDA
	while (X >= 0 && Z >= 0 && X < Width && Z < Height)
	{
		const float Height = GetHeight((u32)X, (u32)Z) * Size.y + Pos.y;

		const float T = (LocalDir.x != 0)
			? (X * CellSizeX + BBoxMin.x - LocalStart.x) / LocalDir.x
			: (Z * CellSizeZ + BBoxMin.z - LocalStart.z) / LocalDir.z;

		const float RayY = LocalStart.y + T * LocalDir.y;

		// Проверка пересечения с поверхностью
		if (Height > 0 && RayY <= Height + EPS && T < Distance)
		{
			Distance = T;
			if (PInf)
			{
				PInf->inf.range = T;
				PInf->pt.mad(LocalStart, LocalDir, T);
			}
			return true;
		}

		// Выбор следующей ячейки
		if (SideDistX < SideDistZ)
		{
			SideDistX += DeltaDistX;
			X += StepX;
		}
		else
		{
			SideDistZ += DeltaDistZ;
			Z += StepZ;
		}
	}

	return false;
}