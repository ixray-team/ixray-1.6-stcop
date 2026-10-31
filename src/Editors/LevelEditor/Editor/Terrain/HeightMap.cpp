#include "stdafx.h"
#include "HeightMap.h"

using XRay::Editor::HeightmapUtils::SHeightMap;
using XRay::Editor::HeightmapUtils::SHeightMapRenderData;

void SHeightMapRenderData::InvalidateGpu()
{
	for (SHeightMapBlock& Block : Blocks)
	{
		RImplementation.Geometry.Free(Block.Alloc);
	}
	Blocks.clear();
}

void SHeightMap::BuildGpu()
{
	RenderData.InvalidateGpu();

	constexpr u32 BlockVertices = 1u << 20;
	xr_vector<SHeightMapChunk>& Chunks = RenderData.Chunks;
	xr_vector<PackedVertex> Records;
	for (size_t First = 0, Last = 0; First < Chunks.size(); First = Last)
	{
		u32 Count = 0;
		for (; Last < Chunks.size() && Count + Chunks[Last].Vertices.size() <= BlockVertices; ++Last)
		{
			Count += u32(Chunks[Last].Vertices.size());
		}

		SHeightMapBlock& Block = RenderData.Blocks.emplace_back();
		Block.Alloc = RImplementation.Geometry.Alloc(Count, 0);
		Block.Geom.create(EGeomSource::Static, Block.Alloc.Page);
		for (u32 Offset = 0; First < Last; ++First)
		{
			SHeightMapChunk& Chunk = Chunks[First];
			const u32 Size = u32(Chunk.Vertices.size());
			Records.resize(Size);
			for (u32 Index = 0; Index < Size; ++Index)
			{
				Records[Index] = { Chunk.Vertices[Index], 0, 0, 0, Chunk.Colors[Index], std::bit_cast<u32>(1.0f) };
			}
			RImplementation.Geometry.Upload(Block.Alloc, Offset, Records.data(), Size);
			Chunk.Block = u32(RenderData.Blocks.size() - 1);
			Chunk.Base = Block.Alloc.VertexBase + Offset;
			Offset += Size;
		}
	}
}

void SHeightMap::Create(u32 w, u32 h, float fill)
{
	if (Data) xr_free(Data);
	Width = w;
	Height = h;
	Data = (float*)xr_malloc(u32(w * h) * sizeof(float));
	for (u32 i = 0; i < w * h; ++i)
		Data[i] = fill;
	MinH = MaxH = fill;
	PrecacheRenderData(50.f, 1.f, 0xffffff, true);
}

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

bool SHeightMap::SaveSteam(IWriter* Writer)
{
	if (!Writer || !Data || Width == 0 || Height == 0)
		return false;

	u16* raw_data = (u16*)xr_malloc(Width * Height * sizeof(u16));

	for (u32 i = 0; i < Width * Height; ++i)
	{
		raw_data[i] = (u16)(Data[i] * 65535.f);
	}

	Writer->w(raw_data, Width * Height * sizeof(u16));
	xr_free(raw_data);

	return true;
}

bool SHeightMap::LoadSteam(IReader* Reader)
{
	if (!Reader)
		return false;

	Width = Height = (u32)sqrt(Reader->length() / sizeof(u16));
	const size_t Offset = Width * Height * sizeof(u16);
	if (Offset > Reader->length())
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

	PrecacheRenderData(50, 1.f, 0xffffff, true);

	return true;
}

void SHeightMap::PrecacheRenderData(float scaleY, float cellSize, u32 baseColor, bool geometryOnly)
{
	if (!RenderData.IsDirty)
		return;

	auto Height2Color = [&](float h, float minH, float maxH) -> u32
	{
		float t = std::clamp((h - minH) / std::max(maxH - minH, EPS_S), 0.0f, 0.9f);
		t = pow(t, 0.7f);
		float brightness = 0.2f + 0.8f * t;

		return color_rgba(
			u8(((baseColor >> 16) & 0xFF) * brightness),
			u8(((baseColor >> 8) & 0xFF) * brightness),
			u8((baseColor & 0xFF) * brightness),
			255
		);
	};

	const u32 ChunkSize = SHeightMapRenderData::CHUNK_SIZE;
	const float FlatThreshold = SHeightMapRenderData::FLAT_THRESHOLD;

	float centerX = (Width * cellSize * Size.x) * 0.5f;
	float centerZ = (Height * cellSize * Size.z) * 0.5f;

	u32 scaledWidth = u32(Width * Size.x);
	u32 scaledHeight = u32(Height * Size.z);

	if (geometryOnly)
	{
		RenderData.Clear();
		u32 chunkCountX = (scaledWidth - 1) / ChunkSize + 1;
		u32 chunkCountZ = (scaledHeight - 1) / ChunkSize + 1;

		struct ChunkInfo
		{
			float minH = FLT_MAX, maxH = -FLT_MAX;
			bool isFlat = false, hasHoles = false;
		};

		xr_vector<ChunkInfo> chunkInfos(chunkCountX * chunkCountZ);
		RenderData.Chunks.reserve(chunkCountX * chunkCountZ);

		float globalMinH = FLT_MAX;
		float globalMaxH = -FLT_MAX;

		// Сканируем чанк-сетку и собираем базовую информацию
		for (u32 cz = 0; cz < chunkCountZ; ++cz)
		{
			for (u32 cx = 0; cx < chunkCountX; ++cx)
			{
				auto& info = chunkInfos[cz * chunkCountX + cx];
				for (u32 z = cz * ChunkSize; z <= std::min((cz + 1) * ChunkSize, scaledHeight - 1); ++z)
				{
					for (u32 x = cx * ChunkSize; x <= std::min((cx + 1) * ChunkSize, scaledWidth - 1); ++x)
					{
						u32 ix = u32((Width - 1 - (x - Pos.x)) / Size.x);
						u32 iz = u32((z - Pos.z) / Size.z);

						if (ix >= Width || iz >= Height)
						{
							info.hasHoles = true;
							continue;
						}

						float h = (GetHeight(ix, iz) + Pos.y) * scaleY * Size.y;
						if (h == 0.0f)
							info.hasHoles = true;

						info.minH = std::min(info.minH, h);
						info.maxH = std::max(info.maxH, h);
						globalMinH = std::min(globalMinH, h);
						globalMaxH = std::max(globalMaxH, h);
					}
				}
				info.isFlat = (info.maxH - info.minH < FlatThreshold) && !info.hasHoles;
			}
		}

		// Стабильный вертикальный центр — середина номинального диапазона высот [0,1],
		// а не текущий min/max, иначе при подъёме горки вся карта "опускается".
		float CenterY = 0.5f * scaleY * Size.y;

		// Генерация чанков
		for (u32 cz = 0; cz < chunkCountZ; ++cz)
		{
			for (u32 cx = 0; cx < chunkCountX; ++cx)
			{
				const ChunkInfo& info = chunkInfos[cz * chunkCountX + cx];
				bool useFlat = info.isFlat;

				// Проверка соседей
				for (int dz = -1; dz <= 1 && useFlat; ++dz)
				{
					for (int dx = -1; dx <= 1 && useFlat; ++dx)
					{
						if (!dx && !dz) continue;
						int nx = int(cx) + dx, nz = int(cz) + dz;
						if (nx >= 0 && nz >= 0 && nx < int(chunkCountX) && nz < int(chunkCountZ))
							useFlat &= chunkInfos[nz * chunkCountX + nx].isFlat;
					}
				}

				SHeightMapChunk chunk;
				chunk.BBox.invalidate();
				chunk.IsFlat = useFlat;
				chunk.Vertices.reserve(ChunkSize * ChunkSize * 6);
				chunk.Colors.reserve(ChunkSize * ChunkSize * 6);

				for (u32 z = cz * ChunkSize; z < std::min((cz + 1) * ChunkSize, scaledHeight - 1); ++z)
				{
					for (u32 x = cx * ChunkSize; x < std::min((cx + 1) * ChunkSize, scaledWidth - 1); ++x)
					{
						u32 ix0 = u32((Width - 1 - (x - Pos.x)) / Size.x);
						u32 iz0 = u32((z - Pos.z) / Size.z);
						u32 ix1 = u32((Width - 1 - ((x + 1) - Pos.x)) / Size.x);
						u32 iz1 = u32(((z + 1) - Pos.z) / Size.z);

						if (ix0 >= Width || iz0 >= Height || ix1 >= Width || iz1 >= Height)
							continue;

						float h0 = GetHeight(ix0, iz0) * scaleY * Size.y;
						float h1 = GetHeight(ix1, iz0) * scaleY * Size.y;
						float h2 = GetHeight(ix1, iz1) * scaleY * Size.y;
						float h3 = GetHeight(ix0, iz1) * scaleY * Size.y;

						if (h0 == 0.0f || h1 == 0.0f || h2 == 0.0f || h3 == 0.0f)
							continue;

						Fvector v0 = { x * cellSize - centerX, h0 - CenterY + Pos.y, z * cellSize - centerZ };
						Fvector v1 = { (x + 1) * cellSize - centerX, h1 - CenterY, z * cellSize - centerZ };
						Fvector v2 = { (x + 1) * cellSize - centerX, h2 - CenterY, (z + 1) * cellSize - centerZ };
						Fvector v3 = { x * cellSize - centerX, h3 - CenterY, (z + 1) * cellSize - centerZ };

					chunk.Vertices.insert(chunk.Vertices.end(), { v0, v1, v2, v0, v2, v3 });
					chunk.BBox.modify(v0);
					chunk.BBox.modify(v2);

					float hAvg = (h0 + h1 + h2 + h3) * 0.25f;
					u32 col = Height2Color(hAvg, globalMinH, globalMaxH);
					chunk.Colors.insert(chunk.Colors.end(), 6, col);
					}
				}

				chunk.IsValid = !chunk.Vertices.empty();
				if (chunk.IsValid)
					RenderData.Chunks.push_back(std::move(chunk));
			}
		}
	}
	else
	{
		// Обновление существующих данных
		bool Changed = RenderData.Blocks.empty();
		float globalMinH = FLT_MAX;
		float globalMaxH = -FLT_MAX;
		for (u32 z = 0; z < Height; ++z)
		{
			for (u32 x = 0; x < Width; ++x)
			{
				float h = GetHeight(x, z);
				if (h == 0.0f)
					continue;

				h *= scaleY * Size.y;
				globalMinH = std::min(globalMinH, h);
				globalMaxH = std::max(globalMaxH, h);
			}
		}

		// Стабильный вертикальный центр (середина диапазона [0,1]), без рецентровки по текущему min/max
		float CenterY = 0.5f * scaleY * Size.y;

		for (auto& chunk : RenderData.Chunks)
		{
			chunk.BBox.invalidate();
			for (size_t i = 0; i < chunk.Vertices.size(); ++i)
			{
				Fvector& v = chunk.Vertices[i];
				float ox = (v.x + centerX) / cellSize;
				float oz = (v.z + centerZ) / cellSize;

				u32 ix = u32((Width - 1 - (ox - Pos.x)) / Size.x);
				u32 iz = u32((oz - Pos.z) / Size.z);

				float h = GetHeight(ix, iz) * scaleY * Size.y;
				const float Y = h - CenterY + Pos.y;
				const u32 Color = Height2Color(h, globalMinH, globalMaxH);
				Changed |= v.y != Y || chunk.Colors[i] != Color;
				v.y = Y;
				chunk.BBox.modify(v);
				chunk.Colors[i] = Color;
			}
		}

		if (!Changed)
		{
			RenderData.IsDirty = false;
			return;
		}
	}

	BuildGpu();
	RenderData.IsDirty = false;
}

void SHeightMap::Draw(float scaleY, float cellSize)
{
	PrecacheRenderData(scaleY, cellSize, 0xffffff, false);

	if (RenderData.Blocks.empty())
		return;

	EDevice->SetShader(GEditorDraw.TerrainShader);
	RImplementation.Imm().SetCullMode(ERHI_CULLMODE::NONE);

	CFrustum& frustum = ::Render->ViewBase;

	for (const SHeightMapChunk& chunk : RenderData.Chunks)
	{
		float aabb[6] =
		{
			chunk.BBox.min.x, chunk.BBox.min.y, chunk.BBox.min.z,
			chunk.BBox.max.x, chunk.BBox.max.y, chunk.BBox.max.z
		};

		u32 mask = 0xFF;
		if (frustum.testAABB(aabb, mask) == fcvNone)
			continue;

		EDevice->DP(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, RenderData.Blocks[chunk.Block].Geom, chunk.Base, u32(chunk.Vertices.size()) / 3);
	}
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