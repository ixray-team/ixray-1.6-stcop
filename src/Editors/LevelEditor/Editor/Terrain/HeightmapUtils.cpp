#include "stdafx.h"
#include "HeightmapUtils.h"
#include <RedImage.hpp>

void XRay::Editor::HeightmapUtils::GenerateHeightmapByMesh(CEditableObject* Mesh, const xr_string& OutputFile)
{
	size_t TextureSizeX = 1024;
	size_t TextureSizeY = 1024;

	size_t MaxTextureSize = TextureSizeY > TextureSizeY ? TextureSizeY : TextureSizeX;
	RedImageTool::RedImage TestImage(TextureSizeX, TextureSizeY);
	{
		Fbox TerrainBound = Mesh->GetBox();
		Fvector TerrainSize;
		TerrainBound.getsize(TerrainSize);
		float StepX = 0, StepY = 0;

		Fvector ResultScale;
		if (TerrainSize.x > TerrainSize.z)
		{
			StepX = TerrainSize.x / static_cast<float>(MaxTextureSize);
			StepY = TerrainSize.x / static_cast<float>(MaxTextureSize);
		}
		else
		{
			StepX = TerrainSize.z / static_cast<float>(MaxTextureSize);
			StepY = TerrainSize.z / static_cast<float>(MaxTextureSize);
		}
		ResultScale.set(TerrainSize.x, TerrainSize.z, TerrainSize.y / 5.12f);
		xr_vector<uint16_t> RawData;
		RawData.resize(TextureSizeX * TextureSizeY);

		Msg("Start generate!");
		for (size_t x = 0; x < TextureSizeX; x++)
		{
			for (size_t z = 0; z < TextureSizeY; z++)
			{
				Fvector StartPoint, Direction;
				StartPoint.set(static_cast<float>(x) * StepX + TerrainBound.x1, TerrainBound.y2, static_cast<float>(z) * StepY + TerrainBound.z1);
				Direction.set(0, -1, 0);
				float dist = FLT_MAX;
				if (Mesh->RayPick(dist, StartPoint, Direction, Fidentity))
				{
					RawData[TextureSizeX * z + (TextureSizeX - x - 1)] = static_cast<u16>((1.f - clampr((dist / TerrainSize.y), 0.f, 1.f)) * 65535.f);
					TestImage.SetPixel(RedImageTool::RedColor((1.f - clampr((dist / TerrainSize.y), 0.f, 1.f)), (1.f - clampr((dist / TerrainSize.y), 0.f, 1.f)), (1.f - clampr((dist / TerrainSize.y), 0.f, 1.f))), x, z);
				}
				else
				{
					TestImage.SetPixel(RedImageTool::RedColor(0, 0, 0, 1), x, z);
					RawData[TextureSizeX * z + (TextureSizeX - x - 1)] = 0;
				}
			}
		}

		Msg("Result scale [%f,%f,%f]", ResultScale.x / static_cast<float>(TextureSizeX) * 100, ResultScale.y / static_cast<float>(TextureSizeY) * 100, ResultScale.z);
		string_path OutPNGFileName = {};
		string_path OutR16FileName = {};

		FS.update_path(OutPNGFileName, "$server_data_root$", "terrain\\");

		xr_strcat(OutPNGFileName, OutputFile.c_str());
		xr_strcat(OutR16FileName, OutPNGFileName);
		xr_strcat(OutPNGFileName, ".png");
		xr_strcat(OutR16FileName, ".r16");
		Msg("Save %s", OutPNGFileName);

		IWriter* F = FS.w_open(OutR16FileName);
		if (F)
		{
			F->w(RawData.data(), RawData.size() * 2);
			FS.w_close(F);
		}

		TestImage.SaveToPng(OutPNGFileName);
	}
}

bool XRay::Editor::HeightmapUtils::SHeightMap::LoadRAW(const char* filename)
{
	if (!FS.TryLoad(filename))
	{
		return false;
	}

	IReader* F = FS.r_open(filename);
	if (!F) return false;

	Width = Height = (u32)sqrt(F->length() / sizeof(u16));
	if (Width * Height * sizeof(u16) != F->length())
	{
		FS.r_close(F);
		return false;
	}

	Data = (float*)xr_malloc(Width * Height * sizeof(float));

	u16* raw_data = (u16*)F->pointer();
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

	FS.r_close(F);
	return true;
}

void XRay::Editor::HeightmapUtils::SHeightMap::PrecacheRenderData(float scaleY, float cellSize, u32 baseColor)
{
	if (!RenderData.IsDirty)
		return;

	RenderData.Clear();
	
	float global_min_h = FLT_MAX;
	float global_max_h = -FLT_MAX;

	const u32 ChunkSize = SHeightMapRenderData::CHUNK_SIZE;
	const float FlatThreshold = SHeightMapRenderData::FLAT_THRESHOLD;

	u32 chunkCountX = (Width - 1) / ChunkSize + 1;
	u32 chunkCountZ = (Height - 1) / ChunkSize + 1;

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
			u32 x_end = std::min(x0 + ChunkSize, Width - 1);
			u32 z_end = std::min(z0 + ChunkSize, Height - 1);

			for (u32 z = z0; z <= z_end; ++z)
			{
				for (u32 x = x0; x <= x_end; ++x)
				{
					float h = GetHeight(x, z) * scaleY;
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

	// 2. Строим геометрию с учётом соседей
	auto Height2Color = [&](float h) -> u32
	{
		float t = (h - global_min_h) / std::max(global_max_h - global_min_h, EPS_S);
		t = std::clamp(t, 0.2f, 1.0f); // чтобы не было чёрного
		return color_rgba
		(
			u8(((baseColor >> 16) & 0xFF) * t),
			u8(((baseColor >> 8) & 0xFF) * t),
			u8((baseColor & 0xFF) * t),
			255
		);
	};

	for (u32 cz = 0; cz < chunkCountZ; ++cz)
	{
		for (u32 cx = 0; cx < chunkCountX; ++cx)
		{
			const ChunkInfo& info = ChunkInfos[cz * chunkCountX + cx];

			// Проверка соседей
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
			u32 x_end = std::min(x0 + ChunkSize, Width - 1);
			u32 z_end = std::min(z0 + ChunkSize, Height - 1);

			for (u32 z = z0; z < z_end; ++z)
			{
				for (u32 x = x0; x < x_end; ++x)
				{
					float h0 = GetHeight(x, z) * scaleY;
					float h1 = GetHeight(x + 1, z) * scaleY;
					float h2 = GetHeight(x + 1, z + 1) * scaleY;
					float h3 = GetHeight(x, z + 1) * scaleY;

					bool same_height = std::abs(h0 - h1) < FlatThreshold && std::abs(h1 - h2) < FlatThreshold && std::abs(h2 - h3) < FlatThreshold;

					if (same_height)
					{
						if (h0 == 0.0f)
							continue;

						Fvector v0 = { x * cellSize, h0, z * cellSize };
						Fvector v1 = { (x + 1) * cellSize, h1, z * cellSize };
						Fvector v2 = { (x + 1) * cellSize, h2, (z + 1) * cellSize };
						Fvector v3 = { x * cellSize, h3, (z + 1) * cellSize };

						chunk.BBox.modify(v0); chunk.BBox.modify(v2);

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
						// Строим как есть
						Fvector v0 = { x * cellSize, h0, z * cellSize };
						Fvector v1 = { (x + 1) * cellSize, h1, z * cellSize };
						Fvector v2 = { (x + 1) * cellSize, h2, (z + 1) * cellSize };
						Fvector v3 = { x * cellSize, h3, (z + 1) * cellSize };

						if (h0 == 0.0f && h1 == 0.0f && h2 == 0.0f && h3 == 0.0f)
							continue;

						chunk.BBox.modify(v0); chunk.BBox.modify(v2);

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


void XRay::Editor::HeightmapUtils::SHeightMap::Draw(float scaleY, float cellSize, u32 baseColor)
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

void XRay::Editor::HeightmapUtils::SHeightMap::MarkDirty()
{
	RenderData.IsDirty = true;
}

void XRay::Editor::HeightmapUtils::SHeightMapRenderData::BuildFromHeightmap(const float* Heightmap, int width, int Height)
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