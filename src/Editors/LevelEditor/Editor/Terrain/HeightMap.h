#pragma once

class CEditableObject;

namespace XRay::Editor::HeightmapUtils
{
	struct SHeightMapVertex
	{
		Fvector p;
		u32 color;
	};

	struct SHeightMapChunk
	{
		xr_vector<Fvector> Vertices;    // Вершины чанка (уже оптимизированные)
		xr_vector<u32> Colors;          // Цвета (если нужны)
		xr_vector<SHeightMapVertex> Gpu; // Кэш вершин (позиция+цвет) для отрисовки
		Fbox BBox;                      // Bounding-box чанка
		bool IsFlat;					// Можно ли упростить (все Y почти одинаковы)
		bool IsValid = false;
		int LodLevel;					// Уровень детализации (0 = максимальный)
	};

	struct SGpuCache; // определён в HeightMap.cpp (ref_geom и т.п.)

	struct SHeightMapRenderData
	{
		static constexpr int CHUNK_SIZE = 32;
		static constexpr float FLAT_THRESHOLD = 0.1f;

		xr_vector<SHeightMapChunk> Chunks;

		bool IsDirty = true;

		xr_vector<u32> Colors;
		Fbox BoundingBox;

		// Персистентный GPU-буфер высотной карты (строится только при IsDirty)
		SGpuCache* Gpu = nullptr;
		xr_vector<u32> ChunkBase;   // базовая вершина чанка в общем буфере
		xr_vector<u32> ChunkCount;  // число вершин чанка

		void InvalidateGpu();

		void BuildFromHeightmap(const float* heightmap, int width, int height);
		void Clear()
		{
			InvalidateGpu();
			Chunks.clear();
			Colors.clear();
			BoundingBox.invalidate();
			ChunkBase.clear();
			ChunkCount.clear();
		}
	};

	struct SHeightMap
	{
		u32 Width;
		u32 Height;			// Размеры карты (в точках)
		float* Data;        // Массив высот [width * height]
		float MinH, MaxH;   // Минимальная и максимальная высота

		Fvector Pos;
		Fvector Size = { 1, 1, 1 };

		SHeightMapRenderData RenderData;

		SHeightMap() : Width(0), Height(0), Data(nullptr), MinH(0), MaxH(0) {}
		~SHeightMap() { if (Data) xr_free(Data); }

		IC float GetHeight(u32 x, u32 z) const
		{
			if (!Data || x >= Width || z >= Height) return 0.f;
			return Data[z * Width + x];
		}

		void Create(u32 w, u32 h, float fill);
		bool LoadRAW(const char* filename);
		bool SaveSteam(IWriter* Writer);
		bool LoadSteam(IReader* Reader);
		void PrecacheRenderData(float scaleY, float cellSize, u32 baseColor, bool geometryOnly);
		void BuildGpu();
		void Draw(float scaleY = 100.f, float cellSize = 1.f);
		void MarkDirty();
		bool RayPick(float& distance, const Fvector& start, const Fvector& direction, const Fmatrix& inv_parent, SRayPickInfo* pinf) const;
	};
};