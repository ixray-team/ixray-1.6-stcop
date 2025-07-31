#pragma once

class CEditableObject;

namespace XRay::Editor::HeightmapUtils
{
	struct SHeightMapChunk
	{
		xr_vector<Fvector> Vertices;    // Вершины чанка (уже оптимизированные)
		xr_vector<u32> Colors;          // Цвета (если нужны)
		Fbox BBox;                      // Bounding-box чанка
		bool IsFlat;					// Можно ли упростить (все Y почти одинаковы)
		bool IsValid = false;
		int LodLevel;					// Уровень детализации (0 = максимальный)
	};

	struct SHeightMapRenderData
	{
		static constexpr int CHUNK_SIZE = 32;
		static constexpr float FLAT_THRESHOLD = 0.1f;

		xr_vector<SHeightMapChunk> Chunks;

		bool IsDirty = true;

		xr_vector<u32> Colors;
		Fbox BoundingBox;


		void BuildFromHeightmap(const float* heightmap, int width, int height);
		void Clear()
		{
			Chunks.clear();
			Colors.clear();
			BoundingBox.invalidate();
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

		bool LoadRAW(const char* filename);
		bool SaveSteam(IWriter* Writer);
		bool LoadSteam(IReader* Reader);
		void PrecacheRenderData(float scaleY, float cellSize, u32 baseColor, bool geometryOnly);
		void Draw(float scaleY = 100.f, float cellSize = 1.f);
		void MarkDirty();
		bool RayPick(float& distance, const Fvector& start, const Fvector& direction, const Fmatrix& inv_parent, SRayPickInfo* pinf) const;
	};
};