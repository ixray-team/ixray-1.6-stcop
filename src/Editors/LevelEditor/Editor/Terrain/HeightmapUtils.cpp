#include "stdafx.h"
#include "HeightmapUtils.h"
#include <RedImage.hpp>

void XRay::Editor::HeightmapUtils::GenerateMeshByHeightmap(const SHeightMap& Heightmap, CEditableObject* OutMesh)
{
	if (!Heightmap.Data || Heightmap.Width < 2 || Heightmap.Height < 2 || !OutMesh)
	{
		Msg("! Invalid heightmap data or output mesh");
		return;
	}

	CEditableMesh* Mesh = new CEditableMesh(OutMesh);
	OutMesh->AppendMesh(Mesh);

	const u32 Width = Heightmap.Width;
	const u32 Height = Heightmap.Height;
	constexpr float SizeHM = 512;

	xr_vector<Fvector> Vertices;
	Vertices.reserve(Width * Height);

	const float StepHM = SizeHM / (Width - 1);
	constexpr float HalfHM = SizeHM / 2.0f;

	for (u32 z = 0; z < Height; z++) 
	{
		for (u32 x = 0; x < Width; x++) 
		{
			Fvector V;
			V.x = -(x * StepHM - HalfHM);
			V.z = (z * StepHM - HalfHM);
			V.y = Heightmap.GetHeight(x, z) * 100.f;
			Vertices.push_back(V);
		}
	}

	xr_vector<st_Face> Faces;
	const u32 QuadsX = Width - 1;
	const u32 QuadsZ = Height - 1;
	Faces.reserve(QuadsX * QuadsZ * 2);

	for (u32 z = 0; z < QuadsZ; z++)
	{
		for (u32 x = 0; x < QuadsX; x++)
		{
			const u32 V0 = z * Width + x;
			const u32 V1 = z * Width + x + 1;
			const u32 V2 = (z + 1) * Width + x;
			const u32 V3 = (z + 1) * Width + x + 1;

			if (Heightmap.GetHeight(x, z) <= 0.0f &&
				Heightmap.GetHeight(x + 1, z) <= 0.0f &&
				Heightmap.GetHeight(x, z + 1) <= 0.0f &&
				Heightmap.GetHeight(x + 1, z + 1) <= 0.0f)
			{
				continue;
			}

			// Первый треугольник
			st_Face Face1;
			Face1.pv[0].pindex = V0;
			Face1.pv[1].pindex = V1;
			Face1.pv[2].pindex = V2;
			Faces.push_back(Face1);

			// Второй треугольник
			st_Face Face2;
			Face2.pv[0].pindex = V1;
			Face2.pv[1].pindex = V3;
			Face2.pv[2].pindex = V2;
			Faces.push_back(Face2);
		}
	}

	Mesh->Create
	(
		Faces.data(),
		static_cast<u32>(Faces.size()),
		Vertices.data(),
		static_cast<u32>(Vertices.size()),
		nullptr,
		0
	);

	// 1. Создаём UV-карту с уникальными координатами
	st_VMap* mainUvMap = new st_VMap("Texture", vmtUV, false);
	const float uvStepX = 1.0f / (Width - 1);
	const float uvStepZ = 1.0f / (Height - 1);

	// Заполняем UV-координаты (аналогично оригиналу)
	for (u32 z = 0; z < Height; z++) 
	{
		for (u32 x = 0; x < Width; x++) 
		{
			Fvector2 uv;
			uv.x = 1.f - float(x) * uvStepX;
			uv.y = 1.f - float(z) * uvStepZ;
			mainUvMap->appendUV(uv);
		}
	}
	Mesh->m_VMaps.push_back(mainUvMap);

	// 2. Настраиваем VMRefs (как в оригинале)
	Mesh->m_VMRefs.resize(Vertices.size()); // Один VMRef на каждую вершину!
	for (u32 vertIdx = 0; vertIdx < Vertices.size(); ++vertIdx) {
		st_VMapPtLst& vmref = Mesh->m_VMRefs[vertIdx];
		vmref.count = 1; // Один UV-слой на вершину
		vmref.pts = xr_alloc<st_VMapPt>(1);
		vmref.pts[0].vmap_index = 0; // Индекс нашей UV-карты
		vmref.pts[0].index = vertIdx; // UV = индексу вершины
	}

	// 3. Настраиваем ссылки в вершинах фейсов
	for (u32 faceIdx = 0; faceIdx < Faces.size(); ++faceIdx) {
		for (u32 j = 0; j < 3; ++j) {
			// Ссылаемся на VMRef соответствующей вершины
			Mesh->m_Faces[faceIdx].pv[j].vmref = Faces[faceIdx].pv[j].pindex;
		}
	}

	CSurface* Surface = Mesh->GetSurfaceByFaceID(0);
	Surface->SetName("terrain");
	Surface->SetShader("levels\\zaton_earth");
	Surface->SetShaderXRLC("default");
	Surface->SetGameMtl("materials\\earth");
	Surface->SetTexture("terrain\\terrain_mp_atp");
	Surface->SetVMap("Texture");

	Surface->SetFVF(D3DFVF_XYZ | D3DFVF_NORMAL | D3DFVF_TEX1);
	Surface->OnDeviceCreate();
	OutMesh->Surfaces().push_back(Surface);
	IntVec FaceIndices(Faces.size());
	for (u32 i = 0; i < Faces.size(); ++i)
		FaceIndices[i] = i;
	Mesh->Surfaces()[Surface] = FaceIndices;

	Mesh->GenerateFNormals();
	Mesh->GenerateVNormals(nullptr, true);
	Mesh->GenerateAdjacency();

	OutMesh->UpdateBox();

	Msg("Terrain mesh created successfully: %d vertices, %d faces", Vertices.size(), Faces.size());
}

void XRay::Editor::HeightmapUtils::GenerateHeightmapByMesh(CEditableObject* Mesh, const xr_string& OutputFile)
{
	size_t TextureSizeX = 512;
	size_t TextureSizeY = 512;

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

