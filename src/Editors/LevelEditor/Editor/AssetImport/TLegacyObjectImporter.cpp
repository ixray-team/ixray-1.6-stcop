#include "stdafx.h"

#include "TLegacyObjectImporter.h"

#include "../../../xrECore/Editor/EditMesh.h"
#include "../../../xrECore/Editor/EditObject.h"
#include "../../../TiramisuMaterialEditor/LegacyObjectMaterialMigration.h"
#include "../../../TiramisuMaterialEditor/MaterialEditorFileIO.h"

#include <MaterialTypes.h>
#include <SceneAsset.h>
#include <SceneConversionDump.h>

#include <algorithm>
#include <array>
#include <cctype>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <ranges>
#include <sstream>
#include <unordered_map>

namespace
{
using namespace Tiramisu;

constexpr u32 LegacyObjectImporterVersion = 2;
constexpr u64 FnvOffset = 14695981039346656037ull;
constexpr u64 FnvPrime = 1099511628211ull;

void HashBytes(u64& Hash, const void* Data, const size_t Size)
{
	const auto* Bytes = static_cast<const u8*>(Data);
	for (size_t Index = 0; Index < Size; ++Index)
	{
		Hash ^= Bytes[Index];
		Hash *= FnvPrime;
	}
}

[[nodiscard]] xr_string HashFile(const std::filesystem::path& Path)
{
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
	{
		return {};
	}
	u64 Hash = FnvOffset;
	xr_array<char, 64 * 1024> Buffer;
	while (Input)
	{
		Input.read(Buffer.data(), Buffer.size());
		const std::streamsize Count = Input.gcount();
		if (Count > 0)
		{
			HashBytes(Hash, Buffer.data(), static_cast<size_t>(Count));
		}
	}
	std::ostringstream Text;
	Text << std::hex << std::setfill('0') << std::setw(16) << Hash;
	return Text.str();
}

[[nodiscard]] xr_string NormalizeSourceIdentity(
	const std::filesystem::path& Path
)
{
	xr_string Result = Path.lexically_normal().generic_string();
	std::ranges::transform(Result, Result.begin(), [](const char Character)
						   { return static_cast<char>(std::tolower(
								 static_cast<unsigned char>(Character)
							 )); });
	return Result;
}

void AddDiagnostic(Scene::FSceneConversionDump& Dump, xr_string Severity, xr_string Code, xr_string Message)
{
	Dump.Diagnostics.push_back(
		{std::move(Severity), std::move(Code), std::move(Message)}
	);
}

void AppendMaterialDiagnostics(Scene::FSceneConversionDump& Dump, const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
	for (const FMaterialDiagnostic& Diagnostic : Diagnostics)
	{
		const char* Severity = "info";
		switch (Diagnostic.Severity)
		{
			case EMaterialDiagnosticSeverity::Warning:
				Severity = "warning";
				break;
			case EMaterialDiagnosticSeverity::Error:
				Severity = "error";
				break;
			default:
				break;
		}
		AddDiagnostic(Dump, Severity, Diagnostic.Code, Diagnostic.Message);
	}
}

[[nodiscard]] bool HasErrors(const Scene::FSceneConversionDump& Dump)
{
	return std::ranges::any_of(Dump.Diagnostics, [](const Scene::FSceneConversionDiagnostic& Diagnostic)
							   { return Diagnostic.Severity == "error"; });
}

[[nodiscard]] bool WriteDump(const std::filesystem::path& Path, const Scene::FSceneConversionDump& Dump)
{
	std::error_code Error;
	std::filesystem::create_directories(Path.parent_path(), Error);
	if (Error)
	{
		return false;
	}
	return Editor::WriteTextFileAtomically(Path, Scene::SerializeSceneConversionDumpJson(Dump)).Success;
}

[[nodiscard]] std::filesystem::path FailedDumpPath(
	const std::filesystem::path& Target
)
{
	std::filesystem::path Result = Target;
	Result += ".migration.failed.json";
	return Result;
}

[[nodiscard]] std::filesystem::path SuccessDumpPath(
	const std::filesystem::path& Target
)
{
	std::filesystem::path Result = Target;
	Result += ".migration.json";
	return Result;
}

[[nodiscard]] xr_vector<Editor::FLegacyObjectSurfaceDescriptor>
BuildSurfaceDescriptors(CEditableObject& Object)
{
	xr_vector<Editor::FLegacyObjectSurfaceDescriptor> Result;
	Result.reserve(Object.Surfaces().size());
	for (const CSurface* Surface : Object.Surfaces())
	{
		Editor::FLegacyObjectSurfaceDescriptor Descriptor;
		if (Surface)
		{
			Descriptor.SurfaceName = Surface->_Name();
			Descriptor.ShaderName = Surface->_ShaderName();
			Descriptor.CompilerShaderName = Surface->_ShaderXRLCName();
			Descriptor.GameMaterialName = Surface->_GameMtlName();
			Descriptor.TextureName = Surface->_Texture();
			Descriptor.VertexMapName = Surface->_VMap();
			Descriptor.Flags = Surface->m_Flags.get();
			Descriptor.VertexFormat = Surface->_FVF();
			Descriptor.TwoSided =
				Surface->m_Flags.is(CSurface::sf2Sided);
		}
		Result.push_back(std::move(Descriptor));
	}
	return Result;
}

void ReadTexCoords(CEditableMesh& Mesh, const st_FaceVert& FaceVertex, xr_array<float, 2>& TexCoord0, xr_array<float, 2>& TexCoord1)
{
	if (FaceVertex.vmref < 0 ||
		static_cast<size_t>(FaceVertex.vmref) >=
			Mesh.GetVMRefs().size())
	{
		return;
	}
	const st_VMapPtLst& References = Mesh.GetVMRefs()[FaceVertex.vmref];
	u32 TexCoordIndex = 0;
	for (u32 ReferenceIndex = 0;
		 ReferenceIndex < References.count && TexCoordIndex < 2;
		 ++ReferenceIndex)
	{
		const st_VMapPt& Point = References.pts[ReferenceIndex];
		if (Point.vmap_index < 0 || Point.index < 0 ||
			static_cast<size_t>(Point.vmap_index) >=
				Mesh.GetVMaps().size())
		{
			continue;
		}
		const st_VMap* VertexMap = Mesh.GetVMaps()[Point.vmap_index];
		if (!VertexMap || VertexMap->type != vmtUV ||
			Point.index >= VertexMap->size())
		{
			continue;
		}
		const Fvector2& UV = VertexMap->getUV(Point.index);
		(TexCoordIndex == 0 ? TexCoord0 : TexCoord1) = {UV.x, UV.y};
		++TexCoordIndex;
	}
}

void CalculateTriangleTangents(
	xr_vector<Scene::FStaticMeshVertex>& Vertices
)
{
	for (size_t Base = 0; Base + 2 < Vertices.size(); Base += 3)
	{
		Scene::FStaticMeshVertex& V0 = Vertices[Base];
		Scene::FStaticMeshVertex& V1 = Vertices[Base + 1];
		Scene::FStaticMeshVertex& V2 = Vertices[Base + 2];
		const float E1x = V1.Position[0] - V0.Position[0];
		const float E1y = V1.Position[1] - V0.Position[1];
		const float E1z = V1.Position[2] - V0.Position[2];
		const float E2x = V2.Position[0] - V0.Position[0];
		const float E2y = V2.Position[1] - V0.Position[1];
		const float E2z = V2.Position[2] - V0.Position[2];
		const float Du1 = V1.TexCoord0[0] - V0.TexCoord0[0];
		const float Dv1 = V1.TexCoord0[1] - V0.TexCoord0[1];
		const float Du2 = V2.TexCoord0[0] - V0.TexCoord0[0];
		const float Dv2 = V2.TexCoord0[1] - V0.TexCoord0[1];
		const float Determinant = Du1 * Dv2 - Dv1 * Du2;
		xr_array<float, 3> Tangent = {1.0f, 0.0f, 0.0f};
		xr_array<float, 3> Bitangent = {0.0f, 1.0f, 0.0f};
		if (std::abs(Determinant) > 1.0e-8f)
		{
			const float Inverse = 1.0f / Determinant;
			Tangent = {
				(E1x * Dv2 - E2x * Dv1) * Inverse,
				(E1y * Dv2 - E2y * Dv1) * Inverse,
				(E1z * Dv2 - E2z * Dv1) * Inverse
			};
			Bitangent = {
				(E2x * Du1 - E1x * Du2) * Inverse,
				(E2y * Du1 - E1y * Du2) * Inverse,
				(E2z * Du1 - E1z * Du2) * Inverse
			};
		}
		const float Length = std::sqrt(Tangent[0] * Tangent[0] + Tangent[1] * Tangent[1] + Tangent[2] * Tangent[2]);
		if (Length > 1.0e-8f)
		{
			for (float& Value : Tangent)
			{
				Value /= Length;
			}
		}
		for (Scene::FStaticMeshVertex* Vertex : {&V0, &V1, &V2})
		{
			const auto& N = Vertex->Normal;
			const xr_array<float, 3> Cross = {
				N[1] * Tangent[2] - N[2] * Tangent[1],
				N[2] * Tangent[0] - N[0] * Tangent[2],
				N[0] * Tangent[1] - N[1] * Tangent[0]
			};
			const float Handedness =
				Cross[0] * Bitangent[0] +
							Cross[1] * Bitangent[1] +
							Cross[2] * Bitangent[2] <
						0.0f
					? -1.0f
					: 1.0f;
			Vertex->Tangent = {
				Tangent[0], Tangent[1], Tangent[2], Handedness
			};
		}
	}
}

[[nodiscard]] bool BuildStaticMeshGeometry(CEditableObject& Object, const xr_vector<Editor::FLegacyObjectMaterialBinding>& Bindings, Scene::FStaticMeshAsset& Asset, Scene::FSceneConversionDump& Dump)
{
	if (Object.Surfaces().size() != Bindings.size())
	{
		AddDiagnostic(Dump, "error", "object_import.surface_mismatch", "Material migration returned a different surface count.");
		return false;
	}

	Asset.MaterialSlots.reserve(Bindings.size());
	for (size_t Index = 0; Index < Bindings.size(); ++Index)
	{
		const CSurface* Surface = Object.Surfaces()[Index];
		if (!Surface)
		{
			AddDiagnostic(Dump, "error", "object_import.null_surface", "Legacy object contains a null surface.");
			return false;
		}
		Asset.MaterialSlots.push_back(
			{Bindings[Index].SurfaceName,
			 Bindings[Index].MaterialInstance,
			 Bindings[Index].TwoSided}
		);
	}

	struct FMeshNormalScope
	{
		CEditableMesh* Mesh = nullptr;
		const Fvector* Normals = nullptr;
		FMeshNormalScope(CEditableMesh* InMesh, const Fvector* InNormals)
			: Mesh(InMesh), Normals(InNormals)
		{
		}
		FMeshNormalScope(const FMeshNormalScope&) = delete;
		FMeshNormalScope& operator=(const FMeshNormalScope&) = delete;
		FMeshNormalScope(FMeshNormalScope&& Other) noexcept
			: Mesh(Other.Mesh), Normals(Other.Normals)
		{
			Other.Mesh = nullptr;
			Other.Normals = nullptr;
		}
		FMeshNormalScope& operator=(FMeshNormalScope&&) = delete;
		~FMeshNormalScope()
		{
			if (Mesh)
			{
				Mesh->UnloadVNormals();
			}
		}
	};
	xr_vector<FMeshNormalScope> NormalScopes;
	NormalScopes.reserve(Object.Meshes().size());
	for (CEditableMesh* Mesh : Object.Meshes())
	{
		if (!Mesh)
		{
			continue;
		}
		Mesh->GenerateVNormals(nullptr);
		const Fvector* Normals = Mesh->GetActiveVNormals();
		NormalScopes.emplace_back(Mesh, Normals);
	}

	for (size_t SlotIndex = 0; SlotIndex < Bindings.size(); ++SlotIndex)
	{
		const CSurface* Surface = Object.Surfaces()[SlotIndex];
		Scene::FStaticMeshSection Section;
		Section.FirstIndex =
			static_cast<u32>(Asset.Indices.size());
		Section.MaterialSlot = static_cast<u32>(SlotIndex);
		for (const FMeshNormalScope& Scope : NormalScopes)
		{
			CEditableMesh& Mesh = *Scope.Mesh;
			const auto Found = Mesh.GetSurfFaces().find(
				const_cast<CSurface*>(Surface)
			);
			if (Found == Mesh.GetSurfFaces().end())
			{
				continue;
			}
			for (const int SignedFaceIndex : Found->second)
			{
				if (SignedFaceIndex < 0 ||
					static_cast<u32>(SignedFaceIndex) >=
						Mesh.GetFCount())
				{
					AddDiagnostic(Dump, "error", "object_import.face_out_of_range", "Legacy surface references a face outside its mesh.");
					return false;
				}
				const u32 FaceIndex =
					static_cast<u32>(SignedFaceIndex);
				const st_Face& Face = Mesh.GetFaces()[FaceIndex];
				for (u32 Corner = 0; Corner < 3; ++Corner)
				{
					const st_FaceVert& FaceVertex = Face.pv[Corner];
					if (FaceVertex.pindex < 0 ||
						static_cast<u32>(FaceVertex.pindex) >=
							Mesh.GetVCount())
					{
						AddDiagnostic(Dump, "error", "object_import.vertex_out_of_range", "Legacy face references a vertex outside its mesh.");
						return false;
					}
					Scene::FStaticMeshVertex Vertex;
					const Fvector& Position =
						Mesh.GetVertices()[FaceVertex.pindex];
					Vertex.Position = {
						Position.x, Position.y, Position.z
					};
					if (Scope.Normals)
					{
						const Fvector& Normal =
							Scope.Normals[FaceIndex * 3 + Corner];
						Vertex.Normal = {
							Normal.x, Normal.y, Normal.z
						};
					}
					ReadTexCoords(Mesh, FaceVertex, Vertex.TexCoord0, Vertex.TexCoord1);
					Asset.Indices.push_back(
						static_cast<u32>(
							Asset.Vertices.size()
						)
					);
					Asset.Vertices.push_back(Vertex);
				}
			}
		}
		Section.IndexCount =
			static_cast<u32>(Asset.Indices.size()) -
			Section.FirstIndex;
		if (Section.IndexCount != 0)
		{
			Asset.Sections.push_back(Section);
		}
	}
	if (Asset.Vertices.empty() || Asset.Indices.empty())
	{
		AddDiagnostic(Dump, "error", "object_import.empty_geometry", "Legacy object produced no renderable static-mesh geometry.");
		return false;
	}
	CalculateTriangleTangents(Asset.Vertices);
	return true;
}
} // namespace

std::filesystem::path MakeImportedStaticMeshPath(
	const std::filesystem::path& SourcePath,
	const std::filesystem::path& StaticMeshRoot
)
{
	std::filesystem::path Relative = SourcePath.filename();
	const std::filesystem::path Normalized =
		SourcePath.lexically_normal();
	xr_vector<std::filesystem::path> Parts;
	for (const std::filesystem::path& Part : Normalized)
	{
		Parts.push_back(Part);
	}
	for (size_t Index = 0; Index < Parts.size(); ++Index)
	{
		xr_string Name = Parts[Index].generic_string();
		std::ranges::transform(Name, Name.begin(), [](const char Character)
							   { return static_cast<char>(std::tolower(
									 static_cast<unsigned char>(Character)
								 )); });
		if (Name == "objects" && Index + 1 < Parts.size())
		{
			Relative.clear();
			for (size_t Tail = Index + 1; Tail < Parts.size(); ++Tail)
			{
				Relative /= Parts[Tail];
			}
			break;
		}
	}
	Relative.replace_extension(".static-mesh.json");
	return StaticMeshRoot / "imported" / Relative;
}

FLegacyObjectImportResult ImportLegacyObjectAsset(
	const std::filesystem::path& SourcePath,
	const std::filesystem::path& MaterialRoot,
	const std::filesystem::path& StaticMeshRoot
)
{
	using namespace Tiramisu;
	FLegacyObjectImportResult Result;
	Result.SourcePath = SourcePath.lexically_normal();
	Result.TargetPath = MakeImportedStaticMeshPath(
		Result.SourcePath, StaticMeshRoot
	);
	Result.TargetPayloadPath =
		Scene::MakeStaticMeshGeometryPath(Result.TargetPath);

	Scene::FSceneConversionDump Dump;
	Dump.Importer = "tiramisu_legacy_object";
	Dump.ImporterVersion = LegacyObjectImporterVersion;
	Dump.SourceType = "object";
	Dump.SourcePath = Result.SourcePath.generic_string();
	Dump.SourceHash = HashFile(Result.SourcePath);
	Dump.TargetPath = Result.TargetPath.generic_string();
	Dump.TargetPayloadPath =
		Result.TargetPayloadPath.generic_string();
	if (Dump.SourceHash.empty())
	{
		AddDiagnostic(Dump, "error", "object_import.source_read_failed", "Cannot read legacy .object source.");
		Result.DumpPath = FailedDumpPath(Result.TargetPath);
		(void)WriteDump(Result.DumpPath, Dump);
		Result.Diagnostics = Dump.Diagnostics;
		return Result;
	}

	CEditableObject Object(Result.SourcePath.stem().string().c_str());
	if (!Object.Load(Result.SourcePath.string().c_str()))
	{
		AddDiagnostic(Dump, "error", "object_import.load_failed", "CEditableObject could not load the legacy source.");
		Result.DumpPath = FailedDumpPath(Result.TargetPath);
		(void)WriteDump(Result.DumpPath, Dump);
		Result.Diagnostics = Dump.Diagnostics;
		return Result;
	}
	Dump.MeshCount = static_cast<u32>(Object.Meshes().size());

	Editor::TiramisuLegacyObjectMaterialMigrationService Migration;
	xr_vector<FMaterialDiagnostic> InitializeDiagnostics;
	if (!Migration.Initialize(MaterialRoot, &InitializeDiagnostics))
	{
		AppendMaterialDiagnostics(Dump, InitializeDiagnostics);
		AddDiagnostic(Dump, "error", "object_import.material_migration_unavailable", "Material migration database could not be initialized.");
		Result.DumpPath = FailedDumpPath(Result.TargetPath);
		(void)WriteDump(Result.DumpPath, Dump);
		Result.Diagnostics = Dump.Diagnostics;
		return Result;
	}
	const xr_vector SurfaceDescriptors =
		BuildSurfaceDescriptors(Object);
	Editor::FLegacyObjectMaterialMigrationResult MaterialMigration =
		Migration.Migrate(Result.SourcePath.generic_string(), SurfaceDescriptors);
	AppendMaterialDiagnostics(Dump, MaterialMigration.Diagnostics);
	Dump.CreatedMaterialInstances =
		MaterialMigration.CreatedInstanceCount;
	Dump.ReusedMaterialInstances =
		MaterialMigration.ReusedInstanceCount;
	for (const Editor::FLegacyObjectMaterialBinding& Binding :
		 MaterialMigration.Bindings)
	{
		Dump.MaterialMappings.push_back({Binding.SurfaceName, Binding.SourceKey, Binding.MaterialInstance, Binding.TwoSided, Binding.Created});
	}
	if (!MaterialMigration.Succeeded())
	{
		Result.DumpPath = FailedDumpPath(Result.TargetPath);
		(void)WriteDump(Result.DumpPath, Dump);
		Result.Diagnostics = Dump.Diagnostics;
		return Result;
	}

	Scene::FStaticMeshAsset Asset;
	Asset.Id = GenerateDeterministicMaterialGuid(
		"legacy-object-static-mesh",
		NormalizeSourceIdentity(Result.SourcePath)
	);
	Asset.Name = Object.GetName();
	Asset.SourcePath = Result.TargetPath.generic_string();
	Dump.TargetAssetId = Asset.Id;
	if (!BuildStaticMeshGeometry(Object, MaterialMigration.Bindings, Asset, Dump))
	{
		Result.DumpPath = FailedDumpPath(Result.TargetPath);
		(void)WriteDump(Result.DumpPath, Dump);
		Result.Diagnostics = Dump.Diagnostics;
		return Result;
	}
	Dump.VertexCount = static_cast<u32>(Asset.Vertices.size());
	Dump.IndexCount = static_cast<u32>(Asset.Indices.size());
	const Scene::FStaticMeshAssetWriteResult Write =
		Scene::SaveStaticMeshAsset(Result.TargetPath, Asset);
	for (const Scene::FSceneDiagnostic& Diagnostic :
		 Write.Diagnostics)
	{
		AddDiagnostic(Dump, Diagnostic.Severity == Scene::ESceneDiagnosticSeverity::Error ? "error" : "warning", Diagnostic.Code, Diagnostic.Message);
	}
	if (!Write.Succeeded() || HasErrors(Dump))
	{
		Result.DumpPath = FailedDumpPath(Result.TargetPath);
		(void)WriteDump(Result.DumpPath, Dump);
		Result.Diagnostics = Dump.Diagnostics;
		return Result;
	}

	Dump.Status = Scene::ESceneConversionStatus::Succeeded;
	Result.DumpPath = SuccessDumpPath(Result.TargetPath);
	Dump.AssetMappings.push_back({Result.SourcePath.generic_string(), Result.TargetPath.generic_string(), Result.DumpPath.generic_string(), Asset.Id, Result.TargetPayloadPath.generic_string()});
	if (!WriteDump(Result.DumpPath, Dump))
	{
		AddDiagnostic(Dump, "error", "object_import.dump_write_failed", "Native asset was written, but the mandatory conversion dump "
																		"could not be published.");
		Result.Diagnostics = Dump.Diagnostics;
		return Result;
	}
	Result.TargetAssetId = Asset.Id;
	Result.Diagnostics = Dump.Diagnostics;
	Result.Succeeded = true;
	return Result;
}
