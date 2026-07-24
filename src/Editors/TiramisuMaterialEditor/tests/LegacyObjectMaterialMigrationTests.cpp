#include "../LegacyObjectMaterialMigration.h"
#include "MaterialTestHarness.h"

#include <MaterialAsset.h>

#include <chrono>
#include <filesystem>
#include <fstream>

using namespace Tiramisu::Editor;

namespace
{
xr_string ReadText(const std::filesystem::path& Path)
{
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
	{
		return {};
	}
	const std::string Text{std::istreambuf_iterator<char>(Input), std::istreambuf_iterator<char>()};
	return ToXrString(Text);
}

void TestDeduplicatedFirstLoadMigration(TiramisuMaterialTestRunner& Runner)
{
	const std::filesystem::path TemporaryRoot =
		std::filesystem::temp_directory_path() /
		("ixray-object-material-migration-" + std::to_string(
												  std::chrono::steady_clock::now().time_since_epoch().count()
											  ));
	struct FCleanup
	{
		std::filesystem::path Path;
		~FCleanup()
		{
			std::error_code Error;
			std::filesystem::remove_all(Path, Error);
		}
	} Cleanup{TemporaryRoot};
	std::error_code Error;
	std::filesystem::copy("gamedata/render_materials", TemporaryRoot, std::filesystem::copy_options::recursive, Error);
	MATERIAL_CHECK(Runner, !Error);
	if (Error)
	{
		return;
	}

	TiramisuLegacyObjectMaterialMigrationService Service;
	xr_vector<FMaterialDiagnostic> Diagnostics;
	MATERIAL_CHECK(Runner, Service.Initialize(TemporaryRoot, &Diagnostics));

	FLegacyObjectSurfaceDescriptor Wall;
	Wall.SurfaceName = "wall";
	Wall.ShaderName = "default";
	Wall.CompilerShaderName = "default";
	Wall.GameMaterialName = "materials\\stone";
	Wall.TextureName = "textures\\wall";
	Wall.VertexMapName = "Texture";
	Wall.VertexFormat = 0x112;
	const xr_vector Surfaces = {Wall, Wall};

	const FLegacyObjectMaterialMigrationResult First =
		Service.Migrate("objects/buildings/wall.object", Surfaces);
	MATERIAL_CHECK(Runner, First.Succeeded());
	MATERIAL_CHECK(Runner, First.Bindings.size() == 2);
	MATERIAL_CHECK(Runner, First.CreatedInstanceCount == 1);
	MATERIAL_CHECK(Runner, First.ReusedInstanceCount == 1);
	MATERIAL_CHECK(Runner, First.Bindings[0].MaterialInstance == First.Bindings[1].MaterialInstance);
	MATERIAL_CHECK(Runner, First.Bindings[0].MaterialAsset == "generated/legacy_objects/" + First.Bindings[0].MaterialInstance + ".material-instance.json");
	MATERIAL_CHECK(Runner, First.Bindings[0].MaterialAsset == First.Bindings[1].MaterialAsset);
	MATERIAL_CHECK(Runner, First.DatabaseChanged);
	MATERIAL_CHECK(Runner, std::filesystem::is_regular_file(Service.GetDatabasePath()));

	const std::filesystem::path InstancePath = TemporaryRoot / "generated" /
											   "legacy_objects" /
											   (First.Bindings[0].MaterialInstance + ".material-instance.json");
	MATERIAL_CHECK(Runner, std::filesystem::is_regular_file(InstancePath));
	const FMaterialInstanceParseResult Instance =
		ParseMaterialInstanceJson(ReadText(InstancePath));
	MATERIAL_CHECK(Runner, Instance.Succeeded());
	MATERIAL_CHECK(Runner, Instance.Value.Parent == "ee5ffbc0-bd24-4aa8-9e16-50651ca1c269");
	MATERIAL_CHECK(Runner, std::get<xr_string>(Instance.Value.Overrides.at(FMaterialParameterId{xr_string(LegacyBaseTextureParameterId)})) == "textures\\wall");

	const FLegacyObjectMaterialMigrationResult Second =
		Service.Migrate("objects/buildings/wall-copy.object", {Wall});
	MATERIAL_CHECK(Runner, Second.Succeeded());
	MATERIAL_CHECK(Runner, Second.CreatedInstanceCount == 0);
	MATERIAL_CHECK(Runner, Second.ReusedInstanceCount == 1);
	MATERIAL_CHECK(Runner, Second.Bindings[0].MaterialInstance == First.Bindings[0].MaterialInstance);
	MATERIAL_CHECK(Runner, Second.Bindings[0].MaterialAsset == First.Bindings[0].MaterialAsset);

	FLegacyObjectSurfaceDescriptor TwoSided = Wall;
	TwoSided.TwoSided = true;
	const FLegacyObjectMaterialMigrationResult Variant =
		Service.Migrate("objects/buildings/fence.object", {TwoSided});
	MATERIAL_CHECK(Runner, Variant.Succeeded());
	MATERIAL_CHECK(Runner, Variant.CreatedInstanceCount == 1);
	MATERIAL_CHECK(Runner, Variant.Bindings[0].MaterialInstance != First.Bindings[0].MaterialInstance);

	FLegacyObjectSurfaceDescriptor DifferentGameMaterial = Wall;
	DifferentGameMaterial.GameMaterialName = "materials\\metal";
	const FLegacyObjectMaterialMigrationResult PhysicalOnly =
		Service.Migrate("objects/buildings/wall-metal-physics.object", {DifferentGameMaterial});
	MATERIAL_CHECK(Runner, PhysicalOnly.Succeeded());
	MATERIAL_CHECK(Runner, PhysicalOnly.Bindings[0].MaterialInstance == First.Bindings[0].MaterialInstance);

	const xr_string BeforeBatch =
		ReadText(Service.GetDatabasePath());
	const FLegacyObjectMaterialMigrationResult Deferred =
		Service.Migrate("levels/zaton#wall_0001", {Wall}, true);
	MATERIAL_CHECK(Runner, Deferred.Succeeded());
	MATERIAL_CHECK(Runner, Deferred.DatabaseChanged);
	MATERIAL_CHECK(Runner, ReadText(Service.GetDatabasePath()) == BeforeBatch);
	Diagnostics.clear();
	MATERIAL_CHECK(Runner, Service.FlushDatabase(Diagnostics));
	const xr_string AfterBatch =
		ReadText(Service.GetDatabasePath());
	MATERIAL_CHECK(Runner, AfterBatch != BeforeBatch);
	MATERIAL_CHECK(Runner, AfterBatch.find("levels/zaton#wall_0001") != xr_string::npos);
	Diagnostics.clear();
	MATERIAL_CHECK(Runner, Service.FlushDatabase(Diagnostics));
	MATERIAL_CHECK(Runner, ReadText(Service.GetDatabasePath()) == AfterBatch);

	// A new editor/service session must recover both the GUID used by native
	// scene assets and the readable relative path used by legacy properties.
	TiramisuLegacyObjectMaterialMigrationService ReloadedService;
	Diagnostics.clear();
	MATERIAL_CHECK(Runner, ReloadedService.Initialize(TemporaryRoot, &Diagnostics));
	const FLegacyObjectMaterialMigrationResult Reloaded =
		ReloadedService.Migrate("levels/zaton#wall_after_restart", {Wall});
	MATERIAL_CHECK(Runner, Reloaded.Succeeded());
	MATERIAL_CHECK(Runner, Reloaded.CreatedInstanceCount == 0);
	MATERIAL_CHECK(Runner, Reloaded.ReusedInstanceCount == 1);
	MATERIAL_CHECK(Runner, Reloaded.Bindings[0].MaterialInstance == First.Bindings[0].MaterialInstance);
	MATERIAL_CHECK(Runner, Reloaded.Bindings[0].MaterialAsset == First.Bindings[0].MaterialAsset);
}
} // namespace

int main()
{
	TiramisuMaterialTestRunner Runner("xrLegacyObjectMaterialMigrationTests");
	TestDeduplicatedFirstLoadMigration(Runner);
	return Runner.Finish();
}
