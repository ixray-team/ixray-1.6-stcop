#include "../EditorViewportMaterialResolver.h"
#include "../MaterialDependencyWatcher.h"
#include "MaterialTestHarness.h"

#include <algorithm>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <string>

using namespace Tiramisu::Editor;

namespace
{
bool HasDependency(const FEditorViewportMaterialResolution& Resolution, const xr_string_view Filename)
{
	return std::ranges::any_of(Resolution.AssetDependencies, [Filename](const std::filesystem::path& Path)
							   { return ToXrString(Path.filename().generic_string()) == Filename; });
}

void TestLegacyViewportResolution(TiramisuMaterialTestRunner& Runner)
{
	const std::filesystem::path Root =
		"gamedata/render_materials";
	TiramisuEditorViewportMaterialResolver Resolver;
	xr_vector<FMaterialDiagnostic> LoadDiagnostics;
	MATERIAL_CHECK(Runner, Resolver.Load(Root, &LoadDiagnostics));
	MATERIAL_CHECK(Runner, Resolver.IsLoaded());

	FEditorViewportLegacyMaterialSource Default;
	Default.MaterialSlot = 0x1234;
	Default.ShaderName = "default";
	Default.Textures = {"textures/kung"};
	Default.SurfaceName = "wall";
	const FEditorViewportMaterialResolution Opaque = Resolver.Resolve(Default);
	MATERIAL_CHECK(Runner, Opaque.Succeeded());
	MATERIAL_CHECK(Runner, Opaque.Legacy.Resolution == ELegacyMaterialResolution::LegacyMap);
	MATERIAL_CHECK(Runner, Opaque.Master.Id.Value == "a0f4d9f3-749c-4bc5-bf64-733927e19b20");
	MATERIAL_CHECK(Runner, Opaque.FlattenedInstance.Parent == Opaque.Master.Id.Value);
	MATERIAL_CHECK(Runner, std::get<xr_string>(Opaque.FlattenedInstance.Overrides.at(FMaterialParameterId{xr_string(LegacyBaseTextureParameterId)})) == "textures/kung");
	MATERIAL_CHECK(Runner, HasDependency(Opaque, "legacy-map.json"));
	MATERIAL_CHECK(Runner, HasDependency(Opaque, "legacy_opaque.material.json"));
	MATERIAL_CHECK(Runner, HasDependency(Opaque, "legacy_default.material-instance.json"));

	FEditorViewportLegacyMaterialSource Lightmapped = Default;
	Lightmapped.MaterialSlot = 0x2345;
	Lightmapped.ShaderName = "lmap";
	Lightmapped.Textures = {"textures/wall", "textures/wall_lmap"};
	const FEditorViewportMaterialResolution Lmap =
		Resolver.Resolve(Lightmapped);
	MATERIAL_CHECK(Runner, Lmap.Succeeded());
	MATERIAL_CHECK(Runner, std::get<bool>(Lmap.Resolved.StaticParameters.at(FMaterialParameterId{"e5d7660f-f7d5-4ccd-9be2-602174fa12aa"})));
	MATERIAL_CHECK(Runner, std::get<xr_string>(Lmap.Resolved.Parameters.at(FMaterialParameterId{xr_string(LegacyLightmapTextureParameterId)})) == "textures/wall_lmap");

	FEditorViewportLegacyMaterialSource Masked = Default;
	Masked.MaterialSlot = 0x3456;
	Masked.ShaderName = "default_aref";
	const FEditorViewportMaterialResolution Aref = Resolver.Resolve(Masked);
	MATERIAL_CHECK(Runner, Aref.Succeeded());
	MATERIAL_CHECK(Runner, Aref.Resolved.BlendMode == EMaterialBlendMode::Masked);
	MATERIAL_CHECK(Runner, Aref.Master.Id.Value == "5f4b3a8e-85fe-42bc-9080-f71eb497f04b");

	FEditorViewportLegacyMaterialSource SpawnIcon = Default;
	SpawnIcon.MaterialSlot = 0x3a56;
	SpawnIcon.ShaderName = "editor\\spawn_icon";
	SpawnIcon.Textures = {"ui\\spawn_test"};
	const FEditorViewportMaterialResolution Sprite =
		Resolver.Resolve(SpawnIcon);
	MATERIAL_CHECK(Runner, Sprite.Succeeded());
	MATERIAL_CHECK(Runner, Sprite.Legacy.Resolution == ELegacyMaterialResolution::LegacyMap);
	MATERIAL_CHECK(Runner, Sprite.Master.Id.Value == "d52ad18c-b4ce-4d4d-a5e3-7269071f43e1");
	MATERIAL_CHECK(Runner, Sprite.Resolved.BlendMode == EMaterialBlendMode::Translucent);
	MATERIAL_CHECK(Runner, Sprite.Resolved.ShadingModel == EMaterialShadingModel::Unlit);
	MATERIAL_CHECK(Runner, Sprite.TwoSided);
	MATERIAL_CHECK(Runner, HasDependency(Sprite, "legacy_editor_sprite.material.json"));
	MATERIAL_CHECK(Runner, std::get<xr_string>(Sprite.Resolved.Parameters.at(FMaterialParameterId{xr_string(LegacyBaseTextureParameterId)})) == "ui\\spawn_test");

	FEditorViewportLegacyMaterialSource ParticleAdditive = Default;
	ParticleAdditive.MaterialSlot = 0x3b56;
	ParticleAdditive.ShaderName = "editor\\particle_additive";
	ParticleAdditive.Textures = {"particles\\fire"};
	const FEditorViewportMaterialResolution Additive =
		Resolver.Resolve(ParticleAdditive);
	MATERIAL_CHECK(Runner, Additive.Succeeded());
	MATERIAL_CHECK(Runner, Additive.Master.Id.Value == "4ea32e17-b945-42ad-9c2d-8cb63b658b9a");
	MATERIAL_CHECK(Runner, Additive.Resolved.BlendMode == EMaterialBlendMode::Additive);
	MATERIAL_CHECK(Runner, Additive.Resolved.ShadingModel == EMaterialShadingModel::Unlit);
	MATERIAL_CHECK(Runner, Additive.TwoSided);
	MATERIAL_CHECK(Runner, HasDependency(Additive, "legacy_editor_particle_additive.material.json"));
	MATERIAL_CHECK(Runner, std::get<xr_string>(Additive.Resolved.Parameters.at(FMaterialParameterId{xr_string(LegacyBaseTextureParameterId)})) == "particles\\fire");

	FEditorViewportLegacyMaterialSource ParticleTranslucent = Default;
	ParticleTranslucent.MaterialSlot = 0x3c56;
	ParticleTranslucent.ShaderName = "editor\\particle_translucent";
	ParticleTranslucent.Textures = {"particles\\smoke"};
	const FEditorViewportMaterialResolution Translucent =
		Resolver.Resolve(ParticleTranslucent);
	MATERIAL_CHECK(Runner, Translucent.Succeeded());
	MATERIAL_CHECK(Runner, Translucent.Master.Id.Value == "ada19ea7-b527-46c0-a006-2e39f5170b45");
	MATERIAL_CHECK(Runner, Translucent.Resolved.BlendMode == EMaterialBlendMode::Translucent);
	MATERIAL_CHECK(Runner, Translucent.TwoSided);
	MATERIAL_CHECK(Runner, HasDependency(Translucent, "legacy_editor_particle_translucent.material.json"));

	FEditorViewportLegacyMaterialSource Glow = ParticleAdditive;
	Glow.MaterialSlot = 0x3d56;
	Glow.ShaderName = "editor\\glow_sprite";
	const FEditorViewportMaterialResolution GlowSprite =
		Resolver.Resolve(Glow);
	MATERIAL_CHECK(Runner, GlowSprite.Succeeded());
	MATERIAL_CHECK(Runner, GlowSprite.Master.Id.Value == Additive.Master.Id.Value);
	MATERIAL_CHECK(Runner, GlowSprite.Resolved.BlendMode == EMaterialBlendMode::Additive);

	FEditorViewportLegacyMaterialSource Automatic = Default;
	Automatic.MaterialSlot = 0x4567;
	Automatic.ShaderName = "unmapped/editor_shader";
	const FEditorViewportMaterialResolution Standard =
		Resolver.Resolve(Automatic);
	MATERIAL_CHECK(Runner, Standard.Succeeded());
	MATERIAL_CHECK(Runner, Standard.Legacy.Resolution == ELegacyMaterialResolution::AutomaticStandard);
	MATERIAL_CHECK(Runner, HasDiagnostic(Standard.Diagnostics, "legacy.automatic_standard"));

	FEditorViewportLegacyMaterialSource Missing;
	Missing.MaterialSlot = 0x5678;
	Missing.SurfaceName = "missing";
	const FEditorViewportMaterialResolution Error = Resolver.Resolve(Missing);
	MATERIAL_CHECK(Runner, Error.Succeeded());
	MATERIAL_CHECK(Runner, Error.Legacy.Resolution == ELegacyMaterialResolution::ErrorMaterial);
	MATERIAL_CHECK(Runner, Error.Master.Id.Value == "e67b251d-7905-4583-8450-0903c46ec652");
	MATERIAL_CHECK(Runner, Error.TwoSided);

	FEditorViewportLegacyMaterialSource TwoSided = Default;
	TwoSided.TwoSided = true;
	const FEditorViewportMaterialResolution DoubleSided =
		Resolver.Resolve(TwoSided);
	MATERIAL_CHECK(Runner, DoubleSided.Succeeded());
	MATERIAL_CHECK(Runner, DoubleSided.TwoSided);
	MATERIAL_CHECK(Runner, DoubleSided.CacheKey != Opaque.CacheKey);

	FEditorViewportLegacyMaterialSource Native;
	Native.MaterialSlot = 0x6789;
	Native.MaterialAsset = "128e21af-5c6f-4ec4-a2e3-8b44f90cb553";
	Native.Textures = {"textures/default/default_error"};
	Native.SurfaceName = "native instance slot";
	const FEditorViewportMaterialResolution Explicit =
		Resolver.Resolve(Native);
	MATERIAL_CHECK(Runner, Explicit.Succeeded());
	MATERIAL_CHECK(Runner, Explicit.Legacy.Resolution == ELegacyMaterialResolution::ExplicitMaterial);
	MATERIAL_CHECK(Runner, Explicit.Master.Id.Value == "67e3bc21-9df5-4fc2-ab60-1ad7d02ad6e3");
	MATERIAL_CHECK(Runner, HasDependency(Explicit, "example_red.material-instance.json"));
	MATERIAL_CHECK(Runner, !HasDependency(Explicit, "legacy-map.json"));
	MATERIAL_CHECK(Runner, std::get<FFloat4>(Explicit.Resolved.Parameters.at(FMaterialParameterId{"915ce004-8c2f-47ce-87c7-b4af787b835e"}))[0] == 1.0f);
	MATERIAL_CHECK(
		Runner,
		std::get<xr_string>(Explicit.Resolved.Parameters.at(
			FMaterialParameterId{xr_string(LegacyBaseTextureParameterId)}
		)) == "textures/kung"
	);

	FEditorViewportLegacyMaterialSource NativeMaster = Native;
	NativeMaster.MaterialSlot = 0x6790;
	NativeMaster.MaterialAsset = "standard_surface.material.json";
	NativeMaster.SurfaceName = "native master slot";
	const FEditorViewportMaterialResolution ExplicitMaster =
		Resolver.Resolve(NativeMaster);
	MATERIAL_CHECK(Runner, ExplicitMaster.Succeeded());
	MATERIAL_CHECK(
		Runner,
		ExplicitMaster.Legacy.Resolution ==
			ELegacyMaterialResolution::ExplicitMaterial
	);
	MATERIAL_CHECK(
		Runner,
		ExplicitMaster.Master.Id.Value ==
			"67e3bc21-9df5-4fc2-ab60-1ad7d02ad6e3"
	);
	MATERIAL_CHECK(
		Runner,
		HasDependency(
			ExplicitMaster,
			"standard_surface.material.json"
		)
	);
	MATERIAL_CHECK(
		Runner,
		std::get<xr_string>(ExplicitMaster.Resolved.Parameters.at(
			FMaterialParameterId{xr_string(LegacyBaseTextureParameterId)}
		)) == "textures/default/default_white"
	);

	TiramisuEditorViewportMaterialResolver MissingResolver;
	MATERIAL_CHECK(Runner, !MissingResolver.Load(Root / "missing", &LoadDiagnostics));
	MATERIAL_CHECK(Runner, !MissingResolver.IsLoaded());
}

void TestResolverDependencyReloadKeepsLastGood(TiramisuMaterialTestRunner& Runner)
{
	const std::filesystem::path SourceRoot = "gamedata/render_materials";
	const std::filesystem::path TemporaryRoot =
		std::filesystem::temp_directory_path() /
		("ixray-editor-material-reload-" + std::to_string(
											   std::chrono::steady_clock::now().time_since_epoch().count()
										   ));
	struct FScopedCleanup
	{
		std::filesystem::path Path;
		~FScopedCleanup()
		{
			std::error_code Error;
			std::filesystem::remove_all(Path, Error);
		}
	} Cleanup{TemporaryRoot};

	std::error_code Error;
	std::filesystem::copy(SourceRoot, TemporaryRoot, std::filesystem::copy_options::recursive, Error);
	MATERIAL_CHECK(Runner, !Error);
	if (Error)
	{
		return;
	}

	FEditorViewportLegacyMaterialSource Source;
	Source.MaterialSlot = 0xabcdu;
	Source.ShaderName = "default";
	Source.Textures = {"textures/kung"};

	TiramisuEditorViewportMaterialResolver LastGood;
	xr_vector<FMaterialDiagnostic> Diagnostics;
	MATERIAL_CHECK(Runner, LastGood.Load(TemporaryRoot, &Diagnostics));
	const FEditorViewportMaterialResolution Initial = LastGood.Resolve(Source);
	MATERIAL_CHECK(Runner, Initial.Succeeded());

	TiramisuMaterialDependencyWatcher Watcher;
	Watcher.Reset(Initial.AssetDependencies);
	MATERIAL_CHECK(Runner, Watcher.Poll().empty());
	const std::filesystem::path LegacyMap = TemporaryRoot / "legacy-map.json";
	{
		std::ofstream Output(LegacyMap, std::ios::binary | std::ios::app);
		Output << "\n ";
	}
	const xr_vector<FMaterialDependencyChange> ValidChanges = Watcher.Poll();
	MATERIAL_CHECK(Runner, ValidChanges.size() == 1);
	MATERIAL_CHECK(Runner, !ValidChanges.empty() && ValidChanges.front().Path.filename() == "legacy-map.json");

	TiramisuEditorViewportMaterialResolver Reloaded;
	MATERIAL_CHECK(Runner, Reloaded.Load(TemporaryRoot, &Diagnostics));
	MATERIAL_CHECK(Runner, Reloaded.Resolve(Source).Succeeded());

	{
		std::ofstream Output(LegacyMap, std::ios::binary | std::ios::trunc);
		Output << "{";
	}
	MATERIAL_CHECK(Runner, !Watcher.Poll().empty());
	TiramisuEditorViewportMaterialResolver InvalidCandidate;
	MATERIAL_CHECK(Runner, !InvalidCandidate.Load(TemporaryRoot, &Diagnostics));
	// Failed reload is never published: the previous resolver remains usable.
	MATERIAL_CHECK(Runner, Reloaded.Resolve(Source).Succeeded());
}
} // namespace

int main()
{
	TiramisuMaterialTestRunner Runner("xrEditorViewportMaterialResolverTests");
	TestLegacyViewportResolution(Runner);
	TestResolverDependencyReloadKeepsLastGood(Runner);
	return Runner.Finish();
}
