#include "MaterialEditorAssetRouting.h"
#include "MaterialTestHarness.h"

#include <fstream>

using namespace Tiramisu;
using namespace Tiramisu::Editor;

int main(int argc, char** argv)
{
	TiramisuMaterialTestRunner Runner("xrMaterialEditorAssetRoutingTests");
	MATERIAL_CHECK(
		Runner,
		ClassifyMaterialEditorAsset("metal.material.json") ==
			EMaterialEditorAssetKind::MasterMaterial
	);
	MATERIAL_CHECK(
		Runner,
		ClassifyMaterialEditorAsset("metal.material-instance.json") ==
			EMaterialEditorAssetKind::MaterialInstance
	);
	MATERIAL_CHECK(
		Runner,
		ClassifyMaterialEditorAsset("METAL.MATERIAL.JSON") ==
			EMaterialEditorAssetKind::MasterMaterial
	);
	MATERIAL_CHECK(
		Runner,
		ClassifyMaterialEditorAsset("metal.json") ==
			EMaterialEditorAssetKind::Unsupported
	);
	MATERIAL_CHECK(
		Runner,
		MakeMaterialInstancePath("render_materials/metal.material.json") ==
			std::filesystem::path(
				"render_materials/metal.material-instance.json"
		)
	);

	const std::filesystem::path TestRoot =
		std::filesystem::temp_directory_path() /
		"ixray-material-editor-asset-routing-tests";
	std::error_code Error;
	std::filesystem::remove_all(TestRoot, Error);
	std::filesystem::create_directories(
		TestRoot / "world",
		Error
	);
	std::filesystem::create_directories(
		TestRoot / ".autosave",
		Error
	);
	std::ofstream(TestRoot / "world/metal.material.json") << "{}";
	std::ofstream(TestRoot / "world/metal.material-instance.json") << "{}";
	std::ofstream(TestRoot / "world/readme.json") << "{}";
	std::ofstream(TestRoot / ".autosave/hidden.material.json") << "{}";

	const xr_vector<FMaterialEditorAssetEntry> Assets =
		CollectMaterialEditorAssets(TestRoot);
	MATERIAL_CHECK(Runner, Assets.size() == 2);
	if (Assets.size() == 2)
	{
		MATERIAL_CHECK(
			Runner,
			Assets[0].RelativePath == std::filesystem::path(
				"world/metal.material-instance.json"
			)
		);
		MATERIAL_CHECK(
			Runner,
			Assets[0].Kind ==
				EMaterialEditorAssetKind::MaterialInstance
		);
		MATERIAL_CHECK(
			Runner,
			Assets[1].RelativePath == std::filesystem::path(
				"world/metal.material.json"
			)
		);
		MATERIAL_CHECK(
			Runner,
			Assets[1].Kind ==
				EMaterialEditorAssetKind::MasterMaterial
		);
	}
	const xr_string MasterKey = MakeMaterialEditorPickerKey({
		"world/metal.material.json",
		EMaterialEditorAssetKind::MasterMaterial
	});
	const xr_string InstanceKey = MakeMaterialEditorPickerKey({
		"world/metal.material-instance.json",
		EMaterialEditorAssetKind::MaterialInstance
	});
	MATERIAL_CHECK(
		Runner,
		MasterKey == "Materials\\world\\metal.material.json"
	);
	MATERIAL_CHECK(
		Runner,
		InstanceKey ==
			"Instances\\world\\metal.material-instance.json"
	);
	const xr_optional<FMaterialEditorAssetEntry> ParsedMaster =
		ParseMaterialEditorPickerKey(MasterKey);
	const xr_optional<FMaterialEditorAssetEntry> ParsedInstance =
		ParseMaterialEditorPickerKey(InstanceKey);
	MATERIAL_CHECK(Runner, ParsedMaster.has_value());
	MATERIAL_CHECK(Runner, ParsedInstance.has_value());
	if (ParsedMaster && ParsedInstance)
	{
		MATERIAL_CHECK(
			Runner,
			ParsedMaster->Kind ==
				EMaterialEditorAssetKind::MasterMaterial
		);
		MATERIAL_CHECK(
			Runner,
			ParsedInstance->Kind ==
				EMaterialEditorAssetKind::MaterialInstance
		);
	}
	MATERIAL_CHECK(
		Runner,
		!ParseMaterialEditorPickerKey(
			"Materials\\..\\outside.material.json"
		).has_value()
	);
	MATERIAL_CHECK(
		Runner,
		!ParseMaterialEditorPickerKey(
			"Instances\\wrong.material.json"
		).has_value()
	);
	std::filesystem::remove_all(TestRoot, Error);
	return Runner.Finish();
}
