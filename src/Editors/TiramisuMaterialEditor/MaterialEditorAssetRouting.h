#pragma once

#include "TiramisuMaterialEditorTypes.h"

#include <filesystem>

namespace Tiramisu::Editor
{
// Тип authoring asset определяет самостоятельное окно редактора.
enum class EMaterialEditorAssetKind : u8
{
	Unsupported,
	MasterMaterial,
	MaterialInstance
};

// Элемент общего каталога выбора материалов в редакторе.
struct FMaterialEditorAssetEntry
{
	std::filesystem::path RelativePath;
	EMaterialEditorAssetKind Kind = EMaterialEditorAssetKind::Unsupported;
};

// Распознаёт составные расширения material assets без чтения JSON.
[[nodiscard]] EMaterialEditorAssetKind ClassifyMaterialEditorAsset(
	const std::filesystem::path& Path
);

// Строит имя нового instance рядом с master material.
[[nodiscard]] std::filesystem::path MakeMaterialInstancePath(
	const std::filesystem::path& MasterMaterialPath
);

// Собирает master materials и instances под единым material root.
[[nodiscard]] xr_vector<FMaterialEditorAssetEntry>
CollectMaterialEditorAssets(const std::filesystem::path& MaterialRoot);

// Кодирует тип asset в корневую группу дерева Material Picker.
[[nodiscard]] xr_string MakeMaterialEditorPickerKey(
	const FMaterialEditorAssetEntry& Entry
);

// Возвращает относительный asset path из выбранного элемента Material Picker.
[[nodiscard]] xr_optional<FMaterialEditorAssetEntry>
ParseMaterialEditorPickerKey(xr_string_view PickerKey);
} // namespace Tiramisu::Editor
