#pragma once

#include "TiramisuMaterialEditorTypes.h"

#include "EditorViewportMaterialResolver.h"

#include <MaterialAsset.h>

#include <cstdint>
#include <filesystem>
#include <string>
#include <string_view>
#include <vector>

namespace Tiramisu::Editor
{
inline constexpr u32 LegacyObjectMaterialMigrationVersion = 1;

// Renderer-neutral копия material-полей CSurface. Physical game material и UV/FVF
// сохраняются для audit, но в ключ дедупликации входят только render-material поля.
struct FLegacyObjectSurfaceDescriptor
{
	xr_string SurfaceName;
	xr_string ShaderName;
	xr_string CompilerShaderName;
	xr_string GameMaterialName;
	xr_string TextureName;
	xr_string VertexMapName;
	u32 Flags = 0;
	u32 VertexFormat = 0;
	bool TwoSided = false;
};

// Связь legacy surface с созданным или повторно использованным MaterialInstance.
struct FLegacyObjectMaterialBinding
{
	xr_string SurfaceName;
	xr_string SourceKey;
	// Стабильный GUID сохраняется в native StaticMesh/RenderScene assets.
	xr_string MaterialInstance;
	// Путь относительно $game_render_materials$. Legacy CSceneObject использует его
	// для читаемых properties и явного viewport material source.
	xr_string MaterialAsset;
	bool TwoSided = false;
	bool Created = false;
};

// Результат транзакционной миграции всех surfaces одного legacy object.
struct FLegacyObjectMaterialMigrationResult
{
	xr_vector<FLegacyObjectMaterialBinding> Bindings;
	xr_vector<FMaterialDiagnostic> Diagnostics;
	u32 CreatedInstanceCount = 0;
	u32 ReusedInstanceCount = 0;
	bool DatabaseChanged = false;

	[[nodiscard]] bool Succeeded() const noexcept;
};

// Строит canonical deduplication key только из render-relevant полей surface.
[[nodiscard]] xr_string BuildLegacyObjectMaterialKey(
	const FLegacyObjectSurfaceDescriptor& Surface
);

// Миграция старых .object при первой загрузке. Сервис разрешает legacy parent со
// static switches, дедуплицирует дочерний instance, атомарно пишет assets и только
// затем публикует migration database.
class TiramisuLegacyObjectMaterialMigrationService
{
public:
	// Загружает database и выполняет транзакционную миграцию объектов.
	[[nodiscard]] bool Initialize(
		const std::filesystem::path& MaterialRoot,
		xr_vector<FMaterialDiagnostic>* OutDiagnostics = nullptr
	);
	[[nodiscard]] bool IsInitialized() const noexcept { return Initialized; }

	[[nodiscard]] FLegacyObjectMaterialMigrationResult Migrate(
		xr_string_view ObjectSource,
		const xr_vector<FLegacyObjectSurfaceDescriptor>& Surfaces,
		bool DeferDatabaseSave = false
	);

	// Импорт уровня накапливает тысячи изменений и публикует deterministic database
	// один раз после успешного завершения всего прохода.
	[[nodiscard]] bool FlushDatabase(
		xr_vector<FMaterialDiagnostic>& Diagnostics
	);

	[[nodiscard]] const std::filesystem::path& GetDatabasePath() const noexcept
	{
		return DatabasePath;
	}

private:
	// Внутренняя детерминированная запись базы дедупликации migration.
	struct FDatabaseEntry
	{
		xr_string Key;
		xr_string Instance;
		xr_string AssetPath;
		xr_string Parent;
		xr_string ShaderName;
		xr_string CompilerShaderName;
		xr_string TextureName;
		bool TwoSided = false;
		xr_vector<xr_string> Sources;
	};

	[[nodiscard]] bool LoadDatabase(
		xr_vector<FMaterialDiagnostic>& Diagnostics
	);
	[[nodiscard]] bool SaveDatabase(
		xr_vector<FMaterialDiagnostic>& Diagnostics
	) const;

	std::filesystem::path Root;
	std::filesystem::path GeneratedRoot;
	std::filesystem::path DatabasePath;
	TiramisuEditorViewportMaterialResolver Resolver;
	xr_vector<FDatabaseEntry> Entries;
	bool PendingDatabaseChanges = false;
	bool Initialized = false;
};
} // namespace Tiramisu::Editor
