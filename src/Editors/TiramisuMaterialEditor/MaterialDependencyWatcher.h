#pragma once

#include "TiramisuMaterialEditorTypes.h"

#include <cstdint>
#include <filesystem>
#include <map>
#include <span>
#include <vector>

namespace Tiramisu::Editor
{
enum class EMaterialDependencyChange : u8
{
	Created,
	Modified,
	Removed
};

// Одно обнаруженное изменение source material dependency.
struct FMaterialDependencyChange
{
	std::filesystem::path Path;
	EMaterialDependencyChange Change = EMaterialDependencyChange::Modified;
};

// Polling watcher source assets. Content hash обнаруживает быстрые записи даже при
// грубом timestamp; Poll принимает новое состояние и сообщает изменение один раз.
class TiramisuMaterialDependencyWatcher
{
public:
	// Reset принимает новый набор dependencies; Poll публикует каждое изменение один раз.
	void Reset(xr_span<const std::filesystem::path> Dependencies);
	[[nodiscard]] xr_vector<FMaterialDependencyChange> Poll();
	[[nodiscard]] bool Empty() const noexcept { return Snapshots.empty(); }
	[[nodiscard]] size_t Size() const noexcept { return Snapshots.size(); }

private:
	// Внутренний снимок состояния файла для polling watcher.
	struct FSnapshot
	{
		bool Exists = false;
		std::uintmax_t Size = 0;
		std::filesystem::file_time_type WriteTime{};
		u64 ContentHash = 0;

		friend bool operator==(const FSnapshot&, const FSnapshot&) = default;
	};

	[[nodiscard]] static std::filesystem::path NormalizePath(
		const std::filesystem::path& Path
	);
	[[nodiscard]] static FSnapshot Capture(const std::filesystem::path& Path);

	xr_map<std::filesystem::path, FSnapshot> Snapshots;
};
} // namespace Tiramisu::Editor
