#include "../MaterialDependencyWatcher.h"
#include "MaterialTestHarness.h"

#include <chrono>
#include <filesystem>
#include <fstream>
#include <string>

using namespace Tiramisu::Editor;

namespace
{
void WriteText(const std::filesystem::path& Path, const xr_string& Text)
{
	std::ofstream Output(Path, std::ios::binary | std::ios::trunc);
	Output << Text;
}

void TestDependencyChanges(TiramisuMaterialTestRunner& Runner)
{
	const std::filesystem::path Root = std::filesystem::temp_directory_path() /
									   ("xr-material-watcher-" + std::to_string(
																	 std::chrono::steady_clock::now().time_since_epoch().count()
																 ));
	std::filesystem::create_directories(Root);
	const std::filesystem::path Existing = Root / "material.hlsl";
	const std::filesystem::path Missing = Root / "created.material.json";
	WriteText(Existing, "old");

	TiramisuMaterialDependencyWatcher Watcher;
	const std::filesystem::path Dependencies[] = {
		Existing, Existing.parent_path() / "." / Existing.filename(), Missing
	};
	Watcher.Reset(Dependencies);
	MATERIAL_CHECK(Runner, Watcher.Size() == 2);
	MATERIAL_CHECK(Runner, Watcher.Poll().empty());

	// The replacement has the same byte count; content hashing must still
	// catch it even if timestamp granularity hides the write.
	WriteText(Existing, "new");
	auto Changes = Watcher.Poll();
	MATERIAL_CHECK(Runner, Changes.size() == 1);
	MATERIAL_CHECK(Runner, Changes.front().Change == EMaterialDependencyChange::Modified);
	MATERIAL_CHECK(Runner, Watcher.Poll().empty());

	WriteText(Missing, "{}");
	Changes = Watcher.Poll();
	MATERIAL_CHECK(Runner, Changes.size() == 1);
	MATERIAL_CHECK(Runner, Changes.front().Change == EMaterialDependencyChange::Created);

	std::filesystem::remove(Existing);
	Changes = Watcher.Poll();
	MATERIAL_CHECK(Runner, Changes.size() == 1);
	MATERIAL_CHECK(Runner, Changes.front().Change == EMaterialDependencyChange::Removed);

	Watcher.Reset({});
	MATERIAL_CHECK(Runner, Watcher.Empty());
	std::filesystem::remove_all(Root);
}
} // namespace

int main()
{
	TiramisuMaterialTestRunner Runner("xrMaterialDependencyWatcherTests");
	TestDependencyChanges(Runner);
	return Runner.Finish();
}
