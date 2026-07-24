#include "MaterialEditorFileIO.h"

#include <MaterialTypes.h>

#include <fstream>
#include <system_error>
#include <utility>

#if defined(_WIN32)
#include <Windows.h>
#endif

namespace Tiramisu::Editor
{
namespace
{
class FTemporaryFileGuard
{
public:
	explicit FTemporaryFileGuard(std::filesystem::path Path)
		: FilePath(std::move(Path)) {}
	~FTemporaryFileGuard()
	{
		if (!Released)
		{
			std::error_code Ignored;
			std::filesystem::remove(FilePath, Ignored);
		}
	}

	void Release() noexcept { Released = true; }

private:
	std::filesystem::path FilePath;
	bool Released = false;
};

xr_string FormatError(const std::filesystem::path& Path, const xr_string_view Operation, const std::error_code& Error = {})
{
	xr_string Result = xr_string(Operation) + " '" +
					   ToXrString(Path.string()) + "'";
	if (Error)
	{
		Result += ": " + ToXrString(Error.message());
	}
	return Result;
}
} // namespace

FAtomicTextFileWriteResult WriteTextFileAtomically(
	const std::filesystem::path& Path, const xr_string_view Text
)
{
	FAtomicTextFileWriteResult Result;
	if (Path.empty())
	{
		Result.Error = "Cannot save an asset to an empty path.";
		return Result;
	}

	std::filesystem::path TemporaryPath = Path;
	TemporaryPath += ".tmp-" + GenerateMaterialGuid();
	FTemporaryFileGuard TemporaryFile(TemporaryPath);

	std::ofstream Output(TemporaryPath, std::ios::binary | std::ios::trunc);
	if (!Output)
	{
		Result.Error = FormatError(TemporaryPath, "Cannot create temporary asset file");
		return Result;
	}

	Output.write(Text.data(), static_cast<std::streamsize>(Text.size()));
	Output.put('\n');
	Output.close();
	if (!Output)
	{
		Result.Error = FormatError(TemporaryPath, "Failed while writing temporary asset file");
		return Result;
	}

#if defined(_WIN32)
	if (!MoveFileExW(TemporaryPath.c_str(), Path.c_str(), MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH))
	{
		const std::error_code Error(static_cast<int>(GetLastError()), std::system_category());
		Result.Error = FormatError(Path, "Cannot atomically replace asset file", Error);
		return Result;
	}
#else
	std::error_code Error;
	std::filesystem::rename(TemporaryPath, Path, Error);
	if (Error)
	{
		Result.Error = FormatError(Path, "Cannot atomically replace asset file", Error);
		return Result;
	}
#endif

	TemporaryFile.Release();
	Result.Success = true;
	return Result;
}
} // namespace Tiramisu::Editor
