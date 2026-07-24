#pragma once

#include "TiramisuMaterialEditorTypes.h"

#include <filesystem>
#include <string>
#include <string_view>

namespace Tiramisu::Editor
{
// Результат атомарной записи текстового editor asset.
struct FAtomicTextFileWriteResult
{
    bool Success = false;
    xr_string Error;
};

// Пишет временный файл рядом с назначением и заменяет оригинал только после
// успешного закрытия; ошибка не повреждает существующий asset.
[[nodiscard]] FAtomicTextFileWriteResult WriteTextFileAtomically(
    const std::filesystem::path& Path, xr_string_view Text);
} // namespace Tiramisu::Editor
