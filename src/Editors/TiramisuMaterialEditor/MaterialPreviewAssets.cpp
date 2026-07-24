#include "MaterialPreviewAssets.h"

#include <algorithm>
#include <cctype>

namespace Tiramisu::Editor
{
namespace
{
bool EqualsCaseInsensitive(const xr_string_view Left,
    const xr_string_view Right)
{
    return Left.size() == Right.size() &&
        std::ranges::equal(Left, Right, [](const char A, const char B)
        {
            return std::tolower(static_cast<unsigned char>(A)) ==
                std::tolower(static_cast<unsigned char>(B));
        });
}

bool StartsWithCaseInsensitive(const xr_string_view Text,
    const xr_string_view Prefix)
{
    return Text.size() >= Prefix.size() &&
        EqualsCaseInsensitive(Text.substr(0, Prefix.size()), Prefix);
}

bool EndsWithCaseInsensitive(const xr_string_view Text,
    const xr_string_view Suffix)
{
    return Text.size() >= Suffix.size() && EqualsCaseInsensitive(
        Text.substr(Text.size() - Suffix.size()), Suffix);
}
} // namespace

xr_string NormalizeMaterialPreviewTexturePath(const xr_string_view AssetPath)
{
    xr_string Result(AssetPath);
    std::ranges::replace(Result, '\\', '/');
    while (Result.starts_with("./"))
        Result.erase(0, 2);
    while (Result.starts_with('/'))
        Result.erase(0, 1);

    constexpr xr_string_view GameTexturesAlias = "$game_textures$/";
    constexpr xr_string_view GameDataTextures = "gamedata/textures/";
    constexpr xr_string_view Textures = "textures/";
    if (StartsWithCaseInsensitive(Result, GameTexturesAlias))
        Result.erase(0, GameTexturesAlias.size());
    else if (StartsWithCaseInsensitive(Result, GameDataTextures))
        Result.erase(0, GameDataTextures.size());
    else if (StartsWithCaseInsensitive(Result, Textures))
        Result.erase(0, Textures.size());

    for (const xr_string_view Extension : {".dds", ".tga", ".bmp", ".png"})
    {
        if (EndsWithCaseInsensitive(Result, Extension))
        {
            Result.resize(Result.size() - Extension.size());
            break;
        }
    }
    return Result;
}

xr_string_view MaterialPreviewEnvironmentAsset(
    const xr_string_view Environment) noexcept
{
    if (Environment == "Neutral")
        return "textures/sky/sky_11_cube#small";
    if (Environment == "Outdoor")
        return "textures/sky/sky_19_cube#small";
    return "textures/sky/sky_10_cube#small";
}
} // namespace Tiramisu::Editor
