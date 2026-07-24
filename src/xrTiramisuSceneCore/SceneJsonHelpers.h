#pragma once

#include "TiramisuSceneTypes.h"

#include <nlohmann/json.hpp>

namespace nlohmann
{
template <>
// Адаптер nlohmann JSON для xr_string на границе scene serialization.
struct adl_serializer<xr_string>
{
    static void to_json(json& JsonValue, const xr_string& Value)
    {
        JsonValue = std::string(Value.data(), Value.size());
    }

    static void from_json(const json& JsonValue, xr_string& Value)
    {
        const auto& StringValue = JsonValue.get_ref<const json::string_t&>();
        Value.assign(StringValue.data(), StringValue.size());
    }
};
} // namespace nlohmann
