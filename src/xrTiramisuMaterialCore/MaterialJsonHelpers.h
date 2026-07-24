#pragma once

#include "TiramisuMaterialCoreTypes.h"

#include <nlohmann/json.hpp>

#include <cmath>
#include <cstdint>
#include <limits>
#include <string>
#include <string_view>

namespace nlohmann
{
template <>
// Адаптер nlohmann JSON для строки движка без утечки std::string в публичный API.
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

namespace MaterialJsonDetail
{
using Json = nlohmann::json;

inline const Json* Find(const Json& Object, const xr_string_view Name)
{
	if (!Object.is_object())
	{
		return nullptr;
	}

	const auto Iterator = Object.find(std::string(Name));
	return Iterator == Object.end() ? nullptr : &*Iterator;
}

inline bool TryGetString(const Json& Value, xr_string& Result)
{
	if (!Value.is_string())
	{
		return false;
	}

	Result = Value.get_ref<const Json::string_t&>();
	return true;
}

inline bool TryGetBoolean(const Json& Value, bool& Result) noexcept
{
	if (!Value.is_boolean())
	{
		return false;
	}

	Result = Value.get_ref<const Json::boolean_t&>();
	return true;
}

inline bool TryGetFloat(const Json& Value, float& Result) noexcept
{
	double Number = 0.0;
	if (Value.is_number_float())
	{
		Number = Value.get_ref<const Json::number_float_t&>();
	}
	else if (Value.is_number_unsigned())
	{
		Number = static_cast<double>(Value.get_ref<const Json::number_unsigned_t&>());
	}
	else if (Value.is_number_integer())
	{
		Number = static_cast<double>(Value.get_ref<const Json::number_integer_t&>());
	}
	else
	{
		return false;
	}

	if (!std::isfinite(Number) || Number < -std::numeric_limits<float>::max() ||
		Number > std::numeric_limits<float>::max())
	{
		return false;
	}

	Result = static_cast<float>(Number);
	return true;
}

inline bool TryGetUInt32(const Json& Value, u32& Result) noexcept
{
	if (Value.is_number_unsigned())
	{
		const auto Number = Value.get_ref<const Json::number_unsigned_t&>();
		if (Number > std::numeric_limits<u32>::max())
		{
			return false;
		}
		Result = static_cast<u32>(Number);
		return true;
	}

	if (!Value.is_number_integer())
	{
		return false;
	}

	const auto Number = Value.get_ref<const Json::number_integer_t&>();
	if (Number < 0 || static_cast<u64>(Number) > std::numeric_limits<u32>::max())
	{
		return false;
	}
	Result = static_cast<u32>(Number);
	return true;
}

inline bool TryGetInt32(const Json& Value, s32& Result) noexcept
{
	if (Value.is_number_unsigned())
	{
		const auto Number = Value.get_ref<const Json::number_unsigned_t&>();
		if (Number > static_cast<u64>(std::numeric_limits<s32>::max()))
		{
			return false;
		}
		Result = static_cast<s32>(Number);
		return true;
	}

	if (!Value.is_number_integer())
	{
		return false;
	}

	const auto Number = Value.get_ref<const Json::number_integer_t&>();
	if (Number < std::numeric_limits<s32>::min() || Number > std::numeric_limits<s32>::max())
	{
		return false;
	}
	Result = static_cast<s32>(Number);
	return true;
}
} // namespace MaterialJsonDetail
