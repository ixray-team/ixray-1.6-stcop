#pragma once
#include <json/json.hpp>

#ifdef XRGAME_EXPORTS
#	error Don't use in Game code! Only for utils!
#endif

namespace XRay::Concepts
{
	template <typename T>
	concept CharPtr = std::is_pointer_v<T> && std::same_as<std::remove_pointer_t<T>, char>;

	template <typename T>
	concept CharArray = std::is_array_v<T> && std::same_as<std::remove_extent_t<T>, char>;

	template <typename T>
	concept XRayString = std::same_as<std::remove_cvref_t<T>, xr_string> || std::same_as<std::remove_cvref_t<T>, shared_str>;

	template <typename T>
	concept Arithmetic = std::integral<T> || std::floating_point<T>;
}

class XRCORE_API CJsonSerializer
{
private:
	string_path jfn;
	nlohmann::json  JSONData = {};

public:
	CJsonSerializer(shared_str FileName);
	~CJsonSerializer();
	void Save() const;

	template <XRay::Concepts::Arithmetic T>
	void Read(shared_str Section, T& Value) const
	{
		if (JSONData.contains(*Section))
		{
			Value = JSONData[*Section].get<T>();
		}
	}

	template <XRay::Concepts::XRayString T>
	void Read(shared_str Section, T& Value) const
	{
		if (JSONData.contains(*Section))
		{
			std::string tmp = JSONData[*Section].get<std::string>();
			Value = tmp.c_str();
		}
	}

	template <XRay::Concepts::CharArray T>
	void Read(shared_str Section, T& Value) const
	{
		if (JSONData.contains(*Section))
		{
			constexpr size_t N = std::extent_v<T>;
			std::string tmp = JSONData[*Section].get<std::string>();
			std::strncpy(Value, tmp.c_str(), N - 1);
			Value[N - 1] = '\0';
		}
	}

	template <XRay::Concepts::CharPtr T>
	void Read(shared_str Section, T& Value) const
	{
		if (JSONData.contains(*Section))
		{
			std::string tmp = JSONData[*Section].get<std::string>();
			std::strcpy(Value, tmp.c_str());
		}
	}

	template <XRay::Concepts::Arithmetic T>
	void Write(shared_str Section, T Value)
	{
		JSONData[*Section] = Value;
	}

	template <XRay::Concepts::XRayString T>
	void Write(shared_str Section, const T& Value)
	{
		JSONData[*Section] = Value;
	}

	template <size_t N>  requires (N > 0)
	void Write(shared_str Section, const char(&Value)[N])
	{
		JSONData[*Section] = std::string(Value, strnlen(Value, N));
	}

	void Write(shared_str Section, const char* Value)
	{
		JSONData[*Section] = Value;
	}
};