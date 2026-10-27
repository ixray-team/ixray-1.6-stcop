#pragma once

enum class EEngineExternalBalance
{
	Auto,
	ShadowOfChernobyl,
	ClearSky,
	CallOfPripyat,
	Unknown
};

namespace EngineBalance
{
	constexpr bool NameEquals(const char* Left, const char* Right)
	{
		if (!Left || !Right)
		{
			return false;
		}
		while (*Left && *Left == *Right)
		{
			++Left;
			++Right;
		}
		return *Left == *Right;
	}

	constexpr EEngineExternalBalance Parse(const char* Name)
	{
		if (NameEquals(Name, "auto"))
		{
			return EEngineExternalBalance::Auto;
		}
		if (NameEquals(Name, "soc"))
		{
			return EEngineExternalBalance::ShadowOfChernobyl;
		}
		if (NameEquals(Name, "cs"))
		{
			return EEngineExternalBalance::ClearSky;
		}
		if (NameEquals(Name, "cop"))
		{
			return EEngineExternalBalance::CallOfPripyat;
		}
		return EEngineExternalBalance::Unknown;
	}

	constexpr EEngineExternalBalance Resolve(EEngineExternalBalance Balance, EEngineExternalBalance Fallback)
	{
		return Balance == EEngineExternalBalance::Auto ? Fallback : Balance;
	}
}
