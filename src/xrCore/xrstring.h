#pragma once

IC bool IsUTF8(const char* string)
{
	if (!string)
		return true;

	const unsigned char* bytes = (const unsigned char*)string;
	int num;
	while (*bytes != 0x00)
	{
		if ((*bytes & 0x80) == 0x00)
		{
			// U+0000 to U+007F
			num = 1;
		}
		else if ((*bytes & 0xE0) == 0xC0)
		{
			// U+0080 to U+07FF
			num = 2;
		}
		else if ((*bytes & 0xF0) == 0xE0)
		{
			// U+0800 to U+FFFF
			num = 3;
		}
		else if ((*bytes & 0xF8) == 0xF0)
		{
			// U+10000 to U+10FFFF
			num = 4;
		}
		else
			return false;
		bytes += 1;
		for (int i = 1; i < num; ++i)
		{
			if ((*bytes & 0xC0) != 0x80)
				return false;
			bytes += 1;
		}
	}
	return true;
}

class xr_string;

class XRCORE_API xr_string : public std::basic_string<char, std::char_traits<char>, xalloc<char>>
{
public:
	typedef std::basic_string<char, std::char_traits<char>, xalloc<char>> Super;

	xr_string() = default;
	xr_string(const xr_string& other) = default;
	xr_string(xr_string&& other) noexcept = default;

	xr_string(LPCSTR Str);
	xr_string(LPCSTR Str, u32 Size);
	xr_string(Super&& other);

	xr_string& operator=(LPCSTR Str);
	xr_string& operator=(const Super& other);

	xr_string& operator=(const xr_string& other) = default;
	xr_string& operator=(xr_string&& other) = default;

	template <size_t ArrayLenght>
	xr_string(char* (&InArray)[ArrayLenght]);

	xr_vector<xr_string> Split(char splitCh) const;
	xr_vector<xr_string> Split(u32 NumberOfSplits, ...) const;

	bool StartWith(const xr_string& Other) const;
	bool StartWith(LPCSTR Str) const;
	bool StartWith(LPCSTR Str, size_t Size) const;

	bool Contains(const xr_string& SubStr) const;
	bool EqualWithCaseInsensitive(const xr_string& SubStr) const;

	xr_string RemoveWhitespaces() const;

	static xr_string ToString(int Value);
	static xr_string ToString(unsigned int Value);
	static xr_string ToString(float Value);
	static xr_string ToString(double Value);
	static xr_string ToString(const Fvector& Value);
	static xr_string ToString(const Dvector& Value);

	using xrStringVector = xr_vector<xr_string>;
	static xr_string Join(xrStringVector::iterator beginIter, xrStringVector::iterator endIter, const char delimeter = '\0');
};

using SStringVec = xr_vector<xr_string>;
using SStringVecIt = SStringVec::iterator;

namespace std 
{
	template<>
	class hash<xr_string> 
	{
	public:
		using is_transparent = void;
		using hash_type = std::hash<std::string_view>;
	public:
		size_t operator()(const xr_string& s) const 
		{
			return hash_type{}(s);
		}

		size_t operator()(std::string_view s) const
		{
			return hash_type{}(s);
		}
	};
}

IC void	xr_strlwr(xr_string& src) { for (xr_string::iterator it = src.begin(); it != src.end(); it++) *it = xr_string::value_type(tolower(*it)); }

// xr_string: template magic
template<size_t ArrayLenght>
inline xr_string::xr_string(char* (&InArray)[ArrayLenght])
{
	assign(InArray, ArrayLenght);
}

// warning
// this function can be used for debug purposes only
template<typename String, typename... Args>
IC String make_string(const char* format, Args... args)
{
	static constexpr size_t bufferSize = 4096;
	static char temp[bufferSize];
	snprintf(temp, bufferSize, format, args...);
	return temp;
}

// FX: Hash str container
// Support: xr_string and shared_str
template<typename Key, typename Value>
using xr_string_map = std::unordered_map<Key, Value, std::hash<Key>, std::equal_to<>>;