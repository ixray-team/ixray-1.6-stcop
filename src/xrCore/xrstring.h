#pragma once
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

	xr_vector<xr_string> Split(char splitCh);
	xr_vector<xr_string> Split(u32 NumberOfSplits, ...);

	bool StartWith(const xr_string& Other) const;
	bool StartWith(LPCSTR Str) const;
	bool StartWith(LPCSTR Str, size_t Size) const;
	xr_string RemoveWhitespaces() const;

	static xr_string ToString(int Value);
	static xr_string ToString(unsigned int Value);
	static xr_string ToString(float Value);
	static xr_string ToString(double Value);
	static xr_string ToString(const Fvector& Value);
	static xr_string ToString(const Dvector& Value);

	using xrStringVector = xr_vector<xr_string>;
	static xr_string Join(xrStringVector::iterator beginIter, xrStringVector::iterator endIter, const char delimeter = '\0');

	template<typename StringType>
	static void FixSlashes(StringType& str);

	template<>
	static void FixSlashes<xr_string>(xr_string& InStr);
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

template<typename StringType>
inline void xr_string::FixSlashes(StringType& str)
{
	// Should be array of chars
	static_assert(std::is_same<std::remove_extent<StringType>::type, char>::value);

	constexpr size_t sizeArray = sizeof(str);

	for (int i = 0; i < sizeArray; ++i)
	{
		if (str[i] == '/')
		{
			str[i] = '\\';
		}
	}
}

template<>
inline void xr_string::FixSlashes(xr_string& InStr)
{
	for (size_t i = 0; i < InStr.size(); ++i)
	{
		if (InStr[i] == '/')
		{
			InStr[i] = '\\';
		}
	}
}

// FX: Hash str container
// Support: xr_string and shared_str
template<typename Key, typename Value>
using xr_string_map = std::unordered_map<Key, Value, std::hash<Key>, std::equal_to<>>;