#pragma once

/*
*
* Author: wh1t3lord
* Description: fast-stack char/wchar string in one class with std::string API reference implementation
* Date: 16.01.2025
*
*/

#include <string.h>
#include <iterator>
#include <cassert>

template<typename char_t, unsigned int _kStringLength>
class stack_string
{
public:
	using value_type = char_t;
	using pointer = char_t*;
	using reference = char_t&;
	using const_pointer = const pointer;

	using number_type = decltype(_kStringLength);

	static_assert(std::is_same<char, char_t>::value || std::is_same<wchar_t, char_t>::value, "unsupported char format, report to developers (maybe you need it, but at least write your problem)");
	static_assert(_kStringLength != number_type(-1), "you can't pass a negative value for instatiation!");
	static_assert(_kStringLength > 0, "you can't make a arr as zero lol");
	static_assert(std::is_signed<number_type>::value == false, "must be unsigned type of length variable");

public:
	stack_string()
	{
		// if you do {} <= initializes like memset(buf,0,sizeof(buf)) not fast, this is faster initialization!
		// https://godbolt.org/z/9cc833e1W
		m_buffer[0] = char_t(0);
	}

	~stack_string() {}


	// const
	inline const char_t* c_str(void) const { return &m_buffer[0]; }
	inline constexpr number_type max_size(void) const { return sizeof(m_buffer) / sizeof(char_t); }
	inline bool empty(void) const { return m_buffer[0] == char_t(0); }
	inline value_type at(number_type index) const
	{
		assert(index >= 0 && index <= this->max_size() && "out of bounds");
		assert(index != number_type(-1) && "invalid value");

		// instead of throwing (aka traditional std::string design) we just make it safe...
		if (index > this->max_size())
			return char_t(0);
		else
			return m_buffer[index];
	}

	// non-const
	inline void clear(void) { m_buffer[0] = char_t(0); }
	inline pointer data(void) { return &m_buffer[0]; }
	inline number_type size(void)
	{
		if constexpr (std::is_same<char,char_t>::value)
		{
			return strlen(m_buffer);
		}

		if constexpr (std::is_same<wchar_t, char_t>::value)
		{
			return wcslen(m_buffer);
		}
	}

	inline reference at(number_type index) 
	{
		assert(index >= 0 && index <= this->max_size() && "out of bounds");
		assert(index != number_type(-1) && "invalid value");

		if (index > this->max_size())
			index = 0;

		return m_buffer[index];
	}

private:
	char_t m_buffer[_kStringLength];
};

static_assert(sizeof(stack_string<char, 1>) == sizeof(char[1]), "you can't add any additional field to this class! pure buffer on stack... (there's no point in reducing counting operations and caching like size of buffer and etc)");