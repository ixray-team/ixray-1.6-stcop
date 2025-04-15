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
	using const_reference = const char_t&;
	using const_pointer = const pointer;

	using number_type = decltype(_kStringLength);

	using iterator = char_t*;
	using const_iterator = const char_t*;
	using reverse_iterator = char_t*;
	using const_reverse_iterator = const char_t*;

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
	inline const_iterator begin() const { return this->m_buffer; }
	inline const_iterator end() const { return this->m_buffer + this->size(); }

	inline const_iterator cbegin() const { return m_buffer; }
	inline const_iterator cend() const { return m_buffer + length(); }

	inline reverse_iterator rbegin() { return m_buffer + length() - 1; }
	inline reverse_iterator rend() { return m_buffer - 1; }

	inline const_reverse_iterator rbegin() const { return m_buffer + length() - 1; }
	inline const_reverse_iterator rend() const { return m_buffer - 1; }

	inline const_reverse_iterator crbegin() const { return m_buffer + length() - 1; }
	inline const_reverse_iterator crend() const { return m_buffer - 1; }

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

	inline stack_string<char_t, _kStringLength> substr(number_type pos = 0, number_type count = number_type(-1)) const
	{
		stack_string<char_t, _kStringLength> result;

		const number_type len = this->length();

		if (pos >= len)
			return result;

		count = (count == number_type(-1) || pos + count > len) ? len - pos : count;

		result.append(this->m_buffer + pos, count);

		return result;
	}

	inline void copy(char_t* p_dest, number_type pos, number_type count) const
	{
		assert(p_dest && "don't pass invalid string please");

		if (!p_dest)
			return;

		const number_type len = this->length();

		if (pos >= len)
			return;

		count = (pos + count > len) ? len - pos : count;
		std::memcpy(p_dest, this->m_buffer + pos, count * sizeof(char_t));
		p_dest[count] = char_t();
	}

	inline number_type find(const char_t* p_str, number_type pos = 0) const 
	{
		assert(p_str && "don't pass an invalid string please");

		if constexpr (std::is_same_v<char_t, char>)
		{
			const char* found = std::strstr(this->m_buffer + pos, p_str);
			return found ? found - this->m_buffer : number_type(-1);
		}

		if constexpr (std::is_same_v<char_t, wchar_t>)
		{
			const wchar_t* found = std::wcsstr(this->m_buffer + pos, p_str);
			return found ? found - this->m_buffer : number_type(-1);
		}

		return number_type(-1);
	}

	// non-const
	inline iterator begin() { return this->m_buffer; }
	inline iterator end() { return this->m_buffer + this->size(); }

	inline void clear(void) { m_buffer[0] = char_t(0); }
	inline pointer data(void) { return &m_buffer[0]; }
	inline number_type size(void) const
	{
		if constexpr (std::is_same<char, char_t>::value)
		{
			return strlen(m_buffer);
		}

		if constexpr (std::is_same<wchar_t, char_t>::value)
		{
			return wcslen(m_buffer);
		}
	}

	inline number_type length(void) const { return this->size(); }

	inline reference at(number_type index)
	{
		assert(index >= 0 && index <= this->max_size() && "out of bounds");
		assert(index != number_type(-1) && "invalid value");

		if (index > this->max_size())
			index = 0;

		return m_buffer[index];
	}

	inline stack_string<char_t, _kStringLength>& append(const char_t* p_str)
	{
		assert(p_str && "don't pass invalid pointer! (NULL)");

		const number_type current_length = this->size();
		const number_type available_length = _kStringLength - current_length;

		if (p_str && available_length > 0)
		{
			if constexpr (std::is_same_v<char_t, char>)
			{
				std::strncat(this->m_buffer, p_str, available_length);
			}

			if constexpr (std::is_same_v<char_t, wchar_t>)
			{
				std::wcsncat(this->m_buffer, p_str, available_length);
			}
		}

		return *this;
	}

	inline void swap(const stack_string<char_t, _kStringLength>& data)
	{
		char_t temp[_kStringLength];
		constexpr number_type _kPrecalcSize = _kStringLength * sizeof(char_t);

		std::memcpy(temp, this->m_buffer, _kPrecalcSize);
		std::memcpy(this->m_buffer, data.m_buffer, _kPrecalcSize);
		std::memcpy(data.m_buffer, temp, _kPrecalcSize);
	}

	inline void push_back(char_t c)
	{
		const number_type len = this->length();

		if (len < _kStringLength)
		{
			this->m_buffer[len] = c;
			this->m_buffer[len + 1] = char_t();
		}
	}

	inline void pop_back()
	{
		const number_type len = this->length();

		if (len > 0)
		{
			this->m_buffer[len - 1] = char_t();
		}
	}

	// operators
	inline stack_string<char_t, _kStringLength>& operator+=(char_t symbol)
	{
		auto current_index = this->size();

		assert(current_index <= _kStringLength - 1 && "overflow, not good!");

		if (current_index <= _kStringLength - 1)
			this->m_buffer[current_index] = symbol;

		return *this;
	}

	inline stack_string<char_t, _kStringLength>& operator+=(const_pointer p_str)
	{
		this->append(p_str);
		return *this;
	}

	inline stack_string<char_t, _kStringLength>& operator=(const stack_string<char_t, _kStringLength>& data)
	{
		memcpy(this->m_buffer, data.m_buffer, sizeof(data.m_buffer));
		return *this;
	}

	inline reference operator[](size_t pos) { return this->m_buffer[pos]; }
	inline const_reference operator[](size_t pos) const { return this->m_buffer[pos]; }

private:
	void append(const char_t* str, size_t count) {
		const size_t current_len = length();
		const size_t available = _kStringLength - current_len;
		count = (count > available) ? available : count;

		std::memcpy(m_buffer + current_len, str, count * sizeof(char_t));
		m_buffer[current_len + count] = char_t();
	}


private:
	char_t m_buffer[_kStringLength];
};

template<typename char_t, stack_string<char, 1>::number_type _kSize>
inline bool operator==(const stack_string<char_t, _kSize>& left, const stack_string<char_t, _kSize>& right)
{
	if constexpr (std::is_same_v<char_t, char>)
	{
		return !strcmp(left.c_str(), right.c_str());
	}

	if constexpr (std::is_same_v<char_t, wchar_t>)
	{
		return !wcscmp(left.c_str(), right.c_str());
	}

	assert(false && "unsupported char type, report to developers");
	return false;
}

template<typename char_t, stack_string<char, 1>::number_type _kSize>
inline bool operator==(const stack_string<char_t, _kSize>& left, const char_t* right)
{
	assert(right && "don't compare with nullptr, no sense");

	if constexpr (std::is_same_v<char_t, char>)
	{
		return std::strncmp(left.c_str(), right, _kSize) == 0;
	}

	if constexpr (std::is_same_v<char_t, wchar_t>)
	{
		return std::wcsncmp(left.c_str(), right, _kSize) == 0;
	}
}

static_assert(sizeof(stack_string<char, 1>) == sizeof(char[1]), "you can't add any additional field to this class! pure buffer on stack... (there's no point in reducing counting operations and caching like size of buffer and etc)");