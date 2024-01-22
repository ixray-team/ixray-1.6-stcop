#pragma once
#include <ctype.h>
#include <cstdlib>

#define _stricmp stricmp
#define strcmpi stricmp
#define lstrcpy strcpy
#define stricmp strcasecmp
#define _vsnprintf vsnprintf
#define vsprintf_s(dest, size, format, args) vsprintf(dest, format, args)

template<typename ValType>
inline const char* itoa(ValType val)
{
    static std::string temp;
    temp = std::to_string(val).c_str();
    return temp.c_str();
}

template<typename ValType>
inline void itoa(ValType val, char* str, [[maybe_unused]]size_t size)
{
    strcpy(str, itoa(val));
}

#define _itoa itoa
inline char* _strlwr(char *str)
{
  unsigned char *p = (unsigned char *)str;
  while (*p) {
     *p = tolower((unsigned char)*p);
      p++;
  }
  return str;
}

inline char* _strupr(char *string)
{
    char* hold = string;
    while (*string)
    {
        *string = (char)toupper(*string);
        string++;
    }

    return hold;
}

inline int strcpy_s(char *dest, size_t num, const char *source)
{
    if(!dest)
        return EINVAL;

    if(0 == num)
    {
        dest[0] = '\0';
        return ERANGE;
    }

    if(!source)
    {
        dest[0] = '\0';
        return EINVAL;
    }

    size_t i;
    for(i = 0; i < num; i++)
    {
        if((dest[i] = source[i]) == '\0')
            return 0;
    }
    dest[0] = '\0';
    return ERANGE;
}

inline int strncpy_s(char * dest, size_t dst_size, const char * source, size_t num)
{
    if (!dest || (0 == dst_size))
        return EINVAL;

    if(0 == num)
    {
        dest[0] = '\0';
        return 0;
    }

    if (!source)
    {
        dest[0] = '\0';
        return EINVAL;
    }

    size_t i, end;
    if(num < dst_size)
        end = num;
    else
        end = dst_size - 1;

    for(i = 0; i < end && source[i]; i++)
        dest[i] = source[i];

    if(!source[i] || end == num)
    {
        dest[i] = '\0';
        return 0;
    }

    dest[0] = '\0';

    return EINVAL;
}

inline int strcat_s(char * dest, size_t num, const char * source)
{
    if(!dest)
        return EINVAL;

    if(!source)
    {
        dest[0] = '\0';
        return EINVAL;
    }

    size_t i, j;
    for(i = 0; i < num; i++)
    {
        if(dest[i] == '\0')
        {
            for(j = 0; (j + i) < num; j++)
            {
                if((dest[j + i] = source[j]) == '\0')
                    return 0;
            }
        }
    }

    dest[0] = '\0';
    return ERANGE;
}

inline int vsnprintf_s(char* buffer, size_t size, size_t, const char* format, va_list list)
{
    //TODO add bound check
    return vsnprintf(buffer, size, format, list);
}

IC bool IsCharAlphaNumeric(char ch)
{
    return iswctype (ch, wctype("upper") | wctype("lower") | wctype("digit"));
}

#include <stdint.h>
#define MSVCRT_EINVAL	22
#define MSVCRT_ERANGE	34

#define MSVCRT_UI64_MAX   (((uint64_t)0xffffffff << 32) | 0xffffffff)

/**
 * from wine@dlls/msvcrt/string.c
 *
 * @param fileName
 * @param readOnly
 * @return
 */
IC int _cdecl _i64toa_s(int64_t value, char *str, size_t size, int radix)
{
    uint64_t val;
    unsigned int digit;
    int is_negative;
    char buffer[65], *pos;
    size_t len;

    if (!(str != NULL))
        return MSVCRT_EINVAL;
    if (!(size > 0))
        return MSVCRT_EINVAL;
    if (!(radix >= 2 && radix <= 36))
    {
        str[0] = '\0';
        return MSVCRT_EINVAL;
    }

    if (value < 0 && radix == 10)
    {
        is_negative = 1;
        val = -value;
    }
    else
    {
        is_negative = 0;
        val = value;
    }

    pos = buffer + 64;
    *pos = '\0';

    do
    {
        digit = val % radix;
        val /= radix;

        if (digit < 10)
            *--pos = '0' + digit;
        else
            *--pos = 'a' + digit - 10;
    } while (val != 0);

    if (is_negative)
        *--pos = '-';

    len = buffer + 65 - pos;
    if (len > size)
    {
        size_t i;
        char *p = str;

    /* Copy the temporary buffer backwards up to the available number of
     * characters. Don't copy the negative sign if present. */

        if (is_negative)
        {
            p++;
            size--;
        }

        for (pos = buffer + 63, i = 0; i < size; i++)
            *p++ = *pos--;

        str[0] = '\0';
        return MSVCRT_ERANGE;
    }

    memcpy(str, pos, len);
    return 0;
}

IC int _cdecl _ui64toa_s(uint64_t value, char *str, size_t size, int radix)
{
    char buffer[65], *pos;
    int digit;

    if (!(str != NULL))
        return MSVCRT_EINVAL;
    if (!(size > 0))
        return MSVCRT_EINVAL;

    if (!(radix >= 2 && radix <= 36))
    {
        str[0] = '\0';
        return MSVCRT_EINVAL;
    }

    pos = buffer + 64;
    *pos = '\0';

    do
    {
    digit = value % radix;
    value /= radix;

    if (digit < 10)
    *--pos = '0' + digit;
    else
    *--pos = 'a' + digit - 10;
    } while (value != 0);

    if (static_cast<size_t>(buffer - pos + 65) > size)
    {
        return MSVCRT_EINVAL;
    }

    memcpy(str, pos, buffer - pos + 65);
    return 0;
}


using LARGE_INTEGER = long long int;
using ULARGE_INTEGER = unsigned long long int;

IC LARGE_INTEGER _cdecl _atoi64(const char *str)
{
    ULARGE_INTEGER RunningTotal = 0;
    char bMinus = 0;

    while (*str == ' ' || (*str >= '\011' && *str <= '\015'))
    {
        str++;
    } /* while */

    if (*str == '+')
        str++;
    else if (*str == '-')
    {
        bMinus = 1;
        str++;
    } /* if */

    while (*str >= '0' && *str <= '9')
    {
        RunningTotal = RunningTotal * 10 + *str - '0';
        str++;
    } /* while */

    return bMinus ? -RunningTotal : RunningTotal;
}

IC uint64_t _cdecl _strtoui64_l(const char *nptr, char **endptr, int base, locale_t locale)
{
    bool negative = false;
    uint64_t ret = 0;

    if (!(nptr != NULL))
        return 0;
    if (!(base == 0 || base >= 2))
        return 0;
    if (!(base <= 36))
        return 0;

    while (isspace(*nptr))
        nptr++;

    if (*nptr == '-')
    {
        negative = true;
        nptr++;
    }
    else if (*nptr == '+')
        nptr++;

    if ((base == 0 || base == 16) && *nptr == '0' && tolower(*(nptr + 1)) == 'x')
    {
        base = 16;
        nptr += 2;
    }

    if (base == 0)
    {
        if (*nptr == '0')
            base = 8;
        else
            base = 10;
    }

    while (*nptr)
    {
        char cur = tolower(*nptr);
        int v;

        if (isdigit(cur))
        {
            if (cur >= '0' + base)
                break;
            v = *nptr - '0';
        }
        else
        {
            if (cur < 'a' || cur >= 'a' + base - 10)
                break;
            v = cur - 'a' + 10;
        }

        nptr++;

        if (ret > MSVCRT_UI64_MAX / base || ret * base > MSVCRT_UI64_MAX - v)
        ret = MSVCRT_UI64_MAX;
        else
        ret = ret * base + v;
    }

    if (endptr)
        *endptr = (char*) nptr;

    return negative ? -ret : ret;
}

IC uint64_t _strtoui64(const char *nptr, char **endptr, int base)
{
    return _strtoui64_l(nptr, endptr, base, NULL);
}

IC char* _strlwr_s(char* strDestination, size_t sizeInBytes)
{
    return _strlwr(strDestination);
}