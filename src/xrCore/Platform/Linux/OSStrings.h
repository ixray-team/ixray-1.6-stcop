#pragma once
#include <ctype.h>

#define _stricmp stricmp
#define strcmpi stricmp
#define lstrcpy strcpy
#define stricmp strcasecmp

#define _vsnprintf vsnprintf
#define vsprintf_s(dest, size, format, args) vsprintf(dest, format, args)

inline char* _strlwr(char *str)
{
  unsigned char *p = (unsigned char *)str;
  while (*p) {
     *p = tolower((unsigned char)*p);
      p++;
  }
  return str;
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
