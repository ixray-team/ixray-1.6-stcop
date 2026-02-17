#pragma once

#include "_std_c_undefs.h"

// token type definition
struct XRCORE_API xr_token
{
	LPCSTR	name;
	int 	id;
};

ICF LPCSTR get_token_name(xr_token* tokens, int key)
{
    for (int k=0; tokens[k].name; k++)
    	if (key==tokens[k].id) return tokens[k].name;
    return "";
}

ICF int get_token_id(xr_token* tokens, LPCSTR key)
{
    for (int k=0; tokens[k].name; k++)
    	if ( _stricmp(tokens[k].name,key)==0 ) 
			return tokens[k].id;
    return -1;
}

struct XRCORE_API xr_token2
{
	LPCSTR	name;
	LPCSTR	info;
	int 	id;
};

template<typename T>
consteval T bit_lshift(T n) { return T{ 1 } << n; }

template<typename T>
consteval T bit_rshift(T value, T n) { return value >> n; }

// generic
template <class T>	ICF T		_sqr	(T a)		{ return a*a;		}

// float
ICF float	_sqrt_sse	(float x)
{
    return _mm_cvtss_f32(_mm_sqrt_ss(_mm_set_ps1(x)));
}
ICF float	_sqrt	(float x)		{ return sqrtf(x); }

// check for: Signaling NaN, Quiet NaN, Negative infinity ( �INF), Positive infinity (+INF), Negative denormalized, Positive denormalized
ICF BOOL _valid(const float x)
{
	const int cls = std::fpclassify(x);
	switch (cls)
	{
	case FP_NAN:
	case FP_INFINITE:
	case FP_SUBNORMAL:
		return false;
	default:
		break;
	}

	return true;
}

// check for: Signaling NaN, Quiet NaN, Negative infinity ( �INF), Positive infinity (+INF), Negative denormalized, Positive denormalized
ICF BOOL _valid(const double x)
{
	const int cls = std::fpclassify(x);
	switch (cls)
	{
	case FP_NAN:
	case FP_INFINITE:
	case FP_SUBNORMAL:
		return false;
	default:
		break;
	}

	return true;
}

ICF u32							xr_strlen				( const char* S );

// string management

// return pointer to ".ext"
ICF char*						strext					( const char* S )
{	return (char*) strrchr(S,'.');	}

ICF u32							xr_strlen				( const char* S )
{	return (u32)strlen(S);			}

ICF char*						xr_strlwr				(char* S)
{	return _strlwr(S);				}

ICF int							xr_strcmp				( const char* S1, const char* S2 )
{	return (int)strcmp(S1,S2);  }

ICF int							xr_strncmp				( const char* S1, const char* S2, int n )
{	return (int)strncmp(S1,S2,n);	}

ICF errno_t xr_strcpy	( LPSTR destination, size_t const destination_size, LPCSTR source )
{
	return						strncpy_s( destination, destination_size, source, destination_size );
}

ICF errno_t xr_strcat		( LPSTR destination, size_t const buffer_size, LPCSTR source )
{
	size_t const destination_length	= xr_strlen(destination);
	LPSTR i						= destination + destination_length;
	LPSTR const e				= destination + buffer_size - 1;
	if ( i > e )
		return					0;

	for ( LPCSTR j = source; *j && (i != e); ++i, ++j )
		*i						= *j;

	*i							= 0;
	return						0;
}

ICF int __cdecl xr_sprintf	( LPSTR destination, size_t const buffer_size, LPCSTR format_string, ... )
{
	va_list args;
	va_start					( args, format_string);
	return						vsnprintf_s( destination, buffer_size, buffer_size - 1, format_string, args );
}

template <int count>
ICF int __cdecl xr_sprintf	( char (&destination)[count], LPCSTR format_string, ... )
{
	va_list args;
	va_start					( args, format_string);
	return						vsnprintf_s( destination, count, count - 1, format_string, args );
}

template <int count>
ICF errno_t xr_strcpy	( char (&destination)[count], LPCSTR source )
{
	return						xr_strcpy( destination, count, source );
}

template <int count>
ICF errno_t xr_strcat	( char (&destination)[count], LPCSTR source )
{
	return						xr_strcat( destination, count, source );
}

XRCORE_API	char*				timestamp				(string64& dest);

extern XRCORE_API u32			crc32					(const void* P, size_t len);
extern XRCORE_API u32			crc32					(const void* P, size_t len, u32 starting_crc);
extern XRCORE_API u32			path_crc32				(const char* path, size_t len); // ignores '/' and '\'

XRCORE_API bool NaturalCompare(const char* a, const char* b);