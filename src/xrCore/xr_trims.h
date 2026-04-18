#pragma once

// refs
struct xr_token;

XRCORE_API int		    	_GetItemCount			( const char* , char separator=',');
XRCORE_API LPSTR	    	_GetItem				( const char*, int, LPSTR, u32 const dst_size, char separator=',', const char* ="", bool trim=true );

template <int count>
inline LPSTR	    		_GetItem				( const char* src, int index, char (&dst)[count], char separator=',', const char* def="", bool trim=true )
{
	return					_GetItem(src,index,dst,count,separator,def,trim);
}

XRCORE_API LPSTR	    	_GetItems				( const char*, int, int, LPSTR, char separator=',');
XRCORE_API const char*	    	_SetPos					( const char* src, u32 pos, char separator=',' );
XRCORE_API const char*	    	_CopyVal				( const char* src, LPSTR dst, char separator=',' );
XRCORE_API LPSTR	    	_Trim					( LPSTR str );
XRCORE_API LPSTR	    	_TrimLeft				( LPSTR str );
XRCORE_API LPSTR	    	_TrimRight				( LPSTR str );
XRCORE_API LPSTR	    	_ChangeSymbol			( LPSTR name, char src, char dest );
XRCORE_API u32		    	_ParseItem				( const char* src, xr_token* token_list );
XRCORE_API u32		    	_ParseItem				( LPSTR src, int ind, xr_token* token_list );
XRCORE_API LPSTR 	    	_ReplaceItem 			( const char* src, int index, const char* new_item, LPSTR dst, char separator );
XRCORE_API LPSTR 	    	_ReplaceItems 			( const char* src, int idx_start, int idx_end, const char* new_items, LPSTR dst, char separator );
XRCORE_API void 	    	_SequenceToList			( LPSTRVec& lst, const char* in, char separator=',' );
XRCORE_API void 			_SequenceToList			( RStringVec& lst, const char* in, char separator=',' );
XRCORE_API void 			_SequenceToList			( SStringVec& lst, const char* in, char separator=',' );

XRCORE_API xr_string& 		_Trim					( xr_string& src );
XRCORE_API xr_string& 		_TrimLeft				( xr_string& src );
XRCORE_API xr_string&		_TrimRight				( xr_string& src );
XRCORE_API xr_string&   	_ChangeSymbol			( xr_string& name, char src, char dest );
XRCORE_API const char*		 	_CopyVal 				( const char* src, xr_string& dst, char separator=',' );
XRCORE_API const char*			_GetItem				( const char* src, int, xr_string& p, char separator=',', const char* ="", bool trim=true );
XRCORE_API xr_string		_ListToSequence			( const SStringVec& lst );
XRCORE_API shared_str		_ListToSequence			( const RStringVec& lst );