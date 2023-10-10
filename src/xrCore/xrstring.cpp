#include "stdafx.h"
#pragma hdrstop

#include "xrstring.h"

#include "FS_impl.h"

XRCORE_API	extern		str_container*	g_pStringContainer	= NULL;

struct str_container_impl
{
	static const u32 buffer_size = 1024*256;
	str_value*		 buffer[buffer_size];
	int              num_docs;

	str_container_impl ()
	{
		num_docs = 0;
		ZeroMemory(buffer, sizeof(buffer));
	}

	str_value*       find   (str_value* value, const char* str)
	{
		str_value* candidate = buffer[ value->dwCRC % buffer_size ];
		while ( candidate )
		{
			if ( candidate->dwCRC == value->dwCRC &&
				 candidate->dwLength == value->dwLength &&
				 !memcmp(candidate->value, str, value->dwLength) )
			{
				return candidate;				
			}

			candidate = candidate->next;
		}

		return NULL;
	}

	void			 insert (str_value* value)
	{
		str_value** element = &buffer[ value->dwCRC % buffer_size ];
		value->next = *element;
		*element = value;
	}

	void			 clean ()
	{
		for ( u32 i=0; i<buffer_size; ++i )
		{
			str_value** current = &buffer[i];

			while ( *current != NULL )
			{
				str_value* value = *current;
				if ( !value->dwReference )
				{
					*current = value->next;
					xr_free(value);
				}
				else
				{
					current = &value->next;
				}
			}
		}
	}

	void			 verify ()
	{
		Msg			("strings verify started");
		for ( u32 i=0; i<buffer_size; ++i )
		{
			str_value* value = buffer[i];
			while ( value )
			{
				u32			crc		= crc32	(value->value, value->dwLength);
				string32	crc_str;
				R_ASSERT3	(crc==value->dwCRC, "CorePanic: read-only memory corruption (shared_strings)", _itoa(value->dwCRC,crc_str,16));
				R_ASSERT3	(value->dwLength == xr_strlen(value->value), "CorePanic: read-only memory corruption (shared_strings, internal structures)", value->value);
				value = value->next;
			}
		}
		Msg			("strings verify completed");
	}

	void			dump (FILE* f) const
	{
		for ( u32 i=0; i<buffer_size; ++i )
		{
			str_value* value = buffer[i];
			while ( value )
			{
				fprintf	(f,"ref[%4d]-len[%3d]-crc[%8X] : %s\n",value->dwReference,value->dwLength,value->dwCRC,value->value);
				value = value->next;
			}
		}
	}

	void			dump (IWriter* f) const
	{
		for ( u32 i=0; i<buffer_size; ++i )
		{
			str_value* value = buffer[i];
			string4096		temp;
			while ( value )
			{
				xr_sprintf	(temp, sizeof(temp), "ref[%4d]-len[%3d]-crc[%8X] : %s\n", value->dwReference, value->dwLength, value->dwCRC, value->value);
				f->w_string	(temp);
				value		= value->next;
			}
		}
	}

	int				stat_economy ()
	{
		int				counter	  = 0;
		for ( u32 i=0; i<buffer_size; ++i )
		{
			str_value* value = buffer[i];
			while ( value )
			{
				counter -= sizeof(str_value);
				counter += (value->dwReference-1)*(value->dwLength+1);
				value = value->next;
			}
		}

		return counter;
	}
};

str_container::str_container ()
{
	impl = xr_new<str_container_impl>();
}

str_value*	str_container::dock		(str_c value)
{
	if (0==value)				return 0;

	cs.Enter					();

	str_value*	result			= 0	;

	// calc len
	u32		s_len				= xr_strlen(value);
	u32		s_len_with_zero		= (u32)s_len+1;
	VERIFY	(sizeof(str_value) + s_len_with_zero < 4096);

	// setup find structure
	char	header[sizeof(str_value)];
	str_value*	sv				= (str_value*)header;
	sv->dwReference				= 0;
	sv->dwLength				= s_len;
	sv->dwCRC					= crc32	(value,s_len);

	// search
	result						= impl->find	(sv, value);
	
#ifdef DEBUG
	bool is_leaked_string = !xr_strcmp(value, "enter leaked string here");
#endif //DEBUG

	// it may be the case, string is not found or has "non-exact" match
	if (0==result 
#ifdef DEBUG
		|| is_leaked_string
#endif //DEBUG
		) {

		result					= (str_value*) Memory.mem_alloc(sizeof(str_value) + s_len_with_zero);

#ifdef DEBUG
		static int num_leaked_string = 0;
		if ( is_leaked_string )
		{
			++num_leaked_string;
			Msg("leaked_string: %d 0x%08x", num_leaked_string, result);
		}
#endif // DEBUG

		result->dwReference		= 0;
		result->dwLength		= sv->dwLength;
		result->dwCRC			= sv->dwCRC;
		CopyMemory				(result->value,value,s_len_with_zero);

		impl->insert (result);
	}
	cs.Leave					();

	return	result;
}

void		str_container::clean	()
{
	cs.Enter	();
	impl->clean ();
	cs.Leave	();
}

void		str_container::verify	()
{
	cs.Enter	();
	impl->verify();
	cs.Leave	();
}

void		str_container::dump	()
{
 	cs.Enter	();

 	FILE* F;
 	fopen_s(&F, "d:\\$str_dump$.txt","w");

 	impl->dump  (F);
 	fclose		(F);
 	cs.Leave	();
}

void		str_container::dump	(IWriter* W)
{
 	cs.Enter	();
 	impl->dump  (W);
 	cs.Leave	();
}

u32			str_container::stat_economy		()
{
 	cs.Enter	();
 	int				counter	= 0;
 	counter			-= sizeof(*this);
	counter			+= impl->stat_economy();
 	cs.Leave		(); 
 	return			u32(counter);
}

str_container::~str_container		()
{
	clean ();
	//dump ();
	xr_delete(impl);
}
