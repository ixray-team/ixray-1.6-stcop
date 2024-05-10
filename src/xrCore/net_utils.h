#ifndef _INCDEF_NETUTILS_H_
#define _INCDEF_NETUTILS_H_
#pragma once

#include "client_id.h"

#pragma pack(push,1)

const	u32			NET_PacketSizeLimit	= 16*1024;

struct	NET_Buffer
{
	BYTE	data	[NET_PacketSizeLimit];
	u32		count;
};

class XRCORE_API NET_Packet
{
public:
    void            construct( const void* data, unsigned size )
                    {
                        memcpy( B.data, data, size );
                        B.count = size;
                    }
                    
	NET_Buffer		B;
	u32				r_pos;
	u32				timeReceive;
public:
	NET_Packet() = default;

	// writing - main
	IC void write_start	()				{	B.count=0;				;}
	IC void	w_begin		( u16 type	)	{	B.count=0;	w(&type, sizeof(u16));}

	IC void	w		( const void* p, u32 count )
	{
		VERIFY		(p && count);
		VERIFY		(B.count + count < NET_PacketSizeLimit);
		CopyMemory(&B.data[B.count],p,count);
		B.count		+= count;
		VERIFY		(B.count<NET_PacketSizeLimit);
	}
	void w_seek	(u32 pos, const void* p, u32 count);
	IC u32	w_tell	()						{ return B.count; }

	// read/write operators
	template <typename T>
	void operator<<(const T& value)
	{
		w(&value, (u32)sizeof(T));
	}

	template <typename T>
	void operator>>(T& value)
	{
		r(&value, (u32)sizeof(T));
	}

	// writing - utilities
	IC void	w_float(float a) { w(&a, 4); }			// float
	IC void w_vec3(const Fvector& a) { w(&a, 3 * sizeof(float)); }			// vec3
	IC void w_vec4(const Fvector4& a) { w(&a, 4 * sizeof(float)); }			// vec4
	IC void w_u64(u64 a) { w(&a, 8); }			// qword (8b)
	IC void w_s64(s64 a) { w(&a, 8); }			// qword (8b)
	IC void w_u32(u32 a) { w(&a, 4); }			// dword (4b)
	IC void w_s32(s32 a) { w(&a, 4); }			// dword (4b)
	IC void w_u16(u16 a) { w(&a, 2); }			// word (2b)
	IC void w_s16(s16 a) { w(&a, 2); }			// word (2b)
	IC void	w_u8(u8 a) { w(&a, 1); }			// byte (1b)
	IC void	w_s8(s8 a) { w(&a, 1); }			// byte (1b)

	IC void w_float_q16	( float a, float min, float max)
	{
		VERIFY		(a>=min && a<=max);
		float q		= (a-min)/(max-min);
		u16 Value = u16(iFloor(q * 65535.f + 0.5f));
		w(&Value, sizeof(u16));
	}
	IC void w_float_q8	( float a, float min, float max)
	{
		VERIFY		(a>=min && a<=max);
		float q		= (a-min)/(max-min);

		u8 Value = u8(iFloor(q * 255.f + 0.5f));
		w(&Value, sizeof(u8));
	}
	IC void w_angle16	( float a		)	{	w_float_q16	(angle_normalize(a),0,PI_MUL_2);}
	IC void w_angle8	( float a		)	{w_float_q8	(angle_normalize(a),0,PI_MUL_2);	}
	IC void w_dir		( const Fvector& D) { u16 value = pvCompress(D); w(&value, sizeof(u16)); }
	IC void w_sdir		( const Fvector& D) {
		Fvector C;
		float mag		= D.magnitude();
		if (mag>EPS_S)	{
			C.div		(D,mag);
		} else {
			C.set		(0,0,1);
			mag			= 0;
		}
		w_dir	(C);
		w(&mag, sizeof(float));
	}
	IC void w_stringZ			( LPCSTR S )	{ w(S,(u32)xr_strlen(S)+1);}
	IC void w_stringZ			( const shared_str& p)
	{
    	if (*p)	
			w(*p,p.size()+1);
		else
		{
			u8 data = 0;
			w(&data, sizeof(u8));
		}
	}
	IC void w_matrix			(Fmatrix& M)
	{
		w(&M.i, sizeof(Fvector));
		w(&M.j, sizeof(Fvector));
		w(&M.k, sizeof(Fvector));
		w(&M.c, sizeof(Fvector));
	}
	
	IC void w_clientID			(ClientID& C) {	u32 val = C.value();	w(&val, sizeof(u32));	}
	
	IC void	w_chunk_open8		(u32& position)
	{
		position	= w_tell();
		u8 data = 0;
		w(&data, sizeof(u8));
	}
	
	IC void w_chunk_close8		(u32 position)
	{
		u32 size	= u32(w_tell() - position) - sizeof(u8);
		VERIFY		(size<256	);
		u8			_size = (u8)size;
		w_seek		(position,&_size,sizeof(_size));
	}

	IC void	w_chunk_open16		(u32& position)
	{
		position	= w_tell	();
		u16 data = 0;
		w(&data, sizeof(u16));
	}

	IC void w_chunk_close16		(u32 position)
	{
		u32 size	= u32(w_tell() - position) - sizeof(u16);
		VERIFY		(size < 65536);
		u16			_size = (u16)size;
		w_seek		(position,&_size,sizeof(_size));
	}

	// reading
	void		read_start		();
	u32			r_begin			( u16& type	);
	void		r_seek			(u32 pos);
	u32			r_tell			();

	IC void		r				( void* p, u32 count)
	{
		VERIFY		(p && count);
		CopyMemory	(p,&B.data[r_pos],count);
		r_pos		+= count;
		VERIFY		(r_pos<=B.count);
	}
	BOOL		r_eof			();
	u32			r_elapsed		();
	void		r_advance		(u32 size);

	// reading - utilities
	void		r_vec3			(Fvector& A);
	void		r_vec4			(Fvector4& A);
	void		r_float			(float& A );
	void 		r_u64			(u64& A);
	void 		r_s64			(s64& A);
	void 		r_u32			(u32& A);
	void		r_s32			(s32& A);
	void		r_u16			(u16& A);
	void		r_s16			(s16& A);
	void		r_u8			(u8&  A);
	void		r_s8			(s8&  A);

	// IReader compatibility
	Fvector		r_vec3			();
	Fvector4	r_vec4			();
	float		r_float_q8		(float min,float max);
	float		r_float_q16		(float min, float max);
	float		r_float			();
	u64 		r_u64			();
	s64 		r_s64			();
	u32 		r_u32			();
	s32			r_s32			();
	u16			r_u16			();
	s16			r_s16			();
	u8			r_u8			();
	s8			r_s8			();

	void		r_float_q16		(float& A, float min, float max);
	void		r_float_q8		(float& A, float min, float max);
	void		r_angle16		(float& A);
	void		r_angle8		(float& A);
	void		r_dir			(Fvector& A);

	void		r_sdir			(Fvector& A);
	void		r_stringZ		(LPSTR S );
	void		r_stringZ		(xr_string& dest );
	void 		r_stringZ		(shared_str& dest);
	
	void		skip_stringZ	();
	
	void		r_stringZ_s		(LPSTR string, u32 size);

	template <u32 size>
	inline void	r_stringZ_s		(char (&string)[size])
	{
		r_stringZ_s	(string, size);
	}

	void		r_matrix		(Fmatrix& M);
	void		r_clientID		(ClientID& C);
};

#pragma pack(pop)

#endif /*_INCDEF_NETUTILS_H_*/