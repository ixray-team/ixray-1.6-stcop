#pragma once

template <class T>
struct _flags
{
	using TYPE		= T;
	using Self		= _flags;
	using SelfRef	= Self&;
	using SelfCRef	= const Self&;
	
	T flags;

	//IC	bool	operator==(SelfCRef Left)
	//{
	//	return Left.flags == flags;
	//}

    IC	TYPE	get		()									const	{ return flags;}
    IC	SelfRef	zero	()											{ flags=T(0);	return *this;	}
    IC	SelfRef	one		()											{ flags=T(-1);	return *this;	}
    IC	SelfRef	invert	()											{ flags	=	~flags;		return *this;	}
    IC	SelfRef	invert	(const Self& f)								{ flags	=	~f.flags;	return *this;	}
    IC	SelfRef	invert	(const T mask)								{ flags ^=	mask;		return *this;	}
	IC	SelfRef	assign	(const Self& f)								{ flags =	f.flags;	return *this;	}
	IC	SelfRef	assign	(const T mask)								{ flags	=	mask;		return *this;	}
	IC	SelfRef	set		(const T mask,	BOOL value)					{ if (value) flags|=mask; else flags&=~mask; return *this; }
	IC 	BOOL	is		(const T mask)						const	{ return mask==(flags&mask);			}
#if defined(IXR_WINDOWS) && !defined(_M_X64)
	IC  bool    bitTest(const int bitNum)					const { u32 tempFlag = flags; return _bittest((long*)&tempFlag, bitNum); }
#else
	IC  bool    bitTest(const int bitNum)					const { u64 tempFlag = flags; return _bittest64((s64*)&tempFlag, bitNum); }
#endif

	IC 	BOOL	is_any	(const T mask)						const	{ return BOOL(!!(flags&mask));			}
	IC 	BOOL	test	(const T mask)						const	{ return BOOL(!!(flags&mask));			}
	IC 	SelfRef	bor		(const T mask)								{ flags|=mask;			return *this;	}
	IC 	SelfRef	bor		(const Self& f, const T mask) 				{ flags=f.flags|mask;	return *this;	}
	IC 	SelfRef	band		(const T mask)								{ flags&=mask;			return *this;	}
	IC 	SelfRef	band		(const Self& f, const T mask) 				{ flags=f.flags&mask;	return *this;	}
	IC 	BOOL	equal	(const Self& f) 			  		const	{ return flags==f.flags;}
	IC 	BOOL	equal	(const Self& f, const T mask) 		const	{ return (flags&mask)==(f.flags&mask);}
};

using Flags8  = _flags<u8> ; using flags8  = _flags<u8> ;		
using Flags16 = _flags<u16>; using flags16 = _flags<u16>;
using Flags32 = _flags<u32>; using flags32 = _flags<u32>;
using Flags64 = _flags<u64>; using flags64 = _flags<u64>;

template <class T>
bool operator == (_flags<T> const& A, _flags<T>  const& B) { return A.flags == B.flags; }
