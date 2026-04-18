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

    ICF TYPE	get		()									const	{ return flags;}
    ICF SelfRef	zero	()											{ flags=T(0);	return *this;	}
    ICF SelfRef	one		()											{ flags=T(-1);	return *this;	}
    ICF SelfRef	invert	()											{ flags	=	~flags;		return *this;	}
    ICF SelfRef	invert	(const Self& f)								{ flags	=	~f.flags;	return *this;	}
    ICF SelfRef	invert	(const T mask)								{ flags ^=	mask;		return *this;	}
	ICF SelfRef	assign	(const Self& f)								{ flags =	f.flags;	return *this;	}
	ICF SelfRef	assign	(const T mask)								{ flags	=	mask;		return *this;	}
	ICF SelfRef	set		(const T mask,	bool value)					{ if (value) flags|=mask; else flags&=~mask; return *this; }
	ICF bool	is		(const T mask)						const	{ return mask==(flags&mask);			}
#if defined(IXR_WINDOWS) && !defined(_M_X64)
	ICF  bool    bitTest(const int bitNum)					const { u32 tempFlag = flags; return _bittest((long*)&tempFlag, bitNum); }
#else
	ICF  bool    bitTest(const int bitNum)					const { u64 tempFlag = flags; return _bittest64((s64*)&tempFlag, bitNum); }
#endif

	ICF bool	is_any	(const T mask)						const	{ return bool(!!(flags&mask));			}
	ICF bool	test	(const T mask)						const	{ return bool(!!(flags&mask));			}
	ICF SelfRef	bor		(const T mask)								{ flags|=mask;			return *this;	}
	ICF SelfRef	bor		(const Self& f, const T mask) 				{ flags=f.flags|mask;	return *this;	}
	ICF SelfRef	band		(const T mask)								{ flags&=mask;			return *this;	}
	ICF SelfRef	band		(const Self& f, const T mask) 				{ flags=f.flags&mask;	return *this;	}
	ICF bool	equal	(const Self& f) 			  		const	{ return flags==f.flags;}
	ICF bool	equal	(const Self& f, const T mask) 		const	{ return (flags&mask)==(f.flags&mask);}
};

using Flags8  = _flags<u8> ; using flags8  = _flags<u8> ;		
using Flags16 = _flags<u16>; using flags16 = _flags<u16>;
using Flags32 = _flags<u32>; using flags32 = _flags<u32>;
using Flags64 = _flags<u64>; using flags64 = _flags<u64>;

template <class T>
bool operator == (_flags<T> const& A, _flags<T>  const& B) { return A.flags == B.flags; }
