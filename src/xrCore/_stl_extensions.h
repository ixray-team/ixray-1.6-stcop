#ifndef _STL_EXT_internal
#define _STL_EXT_internal

using std::swap;

#ifdef	__BORLANDC__
#define M_NOSTDCONTAINERS_EXT
#endif
#ifdef	_M_AMD64
#define M_DONTDEFERCLEAR_EXT
#endif

template <class T>
class	xalloc	{
public:
	typedef	size_t		size_type;
	typedef ptrdiff_t	difference_type;
	typedef T*			pointer;
	typedef const T*	const_pointer;
	typedef T&			reference;
	typedef const T&	const_reference;
	typedef T			value_type;

public:
	template<class _Other>	
	struct rebind			{	typedef xalloc<_Other> other;	};
public:
							pointer					address			(reference _Val) const					{	return (&_Val);	}
							const_pointer			address			(const_reference _Val) const			{	return (&_Val);	}
													xalloc			()										{	}
													xalloc			(const xalloc<T>&)						{	}
	template<class _Other>							xalloc			(const xalloc<_Other>&)					{	}
	template<class _Other>	xalloc<T>&				operator=		(const xalloc<_Other>&)					{	return (*this);	}
							pointer					allocate		(size_type n, const void* p=0) const	{	return xr_alloc<T>((u32)n);	}
							char*					_charalloc		(size_type n)							{	return (char*)allocate(n); }
							void					deallocate		(pointer p, size_type n) const			{	xr_free	(p);				}
							void					deallocate		(void* p, size_type n) const			{	xr_free	(p);				}
							void					construct		(pointer p, const T& _Val)				{	::new(p) T(_Val);	}
							void					destroy			(pointer p)								{	p->~T();			}
							size_type				max_size		() const								{	size_type _Count = (size_type)(-1) / sizeof (T);	return _Count;	}
};

struct xr_allocator {
	template <typename T>
	struct helper {
		typedef xalloc<T>	result;
	};

	static	void	*alloc		(const u32 &n)	{	return xr_malloc((u32)n);	}
	template <typename T>
	static	void	dealloc		(T *&p)			{	xr_free(p);					}
};

template<class _Ty,	class _Other>	inline	bool operator==(const xalloc<_Ty>&, const xalloc<_Other>&)		{	return (true);							}
template<class _Ty, class _Other>	inline	bool operator!=(const xalloc<_Ty>&, const xalloc<_Other>&)		{	return (false);							}

namespace std
{
	template<class _Tp1, class _Tp2>	inline	xalloc<_Tp2>&	__stl_alloc_rebind(xalloc<_Tp1>& __a, const _Tp2*)	{	return (xalloc<_Tp2>&)(__a);	}
	template<class _Tp1, class _Tp2>	inline	xalloc<_Tp2>	__stl_alloc_create(xalloc<_Tp1>&, const _Tp2*)		{	return xalloc<_Tp2>();			}
};

// string(char)
using xr_string = std::basic_string<char, std::char_traits<char>, xalloc<char>>;

// vector
template <typename T, typename allocator = xalloc<T> >
using xr_vector = std::vector<T, allocator>;

template <typename T>
void clear_and_reserve(xr_vector<T> &vector_object) {
    if (vector_object.capacity() <= (vector_object.size() + vector_object.size() / 4)) {
        vector_object.clear();
    } else {
        u32 old = (u32)vector_object.size();
        vector_object.clear();
        vector_object.reserve(old);
    }
}

// deque
template <typename T, typename allocator = xalloc<T>>
using xr_deque = std::deque<T, allocator>;

// stack
template <typename T, class C = xr_deque<T> >
using xr_stack = std::stack<T, C>;

template <typename T, typename allocator = xalloc<T> >							
using xr_list = std::list<T, allocator>;

template <typename K, class P = std::less<K>, typename allocator = xalloc<K> >				
using xr_set = std::set<K, P, allocator>;

template <typename K, class P = std::less<K>, typename allocator = xalloc<K> >			
using xr_multiset = std::multiset<K, P, allocator>;

template <typename K, class V, class P = std::less<K>, typename allocator = xalloc<std::pair<const K,V> > >	
using xr_map = std::map<K, V, P, allocator>;

template <typename K, class V, class P=std::less<K>, typename allocator = xalloc<std::pair<const K,V> > >	
using xr_multimap = std::multimap<K, V, P, allocator>;

template <typename K, class V, class _Traits = std::equal_to<K>, typename allocator = xalloc<std::pair<const K,V> > >	
using xr_hash_map = std::unordered_map<K, V, std::hash<K>, _Traits, allocator>;

struct pred_str {
	IC bool operator()(const char* x, const char* y) const				{	return xr_strcmp(x,y)<0;	}
};
struct pred_stri {
	IC bool operator()(const char* x, const char* y) const				{	return _stricmp(x,y)<0;	}
};

// STL extensions
#include "FixedVector.h"
#include "buffer_vector.h"

// auxilary definition
using FvectorVec = xr_vector<Fvector>;	 
using FvectorIt = FvectorVec::iterator;

using LPSTRVec = xr_vector<LPSTR>;		 
using LPSTRIt = LPSTRVec::iterator;

using LPCSTRVec = xr_vector<LPCSTR>;	 
using LPCSTRIt = LPCSTRVec::iterator;

using SStringVec = xr_vector<xr_string>; 
using SStringVecIt = SStringVec::iterator;

using U16Vec = xr_vector<u16>;			 
using U16It = U16Vec::iterator;

using U32Vec = xr_vector<u32>;			 
using U32It = U32Vec::iterator;

using FloatVec = xr_vector<float>;		 
using FloatIt = FloatVec::iterator;

using IntVec = xr_vector<int>;			 
using IntIt = IntVec::iterator;

using BOOLVec = xr_vector<BOOL>;
using BOOLIt = BOOLVec::iterator;

using LPBOOLVec = xr_vector<BOOL*>;
using LPBOOLIt = LPBOOLVec::iterator;

using FrectVec = xr_vector<Frect>;
using FrectIt = FrectVec::iterator;

using IrectVec = xr_vector<Irect>;
using IrectIt = IrectVec::iterator;

using LPFvectorVec = xr_vector<Fvector*>;
using LPFvectorIt = LPFvectorVec::iterator;

using FcolorVec = xr_vector<Fcolor>;
using FcolorIt = FcolorVec::iterator;

using LPFcolorVec = xr_vector<Fcolor*>;
using LPFcolorIt = LPFcolorVec::iterator;

using string64Vec = xr_vector<string64>;
using string64It = string64Vec::iterator;

using S8Vec = xr_vector<s8>;
using S8It = S8Vec::iterator;

using LPS8Vec = xr_vector<s8*>;
using LPS8It = LPS8Vec::iterator;

using S16Vec = xr_vector<s16>;
using S16It = S16Vec::iterator;

using LPS16Vec = xr_vector<s16*>;
using LPS16It = LPS16Vec::iterator;

using S32Vec = xr_vector<s32>;
using S32It = S32Vec::iterator;

using LPS32Vec = xr_vector<s32*>;
using LPS32It = LPS32Vec::iterator;

using LPU8Vec = xr_vector<u8*>;
using LPU8It = LPU8Vec::iterator;

using LPU16Vec = xr_vector<u16*>;
using LPU16It = LPU16Vec::iterator;

using LPFloatVec = xr_vector<float*>;
using LPFloatIt = LPFloatVec::iterator;

using LPIntVec = xr_vector<int*>;
using LPIntIt = LPIntVec::iterator;

#ifdef __BORLANDC__
using boolVec = xr_vector<bool>;
using boolIt = boolVec::iterator;

using PlaneVec = xr_vector<Fplane>;
using PlaneIt = PlaneVec::iterator;

using Fvector2Vec = xr_vector<Fvector2>;
using Fvector2It = Fvector2Vec::iterator;

using U8Vec = xr_vector<u8>;
using U8It = U8Vec::iterator;

using LPU32Vec = xr_vector<u32*>;
using LPU32It = PlaneVec::iterator;

using AStringVec = xr_vector<AnsiString>;
using AStringIt = AStringVec::iterator;

using LPAStringVec = xr_vector<AnsiString*>;
using LPAStringIt = LPAStringVec::iterator;
#endif

#endif