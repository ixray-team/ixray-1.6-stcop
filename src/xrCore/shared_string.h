#pragma once
#pragma pack(push,4)
//////////////////////////////////////////////////////////////////////////
using str_c = const char*;

#pragma warning(disable : 4200)
struct XRCORE_API str_value
{
	xr_atomic_u32 dwReference;
	u32 dwLength;
	u32 dwCRC;

	str_value* next;
#ifndef MASTER_GOLD
	str_c value_ptr;
#endif
	char value[];
};

struct XRCORE_API str_value_cmp 
{
	IC bool operator ()	(const str_value* A, const str_value* B) const { return A->dwCRC < B->dwCRC; };
};

struct XRCORE_API str_hash_function 
{
	IC size_t operator ()(str_value const* const value) const { return value->dwCRC; };
};

#pragma warning(default : 4200)

struct str_container_impl;
class IWriter;
//////////////////////////////////////////////////////////////////////////
class XRCORE_API str_container
{
private:
	xrCriticalSection cs;
	str_container_impl* impl;
public:
	str_container();
	~str_container();

	str_value* dock(str_c value);
	void clean();
	u32  stat_economy();
};
XRCORE_API extern str_container* g_pStringContainer;

//////////////////////////////////////////////////////////////////////////

constexpr size_t shared_str_limit = 4096;

class XRCORE_API shared_str
{
	str_value* p_;
protected:
	void _dec() { if (nullptr == p_) return;	p_->dwReference--; 	if (0 == p_->dwReference)	p_ = nullptr; }
public:
	void _set(str_c rhs) { str_value* v = g_pStringContainer->dock(rhs); if (nullptr != v) v->dwReference++; _dec(); p_ = v; }
	void _set(shared_str const& rhs) { str_value* v = rhs.p_; if (nullptr != v) v->dwReference++; _dec(); p_ = v; }

	const str_value* _get()	const { return p_; }
public:
	// construction
	shared_str() { p_ = nullptr; }
	shared_str(str_c rhs) { p_ = nullptr;	_set(rhs); }
	shared_str(shared_str const& rhs) { p_ = nullptr;	_set(rhs); }
	~shared_str() { _dec(); }

	// assignment & accessors
	shared_str&			operator=	(str_c rhs) { _set(rhs);	return (shared_str&)*this; }
	shared_str&			operator=	(shared_str const& rhs) { _set(rhs);	return (shared_str&)*this; }
	str_c				operator*	() const { return p_ ? p_->value : nullptr; }
						operator bool	() const { return p_ != nullptr; }
	char				operator[]	(size_t id) { return p_->value[id]; }
	str_c				c_str		() const { return p_ ? p_->value : nullptr; }

	// misc func
	u32					size		() const { if (nullptr == p_) return 0; else return p_->dwLength; }
	void				swap		(shared_str& rhs) { str_value* tmp = p_; p_ = rhs.p_; rhs.p_ = tmp; }
	bool				equal		(const shared_str& rhs) const { return (p_ == rhs.p_); }
	shared_str& 		printf		(const char* format, ...);
};


// res_ptr == res_ptr
// res_ptr != res_ptr
// const res_ptr == ptr
// const res_ptr != ptr
// ptr == const res_ptr
// ptr != const res_ptr
// res_ptr < res_ptr
// res_ptr > res_ptr
IC bool operator	==	(shared_str const& a, shared_str const& b) { return a._get() == b._get(); }
IC bool operator	!=	(shared_str const& a, shared_str const& b) { return a._get() != b._get(); }
IC bool operator	<	(shared_str const& a, shared_str const& b) { return a._get() < b._get();  }
IC bool operator	>	(shared_str const& a, shared_str const& b) { return a._get() > b._get();  }

// externally visible standart functionality
IC void swap		(shared_str& lhs, shared_str& rhs)		{ lhs.swap(rhs); }
IC u32	xr_strlen	(shared_str& a)							{ return a.size(); }
IC int	xr_strcmp	(const shared_str& a, const char* b)	{ return xr_strcmp(*a, b); }
IC int	xr_strcmp	(const char* a, const shared_str& b)	{ return xr_strcmp(a, *b); }
IC int	xr_strcmp	(const shared_str& a, const shared_str& b) 
{
	if (a.equal(b))		return 0;
	else				return xr_strcmp(*a, *b);
}

IC void	xr_strlwr(shared_str& src) { if (*src) { LPSTR lp = xr_strdup(*src); xr_strlwr(lp); src = lp; xr_free(lp); } }

namespace std
{
	template<>
	class hash<shared_str>
	{
	public:
		using is_transparent = void;
	public:
		size_t operator()(const shared_str& s) const
		{
			const str_value* p = s._get();
			return p ? static_cast<size_t>(p->dwCRC) : 0;
		}
	};
}

#pragma pack(pop)