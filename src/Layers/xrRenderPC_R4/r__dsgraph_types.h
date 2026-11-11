#pragma once

#include "../../xrCore/Containers/FixedMap.h"

//#define USE_DOUG_LEA_ALLOCATOR_FOR_RENDER

#ifdef USE_DOUG_LEA_ALLOCATOR_FOR_RENDER
#	include	"../../xrCore/doug_lea_allocator.h"
	extern doug_lea_allocator	g_render_lua_allocator;

	template <class T>
	class doug_lea_alloc {
	public:
		using size_type = size_t;
		using difference_type = ptrdiff_t;
		using pointer = T*;
		using const_pointer = const T*;
		using reference = T&;
		using const_reference = const T&;
		using value_type = T;

	public:
		template<class _Other>	
		struct rebind			{ using other = doug_lea_alloc<_Other>;	};
	public:
								pointer					address			(reference _Val) const					{	return (&_Val);	}
								const_pointer			address			(const_reference _Val) const			{	return (&_Val);	}
														doug_lea_alloc	()										{	}
														doug_lea_alloc	(const doug_lea_alloc<T>&)				{	}
		template<class _Other>							doug_lea_alloc	(const doug_lea_alloc<_Other>&)			{	}
		template<class _Other>	doug_lea_alloc<T>&		operator=		(const doug_lea_alloc<_Other>&)			{	return (*this);	}
								pointer					allocate		(size_type n, const void* p=0) const	{	return (T*)g_render_lua_allocator.malloc_impl(sizeof(T)*(u32)n);	}
								void					deallocate		(pointer p, size_type n) const			{	g_render_lua_allocator.free_impl	((void*&)p);				}
								void					deallocate		(void* p, size_type n) const			{	g_render_lua_allocator.free_impl	(p);				}
								char*					__charalloc		(size_type n)							{	return (char*)allocate(n); }
								void					construct		(pointer p, const T& _Val)				{	::new(p) T(_Val);	}
								void					destroy			(pointer p)								{	p->~T();			}
								size_type				max_size		() const								{	size_type _Count = (size_type)(-1) / sizeof (T);	return (0 < _Count ? _Count : 1);	}
	};

	template<class _Ty,	class _Other>	inline	bool operator==(const doug_lea_alloc<_Ty>&, const doug_lea_alloc<_Other>&)		{	return (true);							}
	template<class _Ty, class _Other>	inline	bool operator!=(const doug_lea_alloc<_Ty>&, const doug_lea_alloc<_Other>&)		{	return (false);							}

	struct doug_lea_allocator_wrapper {
		template <typename T>
		struct helper {
			using result = doug_lea_alloc<T>;
		};

		static	void	*alloc		(const u32 &n)	{	return g_render_lua_allocator.malloc_impl((u32)n);	}
		template <typename T>
		static	void	dealloc		(T *&p) noexcept			{	g_render_lua_allocator.free_impl((void*&)p);	}
	};

#	define render_alloc				doug_lea_alloc
	using render_allocator = doug_lea_allocator_wrapper;

#else
#	define render_alloc				xalloc
	using render_allocator = xr_allocator;
#endif // USE_DOUG_LEA_ALLOCATOR_FOR_RENDER

class dxRender_Visual;

namespace R_dsgraph
{
	struct _MatrixItem
	{
		float ssa;
		IRenderable* pObject;
		dxRender_Visual* pVisual;
		Fmatrix Matrix;				// matrix (copy)
	};

	struct _LodItem
	{
		float ssa;
		dxRender_Visual* pVisual;
	};

	// Geometry==nullptr selects the virtual Render() path
	struct StaticPacket
	{
		u64 Key;
		SPass* Pass;
		dxRender_Visual* Visual;
		SGeometry* Geometry;
		u32 BaseVertex, StartIndex, IndexCount;
		float Ssa, Radius;
	};

	struct DynamicPacket
	{
		u64 Key;
		SPass* Pass;
		ShaderElement* Element;
		dxRender_Visual* Visual;
		IRenderable* Object;
		float Ssa;
		Fmatrix Matrix;
	};

	using StaticList = xr_vector<StaticPacket>;
	using DynamicList = xr_vector<DynamicPacket>;

	using LodList = xr_vector<_LodItem>;
};
