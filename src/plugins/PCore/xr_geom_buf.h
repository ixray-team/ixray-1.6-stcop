#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_GEOM_BUF_H__
#define __XR_GEOM_BUF_H__

#include <vector>
#include "xr_vector2.h"
#include "xr_vector3.h"
#include "xr_color.h"
#include "xr_influence.h"
#include "xr_ogf_format.h"
#include <memory>

namespace xray_re {

// in-game format definitions for level geometry in v5...v8 - from D3D7 actually.
enum {
	D3D_FVF_POSITION_MASK	= 0x00e,
	D3D_FVF_XYZ		= 0x002,	// x, y, z

	D3D_FVF_NORMAL		= 0x010,	// not used
	D3D_FVF_DIFFUSE		= 0x040,	// quantized normal
	D3D_FVF_SPECULAR	= 0x080,	// not used
	D3D_FVF_NC_MASK		= 0x0f0,	// not used

	D3D_FVF_TEX1		= 0x100,	// base texture
	D3D_FVF_TEX2		= 0x200,	// base texture + light map
	D3D_FVF_TEXCOUNT_MASK	= 0xf00,
	D3D_FVF_TEXCOUNT_SHIFT	= 8,
};

// in-game format definitions for v13+ - from D3D9 actually.
enum {
	D3D_VE_USAGE_POSITION		= 0,
	D3D_VE_USAGE_BLENDWEIGHT	= 1,
	D3D_VE_USAGE_BLENDINDICES	= 2,
	D3D_VE_USAGE_NORMAL		= 3,
	D3D_VE_USAGE_PSIZE		= 4,
	D3D_VE_USAGE_TEXCOORD		= 5,
	D3D_VE_USAGE_TANGENT		= 6,
	D3D_VE_USAGE_BINORMAL		= 7,
	D3D_VE_USAGE_TESSFACTOR		= 8,
	D3D_VE_USAGE_POSITIONT		= 9,
	D3D_VE_USAGE_COLOR		= 10,
	D3D_VE_USAGE_FOG		= 11,
	D3D_VE_USAGE_DEPTH		= 12,
	D3D_VE_USAGE_SAMPLE		= 13,
};

enum {
	D3D_VE_METHOD_DEFAULT	= 0,
	D3D_VE_METHOD_PARTIALU	= 1,
	D3D_VE_METHOD_PARTIALV	= 2,
	D3D_VE_METHOD_CROSSUV	= 3,
	D3D_VE_METHOD_UV	= 4,
};

enum {
	D3D_VE_TYPE_FLOAT1	= 0,
	D3D_VE_TYPE_FLOAT2	= 1,
	D3D_VE_TYPE_FLOAT3	= 2,
	D3D_VE_TYPE_FLOAT4	= 3,
	D3D_VE_TYPE_D3DCOLOR	= 4,

	D3D_VE_TYPE_UBYTE4	= 5,
	D3D_VE_TYPE_SHORT2	= 6,
	D3D_VE_TYPE_SHORT4	= 7,

	D3D_VE_TYPE_UBYTE4N	= 8,
	D3D_VE_TYPE_SHORT2N	= 9,
	D3D_VE_TYPE_SHORT4N	= 10,
	D3D_VE_TYPE_USHORT2N	= 11,
	D3D_VE_TYPE_USHORT4N	= 12,
	D3D_VE_TYPE_UDEC3	= 13,
	D3D_VE_TYPE_DEC3N	= 14,
	D3D_VE_TYPE_FLOAT16_2	= 15,
	D3D_VE_TYPE_FLOAT16_4	= 16,
	D3D_VE_TYPE_UNUSED	= 17,
};

typedef uint32_t d3d_fvf;

struct d3d_vertex_element {
	uint16_t	stream;
	uint16_t	offset;
	uint8_t		type;
	uint8_t		method;
	uint8_t		usage;
	uint8_t		usage_index;
};

class xr_flexbuf {
public:
	virtual		~xr_flexbuf();
	size_t		size() const;
	bool		owner() const;

protected:
			xr_flexbuf();
	void		set_size(size_t size);
	void		set_owner(bool owner);
	void		clear();
	template<typename T> T* copy(const T* source, T* target, size_t size) const;
	template<typename T> T*	duplicate(const T* source) const;

private:
	bool		m_owner;
	size_t		m_size;
};

class xr_reader;
class xr_writer;

class xr_vbuf: public xr_flexbuf {
public:
			xr_vbuf();
			xr_vbuf(const xr_vbuf& that);
			xr_vbuf(size_t n, const fvector3* points, const fvector3* normals, const fvector2* texcoords);
	virtual		~xr_vbuf();
	void		clear();
	void		proxy(const xr_vbuf& that, size_t base, size_t n);
	void		load_d3d7(xr_reader& r, size_t n, uint32_t fvf);
	void		load_d3d9(xr_reader& r, size_t n, const d3d_vertex_element ve[], size_t n_ve);
	void		load_dm(xr_reader& r, size_t n);
	void		save_dm(xr_writer& w) const;
	void		load_ogf3(xr_reader& r, size_t n, ogf_vertex_format vf);
	void		load_ogf4(xr_reader& r, size_t n, ogf_vertex_format vf);
	xr_vbuf&	operator=(const xr_vbuf& right);
//	bool		operator==(const xr_vbuf& right) const;
//	bool		operator!=(const xr_vbuf& right) const;

	uint32_t	signature() const;

	bool		has_points() const;
	bool		has_normals() const;
	bool		has_texcoords() const;
	bool		has_influences() const;
	bool		has_colors() const;
	bool		has_lightmaps() const;

	const fvector3&		p(size_t at) const;
	const fvector3&		n(size_t at) const;
	const fvector2&		tc(size_t at) const;
	const fvector2&		lm(size_t at) const;
	const finfluence&	w(size_t at) const;
	const fcolor&		c(size_t at) const;

	const fvector3*		p() const;
	const fvector3*		n() const;
	const fvector2*		tc() const;
	const fvector2*		lm() const;
	const finfluence*	w() const;
	const fcolor*		c() const;

	enum {
		S_POINTS	= 0x01,
		S_NORMALS	= 0x02,
		S_TEXCOORDS	= 0x04,
		S_LIGHTMAPS	= 0x08,
		S_INFLUENCES	= 0x10,
		S_COLORS	= 0x20,
	};

protected:
	void		make_signature();

protected:
	uint32_t	m_signature;

	fvector3*	m_points;
	fvector3*	m_normals;
	fvector2*	m_texcoords;
	fvector2*	m_lightmaps;
	finfluence*	m_influences;
	fcolor*		m_colors;
};

TYPEDEF_STD_VECTOR(xr_vbuf)

class xr_ibuf: public xr_flexbuf {
public:
			xr_ibuf();
			xr_ibuf(const xr_ibuf& that);
	virtual		~xr_ibuf();

	void		load(xr_reader& r, size_t n);
	void		save(xr_writer& w) const;
	void		proxy(const xr_ibuf& that, size_t base, size_t n);
	void		clear();
	xr_ibuf&	operator=(const xr_ibuf& right);
//	bool		operator==(const xr_ibuf& right) const;
//	bool		operator!=(const xr_ibuf& right) const;
	const uint16_t&	operator[](size_t at) const;
	uint16_t&	operator[](size_t at);

private:
	uint16_t*	m_indices;
};

TYPEDEF_STD_VECTOR(xr_ibuf)

class xr_swibuf: public xr_flexbuf {
public:
				xr_swibuf();
				xr_swibuf(const xr_swibuf& that);
	virtual			~xr_swibuf();
	void			proxy(const xr_swibuf& that);
	void			load(xr_reader& r);
	void			save(xr_writer& w) const;
	void			clear();
	xr_swibuf&		operator=(const xr_swibuf& right);
	const ogf4_slide_window&operator[](size_t at) const;

private:
	uint32_t		m_reserved[4];
	ogf4_slide_window*	m_slide_windows;
};

TYPEDEF_STD_VECTOR(xr_swibuf)

inline xr_flexbuf::xr_flexbuf(): m_owner(true), m_size(0) {}
inline xr_flexbuf::~xr_flexbuf() {}
inline void xr_flexbuf::clear() { m_owner = true; m_size = 0; }
inline size_t xr_flexbuf::size() const { return m_size; }
inline void xr_flexbuf::set_size(size_t size) { m_size = size; }
inline bool xr_flexbuf::owner() const { return m_owner; }
inline void xr_flexbuf::set_owner(bool owner) { m_owner = owner; }

template<typename T> inline T* xr_flexbuf::copy(const T* source, T* target, size_t size) const
{
#if defined(_MSC_VER) && _MSC_VER >= 1400 && _MSC_VER < 1600
	stdext::unchecked_uninitialized_copy(source, source + size, target);
#else
	std::uninitialized_copy(source, source + size, target);
#endif
	return target;
}

template<typename T> inline T* xr_flexbuf::duplicate(const T* source) const
{
	return source ? copy(source, new T[m_size], m_size) : 0;
}

inline uint32_t xr_vbuf::signature() const { return m_signature; }
inline bool xr_vbuf::has_points() const { return !!(m_signature & S_POINTS); }
inline bool xr_vbuf::has_normals() const { return !!(m_signature & S_NORMALS); }
inline bool xr_vbuf::has_texcoords() const { return !!(m_signature & S_TEXCOORDS); }
inline bool xr_vbuf::has_lightmaps() const { return !!(m_signature & S_LIGHTMAPS); }
inline bool xr_vbuf::has_influences() const { return !!(m_signature & S_INFLUENCES); }
inline bool xr_vbuf::has_colors() const { return !!(m_signature & S_COLORS); }
//inline bool xr_vbuf::operator!=(const xr_vbuf& right) const { return !(*this == right); }
inline const fvector3& xr_vbuf::p(size_t at) const { return m_points[at]; }
inline const fvector3& xr_vbuf::n(size_t at) const { return m_normals[at]; }
inline const fvector2& xr_vbuf::tc(size_t at) const { return m_texcoords[at]; }
inline const fvector2& xr_vbuf::lm(size_t at) const { return m_lightmaps[at]; }
inline const finfluence& xr_vbuf::w(size_t at) const { return m_influences[at]; }
inline const fcolor& xr_vbuf::c(size_t at) const { return m_colors[at]; }
inline const fvector3* xr_vbuf::p() const { return m_points; }
inline const fvector3* xr_vbuf::n() const { return m_normals; }
inline const fvector2* xr_vbuf::tc() const { return m_texcoords; }
inline const fvector2* xr_vbuf::lm() const { return m_lightmaps; }
inline const finfluence* xr_vbuf::w() const { return m_influences; }
inline const fcolor* xr_vbuf::c() const { return m_colors; }

//inline bool xr_ibuf::operator!=(const xr_ibuf& right) const { return !(*this == right); }
inline const uint16_t& xr_ibuf::operator[](size_t at) const { return m_indices[at]; }
inline uint16_t& xr_ibuf::operator[](size_t at) { return m_indices[at]; }

inline const ogf4_slide_window& xr_swibuf::operator[](size_t at) const { return m_slide_windows[at]; }

} // end of namespace xray_re

#endif
