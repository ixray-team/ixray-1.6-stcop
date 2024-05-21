#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SURFACE_H__
#define __XR_SURFACE_H__

#include <vector>
#include <string>
#include "xr_types.h"

namespace xray_re {

enum {
	RSF_COLLISION	= 0x0001,
	RSF_RENDERING	= 0x0002,
	RSF_LIGHTMAP	= 0x0004,
	RSF_TWO_SIDED	= 0x0008,
	RSF_AUTOMATIC	= 0x0010,	// assign texture/engine shader basing on game material
	RSF_DEBUG	= 0x0020,
};

// editable object surface flags
const uint32_t ESF_TWO_SIDED = 0x00000001;

const uint32_t ESURFACE_DEFAULT_FVF = 0x112;

class xr_raw_surface {
public:
	void		set(uint16_t _texture, uint16_t _eshader = UINT16_MAX,
					uint16_t _gamemtl = UINT16_MAX, uint16_t _flags = RSF_RENDERING);
	void		clear();

	bool		collision() const;
	bool		rendering() const;
	bool		lightmap() const;
	bool		two_sided() const;
	bool		automatic() const;
	bool		debug() const;

	int		compare(const xr_raw_surface& right) const;
	bool		operator<(const xr_raw_surface& right) const;
	bool		operator==(const xr_raw_surface& right) const;

	union {
		struct {
			uint16_t	texture;
			uint16_t	eshader;
			uint16_t	gamemtl;
			uint16_t	flags;
		};
		// for comparison
		struct {
			uint32_t	blob32[2];
		};
		uint64_t		blob64;
	};
};

TYPEDEF_STD_VECTOR(xr_raw_surface)

class xr_reader;
class xr_writer;

class xr_surface {
public:
			xr_surface(bool skeletal = false);
	virtual		~xr_surface();

	void		load_0(xr_reader& r);
	void		load_1(xr_reader& r);
	void		load_2(xr_reader& r);
	void		save(xr_writer& w) const;

	int		compare(const xr_surface& right) const;
	bool		operator==(const xr_surface& right) const;
	bool		operator<(const xr_surface& right) const;

	std::string&		name();
	const std::string&	name() const;
	std::string&		eshader();
	const std::string&	eshader() const;
	std::string&		cshader();
	const std::string&	cshader() const;
	std::string&		gamemtl();
	const std::string&	gamemtl() const;
	std::string&		texture();
	const std::string&	texture() const;
	std::string&		vmap();
	const std::string&	vmap() const;
	uint32_t&		flags();
	uint32_t		flags() const;

	// shortcuts
	bool			two_sided() const;
	void			set_two_sided();

private:
	std::string	m_name;
	std::string	m_eshader;
	std::string	m_cshader;
	std::string	m_gamemtl;
	std::string	m_texture;
	std::string	m_vmap;
	uint32_t	m_flags;
	uint32_t	m_fvf;
};

TYPEDEF_STD_VECTOR_PTR(xr_surface)

inline void xr_raw_surface::clear()
{
	texture = UINT16_MAX;
	eshader = UINT16_MAX;
	gamemtl = UINT16_MAX;
	flags = 0;
}

inline void xr_raw_surface::set(uint16_t _texture, uint16_t _eshader, uint16_t _gamemtl, uint16_t _flags)
{
	texture = _texture;
	eshader = _eshader;
	gamemtl = _gamemtl;
	flags = _flags;
}

inline bool xr_raw_surface::collision() const { return !!(flags & RSF_COLLISION); }
inline bool xr_raw_surface::rendering() const { return !!(flags & RSF_RENDERING); }
inline bool xr_raw_surface::lightmap() const { return !!(flags & RSF_LIGHTMAP); }
inline bool xr_raw_surface::two_sided() const { return !!(flags & RSF_TWO_SIDED); }
inline bool xr_raw_surface::automatic() const { return !!(flags & RSF_AUTOMATIC); }
inline bool xr_raw_surface::debug() const { return !!(flags & RSF_DEBUG); }

inline int xr_raw_surface::compare(const xr_raw_surface& right) const
{
	return (blob64 < right.blob64) ? -1 : ((blob64 == right.blob64) ? 0 : +1);
}
inline bool xr_raw_surface::operator<(const xr_raw_surface& right) const { return blob64 < right.blob64; }
inline bool xr_raw_surface::operator==(const xr_raw_surface& right) const { return blob64 == right.blob64; }

inline std::string& xr_surface::name() { return m_name; }
inline const std::string& xr_surface::name() const { return m_name; }
inline std::string& xr_surface::eshader() { return m_eshader; }
inline const std::string& xr_surface::eshader() const { return m_eshader; }
inline std::string& xr_surface::cshader() { return m_cshader; }
inline const std::string& xr_surface::cshader() const { return m_cshader; }
inline std::string& xr_surface::gamemtl() { return m_gamemtl; }
inline const std::string& xr_surface::gamemtl() const { return m_gamemtl; }
inline std::string& xr_surface::texture() { return m_texture; }
inline const std::string& xr_surface::texture() const { return m_texture; }
inline std::string& xr_surface::vmap() { return m_vmap; }
inline const std::string& xr_surface::vmap() const { return m_vmap; }
inline uint32_t& xr_surface::flags() { return m_flags; }
inline uint32_t xr_surface::flags() const { return m_flags; }

inline bool xr_surface::two_sided() const { return !!(m_flags & ESF_TWO_SIDED); }
inline void xr_surface::set_two_sided() { m_flags = ESF_TWO_SIDED; }

inline bool xr_surface::operator==(const xr_surface& right) const { return compare(right) == 0; }
inline bool xr_surface::operator<(const xr_surface& right) const { return compare(right) < 0; }

} // end of namespace xray_re

#endif
