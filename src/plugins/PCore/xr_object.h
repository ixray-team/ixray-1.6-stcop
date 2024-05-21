#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_OBJECT_H__
#define __XR_OBJECT_H__

#include <string>
#include <vector>

#include <map>

#include "xr_bone.h"
#include "xr_skl_motion.h"
#include "xr_surface.h"
#include "xr_surface_factory.h"
#include "xr_mesh.h"

namespace xray_re {

// EOBJ_CHUNK_FLAGS
enum {
	EOF_STATIC		= 0x00,
	EOF_DYNAMIC		= 0x01,
	EOF_PROGRESSIVE		= 0x02,	// use SWPM
	EOF_USING_LOD		= 0x04,	// generate billboards (unused?)
	EOF_HOM			= 0x08,	// level.hom
	EOF_MULTIPLE_USAGE	= 0x14,	// MT4_LOD (trees and vehicles)
	EOF_SOUND_OCCLUDER	= 0x20,	// level.som
};

class xr_reader;
class xr_writer;

#ifdef _MSC_VER
inline size_t hash_value(const xr_raw_surface& surface)
{
#if SIZE_MAX == _UI64_MAX
	return surface.blob64;
#else
	return surface.blob32[0] ^ surface.blob32[1];
#endif
}
#endif

enum class compress_options
{
	none,
	compress,
};

class xr_object: public xr_surface_factory {
public:
			xr_object();
			xr_object(const xr_surface_factory* surface_factory);
	virtual		~xr_object();

	virtual void	clear();
	virtual void	to_object();
	virtual void	denominate();
	virtual void	denominate_surfaces();

	virtual bool	load_object(const char* path);
	virtual void	load_object(xr_reader& r);
	virtual bool	save_object(const char* path, compress_options compress = compress_options::none) const;
	virtual bool	save_object(const char* path, const std::string& name, compress_options compress = compress_options::none) const;
	virtual void	save_object(xr_writer& w) const;

	virtual bool	load_bones(const char* path);
	virtual void	load_bones(xr_reader& r);
	virtual bool	save_bones(const char* path) const;
	virtual void	save_bones(xr_writer& w) const;

	virtual bool	load_skls(const char* path);
	virtual void	load_skls(xr_reader& r);
	virtual bool	save_skls(const char* path) const;
	virtual void	save_skls(xr_writer& w) const;

	xr_surface*	attach(const xr_raw_surface& raw_surface);

	void		calculate_bind();

	xr_surface*	find_surface(const std::string& name);
	xr_mesh*	find_mesh(const std::string& name);
	xr_bone*	find_bone(const std::string& name);
	xr_partition*	find_partition(const std::string& name);
	xr_skl_motion*	find_motion(const std::string& name);

	uint32_t&		flags();
	uint32_t		flags() const;
	const std::string&	userdata() const;
	const std::string&	motion_refs() const;
	xr_surface_vec&		surfaces();
	const xr_surface_vec&	surfaces() const;
	xr_mesh_vec&		meshes();
	const xr_mesh_vec&	meshes() const;
	xr_bone_vec&		bones();
	const xr_bone_vec&	bones() const;
	xr_skl_motion_vec&	motions();
	const xr_skl_motion_vec&motions() const;
	xr_partition_vec&	partitions();
	const xr_partition_vec&	partitions() const;

	const xr_bone*		root_bone() const;

protected:
	void			setup_bones();
	void			setup_partitions();
	virtual xr_surface*	create_surface(const xr_raw_surface& raw_surface) const;

protected:
	uint32_t		m_flags;	// EOBJ_CHUNK_FLAGS
	std::string		m_userdata;	// EOBJ_CHUNK_USERDATA
	std::string		m_lod_ref;	// EOBJ_CHUNK_LOD_REF
	xr_surface_vec		m_surfaces;	// EOBJ_CHUNK_SURFACES_X
	xr_mesh_vec		m_meshes;	// EOBJ_CHUNK_MESHES
	xr_bone_vec		m_bones;	// EOBJ_CHUNK_BONES_X
	xr_skl_motion_vec	m_motions;	// EOBJ_CHUNK_MOTIONS
	std::string		m_motion_refs;	// EOBJ_CHUNK_MOTION_REFS
	xr_partition_vec	m_partitions;	// EOBJ_CHUNK_PARTITIONS_X
	fvector3		m_position;	// EOBJ_CHUNK_TRANSFORM
	fvector3		m_rotation;
	std::string		m_owner_name;	// EOBJ_CHUNK_REVISION
	uint32_t		m_creation_time;
	std::string		m_modif_name;
	uint32_t		m_modified_time;

	const xr_surface_factory* m_surface_factory;
	std::map<xr_raw_surface, xr_surface*>
				m_raw_surfaces;
};

TYPEDEF_STD_VECTOR_PTR(xr_object)

inline uint32_t& xr_object::flags() { return m_flags; }
inline uint32_t xr_object::flags() const { return m_flags; }
inline const std::string& xr_object::userdata() const { return m_userdata; }
inline const std::string& xr_object::motion_refs() const { return m_motion_refs; }
inline xr_surface_vec& xr_object::surfaces() { return m_surfaces; }
inline const xr_surface_vec& xr_object::surfaces() const { return m_surfaces; }
inline xr_mesh_vec& xr_object::meshes() { return m_meshes; }
inline const xr_mesh_vec& xr_object::meshes() const { return m_meshes; }
inline xr_bone_vec& xr_object::bones() { return m_bones; }
inline const xr_bone_vec& xr_object::bones() const { return m_bones; }
inline xr_skl_motion_vec& xr_object::motions() { return m_motions; }
inline const xr_skl_motion_vec& xr_object::motions() const { return m_motions; }
inline xr_partition_vec& xr_object::partitions() { return m_partitions; }
inline const xr_partition_vec& xr_object::partitions() const { return m_partitions; }

} // end of namespace xray_re

#endif
