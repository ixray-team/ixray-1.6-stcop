//
// OGF v3 - builds 1098, 1114, 1154
// OGF v4 - builds 2215, 2945, 2939, 2947+
// OGF v4+ - builds 3120, 3456+
//
// FIXME: merge with xr_ogf.h?
//
#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_OGF_FORMAT_H__
#define __XR_OGF_FORMAT_H__

#include "xr_vector3.h"
#include "xr_quaternion.h"
#include "xr_aabb.h"
#include "xr_sphere.h"
#include "xr_cylinder.h"
#include "xr_color.h"
#include "xr_skeleton.h"

namespace xray_re {

// OGF chunk ids.
enum ogf_chunk_id {
	OGF_HEADER		= 0x1,

	//build 729
	OGF2_TEXTURE        = 0x2,
	OGF2_TEXTURE_L      = 0x3,
	OGF2_BBOX           = 0x6,
	OGF2_VERTICES       = 0x7,
	OGF2_INDICES        = 0x8,
	OGF2_VCONTAINER     = 0xb,
	OGF2_BSPHERE        = 0xc,

	OGF3_TEXTURE		= 0x2,
	OGF3_TEXTURE_L		= 0x3,
	OGF3_CHILD_REFS		= 0x5,
	OGF3_BBOX			= 0x6,
	OGF3_VERTICES		= 0x7,
	OGF3_INDICES		= 0x8,
	OGF3_LODDATA		= 0x9,	// not sure
	OGF3_VCONTAINER		= 0xa,
	OGF3_BSPHERE		= 0xb,
	OGF3_CHILDREN_L		= 0xc,
	OGF3_S_BONE_NAMES	= 0xd,
	OGF3_S_MOTIONS		= 0xe,// build 1469 - 1580
	OGF3_DPATCH			= 0xf,	// guessed name
	OGF3_LODS			= 0x10,	// guessed name
	OGF3_CHILDREN		= 0x11,
	OGF3_S_SMPARAMS		= 0x12,// build 1469
	OGF3_ICONTAINER		= 0x13,// build 1865
	OGF3_S_SMPARAMS_NEW = 0x14,// build 1472 - 1865
	OGF3_LODDEF2		= 0x15,// build 1865
	OGF3_TREEDEF2		= 0x16,// build 1865
	OGF3_S_IKDATA_0     = 0x17,// build 1475 - 1580
	OGF3_S_USERDATA     = 0x18,// build 1537 - 1865
	OGF3_S_IKDATA       = 0x19,// build 1616 - 1829, 1844
	OGF3_S_MOTIONS_NEW  = 0x1a,// build 1616 - 1865
	OGF3_S_DESC         = 0x1b,// build 1844
	OGF3_S_IKDATA_2     = 0x1C,// build 1842 - 1865
	OGF3_S_MOTION_REFS  = 0x1D,// build 1842

	OGF4_TEXTURE		= 0x2,
	OGF4_VERTICES		= 0x3,
	OGF4_INDICES		= 0x4,
	OGF4_P_MAP			= 0x5,
	OGF4_SWIDATA		= 0x6,
	OGF4_VCONTAINER		= 0x7,
	OGF4_ICONTAINER		= 0x8,
	OGF4_CHILDREN		= 0x9,
	OGF4_CHILDREN_L		= 0xa,
	OGF4_LODDEF2		= 0xb,
	OGF4_TREEDEF2		= 0xc,
	OGF4_S_BONE_NAMES	= 0xd,
	OGF4_S_MOTIONS		= 0xe,
	OGF4_S_SMPARAMS		= 0xf,
	OGF4_S_IKDATA		= 0x10,
	OGF4_S_USERDATA		    = 0x11,
	OGF4_S_DESC             = 0x12,
	OGF4_S_MOTION_REFS_0    = 0x13,	// pre-CS format
	OGF4_SWICONTAINER       = 0x14,
	OGF4_GCONTAINER		= 0x15,
	OGF4_FASTPATH		= 0x16,
	OGF4_S_LODS		= 0x17,
	OGF4_S_MOTION_REFS_1	= 0x18,	// introduced in clear sky
};

// Common OGF data structures.
typedef fbox ogf_bbox;
typedef fsphere ogf_bsphere;

// OGF header definitions.
enum ogf_version {
	OGF2_VERSION    = 2,
	OGF3_VERSION    = 3,
	OGF4_VERSION    = 4,
};

enum ogf_model_type {
	MT3_NORMAL		= 0,	// Fvisual
	MT3_HIERRARHY		= 0x1,	// FHierrarhyVisual
	MT3_PROGRESSIVE		= 0x2,	// FProgressiveFixedVisual
	MT3_SKELETON_GEOMDEF_PM	= 0x3,	// CSkeletonX_PM
	MT3_SKELETON_ANIM	= 0x4,	// CKinematics
	MT3_DETAIL_PATCH	= 0x6,	// FDetailPatch
	MT3_SKELETON_GEOMDEF_ST	= 0x7,	// CSkeletonX_ST
	MT3_CACHED		= 0x8,	// FCached
	MT3_PARTICLE		= 0x9,	// CPSVisual
	MT3_PROGRESSIVE2	= 0xa,	// FProgressive
	MT3_LOD			= 0xb,	// FLOD build 1472 - 1865
	MT3_TREE		= 0xc,	// FTreeVisual build 1472 - 1865
	//				= 0xd,	// CParticleEffect 1844
	//				= 0xe,	// CParticleGroup 1844
	MT3_SKELETON_RIGID	= 0xf,	// CSkeletonRigid 1844

	MT4_NORMAL		= 0,	// Fvisual
	MT4_HIERRARHY		= 0x1,	// FHierrarhyVisual
	MT4_PROGRESSIVE		= 0x2,	// FProgressive
	MT4_SKELETON_ANIM	= 0x3,	// CKinematicsAnimated
	MT4_SKELETON_GEOMDEF_PM	= 0x4,	// CSkeletonX_PM
	MT4_SKELETON_GEOMDEF_ST	= 0x5,	// CSkeletonX_ST
	MT4_LOD			= 0x6,	// FLOD
	MT4_TREE_ST		= 0x7,	// FTreeVisual_ST
	MT4_PARTICLE_EFFECT	= 0x8,	// PS::CParticleEffect
	MT4_PARTICLE_GROUP	= 0x9,	// PS::CParticleGroup
	MT4_SKELETON_RIGID	= 0xa,	// CKinematics
	MT4_TREE_PM		= 0xb,	// FTreeVisual_PM

	MT4_OMF			= 0x40,	// fake model type to distinguish .omf
};

struct ogf3_header {
	uint8_t		version;
	uint8_t		type;
	uint16_t	unused;		// really?
};

struct ogf4_header {
	uint8_t		format_version;
	uint8_t		type;
	uint16_t	shader_id;
	ogf_bbox	bb;
	ogf_bsphere	bs;
};

// Vertex formats.
enum ogf_vertex_format {
	OGF_VERTEXFORMAT_FVF		= 0x112,

	OGF3_VERTEXFORMAT_FVF_1L	= 0x12071980,
	OGF3_VERTEXFORMAT_FVF_2L	= 0x240e3300,

	OGF4_VERTEXFORMAT_FVF_1L	= 0x12071980,
	OGF4_VERTEXFORMAT_FVF_2L	= 0x240e3300,
	OGF4_VERTEXFORMAT_FVF_NL	= 0x36154c80,

	OGF4_VERTEXFORMAT_FVF_1L_CS	= 0x1,
	OGF4_VERTEXFORMAT_FVF_2L_CS	= 0x2,
	OGF4_VERTEXFORMAT_FVF_3L_CS	= 0x3,
	OGF4_VERTEXFORMAT_FVF_4L_CS	= 0x4,
};

struct ogf_vert_render {
	fvector3	p;
	fvector3	n;
	float		u, v;
};

struct ogf3_vert_boned_1w {
	fvector3	p;
	fvector3	n;
	float		u, v;
	uint32_t	matrix;
};

struct ogf4_vert_boned_1w {
	fvector3	p;
	fvector3	n, t, b;
	float		u, v;
	uint32_t	matrix;
};

struct ogf4_vert_boned_2w {
	uint16_t	matrix0, matrix1;
	fvector3	p;
	fvector3	n, t, b;
	float		w;
	float		u, v;
};

// FIXME: check the actual engine code.
struct ogf4_vert_boned_3w {
	uint16_t	matrices[3];
	uint16_t	unused;
	fvector3	p;
	fvector3	n, t, b;
	float		w[2];
	float		u, v;
};

struct ogf4_vert_boned_4w {
	uint16_t	matrices[4];
	fvector3	p;
	fvector3	n, t, b;
	float		w[3];
	float		u, v;
};

enum {
	OGF3_HOPPE_HEADER	= 1,
	OGF3_HOPPE_VERT_SPLITS	= 2,
	OGF3_HOPPE_FIX_FACES	= 3,
};

// OGF v3 LOD information in fixed progressive (Hoppe's PM).
struct ogf3_vsplit {
	uint16_t	vert;
	uint8_t		new_tris;
	uint8_t		fix_faces;
};

// OGF v4 progressive mesh definitions (SWPM).
struct ogf4_slide_window {
	uint32_t	offset;
	uint16_t	num_tris;
	uint16_t	num_verts;
};

// Bone and kinematics definitions are mostly shared with editor object.
const uint32_t OGF3_S_JOINT_IK_DATA_VERSION = 1;
const uint32_t OGF4_S_JOINT_IK_DATA_VERSION = 1;

// Motions definitions.
const uint16_t OGF3_S_SMPARAMS_VERSION_1 = 1;	// 1829
const uint16_t OGF3_S_SMPARAMS_VERSION_3 = 3;	// 1842
const uint16_t OGF4_S_SMPARAMS_VERSION_3 = 3;	// 2215, 2947+
const uint16_t OGF4_S_SMPARAMS_VERSION_4 = 4;	// 3120, 3456+

const float OGF3_MOTION_FPS = 30.f;
const float OGF4_MOTION_FPS = 30.f;

enum ogf3_motion_type {
	SMT_FX		= 0,
	SMT_CYCLE	= 1,
};

enum ogf4_key_presence_flag {
	KPF_T_PRESENT	= 0x01,
	KPF_R_ABSENT	= 0x02,
	KPF_T_HQ	= 0x04,		// 3456+
};

struct ogf_key_qr {
	int16_t				x, y, z, w;
	template<typename T> void	dequantize(_quaternion<T>& q) const;
};

template<typename T> inline void ogf_key_qr::dequantize(_quaternion<T>& q) const
{
	const T m = 1/T(32767);
	q.x = x*m;
	q.y = y*m;
	q.z = z*m;
	q.w = w*m;
}

template<typename T> struct _ogf4_key_qt {
	T	x, y, z;
	void	dequantize(fvector3& value, const fvector3& scale) const;
};

typedef _ogf4_key_qt<int8_t> ogf4_key_qt;
typedef _ogf4_key_qt<int16_t> ogf4_key_qt_hq;

template<typename T> inline void _ogf4_key_qt<T>::dequantize(fvector3& value, const fvector3& scale) const
{
	value.x = x*scale.x;
	value.y = y*scale.y;
	value.z = z*scale.z;
};

struct ogf_motion_def {
	uint16_t	bone_or_part;
	uint16_t	motion;
	float		speed;
	float		power;
	float		accrue;
	float		falloff;
};

// level tree definitions
struct ogf4_5color {
	fvector3	rgb;
	float		hemi;
	float		sun;
};

// level mesh LOD definitions (billboards)
struct ogf4_lod_vertex {
	fvector3	v;
	fvector2	t;
	rgba32		c_rgb_hemi;
	uint8_t		c_sun;
	uint8_t		pad[3];
};

// on-disk format!!!
struct ogf4_lod_face {
	ogf4_lod_vertex	v[4];
};

} // end of namespace xray_re

#endif
