//
// i- X-Ray game object (*.ogf)
// i- X-Ray game skeletal motions (*.omf)
// i- X-Ray game detail object (*.dm)
// ie X-Ray object (*.object)
// -e X-Ray skeletal object (*.object)
// -e X-Ray skeletal motion (*.skl)
// i- X-Ray skeletal motions (.skls;*.skl)

#define NOMINMAX
#include <cstring>
#include <algorithm>
#include <map>
#include <maya/MTypes.h>
#if MAYA_API_VERSION >= 20180000 && MAYA_API_VERSION <= 20190200
#include <maya/MCppCompat.h>
#endif
#include <maya/MGlobal.h>
#include <maya/MFnPlugin.h>
#include <maya/MPxFileTranslator.h>
#include <maya/MFnTransform.h>
#include <maya/MFnAnimCurve.h>
#include <maya/MFnDependencyNode.h>
#include <maya/MEulerRotation.h>
#include <maya/MDistance.h>
#include <maya/MDGModifier.h>
#include <maya/MTimeArray.h>
#include <maya/MDoubleArray.h>
#include <maya/MSelectionList.h>
#include <maya/MDagPath.h>
#include <maya/MPlug.h>
#include "maya_import_tools.h"
#include "maya_export_tools.h"
#include "maya_xray_material.h"
#include "maya_progress.h"
#include "xr_file_system.h"
#include "xr_log.h"
#include "xr_object.h"
#include "xr_dm.h"
#include "xr_string_utils.h"
#include "xr_ogf.h"
#include "xr_ogf_v4.h"
#include "xr_skl_motion.h"
#include "xr_obj_motion.h"
#include "xr_object.h"
#include "xr_sdk_version.h"

using namespace xray_re;

const char PLUGIN_VENDOR[] = "ZENOBIAN mod team";
const char PLUGIN_VERSION[] = __DATE__;
const char BUILD_DATE[] = __DATE__ " at " __TIME__;

const MString dm_reader("X-Ray game detail object");
const MString object_reader("X-Ray object");
const MString object_writer("X-Ray object");
const MString skl_object_writer("X-Ray skeletal object");
const MString ogf_reader("X-Ray game object");
const MString omf_reader("X-Ray game skeletal motions");
const MString skl_translator("X-Ray skeletal motion");
const MString skls_reader("X-Ray skeletal motions");
const MString anm_writer("X-Ray camera motion");

class maya_dm_reader: public MPxFileTranslator {
public:
	virtual MStatus		reader(const MFileObject& file, const MString& options, FileAccessMode mode);
	virtual bool		haveReadMethod() const;
	virtual bool		canBeOpened() const;
	virtual MString		defaultExtension() const;
	virtual MString		filter() const;
	virtual MFileKind	identifyFile(const MFileObject& file, const char* buffer, short size) const;

	static void*		creator();
};

class maya_object_reader: public MPxFileTranslator {
public:
	virtual MStatus		reader(const MFileObject& file, const MString& options, FileAccessMode mode);
	virtual bool		haveReadMethod() const;
	virtual MString		defaultExtension() const;
	virtual MString		filter() const;
	virtual bool		canBeOpened() const;
	virtual MFileKind	identifyFile(const MFileObject& file, const char* buffer, short size) const;

	static void*		creator();
};

class maya_object_writer: public MPxFileTranslator {
public:
	virtual MStatus		writer(const MFileObject& file, const MString& options, FileAccessMode mode);
	virtual bool		haveWriteMethod() const;
	virtual MString		defaultExtension() const;
	virtual MString		filter() const;
	virtual MFileKind	identifyFile(const MFileObject& file, const char* buffer, short size) const;

	static void*		creator();
};

class maya_skl_object_writer: public MPxFileTranslator {
public:
	virtual MStatus		writer(const MFileObject& file, const MString& options, FileAccessMode mode);
	virtual bool		haveWriteMethod() const;
	virtual MString		defaultExtension() const;
	virtual MString		filter() const;
	virtual MFileKind	identifyFile(const MFileObject& file, const char* buffer, short size) const;

	static void*		creator();
};

class maya_ogf_reader: public MPxFileTranslator {
public:
	virtual MStatus		reader(const MFileObject& file, const MString& options, FileAccessMode mode);
	virtual bool		haveReadMethod() const;
	virtual bool		canBeOpened() const;
	virtual MString		defaultExtension() const;
	virtual MString		filter() const;
	virtual MFileKind	identifyFile(const MFileObject& file, const char* buffer, short size) const;

	static void*		creator();
};

class maya_omf_reader: public MPxFileTranslator {
public:
	virtual MStatus		reader(const MFileObject& file, const MString& options, FileAccessMode mode);
	virtual bool		haveReadMethod() const;
	virtual MString		defaultExtension() const;
	virtual MString		filter() const;
	virtual MFileKind	identifyFile(const MFileObject& file, const char* buffer, short size) const;

	static void*		creator();
};

class maya_skl_translator: public MPxFileTranslator {
public:
	virtual MStatus		reader(const MFileObject& file, const MString& options, FileAccessMode mode);
	virtual MStatus		writer(const MFileObject& file, const MString& options, FileAccessMode mode);
	virtual bool		haveReadMethod() const;
	virtual bool		haveWriteMethod() const;
	virtual MString		defaultExtension() const;
	virtual MString		filter() const;
	virtual MFileKind	identifyFile(const MFileObject& file, const char* buffer, short size) const;

	static void*		creator();
};

class maya_skls_reader: public MPxFileTranslator {
public:
	virtual MStatus		reader(const MFileObject& file, const MString& options, FileAccessMode mode);
	virtual bool		haveReadMethod() const;
	virtual MString		defaultExtension() const;
	virtual MString		filter() const;
	virtual MFileKind	identifyFile(const MFileObject& file, const char* buffer, short size) const;

	static void*		creator();
};

class maya_anm_writer: public MPxFileTranslator {
public:
	virtual MStatus		reader(const MFileObject& file, const MString& options, FileAccessMode mode);
	virtual MStatus		writer(const MFileObject& file, const MString& options, FileAccessMode mode);
	virtual bool		haveReadMethod() const;
	virtual bool		haveWriteMethod() const;
	virtual bool		canBeOpened() const;
	virtual MString		defaultExtension() const;
	virtual MString		filter() const;
	virtual MFileKind	identifyFile(const MFileObject& file, const char *buffer, short size) const;

	static void*		creator();
};

static inline MString extract_extension(const MFileObject& file)
{
	MString name(file.resolvedName());
	// FIXME: assumes there _is_ extension.
	return name.substring(name.rindex('.') + 1, name.numChars() - 1).toLowerCase();
}

MStatus maya_dm_reader::reader(const MFileObject& file, const MString& options, FileAccessMode mode)
{
	

	MStatus status = MS::kFailure;
	if (mode == kImportAccessMode || mode == kOpenAccessMode) {
		start_progress(2, "Loading DM");
		const MString path = file.resolvedFullName();
		xr_dm* dm = new xr_dm;
		if (dm->load_dm(path.asChar())) {
			advance_progress();
			dm->to_object();
			advance_progress();
			end_progress();
			maya_import_tools(dm, &status, "smoothing_mode=normals");
		} else {
			msg("xray_re: can't open %s", path.asUTF8());
			MGlobal::displayError(MString("xray_re: can't open ") + path);
			end_progress();
		}
		delete dm;
	}
	return status;
}

bool maya_dm_reader::haveReadMethod() const { return true; }

bool maya_dm_reader::canBeOpened() const { return true; }

MString maya_dm_reader::defaultExtension() const { return MString("dm"); }

MString maya_dm_reader::filter() const { return MString("*.dm"); }

MPxFileTranslator::MFileKind maya_dm_reader::identifyFile(const MFileObject& file, const char* buffer, short size) const
{
	return extract_extension(file) == defaultExtension() ? kIsMyFileType : kNotMyFileType;
}

void* maya_dm_reader::creator() { return new maya_dm_reader; }

// forward declaration: defined further below, used here by maya_object_reader::reader
static void split_object_meshes_by_material(xr_object* object);

MStatus maya_object_reader::reader(const MFileObject& file, const MString& options, FileAccessMode mode)
{
	

	MStatus status = MS::kFailure;
	if (mode == kImportAccessMode || mode == kOpenAccessMode) {
		const MString path = file.resolvedFullName();
		xr_object* object = new xr_object;
		if (object->load_object(path.asChar())) {
			object->calculate_bind();
			bool replace_mode = strstr(options.asChar(), "attach_to_selection=true") != 0;
			bool split_explicitly_disabled = strstr(options.asChar(), "split_parts=false") != 0;
			if (!replace_mode && !split_explicitly_disabled)
				split_object_meshes_by_material(object);

			MString base_name = file.resolvedName();
			int dot = base_name.rindex('.');
			if (dot > 0)
				base_name = base_name.substring(0, dot - 1);
			MString combined_options = options;
			if (combined_options.length())
				combined_options += ";";
			combined_options += "group_name=";
			combined_options += base_name;

			maya_import_tools(object, &status, combined_options);
		} else {
			msg("xray_re: can't open %s", path.asUTF8());
			MGlobal::displayError(MString("xray_re: can't open ") + path);
		}
		delete object;
	}
	return status;
}

bool maya_object_reader::haveReadMethod() const { return true; }

MString maya_object_reader::defaultExtension() const { return MString("object"); }

MString maya_object_reader::filter() const
{
#	if (MAYA_API_VERSION >= 201100) 
		return MString("*.object");
#	else
		return MString("*.ob*");
#	endif
}

bool maya_object_reader::canBeOpened() const { return true; }

MPxFileTranslator::MFileKind maya_object_reader::identifyFile(const MFileObject& file, const char* buffer, short size) const
{
	return extract_extension(file) == defaultExtension() ? kIsMyFileType : kNotMyFileType;
}

void* maya_object_reader::creator() { return new maya_object_reader; }

MStatus maya_object_writer::writer(const MFileObject& file, const MString& options, FileAccessMode mode)
{
	

	switch (mode) {
	case kExportAccessMode:
	case kSaveAccessMode:
	case kExportActiveAccessMode:
		break;
	default:
		return MS::kFailure;
	}

	maya_export_tools tools(options);

	return tools.export_object(file.resolvedFullName().asChar(),
		mode == kExportActiveAccessMode);
}

bool maya_object_writer::haveWriteMethod() const { return true; }

MString maya_object_writer::defaultExtension() const { return MString("object"); }

MString maya_object_writer::filter() const
{
#	if (MAYA_API_VERSION >= 201100) 
		return MString("*.object");
#	else
		return MString("*.ob*");
#	endif
}

MPxFileTranslator::MFileKind maya_object_writer::identifyFile(const MFileObject& file, const char* buffer, short size) const
{
	return extract_extension(file) == defaultExtension() ? kIsMyFileType : kNotMyFileType;
}

void* maya_object_writer::creator() { return new maya_object_writer; }

MStatus maya_skl_object_writer::writer(const MFileObject& file, const MString& options, FileAccessMode mode)
{
	

	switch (mode) {
	case kExportAccessMode:
	case kSaveAccessMode:
	case kExportActiveAccessMode:
		break;
	default:
		return MS::kFailure;
	}

	maya_export_tools tools(options);

	return tools.export_skl_object(file.resolvedFullName().asChar(),
		mode == kExportActiveAccessMode);
}

bool maya_skl_object_writer::haveWriteMethod() const { return true; }

MString maya_skl_object_writer::defaultExtension() const { return MString("object"); }

MString maya_skl_object_writer::filter() const
{
#	if (MAYA_API_VERSION >= 201100) 
		return MString("*.object");
#	else
		return MString("*.ob*");
#	endif
}

MPxFileTranslator::MFileKind maya_skl_object_writer::identifyFile(const MFileObject& file, const char* buffer, short size) const
{
	return extract_extension(file) == defaultExtension() ? kIsMyFileType : kNotMyFileType;
}

void* maya_skl_object_writer::creator() { return new maya_skl_object_writer; }

static xr_vmap* find_or_create_local_vmap(xr_vmap_vec& local_vmaps,
		std::map<xr_vmap*, xr_vmap*>& vmap_remap, xr_vmap* src_vmap)
{
	std::map<xr_vmap*, xr_vmap*>::iterator it = vmap_remap.find(src_vmap);
	if (it != vmap_remap.end())
		return it->second;

	unsigned dimension = (src_vmap->type() == xr_vmap::VMT_UV) ? 2 : 1;
	xr_vmap* local_vmap = xr_vmap::create(src_vmap->name().c_str(), src_vmap->type(),
		dimension, false);
	local_vmaps.push_back(local_vmap);
	vmap_remap[src_vmap] = local_vmap;
	return local_vmap;
}

static void split_object_meshes_by_material(xr_object* object)
{
	xr_mesh_vec original_meshes = object->meshes();
	object->meshes().clear();

	for (xr_mesh_vec_it mit = original_meshes.begin(), mend = original_meshes.end();
			mit != mend; ++mit) {
		xr_mesh* src_mesh = *mit;
		const xr_surfmap_vec& surfmaps = src_mesh->surfmaps();

		if (surfmaps.size() <= 1) {
			object->meshes().push_back(src_mesh);
			continue;
		}

		const std::vector<fvector3>& src_points = src_mesh->points();
		const lw_face_vec& src_faces = src_mesh->faces();
		const lw_vmref_vec& src_vmrefs = src_mesh->vmrefs();
		const xr_vmap_vec& src_vmaps = src_mesh->vmaps();
		const fvector3_vec& src_vnorm = src_mesh->vnorm();
		const fvector3_vec& src_cnorm = src_mesh->cnorm();

		for (xr_surfmap_vec_cit sit = surfmaps.begin(), send = surfmaps.end();
				sit != send; ++sit) {
			const xr_surfmap* smap = *sit;
			if (smap->faces.empty())
				continue;

			xr_mesh* part = new xr_mesh;
			part->name() = smap->surface->name();
			part->flags() = src_mesh->flags();

			std::map<uint32_t, uint32_t> point_remap;	// src point idx -> local idx
			std::map<xr_vmap*, xr_vmap*> vmap_remap;	// src vmap -> local vmap
			xr_surfmap* local_smap = new xr_surfmap(smap->surface);

			for (std::vector<uint32_t>::const_iterator fit = smap->faces.begin(),
					fend = smap->faces.end(); fit != fend; ++fit) {
				const lw_face& src_face = src_faces[*fit];
				lw_face local_face;

				for (uint_fast32_t i = 0; i != 3; ++i) {
					uint32_t src_v = src_face.v[i];
					std::map<uint32_t, uint32_t>::iterator pit = point_remap.find(src_v);
					uint32_t local_v;
					if (pit != point_remap.end()) {
						local_v = pit->second;
					} else {
						local_v = uint32_t(part->points().size());
						part->points().push_back(src_points[src_v]);
						if (!src_vnorm.empty())
							part->vnorm().push_back(src_vnorm[src_v]);
						point_remap[src_v] = local_v;
					}
					local_face.v[i] = local_v;

					if (!src_cnorm.empty()) {
						uint32_t src_corner = uint32_t(*fit)*3 + i;
						part->cnorm().push_back(src_cnorm[src_corner]);
					}

					const lw_vmref& src_ref = src_vmrefs[src_face.ref[i]];
					lw_vmref local_ref;
					for (lw_vmref::const_iterator rit = src_ref.begin(),
							rend = src_ref.end(); rit != rend; ++rit) {
						xr_vmap* src_vmap = src_vmaps[rit->vmap];
						xr_vmap* local_vmap = find_or_create_local_vmap(
							part->vmaps(), vmap_remap, src_vmap);
						uint32_t local_offset;
						if (src_vmap->type() == xr_vmap::VMT_UV) {
							const xr_uv_vmap* src_uv = static_cast<const xr_uv_vmap*>(src_vmap);
							local_offset = static_cast<xr_uv_vmap*>(local_vmap)->add_uv(
								src_uv->uvs()[rit->offset], local_v);
						} else {
							const xr_weight_vmap* src_w = static_cast<const xr_weight_vmap*>(src_vmap);
							local_offset = static_cast<xr_weight_vmap*>(local_vmap)->add_weight(
								src_w->weights()[rit->offset], local_v);
						}
						uint32_t local_vmap_idx = uint32_t(std::find(part->vmaps().begin(),
							part->vmaps().end(), local_vmap) - part->vmaps().begin());
						local_ref.push_back(lw_vmref_entry(local_vmap_idx, local_offset));
					}
					part->vmrefs().push_back(local_ref);
					local_face.ref[i] = uint32_t(part->vmrefs().size() - 1);
				}
				local_smap->faces.push_back(uint32_t(part->faces().size()));
				part->faces().push_back(local_face);
			}

			part->surfmaps().push_back(local_smap);
			part->calculate_bbox();
			object->meshes().push_back(part);
		}

		delete src_mesh;
	}
}

static void split_ogf_children(xr_ogf* ogf)
{
	if (!ogf->hierarchical() || ogf->children().empty())
		return;

	const xr_ogf_vec& children = ogf->children();

	for (size_t i = 0, n = children.size(); i != n; ++i) {
		xr_ogf* child = children[i];

		if (child->hierarchical() && !child->children().empty()) {
			split_ogf_children(child);
			for (xr_mesh_vec_it mit = child->meshes().begin(),
					mend = child->meshes().end(); mit != mend; ++mit) {
				ogf->meshes().push_back(*mit);
			}
			child->meshes().clear();
			for (xr_surface_vec_it sit = child->surfaces().begin(),
					send = child->surfaces().end(); sit != send; ++sit) {
				ogf->surfaces().push_back(*sit);
			}
			child->surfaces().clear();
			continue;
		}

		child->bones() = ogf->bones();

		child->to_object();
		child->bones().clear();
		if (child->meshes().empty())
			continue;

		xr_mesh* mesh = child->meshes().front();

		std::string part_name;
		if (!mesh->surfmaps().empty() && mesh->surfmaps().front()->surface)
			part_name = mesh->surfmaps().front()->surface->name();
		if (part_name.empty()) {
			part_name = child->shader();
			std::string::size_type pos = part_name.find_last_of("\\/");
			if (pos != std::string::npos)
				part_name = part_name.substr(pos + 1);
		}
		if (part_name.empty()) {
			char buf[32];
			xr_snprintf(buf, sizeof(buf), "part_%u", unsigned(i));
			part_name = buf;
		}
		mesh->name() = part_name;

		ogf->meshes().push_back(mesh);
		child->meshes().clear();	

		for (xr_surface_vec_it sit = child->surfaces().begin(),
				send = child->surfaces().end(); sit != send; ++sit) {
			ogf->surfaces().push_back(*sit);
		}
		child->surfaces().clear();
	}
}

MStatus maya_ogf_reader::reader(const MFileObject& file, const MString& options, FileAccessMode mode)
{
	

	MStatus status = MS::kFailure;
	if (mode == kImportAccessMode || mode == kOpenAccessMode) {
		start_progress(2, "Loading OGF");
		const MString path = file.resolvedFullName();
		xr_ogf* ogf = xr_ogf::load_ogf(path.asChar());
		if (ogf) {
			advance_progress();
			ogf->calculate_bind();
			bool replace_mode = strstr(options.asChar(), "attach_to_selection=true") != 0;
			bool split_explicitly_disabled = strstr(options.asChar(), "split_parts=false") != 0;
			if (!replace_mode && !split_explicitly_disabled && ogf->hierarchical() && !ogf->children().empty())
				split_ogf_children(ogf);
			else
				ogf->to_object();
			advance_progress();
			end_progress();
			MString smoothing_mode = "normals";
			const char* sm = strstr(options.asChar(), "smoothing_mode=");
			if (sm) {
				sm += strlen("smoothing_mode=");
				const char* end = strchr(sm, ';');
				smoothing_mode = end ? MString(sm, int(end - sm)) : MString(sm);
			}
			MString combined_options("smoothing_mode=");
			combined_options += smoothing_mode;
			if (replace_mode)
				combined_options += ";attach_to_selection=true";

			MString base_name = file.resolvedName();
			int dot = base_name.rindex('.');
			if (dot > 0)
				base_name = base_name.substring(0, dot - 1);
			combined_options += ";group_name=";
			combined_options += base_name;

			maya_import_tools(ogf, &status, combined_options);
			delete ogf;
		} else {
			msg("xray_re: can't open %s", path.asUTF8());
			MGlobal::displayError(MString("xray_re: can't open ") + path);
			end_progress();
		}
	}
	return status;
}

bool maya_ogf_reader::haveReadMethod() const { return true; }

bool maya_ogf_reader::canBeOpened() const { return true; }

MString maya_ogf_reader::defaultExtension() const { return MString("ogf"); }

MString maya_ogf_reader::filter() const { return MString("*.ogf"); }

MPxFileTranslator::MFileKind maya_ogf_reader::identifyFile(const MFileObject& file, const char* buffer, short size) const
{
	return extract_extension(file) == defaultExtension() ? kIsMyFileType : kNotMyFileType;
}

void* maya_ogf_reader::creator() { return new maya_ogf_reader; }

MStatus maya_omf_reader::reader(const MFileObject& file, const MString& options, FileAccessMode mode)
{
	

	MStatus status = MS::kFailure;
	if (mode == kImportAccessMode) {
		start_progress(1, "Loading OMF");
		const MString path = file.resolvedFullName();
		xr_ogf_v4* omf = new xr_ogf_v4;
		if (omf->load_omf(path.asChar())) {
			advance_progress();
			maya_import_tools imp_tools;
			MObject character_obj = imp_tools.lookup_character(&status);
			if (status) {
				imp_tools.reset_animation_state();
				status = imp_tools.import_motions(omf->motions(), character_obj);
			}
			end_progress();
		} else {
			msg("xray_re: can't open %s", path.asUTF8());
			MGlobal::displayError(MString("xray_re: can't open ") + path);
			end_progress();
		}
		delete omf;
	}
	return status;
}

bool maya_omf_reader::haveReadMethod() const { return true; }

MString maya_omf_reader::defaultExtension() const { return MString("omf"); }

MString maya_omf_reader::filter() const { return MString("*.omf"); }

MPxFileTranslator::MFileKind maya_omf_reader::identifyFile(const MFileObject& file, const char* buffer, short size) const
{
	return extract_extension(file) == defaultExtension() ? kIsMyFileType : kNotMyFileType;
}

void* maya_omf_reader::creator() { return new maya_omf_reader; }

MStatus maya_skl_translator::reader(const MFileObject& file, const MString& options, FileAccessMode mode)
{
	

	MStatus status = MS::kFailure;
	if (mode == kImportAccessMode) {
		maya_import_tools imp_tools;
		const MString path = file.resolvedFullName();
		xr_skl_motion* smotion = new xr_skl_motion;
		if (!smotion->load_skl(path.asChar())) {
			msg("xray_re: can't open %s", path.asUTF8());
			MGlobal::displayError(MString("xray_re: can't open ") + path);
			delete smotion;
			return MS::kFailure;
		}
		MObject character_obj = imp_tools.lookup_character(&status);
		if (status) {
			imp_tools.reset_animation_state();
			status = imp_tools.import_motion(smotion, character_obj);
		}
		delete smotion;
	}
	return status;
}

MStatus maya_skl_translator::writer(const MFileObject& file, const MString& options, FileAccessMode mode)
{
	

	switch (mode) {
	case kExportAccessMode:
	case kSaveAccessMode:
	case kExportActiveAccessMode:
		break;
	default:
		return MS::kFailure;
	}

	return maya_export_tools().export_skl(file.resolvedFullName().asChar(),
			mode == kExportActiveAccessMode);
}

bool maya_skl_translator::haveReadMethod() const { return true; }

bool maya_skl_translator::haveWriteMethod() const { return true; }

MString maya_skl_translator::defaultExtension() const { return MString("skl"); }

MString maya_skl_translator::filter() const { return MString("*.skl"); }

MPxFileTranslator::MFileKind maya_skl_translator::identifyFile(const MFileObject& file, const char* buffer, short size) const
{
	return extract_extension(file) == defaultExtension() ? kIsMyFileType : kNotMyFileType;
}

void* maya_skl_translator::creator() { return new maya_skl_translator; }

MStatus maya_skls_reader::reader(const MFileObject& file, const MString& options, FileAccessMode mode)
{
	

	MStatus status = MS::kFailure;
	if (mode == kImportAccessMode) {
		maya_import_tools imp_tools;
		const MString path = file.resolvedFullName();
		xr_object* object = new xr_object;
		if (!object->load_skls(path.asChar())) {
			msg("xray_re: can't open %s", path.asUTF8());
			MGlobal::displayError(MString("xray_re: can't open ") + path);
			delete object;
			return MS::kFailure;
		}
		MObject character_obj = imp_tools.lookup_character(&status);
		if (status) {
			imp_tools.reset_animation_state();
			status = imp_tools.import_motions(object->motions(), character_obj);
		}
		delete object;
	}
	return status;
}

bool maya_skls_reader::haveReadMethod() const { return true; }

MString maya_skls_reader::defaultExtension() const { return MString("skls"); }

MString maya_skls_reader::filter() const
{
#	if (MAYA_API_VERSION >= 201100) 
		return MString("*.skls");
#	else
		return MString("*.sk*");
#	endif
}

MPxFileTranslator::MFileKind maya_skls_reader::identifyFile(const MFileObject& file, const char* buffer, short size) const
{
	return extract_extension(file) == defaultExtension() ? kIsMyFileType : kNotMyFileType;
}

void* maya_skls_reader::creator() { return new maya_skls_reader; }

// exact copy of the helper of the same name in maya_import_tools.cxx (static,
// file-local scope there, so it isn't visible from this translation unit).
static inline void append_key(MTimeArray& times, MDoubleArray& values, double time, double value)
{
	unsigned size = values.length();
	if (size == 0 || values[size-1] != value) {
		times.append(MTime(time, MTime::kSeconds));
		values.append(value);
	}
}

MStatus maya_anm_writer::reader(const MFileObject& file, const MString& options, FileAccessMode mode)
{
	MStatus status = MS::kFailure;
	if (mode != kImportAccessMode && mode != kOpenAccessMode)
		return status;

	MSelectionList s_list;
	MGlobal::getActiveSelectionList(s_list);
	if (s_list.length() != 1) {
		msg("xray_re: select exactly one object to apply the .anm to");
		MGlobal::displayError("xray_re: select exactly one object to apply the .anm to");
		return status;
	}

	MDagPath dp;
	status = s_list.getDagPath(0, dp);
	if (!status) {
		msg("xray_re: selected object has no transform");
		MGlobal::displayError("xray_re: selected object has no transform");
		return status;
	}
	MFnTransform transform_fn(dp, &status);
	CHECK_MSTATUS_AND_RETURN_IT(status);

	const MString path = file.resolvedFullName();
	xr_obj_motion anm;
	if (!anm.load_anm(path.asChar())) {
		msg("xray_re: can't open %s", path.asUTF8());
		MGlobal::displayError(MString("xray_re: can't open ") + path);
		return MS::kFailure;
	}

	double fps = anm.fps();
	if (fps <= 0.0)
		fps = 30.0;

	MTimeArray times[6];
	MDoubleArray values[6];

	for (int32_t frame = anm.frame_start(), frame_end = anm.frame_end();
			frame < frame_end; ++frame) {
		double time = frame/fps;

		fvector3 offs, rot;
		anm.evaluate(float(time), offs, rot);

		append_key(times[0], values[0], time, MDistance(offs.x, MDistance::kMeters).asCentimeters());
		append_key(times[1], values[1], time, MDistance(offs.y, MDistance::kMeters).asCentimeters());
		append_key(times[2], values[2], time, MDistance(-offs.z, MDistance::kMeters).asCentimeters());

		MEulerRotation maya_rot(-rot.x, -rot.y, rot.z, MEulerRotation::kZXY);
		maya_rot.reorderIt(MEulerRotation::kXYZ);
		append_key(times[3], values[3], time, maya_rot.x);
		append_key(times[4], values[4], time, maya_rot.y);
		append_key(times[5], values[5], time, maya_rot.z);
	}

	static const MString k_plug_names[6] = { "tx", "ty", "tz", "rx", "ry", "rz" };
	for (uint_fast32_t i = 6; i != 0;) {
		--i;
		MFnAnimCurve curve_fn;
		MObject curve_obj = curve_fn.create(i >= 3 ?
				MFnAnimCurve::kAnimCurveTA : MFnAnimCurve::kAnimCurveTL,
				0, &status);
		CHECK_MSTATUS(status);
		status = curve_fn.addKeys(&times[i], &values[i],
				MFnAnimCurve::kTangentStep, MFnAnimCurve::kTangentStep);
		CHECK_MSTATUS(status);
		MPlug plug = transform_fn.findPlug(k_plug_names[i], true, &status);
		CHECK_MSTATUS(status);
		MFnDependencyNode curve_dep_fn(curve_obj);
		MPlug output_plug = curve_dep_fn.findPlug("output", true, &status);
		CHECK_MSTATUS(status);
		MDGModifier dg_modifier;
		status = dg_modifier.connect(output_plug, plug);
		CHECK_MSTATUS(status);
		dg_modifier.doIt();
	}

	msg("xray_re: imported anm motion %s onto %s", anm.name().c_str(), transform_fn.name().asChar());
	MGlobal::displayInfo(MString("xray_re: imported anm motion ") + anm.name().c_str() +
		" onto " + transform_fn.name());

	return MS::kSuccess;
}

MStatus maya_anm_writer::writer(const MFileObject& file, const MString& options, FileAccessMode mode)
{
	

	switch(mode)
	{
	case kExportAccessMode:
	case kSaveAccessMode:
	case kExportActiveAccessMode:
		break;
	default:
		return MS::kFailure;
	}

	return maya_export_tools().export_anm(file.resolvedFullName().asChar(), mode == kExportActiveAccessMode);
}

bool maya_anm_writer::haveReadMethod() const { return true; }

bool maya_anm_writer::haveWriteMethod() const { return true; }

bool maya_anm_writer::canBeOpened() const { return true; }

MString maya_anm_writer::defaultExtension() const { return MString("anm"); }

MString maya_anm_writer::filter() const { return MString("*.anm"); }

MPxFileTranslator::MFileKind maya_anm_writer::identifyFile(const MFileObject& file, const char* buffer, short size) const
{
	return extract_extension(file) == defaultExtension() ? kIsMyFileType : kNotMyFileType;
}

void* maya_anm_writer::creator() { return new maya_anm_writer; }

MStatus initializePlugin(MObject obj)
{
	MStatus status;

	MString fs_spec("$MAYA_LOCATION\\bin\\xray_path.ltx");
	xr_file_system& fs = xr_file_system::instance();
	if (!fs.initialize(fs_spec.expandEnvironmentVariablesAndTilde().asChar())) {
		msg("xray_re: can't initialize the file system");
		MGlobal::displayError("xray_re: can't initialize the file system");
		return MS::kFailure;
	}
	xr_log::instance().init("xrayMayaTools");
	msg("X-Ray Maya tools for Maya %s ", MGlobal::mayaVersion().asChar());
	MGlobal::displayInfo(MString("X-Ray Maya tools for Maya ") + MGlobal::mayaVersion());
	msg("xray_re built on %s ", BUILD_DATE);
	MGlobal::displayInfo(MString("xray_re built on ") + BUILD_DATE);

	MFnPlugin plugin_fn(obj, PLUGIN_VENDOR, PLUGIN_VERSION);
	if (!(status = maya_xray_material::initialize(plugin_fn)))
		return status;
	if (!(status = plugin_fn.registerFileTranslator(object_reader, "", maya_object_reader::creator, "xray_re_object_import_options", "", true)))
		return status;
	if (!(status = plugin_fn.registerFileTranslator(object_writer, "", maya_object_writer::creator, "xray_re_object_export_options", "", true)))
		return status;
	if (!(status = plugin_fn.registerFileTranslator(skl_object_writer, "", maya_skl_object_writer::creator, "xray_re_object_export_options", "", true)))
		return status;
	if (!(status = plugin_fn.registerFileTranslator(dm_reader, "", maya_dm_reader::creator, "", "", true)))
		return status;
	if (!(status = plugin_fn.registerFileTranslator(ogf_reader, "", maya_ogf_reader::creator, "xray_re_object_import_options", "", true)))
		return status;
	if (!(status = plugin_fn.registerFileTranslator(omf_reader, "", maya_omf_reader::creator, "", "", true)))
		return status;
	if (!(status = plugin_fn.registerFileTranslator(skl_translator, "", maya_skl_translator::creator, "", "", true)))
		return status;
	if (!(status = plugin_fn.registerFileTranslator(skls_reader, "", maya_skls_reader::creator, "", "", true)))
		return status;
	if (!(status = plugin_fn.registerFileTranslator(anm_writer, "", maya_anm_writer::creator, "", "", true)))
		return status;

	return status;
}

MStatus uninitializePlugin(MObject obj)
{
	MFnPlugin plugin_fn(obj);
	maya_xray_material::uninitialize(plugin_fn);
	plugin_fn.deregisterFileTranslator(object_reader);
	plugin_fn.deregisterFileTranslator(object_writer);
	plugin_fn.deregisterFileTranslator(skl_object_writer);
	plugin_fn.deregisterFileTranslator(dm_reader);
	plugin_fn.deregisterFileTranslator(ogf_reader);
	plugin_fn.deregisterFileTranslator(omf_reader);
	plugin_fn.deregisterFileTranslator(skl_translator);
	plugin_fn.deregisterFileTranslator(skls_reader);
	plugin_fn.deregisterFileTranslator(anm_writer);

	return MS::kSuccess;
}
