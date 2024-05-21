// This code is inspired by Maya plugins from NifTools and Valve SDK.

#define NOMINMAX
#include <maya/MTypes.h>

#if MAYA_API_VERSION >= 20180000 && MAYA_API_VERSION <= 20190200
#	include <MCppCompat.h>
#endif

#include <maya/MAnimControl.h>
#include <maya/MDGModifier.h>
#include <maya/MDagPath.h>
#include <maya/MDagPathArray.h>
#include <maya/MDistance.h>
#include <maya/MEulerRotation.h>
#include <maya/MFloatArray.h>
#include <maya/MFnAnimCurve.h>
#include <maya/MFnCharacter.h>
#include <maya/MFnClip.h>
#include <maya/MFnEnumAttribute.h>
#include <maya/MFnIkJoint.h>
#include <maya/MFnMesh.h>
#include <maya/MFnSet.h>
#include <maya/MFnSingleIndexedComponent.h>
#include <maya/MFnSkinCluster.h>
#include <maya/MFnTransform.h>
#include <maya/MGlobal.h>
#include <maya/MIntArray.h>
#include <maya/MItDag.h>
#include <maya/MItDependencyGraph.h>
#include <maya/MItDependencyNodes.h>
#include <maya/MItMeshEdge.h>
#include <maya/MItMeshPolygon.h>
#include <maya/MPlugArray.h>
#include <maya/MPointArray.h>
#include <maya/MProgressWindow.h>
#include <maya/MSelectionList.h>
#include "maya_import_tools.h"
#include "maya_progress.h"
#include "xr_object.h"
#include "xr_envelope.h"
#include "xr_file_system.h"

using namespace xray_re;

static MObject create_texture(const std::string& texture, MStatus* return_status = 0);

maya_import_tools::maya_import_tools(const MString& options)
{
	set_default_options();
	parse_options(options);
}

maya_import_tools::maya_import_tools(const xray_re::xr_object* object, MStatus* return_status, const MString& options)
{
	set_default_options();
	parse_options(options);

	MStatus status = import_object(object);
	if (return_status)
		*return_status = status;
}

maya_import_tools::~maya_import_tools()
{
	MGlobal::clearSelectionList();
}

static MString make_maya_name(const std::string& base, const char* old_suffix, const char* suffix = "")
{
	std::string::size_type pos = base.rfind(old_suffix);
	if (pos == std::string::npos)
		return MString(base.c_str()) + suffix;
	else
		return MString(base.c_str(), int(pos & INT_MAX)) + suffix;
}

MStatus maya_import_tools::import_object(const xr_object* object)
{
	MStatus status = MS::kSuccess;
	const xr_bone_vec& bones = object->bones();

	start_progress(bones.size(), "Importing bones");
	for (xr_bone_vec_cit it = bones.begin(), end = bones.end(); it != end; ++it) {
		if ((*it)->is_root()) {
			status = import_bone(*it, MObject::kNullObj);
			break;
		}
	}
	end_progress();
	if (!status)
		return status;

	maya_object_map shared_textures;
	start_progress(object->surfaces().size(), "Importing surfaces");
	for (xr_surface_vec_cit it = object->surfaces().begin(),
			end = object->surfaces().end(); it != end; ++it) {
		xr_surface* surface = *it;
		MObject& texture_obj = shared_textures[surface->texture()];
		if (texture_obj.isNull()) {
			texture_obj = create_texture(surface->texture(), &status);
			if (!status)
				break;
		}
		if (!(status = import_surface(surface, texture_obj)))
			break;
		advance_progress();
	}
	end_progress();
	if (!status)
		return status;
	shared_textures.clear();

	start_progress(object->meshes().size(), "Importing meshes");
	for (xr_mesh_vec_cit it = object->meshes().begin(),
			end = object->meshes().end(); it != end; ++it) {
		if (!(status = import_mesh(*it, bones)))
			break;
		advance_progress();
	}
	end_progress();
	if (!status)
		return status;

	if (!bones.empty()) {
		MObject character_obj = create_character(&status);
		if (status && !object->motions().empty()) {
			reset_animation_state();
			status = import_motions(object->motions(), character_obj);
		}
	}

	return status;
}

static MPlug get_free_plug(MFn::Type filter, const char* list_name)
{
	MItDependencyNodes it(filter);
	if (!it.isDone()) {
		MFnDependencyNode dep_fn(it.thisNode());
		MStatus status;
		MPlug list_plug = dep_fn.findPlug(list_name, &status);
		if (status) {
			for (unsigned i = 0;; ++i) {
				MPlug plug = list_plug.elementByLogicalIndex(i, &status);
				CHECK_MSTATUS(status);
				if (!plug.isConnected())
					return plug;
			}
		}
	}
	return MPlug();
}

static MObject create_texture(const std::string& texture, MStatus* return_status)
{
	std::string path(texture);
	xr_file_system& fs = xr_file_system::instance();
	if (fs.file_exist(PA_GAME_TEXTURES, path.append(".dds"))) {
		std::string full_path;
		fs.resolve_path(PA_GAME_TEXTURES, path, full_path);
		path = full_path;
	}
	for (std::string::size_type i = 0; (i = path.find('\\', i)) != std::string::npos; ++i)
		path[i] = '/';

	std::string::size_type i = texture.rfind('\\');
	MString maya_name(i != std::string::npos ? texture.c_str() + i + 1 : texture.c_str());

	MStatus status;
	MFnDependencyNode texture_fn;
	MObject texture_obj = texture_fn.create("file", maya_name += "_F", &status);
	if (!status) {
		if (return_status)
			*return_status = status;
		return MObject::kNullObj;
	}
	texture_fn.findPlug("ftn").setValue(path.c_str());

	MDGModifier dg_modifier;
	dg_modifier.connect(texture_fn.findPlug("msg"), get_free_plug(MFn::kTextureList, "textures"));
	dg_modifier.doIt();

	MFnDependencyNode p2dt_fn;
	p2dt_fn.create("place2dTexture", maya_name += "_P2DT", &status);
	if (!status) {
		if (return_status)
			*return_status = status;
		return MObject::kNullObj;
	}
	dg_modifier.connect(p2dt_fn.findPlug("c"), texture_fn.findPlug("c"));
	dg_modifier.connect(p2dt_fn.findPlug("tf"), texture_fn.findPlug("tf"));
	dg_modifier.connect(p2dt_fn.findPlug("rf"), texture_fn.findPlug("rf"));
	dg_modifier.connect(p2dt_fn.findPlug("mu"), texture_fn.findPlug("mu"));
	dg_modifier.connect(p2dt_fn.findPlug("mv"), texture_fn.findPlug("mv"));
	dg_modifier.connect(p2dt_fn.findPlug("s"), texture_fn.findPlug("s"));
	dg_modifier.connect(p2dt_fn.findPlug("wu"), texture_fn.findPlug("wu"));
	dg_modifier.connect(p2dt_fn.findPlug("wv"), texture_fn.findPlug("wv"));
	dg_modifier.connect(p2dt_fn.findPlug("re"), texture_fn.findPlug("re"));
	dg_modifier.connect(p2dt_fn.findPlug("of"), texture_fn.findPlug("of"));
	dg_modifier.connect(p2dt_fn.findPlug("r"), texture_fn.findPlug("ro"));
	dg_modifier.connect(p2dt_fn.findPlug("n"), texture_fn.findPlug("n"));
	dg_modifier.connect(p2dt_fn.findPlug("vt1"), texture_fn.findPlug("vt1"));
	dg_modifier.connect(p2dt_fn.findPlug("vt2"), texture_fn.findPlug("vt2"));
	dg_modifier.connect(p2dt_fn.findPlug("vt3"), texture_fn.findPlug("vt3"));
	dg_modifier.connect(p2dt_fn.findPlug("vc1"), texture_fn.findPlug("vc1"));
	dg_modifier.connect(p2dt_fn.findPlug("o"), texture_fn.findPlug("uv"));
	dg_modifier.connect(p2dt_fn.findPlug("ofs"), texture_fn.findPlug("fs"));
	dg_modifier.doIt();

	if (return_status)
		*return_status = MS::kSuccess;
	return texture_obj;
}

static void set_xraymtl_attr(MFnDependencyNode& dep_fn, const char* name, const std::string& value)
{
	MStatus status;
	MPlug plug = dep_fn.findPlug(name, &status);
	if (status) {
		MFnEnumAttribute attr_fn(plug.attribute(), &status);
		if (status) {
			std::string temp(value);
			for (std::string::size_type i = 0; (i = temp.find('\\', i)) != std::string::npos; ++i)
				temp[i] = '/';
			short index = attr_fn.fieldIndex(temp.c_str(), &status);
			if (!status) {
				msg("xray_re: can't set attribute %s to %s", name, value.c_str());
				MGlobal::displayError(MString("xray_re: can't set attribute ") +
					name + " to " + value.c_str());
				return;
			}
			plug.setShort(index);
		}
	}
}

MStatus maya_import_tools::import_surface(const xr_surface* surface, MObject& texture_obj)
{
	MStatus status;
	MFnDependencyNode shader_fn;
	MObject shader_obj = shader_fn.create("XRayMtl",
			make_maya_name(surface->name(), "_S", "_M"), &status);
	if (!status) {
		msg("xray_re: can't create shader");
		MGlobal::displayError("xray_re: can't create shader");
		return status;
	}

	set_xraymtl_attr(shader_fn, "xrayGameMaterial", surface->gamemtl());
	set_xraymtl_attr(shader_fn, "xrayEngineShader", surface->eshader());
	set_xraymtl_attr(shader_fn, "xrayCompilerShader", surface->cshader());
	shader_fn.findPlug("xrayDoubleSide").setBool(surface->two_sided());

	MDGModifier dg_modifier;
	dg_modifier.connect(shader_fn.findPlug("msg"), get_free_plug(MFn::kShaderList, "shaders"));
	dg_modifier.doIt();

	MFnSet set_fn;
	MObject set_obj = set_fn.create(MSelectionList(), MFnSet::kRenderableOnly, &status);
	if (!status) {
		msg("xray_re: can't create shading group");
		MGlobal::displayError("xray_re: can't create shading group");
		return status;
	}
	set_fn.setName(make_maya_name(surface->name(), "_S", "_SG"));

	MPlug ss_plug = set_fn.findPlug("ss", &status);
	MPlugArray connected_plugs;
	if (ss_plug.connectedTo(connected_plugs, true, false)) {
		for (unsigned i = connected_plugs.length(); i != 0;)
			dg_modifier.disconnect(connected_plugs[--i], ss_plug);
		dg_modifier.doIt();
	}
	dg_modifier.connect(shader_fn.findPlug("oc"), ss_plug);
	dg_modifier.doIt();

	MFnDependencyNode texture_fn(texture_obj);
	dg_modifier.connect(texture_fn.findPlug("oc"), shader_fn.findPlug("c"));
	dg_modifier.doIt();

	// connect hw materialInfo's texture plug to workaround Maya's blurry default setting
	auto Plug = set_fn.findPlug("msg");
	MItDependencyGraph dg_it(Plug, MFn::kMaterialInfo,
			MItDependencyGraph::kDownstream, MItDependencyGraph::kDepthFirst,
			MItDependencyGraph::kNodeLevel);
	if (!dg_it.isDone()) {
		MFnDependencyNode info_fn(dg_it.thisNode());
		status = dg_modifier.connect(texture_fn.findPlug("msg"),
				info_fn.findPlug("t").elementByLogicalIndex(0));
		CHECK_MSTATUS(status);
		dg_modifier.doIt();
	}

	m_sets[surface->name()] = set_obj;

	return status;
}

MStatus maya_import_tools::import_bone(const xr_bone* bone, MObject& parent_obj)
{
	MStatus status;

	MFnIkJoint joint_fn;
	MObject joint_obj = joint_fn.create(parent_obj, &status);
	if (!status) {
		msg("xray_re: can't create joint %s", bone->name().c_str());
		MGlobal::displayError(MString("xray_re: can't create joint ") + bone->name().c_str());
		return status;
	}
	joint_fn.setName(bone->name().c_str());

	const fvector3& t = bone->bind_offset();
	double x = MDistance(t.x, MDistance::kMeters).asCentimeters();
	double y = MDistance(t.y, MDistance::kMeters).asCentimeters();
	double z = MDistance(t.z, MDistance::kMeters).asCentimeters();
	joint_fn.setTranslation(MVector(x, y, -z), MSpace::kTransform);

	const fvector3& r = bone->bind_rotate();
	joint_fn.setRotation(MEulerRotation(-r.x, -r.y, r.z, MEulerRotation::kZXY));

	m_joints.insert(maya_object_pair(bone->name(), joint_obj));
	advance_progress();

	for (xr_bone_vec_cit it = bone->children().begin(), end = bone->children().end();
			it != end; ++it) {
		if (!(status = import_bone(*it, joint_obj)))
			break;
	}
	return status;
}

static inline void append_uvs(const std::vector<fvector2>& uvs, MFloatArray& u_values, MFloatArray& v_values)
{
	unsigned offset = u_values.length(), size = unsigned(uvs.size() & UINT_MAX) + offset;
	u_values.setLength(size);
	v_values.setLength(size);
	for (std::vector<fvector2>::const_iterator it = uvs.begin(), end = uvs.end(); it != end; ++it) {
		u_values.set(it->x, offset);
		v_values.set(1.f - it->y, offset);
		++offset;
	}
}

MStatus maya_import_tools::import_mesh(const xr_mesh* mesh, const xr_bone_vec& bones)
{
	MStatus status;

	MFnTransform transform_fn;
	MObject transform_obj = transform_fn.create(MObject::kNullObj, &status);
	if (!status) {
		msg("xray_re: can't create mesh %s transform", mesh->name().c_str());
		MGlobal::displayError(MString("xray_re: can't create mesh ") +
			mesh->name().c_str() + " transform");
		return status;
	}
	transform_fn.setName(make_maya_name(mesh->name(), "Shape"));

	const std::vector<fvector3>& points = mesh->points();
	MPointArray vertices;
	vertices.setLength(unsigned(points.size() & UINT_MAX));
	uint_fast32_t vert_idx = 0;
	for (std::vector<fvector3>::const_iterator it = points.begin(), end = points.end();
			it != end; ++it) {
		// hard-coded conversion to centimeters for compatibility with official GSC
		// export plug-in.
		double x = MDistance(it->x, MDistance::kMeters).asCentimeters();
		double y = MDistance(it->y, MDistance::kMeters).asCentimeters();
		double z = MDistance(it->z, MDistance::kMeters).asCentimeters();
		vertices.set(unsigned(vert_idx++), x, y, -z);
	}

	const xr_vmap_vec& vmaps = mesh->vmaps();

	MFloatArray u_values, v_values;
	unsigned* uv_offsets = new unsigned[vmaps.size()];
	for (xr_vmap_vec_cit it = vmaps.begin(), end = vmaps.end(); it != end; ++it) {
		if ((*it)->type() == xr_vmap::VMT_UV) {
			const xr_uv_vmap* vmap = static_cast<const xr_uv_vmap*>(*it);
			uv_offsets[it - vmaps.begin()] = u_values.length();
			append_uvs(vmap->uvs(), u_values, v_values);
		}
	}

	const lw_face_vec& faces = mesh->faces();
	MIntArray poly_connects, uv_ids;
	poly_connects.setLength(unsigned(faces.size() & UINT_MAX)*3);
	uv_ids.setLength(poly_connects.length());
	const lw_vmref_vec& vmrefs = mesh->vmrefs();
	vert_idx = 0;
	for (lw_face_vec_cit it = faces.begin(), end = faces.end(); it != end; ++it) {
		for (uint_fast32_t i = 3; i != 0;) {
			poly_connects.set(it->v[--i], unsigned(vert_idx));
			const lw_vmref& vmref = vmrefs[it->ref[i]];
			for (lw_vmref::const_iterator it1 = vmref.begin(), end1 = vmref.end(); it1 != end1; ++it1) {
				if (vmaps[it1->vmap]->type() == xr_vmap::VMT_UV)
					uv_ids.set(it1->offset + uv_offsets[it1->vmap], unsigned(vert_idx));
			}
			++vert_idx;
		}
	}
	delete[] uv_offsets;

	MIntArray poly_counts(unsigned(faces.size() & UINT_MAX), 3);

	MFnMesh mesh_fn;
	MObject mesh_obj = mesh_fn.create(vertices.length(), poly_counts.length(),
			vertices, poly_counts, poly_connects, u_values, v_values,
			transform_obj, &status);
	if (!status) {
		msg("xray_re: can't create mesh %s", mesh->name().c_str());
		MGlobal::displayError(MString("xray_re: can't create mesh ") + mesh->name().c_str());
		return status;
	}
	mesh_fn.setName(make_maya_name(mesh->name(), "Shape", "Shape"));

	// temporary safety measure
	if (mesh_fn.numPolygons() != faces.size()) {
		msg("xray_re: mesh polygon count was changed");
		MGlobal::displayError("xray_re: mesh polygon count was changed");
//		return MS::kFailure;
	}

	status = mesh_fn.assignUVs(poly_counts, uv_ids, 0);
	CHECK_MSTATUS(status);

	if (mesh->sgroups().empty()) {
		for (MItMeshEdge it(mesh_obj); !it.isDone(); it.next())
			it.setSmoothing(false);
	} else {
		
		MIntArray connected;
		const std::vector<uint32_t>& sgroups = mesh->sgroups();
		if (m_target_sdk <= xray_re::SDK_VER_0_4) {
			if (mesh->flags() & EMF_3DSMAX) {
				for (MItMeshEdge it(mesh_obj); !it.isDone(); it.next()) {
					it.getConnectedFaces(connected, &status);
					it.setSmoothing(status && connected.length() == 2 &&
							(sgroups[connected[0]] & sgroups[connected[1]]) != 0);
				}
			} else {
				for (MItMeshEdge it(mesh_obj); !it.isDone(); it.next()) {
					it.getConnectedFaces(connected, &status);
					it.setSmoothing(status && connected.length() == 2 &&
							sgroups[connected[0]] != EMESH_NO_SG &&
							sgroups[connected[0]] == sgroups[connected[1]]);
				}
			}
		} else {
			for (MItMeshPolygon it(mesh_obj); !it.isDone(); it.next()) {
				status = it.getEdges(connected);
				if (!status)
					return MS::kFailure;

				mesh_fn.setEdgeSmoothing(connected[0], !(sgroups[it.index()] & 0x2));
				mesh_fn.setEdgeSmoothing(connected[1], !(sgroups[it.index()] & 0x1));
				mesh_fn.setEdgeSmoothing(connected[2], !(sgroups[it.index()] & 0x4));
			}
		}
	}

	MDagPath mesh_path;
	if (!(status = mesh_fn.getPath(mesh_path)))
		return MS::kFailure;

	MDGModifier dg_modifier;
	for (xr_surfmap_vec_cit it = mesh->surfmaps().begin(), end = mesh->surfmaps().end();
			it != end; ++it) {
		const xr_surfmap* smap = *it;
		MObject& set_obj = m_sets[smap->surface->name()];
		if (set_obj.isNull()) {
			msg("xray_re: null shading group object");
			MGlobal::displayError("xray_re: null shading group object");
			return MS::kFailure;
		}

		MIntArray shaded_faces;
		shaded_faces.setLength(unsigned(smap->faces.size() & UINT_MAX));
		std::vector<uint32_t>::const_reverse_iterator it1 = smap->faces.rbegin();
		for (unsigned i = shaded_faces.length(); i != 0; ++it1)
			shaded_faces.set(*it1, --i);

		MFnSingleIndexedComponent component_fn;
		MObject component_obj = component_fn.create(MFn::kMeshPolygonComponent, &status);
		if (!status || !(status = component_fn.addElements(shaded_faces)))
			return status;

		MSelectionList members;
		if (!(status = members.add(mesh_path, component_obj)))
			return status;

		MFnSet set_fn(set_obj);
		if (!(status = set_fn.addMembers(members)))
			return status;
	}

	if (mesh_fn.numVertices() != points.size()) {
		msg("xray_re: mesh vertex count was changed");
		MGlobal::displayError("xray_re: mesh vertex count was changed");
		return MS::kFailure;
	}

	if (!bones.empty()) {
		MString command("skinCluster -mi 2 -tsb ");
		for (maya_object_map_it it = m_joints.begin(), end = m_joints.end(); it != end; ++it) {
			MFnIkJoint joint_fn(it->second, &status);
			CHECK_MSTATUS(status);
			command += joint_fn.partialPathName();
			command += " ";
		}
		command += mesh_fn.partialPathName();

		MStringArray result;
		status = MGlobal::executeCommand(command, result);
		if (!status) {
			msg("xray_re: can't create skin cluster");
			MGlobal::displayError("xray_re: can't create skin cluster");
			return status;
		}

		MSelectionList selection_list;
		status = selection_list.add(result[0]);
		CHECK_MSTATUS(status);
		MObject skin_obj;
		selection_list.getDependNode(0, skin_obj);
		MFnSkinCluster skin_fn(skin_obj, &status);
		if (!status)
			return status;

		MFnSingleIndexedComponent component_fn;
		MObject component_obj = component_fn.create(MFn::kMeshVertComponent, &status);
		if (!status)
			return status;
		status = mesh_fn.setObject(mesh_path);
		if (!status)
			return status;
		MIntArray vertex_indices;
		vertex_indices.setLength(mesh_fn.numVertices());
		for (unsigned i = vertex_indices.length(); i != 0;) {
			--i;
			vertex_indices[i] = i;
		}
		status = component_fn.addElements(vertex_indices);
		CHECK_MSTATUS(status);

		unsigned num_bones = unsigned(bones.size() & UINT_MAX), bone_idx = 0;
		MIntArray influence_indices(num_bones);
		MDoubleArray vertex_weights(vertex_indices.length()*num_bones, 0);
		for (xr_bone_vec_cit it = bones.begin(), end = bones.end(); it != end; ++it, ++bone_idx) {
			const xr_bone* bone = *it;

			maya_object_map_it joint_it = m_joints.find(bone->name());
			xr_assert(joint_it != m_joints.end());
			MFnIkJoint joint_fn(joint_it->second, &status);
			CHECK_MSTATUS(status);
			MDagPath joint_path;
			status = joint_fn.getPath(joint_path);
			CHECK_MSTATUS(status);
			unsigned influence_idx = skin_fn.indexForInfluenceObject(joint_path, &status);
			CHECK_MSTATUS(status);
			influence_indices[bone_idx] = influence_idx;

			const xr_weight_vmap* vmap = 0;
			for (xr_vmap_vec_cit it1 = vmaps.begin(), end1 = vmaps.end(); it1 != end1; ++it1) {
				if ((*it1)->name() == bone->name()) {
					vmap = static_cast<const xr_weight_vmap*>(*it1);
					break;
				}
			}
			if (vmap) {
				xr_assert(vmap->type() == xr_vmap::VMT_WEIGHT);
				std::vector<uint32_t>::const_iterator v = vmap->vertices().begin();
				for (std::vector<float>::const_iterator w = vmap->weights().begin(),
						w_end = vmap->weights().end(); w != w_end; ++v, ++w) {
					vertex_weights[*v * num_bones + bone_idx] = *w;
				}
			}
		}
		status = skin_fn.setWeights(mesh_path, component_obj,
				influence_indices, vertex_weights, false);
		CHECK_MSTATUS(status);
	}
	return MS::kSuccess;
}

MObject maya_import_tools::lookup_character(MStatus* return_status)
{
	MSelectionList selection_list;
	MStatus status = MGlobal::getActiveSelectionList(selection_list);
	if (selection_list.isEmpty()) {
		MGlobal::displayError("xray_re: nothing is selected");
		if (return_status)
			*return_status = MS::kInvalidParameter;
		return MObject::kNullObj;
	}
	MObject character_obj;
	selection_list.getDependNode(0, character_obj);
	if (!character_obj.hasFn(MFn::kCharacter)) {
		MGlobal::displayError("xray_re: selected object is not a character");
		if (return_status)
			*return_status = MS::kInvalidParameter;
		return MObject::kNullObj;
	}
	MFnCharacter character_fn(character_obj, &status);
	MPlugArray member_plugs;
	character_fn.getMemberPlugs(member_plugs);
	for (unsigned i = member_plugs.length(); i != 0;) {
		MObject member_obj = member_plugs[--i].node();
		if (!member_obj.hasFn(MFn::kJoint))
			continue;
		MFnIkJoint joint_fn(member_obj);
		MObject& joint_obj = m_joints[joint_fn.name().asChar()];
		if (joint_obj.isNull()) {
			joint_obj = member_obj;
			msg("xray_re: found bone %s", joint_fn.name().asChar());
			MGlobal::displayInfo(MString("xray_re: found bone ") + joint_fn.name().asChar());
		}
	}
	if (return_status)
		*return_status = status;
	return character_obj;
}

MObject maya_import_tools::create_character(MStatus* return_status)
{
	MStatus status;
	MSelectionList member_objs;
	for (maya_object_map_it it = m_joints.begin(), end = m_joints.end(); it != end; ++it) {
		if (!(status = member_objs.add(it->second))) {
			if (return_status)
				*return_status = status;
			return MObject::kNullObj;
		}
	}
	MFnCharacter character_fn;
	MObject character_obj = character_fn.create(member_objs, MFnSet::kNone, &status);
	if (status)
		character_fn.setName("character");
	if (return_status)
		*return_status = status;
	return character_obj;
}

void maya_import_tools::reset_animation_state() const
{
	MTime::setUIUnit(MTime::kNTSCFrame);
	MAnimControl::setMinTime(MTime(0, MTime::kSeconds));
	MAnimControl::setAnimationStartTime(MTime(0, MTime::kSeconds));
}

static MFnAnimCurve::InfinityType maya_infinity(uint8_t behaviour)
{
	switch (behaviour) {
	case xr_envelope::BEH_RESET:
		return MFnAnimCurve::kConstant;		// FIXME
	default:
	case xr_envelope::BEH_CONSTANT:
		return MFnAnimCurve::kConstant;
	case xr_envelope::BEH_REPEAT:
		return MFnAnimCurve::kCycle;
	case xr_envelope::BEH_OSCILLATE:
		return MFnAnimCurve::kOscillate;
	case xr_envelope::BEH_OFFSET:
		return MFnAnimCurve::kConstant;		// FIXME
	case xr_envelope::BEH_LINEAR:
		return MFnAnimCurve::kLinear;
	}
}

static inline void append_key(MTimeArray& times, MDoubleArray& values, double time, double value)
{
	unsigned size = values.length();
	if (size == 0 || values[size-1] != value) {
		times.append(MTime(time, MTime::kSeconds));
		values.append(value);
	}
}

MStatus maya_import_tools::import_motion(const xray_re::xr_skl_motion* smotion, MObject& character_obj)
{
	MStatus status;
	MDGModifier dg_modifier;

	MFnCharacter character_fn(character_obj);

	MString clip_name(smotion->name().c_str());

	double fps = smotion->fps();
	double start_time = smotion->frame_start()/fps;
	double end_time = smotion->frame_end()/fps;

	MFnClip clip_fn;
	MObject clip_obj = clip_fn.createSourceClip(MTime(start_time, MTime::kSeconds),
			MTime(end_time - start_time, MTime::kSeconds), dg_modifier, &status);
	if (!status) {
		msg("xray_re: can't create clip %s", clip_name.asChar());
		MGlobal::displayError(MString("xray_re: can't create clip ") + clip_name.asChar());
		return status;
	}
	clip_fn.setName(clip_name);
	dg_modifier.doIt();

	MTimeArray times[6];
	MDoubleArray values[6];

	for (xr_bone_motion_vec_cit it = smotion->bone_motions().begin(),
			end = smotion->bone_motions().end(); it != end; ++it) {
		const xr_bone_motion* bmotion = *it;
		maya_object_map_it joint_it = m_joints.find(bmotion->name());
		if (joint_it == m_joints.end()) {
			msg("xray_re: can't find bone %s referenced by motion %s",
				bmotion->name().c_str(), smotion->name().c_str());
			MGlobal::displayError(MString("xray_re: can't find bone ") +
				bmotion->name().c_str() + " referenced by motion " + smotion->name().c_str());
			continue;
		}
		MFnTransform joint_fn(joint_it->second, &status);
		CHECK_MSTATUS(status);

		MString name(clip_name);
		name += '_';
		name += bmotion->name().c_str();
		name += '_';

		// rebuild from scratch to handle differently-ordered rotation and non-step shape.
		// FIXME: it might be better to force the conversion into bezier curves.
		for (int32_t frame = smotion->frame_start(), frame_end = smotion->frame_end();
				frame < frame_end; ++frame) {
			// divide here for better precision.
			double time = frame/fps;

			fvector3 offs, rot;
			bmotion->evaluate(float(time), offs, rot);

			append_key(times[0], values[0], time, MDistance(offs.x, MDistance::kMeters).asCentimeters());
			append_key(times[1], values[1], time, MDistance(offs.y, MDistance::kMeters).asCentimeters());
			append_key(times[2], values[2], time, MDistance(-offs.z, MDistance::kMeters).asCentimeters());

			MEulerRotation maya_rot(-rot.x, -rot.y, rot.z, MEulerRotation::kZXY);
			maya_rot.reorderIt(MEulerRotation::kXYZ);
			append_key(times[3], values[3], time, maya_rot.x);
			append_key(times[4], values[4], time, maya_rot.y);
			append_key(times[5], values[5], time, maya_rot.z);
		}

		const xr_envelope* const* envelopes = bmotion->envelopes();
		for (uint_fast32_t i = 6; i != 0;) {
			static const MString k_plug_names[6] = { "tx", "ty", "tz", "rx", "ry", "rz" };

			MFnAnimCurve curve_fn;
			MObject curve_obj = curve_fn.create(--i >= 3 ?
					MFnAnimCurve::kAnimCurveTA : MFnAnimCurve::kAnimCurveTL,
					0, &status);
			CHECK_MSTATUS(status);
			curve_fn.setName(name + k_plug_names[i]);
			curve_fn.setPreInfinityType(maya_infinity(envelopes[i]->pre_behaviour()));
			curve_fn.setPostInfinityType(maya_infinity(envelopes[i]->post_behaviour()));
			status = curve_fn.addKeys(&times[i], &values[i],
					MFnAnimCurve::kTangentStep, MFnAnimCurve::kTangentStep);
			CHECK_MSTATUS(status);
			auto Plug = joint_fn.findPlug(k_plug_names[i]);
			status = character_fn.addCurveToClip(curve_obj, clip_obj,
				Plug, dg_modifier);
			CHECK_MSTATUS(status);
			times[i].clear();
			values[i].clear();
		}
	}

	status = character_fn.attachSourceToCharacter(clip_obj, dg_modifier);
	CHECK_MSTATUS(status);
	dg_modifier.doIt();

	return MS::kSuccess;
}

MStatus maya_import_tools::import_motions(const xr_skl_motion_vec& motions, MObject& character_obj)
{
	MStatus status = MS::kFailure;

	start_progress(motions.size(), "Importing motions");
	for (xr_skl_motion_vec_cit it = motions.begin(), end = motions.end();
			it != end; ++it) {
		if (!(status = import_motion(*it, character_obj)))
			break;
		advance_progress();
	}
	end_progress();

	return status;
}

void maya_import_tools::set_default_options(void)
{
	m_target_sdk = xray_re::SDK_VER_DEFAULT;
}

MStatus maya_import_tools::parse_options(const MString& options)
{
	MStatus status;
	MStringArray params;

	if (!(status = options.split(';', params)))
		return status;

	for (size_t i = 0; i < params.length(); i++)
	{
		MStringArray key_value;
		if (!(status = params[i].split('=', key_value)))
			return status;

		if (key_value.length() < 2)
			continue;

		if (key_value[0] == "sdk_ver")
		{
			xray_re::sdk_version ver = xray_re::sdk_version_from_string(key_value[1].asChar());
			m_target_sdk = (ver == xray_re::SDK_VER_UNKNOWN ? xray_re::SDK_VER_0_6 : ver);
		}
	}

	return MS::kSuccess;
}
