#pragma once
#ifndef __MAYA_EXPORT_TOOLS_H__
#define __MAYA_EXPORT_TOOLS_H__

#include <string>
#include <vector>
#include <map>

#include <maya/MStatus.h>

#include "xr_sdk_version.h"

class MObject;
class MObjectArray;
class MFnMesh;
class MFnSet;
class MString;

namespace xray_re {
	class xr_object;
	class xr_surface;
	struct xr_surfmap;
};

typedef std::map<std::string, xray_re::xr_surface*> xr_surface_map;
typedef xr_surface_map::iterator xr_surface_map_it;

class maya_export_tools {
public:
					maya_export_tools(const MString& options = "");

	MStatus			export_object(const char* path, bool selection_only = false);
	MStatus			export_skl_object(const char* path, bool selection_only = false);
	MStatus			export_skl(const char* path, bool selection_only = false);
	MStatus			export_anm(const char* path, bool selection_only = false);

private:
	MStatus			extract_surfaces(MFnMesh& mesh_fn, std::vector<xray_re::xr_surfmap*>& surfmaps);
	xray_re::xr_surface*	create_surface(const char* surf_name, MFnSet& set_fn);
	xray_re::xr_object*	create_object(MObjectArray& mesh_objs);
	xray_re::xr_object*	create_skl_object(MObject& mesh_obj, MObject& skin_obj);

	void			commit_surfaces(std::vector<xray_re::xr_surface*>& surfaces);

	void			set_default_options(void);
	MStatus			parse_options(const MString& options);

private:
	xr_surface_map		m_shared_surfaces;
	bool			m_skeletal;

	xray_re::sdk_version m_target_sdk;
	bool m_compressed;
	bool m_vnormals;
};

#endif
