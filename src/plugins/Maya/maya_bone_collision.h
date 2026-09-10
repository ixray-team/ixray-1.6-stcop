#pragma once

#include <maya/MStatus.h>
#include <maya/MObject.h>

namespace xray_re { class xr_bone; }

MStatus initialize_bone_collision();
void uninitialize_bone_collision();
MStatus import_bone_collision(MObject joint, const xray_re::xr_bone& bone);
MStatus export_bone_collision(MObject joint, xray_re::xr_bone& bone);
bool is_bone_collision_helper(MObject node);
