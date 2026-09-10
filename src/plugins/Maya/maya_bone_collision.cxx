#define NOMINMAX
#include "maya_bone_collision.h"
#include "maya_bone_collision_script.h"
#include "xr_bone.h"
#include <maya/MAngle.h>
#include <maya/MDistance.h>
#include <maya/MEulerRotation.h>
#include <maya/MFnDagNode.h>
#include <maya/MFnDependencyNode.h>
#include <maya/MGlobal.h>
#include <maya/MMatrix.h>
#include <maya/MPlug.h>
#include <maya/MQuaternion.h>
#include <maya/MVector.h>
#include <cmath>
#include <stdexcept>
#include <string>

using namespace xray_re;

namespace {
MString python_string(const MString& value)
{
	std::string result("'");
	for (const char* p = value.asUTF8(); *p; ++p)
	{
		switch (*p)
		{
		case '\\': result += "\\\\"; break;
		case '\'': result += "\\'"; break;
		case '\n': result += "\\n"; break;
		case '\r': result += "\\r"; break;
		default: result += *p; break;
		}
	}
	result += "'";
	MString text;
	text.setUTF8(result.c_str());
	return text;
}

MStatus python_bone(const char* function, MObject joint)
{
	return MGlobal::executePythonCommand(MString("import xray_bone_collision as _xbc; _xbc.") +
		function + "(" + python_string(MFnDagNode(joint).fullPathName()) + ")");
}

class bone_attributes {
public:
	explicit bone_attributes(MObject joint): node(joint) {}
	MPlug plug(const char* name)
	{
		MStatus status;
		MPlug p = node.findPlug(name, true, &status);
		if (!status) throw std::runtime_error(std::string("Missing bone attribute: ") + name);
		return p;
	}
	double get(const char* name)
	{
		MStatus status;
		double v = plug(name).asDouble(&status);
		if (!status || !std::isfinite(v)) throw std::runtime_error(std::string("Invalid bone attribute: ") + name);
		return v;
	}
	void set(const char* name, double v)
	{
		check(plug(name).setDouble(v));
	}
	void distance(const char* name, double cm)
	{
		check(plug(name).setMDistance(MDistance(cm, MDistance::kCentimeters)));
	}
	double distance(const char* name)
	{
		MStatus status;
		double value = plug(name).asMDistance(&status).asCentimeters();
		check(status);
		if (!std::isfinite(value)) throw std::runtime_error("Non-finite bone position");
		return value;
	}
	void angle(const char* name, double radians)
	{
		check(plug(name).setMAngle(MAngle(radians)));
	}
	double angle(const char* name)
	{
		MStatus status;
		double value = plug(name).asMAngle(&status).asRadians();
		check(status);
		if (!std::isfinite(value)) throw std::runtime_error("Non-finite bone angle");
		return value;
	}
	void vector(const char* base, const fvector3& v)
	{
		distance((std::string(base) + "X").c_str(), v.x * 100.0);
		distance((std::string(base) + "Y").c_str(), v.y * 100.0);
		distance((std::string(base) + "Z").c_str(), -v.z * 100.0);
	}
	fvector3 vector(const char* base)
	{
		fvector3 v;
		v.set(float(distance((std::string(base) + "X").c_str()) * .01),
			float(distance((std::string(base) + "Y").c_str()) * .01),
			float(-distance((std::string(base) + "Z").c_str()) * .01));
		return v;
	}
	static void check(MStatus status) { if (!status) throw std::runtime_error(status.errorString().asChar()); }
	MFnDependencyNode node;
};

const char* shape_flags[] = { "xrayNoPickable", "xrayRemoveAfterBreak", "xrayNoPhysics", "xrayNoFogCollider" };
const char* axes[] = { "X", "Y", "Z" };

MStatus error(const std::exception& e, MObject joint)
{
	MGlobal::displayError(MString("IX-Ray bone ") + MFnDagNode(joint).fullPathName() + ": " + e.what());
	return MS::kFailure;
}
}

MStatus initialize_bone_collision()
{
	return MGlobal::executePythonCommand(MString(
		"import sys, types\n"
		"_xbc = types.ModuleType('xray_bone_collision')\n"
		"sys.modules['xray_bone_collision'] = _xbc\nexec(") +
		python_string(bone_collision_script) + ", _xbc.__dict__)\n_xbc.install()");
}

void uninitialize_bone_collision()
{
	MGlobal::executePythonCommand("import xray_bone_collision; xray_bone_collision.uninstall()");
}

bool is_bone_collision_helper(MObject node)
{
	MStatus status;
	MFnDependencyNode fn(node);
	MPlug marker = fn.findPlug("xrayCollisionHelper", true, &status);
	return status && marker.asBool();
}

MStatus import_bone_collision(MObject joint, const xr_bone& bone)
{
	MStatus status = python_bone("ensure_bone", joint);
	if (!status) return status;
	try
	{
		bone_attributes a(joint);
		const s_bone_shape& shape = bone.shape();
		const s_joint_ik_data& ik = bone.joint_ik_data();
		if (shape.type > ST_CYLINDER || ik.type > JT_SLIDER)
			throw std::runtime_error("Unsupported shape or joint type");
		a.set("xrayShapeType", shape.type);
		a.set("xrayShapeFlags", shape.flags);
		for (unsigned i = 0; i != 4; ++i) a.set(shape_flags[i], (shape.flags >> i) & 1);
		a.set("xrayJointType", ik.type);
		a.set("xrayIKFlags", ik.ik_flags);
		a.set("xrayBreakable", (ik.ik_flags & JF_BREAKABLE) != 0);
		a.set("xrayMass", bone.mass());
		a.vector("xrayCenterOfMass", bone.center_of_mass());
		bone_attributes::check(a.plug("xrayGameMaterial").setString(bone.gamemtl().c_str()));
		a.set("xraySpring", ik.spring_factor);
		a.set("xrayDamping", ik.damping_factor);
		a.set("xrayFriction", ik.friction);
		a.set("xrayBreakForce", ik.break_force);
		a.set("xrayBreakTorque", ik.break_torque);
		for (unsigned i = 0; i != 3; ++i)
		{
			std::string base = std::string("xrayLimit") + axes[i];
			a.angle((base + "Min").c_str(), ik.limits[i].limit.x);
			a.angle((base + "Max").c_str(), ik.limits[i].limit.y);
			a.set((base + "Spring").c_str(), ik.limits[i].spring_factor);
			a.set((base + "Damping").c_str(), ik.limits[i].damping_factor);
		}
		if (ik.type == JT_SLIDER)
		{
			a.distance("xraySlideMin", ik.limits[0].limit.x * 100.0);
			a.distance("xraySlideMax", ik.limits[0].limit.y * 100.0);
		}

		MEulerRotation rotation;
		if (shape.type == ST_BOX)
		{
			a.vector("xrayShapeOffset", shape.box.translate);
			MMatrix matrix;
			const fvector3 rows[] = { shape.box.rotate.i, shape.box.rotate.j, shape.box.rotate.k };
			for (unsigned i = 0; i != 3; ++i)
			{
				const double sign = i == 2 ? -1.0 : 1.0;
				matrix[i][0] = sign * rows[i].x;
				matrix[i][1] = sign * rows[i].y;
				matrix[i][2] = -sign * rows[i].z;
			}
			rotation = MEulerRotation::decompose(matrix, MEulerRotation::kXYZ);
			a.set("xrayHalfSizeX", shape.box.halfsize.x * 100.0);
			a.set("xrayHalfSizeY", shape.box.halfsize.y * 100.0);
			a.set("xrayHalfSizeZ", shape.box.halfsize.z * 100.0);
		}
		else if (shape.type == ST_SPHERE)
		{
			a.vector("xrayShapeOffset", shape.sphere.p);
			a.set("xrayRadius", shape.sphere.r * 100.0);
		}
		else if (shape.type == ST_CYLINDER)
		{
			a.vector("xrayShapeOffset", shape.cylinder.center);
			const fvector3& d = shape.cylinder.direction;
			rotation = MQuaternion(MVector(0, 1, 0), MVector(d.x, d.y, -d.z)).asEulerRotation();
			a.set("xrayRadius", shape.cylinder.radius * 100.0);
			a.set("xrayHalfHeight", shape.cylinder.height * 50.0);
		}
		a.angle("xrayShapeRotateX", rotation.x);
		a.angle("xrayShapeRotateY", rotation.y);
		a.angle("xrayShapeRotateZ", rotation.z);
	}
	catch (const std::exception& e) { return error(e, joint); }
	return python_bone("rebuild_shape", joint);
}

MStatus export_bone_collision(MObject joint, xr_bone& bone)
{
	MFnDependencyNode fn(joint);
	if (!fn.hasAttribute("xrayBoneVersion")) return MS::kSuccess;
	try
	{
		bone_attributes a(joint);
		s_bone_shape& shape = bone.shape();
		s_joint_ik_data& ik = bone.joint_ik_data();
		shape.type = uint16_t(a.get("xrayShapeType"));
		ik.type = uint32_t(a.get("xrayJointType"));
		if (shape.type > ST_CYLINDER || ik.type > JT_SLIDER) throw std::runtime_error("Unsupported shape or joint type");
		shape.flags = uint16_t(a.get("xrayShapeFlags")) & ~uint16_t(15);
		for (unsigned i = 0; i != 4; ++i) if (a.get(shape_flags[i])) shape.flags |= uint16_t(1 << i);
		ik.ik_flags = uint32_t(a.get("xrayIKFlags")) & ~uint32_t(JF_BREAKABLE);
		if (a.get("xrayBreakable")) ik.ik_flags |= JF_BREAKABLE;
		bone.mass() = float(a.get("xrayMass"));
		bone.center_of_mass() = a.vector("xrayCenterOfMass");
		bone.gamemtl() = a.plug("xrayGameMaterial").asString().asChar();
		ik.spring_factor = float(a.get("xraySpring"));
		ik.damping_factor = float(a.get("xrayDamping"));
		ik.friction = float(a.get("xrayFriction"));
		ik.break_force = float(a.get("xrayBreakForce"));
		ik.break_torque = float(a.get("xrayBreakTorque"));
		if (bone.mass() < 0 || ik.spring_factor < 0 || ik.damping_factor < 0 || ik.friction < 0 ||
			ik.break_force < 0 || ik.break_torque < 0) throw std::runtime_error("Negative physical parameter");
		for (unsigned i = 0; i != 3; ++i)
		{
			std::string base = std::string("xrayLimit") + axes[i];
			double lo = a.angle((base + "Min").c_str()), hi = a.angle((base + "Max").c_str());
			if (ik.type == JT_SLIDER && i == 0)
			{
				lo = a.distance("xraySlideMin") * .01;
				hi = a.distance("xraySlideMax") * .01;
			}
			if (lo > hi) throw std::runtime_error("IK minimum exceeds maximum");
			ik.limits[i].limit.set(float(-hi), float(-lo));
			ik.limits[i].spring_factor = float(a.get((base + "Spring").c_str()));
			ik.limits[i].damping_factor = float(a.get((base + "Damping").c_str()));
			if (ik.limits[i].spring_factor < 0 || ik.limits[i].damping_factor < 0)
				throw std::runtime_error("Negative IK spring or damping");
		}
		const fvector3 offset = a.vector("xrayShapeOffset");
		const MMatrix matrix = MEulerRotation(a.angle("xrayShapeRotateX"),
			a.angle("xrayShapeRotateY"), a.angle("xrayShapeRotateZ")).asMatrix();
		if (shape.type == ST_BOX)
		{
			shape.box.translate = offset;
			fvector3* rows[] = { &shape.box.rotate.i, &shape.box.rotate.j, &shape.box.rotate.k };
			for (unsigned i = 0; i != 3; ++i)
			{
				const double sign = i == 2 ? -1.0 : 1.0;
				rows[i]->set(float(sign * matrix[i][0]), float(sign * matrix[i][1]), float(-sign * matrix[i][2]));
			}
			shape.box.halfsize.set(float(a.get("xrayHalfSizeX") * .01),
				float(a.get("xrayHalfSizeY") * .01), float(a.get("xrayHalfSizeZ") * .01));
			if (shape.box.halfsize.x <= 0 || shape.box.halfsize.y <= 0 || shape.box.halfsize.z <= 0)
				throw std::runtime_error("Box half sizes must be positive");
		}
		else if (shape.type == ST_SPHERE)
		{
			shape.sphere.p = offset;
			shape.sphere.r = float(a.get("xrayRadius") * .01);
			if (shape.sphere.r <= 0) throw std::runtime_error("Sphere radius must be positive");
		}
		else if (shape.type == ST_CYLINDER)
		{
			shape.cylinder.center = offset;
			shape.cylinder.direction.set(float(matrix[1][0]), float(matrix[1][1]), float(-matrix[1][2]));
			shape.cylinder.radius = float(a.get("xrayRadius") * .01);
			shape.cylinder.height = float(a.get("xrayHalfHeight") * .02);
			if (shape.cylinder.radius <= 0 || shape.cylinder.height <= 0)
				throw std::runtime_error("Cylinder radius and height must be positive");
		}
	}
	catch (const std::exception& e) { return error(e, joint); }
	return MS::kSuccess;
}
