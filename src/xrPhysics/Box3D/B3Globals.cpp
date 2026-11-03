#include "StdAfx.h"
#include "PhysicsCommon.h"
#include "IPHCapture.h"
#include "ActorCameraCollision.h"
#include "ICollideValidator.h"
#include "MathUtils.h"

// Shared physics globals that the ODE backend keeps in Physics.cpp.
const float default_w_limit = 9.8174770f;
const float default_l_limit = 150.f;
const float default_l_scale = 1.01f;
const float default_w_scale = 1.01f;
const float default_k_l = 0.0002f;
const float default_k_w = 0.05f;

extern const u16 max_joint_allowed_for_exeact_integration = 30;

const float base_fixed_step = 0.02f;
const float base_erp = 0.54545456f;
const float base_cfm = 1.1363636e-006f;

float fixed_step = 0.00666666667f; // 150 Hz, same as ODE
float world_cfm = Cfm(Spring(base_cfm, base_erp, base_fixed_step), Damping(base_cfm, base_erp));
float world_erp = Erp(Spring(base_cfm, base_erp, base_fixed_step), Damping(base_cfm, base_erp));
float world_spring = 1.0f * Spring(world_cfm, world_erp);
float world_damping = 1.0f * Damping(world_cfm, world_erp);

const float default_world_gravity = 2 * 9.81f;

int phIterations = 18;
float phTimefactor = 1.f;
Fbox phBoundaries = {1000.f, 1000.f, -1000.f, -1000.f, 0.f, 0.f};

XRPHYSICS_API const float phInfinity = flt_max;

XRPHYSICS_API float g_CameraCaptureMotorForce = 50000.f;
XRPHYSICS_API bool g_CameraCaptureNoCollision = false;
XRPHYSICS_API bool g_CameraCaptureSelfCollision = false;
XRPHYSICS_API bool g_CameraCaptureExactIntegration = false;
XRPHYSICS_API SCameraCaptureShellInfo g_CameraCaptureShellInfo = {0, 0, false};

CPhysicsShell* actor_camera_shell = nullptr;
#ifdef DEBUG
bool dbg_draw_camera_collision = false;
float camera_collision_character_skin_depth = 0.4f;
float camera_collision_character_shift_z = 0.3f;
#endif

static CGID g_FreeGroupId = 1; // 0 means no group

CGID RegisterGroup()
{
	return g_FreeGroupId++;
}
