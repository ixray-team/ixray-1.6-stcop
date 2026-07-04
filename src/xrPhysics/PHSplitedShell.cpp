#include "StdAfx.h"
#include "PhysicsShell.h"
#include "PHObject.h"
#include "PHWorld.h"
#include "PHInterpolation.h"
#include "PHShell.h"
#include "PHJoint.h"
#include "PHElement.h"
#include "PHSplitedShell.h"
#include "Physics.h"
#include "SpaceUtils.h"

void CPHSplitedShell::Collide()
{
	CollideStatic(dSpacedGeom(), CPHObject::SelfPointer());
}

void CPHSplitedShell::get_spatial_params()
{
	spatialParsFromDGeom((dGeomID)m_space, SpatialComponent->sphere.P, AABB, SpatialComponent->sphere.R);

	if (SpatialComponent->sphere.R > m_max_AABBradius)
	{
		SpatialComponent->sphere.R = m_max_AABBradius;
	}
}

void CPHSplitedShell::DisableObject()
{
	CPHObject::deactivate();
}