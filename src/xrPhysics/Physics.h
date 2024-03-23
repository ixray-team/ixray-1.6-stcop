#pragma once

#include "dCylinder/dCylinder.h"
#include "PhysicsShell.h"
#include "PHObject.h"
#include "PHInterpolation.h"
#include "../xrcore/_cylinder.h"
#include "BlockAllocator.h"
#include "PhysicsCommon.h"
#include "PHWorld.h"
#include "PHContactBodyEffector.h"
#include "phvalide.h"

///////////////////////////////////////////////////////////////////////////////
		void	BodyCutForce				(dBodyID body,float l_limit,float w_limit)					;
		void	dBodyAngAccelFromTorqu		(const dBodyID body, dReal* ang_accel, const dReal* torque)	;
		float	E_NLD						(dBodyID b1,dBodyID b2,const dReal* norm);

		void	ApplyGravityAccel			(dBodyID body,const dReal* accel);
const	dReal	fix_ext_param				=10000.f;
const	dReal	fix_mass_param				=100000000.f;
		void	FixBody						(dBodyID body)												;
		void	dMassSub					(dMass *a,const dMass *b)									;
		void	SaveContacts				(dGeomID o1, dGeomID o2,dJointGroupID jointGroup)			;
const	dReal	*dJointGetPositionContact	(dJointID joint);

extern class CBlockAllocator<dJointFeedback,128> ContactFeedBacks;
extern CBlockAllocator<CPHContactBodyEffector,128> ContactEffectors;

void NearCallback(CPHObject* obj1,CPHObject* obj2, dGeomID o1, dGeomID o2);
void CollideStatic(dGeomID o2,CPHObject* obj2);

class CPHElement;
class CPHShell;
extern dJointGroupID ContactGroup;
extern Fbox			 phBoundaries;