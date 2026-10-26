#ifndef PHYSICS_SKELETON_OBJECT_H
#define PHYSICS_SKELETON_OBJECT_H
#include "PhysicsShellHolder.h"
#include "PHSkeleton.h"


class CSE_ALifePHSkeletonObject;
class CPhysicsSkeletonObject : 
	public CPhysicsShellHolder,
	public CPHSkeleton

{
typedef CPhysicsShellHolder inherited;

public:
	CPhysicsSkeletonObject(void);
virtual ~CPhysicsSkeletonObject(void) override;


virtual bool					net_Spawn			( CSE_Abstract* DC) override;
virtual void					net_Destroy			() override;
virtual void					Load				(const char* section) override;
virtual void					UpdateCL			( ) override;// Called each frame, so no need for dt
virtual void					shedule_Update		(u32 dt) override;	//
virtual void					net_Save			(NET_Packet& P) override;
virtual bool					net_SaveRelevant	() override;
virtual bool					UsedAI_Locations	() override;
protected:
virtual CPhysicsShellHolder		*PPhysicsShellHolder() override {return PhysicsShellHolder();}
virtual CPHSkeleton				*PHSkeleton			() override {return this;}
virtual void					SpawnInitPhysics	(CSE_Abstract	*D) override;
	virtual void					PHObjectPositionUpdate()																			;
	virtual	void					CreatePhysicsShell	(CSE_Abstract	*e)																;
};



#endif