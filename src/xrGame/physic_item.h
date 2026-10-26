////////////////////////////////////////////////////////////////////////////
//	Module 		: physic_item.h
//	Created 	: 11.02.2004
//  Modified 	: 11.02.2004
//	Author		: Dmitriy Iassenev
//	Description : Physic item
////////////////////////////////////////////////////////////////////////////
#pragma once

#include "GameObject.h"
#include "PhysicsShellHolder.h"
#include "ph_shell_interface.h"

class CPhysicItem :
	public CPhysicsShellHolder,
	public IPhysicShellCreator
{
	typedef CPhysicsShellHolder inherited;
public:
	bool			m_ready_to_destroy;

public:
					CPhysicItem						();
	virtual ~CPhysicItem					() override;
			void	init							();
	virtual void	reinit							() override;
	virtual void	Load							(const char* section) override;
	virtual void	reload							(const char* section) override;
	virtual void	OnH_B_Independent				(bool just_before_destroy) override;
	virtual void	OnH_B_Chield					() override;
	virtual void	UpdateCL						() override;
	virtual bool	net_Spawn						(CSE_Abstract* DC) override;
	virtual void	net_Destroy						() override;
	virtual void	activate_physic_shell			() override;
	virtual void	setup_physic_shell				() override;
	virtual void	create_box_physic_shell			();
	virtual void	create_box2sphere_physic_shell	();
	virtual void	create_physic_shell				() override;
	virtual CPhysicItem* cast_physics_item() override { return this; }
	virtual CPhysicsShellHolder* cast_physics_shell_holder() override { return this; }
	virtual void CreatePhysicsShell() override;

	virtual void Serialize(ISaveObject& Object) override { inherited::Serialize(Object); }
};