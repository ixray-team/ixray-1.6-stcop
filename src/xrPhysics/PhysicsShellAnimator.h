#pragma once
#include "PhysicsShell.h"
#include "PHShell.h"

//Содержит информацию об целевой матрице анимации
struct CPhysicsShellAnimatorBoneData
{
	dJointID m_anim_fixed_dJointID;
	CPHElement* m_element;
};

class animation_movement_controller;
class CPhysicsShellAnimator
{
	xr_vector<CPhysicsShellAnimatorBoneData> m_bones_data;
	CPhysicsShell* m_pPhysicsShell;
	Fmatrix m_StartXFORM;

	void CreateJoints(const char* controled);
	void CreateJoint(CPHElement* e);

public:
	CPhysicsShellAnimator(CPhysicsShell* _pPhysicsShell, CInifile const* ini, const char* section);
	~CPhysicsShellAnimator();
	void OnFrame(bool calculate_bones = true);
};