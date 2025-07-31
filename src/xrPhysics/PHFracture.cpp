#include "stdafx.h"
#include "PHFracture.h"
#include "Physics.h"
#include "PHElement.h"
#include "PHShell.h"
#include "console_vars.h"

#include "../Include/xrRender/Kinematics.h"
#include "ph_valid_ode.h"
#include "../xrEngine/bone.h"

#pragma warning(disable:4995)
#pragma warning(disable:4267)
#include "../3rd party/ode/ode/src/joint.h"
#pragma warning(default:4995)
#pragma warning(default:4267)

extern class CPHWorld* ph_world;
static const float torque_factor = 10000000.f;

CPHFracturesHolder::CPHFracturesHolder()
{
	m_has_breaks=false;
}

CPHFracturesHolder::~CPHFracturesHolder()
{
	m_has_breaks=false;
	m_fractures.clear();
	m_impacts.clear();
	m_feedbacks.clear();
}

void CPHFracturesHolder::ApplyImpactsToElement(CPHElement* E)
{
	PH_IMPACT_I i = m_impacts.begin(), e = m_impacts.end();
	BOOL ac_state = E->isActive();

	E->m_flags.set(CPHElement::flActive, TRUE);
	for (; e != i; ++i)
	{
		E->applyImpact(*i);
	}

	E->m_flags.set(CPHElement::flActive, ac_state);
}

element_fracture CPHFracturesHolder::SplitFromEnd(CPHElement* element, u16 fracture)
{
	FRACTURE_I fract_i = m_fractures.begin() + fracture;
	SubFractureMass(fracture);

	CPHElement* new_element = cast_PHElement(P_create_Element());
	new_element->m_SelfID = fract_i->m_bone_id;
	new_element->mXFORM.set(element->mXFORM);
	element->PassEndGeoms(fract_i->m_start_geom_num, fract_i->m_end_geom_num, new_element);
	/////////////////////////////////////////////
	IKinematics* pKinematics = element->m_shell->PKinematics();
	const CBoneInstance& new_bi = pKinematics->LL_GetBoneInstance(new_element->m_SelfID);
	const CBoneInstance& old_bi = pKinematics->LL_GetBoneInstance(element->m_SelfID);

	Fmatrix shift_pivot = new_bi.mTransform;
	shift_pivot.invert();
	shift_pivot.mulB_43(old_bi.mTransform);

	float density = element->getDensity();
	new_element->SetShell(element->PHShell());
	Fmatrix current_transtform;
	element->GetGlobalTransformDynamic(&current_transtform);
	InitNewElement(new_element, shift_pivot, density);

	Fmatrix shell_form;
	element->PHShell()->GetGlobalTransformDynamic(&shell_form);

	current_transtform.mulA_43(shell_form);
	new_element->SetTransform(current_transtform, mh_unspecified);

	ApplyImpactsToElement(new_element);
	element_fracture ret = std::make_pair(new_element, (CShellSplitInfo)(*fract_i));

	if (m_fractures.size() - fracture > 0)
	{
		if (new_element->m_fratures_holder == nullptr)//create fractures holder if it was not created before
		{
			new_element->m_fratures_holder = new CPHFracturesHolder();
		}
		PassEndFractures(fracture, new_element);
	}

	return ret;
}

void CPHFracturesHolder::PassEndFractures(u16 from,CPHElement* dest)
{
	FRACTURE_I i=m_fractures.begin(),i_from=m_fractures.begin()+from,e=m_fractures.end();
	u16 end_geom=i_from->m_end_geom_num;
	u16 begin_geom_num=i_from->m_start_geom_num;
	u16 leaved_geoms=begin_geom_num;
	u16 passed_geoms=end_geom-begin_geom_num;
	if(i_from==e) return;

	for(;i!=i_from;++i)//correct end geoms for fractures leaved in source
	{
		u16& cur_end_geom=i->m_end_geom_num;
		if(cur_end_geom>begin_geom_num) cur_end_geom=cur_end_geom-passed_geoms;
	}

	i++; // omit used fracture;
	//these to be passed
	for(;i!=e;i++)//itterate antil a fracture where geom num > end geom num
	{
		u16 &cur_end_geom	=i->m_end_geom_num;
		u16 &cur_geom		=i->m_start_geom_num;
		if(cur_geom>=end_geom) break;
		cur_end_geom=cur_end_geom-leaved_geoms;
		cur_geom=cur_geom-leaved_geoms;
	}
	FRACTURE_I i_to=i;
	for(;i!=e;++i)//correct data in the rest leaved fractures
	{
		u16 &cur_end_geom	=i->m_end_geom_num;
		u16 &cur_geom		=i->m_start_geom_num;
		cur_end_geom		=cur_end_geom-passed_geoms;
		cur_geom			=cur_geom-passed_geoms;
	}

	if(i_from + 1 != i_to)//insure it!!
	{
	
	CPHFracturesHolder* &dest_fract_holder=dest->m_fratures_holder;
	if (!dest_fract_holder) dest_fract_holder = new CPHFracturesHolder();

	//pass fractures not including end fracture
	dest_fract_holder->m_fractures.insert(dest_fract_holder->m_fractures.end(),i_from+1,i_to);
	}
	m_fractures.erase(i_from,i_to);//erase along whith used fracture
}
void CPHFracturesHolder::SplitProcess(CPHElement* element, ELEMENT_PAIR_VECTOR& new_elements)
{
	u16 i = u16(m_fractures.size() - 1);

	for (; i != u16(-1); i--)
	{
		if (m_fractures[i].Breaked())
		{
			new_elements.push_back(SplitFromEnd(element, i));
		}
	}
}

void CPHFracturesHolder::InitNewElement(CPHElement* element, const Fmatrix& shift_pivot, float density)
{
	element->CreateSimulBase();
	element->ReInitDynamics(shift_pivot, density);
	VERIFY(dBodyStateValide(element->get_body()));
}

void CPHFracturesHolder::PhTune(dBodyID body)
{
	//iterate through all body's joints and set joints feedbacks where is not already set
	//contact feedbacks stored in global storage - ContactFeedBacks wich cleared on each step
	//breacable joints already has their feedbacks, 
	//feedbacks for rest noncontact joints stored in m_feedbacks in runtime in this function and
	//and killed by destructor

	int num = dBodyGetNumJoints(body);
	for (int i = 0; i < num; ++i)
	{
		dJointID joint = dBodyGetJoint(body, i);

		if (dJointGetType(joint) == dJointTypeContact)
		{
			dJointSetFeedback(joint, ContactFeedBacks.add());
		}
		else
		{
			CPHJoint* ph_joint = (CPHJoint*)dJointGetData(joint);
			if (!(ph_joint && ph_joint->JointDestroyInfo())) dJointSetFeedback(joint, ContactFeedBacks.add());
		}
	}
}

bool CPHFracturesHolder::PhDataUpdate(CPHElement* element)
{
	FRACTURE_I i = m_fractures.begin(), e = m_fractures.end();
	for (; i != e; ++i)
	{
		m_has_breaks = i->Update(element) || m_has_breaks;
	}

	if (!m_has_breaks)
		m_impacts.clear();

	return m_has_breaks;
}

void CPHFracturesHolder::AddImpact(const Fvector& force,const Fvector& point,u16 id)
{
	m_impacts.push_back(SPHImpact(force,point,id));
}

u16 CPHFracturesHolder::AddFracture(const CPHFracture& fracture)
{
	m_fractures.push_back(fracture);
	return u16(m_fractures.size()-1);
}

CPHFracture& CPHFracturesHolder::Fracture(u16 num)
{
	R_ASSERT2(num<m_fractures.size(),"out of range!");
	return m_fractures[num];
}

void CPHFracturesHolder::DistributeAdditionalMass(u16 geom_num, const dMass& m)
{
	FRACTURE_I f_i = m_fractures.begin(), f_e = m_fractures.end();
	for (; f_i != f_e; ++f_i)
	{
		R_ASSERT2(u16(-1) != f_i->m_start_geom_num, "fracture does not initialized!");

		if (f_i->m_end_geom_num == u16(-1))
			f_i->MassAddToSecond(m);
		else
			f_i->MassAddToFirst(m);
	}
}

void CPHFracturesHolder::SubFractureMass(u16 fracture_num)
{
	FRACTURE_I f_i = m_fractures.begin(), f_e = m_fractures.end();
	FRACTURE_I fracture = f_i + fracture_num;
	u16 start_geom = fracture->m_start_geom_num;
	u16	end_geom = fracture->m_end_geom_num;
	dMass& second_mass = fracture->m_secondM;
	dMass& first_mass = fracture->m_firstM;
	for (; f_i != f_e; ++f_i)
	{
		if (f_i == fracture) continue;
		R_ASSERT2(start_geom != f_i->m_start_geom_num, "Double fracture!!!");

		if (start_geom > f_i->m_start_geom_num)
		{

			if (end_geom <= f_i->m_end_geom_num)
			{
				f_i->MassSubFromSecond(second_mass);//tag fracture is in current
			}
			else
			{
				R_ASSERT2(start_geom >= f_i->m_end_geom_num, "Odd fracture!!!");
				f_i->MassSubFromFirst(second_mass);//tag fracture is ouside current
			}
		}
		else
		{
			if (end_geom >= f_i->m_end_geom_num)
			{
				f_i->MassSubFromFirst(first_mass);//current fracture is in tag
			}
			else
			{
				R_ASSERT2(end_geom <= f_i->m_start_geom_num, "Odd fracture!!!");
				f_i->MassSubFromFirst(second_mass);//tag fracture is ouside current
			}
		}
	}
}

CPHFracture::CPHFracture()
{
	m_start_geom_num = u16(-1);
	m_end_geom_num = u16(-1);
	m_breaked = false;
}

bool CPHFracture::Update(CPHElement* element)
{
	dBodyID body = element->get_body();
	CPHFracturesHolder* holder = element->FracturesHolder();
	PH_IMPACT_STORAGE& impacts = holder->Impacts();

	Fvector second_force{}, first_force{};
	Fvector second_torque{}, first_torque{};

	const Fvector& body_pos = *(const Fvector*)dBodyGetPosition(body);
	Fvector body_to_first{ *((const Fvector*)m_firstM.c) };
	Fvector body_to_second{ *((const Fvector*)m_secondM.c) };

	const int num_joints = dBodyGetNumJoints(body);
	for (int i = 0; i < num_joints; ++i)
	{
		dJointID joint = dBodyGetJoint(body, i);
		dJointFeedback* feedback = dJointGetFeedback(joint);
		VERIFY2(feedback, "Feedback was not set!!!");

		dxJoint* b_joint = (dxJoint*)joint;
		bool b_body_second = (b_joint->node[1].body == body);

		Fvector joint_pos;
		bool applied_to_second = false;

		if (dJointGetType(joint) == dJointTypeContact)
		{
			dxJointContact* c_joint = (dxJointContact*)joint;
			joint_pos.set(*(Fvector*)c_joint->contact.geom.pos);

			auto check_geom = [&](dGeomID geom)
			{
				if (dGeomGetClass(geom) == dGeomTransformClass)
					geom = dGeomTransformGetGeom(geom);

				if (auto* user_data = dGeomGetUserData(geom))
				{
					const u16 pos = user_data->element_position;
					if (pos < element->numberOfGeoms() && pos >= m_start_geom_num && pos < m_end_geom_num && geom == element->Geom(pos)->geometry())
					{
						applied_to_second = true;
					}
				}
			};
			check_geom(c_joint->contact.geom.g1);
			check_geom(c_joint->contact.geom.g2);
		}
		else
		{
			CPHJoint* J = (CPHJoint*)dJointGetData(joint);
			if (!J) continue;

			J->PSecondElement()->InterpolateGlobalPosition(&joint_pos);
			if (CODEGeom* root_geom = J->RootGeom())
			{
				const u16 pos = root_geom->element_position();
				if (element == J->PFirst_element() && pos < element->numberOfGeoms() && pos >= m_start_geom_num && pos < m_end_geom_num)
				{
					applied_to_second = true;
				}
			}
		}

		Fvector shoulder, joint_force, torque;
		Fvector body_to_joint; body_to_joint.sub(joint_pos, body_pos);
		shoulder.sub(body_to_joint, applied_to_second ? body_to_second : body_to_first);
		joint_force.set
		(
			b_body_second ? feedback->f2[0] : feedback->f1[0],
			b_body_second ? feedback->f2[1] : feedback->f1[1],
			b_body_second ? feedback->f2[2] : feedback->f1[2]
		);

		if (applied_to_second)
		{
			second_force.add(joint_force);
			torque.crossproduct(shoulder, joint_force);
			second_torque.add(torque);
		}
		else
		{
			first_force.add(joint_force);
			torque.crossproduct(shoulder, joint_force);
			first_torque.add(torque);
		}
	}

	for (const auto& impact : impacts)
	{
		Fvector force = impact.force;
		Fvector point = impact.point;
		Fvector shoulder, torque;

		if (impact.geom >= m_start_geom_num && impact.geom < m_end_geom_num)
		{
			force.mul(ph_console::phRigidBreakWeaponFactor);
			shoulder.sub(body_to_second, point);
			second_force.add(force);
			torque.crossproduct(shoulder, force);
			second_torque.add(torque);
		}
		else
		{
			shoulder.sub(body_to_first, point);
			first_force.add(force);
			torque.crossproduct(shoulder, force);
			second_torque.add(torque); // Возможно тут ошибка, torque может идти в first_torque?
		}
	}

	Fvector gravity_force(0.f, -ph_world->Gravity() * m_firstM.mass, 0.f);
	first_force.add(gravity_force);
	second_force.add(gravity_force);

	// Inertia tensors
	dMatrix3 glI1, glI2, glInvI, tmp;
	dMULTIPLY2_333(tmp, body->invI, body->R);
	dMULTIPLY0_333(glInvI, body->R, tmp);
	dMULTIPLY2_333(tmp, m_firstM.I, body->R);
	dMULTIPLY0_333(glI1, body->R, tmp);
	dMULTIPLY2_333(tmp, m_secondM.I, body->R);
	dMULTIPLY0_333(glI2, body->R, tmp);

	// Compute break torque
	Fvector break_torque, vtemp;
	dMULTIPLY0_331((float*)&break_torque, glInvI, (float*)&first_torque);
	dMULTIPLY0_331((float*)&break_torque, glI2, (float*)&break_torque);
	dMULTIPLY0_331((float*)&vtemp, glInvI, (float*)&second_torque);
	dMULTIPLY0_331((float*)&vtemp, glI1, (float*)&vtemp);
	break_torque.sub(vtemp);

	if (break_torque.magnitude() * ph_console::phBreakCommonFactor > m_break_torque * torque_factor)
	{
		m_pos_in_element.set(second_force);
		m_break_force = second_torque.x;
		m_break_torque = second_torque.y;
		m_add_torque_z = second_torque.z;
		m_breaked = true;
		return true;
	}

	// Compute break force
	Fvector break_force(first_force); break_force.mul(m_secondM.mass);
	vtemp.set(second_force); vtemp.mul(m_firstM.mass);
	break_force.sub(vtemp);
	break_force.mul(1.f / element->getMass());

	const float bfm = break_force.magnitude() * ph_console::phBreakCommonFactor;
	if (m_break_force < bfm)
	{
		second_force.mul(bfm / m_break_force);
		m_pos_in_element.set(second_force);
		m_break_force = second_torque.x;
		m_break_torque = second_torque.y;
		m_add_torque_z = second_torque.z;
		m_breaked = true;
		return true;
	}

	return m_breaked;
}


void CPHFracture::SetMassParts(const dMass& first,const dMass& second)
{
	m_firstM=first;
	m_secondM=second;
}

void CPHFracture::MassAddToFirst(const dMass& m)
{
	dMassAdd(&m_firstM,&m);
}

void CPHFracture::MassAddToSecond(const dMass& m)
{
	dMassAdd(&m_secondM,&m);
}
void CPHFracture::MassSubFromFirst(const dMass& m)
{
	dMassSub(&m_firstM,&m);
}
void CPHFracture::MassSubFromSecond(const dMass& m)
{
	dMassSub(&m_secondM,&m);
}
void CPHFracture::MassSetFirst(const dMass& m)
{
	m_firstM=m;
}
void CPHFracture::MassSetSecond(const dMass& m)
{
	m_secondM=m;
}
void CPHFracture::MassUnsplitFromFirstToSecond(const dMass& m)
{
	dMassSub(&m_firstM,&m);
	dMassAdd(&m_secondM,&m);
}
void CPHFracture::MassSetZerro()
{
	dMassSetZero(&m_firstM);
	dMassSetZero(&m_secondM);
}