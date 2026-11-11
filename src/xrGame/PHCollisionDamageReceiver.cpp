#include "StdAfx.h"
#include "PHCollisionDamageReceiver.h"
#include "../xrPhysics/IPhysicsShellHolder.h"
#include "../xrCore/xr_ini.h"
#include "../Include/xrRender/Kinematics.h"
#include "../xrPhysics/PhysicsShell.h"
//#include "../xrEngine/gamemtllib.h"
//#include "Physics.h"
#include "xrMessages.h"
#include "CharacterPhysicsSupport.h"
void CPHCollisionDamageReceiver::BoneInsert(u16 id,float k)
{
	R_ASSERT2(FindBone(id)==m_controled_bones.end(),"duplicate bone!");
	m_controled_bones.push_back(SControledBone(id,k));
}

void CPHCollisionDamageReceiver::Init()
{
	CPhysicsShellHolder* sh = PPhysicsShellHolder();
	IKinematics* K = PKinematics(sh->Visual());
	CInifile* ini = K->LL_UserData();
	if (ini && ini->section_exist("collision_damage"))
	{
		CInifile::Sect& data = ini->r_section("collision_damage");
		for (CInifile::SectCIt I = data.Data.begin(); I != data.Data.end(); I++)
		{
			const CInifile::Item& item = *I;
			u16 index = K->LL_BoneID(*item.first);
			R_ASSERT3(index != BI_NONE, "Wrong bone name", *item.first);
			BoneInsert(index, float(atof(*item.second)));
			CODEGeom* og = sh->PPhysicsShell()->get_GeomByID(index);
			//R_ASSERT3(og, "collision damage bone has no physics collision", *item.first);
			if (og)
				PHGeomAddObjectContactCallback(og, DamageReceiverCollisionCallback);
		}

	}
}

void CPHCollisionDamageReceiver::CollisionHit(ALife::_OBJECT_ID source_id,u16 bone_id,float power,const Fvector& dir,Fvector &pos )
{
	// Only bones registered from [collision_damage] may send a hit or start the
	// cooldown. A stray callback for any other bone is ignored here.
	DAMAGE_BONES_I i=FindBone(bone_id);
	if(i==m_controled_bones.end())return;
	power*=i->second;
	if(power<m_hit_threshold)return;

	// One impact can reach several nearby collision geoms in the same frame or
	// in consecutive frames. Send only the first damaging hit and ignore the
	// rest for a short cooldown, otherwise the car takes double damage.
	const u32 Now=Device.dwTimeGlobal;
	if(m_hit_cooldown>0.f)
	{
		if(m_last_hit_time!=0 && Now-m_last_hit_time<u32(m_hit_cooldown*1000.f))
			return;
	}
	// Do not let a hit that cannot damage this object (or a wheel contact)
	// suppress the next real body hit.
	const bool StartsCooldown=CollisionHitStartsCooldown(bone_id,power);
	
	NET_Packet		P;
	CPhysicsShellHolder *ph=PPhysicsShellHolder();
	SHit	HS;
	
	HS.GenHeader(GE_HIT,ph->ID());					//	ph->u_EventGen(P,GE_HIT,ph->ID());
	// A hit must have a valid source entity: the single-player server drops
	// GE_HIT events whose source cannot be resolved (static level geometry,
	// water, etc.). Attribute such hits to the car itself; the weapon field
	// still carries the original source for diagnostics.
	HS.whoID			= (source_id == ALife::INVALID_OBJECT_ID && ph->cast_car()) ? ph->ID() : source_id;
	HS.weaponID			= source_id;				//	P.w_u16		(source_id);
	HS.dir				= dir;						//	P.w_dir		(dir);
	HS.power			= power;					//	P.w_float	(power);
	HS.boneID			= s16(bone_id);				//	P.w_s16		(s16(bone_id));
	HS.p_in_bone_space	= pos;						//	P.w_vec3	(pos);
	HS.impulse			= 0.f;						//	P.w_float	(0.f);
	HS.hit_type			= (ALife::eHitTypeStrike);	//	P.w_u16		(ALife::eHitTypeStrike);
	HS.Write_Packet(P);						

	ph->u_EventSend(P);

	// Start the cooldown only for a hit that is going to damage the object.
	if (StartsCooldown && m_hit_cooldown>0.f)
		m_last_hit_time = Now;
}

void CPHCollisionDamageReceiver::Clear()
{
	//IPhysicsShellHolder *sh	=PPhysicsShellHolder	();
	//xr_map<u16,float>::iterator i=m_controled_bones.begin(),e=m_controled_bones.end();
	//for(;e!=i;++i)
	//{
	//	CODEGeom* og= sh->PPhysicsShell()->get_GeomByID(i->first);
	//	if(og)og->set_obj_contact_cb(nullptr);
	//}
		m_controled_bones.clear();
}