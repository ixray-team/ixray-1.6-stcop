#pragma once

struct SHit final
{
	SHit(float powerA, Fvector& dirA, CObject* whoA, u16 elementA, Fvector p_in_bone_spaceA, float impulseA, ALife::EHitType hit_typeA, float armor_piercingA, bool AimBullet);

	SHit();
	bool is_valide() const;
	void invalidate();
	IC float damage() const { VERIFY(is_valide()); return power; }
	IC const Fvector& direction() const { VERIFY(is_valide()); return dir; }
	IC const CObject* initiator() const { VERIFY(is_valide()); return who; }
	IC u16	bone() const { VERIFY(is_valide()); return boneID; }
	IC const Fvector& bone_space_position()	const { VERIFY(is_valide()); return p_in_bone_space; }
	IC float phys_impulse()	const { VERIFY(is_valide()); return impulse; }
	IC ALife::EHitType type() const { VERIFY(is_valide()); return hit_type; }
	void Read_Packet(NET_Packet	P);
	void Read_Packet_Cont(NET_Packet P);
	void Write_Packet(NET_Packet& P);
	void Write_Packet_Cont(NET_Packet& P);

	void GenHeader(u16 PacketType, ALife::_OBJECT_ID ID);

	u32 Time = 0;
	u16 PACKET_TYPE = 0;
	ALife::_OBJECT_ID DestID = 0;

	float power = 0.0f;
	Fvector	dir = zero_vel;
	CObject* who = nullptr;
	ALife::_OBJECT_ID whoID = 0;
	ALife::_OBJECT_ID weaponID = 0;
	u16	boneID = BI_NONE;
	Fvector	p_in_bone_space = zero_vel;
	float impulse = 0.0f;

	ALife::EHitType	hit_type = ALife::EHitType::eHitTypeMax;
	float armor_piercing = 0.0f;
	bool add_wound = false;
	bool aim_bullet = false;
	//---------------------------------------------------
	//GE_HIT_STATISTIC
	u32 BulletID = 0;
	u32	SenderID = 0;
#ifdef DEBUG
	void _dump();
#endif
};