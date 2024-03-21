#pragma once
class CCar;

class CCarDoor :
	public CDamagableHealthItem
{
	using inherited = CDamagableHealthItem;

public:
	bool	update;
	u16		bone_id;
	float	torque;
	float	a_vel;
	float	pos_open;
	float	opened_angle;
	float	closed_angle;
	u32		open_time;

	CCar*			pCar;
	CPhysicsJoint*	Joint;

	Fvector2		door_plane_ext;
	Ivector2		door_plane_axes;
	Fvector			door_dir_in_door;
	Fmatrix			closed_door_form_in_object;

	void Use();
	void Switch();
	void Init();
	void Open();
	void Close();
	void Break();

	virtual void ApplyDamage(u16 level);
	void Update();
	float GetAngle();
	bool CanEnter(const Fvector& pos, const Fvector& dir, const Fvector& foot_pos);
	bool IsInArea(const Fvector& pos, const Fvector& dir);
	bool IsFront(const Fvector& pos, const Fvector& dir);
	bool CanExit(const Fvector& pos, const Fvector& dir);
	bool TestPass(const Fvector& pos, const Fvector& dir);
	void GetExitPosition(Fvector& pos);
	void ApplyOpenTorque();
	void ApplyTorque(float atorque, float aa_vel);
	void ApplyCloseTorque();
	void NeutralTorque(float atorque);
	void ClosingToClosed();
	void ClosedToOpening();
	void PlaceInUpdate();
	void RemoveFromUpdate();
	void SaveNetState(NET_Packet& P);
	void RestoreNetState(const CSE_ALifeCar::SDoorState& a_state);
	void SetDefaultNetState();

	CCarDoor(CCar* acar);
public:
	struct SDoorway
	{
		Fvector2	door_plane_ext;
		Ivector2	door_plane_axes;
		CCarDoor* door;

		SDoorway();
		void Init(CCarDoor* adoor);
	};

	enum eState
	{
		opening,
		closing,
		opened,
		closed,
		broken
	};

	eState state;
};
