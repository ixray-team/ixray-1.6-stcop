#pragma once

#include "weaponcustompistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponShotgun :	public CWeaponCustomPistol
{
	typedef CWeaponCustomPistol inherited;
public:
					CWeaponShotgun		();
	virtual			~CWeaponShotgun		();

	virtual void	Load				(LPCSTR section);
	
	// Happens when object gets online. Import saved data from server here. Do oject initialization based on data imported from server
	virtual BOOL	net_Spawn			(CSE_Abstract* server_entity);
	virtual void	net_Destroy			();
	// constant export of various weapon data to server. Here should be stuff, needed to be saved before object switches offline. Than it should be imported in net_spawn, when object gets online again
	virtual void	net_Export			(NET_Packet& P);
	// not happens in Single Player
	virtual void	net_Import			(NET_Packet& P);

	virtual void	Reload				();
	virtual void	switch2_Fire		();
	void			switch2_StartReload ();
	void			switch2_AddCartgidge();
	void			switch2_EndReload	();

	virtual void	PlayAnimOpenWeapon	();
	virtual void	PlayAnimAddOneCartridgeWeapon();
	void			PlayAnimCloseWeapon	();

	virtual bool	Action(u16 cmd, u32 flags);

protected:
	virtual void	OnAnimationEnd		(u32 state);
	void			TriStateReload		();
	virtual void	OnStateSwitch		(u32 S);

	bool			HaveCartridgeInInventory(u8 cnt);
	virtual u8		AddCartridge		(u8 cnt);

	ESoundTypes		m_eSoundOpen;
	ESoundTypes		m_eSoundAddCartridge;
	ESoundTypes		m_eSoundClose;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
