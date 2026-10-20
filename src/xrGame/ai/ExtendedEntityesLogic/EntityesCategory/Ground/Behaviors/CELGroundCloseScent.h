#pragma once
class IELAdapter;
class CEntityAlive;
class CELSharedMemory;
struct SELPerceptionProfile;
struct SELDebugRecord;

// Close scent is admitted and refreshed exclusively by a real visual contact.
class CELGroundCloseScent final
{
public:
	void See(IELAdapter& Adapter, CEntityAlive& Target, u32 Time, const SELPerceptionProfile& Profile);
	void Update(IELAdapter& Adapter, CELSharedMemory& Memory, const SELPerceptionProfile& Profile);
	void FillDebug(SELDebugRecord& Record, const SELPerceptionProfile& Profile) const;
	void Remove(u16 Target);
	void Clear() { Contacts.clear(); }

private:
	struct SContact
	{
		u32 SeenAt = 0;
		u32 Generation = 0;
	};
	xr_map<u16, SContact> Contacts;
};
