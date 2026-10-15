#ifndef XRSHEDULER_H_INCLUDED
#define XRSHEDULER_H_INCLUDED

#include "ISheduled.h"

extern ENGINE_API bool g_SchedulerPhysicsOverlap;

class	ENGINE_API	CSheduler
{
private:
	struct Item
	{
		u32			dwTimeForExecute;
		u32			dwTimeOfLastExecute;
		shared_str	scheduled_name;
		ISheduled*	Object;
		u32			dwPadding;				// for align-issues

		IC bool operator<(const Item& I) const
		{	return dwTimeForExecute > I.dwTimeForExecute; }
	};
	struct	ItemReg
	{
		bool		OP;
		bool		RT;
		ISheduled*	Object;
	};
private:
	xr_vector<Item>			ItemsRT			;
	xr_vector<Item>			Items			;
	xr_vector<Item>			ItemsProcessed	;
	xr_vector<ItemReg>		Registration	;
	ISheduled*				m_current_step_obj;
	bool					m_processing_now;
	xr_vector<ISheduled*> PhysicsIndependentWork;
	bool PhysicsOverlapEnabled = false;
	bool IsProcessingPhysicsWork = false;

	IC void			Push	(Item& I);
	IC void			Pop		();
	IC Item&		Top		()
	{
		return Items.front();
	}
	void			internal_Register		(ISheduled* A, bool RT=false		);
	bool			internal_Unregister		(ISheduled* A, bool RT, bool warn_on_not_found = true);
	void			internal_Registration	();
public:
	u64				cycles_start;
	u64				cycles_limit;
public:
	void			ProcessStep	();
	void			Process		();
	void			Update		();

	// Owner-thread API. Only explicitly audited scheduled work may use this queue.
	bool QueuePhysicsIndependent(ISheduled* Object);
	bool HasPhysicsIndependentWork() const;
	void CancelPhysicsIndependent(ISheduled* Object);
	void ProcessPhysicsIndependent();
	void SetPhysicsOverlapEnabled(bool Enabled);

#ifdef DEBUG
	bool			Registered	(ISheduled *object) const;
#endif // DEBUG
	void			Register	(ISheduled* A, bool RT=false		);
	void			Unregister	(ISheduled* A						);
	void			EnsureOrder	(ISheduled* Before, ISheduled* After);

	void			Initialize	();
	void			Destroy		();
};

#endif // XRSHEDULER_H_INCLUDED
