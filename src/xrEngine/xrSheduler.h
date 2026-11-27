#ifndef XRSHEDULER_H_INCLUDED
#define XRSHEDULER_H_INCLUDED

#include "ISheduled.h"

class	IGame_Level;

class	ENGINE_API	CSheduler
{
private:
	struct Item
	{
		u32			dwTimeForExecute;
		u32			dwTimeOfLastExecute;
		shared_str	scheduled_name;
		ISheduled*	Object;
		// Owning level at registration time. Update() processes only items whose
		// owner matches the current g_pGameLevel, so a parallel (sandbox) level's
		// objects are never updated under the wrong level context.
		IGame_Level* Owner;

		IC bool operator<(const Item& I) const
		{	return dwTimeForExecute > I.dwTimeForExecute; }
	};
	struct	ItemReg
	{
		bool		OP;
		bool		RT;
		ISheduled*	Object;
		IGame_Level* Owner;
	};
private:
	xr_vector<Item>			ItemsRT			;
	xr_vector<Item>			Items			;
	xr_vector<Item>			ItemsProcessed	;
	xr_vector<ItemReg>		Registration	;
	ISheduled*				m_current_step_obj;
	bool					m_processing_now;
	// Serializes Update(): the GameThread and a parallel (sandbox) session's
	// ticker on the primary thread can both pump the scheduler.
	xrCriticalSection		m_update_lock;
public:
	// A parallel session's render pass lends g_pGameLevel while the GameThread
	// may run concurrently; holding this lock for the pass keeps the GameThread
	// scheduler from processing the session's objects mid-render.
	xrCriticalSection&		UpdateMutex() { return m_update_lock; }

	IC void			Push	(Item& I);
	IC void			Pop		();
	IC Item&		Top		()
	{
		return Items.front();
	}
	void			internal_Register		(ISheduled* A, bool RT, IGame_Level* Owner);
	bool			internal_Unregister		(ISheduled* A, bool RT, bool warn_on_not_found = true);
	void			internal_Registration	();
public:
	u64				cycles_start;
	u64				cycles_limit;
public:
	void			ProcessStep	();
	void			Process		();
	void			Update		();

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
