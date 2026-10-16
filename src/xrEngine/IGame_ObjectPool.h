#pragma once

// refs
class ENGINE_API				CObject;

//-----------------------------------------------------------------------------------------------------------
class ENGINE_API 				IGame_ObjectPool
{
	typedef xr_vector<CObject*>	ObjectVec;
	typedef ObjectVec::iterator	ObjectVecIt;
	ObjectVec					m_PrefetchObjects;

	struct PrefetchItem
	{
		CObject*							Object = nullptr;
		shared_str							Section;
		xr_vector<std::function<void()>>	PrimaryWork;
	};

	xr_vector<PrefetchItem>		m_PendingPrefetch;
	xr_task_group				m_PrefetchTasks;
	bool						m_IsPrefetchRunning = false;

	void						WaitPrefetch		( );
public:
	void						prefetch			( );
	void						FinishPrefetch		( );
	void						clear				( );

	CObject*					create				( const char*	name	);
	void						destroy				( CObject*	O		);

	static void					RunOnPrimaryThread	(std::function<void()>&& Action);
	static bool					IsParallelLoad		();

	IGame_ObjectPool			();
	virtual ~IGame_ObjectPool	();
};
//-----------------------------------------------------------------------------------------------------------
