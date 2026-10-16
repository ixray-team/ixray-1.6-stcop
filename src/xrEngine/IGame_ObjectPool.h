#pragma once

// refs
class ENGINE_API				CObject;

//-----------------------------------------------------------------------------------------------------------
class ENGINE_API 				IGame_ObjectPool
{
	typedef xr_vector<CObject*>	ObjectVec;
	typedef ObjectVec::iterator	ObjectVecIt;
	ObjectVec					m_PrefetchObjects;
public:
	void						prefetch			( );
	void						clear				( );

	CObject*					create				( const char*	name	);
	void						destroy				( CObject*	O		);

	static void					RunOnPrimaryThread	(std::function<void()>&& Action);
	static bool					IsParallelLoad		();

	IGame_ObjectPool			();
	virtual ~IGame_ObjectPool	();
};
//-----------------------------------------------------------------------------------------------------------
