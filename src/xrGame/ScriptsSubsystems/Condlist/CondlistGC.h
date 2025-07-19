#pragma once
#include "script_utility.h"
#include "GameObject.h"

class CCondlistGarbageCollector final
{
public:
	CCondlistGarbageCollector() = default;
	~CCondlistGarbageCollector() = default;

	const char* Registry(shared_str Value);

	CCondlistEmbedded& GetEmbedded();
	luabind::functor<bool>& GetFunctorCond(shared_str Name);
	luabind::functor<void>& GetFunctorEffect(shared_str Name);

	void Update();

private:
	xr_set<shared_str> Storage;
	u32 LastFrameCall = 0;

	size_t CondlistIter = 0;
	xr_vector<CCondlistEmbedded> EmbeddedList;

	xr_string_map<shared_str, luabind::functor<bool>> FunctorCondList;
	xr_string_map<shared_str, luabind::functor<void>> FunctorEffectList;
};

extern CCondlistGarbageCollector* GCondlistGC;