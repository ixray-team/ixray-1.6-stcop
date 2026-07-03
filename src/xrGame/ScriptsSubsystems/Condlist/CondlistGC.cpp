#include "StdAfx.h"
#include "CondlistGC.h"
#include "script_game_object.h"

CCondlistGarbageCollector* GCondlistGC = nullptr;

const char* CCondlistGarbageCollector::Registry(shared_str Value)
{
	return **Storage.emplace(Value).first;
}

CCondlistEmbedded& CCondlistGarbageCollector::GetEmbedded()
{
	if (LastFrameCall != Device.dwFrame)
	{
		CondlistIter = 0;
		LastFrameCall = Device.dwFrame;
	}


	PROF_EVENT("Get Embedded");

	if (CondlistIter == EmbeddedList.size())
	{
		CondlistIter++;
		return EmbeddedList.emplace_back();
	}

	CondlistIter++;
	CCondlistEmbedded& Embedded = EmbeddedList[CondlistIter - 1];
	Embedded.Clear();

	return Embedded;
}

luabind::functor<void>& CCondlistGarbageCollector::GetFunctorEffect(shared_str Name)
{
	if (!FunctorEffectList.contains(Name))
	{
		PROF_EVENT("Load EffectFunctor");
		char function_name[ixray::kXRParserFunctionNameBufferSize] = "xr_effects.";
		static constexpr auto sizeOfXRConditionsString = sizeof("xr_effects.");

		R_ASSERT2(sizeOfXRConditionsString + Name.size() <= ixray::kXRParserFunctionNameBufferSize, "overflow!");
		std::memcpy(function_name + (sizeOfXRConditionsString - 1), *Name, Name.size());

		luabind::functor<void>& function_from_xr_conditions = FunctorEffectList[Name];
		R_ASSERT3(ai().script_engine().functor(function_name, function_from_xr_conditions), "Not found function: ", function_name);

		return function_from_xr_conditions;
	}

	return FunctorEffectList[Name];
}

luabind::functor<bool>& CCondlistGarbageCollector::GetFunctorCond(shared_str Name)
{
	if (!FunctorCondList.contains(Name))
	{
		PROF_EVENT("Load CondFunctor");
		char function_name[ixray::kXRParserFunctionNameBufferSize] = "xr_conditions.";
		static constexpr auto sizeOfXRConditionsString = sizeof("xr_conditions.");

		R_ASSERT2(sizeOfXRConditionsString + Name.size() <= ixray::kXRParserFunctionNameBufferSize, "overflow!");
		std::memcpy(function_name + (sizeOfXRConditionsString - 1), *Name, Name.size());

		luabind::functor<bool>& function_from_xr_conditions = FunctorCondList[Name];
		R_ASSERT3(ai().script_engine().functor(function_name, function_from_xr_conditions), "Not found function: ", function_name);

		return function_from_xr_conditions;
	}
	
	return FunctorCondList[Name];
}