#pragma once

extern "C" {
	typedef __declspec(dllimport)  ISE_Abstract*	__stdcall SEFactory_Create	(LPCSTR section);
	typedef __declspec(dllimport)  void				__stdcall SEFactory_Destroy	(ISE_Abstract *&);
};

extern SEFactory_Create	*create_entity;
extern SEFactory_Destroy	*destroy_entity;

IC	CSE_Abstract *F_entity_Create(LPCSTR section)
{
	ISE_Abstract	*i = create_entity(section);
	CSE_Abstract	*j = smart_cast<CSE_Abstract*>(i);
	return			(j);
}

IC	void F_entity_Destroy(CSE_Abstract *&i)
{
	ISE_Abstract	*j = i;
	destroy_entity	(j);
	i				= 0;
}
