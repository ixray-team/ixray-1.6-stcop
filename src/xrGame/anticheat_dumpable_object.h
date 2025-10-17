#pragma once

class IAnticheatDumpable
{
public:
	virtual ~IAnticheatDumpable() = default;

	virtual void				DumpActiveParams		(shared_str const & section_name, CInifile & dst_ini) const = 0;
	virtual shared_str const 	GetAnticheatSectionName	() const { return ""; }
};