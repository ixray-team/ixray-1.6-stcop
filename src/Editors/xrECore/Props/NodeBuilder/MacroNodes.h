#pragma once
#include "Nodes.h"

enum class MacroType
{
	eVoid,
	eOneArg,
	eTwoArg
};

class ECORE_API CNodeMacro :
	public INodeUnknown
{
	MacroType Type = MacroType::eVoid;

public:
	CNodeMacro(size_t Marco, const xr_string Name);
	void Draw() override;

	void SetType(MacroType NewType);
	MacroType GetType() const;

public:
	size_t MacroCommandID = -1;
	xr_string FirstValue;
	xr_string SecondValue;
};