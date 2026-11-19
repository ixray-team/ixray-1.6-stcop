///////////////////////////////////////////////////////////////
// ElectricBall.h
// ElectricBall - артефакт электрический шар
///////////////////////////////////////////////////////////////

#pragma once
#include "Artefact.h"

XREFLECT(LuaExport, Bases="CArtefact", Constructible)
class CElectricBall final : public CArtefact 
{
private:
	typedef CArtefact inherited;
public:
	CElectricBall(void);
	virtual ~CElectricBall(void) override;

	virtual void Load				(const char* section) override;

protected:
	virtual void	UpdateCLChild	() override;

};