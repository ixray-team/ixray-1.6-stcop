#pragma once
#include "stdafx.h"

class CHudInitializer {
public:
	CHudInitializer(bool);
	~CHudInitializer();

	void SetHudMode();
	void SetDefaultMode();
private:
	bool b_auto_setup;
	Fmatrix mView_saved;
	Fmatrix mProject_saved;
	Fmatrix mFullTransform_saved;
};