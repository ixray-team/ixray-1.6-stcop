#pragma once

class CHudInitializer {
public:
	CHudInitializer(bool, bool = false);
	~CHudInitializer();

	void SetHudMode();
	void SetDefaultMode();
private:
	bool b_auto_setup;
	bool b_ajust;

	Fmatrix mView_saved;
	Fmatrix mProject_saved;
	Fmatrix mFullTransform_saved;
};