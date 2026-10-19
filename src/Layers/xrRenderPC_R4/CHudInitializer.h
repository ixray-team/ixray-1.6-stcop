#pragma once

class CHudInitializer {
public:
	CHudInitializer(bool, bool = false);
	~CHudInitializer();

	void SetHudMode(bool IsCameraRelative = true);
	void SetDefaultMode();
private:
	bool b_auto_setup;
	bool b_ajust;
};
