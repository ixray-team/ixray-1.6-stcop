#pragma once

class CHudInitializer {
public:
	CHudInitializer(GfxCmdList& Cmd, bool, bool = false);
	~CHudInitializer();

	void SetHudMode(bool IsCameraRelative = true);
	void SetDefaultMode();
private:
	GfxCmdList& Cmd;
	bool b_auto_setup;
	bool b_ajust;
};
