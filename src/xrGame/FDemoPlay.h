#pragma once
#include "../xrEngine/Effector.h"

class COMotion;
struct SAnimParams;

class CDemoPlay : public CEffectorCam, public IInputReceiver
{
	COMotion* m_pMotion;
	SAnimParams* m_MParam;

	xr_vector<Fmatrix> seq;
	int m_count;
	float fStartTime;
	float fSpeed;
	u32 dwCyclesLeft;

	// statistics
	bool stat_started;
	CTimer stat_Timer_frame;
	CTimer stat_Timer_total;
	u32 stat_StartFrame;
	xr_vector<float> stat_table;
	
	bool redirect_input_to_level = true;

	void stat_Start();
	void stat_Stop();

public:
	virtual bool ProcessCam(SCamEffectorInfo& info);

	CDemoPlay(const char* name, float ms, u32 cycles, float life_time = 60 * 60 * 1000);
	virtual ~CDemoPlay();
	
	virtual void IR_OnKeyboardPress(int dik) override;
	virtual void IR_OnKeyboardHold(int dik) override;
	virtual void IR_OnKeyboardRelease(int dik) override;
	virtual void IR_OnMousePress(int btn) override;
	virtual void IR_OnMouseMove(int x, int y) override;
	virtual void IR_OnMouseRelease(int btn) override; 
};