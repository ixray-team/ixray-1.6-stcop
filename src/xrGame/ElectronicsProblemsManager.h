#pragma once

class CElectronicsProblemsManager final
{
	float m_previous_electronics_problems_counter = 0.0f;
	float m_current_electronics_problems_counter = 0.0f;
	float m_target_electronics_problems_counter = 0.0f;

	bool m_last_problems_update_was_decrease = false;

	u32 m_last_update_time = 0;

public:
	CElectronicsProblemsManager();
	~CElectronicsProblemsManager() = default;

	void ResetElectronicsProblems();
	void ResetElectronicsProblems_Full();
	float PreviousElectronicsProblemsCnt() const;
	bool ElectronicsProblemsImmediateApply();
	bool ElectronicsProblemsInc();
	float TargetElectronicsProblemsCnt() const;
	float CurrentElectronicsProblemsCnt() const;
	bool ElectronicsProblemsDec();
	bool IsElectronicsProblemsDecreasing() const;
	void UpdateElectronicsProblemsCnt(u32 dt);
	void UpdateCL();
};