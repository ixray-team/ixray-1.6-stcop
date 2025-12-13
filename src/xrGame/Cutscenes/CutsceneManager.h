#pragma once

struct SCutsceneObjectElement;
class CCutsceneItem;
class IKinematicsAnimated;

class CCutsceneManager {
	CCutsceneItem* m_pCurrentCutscene = nullptr;
	bool m_bToDelete = false;

#ifndef MASTER_GOLD
	bool Adjust = false;
	bool IsLocation = true;
	shared_str AdjustCutsceneSection = nullptr;
	Fvector AdjustDeviation;
	Fvector AdjustRotation;
	SCutsceneObjectElement* CurrentPosChangeElem;
#endif

	CCutsceneManager(){}
public:
	CCutsceneManager(const CCutsceneManager& other) = delete;
	CCutsceneManager(CCutsceneManager&& other) = delete;
	CCutsceneManager& operator=(const CCutsceneManager& other) = delete;
	CCutsceneManager& operator=(CCutsceneManager&& other) = delete;

	static CCutsceneManager& GetInstance();
	static void PlayCutscene(LPCSTR section);
	static void FinishCurrentCutscene();
	

	void Update();
	
#ifndef MASTER_GOLD
	inline void SetAdjust(bool Adjust) { this->Adjust = Adjust; }
	inline bool GetAdjust() { return Adjust; }
	inline void SetAdjustSection(shared_str str) { AdjustCutsceneSection = str; }
	void SaveAdjust();
	void ResetAdjust();
	Fvector GetAdjustDelta();
	void DrawData();
#endif
};