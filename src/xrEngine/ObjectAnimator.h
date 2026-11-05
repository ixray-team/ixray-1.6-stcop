#ifndef ObjectAnimatorH
#define ObjectAnimatorH
#pragma once

#include "motion.h"

// refs
class ENGINE_API CObjectAnimator
{
private:
	using MotionVec = xr_vector<COMotion*>;
	using MotionIt = MotionVec::iterator;

protected:
	bool				bLoop;

    shared_str			m_Name;
    
	Fmatrix				m_XFORM;
    SAnimParams			m_MParam;
	MotionVec			m_Motions;
    float				m_Speed;

    COMotion*			m_Current;
	void				LoadMotions		(const char* fname);
	void				SetActiveMotion	(COMotion* mot);
	COMotion*			FindMotionByName(const char* name);
public:
						CObjectAnimator	();
	virtual				~CObjectAnimator();

	void				Clear			();
	void				Load			(const char* name);
    IC const char*			Name			(){return *m_Name;}
    float&				Speed			(){return m_Speed;}

	COMotion*			Play			(bool bLoop, const char* name=0);
	void				Pause			(bool val){return m_MParam.Pause(val);}
	void				Stop			();
	IC bool				IsPlaying		(){return m_MParam.bPlay;}

    IC const Fmatrix&	XFORM			(){return m_XFORM;}
	const SAnimParams& anim_param() { return m_MParam; }
	bool				IsLooped() const { return bLoop; }
	float				GetLength		();
	// Update
	void				Update			(float dt);
    void				DrawPath		();
};

#endif //ObjectAnimatorH
