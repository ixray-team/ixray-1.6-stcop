#include "stdafx.h"
#pragma hdrstop

#include "motion.h"
#include "envelope.h"      

#define EOBJ_OMOTION   			0x1100
#define EOBJ_SMOTION   			0x1200
#define EOBJ_OMOTION_VERSION   	0x0005
#define EOBJ_SMOTION_VERSION   	0x0007

#ifdef _LW_EXPORT
	extern void ReplaceSpaceAndLowerCase(shared_str& s);
#endif


//------------------------------------------------------------------------------------------
// CCustomMotion
//------------------------------------------------------------------------------------------
CCustomMotion::CCustomMotion()
{
    iFrameStart		=0;
    iFrameEnd		=0;
    fFPS			=30.f;
}

CCustomMotion::CCustomMotion(CCustomMotion* source){
	*this			= *source;
}

CCustomMotion::~CCustomMotion()
{
}

void CCustomMotion::Save(IWriter& F)
{
#ifdef _LW_EXPORT
	ReplaceSpaceAndLowerCase(name);
#endif
	F.w_stringZ	(name);
	F.w_u32		(iFrameStart);
	F.w_u32		(iFrameEnd);
	F.w_float	(fFPS);
}

bool CCustomMotion::Load(IReader& F)
{
	F.r_stringZ	(name);
	iFrameStart	= F.r_u32();
	iFrameEnd	= F.r_u32();
	fFPS		= F.r_float();
	return true;
}

//------------------------------------------------------------------------------------------
// Object Motion
//------------------------------------------------------------------------------------------
COMotion::COMotion():CCustomMotion()
{
	mtype			=mtObject;
	for (int ch=0; ch<ctMaxChannel; ch++)
		envs[ch]	= new CEnvelope ();
}

COMotion::COMotion(COMotion* source):CCustomMotion(source)
{
	// bone motions
	mtype = source->mtype;
	for (int ch=0; ch<ctMaxChannel; ch++)
		envs[ch]	= new CEnvelope (source->envs[ch]);
}

COMotion::~COMotion()
{
	Clear			();
}

void COMotion::Clear()
{
	for (int ch=0; ch<ctMaxChannel; ch++) xr_delete(envs[ch]);
}

void COMotion::_Evaluate(float t, Fvector& T, Fvector& R)
{
	T.x = envs[ctPositionX]->Evaluate(t);
	T.y = envs[ctPositionY]->Evaluate(t);
	T.z = envs[ctPositionZ]->Evaluate(t);

	R.y = envs[ctRotationH]->Evaluate(t);
	R.x = envs[ctRotationP]->Evaluate(t);
	R.z = envs[ctRotationB]->Evaluate(t);
}

void COMotion::SaveMotion(const char* buf){
	CMemoryWriter	F;
	F.open_chunk	(EOBJ_OMOTION);
	Save			(F);
	F.close_chunk	();
	if (!F.save_to(buf)) 
        Msg("!Can't save object motion: %s",buf);
}

bool COMotion::LoadMotion(const char* buf)
{
	destructor<IReader>	F(FS.r_open(buf));
	R_ASSERT(F().find_chunk(EOBJ_OMOTION));
	return Load		(F());
}

void COMotion::Save(IWriter& F)
{
	CCustomMotion::Save(F);
	F.w_u16		(EOBJ_OMOTION_VERSION);
	for (int ch=0; ch<ctMaxChannel; ch++)
		envs[ch]->Save(F);
}

bool COMotion::Load(IReader& F)
{
	CCustomMotion::Load(F);
	u16 vers	= F.r_u16();
    if (vers==0x0003){
	    Clear	();
        for (int ch=0; ch<ctMaxChannel; ch++){
            envs[ch] = new CEnvelope ();
            envs[ch]->Load_1(F);
        }
    }else if (vers==0x0004){
	    Clear	();
        envs[ctPositionX] = new CEnvelope();	envs[ctPositionX]->Load_2(F);
        envs[ctPositionY] = new CEnvelope();	envs[ctPositionY]->Load_2(F);
        envs[ctPositionZ] = new CEnvelope();	envs[ctPositionZ]->Load_2(F);
        envs[ctRotationP] = new CEnvelope();	envs[ctRotationP]->Load_2(F);
        envs[ctRotationH] = new CEnvelope();	envs[ctRotationH]->Load_2(F);
        envs[ctRotationB] = new CEnvelope();	envs[ctRotationB]->Load_2(F);
    }else{
		if (vers!=EOBJ_OMOTION_VERSION) return false;
	    Clear	();

        for (int ch=0; ch<ctMaxChannel; ch++){
            envs[ch] = new CEnvelope ();
            envs[ch]->Load_2(F);
        }
    }
	return true;
}

void COMotion::CreateKey(float t, const Fvector& P, const Fvector& R)
{
	envs[ctPositionX]->InsertKey(t,P.x);
	envs[ctPositionY]->InsertKey(t,P.y);
	envs[ctPositionZ]->InsertKey(t,P.z);
	envs[ctRotationH]->InsertKey(t,R.y);
	envs[ctRotationP]->InsertKey(t,R.x);
	envs[ctRotationB]->InsertKey(t,R.z);
}
void COMotion::DeleteKey(float t)
{
	envs[ctPositionX]->DeleteKey(t);
	envs[ctPositionY]->DeleteKey(t);
	envs[ctPositionZ]->DeleteKey(t);
	envs[ctRotationH]->DeleteKey(t);
	envs[ctRotationP]->DeleteKey(t);
	envs[ctRotationB]->DeleteKey(t);
}
int COMotion::KeyCount()
{
	return envs[ctPositionX]->keys.size();
}
CEnvelope* COMotion::Envelope(EChannelType et)
{
    return envs[et];
}
void COMotion::FindNearestKey(float t, float& mn, float& mx, float eps)
{
	KeyIt min_k;
    KeyIt max_k;
	envs[ctPositionX]->FindNearestKey(t, min_k, max_k, eps);
    mn = (min_k!=envs[ctPositionX]->keys.end())?(*min_k)->time:t; 
    mx = (max_k!=envs[ctPositionX]->keys.end())?(*max_k)->time:t; 
}
float COMotion::GetLength(float* mn, float* mx)
{
	float ln,len=0.f; 
    for (int ch=0; ch<ctMaxChannel; ch++)
        if ((ln=envs[ch]->GetLength(mn,mx))>len) len=ln;
    return len;
}
BOOL COMotion::ScaleKeys(float from_time, float to_time, float scale_factor)
{
	BOOL bRes=TRUE;
    for (int ch=0; ch<ctMaxChannel; ch++)
        if (FALSE==(bRes=envs[ch]->ScaleKeys(from_time, to_time, scale_factor, 1.f/fFPS))) break;
    return bRes;
}
BOOL COMotion::NormalizeKeys(float from_time, float to_time, float speed)
{
	if (to_time<from_time) return FALSE;
	CEnvelope* E 	= Envelope(ctPositionX);
    float new_tm	= 0;
    float t0		= E->keys.front()->time;
    FloatVec tms;
    tms.push_back	(t0);
    for (KeyIt it=E->keys.begin()+1; it!=E->keys.end(); it++){
    	if ((*it)->time>from_time){
        	if ((*it)->time<to_time+EPS){
                float dist	= 0;
                Fvector PT,T,R;
                _Evaluate	(t0, PT, R);
                for (float tm=t0+1.f/fFPS; tm<=(*it)->time; tm+=EPS_L){
                    _Evaluate	(tm, T, R);
                    dist		+= PT.distance_to(T);
                    PT.set		(T);
                }
                new_tm			+= dist / speed;
                t0				= (*it)->time;
                tms.push_back	(new_tm);
	        }else{
                float dt		= (*it)->time-t0;
                t0				= (*it)->time;
                new_tm			+=dt;
                tms.push_back	(new_tm);
            }
        }
    }
    for (int ch=0; ch<ctMaxChannel; ch++){
    	E				= Envelope(EChannelType(ch));
        FloatIt	f_it	= tms.begin();   VERIFY(tms.size()==E->keys.size());
	    for (KeyIt k_it=E->keys.begin(); k_it!=E->keys.end(); k_it++,f_it++)
            (*k_it)->time = *f_it;
    }
    
/*
	CEnvelope* E = Envelope();
    for (KeyIt it=E->keys.begin(); it!=E->keys.end(); it++){
    	if (((*it)->time>from_time)&&((*it)->time<to_time)){
		    for (float tm=from_time; tm<=to_time; tm+=1.f/fFPS){
        	
        }
    }
*/
	return TRUE;
}


void SAnimParams::Set(float start_frame, float end_frame, float fps)
{
    min_t = start_frame / fps;
    max_t = end_frame / fps;
}

void SAnimParams::Set(CCustomMotion* M)
{
    Set((float)M->FrameStart(), (float)M->FrameEnd(), M->FPS());
    t_current = min_t;
    tmp = t_current;
    //    bPlay=true;
}
void SAnimParams::Update(float dt, float speed, bool loop)
{
    if (!bPlay) return;
    bWrapped = false;

    t_current += speed * dt;
    tmp = t_current;

    if (t_current > max_t)
    {
        bWrapped = true;
        if (loop)
        {
            float len = max_t - min_t;
            float k = float(iFloor((t_current - min_t) / len));
            t_current = t_current - k * len;
        }
        else
            t_current = max_t;

        tmp = t_current;
    }
}