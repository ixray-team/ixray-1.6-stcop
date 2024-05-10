#include "stdafx.h"
#include "../../xrEngine/envelope.h"
#include "../../xrEngine/motion.h"

#define EOBJ_OMOTION   			0x1100
#define EOBJ_SMOTION   			0x1200
#define EOBJ_OMOTION_VERSION   	0x0005
#define EOBJ_SMOTION_VERSION   	0x0007

CSMotion::CSMotion() :CCustomMotion()
{
    mtype = mtSkeleton;
    m_BoneOrPart = BI_NONE;
    fSpeed = 1.0f;
    fAccrue = 2.0f;
    fFalloff = 2.0f;
    fPower = 1.f;
    m_Flags.zero();
}

CSMotion::CSMotion(CSMotion* source) :CCustomMotion(source) {
    // bone motions
    st_BoneMotion* src;
    st_BoneMotion* dest;
    for (u32 i = 0; i < bone_mots.size(); i++) {
        dest = &bone_mots[i];
        src = &source->bone_mots[i];
        for (int ch = 0; ch < ctMaxChannel; ch++)
            dest->envs[ch] = new CEnvelope(src->envs[ch]);
    }
}

CSMotion::~CSMotion() {
    Clear();
}

void CSMotion::Clear()
{
    for (BoneMotionIt bm_it = bone_mots.begin(); bm_it != bone_mots.end(); bm_it++)
    {
        for (int ch = 0; ch < ctMaxChannel; ch++) xr_delete(bm_it->envs[ch]);
    }
    bone_mots.clear();
}

st_BoneMotion* CSMotion::FindBoneMotion(shared_str name)
{
    for (BoneMotionIt bm_it = bone_mots.begin(); bm_it != bone_mots.end(); bm_it++)
        if (bm_it->name.equal(name)) return &*bm_it;
    return 0;
}

void CSMotion::add_empty_motion(shared_str const& bone_id)
{
    VERIFY(!FindBoneMotion(bone_id));

    st_BoneMotion			motion;

    motion.SetName(bone_id.c_str());
    // flRKeyAbsent = (1<<1),
    motion.m_Flags.assign(1 << 1);

    for (int ch = 0; ch < ctMaxChannel; ch++) {
        motion.envs[ch] = new CEnvelope();
        //		motion.envs[ch];
    }

    bone_mots.push_back(motion);
}

void CSMotion::CopyMotion(CSMotion* source) {
    Clear();

    iFrameStart = source->iFrameStart;
    iFrameEnd = source->iFrameEnd;
    fFPS = source->fFPS;
    st_BoneMotion* src;
    st_BoneMotion* dest;
    bone_mots.resize(source->bone_mots.size());
    for (u32 i = 0; i < bone_mots.size(); i++) {
        dest = &bone_mots[i];
        src = &source->bone_mots[i];
        for (int ch = 0; ch < ctMaxChannel; ch++)
            dest->envs[ch] = new CEnvelope(src->envs[ch]);
    }
}

void CSMotion::_Evaluate(int bone_idx, float t, Fvector& T, Fvector& R)
{
    VERIFY(bone_idx < (int)bone_mots.size());
    CEnvelope** envs = bone_mots[bone_idx].envs;
    T.x = envs[ctPositionX]->Evaluate(t);
    T.y = envs[ctPositionY]->Evaluate(t);
    T.z = envs[ctPositionZ]->Evaluate(t);

    R.y = envs[ctRotationH]->Evaluate(t);
    R.x = envs[ctRotationP]->Evaluate(t);
    R.z = envs[ctRotationB]->Evaluate(t);
}

void CSMotion::WorldRotate(int boneId, float h, float p, float b)
{
    R_ASSERT((boneId >= 0) && (boneId < (int)bone_mots.size()));
    st_BoneMotion& BM = bone_mots[boneId];

    BM.envs[ctRotationH]->RotateKeys(h);
    BM.envs[ctRotationP]->RotateKeys(p);
    BM.envs[ctRotationB]->RotateKeys(b);
}

void CSMotion::SaveMotion(const char* buf) {
    CMemoryWriter	F;
    F.open_chunk(EOBJ_SMOTION);
    Save(F);
    F.close_chunk();
    if (!F.save_to(buf))
        Msg("!Can't save skeleton motion: %s", buf);
}

bool CSMotion::LoadMotion(const char* buf)
{
    destructor<IReader>	F(FS.r_open(buf));
    R_ASSERT(F().find_chunk(EOBJ_SMOTION));
    return Load(F());
}

void CSMotion::Save(IWriter& F)
{
    CCustomMotion::Save(F);
    F.w_u16(EOBJ_SMOTION_VERSION);
    F.w_s8(m_Flags.get());
    F.w_u16(m_BoneOrPart);
    F.w_float(fSpeed);
    F.w_float(fAccrue);
    F.w_float(fFalloff);
    F.w_float(fPower);
    F.w_u16((u16)bone_mots.size());
    for (BoneMotionIt bm_it = bone_mots.begin(); bm_it != bone_mots.end(); bm_it++) {
        xr_strlwr(bm_it->name);
        F.w_stringZ(bm_it->name);
        F.w_u8(bm_it->m_Flags.get());
        for (int ch = 0; ch < ctMaxChannel; ch++)
            bm_it->envs[ch]->Save(F);
    }

    u32 sz = marks.size();
    F.w_u32(sz);
    for (u32 i = 0; i < sz; ++i)
        marks[i].Save(&F);

}

bool CSMotion::Load(IReader& F)
{
    CCustomMotion::Load(F);
    u16 vers = F.r_u16();
    if (vers == 0x0004) {
        m_BoneOrPart = u16(F.r_u32() & 0xffff);
        m_Flags.set(esmFX, F.r_u8());
        m_Flags.set(esmStopAtEnd, F.r_u8());
        fSpeed = F.r_float();
        fAccrue = F.r_float();
        fFalloff = F.r_float();
        fPower = F.r_float();
        bone_mots.resize(F.r_u32());
        string64	temp_buf;
        for (BoneMotionIt bm_it = bone_mots.begin(); bm_it != bone_mots.end(); bm_it++) {
            bm_it->SetName(itoa(int(bm_it - bone_mots.begin()), temp_buf, 10));
            bm_it->m_Flags.assign((u8)F.r_u32());
            for (int ch = 0; ch < ctMaxChannel; ch++) {
                bm_it->envs[ch] = new CEnvelope();
                bm_it->envs[ch]->Load_1(F);
            }
        }
    }
    else {
        if (vers == 0x0005) {
            m_Flags.assign((u8)F.r_u32());
            m_BoneOrPart = u16(F.r_u32() & 0xffff);
            fSpeed = F.r_float();
            fAccrue = F.r_float();
            fFalloff = F.r_float();
            fPower = F.r_float();
            bone_mots.resize(F.r_u32());
            string64 	buf;
            for (BoneMotionIt bm_it = bone_mots.begin(); bm_it != bone_mots.end(); bm_it++) {
                F.r_stringZ(buf, sizeof(buf));
                bm_it->SetName(buf);
                bm_it->m_Flags.assign((u8)F.r_u32());
                for (int ch = 0; ch < ctMaxChannel; ch++) {
                    bm_it->envs[ch] = new CEnvelope();
                    bm_it->envs[ch]->Load_1(F);
                }
            }
        }
        else {
            if (vers >= 0x0006)
            {
                m_Flags.assign(F.r_u8());
                m_BoneOrPart = F.r_u16();
                fSpeed = F.r_float();
                fAccrue = F.r_float();
                fFalloff = F.r_float();
                fPower = F.r_float();
                bone_mots.resize(F.r_u16());
                string64 	buf;
                for (BoneMotionIt bm_it = bone_mots.begin(); bm_it != bone_mots.end(); bm_it++) {
                    F.r_stringZ(buf, sizeof(buf));
                    bm_it->SetName(buf);
                    bm_it->m_Flags.assign(F.r_u8());
                    for (int ch = 0; ch < ctMaxChannel; ch++) {
                        bm_it->envs[ch] = new CEnvelope();
                        bm_it->envs[ch]->Load_2(F);
                    }
                }
            }
        }
    }
    if (vers >= 0x0007)
    {
        u32 sz = F.r_u32();
        if (sz > 0)
        {
            marks.resize(sz);
            for (u32 i = 0; i < sz; ++i)
                marks[i].Load(&F);
        }
    }
    for (BoneMotionIt bm_it = bone_mots.begin(); bm_it != bone_mots.end(); bm_it++)
        xr_strlwr(bm_it->name);
    return true;
}

void CSMotion::Optimize()
{
    for (BoneMotionIt bm_it = bone_mots.begin(); bm_it != bone_mots.end(); bm_it++) {
        for (int ch = 0; ch < ctMaxChannel; ch++)
            bm_it->envs[ch]->Optimize();
    }
}

void CSMotion::SortBonesBySkeleton(BoneVec& bones)
{
    BoneMotionVec new_bone_mots;
    for (BoneIt b_it = bones.begin(); b_it != bones.end(); ++b_it)
    {
        st_BoneMotion* BM = FindBoneMotion((*b_it)->Name());
        if (!BM)
        {
            CBone* B = *(b_it);
            bone_mots.push_back(st_BoneMotion());
            st_BoneMotion& bm0 = bone_mots[0];
            st_BoneMotion& bm = bone_mots.back();
            bm.SetName(B->Name().c_str());
            bm.m_Flags.assign(bm0.m_Flags);

            for (int ch = 0; ch < ctMaxChannel; ++ch)
            {
                bm.envs[ch] = new CEnvelope();
                //.                bm.envs[ch]->Load_2(F);
            }
            bm.envs[ctPositionX]->InsertKey(0.0f, B->_Offset().x);
            bm.envs[ctPositionY]->InsertKey(0.0f, B->_Offset().y);
            bm.envs[ctPositionZ]->InsertKey(0.0f, B->_Offset().z);
            bm.envs[ctRotationH]->InsertKey(0.0f, B->_Rotate().x);
            bm.envs[ctRotationP]->InsertKey(0.0f, B->_Rotate().y);
            bm.envs[ctRotationB]->InsertKey(0.0f, B->_Rotate().z);
            BM = &bm;
        };
        new_bone_mots.push_back(*BM);
    }
    bone_mots.clear();
    bone_mots = new_bone_mots;
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