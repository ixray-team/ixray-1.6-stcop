#include "stdafx.h"
#include "ClipMaker.h"

TClipMaker* g_clip_maker = nullptr;
#define	CHUNK_ZOOM	0x9000
#define	CHUNK_CLIPS	0x9001

int CAnimationClip::PWidthUI()
{
	return static_cast<int>(Length() * owner->m_Zoom);
}

int CAnimationClip::PLeftUI()
{
	return static_cast<int>(StartTime() * owner->m_Zoom);
}

void TClipMaker::LoadClips()
{
    bool bRes = true;
    //if (EFS.GetOpenName("$clips$", m_ClipFName))
    //{
    //    Clear();
    //    IReader* F = FS.r_open(m_ClipFName.c_str()); VERIFY(F);
    //    m_ClipFName = EFS.ExcludeBasePath(m_ClipFName.c_str(), FS.get_path("$clips$")->m_Path);
    //    if (F->find_chunk(CHUNK_ZOOM))
    //    {
    //        m_Zoom = F->r_float();
    //    }
    //    IReader* C = F->open_chunk(CHUNK_CLIPS);
    //    if (C) {
    //        IReader* M = C->open_chunk(0);
    //        for (int count = 1; M; ++count)
    //        {
    //            CAnimationClip* clip = new CAnimationClip(this);
    //            if (!clip->Load(*M))
    //            {
    //                ELog.Msg(mtError, "Unsupported clip version. Load failed.");
    //                xr_delete(clip);
    //                bRes = false;
    //            }
    //            M->close();
    //            if (!bRes)	break;
    //            clips.push_back(clip);
    //            M = C->open_chunk(count);
    //        }
    //        C->close();
    //        UpdateClips();
    //    }
    //    FS.r_close(F);
    //}
}
void TClipMaker::SaveClips()
{
    //if (!clips.empty()) {
    //    if (EFS.GetSaveName("$clips$", m_ClipFName))
    //    {
    //        IWriter* F = FS.w_open(m_ClipFName.c_str()); VERIFY(F);
    //        m_ClipFName = EFS.ExcludeBasePath(m_ClipFName.c_str(), FS.get_path("$clips$")->m_Path);
    //        if (F)
    //        {
    //            F->open_chunk(CHUNK_ZOOM);
    //            F->w_float(m_Zoom);
    //            F->close_chunk();
    //
    //            F->open_chunk(CHUNK_CLIPS);
    //            int count = 0;
    //            for (AnimClipIt c_it = clips.begin(); c_it != clips.end(); ++c_it)
    //            {
    //                F->open_chunk(count); count++;
    //                (*c_it)->Save(*F);
    //                F->close_chunk();
    //            }
    //            F->close_chunk();
    //            FS.w_close(F);
    //        }
    //        else
    //        {
    //            Log("!Can't save clip:", m_ClipFName.c_str());
    //        }
    //    }
    //}
    //else {
    //    ELog.DlgMsg(mtError, "Clip list empty.");
    //}
}


void TClipMaker::ShowEditor(CKinematicsAnimated* O)
{
    VERIFY(O);
    bOpen = true;

    if (RenderObject != O)
    {
        RenderObject = O;
        ListItemsVec items;

        u16 cnt = RenderObject->LL_MotionsSlotCount();
        for (u16 k = 0; k < cnt; k++)
        {
            accel_map* ll_motions = RenderObject->LL_Motions(k);
            accel_map::iterator 	_I, _E;
            _I = ll_motions->begin();
            _E = ll_motions->end();
            for (; _I != _E; ++_I)
            {
                ListItem* I = LHelper().CreateItem(items, _I->first.c_str(), 0, 0, 0);

                MotionID 			mid;
                mid.set(k, _I->second);
                I->tag = mid.val;
            }
        }

        m_ObjectItems.AssignItems(items);
    }

    UpdateClips();
    UpdateProperties();
}

xr_string FloatTimeToStrTime(float v, bool _h, bool _m, bool _s, bool _ms)
{
    xr_string buf;
    int h = 0, m = 0, s = 0, ms = 0;
    char temp[8]; // Временный буфер для форматирования

    if (_h) {
        h = iFloor(v / 3600);
        xr_sprintf(temp, sizeof(temp), "%02d", h);
        buf += temp;
    }

    if (_m) {
        m = iFloor((v - h * 3600) / 60);
        xr_sprintf(temp, sizeof(temp), "%02d", m);
        if (!buf.empty()) buf += ":";
        buf += temp;
    }

    if (_s) {
        s = iFloor(v - h * 3600 - m * 60);
        xr_sprintf(temp, sizeof(temp), "%02d", s);
        if (!buf.empty()) buf += ":";
        buf += temp;
    }

    if (_ms) {
        ms = iFloor((v - h * 3600 - m * 60 - s) * 1000.f);
        xr_sprintf(temp, sizeof(temp), "%03d", ms);
        if (!buf.empty()) buf += ".";
        buf += temp;
    }

    return buf;
}

IC bool clip_pred(CAnimationClip* x, CAnimationClip* y)
{
    return x->start_time < y->start_time;
};
void TClipMaker::UpdateClips(bool, bool Repaint)
{
    m_TotalLength = 0.f;
    std::sort(clips.begin(), clips.end(), clip_pred);
    auto it = clips.begin();
    for (; it != clips.end(); ++it)
    {
        (*it)->start_time = m_TotalLength;
        m_TotalLength += (*it)->length;
        (*it)->idx = it - clips.begin();
    }
    //paFrame->Width = m_TotalLength * m_Zoom;
    Stop();
    // clip list
    ListItemsVec			l_items;

    for (it = clips.begin(); it != clips.end(); ++it)
        LHelper().CreateItem(l_items, *(*it)->name, 0, 0, *it);

    m_ClipList.AssignItems(l_items);

    // select default clip
    if (!clips.empty() && (sel_clip == 0))
        SelectClip(clips[0]);

    if (Repaint)
        RepaintClips();
}

void TClipMaker::RepaintClips(bool)
{
    // repaint
    //paClips->Repaint();
    //gtClip->Repaint();
    //paBP0->Repaint();
    //paBP1->Repaint();
    //paBP2->Repaint();
    //paBP3->Repaint();
    //paFXs->Repaint();

    // set BP name                   
    CPartition* P = RenderObject->m_Partition;
    for (u16 k = 0; k < MAX_PARTS; ++k)
        bp_names[k] = (P->part(k).Name.size()) ? P->part(k).Name.c_str() : "-";

    UpdateProperties();
}

void TClipMaker::UpdateProperties(bool)
{
    // clip props
    PropItemVec		p_items;
    PropValue* V = 0;
    PHelper().CreateCaption(p_items, "Length", FloatTimeToStrTime(m_TotalLength, true, true, true, true).c_str());
    V = PHelper().CreateFloat(p_items, "Zoom", &m_Zoom, 1.f, 1000.f, 0.1f, 1);
   // V->OnChangeEvent.bind(this, &TClipMaker::OnZoomChange);
    if (sel_clip) {
        ListItem* l_owner = nullptr;// m_ClipList->FindItem(*sel_clip->name); VERIFY(l_owner);
        V = PHelper().CreateName(p_items, "Current Clip\\Name", &sel_clip->name, l_owner);
        //V->OnChangeEvent.bind(this, &TClipMaker::OnNameChange);
        V = PHelper().CreateFloat(p_items, "Current Clip\\Length", &sel_clip->length, 0.f, 10000.f, 0.1f, 2);
        //V->OnChangeEvent.bind(this, &TClipMaker::OnClipLengthChange);
        //TEMP        
        /*
                for (u16 k=0; k<4; k++)
                {
                    AnsiString mname	= sel_clip->CycleName(k);
                    u16 slot			= sel_clip->CycleSlot(k);
                    if (mname.IsEmpty())
                        continue;

                    CMotionDef* MD		= m_RenderObject->FindMotionDef		(mname.c_str(),slot);
                    CMotion* MI			= m_RenderObject->FindMotionKeys	(mname.c_str(),slot);
                    SBonePart* BP		= (k<(u16)m_CurrentObject->BoneParts().size())?&m_CurrentObject->BoneParts()[k]:0;

                    shared_str tmp;
                    if (MI)
                        tmp.sprintf("%s [%3.2fs, %s]",mname.c_str(),MI->GetLength()/MD->Speed(),MD->bone_or_part?"stop at end":"looped");

                    if (BP)
                        PHelper().CreateCaption	(p_items,PrepareKey("Current Clip\\Cycles",BP->alias.c_str()), tmp);
                }

                if (sel_clip->fx.valid())
                    PHelper().CreateFloat		(p_items,PrepareKey("Current Clip\\FXs",*sel_clip->fx.name), &sel_clip->fx_power, 0.f, 1000.f);
        */
    }
    m_ClipProps.AssignItems(p_items);
}