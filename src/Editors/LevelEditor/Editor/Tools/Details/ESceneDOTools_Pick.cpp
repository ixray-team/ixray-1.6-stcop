#include "stdafx.h"

int EDetailManager::RaySelect(int flag, float& dist, const Fvector& start, const Fvector& direction, bool bDistanceOnly)
{
	// box selected only
	if (!m_Flags.is(flSlotBoxesDraw))
	{
		return 0;
	}

	float Fx, Fz;
	Fbox Bbox;
	Fvector P;
	int Sx = -1, Sz = -1;

	int Count = 0;

	for (u32 ZCoord = 0; ZCoord < dtH.size_z; ZCoord++)
	{
		Fz = fromSlotZ(ZCoord);
		for (u32 x = 0; x < dtH.size_x; x++)
		{
			DetailSlot* slot = dtSlots + ZCoord * dtH.size_x + x;
			Fx = fromSlotX(x);
			Bbox.min.set(Fx - DETAIL_SLOT_SIZE_2, slot->r_ybase(), Fz - DETAIL_SLOT_SIZE_2);
			Bbox.max.set(Fx + DETAIL_SLOT_SIZE_2, slot->r_ybase() + slot->r_yheight(), Fz + DETAIL_SLOT_SIZE_2);
			if (Bbox.Pick2(start, direction, P))
			{
				float d = start.distance_to(P);
				if (d < dist)
				{
					dist = d;
					Sx = x;
					Sz = ZCoord;
				}
			}
		}
	}
	if ((Sx >= 0) || (Sz >= 0))
	{
		if (!bDistanceOnly)
		{
			if (flag == -1)
			{
				m_Selected[Sz * dtH.size_x + Sx] = !m_Selected[Sz * dtH.size_x + Sx];
			}
			else
			{
				m_Selected[Sz * dtH.size_x + Sx] = (u8)flag;
			}
			Count++;
			EContext.UI->RedrawScene();
		}
	}
	return Count;
}

int EDetailManager::FrustumSelect(int flag, const CFrustum& frustum)
{
// box selected only

	if (!m_Flags.is(flSlotBoxesDraw)) return 0;

    int count=0;

    float 			fx,fz;
    Fbox			bbox;
    for (u32 z=0; z<dtH.size_z; z++){
        fz			= fromSlotZ(z);
        for (u32 x=0; x<dtH.size_x; x++){
            DetailSlot* slot = dtSlots+z*dtH.size_x+x;
            fx			= fromSlotX(x);

            bbox.min.set(fx-DETAIL_SLOT_SIZE_2, slot->r_ybase(), 					fz-DETAIL_SLOT_SIZE_2);
            bbox.max.set(fx+DETAIL_SLOT_SIZE_2, slot->r_ybase()+slot->r_yheight(), 	fz+DETAIL_SLOT_SIZE_2);
			u32 mask	= 0xff;
            bool bRes 	= !!frustum.testAABB(bbox.data(),mask);
            if (bRes){
            	if (flag==-1)	
                	m_Selected[z*dtH.size_x+x] = !m_Selected[z*dtH.size_x+x];
                else
                	m_Selected[z*dtH.size_x+x] = (u8)flag;
                
            	count++;
            }
        }
    }
	EContext.UI->RedrawScene();
    return count;
}

void EDetailManager::SelectObjects(bool flag)
{
    if (!IsLoaded)
        return;

	for (U8It it=m_Selected.begin(); it!=m_Selected.end(); it++)
    	*it = flag;
}

void EDetailManager::InvertSelection()
{
	if (!m_Flags.is(flSlotBoxesDraw)) return;

	for (U8It it=m_Selected.begin(); it!=m_Selected.end(); it++)
    	*it = !*it;
}

int EDetailManager::SelectionCount(bool testflag)
{
	if (!m_Flags.is(flSlotBoxesDraw)) return 0;
	int count = 0;

	for (U8It it=m_Selected.begin(); it!=m_Selected.end(); it++)
    	if ((bool)*it==testflag) count++;
    return count;
}

