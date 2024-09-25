// Originally by B.O.R.S.C.H.T. team
// see https://bitbucket.org/stalker/xray-csky_borscht_sdk

#pragma once
class IM_Manipulator
{
public:
    bool m_active;

    IM_Manipulator(): m_active(false) {}

    void Render(float canvasX, float canvasY, float canvasWidth, float canvasHeight);
};
extern IM_Manipulator imManipulator;
