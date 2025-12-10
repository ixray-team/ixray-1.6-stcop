#pragma once

struct SCartridgeParam
{
    float	kDist, kDisp, kHit/*, kCritical*/, kImpulse, kAP, kAirRes;
    int		buckShot;
    float	impair;
    float	fWallmarkSize;
    u8		u8ColorID;

    IC void Init()
    {
        kDist = kDisp = kHit = kImpulse = 1.0f;
        //		kCritical = 0.0f;
        kAP       = 0.0f;
        kAirRes   = 0.0f;
        buckShot  = 1;
        impair    = 1.0f;
        fWallmarkSize = 0.0f;
        u8ColorID     = 0;
    }
};