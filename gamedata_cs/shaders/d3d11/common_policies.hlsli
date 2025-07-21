#ifndef common_policies_h_included
#define common_policies_h_included

#ifndef ISAMPLE
    #define ISAMPLE 0
#endif //	ISAMPLE

/////////////////////////////////////////////////////////////////////////////
#define GLD_P(_tc, _pos2d, _iSample) _tc, _pos2d
#define CS_P(_P, _N, _tc0, _tcJ, _pos2d, _iSample) _P, _N, _tc0, _tcJ, _pos2d

#endif //	common_policies_h_included
