#ifndef	common_policies_h_included
#define	common_policies_h_included

#ifndef	ISAMPLE
#define ISAMPLE 0
#endif	//	ISAMPLE

/////////////////////////////////////////////////////////////////////////////
// GLD_P - gbuffer_load_data
#define	GLD_P( _tc, _pos2d, _iSample ) _tc, _iSample
#define	CS_P( _P, _N, _tc0, _tcJ, _pos2d, _iSample ) _P, _N, _tc0, _tcJ

#endif	//	common_policies_h_included