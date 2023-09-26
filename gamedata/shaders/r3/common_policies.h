#ifndef	common_policies_h_included
#define	common_policies_h_included

#ifndef	ISAMPLE
#define ISAMPLE 0
#endif	//	ISAMPLE

/////////////////////////////////////////////////////////////////////////////
// GLD_P - gbuffer_load_data
#ifdef	GBUFFER_OPTIMIZATION
	#define	GLD_P( _tc, _pos2d, _iSample ) _tc, _pos2d, _iSample
#else	//	GBUFFER_OPTIMIZATION
	#define	GLD_P( _tc, _pos2d, _iSample ) _tc, _iSample
#endif	//	GBUFFER_OPTIMIZATION

/////////////////////////////////////////////////////////////////////////////
// CS_P

#	ifdef	GBUFFER_OPTIMIZATION
#		define	CS_P( _P, _N, _tc0, _tcJ, _pos2d, _iSample ) _P, _N, _tc0, _tcJ, _pos2d
#	else	//	GBUFFER_OPTIMIZATION
#		define	CS_P( _P, _N, _tc0, _tcJ, _pos2d, _iSample ) _P, _N, _tc0, _tcJ
#endif

#endif	//	common_policies_h_included