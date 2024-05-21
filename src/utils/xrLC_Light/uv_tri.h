

#include "xrfacedefs.h"
#include "tcf.h"


struct XRLC_LIGHT_API UVtri : public _TCF		
{
	Face*	owner;
 
	bool	similar				( const UVtri &uv, float eps = EPS ) const;
};

