#ifndef	_CONVERT_H_
#define	_CONVERT_H_
IC MenderVec3& cv_vector ( MenderVec3	&l, const Fvector& r  )
{
	l.x = r.x;
	l.y = r.y;
	l.z = r.z;
	return l;
}

IC Fvector&  cv_vector (  Fvector& l, const MenderVec3	&r  )
{
	l.x = r.x;
	l.y = r.y;
	l.z = r.z;
	return l;
}




#endif