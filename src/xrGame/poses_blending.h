#pragma once

class poses_interpolation
{
	Fvector		p0, p1;
	Fquaternion q0, q1;
public:
			poses_interpolation ( const Fmatrix &m0, const Fmatrix &m1 );
	void ScaleTranslations(const Fvector& Pivot, float Ratio)
	{
		p0.sub(Pivot).mul(Ratio).add(Pivot);
		p1.sub(Pivot).mul(Ratio).add(Pivot);
	}
	void	pose				( Fmatrix &p, float factor ) const ;
};


class poses_blending
{
	poses_interpolation	interpolation;
	float				target_time;
public:
			poses_blending	( const Fmatrix &m0, const Fmatrix &m1, float target_time_ );

	void ScaleTranslations(const Fvector& Pivot, float Ratio) { interpolation.ScaleTranslations(Pivot, Ratio); }
	void	pose			( Fmatrix &p, float time )									const ;
IC	bool	target_reached	( float time )												const 
			{ 
				return time >= target_time;
			}

};