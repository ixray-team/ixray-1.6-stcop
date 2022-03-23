#ifndef ShapeDataH
#define ShapeDataH

struct CShapeData
{
	enum{
    	cfSphere=0,
        cfBox
    };
	union shape_data
	{
		Fsphere		sphere;
		Fmatrix		box;
	};
	struct shape_def
	{
		u8			type;
		shape_data	data;
	};

	using ShapeVec = xr_vector<shape_def>;
	using ShapeIt = ShapeVec::iterator;

	ShapeVec						shapes;
};

#endif