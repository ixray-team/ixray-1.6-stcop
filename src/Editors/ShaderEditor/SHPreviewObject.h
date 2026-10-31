//---------------------------------------------------------------------------
#ifndef SHPreviewObjectH
#define SHPreviewObjectH

#include "SHToolsInterface.h"

// refs
class CEditableMesh;

class CPreviewObject
{
	CEditableObject*				Object = nullptr;
	bool							bNotransform = false; // stub_notransform* VS: positions are screen-space

public:
	void							SetObject		(CEditableObject* o)
	{
		Object = o;
		OrigPos.clear();
		bWasClip = false;
	}
	CEditableObject*				GetObject		()						{ return Object; }

	bool							IsNotransform	() const { return bNotransform; }
	void							SetNotransform	(bool Value)			{ bNotransform = Value; }
	void							UpdateClipSpace	(const Fmatrix& WVP);
	CMatrix*						FindTexGenMatrix() const;

private:
	// Pristine (object-space) vertex positions of the preview meshes, cached so
	// the clip-space transform can be re-derived from the camera each frame and
	// reverted when notransform is off. Keyed by mesh; cleared on SetObject.
	xr_map<CEditableMesh*, xr_vector<Fvector>>	OrigPos;
	bool										bWasClip = false;
};

#endif
