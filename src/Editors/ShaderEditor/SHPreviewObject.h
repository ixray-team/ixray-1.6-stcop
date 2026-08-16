//---------------------------------------------------------------------------
#ifndef SHPreviewObjectH
#define SHPreviewObjectH

#include "SHToolsInterface.h"
#include <d3d11.h>
#include <d3d11shader.h>

// refs
class CEditableMesh;

// One required input of a vertex shader.
struct SVSInput
{
	xr_string		semantic;
	u32				index;
	ERHI_FORMAT	format;
	u32				byteSize;
};

// Internal preview-object helper.
//
// dx9 never required a vertex shader, so preview meshes were built with loose
// vertex declarations. After the move to dx11 every vertex shader strictly
// validates its input signature against the geometry declaration, and the old
// preview declarations no longer match (CreateInputLayout fails -> broken
// preview). CPreviewObject detects every input the VS actually consumes and
// composes a correct dx10_dcl_code, reusing existing vertex channels where
// possible and synthesizing the missing ones, so the declaration is patched
// on the fly without touching the vertex buffer layout for the common case.
class CPreviewObject
{
	CEditableObject*				Object;
	ID3DBlob*						VSSignature;	// not owned, caller lifetime
	xr_vector<RHIInputElementDesc>	SourceDecl;
	xr_vector<RHIInputElementDesc>	OriginalDecl;	// pristine geometry decl, captured once
	xr_vector<SVSInput>				Required;
	xr_vector<RHIInputElementDesc>	CompatibleDecl;
	xr_vector<xr_string>				SemanticStorage; // keeps SemanticName pointers alive
	bool							bNotransform = false; // VS reads POSITIONT (clip-space) inputs

public:
									CPreviewObject	();
									~CPreviewObject	();

	void							SetObject		(CEditableObject* o)
	{
		Object = o;
		OriginalDecl.clear();
		OrigPos.clear();
		bWasClip = false;
	}
	CEditableObject*				GetObject		()						{ return Object; }

	// Enumerate every input a compiled VS consumes from its input signature.
	static bool						EnumerateVSInputs	(ID3DBlob* signature, xr_vector<SVSInput>& out);

	void							SetVSSignature	(ID3DBlob* sig);
	void							SetSourceDeclaration(const xr_vector<RHIInputElementDesc>& src);

	// Compose a dx10_dcl_code that satisfies every VS input, reusing source
	// channels where they match and synthesizing the rest.
	const xr_vector<RHIInputElementDesc>&
									BuildCompatibleDeclaration(bool bLog = false);

	// Required VS inputs that have no matching source channel (need VB synthesis).
	void							GetMissingSemantics(xr_vector<SVSInput>& missing) const;

	// Patch an existing SDeclaration (e.g. preview geometry's dcl).
	void							ApplyToDeclaration(SDeclaration* dcl, bool bLog = false);

	// Walk the preview object's meshes and patch their geometry declarations.
	bool							Apply			(CEditableObject* o = nullptr);

	// Re-patch the geometry declarations of the current preview object. Called
	// after a render-buffer rebuild (e.g. a clip-space vertex rewrite) that
	// recreated the SDeclaration and reset dx10_dcl_code to the source decl.
	void							ReapplyDeclarations(bool bLog = false);

	// notransform (POSITIONT) shaders expect clip-space positions.
	bool							IsNotransform	() const { return bNotransform; }
	void							UpdateClipSpace	(const Fmatrix& WVP);

private:
	// Pristine (object-space) vertex positions of the preview meshes, cached so
	// the clip-space transform can be re-derived from the camera each frame and
	// reverted when notransform is off. Keyed by mesh; cleared on SetObject/Apply.
	xr_map<CEditableMesh*, xr_vector<Fvector>>	OrigPos;
	bool										bWasClip = false;

	// Resolve the render-layer geometry declaration for an editor mesh. Returns
	// nullptr when the visual has not been built yet (integration point).
	SDeclaration*					GetMeshDeclaration(CEditableMesh* M);
};

#endif
