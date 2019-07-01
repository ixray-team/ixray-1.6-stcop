#include "stdafx.h"
#pragma hdrstop

#ifdef _EDITOR
#	include "skeletonX.h"
#	include "skeletoncustom.h"
#else // _EDITOR
#include "../xrEngine/bone.h"
#include "../Layers/xrRender/SkeletonXVertRender.h"
//#	include "../xrEngine/skeletonX.h"
//#	include "../Layers/xrRender/skeletonX.h"
//#	include "../Layers/xrRender/skeletoncustom.h"
#endif // _EDITOR


//#pragma optimize("a",on)
// it means no aliasing inside the function, /Oa compiler option
void __stdcall xrSkin2W_x86(vertRender*		D,
							vertBoned2W*	S,
							u32				vCount,
							CBoneInstance*	Bones) 
{
	// Prepare
	int U_Count			= vCount;
	vertBoned2W*	V	= S;
	vertBoned2W*	E	= V+U_Count;
	Fvector			P0,N0,P1,N1;

	// NON-Unrolled loop
	for (; S!=E; ){
    	if (S->matrix1!=S->matrix0){
            Fmatrix& M0		= Bones[S->matrix0].mRenderTransform;
            Fmatrix& M1		= Bones[S->matrix1].mRenderTransform;
            M0.transform_tiny(P0,S->P);
            M0.transform_dir (N0,S->N);
            M1.transform_tiny(P1,S->P);
            M1.transform_dir (N1,S->N);
            D->P.lerp		(P0,P1,S->w);
            D->N.lerp		(N0,N1,S->w);
            D->u			= S->u;
            D->v			= S->v;
        }else{
            Fmatrix& M0		= Bones[S->matrix0].mRenderTransform;
            M0.transform_tiny(D->P,S->P);
            M0.transform_dir (D->N,S->N);
            D->u			= S->u;
            D->v			= S->v;
        }
		S++; D++;
	}
}



void __stdcall xrSkin3W_x86(vertRender*		D,
							vertBoned3W*	S,
							u32				vCount,
							CBoneInstance*	Bones) 
{
	// Prepare
	int U_Count			= vCount;
	vertBoned3W*	V	= S;
	vertBoned3W*	E	= V+U_Count;
	Fvector			P0,N0,P1,N1,P2,N2;

	// NON-Unrolled loop
	for (; S!=E; )
	{
		Fmatrix& M0		= Bones[ S->m[0] ].mRenderTransform;
        Fmatrix& M1		= Bones[ S->m[1] ].mRenderTransform;
        Fmatrix& M2		= Bones[ S->m[2] ].mRenderTransform;

		M0.transform_tiny(P0,S->P); P0.mul(S->w[0]);
        M0.transform_dir (N0,S->N); N0.mul(S->w[0]);

        M1.transform_tiny(P1,S->P); P1.mul(S->w[1]);
        M1.transform_dir (N1,S->N); N1.mul(S->w[1]);

        M2.transform_tiny(P2,S->P); P2.mul(1.0f-S->w[0]-S->w[1]);
        M2.transform_dir (N2,S->N); N2.mul(1.0f-S->w[0]-S->w[1]);

		D->P			= P0;
		D->P.add		(P1);
		D->P.add		(P2);

		D->N			= N0;
		D->N.add		(N1);
		D->N.add		(N2);
		
		D->u			= S->u;
        D->v			= S->v;

		S++; 
		D++;
	}
}



void __stdcall xrSkin4W_x86(vertRender*		D,
							vertBoned4W*	S,
							u32				vCount,
							CBoneInstance*	Bones) 
{
	// Prepare
	int U_Count			= vCount;
	vertBoned4W*	V	= S;
	vertBoned4W*	E	= V+U_Count;
	Fvector			P0,N0,P1,N1,P2,N2,P3,N3;

	// NON-Unrolled loop
	for (; S!=E; )
	{
		Fmatrix& M0		= Bones[ S->m[0] ].mRenderTransform;
        Fmatrix& M1		= Bones[ S->m[1] ].mRenderTransform;
        Fmatrix& M2		= Bones[ S->m[2] ].mRenderTransform;
        Fmatrix& M3		= Bones[ S->m[3] ].mRenderTransform;

		M0.transform_tiny(P0,S->P); P0.mul(S->w[0]);
        M0.transform_dir (N0,S->N); N0.mul(S->w[0]);

        M1.transform_tiny(P1,S->P); P1.mul(S->w[1]);
        M1.transform_dir (N1,S->N); N1.mul(S->w[1]);

        M2.transform_tiny(P2,S->P); P2.mul(S->w[2]);
        M2.transform_dir (N2,S->N); N2.mul(S->w[2]);

		M3.transform_tiny(P3,S->P); P3.mul(1.0f-S->w[0]-S->w[1]-S->w[2]);
        M3.transform_dir (N3,S->N); N3.mul(1.0f-S->w[0]-S->w[1]-S->w[2]);

		D->P			= P0;
		D->P.add		(P1);
		D->P.add		(P2);
		D->P.add		(P3);

		D->N			= N0;
		D->N.add		(N1);
		D->N.add		(N2);
		D->N.add		(N3);
		
		D->u			= S->u;
        D->v			= S->v;

		S++; 
		D++;
	}
}
