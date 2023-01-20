//---------------------------------------------------------------------------
#include "stdafx.h"
#pragma hdrstop

#include "ColorPicker.h"

class CTCD{
public:
	TColorDialog* cdColor;
public:
	CTCD(){
		cdColor = xr_new<TColorDialog>((TComponent*)0);
        cdColor->Options = TColorDialogOptions()<<cdFullOpen;
    }
    ~CTCD(){
    	xr_delete(cdColor);
    }
};
static CTCD TCD;

bool SelectColor(u32* currentcolor, bool bDefaultPicker){
	VERIFY(currentcolor);
	bDefaultPicker = true;
	if (bDefaultPicker){
        TCD.cdColor->Color = TColor(rgb2bgr(*currentcolor));
        if (TCD.cdColor->Execute()){
			*currentcolor = bgr2rgb(TCD.cdColor->Color);
        	return true;
        }
        return false;
    }
}

bool SelectColorWin(u32* currentcolor, bool bDefaultPicker){
	VERIFY(currentcolor);
	bDefaultPicker = true;
	if (bDefaultPicker){
        TCD.cdColor->Color = TColor(*currentcolor);
        if (TCD.cdColor->Execute()){
			*currentcolor = TCD.cdColor->Color;
        	return true;
        }
        return false;
    }
}

