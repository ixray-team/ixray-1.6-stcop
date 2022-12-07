//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dclelpB4.res");
USEFORMNS("Design\MlCapProp.pas", Mlcapprop, MlCapEditDialog);
USEUNIT("Design\ElTBDsgn.pas");
USEUNIT("Design\ElEBDsgn.pas");
USEUNIT("Design\TreeDsgn.pas");
USEUNIT("Design\FormCtlProp.pas");
USEUNIT("Design\PgCtlProp.pas");
USEFORMNS("Design\frmFormPers.pas", Frmformpers, TPersPropsForm);
USEFORMNS("Design\frmItemCol.pas", Frmitemcol, ItemColDlg);
USEFORMNS("Design\frmItemsProp.pas", Frmitemsprop, TItemsPropDlg);
USEFORMNS("Design\frmSectEdit.pas", Frmsectedit, SectEdit);
USEFORMNS("Design\frmSectProp.pas", Frmsectprop, ElSectionsPropDlg);
USEFORMNS("Design\frmSoundMap.pas", Frmsoundmap, TSoundMapForm);
USEFORMNS("Design\frmStrPoolEdit.pas", frmStrPoolEdit, TStrPoolEditForm);
USEFORMNS("Design\ElMenuDsgn.pas", ElMenuDsgn, TElDesignMenu);
USEUNIT("Design\ColorMapProp.pas");
USEUNIT("ElReg.pas");
USEPACKAGE("vclx40.bpi");
USEPACKAGE("dclstd40.bpi");
USEPACKAGE("VCL40.bpi");
USEPACKAGE("elpackB4.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------

