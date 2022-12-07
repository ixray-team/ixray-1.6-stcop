//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dclelpB3.res");
USEPACKAGE("vclx35.bpi");
USEPACKAGE("VCL35.bpi");
USEPACKAGE("dclstd35.bpi");
USEPACKAGE("elpackb3.bpi");
USEFORMNS("Design\MlCapProp.pas", Mlcapprop, MlCapEditDialog);
USEUNIT("Design\ElTBDsgn.pas");
USEUNIT("Design\ElEBDsgn.pas");
USEUNIT("Design\TreeDsgn.pas");
USEUNIT("Design\PgCtlProp.pas");
USEUNIT("Design\FormCtlProp.pas");
USEFORMNS("Design\frmFormPers.pas", frmFormPers, TPersPropsForm);
USEFORMNS("Design\frmItemCol.pas", Frmitemcol, ItemColDlg);
USEFORMNS("Design\frmItemsProp.pas", frmItemsProp, TItemsPropDlg);
USEFORMNS("Design\frmSectEdit.pas", Frmsectedit, SectEdit);
USEFORMNS("Design\frmSectProp.pas", Frmsectprop, ElSectionsPropDlg);
USEFORMNS("Design\frmSoundMap.pas", frmSoundMap, TSoundMapForm);
USEFORMNS("Design\frmStrPoolEdit.pas", frmStrPoolEdit, TStrPoolEditForm);
USEFORMNS("Design\ElMenuDsgn.pas", ElMenuDsgn, TElDesignMenu);
USEUNIT("Design\ColorMapProp.pas");
USEUNIT("ElReg.pas");
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

