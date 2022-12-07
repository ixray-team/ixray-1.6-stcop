//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("elpkdbB3.res");
USEPACKAGE("vclx35.bpi");
USEPACKAGE("VCL35.bpi");
USEPACKAGE("VCLDB35.bpi");
USEPACKAGE("elpackB3.bpi");
//USEUNIT("Source\ElDBTree.pas");
USEUNIT("Source\ElDBBoxes.pas");
USEUNIT("Source\ElDBBtnEdit.pas");
USEUNIT("Source\ElDBCtrls.pas");
USEUNIT("Source\ElDBCurrEdit.pas");
USEUNIT("Source\ElDBDTPick.pas");
USEUNIT("Source\ElDBHTMLView.pas");
USEUNIT("Source\ElDBLbl.pas");
USEUNIT("Source\ElDBSpin.pas");

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

