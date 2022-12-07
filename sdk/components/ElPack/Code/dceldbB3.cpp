//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dceldbB3.res");
USEPACKAGE("vclx35.bpi");
USEPACKAGE("VCL35.bpi");
USEPACKAGE("VCLDB35.bpi");
USEPACKAGE("elpkdbB3.bpi");
USEUNIT("ElDBReg.pas");
USEUNIT("Design\ElDbNavDsgn.pas");
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

