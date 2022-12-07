//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dceldbB5.res");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("VCL50.bpi");
USEPACKAGE("VCLDB50.bpi");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("elpackB5.bpi");
USEUNIT("ElDBReg.pas");
USEUNIT("Design\ElDbNavDsgn.pas");
USEPACKAGE("elpkdbB5.bpi");
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

