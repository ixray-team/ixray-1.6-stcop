//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dcelppB5.res");
USEUNIT("ElProReg.pas");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("VCL50.bpi");
USEPACKAGE("elpproB5.bpi");
USEPACKAGE("dclstd50.bpi");
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

