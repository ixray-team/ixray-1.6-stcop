//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dcelppB4.res");
USEUNIT("ElProReg.pas");
USEPACKAGE("vclx40.bpi");
USEPACKAGE("dclstd40.bpi");
USEPACKAGE("VCL40.bpi");
USEPACKAGE("elpproB4.bpi");
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

