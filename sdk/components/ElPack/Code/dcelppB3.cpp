//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dcelppB3.res");
USEPACKAGE("vclx35.bpi");
USEPACKAGE("VCL35.bpi");
USEPACKAGE("dclstd35.bpi");
USEPACKAGE("elpproB3.bpi");
USEUNIT("ElProReg.pas");
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

