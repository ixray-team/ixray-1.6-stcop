//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("elmltgB5.res");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("VCL50.bpi");
USEPACKAGE("elmlgnB5.bpi");
USEPACKAGE("elpackB5.bpi");
USEUNIT("Source\ElTreeMLGen.pas");
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

