//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("elpproB4.res");
USEPACKAGE("vclx40.bpi");
USEPACKAGE("VCL40.bpi");
USEPACKAGE("ElPackB4.bpi");
USEUNIT("Source\ElShellCtl.pas");
USEUNIT("Source\ElPrinter.pas");
USEUNIT("Source\ElTreePrinter.pas");
USEFORMNS("Source\ElPrinterPreview.pas", ElPrinterPreview, ElPrinterPreviewDlg);
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

