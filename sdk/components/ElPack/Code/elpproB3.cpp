//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("elpproB3.res");
USEPACKAGE("vclx35.bpi");
USEPACKAGE("VCL35.bpi");
USEPACKAGE("ElPackB3.bpi");
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

