//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("elpproB5.res");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("VCL50.bpi");
USEPACKAGE("ElPackB5.bpi");
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

