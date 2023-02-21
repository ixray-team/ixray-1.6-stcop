// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'dceldbB6.dpk' rev: 35.00 (Windows)

#ifndef Dceldbb6HPP
#define Dceldbb6HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// (rtl)
#include <SysInit.hpp>
#include <ElDBNavDsgn.hpp>
#include <ElDBReg.hpp>
#include <System.UITypes.hpp>	// (rtl)
#include <Winapi.Windows.hpp>	// (rtl)
#include <Winapi.PsAPI.hpp>	// (rtl)
#include <System.Character.hpp>	// (rtl)
#include <System.Internal.ExcUtils.hpp>	// (rtl)
#include <System.SysUtils.hpp>	// (rtl)
#include <System.VarUtils.hpp>	// (rtl)
#include <System.Variants.hpp>	// (rtl)
#include <System.TypInfo.hpp>	// (rtl)
#include <System.Math.hpp>	// (rtl)
#include <System.Generics.Defaults.hpp>	// (rtl)
#include <System.Rtti.hpp>	// (rtl)
#include <System.TimeSpan.hpp>	// (rtl)
#include <System.Classes.hpp>	// (rtl)
#include <System.DateUtils.hpp>	// (rtl)
#include <Data.SqlTimSt.hpp>	// (dbrtl)
#include <Data.FmtBcd.hpp>	// (dbrtl)
#include <Data.DB.hpp>	// (dbrtl)
#include <Winapi.ShellAPI.hpp>	// (rtl)
#include <System.IOUtils.hpp>	// (rtl)
#include <System.IniFiles.hpp>	// (rtl)
#include <System.Win.Registry.hpp>	// (rtl)
#include <System.UIConsts.hpp>	// (rtl)
#include <Vcl.Graphics.hpp>	// (vcl)
#include <System.Messaging.hpp>	// (rtl)
#include <System.Actions.hpp>	// (rtl)
#include <Vcl.ActnList.hpp>	// (vcl)
#include <System.HelpIntfs.hpp>	// (rtl)
#include <System.SyncObjs.hpp>	// (rtl)
#include <Winapi.UxTheme.hpp>	// (rtl)
#include <Vcl.GraphUtil.hpp>	// (vcl)
#include <Vcl.StdCtrls.hpp>	// (vcl)
#include <Vcl.Clipbrd.hpp>	// (vcl)
#include <Vcl.Printers.hpp>	// (vcl)
#include <Vcl.ComCtrls.hpp>	// (vcl)
#include <Vcl.Dialogs.hpp>	// (vcl)
#include <Vcl.ExtCtrls.hpp>	// (vcl)
#include <Vcl.Themes.hpp>	// (vcl)
#include <System.AnsiStrings.hpp>	// (rtl)
#include <System.Win.ComObj.hpp>	// (rtl)
#include <Winapi.FlatSB.hpp>	// (rtl)
#include <Vcl.Forms.hpp>	// (vcl)
#include <Vcl.Menus.hpp>	// (vcl)
#include <Winapi.MsCTF.hpp>	// (rtl)
#include <Vcl.Controls.hpp>	// (vcl)
#include <Vcl.Buttons.hpp>	// (vcl)
#include <Vcl.ImageCollection.hpp>	// (vclwinx)
#include <Vcl.DBLogDlg.hpp>	// (vcldb)
#include <Vcl.DBPWDlg.hpp>	// (vcldb)
#include <Vcl.DBCtrls.hpp>	// (vcldb)
#include <ElUxTheme.hpp>	// (elpackB6)
#include <ElTools.hpp>	// (elpackB6)
#include <ElVCLUtils.hpp>	// (elpackB6)
#include <ElExtBkgnd.hpp>	// (elpackB6)
#include <ElHook.hpp>	// (elpackB6)
#include <ElRegUtils.hpp>	// (elpackB6)
#include <ElObjList.hpp>	// (elpackB6)
#include <ElIni.hpp>	// (elpackB6)
#include <HTMLRender.hpp>	// (elpackB6)
#include <ElHandPt.hpp>	// (elpackB6)
#include <ElMenus.hpp>	// (elpackB6)
#include <ElPopBtn.hpp>	// (elpackB6)
#include <ElScrollBar.hpp>	// (elpackB6)
#include <ElFrmPers.hpp>	// (elpackB6)
#include <ElToolbar.hpp>	// (elpackB6)
#include <ElDBCtrls.hpp>	// (elpkdbB6)
#include <IDEMessages.hpp>	// (designide)
#include <Vcl.CaptionedDockTree.hpp>	// (vcl)
#include <Vcl.DockTabSet.hpp>	// (vcl)
#include <Vcl.Grids.hpp>	// (vcl)
#include <Vcl.CategoryButtons.hpp>	// (vcl)
#include <Vcl.ButtonGroup.hpp>	// (vcl)
#include <Vcl.SysStyles.hpp>	// (vcl)
#include <Winapi.D2D1.hpp>	// (rtl)
#include <Vcl.Direct2D.hpp>	// (vcl)
#include <Vcl.Styles.hpp>	// (vcl)
#include <BrandingAPI.hpp>	// (designide)
#include <Winapi.GDIPOBJ.hpp>	// (rtl)
#include <Vcl.Imaging.pngimage.hpp>	// (vclimg)
#include <System.Devices.hpp>	// (rtl)
#include <Proxies.hpp>	// (designide)
#include <Vcl.AxCtrls.hpp>	// (vcl)
#include <Vcl.AppEvnts.hpp>	// (vcl)
#include <TreeIntf.hpp>	// (designide)
#include <TopLevels.hpp>	// (designide)
#include <StFilSys.hpp>	// (designide)
#include <IDEHelp.hpp>	// (designide)
#include <ComponentDesigner.hpp>	// (designide)
#include <PercentageDockTree.hpp>	// (designide)
#include <Vcl.WinXCtrls.hpp>	// (vclwinx)
#include <WaitDialog.hpp>	// (designide)
#include <Vcl.ExtDlgs.hpp>	// (vcl)
#include <Winapi.Mapi.hpp>	// (rtl)
#include <Vcl.ExtActns.hpp>	// (vcl)
#include <Vcl.ActnMenus.hpp>	// (vclactnband)
#include <Vcl.ActnMan.hpp>	// (vclactnband)
#include <Vcl.PlatformDefaultStyleActnCtrls.hpp>	// (vclactnband)
#include <BaseDock.hpp>	// (designide)
#include <DeskUtil.hpp>	// (designide)
#include <DeskForm.hpp>	// (designide)
#include <DockForm.hpp>	// (designide)
#include <Xml.Win.msxmldom.hpp>	// (xmlrtl)
#include <Xml.xmldom.hpp>	// (xmlrtl)
#include <ToolsAPI.hpp>	// (designide)
#include <DesignEditors.hpp>	// (designide)
#include <VCLEditors.hpp>	// (designide)
#include <ElCalendar.hpp>	// (elpackB6)
#include <ElCalendDlg.hpp>	// (elpackB6)
// PRG_EXT: .bpl
// BPI_DIR: ..\..\..\..\lib\Win32\Debug
// OBJ_DIR: ..\..\..\..\lib\Win32\Debug
// OBJ_EXT: .obj

//-- user supplied -----------------------------------------------------------

namespace Dceldbb6
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dceldbb6 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DCELDBB6)
using namespace Dceldbb6;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Dceldbb6HPP
