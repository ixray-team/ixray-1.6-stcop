{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{   Parts:                                           }
{   copyright (c) 2001 Akzhan Abdulin                }
{                                                    }
{====================================================}
{$include elpack2.inc}
{$ifdef ELPACK_SINGLECOMP}
{$I ElPack.inc}
{$else}
{$ifdef LINUX}
{$I ../ElPack.inc}
{$else}
{$I ..\ElPack.inc}
{$endif}
{$endif}

unit ElPrinterPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ElPanel, ElToolBar, ElStatBar, ElPopBtn, StdCtrls, ElACtrls,
  ElTools, Printers, ElPrinter, ElSpin, ElList, ElHook, ElCombos,
  ElXPThemedControl, ElEdits, ElScrollBox;

type
  TElPrinterPreviewDlg = class(TForm)
    Toolbar: TElToolBar;
    ScrollBox: TScrollBox;
    StatusBar: TElStatusBar;
    PrintBtn: TElToolButton;
    ElToolButton2: TElToolButton;
    OnePageBtn: TElToolButton;
    MultipageBtn: TElToolButton;
    ElToolButton1: TElToolButton;
    SaveBtn: TElToolButton;
    ElToolButton3: TElToolButton;
    PrintDialog: TPrintDialog;
    PrintSetupBtn: TElToolButton;
    PrevPageBtn: TElToolButton;
    ElToolButton5: TElToolButton;
    NextPageBtn: TElToolButton;
    PageSpin: TElSpinEdit;
    PrinterSetupDialog: TPrinterSetupDialog;
    SaveDialog: TSaveDialog;
    CloseBtn: TElGraphicButton;
    PagesPanel: TElPanel;
    MainPagePanel: TElPanel;
    ElHook1: TElHook;
    ScaleCombo: TElComboBox;
    ElHook2: TElHook;
    procedure PrintBtnClick(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
    procedure MainPagePanelPaint(Sender: TObject);
    procedure PageSpinChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScaleComboExit(Sender: TObject);
    procedure NextPageBtnClick(Sender: TObject);
    procedure PrintSetupBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PrevPageBtnClick(Sender: TObject);
    procedure ScComboKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ElHook1AfterProcess(Sender: TObject; var Msg: TMessage;
      var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure ScaleComboChange(Sender: TObject);
    procedure MultipageBtnClick(Sender: TObject);
    procedure OnePageBtnClick(Sender: TObject);
    procedure ScrollBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ElHook2AfterProcess(Sender: TObject; var Msg: TMessage;
      var Handled: Boolean);
  private
    FScale   : Integer;
    FCurrentPage: Integer;
    FTotalPages: Integer;
    Panels   : TElList;
    PagePanels: integer;
    HorzPages,
    VertPages : integer;
    FPrinter  : TElPrinter;
    FScaleIdx : integer;
    FRealIdx  : integer;
    procedure SetCurrentPage(Value: Integer);
    procedure SetTotalPages(Value: Integer);
    procedure SetScale(Value: Integer);

  protected
    procedure UpdatePageNumbers;
    procedure UpdatePanels;
    procedure UpdateMultiPage;
  public
    procedure SetData(Printer : TElPrinter);
    property CurrentPage: Integer read FCurrentPage write SetCurrentPage;
    property TotalPages: Integer read FTotalPages write SetTotalPages;
    property Scale: Integer read FScale write SetScale;

  end;

var
  ElPrinterPreviewDlg: TElPrinterPreviewDlg;

implementation

const Margin = 40;

{$R *.DFM}

function ScaleSizeDlg(var HorzPages, VertPages : integer) : boolean;
var Label1,
    Label2 : TLabel;
    HorzSpin,
    VertSpin : TElSpinEdit;
    ElPopupButton1: TElPopupButton;
    ElPopupButton2: TElPopupButton;
    AForm : TForm;
begin
  result := false;
  AForm := TForm.CreateNew(nil);
  try
    with AForm do
    begin
      BorderStyle := bsDialog;
      Scaled := false;
      Scaled := true;
      Height := MulDiv(128, PixelsPerInch, 96);
      Width := MulDiv(184, PixelsPerInch, 96);
      Position := poScreenCenter;
      Caption := 'Multiple pages';

      Label1 := TLabel.Create(AForm);
      HorzSpin := TElSpinEdit.Create(AForm);
      Label2 := TLabel.Create(AForm);
      VertSpin := TElSpinEdit.Create(AForm);
      ElPopupButton1 := TElPopupButton.Create(AForm);
      ElPopupButton2 := TElPopupButton.Create(AForm);
      with Label1 do
      begin
        Name := 'Label1';
        Parent := AForm;
        Left := MulDiv(8, PixelsPerInch, 96);
        Top := MulDiv(8, PixelsPerInch, 96);
        Width := MulDiv(160, PixelsPerInch, 96);
        Height := MulDiv(26, PixelsPerInch, 96);
        Caption := 'Select the number of pages to be '#13#10'displayed at the same time';
      end;
      with HorzSpin do
      begin
        Name := 'HorzSpin';
        Parent := AForm;
        Left := MulDiv(44, PixelsPerInch, 96);
        Top := MulDiv(40, PixelsPerInch, 96);
        Width := MulDiv(33, PixelsPerInch, 96);
        Height := MulDiv(21, PixelsPerInch, 96);
        Value := 1;
        MaxValue := 4;
        MinValue := 1;
        Flat := True;
        TabOrder := 2;
      end;
      with Label2 do
      begin
        Name := 'Label2';
        Parent := AForm;
        Left := MulDiv(84, PixelsPerInch, 96);
        Top := MulDiv(44, PixelsPerInch, 96);
        Width := MulDiv(5, PixelsPerInch, 96);
        Height := MulDiv(13, PixelsPerInch, 96);

        Caption := 'x';
      end;
      with VertSpin do
      begin
        Name := 'VertSpin';
        Parent := AForm;

        Left := MulDiv(100, PixelsPerInch, 96);
        Top := MulDiv(40, PixelsPerInch, 96);
        Width := MulDiv(33, PixelsPerInch, 96);
        Height := MulDiv(21, PixelsPerInch, 96);

        Value := 1;
        MaxValue := 4;
        MinValue := 1;
        Flat := True;
        TabOrder := 3;
      end;
      with ElPopupButton1 do
      begin
        Name := 'ElPopupButton1';
        Parent := AForm;
        Left := MulDiv(11, PixelsPerInch, 96);
        Top := MulDiv(72, PixelsPerInch, 96);
        Width := MulDiv(75, PixelsPerInch, 96);
        Height := MulDiv(25, PixelsPerInch, 96);

        DrawDefaultFrame := True;
        Default := True;
        ModalResult := 1;
        Caption := 'OK';
        TabOrder := 0;
      end;
      with ElPopupButton2 do
      begin
        Name := 'ElPopupButton2';
        Parent := AForm;
        Left := MulDiv(91, PixelsPerInch, 96);
        Top := MulDiv(72, PixelsPerInch, 96);
        Width := MulDiv(75, PixelsPerInch, 96);
        Height := MulDiv(25, PixelsPerInch, 96);
        DrawDefaultFrame := True;
        Cancel := True;
        ModalResult := 2;
        Caption := 'Cancel';
        TabOrder := 1;
      end;
      if ShowModal = mrOk then
      begin
        HorzPages := HorzSpin.Value;
        VertPages := VertSpin.Value;
        result := true;
      end;
    end;
  finally
    AForm.Free;
  end;
end;

procedure TElPrinterPreviewDlg.PrintBtnClick(Sender: TObject);
var AFrom, ATo : integer;
begin
  PrintDialog.Options := PrintDialog.Options + [poPageNums];
  if PrintDialog.Execute then
  begin
    AFrom := PrintDialog.FromPage;
    ATo   := PrintDialog.ToPage;
    if AFrom < 1 then AFrom := 1;
    if (ATo < 1) or (ATo > FTotalPages) then
      ATo := FTotalPages; 
    FPrinter.PrintPages(AFrom, ATo);
  end;
end;

procedure TElPrinterPreviewDlg.ScrollBoxResize(Sender: TObject);
begin
  UpdatePanels;
end;

procedure TElPrinterPreviewDlg.MainPagePanelPaint(Sender: TObject);
var APanel : TElPanel;
    AMetafile : TMetafile;
    R : TRect;
    DC : HDC;
begin
  APanel := TElPanel(Sender);
  AMetafile := FPrinter.Page[APanel.Tag - 1];
  R := Rect(0, 0, APanel.ClientWidth, APanel.ClientHeight);
  APanel.Canvas.Brush.Color := clWhite;
  APanel.Canvas.FillRect(R);
  DC := GetDC(0);

  R.Left := MulDiv(FPrinter.LeftMargin, Scale * GetDeviceCaps(DC, HORZRES), 10000 * GetDeviceCaps(DC, HORZSIZE));
  R.Top  := MulDiv(FPrinter.TopMargin, Scale * GetDeviceCaps(DC, VERTRES), 10000 * GetDeviceCaps(DC, VERTSIZE));

  R.Right := R.Right - MulDiv(FPrinter.RightMargin, Scale * GetDeviceCaps(DC, HORZRES), 10000 * GetDeviceCaps(DC, HORZSIZE));
  R.Bottom := R.Bottom - MulDiv(FPrinter.BottomMargin, Scale * GetDeviceCaps(DC, VERTRES), 10000 * GetDeviceCaps(DC, VERTSIZE));
  APanel.Canvas.StretchDraw(R, AMetafile);
  ReleaseDC(0, DC);
end;

procedure TElPrinterPreviewDlg.PageSpinChange(Sender: TObject);
begin
  CurrentPage := PageSpin.Value;
  PagesPanel.SetFocus;
end;

procedure TElPrinterPreviewDlg.SetCurrentPage(Value: Integer);
begin
  if FCurrentPage <> Value then
  begin
    FCurrentPage := Value;
    UpdatePageNumbers;
    NextPageBtn.Enabled := CurrentPage < FPrinter.PageCount;
    PrevPageBtn.Enabled := CurrentPage > 1;
    PageSpin.Value := Value;
    if PagePanels > 1 then
      UpdateMultipage
    else
      MainPagePanel.Invalidate;
  end;
end;

procedure TElPrinterPreviewDlg.SetTotalPages(Value: Integer);
begin
  if FTotalPages <> Value then
  begin
    FTotalPages := Value;
    PageSpin.MinValue := 1;
    PageSpin.MaxValue := Value;

    if PagePanels > 1 then
    begin
      UpdateMultipage;
    end;
  end;
end;

procedure TElPrinterPreviewDlg.UpdatePageNumbers;
var i : integer;
begin
  if Panels.Count = 1 then
    MainPagePanel.Tag := CurrentPage
  else
  for i := 0 to Panels.Count - 1 do
  begin
    TElPanel(Panels[i]).Tag := CurrentPage + i;
    TElPanel(Panels[i]).Invalidate;
  end;
end;

procedure TElPrinterPreviewDlg.FormCreate(Sender: TObject);
begin
  Panels := TElList.Create;
  Panels.Add(MainPagePanel);
  FScale := 100;
  FScaleIdx := 3;
  ScaleCombo.ItemIndex := 3;
end;

procedure TElPrinterPreviewDlg.FormDestroy(Sender: TObject);
begin
  while Panels.Count > 1 do
  begin
    TElPanel(Panels[1]).Free;
    Panels.Delete(1);
  end;
  Panels.Free;
end;

procedure TElPrinterPreviewDlg.ScaleComboExit(Sender: TObject);
var Text : string;
    i    : integer;
begin
  if ScaleCombo.ItemIndex <> -1 then
    Text := ScaleCombo.Items[ScaleCombo.ItemIndex]
  else
    Text := ScaleCombo.Text;
  if (Pos('%', Text) = Length(Text)) or (StrToIntDef(Text, -1) <> -1) then
  begin
    if (Pos('%', Text) = Length(Text)) then
      Delete(Text, Length(Text), 1);
    i := StrToIntDef(Text, -1);
    if i = -1 then
    begin
      ScaleCombo.Text := IntToStr(Scale) + '%';
    end
    else
    begin
      Scale := i;
    end;
  end
  else
  if Text = ScaleCombo.Items[8] then
  begin
    Scale := -5;
  end
  else
  if Text = ScaleCombo.Items[9] then
  begin
    Scale := -4;
  end
  else
  if Text = ScaleCombo.Items[10] then
  begin
    Scale := -3;
  end
  else
  if Text = ScaleCombo.Items[11] then
  begin
    // multiple pages
    Scale := -2;
    if ScaleCombo.ItemIndex = 11 then
      ScaleCombo.Text := Text;
  end
  else
  begin
    if Scale < 0 then
      ScaleCombo.Text := ScaleCombo.Items[Scale + 12]
    else
    begin
      ScaleCombo.Text := IntToStr(Scale) + '%';
    end;
  end;
  FScaleIdx := ScaleCombo.ItemIndex;
end;

procedure TElPrinterPreviewDlg.SetScale(Value: Integer);
var Panel : TElPanel;
    SHPos,
    SVPos : Integer;
    DC    : integer;
    V1,
    V2    : integer;
begin
  if FScale <> Value then
  begin
    if FScale = -2 then
    begin
      while Panels.Count > 1 do
      begin
        TElPanel(Panels[1]).Free;
        Panels.Delete(1);
      end;
    end;

    SHPos := MulDiv(ScrollBox.HorzScrollBar.Position, 10000, FScale * ScrollBox.HorzScrollBar.Range);
    SVPos := MulDiv(ScrollBox.VertScrollBar.Position, 10000, FScale * ScrollBox.VertScrollBar.Range);

    FRealIdx := Value;
    if Value < 0 then
    begin
      DC := GetDC(0);
      try
        case Value of
          -2: begin
                // Multiple pages
                if not ScaleSizeDlg(HorzPages, VertPages) then
                begin
                  ScaleCombo.ItemIndex := FScaleIdx;
                  PagesPanel.SetFocus;
                  exit;
                end;

                PagePanels := HorzPages * VertPages;

                while Panels.Count > PagePanels do
                begin
                  TObject(Panels.Last).Free;
                  Panels.Delete(Panels.Count - 1);
                end;

                while Panels.Count < PagePanels do
                begin
                  Panel := TElPanel.Create(nil);
                  Panels.Add(Panel);
                  Panel.Parent := PagesPanel;
                  Panel.OwnerDraw := true;
                  Panel.OnPaint := MainPagePanelPaint;
                end;
                UpdateMultipage;

                V1 := MulDiv(ScrollBox.ClientWidth - Margin * (HorzPages + 1), 10000 * GetDeviceCaps(DC, HORZSIZE), (FPrinter.PageWidth + FPrinter.LeftMargin + FPrinter.RightMargin) * HorzPages * GetDeviceCaps(DC, HORZRES));
                V2 := MulDiv(ScrollBox.ClientHeight - Margin * (VertPages + 1), 10000 * GetDeviceCaps(DC, VERTSIZE), (FPrinter.PageHeight + FPrinter.TopMargin + FPrinter.BottomMargin) * VertPages * GetDeviceCaps(DC, VERTRES));

                Value := Min(V1, V2);

              end;
          -3: begin
                // Whole page
                if Printer.Orientation = poLandscape then
                // if (FPrinter.Width + FPrinter.LeftMargin + FPrinter.RightMargin) >
                //   (FPrinter.Height + FPrinter.TopMargin + FPrinter.BottomMargin) then
                begin
                  Value := MulDiv(ScrollBox.ClientWidth - Margin * 2, 10000 * GetDeviceCaps(DC, HORZSIZE), (FPrinter.PageWidth + FPrinter.LeftMargin + FPrinter.RightMargin) * GetDeviceCaps(DC, HORZRES));
                end
                else
                begin
                  Value := MulDiv(ScrollBox.ClientHeight - Margin * 2, 10000 * GetDeviceCaps(DC, VERTSIZE), (FPrinter.PageHeight + FPrinter.TopMargin + FPrinter.BottomMargin) * GetDeviceCaps(DC, VERTRES));
                end;
              end;
          -4: begin
                // Text width
                Value := MulDiv(ScrollBox.ClientWidth - Margin * 2, 10000 * GetDeviceCaps(DC, HORZSIZE), FPrinter.PageWidth * GetDeviceCaps(DC, HORZRES));
              end;
          -5: begin
                // Page width
                Value := MulDiv(ScrollBox.ClientWidth - Margin * 2, 10000 * GetDeviceCaps(DC, HORZSIZE), (FPrinter.PageWidth + FPrinter.LeftMargin + FPrinter.RightMargin) * GetDeviceCaps(DC, HORZRES));
              end;
        end;
      finally
        ReleaseDC(0, DC);
      end;
    end;

    FScale := Value;

    ScaleCombo.ItemIndex := -1;
    FScaleIdx := -1;
    // PostMessage(ScaleCombo.Handle, WM_SETTEXT, 0, Integer(PChar(IntToStr(FScale) + '%')));
    ScaleCombo.Text := IntToStr(FScale) + '%';

    ScrollBox.HorzScrollBar.Position := 0;
    ScrollBox.VertScrollBar.Position := 0;

    UpdatePanels;

    SHPos := MulDiv(SHPos, FScale * ScrollBox.HorzScrollBar.Range, 10000);
    SVPos := MulDiv(SVPos, FScale * ScrollBox.VertScrollBar.Range, 10000);

    ScrollBox.UpdateControlState;

    SetScrollPos(ScrollBox.Handle, SB_HORZ, SHPos, true);
    SetScrollPos(ScrollBox.Handle, SB_VERT, SVPos, true);
  end;
end;

procedure TElPrinterPreviewDlg.UpdatePanels;
var RealSize : TSize;
    DC : HDC;
    R1 : TRect;
    APanel : TElPanel;
    i, j   : integer;
begin
  DC := GetDC(0);

  RealSize.cx := FPrinter.PageWidth + FPrinter.LeftMargin + FPrinter.RightMargin;
  RealSize.cx := MulDiv(RealSize.cx, Scale * GetDeviceCaps(DC, HORZRES), 10000 * GetDeviceCaps(DC, HORZSIZE));

  RealSize.cy := FPrinter.PageHeight + FPrinter.TopMargin + FPrinter.BottomMargin;
  RealSize.cy := MulDiv(RealSize.cy, Scale * GetDeviceCaps(DC, VERTRES), 10000 * GetDeviceCaps(DC, VERTSIZE));

  if (PagePanels = 1) then
  begin

    MainPagePanel.SetBounds(Margin, Margin, RealSize.cx, RealSize.cy);

    CenterRects(RealSize.cx + Margin * 2, ScrollBox.ClientWidth,
                RealSize.cy + Margin * 2, ScrollBox.ClientHeight, R1);

    PagesPanel.SetBounds(Max(R1.Left, 0), Max(R1.Top, 0), RealSize.cx + Margin * 2, RealSize.cy + Margin * 2);
  end
  else
  begin
    for i := 0 to HorzPages -1 do
    begin
      for j := 0 to VertPages - 1 do
      begin
        APanel   := TElPanel(Panels[j * HorzPages + i]);
        R1.Left  := Margin * (i + 1) + RealSize.cx * i;
        R1.Top   := Margin * (j + 1) + RealSize.cy * j;

        APanel.SetBounds(R1.Left, R1.Top, RealSize.cx, RealSize.cy);        
      end;
    end;
    PagesPanel.SetBounds(0, 0, RealSize.cx * HorzPages + Margin * (HorzPages + 1),
                               RealSize.cy * VertPages + Margin * (VertPages + 1));
  end;

  ReleaseDC(0, DC);
end;

procedure TElPrinterPreviewDlg.SetData(Printer : TElPrinter);
begin
  FPrinter := Printer;
  PagePanels  := 1;
  PageSpin.MaxValue := FPrinter.PageCount;
  FTotalPages := FPrinter.PageCount;
  CurrentPage := 1;
end;

procedure TElPrinterPreviewDlg.NextPageBtnClick(Sender: TObject);
begin
  if CurrentPage < FPrinter.PageCount then
    CurrentPage := CurrentPage + 1
  else
    NextPageBtn.Enabled := false;
end;

procedure TElPrinterPreviewDlg.PrintSetupBtnClick(Sender: TObject);
begin
  if PrinterSetupDialog.Execute then
    ModalResult := mrAbort;
end;

procedure TElPrinterPreviewDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TElPrinterPreviewDlg.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    FPrinter.SavePage(SaveDialog.FileName, CurrentPage - 1);
end;

procedure TElPrinterPreviewDlg.FormShow(Sender: TObject);
begin
  UpdatePanels;
end;

procedure TElPrinterPreviewDlg.PrevPageBtnClick(Sender: TObject);
begin
  if CurrentPage > 1 then
    CurrentPage := CurrentPage - 1
  else
     PrevPageBtn.Enabled := false;
end;

procedure TElPrinterPreviewDlg.ScComboKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var AText : string;
begin
  if (Key = VK_RETURN) and (Shift = []) then
  begin
    AText := scaleCombo.Text;
    scaleCombo.ItemIndex := -1;
    scaleCombo.Text := AText;
    PagesPanel.SetFocus;
  end;
end;

procedure TElPrinterPreviewDlg.ElHook1AfterProcess(Sender: TObject;
  var Msg: TMessage; var Handled: Boolean);
begin
  if Msg.Msg = CN_COMMAND then
  begin
    if TWMCommand(Msg).NotifyCode = CBN_SELENDOK then
    begin
      if ScaleCombo.Focused then
        PagesPanel.SetFocus
      else
        ScaleComboExit(Self);
    end;
  end;
end;

procedure TElPrinterPreviewDlg.FormResize(Sender: TObject);
begin
  if ScaleCombo.ItemIndex = -1 then
  begin
    SetScale(FRealIdx);
  end;
end;

procedure TElPrinterPreviewDlg.ScaleComboChange(Sender: TObject);
begin
  if (not ScaleCombo.Focused) or (FScaleIdx <> ScaleCombo.ItemIndex) then
  begin
    if Focused then
      PagesPanel.SetFocus
    else
      ScaleComboExit(Sender);
    FScaleIdx := ScaleCombo.ItemIndex;
  end;
end;

procedure TElPrinterPreviewDlg.UpdateMultiPage;
var i : integer;
begin
  for i := 0 to Panels.Count - 1 do
  begin
    TElPanel(Panels[i]).Tag := i + FCurrentPage;
    TElPanel(Panels[i]).Visible := i + FCurrentPage <= FTotalPages;
    TElPanel(Panels[i]).Invalidate;
  end;
end;

procedure TElPrinterPreviewDlg.MultipageBtnClick(Sender: TObject);
begin
  ScaleCombo.ItemIndex := 11;
  ScaleComboExit(Self);
  PagesPanel.SetFocus;
end;

procedure TElPrinterPreviewDlg.OnePageBtnClick(Sender: TObject);
begin
  ScaleCombo.ItemIndex := 3;
  ScaleComboExit(Self);
  PagesPanel.SetFocus;
end;

procedure TElPrinterPreviewDlg.ScrollBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case key of
//    VK_LEFT  :
//    VK_RIGHT :
    VK_DOWN  : ScrollBox.Perform(WM_VSCROLL, MakeWParam(SB_LINEDOWN,0), 0);
    VK_UP    : ScrollBox.Perform(WM_VSCROLL, {MakeWParam(}SB_LINEUP{,0)}, 0);
    VK_PRIOR : ScrollBox.Perform(WM_VSCROLL, {MakeWParam(}SB_PAGEUP{,0)}, 0);
    VK_NEXT  : ScrollBox.Perform(WM_VSCROLL, {MakeWParam(}SB_PAGEDOWN{,0)}, 0);
  end;
end;

procedure TElPrinterPreviewDlg.ElHook2AfterProcess(Sender: TObject;
  var Msg: TMessage; var Handled: Boolean);
begin
  if Msg.Msg = WM_GETDLGCODE then
    Msg.Result := DLGC_WANTARROWS;

  if Msg.Msg = WM_KEYDOWN then
  begin
     case Msg.WParam of
      VK_LEFT  : ScrollBox.Perform(WM_HSCROLL, SB_LINELEFT, 0);
      VK_RIGHT : ScrollBox.Perform(WM_HSCROLL, SB_LINERIGHT, 0);
      VK_DOWN  : ScrollBox.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
      VK_UP    : ScrollBox.Perform(WM_VSCROLL, SB_LINEUP, 0);
      VK_PRIOR : PrevPageBtnClick(Self);{ScrollBox.Perform(WM_VSCROLL, SB_PAGEUP, 0);}
      VK_NEXT  : NextPageBtnClick(Self);{ScrollBox.Perform(WM_VSCROLL, SB_PAGEDOWN, 0);}
      VK_END   : begin
                   PageSpin.Value := FPrinter.PageCount;
                   PageSpinChange(Self);
                 end;
    end;
    Msg.Result := 0;
  end;
end;

end.
