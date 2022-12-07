{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
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


unit PgCtlProp;

interface

uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    Menus,
    ElPgCtl,
    {$ifdef VCL_4_USED}
    ImgList,
    {$endif}
{$IFDEF D_5_UP}
    ElImageIndexProp,
{$ENDIF}
    DesignIntf, DesignMenus, DesignEditors, DesignWindows, DsnConst
    ;

type
  TElPageControlEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer): string; override;
    function GetVerbCount: Integer; override;
  private
    function GetPageControl : TElPageControl;
  public
    {$ifdef VCL_5_USED}
    {$ifndef VCL_6_USED}
    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override;
    {$else}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
    {$endif}
    {$endif}
  end;

  TElActivePageProperty = class(TComponentProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{$ifndef CLX_USED}
{$IFDEF D_5_UP}
type
  TElTabSheetImageIndexProperty = class(TElImageIndexProperty)
  protected
    function GetImages: TCustomImageList; override;
  end;

{$endif}
{$endif}

implementation

{$ifndef CLX_USED}
{$IFDEF D_5_UP}

function TElTabSheetImageIndexProperty.GetImages: TCustomImageList;
var
  PageControl : TElPageControl;
  Component: TPersistent;
begin
  Result := nil;
  Component := GetComponent(0);
  if not (Assigned(Component) and (Component is TElTabSheet)) then Exit;
  PageControl := TElTabSheet(Component).PageControl;
  if PageControl <> nil then
  begin
    Result := PageControl.Images;
  end;
end;

{$endif}
{$endif}

procedure TElPageControlEditor.ExecuteVerb(Index : Integer);
var PageControl : TElPageControl;

    procedure AddPage;
    var Page : TElTabSheet;
    begin
      Page := TElTabSheet.Create(Designer.GetRoot);
      try
        Page.Name := Designer.UniqueName(TElTabSheet.ClassName);
        Page.Caption := Page.Name;
        Page.PageControl := PageControl;
      except
        Page.Free;
        raise;
      end;
      PageControl.ActivePage := Page;
      {$IFDEF VCL_4_USED}
      Designer.SelectComponent(Page);
      {$ENDIF}
      Designer.Modified;
    end;

    procedure GoToPage(Next : boolean);
    var ASheet : TElTabSheet;
    begin
      ASheet := PageControl.FindNextPage(PageControl.ActivePage, Next, false, false);
      if (ASheet <> nil) and (ASheet <> PageControl.ActivePage) then
      begin
        PageControl.ActivePage := ASheet;
        {$IFDEF VCL_4_USED}
        if Component is TElTabSheet then
          Designer.SelectComponent(ASheet);
        {$ENDIF}
        Designer.Modified;
      end;
    end;

begin
  PageControl := GetPageControl;
  case Index of
    0: AddPage;
    1: GoToPage(true);
    2: GoToPage(false);
    3: if PageControl.ActivePage <> nil then
         PageControl.ActivePage.Free;
    else
      inherited;
  end;
end;

function TElPageControlEditor.GetVerb(Index : Integer): string;
begin
  case Index of
    0: result := 'New Page';
    1: result := 'Next Page';
    2: result := 'Previous Page';
    3: result := 'Delete Page';
    else result := '';
  end;
end;

function TElPageControlEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

function TElPageControlEditor.GetPageControl : TElPageControl;
begin
  if (Component is TElPageControl) then
    result := TElPageControl(Component)
  else
  if (Component is TElTabSheet) then
    result := TElTabSheet(Component).PageControl
  else
    raise Exception.Create('ElPageControl property editor used for unknown component');
end;

{$ifdef VCL_5_USED}
{$ifndef VCL_6_USED}
procedure TElPageControlEditor.PrepareItem(Index: Integer; const AItem: TMenuItem);
{$else}
procedure TElPageControlEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
{$endif}
begin
  if Index = 3 then
  begin
    AItem.Enabled := GetPageControl.ActivePage <> nil;
  end
  else
    inherited;
end;
{$endif}

function TElActivePageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TElActivePageProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  if GetComponent(0) is TElPageControl then
  begin
    for I := 0 to TElPageControl(GetComponent(0)).PageCount - 1 do
      Proc(TElPageControl(GetComponent(0)).Pages[i].Name);
  end;
end;

end.
