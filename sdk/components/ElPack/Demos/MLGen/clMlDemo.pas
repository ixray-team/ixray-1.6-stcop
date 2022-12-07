
{*******************************************************}
{                                                       }
{  EldoS MlGen Demo                                     }
{                                                       }
{  Copyright (c) 1999-2001 Mikhail Chernyshev           }
{                                                       }
{*******************************************************}

unit clMlDemo;

interface

uses
  Windows, SysUtils, Messages, Classes, ElMlGen;

type
  TDemoMLGen = class (TBaseElMLGen)
  private
    FCellWidths: array of array of integer;
    FColWidths: array of integer;
    FOnProcessMessages: TNotifyEvent;
    FOutputStream: TFileStream;
    procedure FreeArrays;
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    procedure IfFound(IfName : string; TagParameters : TStringParameters; var 
            ResultValue : boolean); override;
    procedure LoopIteration(LoopNumb: integer; LoopName: string; TagParameters 
            : TStringParameters; var LoopDone : boolean); override;
    procedure MacroFound(MacroName : string; TagParameters : TStringParameters; 
            var MacroResult : string; var UseTranslationTable : boolean); 
            override;
    procedure PageBegin(PageNumb : integer); override;
    procedure PageEnd(PageNumb : integer); override;
    procedure WriteString(Value : string); override;
    property OnProcessMessages: TNotifyEvent read FOnProcessMessages write 
            FOnProcessMessages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FillCellWidths;
    function GetFileName(FileNumb : integer): string;
    function GetTotalFilesCount: Integer;
  end;
  

implementation
uses frmMain;

{
********************************** TDemoMLGen **********************************
}
constructor TDemoMLGen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // FOutputStream := nil;
end;{TDemoMLGen.Create}

destructor TDemoMLGen.Destroy;
begin
  FreeArrays;
  if Assigned(FOutputStream) then
  begin
    FOutputStream.Free;
    FOutputStream := nil;
  end;
  inherited Destroy;
end;{TDemoMLGen.Destroy}

procedure TDemoMLGen.AfterExecute;
begin
  FreeArrays;
end;{TDemoMLGen.AfterExecute}

procedure TDemoMLGen.BeforeExecute;
begin
  FillCellWidths;
end;{TDemoMLGen.BeforeExecute}

procedure TDemoMLGen.FillCellWidths;
var
  i, j, k, Cols, Rows: Integer;
begin
  Cols := MainForm.StringGrid.ColCount;
  Rows := MainForm.StringGrid.RowCount;
  SetLength(FCellWidths, Rows);
  SetLength(FColWidths, Cols);
  for i := 0 to Cols - 1 do
    FColWidths[i] := 0;
  for i := 0 to Rows - 1 do
  begin
    SetLength(FCellWidths[i], Cols);
    for j := 0 to Cols - 1 do
    begin
      k := Length(MainForm.StringGrid.Cells[j, i]);
      FCellWidths[i,j] := k;
      if FColWidths[j] < k then
        FColWidths[j] := k;
    end;
  end; // for
end;{TDemoMLGen.FillCellWidths}

procedure TDemoMLGen.FreeArrays;
var
  i: Integer;
begin
  for i := 0 to High(FCellWidths) do
  begin
    FCellWidths[i] := nil;
  end; // for
  FCellWidths := nil;
  FColWidths := nil;
end;{TDemoMLGen.FreeArrays}

function TDemoMLGen.GetFileName(FileNumb : integer): string;
var
  s: string;
begin
  Result := MainForm.OutputFilesEdit.Text;
  if MainForm.MultipageCheckBox.Checked and (FileNumb > 1) then
  begin
    s := ExtractFileExt(Result);
    Insert(Format('%d', [FileNumb]), Result, Length(Result) - Length(s) + 1);
  end;
end;{TDemoMLGen.GetFileName}

function TDemoMLGen.GetTotalFilesCount: Integer;
var
  i, j: Integer;
begin
  if MainForm.MultipageCheckBox.Checked then
  begin
    i := MainForm.LinesOnPageEdit.Value;
    j := Length(FCellWidths);
    Result := j div i;
    if (j mod i) <> 0 then
      inc(Result);
  end
  else
    Result := 1;
end;{TDemoMLGen.GetTotalFilesCount}

procedure TDemoMLGen.IfFound(IfName : string; TagParameters : TStringParameters;
        var ResultValue : boolean);
var
  s: string;
  i, j: Integer;
begin
  if IfName = 'Header' then
  begin
    ResultValue := (PageCount = 1) or MainForm.HeaderCheckBox.Checked;
  end
  else
  if IfName = 'Footer' then
  begin
    ResultValue := (PageCount = 1) or MainForm.FooterCheckBox.Checked;
  end
  else
  if IfName = 'NotLastCol' then
  begin
    s := TagParameters.GetValueByNameEx('loop', '');
    if s = '' then
      Raise Exception.Create('В теге <if name="NotLastCol"> должен быть параметр loop с именем цикла');
    j := -1;
    for i := 0 to LoopCount - 1 do
    begin
      if LoopName[i] = s then
      begin
        j := i;
        break;
      end;
    end; // for
    if j < 0 then
      Raise Exception.Create(Format('Цикл указанный в теге <if name="NotLastCol" loop="%s"> не найден', [s]));
    ResultValue := (LoopCounter[j] + 1) <> MainForm.StringGrid.ColCount;
  end
  else
  if IfName = 'FileExist' then
  begin
    i := TagParameters.FindItemByName('offset');
    if i < 0 then
      i := PageCount
    else
      i := PageCount + StrToInt(TagParameters.Value[i]);
    ResultValue := (i > 0) and (i < GetTotalFilesCount);
  end
  else
    Raise Exception.Create('Unknown название условия');
end;{TDemoMLGen.IfFound}

procedure TDemoMLGen.LoopIteration(LoopNumb: integer; LoopName: string; 
        TagParameters : TStringParameters; var LoopDone : boolean);
begin
  if LoopName = 'Rows' then
  begin
    LoopDone := (LoopCounter[LoopNumb] + 1) >= MainForm.StringGrid.RowCount;
    if not LoopDone then
    begin
      // Проверка не пора ли начать новый файл
      if MainForm.MultipageCheckBox.Checked then
      begin
        if LoopCountersCurrentPage[LoopNumb] >= MainForm.LinesOnPageEdit.Value then
          NextPage;
      end;
    end;
  end
  else
  
  if (LoopName = 'Cols') or (LoopName = 'Header') then
  begin
    LoopDone := LoopCounter[LoopNumb] >= MainForm.StringGrid.ColCount;
  end
  else
    Raise Exception.Create('Unknown loop name');
end;{TDemoMLGen.LoopIteration}

procedure TDemoMLGen.MacroFound(MacroName : string; TagParameters : 
        TStringParameters; var MacroResult : string; var UseTranslationTable : 
        boolean);
  
    function GetWidth(aCol, aRow : integer) : Integer;
    var
      i : Integer;
    begin
      Result := TagParameters.FindItemByName('width');
      if Result >= 0 then
        if TagParameters.Value[Result] <> 'auto' then
          Result := StrToInt(TagParameters.Value[Result])
        else begin
          if aCol >= 0 then
            Result := FColWidths[aCol]
          else begin
            Result := 0;
            for i := 0 to High(FColWidths) do
              Result := Result + FColWidths[i];
          end;
        end;
    end;
  
    function CalcSeparatorWidth(Space : Integer) : Integer;
    var
      i : Integer;
    begin
      Result := 0;
      for i := 0 to High(FColWidths) do
      begin
        Result := Result + FColWidths[i] + Space;
      end; // for
    end;
  
  var
    i, w : Integer;
    Col, Row : Integer;
    c : Char;
    s : string;
  
begin
  if MacroName = 'Cell' then
  begin
    Col := LoopCounterStr['Cols'];
    Row := LoopCounterStr['Rows'] + 1;
    w := GetWidth(Col, Row);
    MacroResult := MainForm.StringGrid.Cells[Col, Row];
    if w > 0 then
      MacroResult := SetStrWidth(MacroResult, w);
  end
  else
  
  if MacroName = 'CellHeader' then
  begin
    Col := LoopCounterStr['Header'];
    w := GetWidth(Col, 0);
    MacroResult := MainForm.StringGrid.Cells[Col, 0];
    if w > 0 then
      MacroResult := SetStrWidth(MacroResult, w);
  end
  else
  
  if MacroName = 'Row' then
  begin
    Row := LoopCounterStr['Rows'] + 1;
    w := GetWidth(-1, Row);
    MacroResult := MainForm.StringGrid.Cells[0, Row];
    if MainForm.StringGrid.ColCount > 1 then
      MacroResult := MacroResult + ' ';
    for i := 1 to MainForm.StringGrid.ColCount - 1 do
    begin
      if i <> 1 then
        MacroResult := MacroResult + ', ';
      MacroResult := MacroResult + #39 + MainForm.StringGrid.Cells[i, Row] + #39;
    end; // for
    if w > 0 then
      MacroResult := SetStrWidth(MacroResult, w);
  end
  else
  
  if MacroName = 'FileName' then
  begin
    i := TagParameters.FindItemByName('offset');
    if i < 0 then
      i := 0
    else
      i := StrToInt(TagParameters.Value[i]);
    MacroResult := ExtractFileName(GetFileName(PageCount + i));
  end
  else
  
  if MacroName = 'FileNumb' then
  begin
    i := TagParameters.FindItemByName('offset');
    if i < 0 then
      i := 0
    else
      i := StrToInt(TagParameters.Value[i]);
    MacroResult := IntToStr(PageCount + i);
  end
  else
  
  if MacroName = 'TotalFileNumb' then
  begin
    MacroResult := IntToStr(GetTotalFilesCount);
  end
  else
  
  if MacroName = 'Separator' then
  begin
    i := TagParameters.FindItemByName('spacecol');
    if i < 0 then
      i := 0
    else
      i := StrToInt(TagParameters.Value[i]);
    w := CalcSeparatorWidth(i);
  
    i := TagParameters.FindItemByName('space');
    if i < 0 then
      i := 0
    else
      i := StrToInt(TagParameters.Value[i]);
    inc(w, i);
  
    i := TagParameters.FindItemByName('char');
    if i < 0 then
      c := '-'
    else begin
      s := TagParameters.Value[i];
      if Length(s) > 0 then
        c := s[1]
      else
        c := '-';
    end;
    SetLength(MacroResult, w);
    if w > 0 then
      Fillchar(MacroResult[1], w, c);
  end
  else
    // Raise Exception.Create('Unknown macro name');
    MacroResult := 'Unknown macro name';
end;{TDemoMLGen.MacroFound}

procedure TDemoMLGen.PageBegin(PageNumb : integer);
begin
  if Assigned(FOutputStream) then
    Raise Exception.Create('Internal error. Outpust stream already exist.');
  FOutputStream := TFileStream.Create(GetFileName(PageNumb), fmCreate or fmShareDenyWrite);
end;{TDemoMLGen.PageBegin}

procedure TDemoMLGen.PageEnd(PageNumb : integer);
begin
  FOutputStream.Free;
  FOutputStream := nil;
end;{TDemoMLGen.PageEnd}

procedure TDemoMLGen.WriteString(Value : string);
begin
  FOutputStream.WriteBuffer(Value[1], Length(Value));
end;{TDemoMLGen.WriteString}

end.
