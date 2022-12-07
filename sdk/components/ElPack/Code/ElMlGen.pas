{*******************************************************}
{                                                       }
{  EldoS Markup Language Generator (MlGen)              }
{                                                       }
{  Copyright (c) 1999-2001 Mikhail Chernyshev           }
{                                                       }
{*******************************************************}

{-$define ELMLGEN_ACTIVEX}

{$ifdef ELMLGEN_ACTIVEX}
{$R ElMLGen.dcr}
{$endif}

unit ElMlGen;

(* Language description

Template parameters.
<params>
  <param name="Param_Name" value="ParamValue"/>
  <param name="Param_Name">ParamValue</param>
  <param name="Param_Name" value="ParamVa">lue</param>
  <translation name="Translation_Table_Name" default="True">
    <replace string="string1" with="string2"/>
    <replace string="string1">string2</replace>
    <replace string="string1" with="stri">ng2</replace>
  </translation name="Translation_Table_Name">
</params>

Loops.
<repeat name="loop name">
</repeat name="loop name">

Macros. Tag is completely replaced.
<macro name="macro name"/>

Conditions. Either firs or second part of the comlex tag is executed.
<if name="condition name">
<else name="condition name"/>
</if name="condition name">

Comments:
<comment name="any_name">
</comment name="any_name">
or
<comment name="any_name"/>

If the line doesn't contain anything except spaces and macros which turns into empty space,
empty line will not be generated.

*)

interface

uses
  SysUtils,
  {$ifdef ELMLGEN_ACTIVEX}
  Windows,
  Messages,
  Forms,
  Controls,
  Graphics,
  {$endif}
  Classes;

type
  TDynamicString = array of String;
  TStringsRec = record
    ValueName, Value : string;
  end;
  TArrayOfStringsRec = array of TStringsRec;

  TTranslationTable = class;
  TStringParameters = class;
  TMlPageEvent = procedure (Sender: TObject; PageNumb : integer) of object;
  TMlIfFoundEvent = procedure (Sender: TObject; IfName : string; TagParameters 
          : TStringParameters; var ResultValue : boolean) of object;
  TMlLoopIterateEvent = procedure (Sender: TObject; LoopNumb : integer; 
          LoopName : string; TagParameters : TStringParameters; var LoopDone : 
          boolean) of object;
  TMlMacroFoundEvent = procedure (Sender: TObject; MacroName : string; 
          TagParameters : TStringParameters; var MacroResult : string; var 
          UseTranslationTable : boolean) of object;
  TMlWriteStringEvent = procedure (Sender: TObject; Value : string) of object;
  TMlTagFoundEvent = procedure (Sender: TObject; Tag : string; const TagClosed 
          : boolean; TagParameters : TStringParameters) of object;
  TMlIsTagEvent = procedure (Sender: TObject; TagName : string; var IsTag : 
          boolean) of object;
  TStringParameters = class (TPersistent)
  private
    FData: TArrayOfStringsRec;
    function GetCount: Integer;
    function GetValue(Index: Integer): string;
    function GetValueByName(ValueName : string): string;
    function GetValueName(Index: Integer): string;
    procedure SetCount(Value: Integer);
    procedure SetValue(Index: Integer; const Value: string);
    procedure SetValueByName(ValueName : string; const Value: string);
    procedure SetValueName(Index: Integer; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function AddValue(const aValueName, aValue : string): Integer;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Clear;
    function FindItemByName(ValueName : string): Integer;
    function GetValueByNameEx(ValueName, DefaultValue : string): string;
    property Count: Integer read GetCount write SetCount;
    property Data: TArrayOfStringsRec read FData;
    property Value[Index: Integer]: string read GetValue write SetValue;
    property ValueByName[ValueName : string]: string read GetValueByName write 
            SetValueByName;
    property ValueName[Index: Integer]: string read GetValueName write 
            SetValueName;
  end;
  
  TTranslationTables = class (TCollection)
  private
    FDefaultTable: Integer;
    function GetItems(Index: Integer): TTranslationTable;
    procedure SetDefaultTable(Value: Integer);
    procedure SetItems(Index: Integer; Value: TTranslationTable);
  public
    constructor Create;
    function Add(const TableName : string): TTranslationTable;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function FindTableByName(const TableName : string): Integer;
    procedure GetTableNames(Strings : TStrings);
    property DefaultTable: Integer read FDefaultTable write SetDefaultTable;
    property Items[Index: Integer]: TTranslationTable read GetItems write 
            SetItems; default;
  end;
  
  TTranslationTable = class (TCollectionItem)
  private
    FName: string;
    FTable: TStringParameters;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Translate(Source : string): string;
    property Name: string read FName write FName;
    property Table: TStringParameters read FTable;
  end;
  
  {$ifdef ELMLGEN_ACTIVEX}
  TBaseElMLGen = class (TCustomControl)
  {$else}
  TBaseElMLGen = class (TComponent)
  {$endif}
  private
    FCancelJob: Boolean;
    FCode: TStrings;
    FCommentName: string;
    FCustomTagNames: TStrings;
    {{
    Устанавливается в истину при открытии следующего выходящего файла.
    Снимается тогда, когда будет найдена точка из которой была вызвана команда
    на открытие нового файла. Считается,что точка найдена, когда будет найден
    последний цикл из которого была вызвано требование открыть следующий файл.
    Если в этот момент циклов не было, то флаг будет снят немедленно при
    открытии выходящего файла.
    }
    FEnteringInNextPage: Boolean;
    FExecuting: Boolean;
{$ifdef ELMLGEN_ACTIVEX}
    FIcon: TBitmap;
{$endif}
    FIfNames: array of string;
    {{
    Указатель на начало циклов
    }
    FLoopBeginPos: array of integer;
    {{
    Указатель на начало циклов
    }
    FLoopBeginPosRzrv: array of integer;
    {{
    Если не пустая строка, то это означает, что мы пропускаем все до конца
    цикла.
    }
    FLoopBreak: string;
    {{
    Счетчики циклов
    }
    FLoopCounters: array of integer;
    {{
    Счетчики циклов
    }
    FLoopCountersCurrentPage: array of integer;
    {{
    Счетчики циклов
    }
    FLoopCountersRzrv: array of integer;
    FLoopNames: array of string;
    FLoopNamesRzrv: array of string;
    FNewPageProcessing: Boolean;
    FPageCount: Integer;
    FParameters: TStringParameters;
    FSource: string;
    FSrcPos: Integer;
    FTagParameters: TStringParameters;
    FTagPrefix: string;
    FTemplate: TStrings;
    FTranslationTable: TTranslationTable;
    FTranslationTables: TTranslationTables;
    procedure CheckEndPos;
    function DoCodeBegin: Boolean;
    procedure DoCodeEnd;
    function DoCommentBegin: Boolean;
    function DoCommentEnd: Boolean;
    {{
    Возвращает истину, если обнаруженный тэг является концом последнего
    обрабатываемого if'а.
    }
    function DoIfDone(const SkipUnknown : boolean): Boolean;
    function DoIfElse: Boolean;
    function DoIfFound: Boolean;
    function DoIsTag(const TagName : string): Boolean;
    {{
    Разбирает тэг макроса. При вызове переменная SrcPos установлена на начало 
    '<macro' В конце работы должна быть установлена на следующий символ после 
    '>'.
    }
    function DoMacroFound: string;
    procedure DoNextPage;
    function DoParamBegin(var ParamName : string; var ParamNumb : integer): 
            Boolean;
    procedure DoParamEnd;
    function DoParamsBegin: Boolean;
    procedure DoParamsEnd;
    procedure DoRepeatBegin;
    procedure DoRepeatDone;
    function DoRepeatSkipDone: Boolean;
    function DoReplaceBegin(var ParamName : string; var ParamNumb : integer):
            Boolean;
    procedure DoReplaceEnd;
    procedure DoTagFound(TagName : string);
    procedure DoTranslationBegin(var TableName : string);
    procedure DoTranslationEnd(var TableName : string);
    procedure FreeArrays;
    procedure FreeRzrvArrays;
    function GetLoopCount: Integer;
    function GetLoopCounter(Index: Integer): Integer;
    function GetLoopCountersCurrentPage(Index: Integer): Integer;
    function GetLoopCountersCurrentPageStr(LoopName : string): Integer;
    function GetLoopCounterStr(LoopName : string): Integer;
    function GetLoopName(Index: Integer): string;
    {{
    При вызове метода считается,что мы находимся внутри тега, и нам предстоит 
    разобрать, что именно за параметры у этого тэга. Если параметров больше не 
    найдено, возвращаем пустые строки.
    }
    procedure GetTagProp(var ClosedTag : boolean; TagOptions : 
            TStringParameters);
    procedure PrepareTemplate;
    procedure SetCustomTagNames(Value: TStrings);
    procedure SetTagPrefix(const Value: string);
    procedure SetTemplate(Value: TStrings);
    procedure TemplateChanged(Sender: TObject);
  protected
    procedure AfterExecute; virtual; abstract;
    procedure BeforeExecute; virtual; abstract;
    {{
    Calculates the part to be processed
    }
    procedure IfFound(IfName : string; TagParameters : TStringParameters; var 
            ResultValue : boolean); virtual; abstract;
    procedure IsTag(TagName : string; var IsTag : boolean); virtual;
    {{
    One of the loops starts new iteration. The user can interrupt loop execution
    }
    procedure LoopIteration(LoopNumb: integer; LoopName: string; TagParameters 
            : TStringParameters; var LoopDone : boolean); virtual; abstract;
    {{
    Macro detected
    }
    procedure MacroFound(MacroName : string; TagParameters : TStringParameters; 
            var MacroResult : string; var UseTranslationTable : boolean); 
            virtual; abstract;
    procedure PageBegin(PageNumb : integer); virtual; abstract;
    procedure PageEnd(PageNumb : integer); virtual; abstract;
{$ifdef ELMLGEN_ACTIVEX}
    procedure Paint; override;
    procedure Resize; override;
{$endif}
    procedure ProcessMessages; virtual;
    procedure TagFound(Tag : string; const TagClosed : boolean; TagParameters : 
            TStringParameters); virtual;
    procedure WriteString(Value : string); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(AOwner: TBaseElMLGen);
    destructor Destroy; override;
    procedure Abort; virtual;
    {{
    Starts template processing.
    }
    procedure Execute; virtual;
    procedure Assign(Source: TPersistent); override;
    function GetLoopIndex(const LoopName : string): Integer;
    procedure LoopBreak(LoopName : string);
    procedure LoopContinue(LoopName : string);
    procedure NextPage;
    property Code: TStrings read FCode;
    property CustomTagNames: TStrings read FCustomTagNames write 
            SetCustomTagNames;
    property Executing: Boolean read FExecuting;
    {{
    The number of loops that we are currently "in"
    }
    property LoopCount: Integer read GetLoopCount;
    {{
    Provides access to loop iteration counters. The oughtermost loop has index
    0.
    }
    property LoopCounter[Index: Integer]: Integer read GetLoopCounter;
    {{
    Provides access to loop iteration counters that count iterations on the
    current page. The oughtermost loop has index 0.
    }
    property LoopCountersCurrentPage[Index: Integer]: Integer read 
            GetLoopCountersCurrentPage;
    property LoopCountersCurrentPageStr[LoopName : string]: Integer read 
            GetLoopCountersCurrentPageStr;
    property LoopCounterStr[LoopName : string]: Integer read GetLoopCounterStr;
    property LoopName[Index: Integer]: string read GetLoopName;
    property PageCount: Integer read FPageCount;
    property Parameters: TStringParameters read FParameters;
    property TagPrefix: string read FTagPrefix write SetTagPrefix;
    property Template: TStrings read FTemplate write SetTemplate;
    property TranslationTables: TTranslationTables read FTranslationTables;
  end;
  
  TCustomElMLGen = class (TBaseElMLGen)
  private
    FOnAfterExecute: TNotifyEvent;
    FOnBeforeExecute: TNotifyEvent;
    FOnIfFound: TMlIfFoundEvent;
    FOnIsTag: TMlIsTagEvent;
    FOnLoopIteration: TMlLoopIterateEvent;
    FOnMacroFound: TMlMacroFoundEvent;
    FOnPageBegin: TMlPageEvent;
    FOnPageEnd: TMlPageEvent;
    FOnProcessMessages: TNotifyEvent;
    FOnTagFound: TMlTagFoundEvent;
    FOnWriteString: TMlWriteStringEvent;
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    procedure IfFound(IfName : string; TagParameters : TStringParameters; var 
            ResultValue : boolean); override;
    procedure IsTag(TagName : string; var IsTag : boolean); override;
    procedure LoopIteration(LoopNumb: integer; LoopName: string; TagParameters 
            : TStringParameters; var LoopDone : boolean); override;
    procedure MacroFound(MacroName : string; TagParameters : TStringParameters; 
            var MacroResult : string; var UseTranslationTable : boolean); 
            override;
    procedure PageBegin(PageNumb : integer); override;
    procedure PageEnd(PageNumb : integer); override;
    procedure ProcessMessages; override;
    procedure TagFound(Tag : string; const TagClosed : boolean; TagParameters : 
            TStringParameters); override;
    procedure WriteString(Value : string); override;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write 
            FOnAfterExecute;
    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write 
            FOnBeforeExecute;
    property OnIfFound: TMlIfFoundEvent read FOnIfFound write FOnIfFound;
    property OnIsTag: TMlIsTagEvent read FOnIsTag write FOnIsTag;
    property OnLoopIteration: TMlLoopIterateEvent read FOnLoopIteration write 
            FOnLoopIteration;
    property OnMacroFound: TMlMacroFoundEvent read FOnMacroFound write
            FOnMacroFound;
    property OnPageBegin: TMlPageEvent read FOnPageBegin write FOnPageBegin;
    property OnPageEnd: TMlPageEvent read FOnPageEnd write FOnPageEnd;
    property OnProcessMessages: TNotifyEvent read FOnProcessMessages write 
            FOnProcessMessages;
    property OnTagFound: TMlTagFoundEvent read FOnTagFound write FOnTagFound;
    property OnWriteString: TMlWriteStringEvent read FOnWriteString write 
            FOnWriteString;
  end;
  
  TElMLGen = class (TCustomElMLGen)
  published
    property CustomTagNames;
    property OnAfterExecute;
    property OnBeforeExecute;
    property OnIfFound;
    property OnIsTag;
    property OnLoopIteration;
    property OnMacroFound;
    property OnPageBegin;
    property OnPageEnd;
    property OnProcessMessages;
    property OnTagFound;
    property OnWriteString;
    property TagPrefix;
    property Template;
  end;
  
  MlGenException = class (Exception)
  private
    FMlGen: TBaseElMLGen;
  public
    constructor Create(MlGen : TBaseElMlGen; Msg: string);
  end;
  
  procedure Register;

{$ifndef HAS_ALSTRTOOLS}
function CheckPath (InpPath : string; IsPath : boolean) : string;
function SetStrWidth(Value : string; Width : integer) : string;
function GetWordEx(S : string; var CharNumb : integer; Breaks : TSysCharSet) : string;
function PosP (const SubStr :string; Value : string; BegPos, EndPos : integer) : integer;
function GetPosOfNewString(const Value : string; CurPos : Integer) : Integer;
procedure GetTextLineByStringPos(const Value : string; ValuePos: integer; var Line,
  CharPos: Integer);
{$endif}

resourcestring
  SCanNotFindValueNameS = 'Can not find ValueName "%s"';
  SDuplicateTranslationTableName = 'Duplicate translation table name.';
  SCanNotSetDefaultTranslationTableIndexOutOfRange = 'Can not set default Translation Table. Index out of range.';
  SNameOfCommentTagMustBeSpecified = 'Name of ''comment'' tag must be specified.';
  SErrorInCommentTagDeclaration = 'Error in ''/comment'' tag declaration.';
  SConditionWasNotDeclaredOrClosingTagForInnerConditionIsSkipped = 'Condition was not declared, or closing tag for inner condition is skipped.';
  SErrorInElseTagDeclaration = 'Error in ''else'' tag declaration.';
  SNameOfElseTagMustBeSpecified = 'Name of ''else'' tag must be specified.';
  SErrorInIfTagDeclaration = 'Error in ''if'' tag declaration.';
  SNameOfIfTagMustBeSpecified = 'Name of ''if'' tag must be specified.';
  SErrorInMacroDeclaration = 'Error in macro declaration.';
  SNameOfMacroMustBeSpecified = 'Name of macro must be specified.';
  SNameOfParamTagMustBeSpecified = 'Name of ''param'' tag must be specified.';
  SErrorInParamTagDeclaration1 = 'Error in ''/param'' tag declaration.';
  SErrorInParamsTagDeclaration1 = 'Error in ''/params'' tag declaration.';
  SErrorInRepeatTagDeclaration = 'Error in ''repeat'' tag declaration.';
  SNameOfRepeatTagMustBeSpecified = 'Name of ''repeat'' tag must be specified.';
  SDuplicateLoopNameS = 'Duplicate loop name ''%s''.';
  STryingToCloseNotOpenedLoopInternalError = 'Trying to close not opened loop. Internal error.';
  SStringInReplaceTagMustBeSpecified = 'String in ''replace'' tag must be specified.';
  SEmbeddedTranslationTablesAreNotAllowed = 'Embedded translation tables are not allowed.';
  SNameOfTranslationTagMustBeSpecified = 'Name of ''translation'' tag must be specified.';
  SErrorInTranslationTagDeclaration1 = 'Error in ''/translation'' tag declaration.';
  SNameOfTranslationTagMustBeSpecified1 = 'Name of ''/translation'' tag must be specified.';
  STryingToCloseNotPreviouslyOpenedTranslationTableSS = 'Trying to close not previously opened translation table "%s". Ожидается закрытие "%s".';
  SErrorInTemplateDLoopsWereNotClosed = 'Error in template: %d loops were not closed.';
  SErrorInTemplateDConditionsWereNotClosed = 'Error in template: %d conditions were not closed.';
  SErrorInTemplateNotAllTagsHaveBeenClosed = 'Error in template. Not all tags have been closed.';
  SCanNotAnalyzeTag = 'Can not analyze tag';
  SAttemptToCloseTheLoopThatHasNotBeenOpenedBefore = 'Attempt to close the loop that has not been opened before.';
  SRepeatedRequestForNewPageIsNotAllowed = 'Repeated request for new page is not allowed.';
  SErrorInTemplateParamsDeclaration = 'Error in template params declaration.';
  SInvalidCharDSInTagPrefixString = 'Invalid char %d (%s) in tag prefix string';
  SCanTChangeTemplateDuringExecution = 'Can''t change template during execution.';
  SErrorInIfTagDeclaration1 = 'Error in ''/if'' tag declaration.';
  SNameOfIfTagMustBeSpecified1 = 'Name of ''/if'' tag must be specified.';
  SErrorInRepeatTagDeclaration1 = 'Error in ''/repeat'' tag declaration.';
  SRepeatTagDoesnTHaveMatchingRepeatTag1 = '''/repeat'' tag doesn''t have matching ''repeat'' tag';
  SErrorInReplaceTagDeclaration1 = 'Error in ''/replace'' tag declaration.';
  SErrorWhileDetectingPositionInLineIndexOutOfRange = 'Error while detecting position in line. Index (%d) out of range (%d).';
  SCanNotAssignObjectToTranslationTableList = 'Can not assign object to translation table list';
  SIndexOutOfRange = 'Index out of range.';
  SLoopNameIsNotFound = 'Loop name is not found.';
  SErrorInCodeTagDeclaration = 'Error in ''/code'' tag declaration';
  SLineChar = '%s Line: %d, Char: %d.';
  SError = 'Error.';
  SCanNotAssignToTranslationTable = 'Can not assign to Translation Table.';
  SUnableToFindPointToReturn = 'Unable to find point to return.';
  SUnableToFindPointToReturnPossibleReasonIsDuplicatedLoopName = 'Unable to find point to return. Possible reason is duplicated loop name.';

implementation
{$ifdef HAS_ALSTRTOOLS}
uses AlStrTools;
{$endif}

procedure Register;
begin
  RegisterComponents('EldoS', [TElMLGen]);
end;


{$ifndef HAS_ALSTRTOOLS}
{------------------------------------------------------------------------------}
// проверка и модификация пути
function CheckPath (InpPath : string; IsPath : boolean) : string;
label l1;
var
  p, l : integer;
begin
//1. Проверяем на отсутствие двойных косых черточек \\
  Result := InpPath;
  if Result = '' then Exit;
l1:
  p := Pos('\\', InpPath);
  if p <> 0 then begin
    Result := Copy(InpPath, 1, p);
    l := Length(InpPath);
    if p+1 < l then Result := Result + Copy(InpPath, p+2, l-p-1);
    InpPath := Result;
    goto l1;
  end;

//2. Проверяем и удаляем кавычки
  if Result[1] = '"' then Result := Copy(Result, 2, Length(Result)-1);
  l := Length(Result);
  if Result[l] = '"' then Result := Copy(Result, 1, l-1);

//3. Если это путь, то проверяем на наличие в конце косой черты \
  if IsPath then begin
    l := Length(Result);
    if l <> 0 then if Result[l] <> '\' then Result := Result + '\';
  end;
end;


{------------------------------------------------------------------------------}
// выделение слова
function GetWordEx(S : string; var CharNumb : integer; Breaks : TSysCharSet) : string;
var
  Len : integer;
begin
  Result := '';
  Len := Length(s);
  if CharNumb > Len then
    exit;
  while CharNumb <= Len do
    if not (s[CharNumb] in Breaks) then
      break
    else
      inc(CharNumb);
  if CharNumb > Len then exit;
  while CharNumb <= Len do begin
    if s[CharNumb] in Breaks then
      break;
    Result := Result + S[CharNumb];
    inc(CharNumb);
  end;
  inc(CharNumb);
end;

{------------------------------------------------------------------------------}
// Поиск подстроки в строке начиная с символа номер N
function PosP (const SubStr :string; Value : string; BegPos, EndPos : integer) : integer;
begin
  if EndPos <= 0 then
    EndPos := Length(Value);
  Value := Copy(Value, BegPos, EndPos-BegPos+1);
  Result := Pos(SubStr, Value);
  if Result > 0 then
    inc(Result, BegPos - 1);
end;


{------------------------------------------------------------------------------}
// возвращает номер позиции с которой начинается следующая строка.
function GetPosOfNewString(const Value : string; CurPos : Integer) : Integer;
var
  EndPos, i : Integer;
  // EndStrType:
  // 0 еще не известно
  // 1 #13 - Unix
  // 2 #10
  // 3 #13#10 - Windows
  // 4 #10#13 - Mac
  EndStrType : Integer;
begin
  Result := -1;
  EndPos := Length(Value);
  EndStrType := 0; //
  for i := CurPos to EndPos do begin
    case Value[i] of
      #13 :
        case EndStrType of
          0 : EndStrType := 1;
          1, 3, 4 :
            begin
              Result := i;
              break;
            end;
          2 : EndStrType := 4;
        end;    // case

      #10 :
        case EndStrType of
          0 : EndStrType := 2;
          1 : EndStrType := 3;
          2, 3, 4 :
            begin
              Result := i;
              break;
            end;
        end;    // case

      else
        case EndStrType of
          1, 2, 3, 4 :
            begin
              Result := i;
              break;
            end;
        end;    // case
    end;    // case
  end; // for
end;

{------------------------------------------------------------------------------}
// Устанавливает у строки длину в символах, обрезает лишнее добавляя '...' если текст
// не влазит, добавляет пробелами если строка слишком короткая
function SetStrWidth(Value : string; Width : integer) : string;
var
  i, j : Integer;
begin
  j := Length(Value);
  Result := Value;
  if j < Width then
  begin
    SetLength(Result, Width);
    FillChar(Result[j+1], Width-j, #32);
  end
  else
  if j > Width then
  begin
    SetLength(Result, Width);
    for i := Width - 2 to Width do
      Result[i] := '.';
  end;
end;

{------------------------------------------------------------------------------}
procedure GetTextLineByStringPos(const Value : string; ValuePos: integer; var Line,
  CharPos: Integer);
var
  SrcPos, SrcLen : Integer;
begin
  SrcLen := Length(Value);
  if ValuePos > SrcLen then
    Raise Exception.Create(Format(
      SErrorWhileDetectingPositionInLineIndexOutOfRange, [ValuePos, SrcLen]));
  SrcPos := 1;
  Line := 1;
  CharPos := 1;
  while SrcPos <= SrcLen do
  begin
    if SrcPos >= ValuePos then
      break;
    case Value[SrcPos] of
      #10, #13 :
      begin
        SrcPos := GetPosOfNewString(Value, SrcPos);
        inc(Line);
        CharPos := 1;
      end;
      else begin
        inc(CharPos);
        inc(SrcPos);
      end;
    end;    // case
  end;    // while
end;
{$endif}

{
****************************** TStringParameters *******************************
}
constructor TStringParameters.Create;
begin
  inherited Create;
end;{TStringParameters.Create}

destructor TStringParameters.Destroy;
begin
  FData := nil;
  inherited Destroy;
end;{TStringParameters.Destroy}

function TStringParameters.AddValue(const aValueName, aValue : string): Integer;
begin
  Result := FindItemByName(aValueName);
  if Result < 0 then
  begin
    Result := Length(FData);
    SetLength(FData, Result + 1);
    with FData[Result] do
    begin
      ValueName := aValueName;
      Value := aValue;
    end;  // with
  end
  else begin
    with FData[Result] do
      Value := Value + aValue;
  end;
end;{TStringParameters.AddValue}

procedure TStringParameters.Assign(Source: TPersistent);
var
  i, j: Integer;
begin
  if Source is TStringParameters then
  begin
    j := Length(TStringParameters(Source).FData);
    SetLength(FData, j);
    for i := 0 to j - 1 do
    begin
      with TStringParameters(Source).FData[i] do
      begin
        FData[i].Value := Value;
        FData[i].ValueName := ValueName;
      end;    // with
    end; // for
  end
  else
    inherited;
end;{TStringParameters.Assign}

procedure TStringParameters.AssignTo(Dest: TPersistent);
var
  i: Integer;
begin
  if Dest is TStringParameters then
    Dest.Assign(self)
  else
    if Dest is TStrings then
    begin
      TStrings(Dest).Clear;
      if Length(FData) > 0 then
        for i := 0 to High(FData) do
        begin
          TStrings(Dest).Add(Format('%s=%s', [FData[i].ValueName, FData[i].Value]));
        end; // for
    end
    else
      inherited;
end;{TStringParameters.AssignTo}

procedure TStringParameters.Clear;
begin
  FData := nil;
end;{TStringParameters.Clear}

function TStringParameters.FindItemByName(ValueName : string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FData) do
  begin
    if FData[i].ValueName = ValueName then
    begin
      Result := i;
      break;
    end;
  end; // for
end;{TStringParameters.FindItemByName}

function TStringParameters.GetCount: Integer;
begin
  Result := Length(FData);
end;{TStringParameters.GetCount}

function TStringParameters.GetValue(Index: Integer): string;
begin
  Result := FData[Index].Value;
end;{TStringParameters.GetValue}

function TStringParameters.GetValueByName(ValueName : string): string;
var
  i: Integer;
begin
  i := FindItemByName(ValueName);
  if i < 0 then
    Raise Exception.Create(Format(SCanNotFindValueNameS, [ValueName]));
  Result := FData[i].Value;
end;{TStringParameters.GetValueByName}

function TStringParameters.GetValueByNameEx(ValueName, DefaultValue : string): 
        string;
var
  i: Integer;
begin
  i := FindItemByName(ValueName);
  if i >= 0 then
    Result := FData[i].Value
  else
    Result := DefaultValue;
end;{TStringParameters.GetValueByNameEx}

function TStringParameters.GetValueName(Index: Integer): string;
begin
  Result := FData[Index].ValueName;
end;{TStringParameters.GetValueName}

procedure TStringParameters.SetCount(Value: Integer);
begin
  SetLength(FData, Value);
end;{TStringParameters.SetCount}

procedure TStringParameters.SetValue(Index: Integer; const Value: string);
begin
  FData[Index].Value := Value;
end;{TStringParameters.SetValue}

procedure TStringParameters.SetValueByName(ValueName : string; const Value: 
        string);
var
  i: Integer;
begin
  i := FindItemByName(ValueName);
  if i < 0 then
    Raise Exception.Create(Format(SCanNotFindValueNameS, [ValueName]));
  FData[i].Value := Value;
end;{TStringParameters.SetValueByName}

procedure TStringParameters.SetValueName(Index: Integer; const Value: string);
begin
  FData[Index].ValueName := Value;
end;{TStringParameters.SetValueName}

{
****************************** TTranslationTables ******************************
}
constructor TTranslationTables.Create;
begin
  inherited Create(TTranslationTable);
  FDefaultTable := -1;
end;{TTranslationTables.Create}

function TTranslationTables.Add(const TableName : string): TTranslationTable;
var
  i: Integer;
begin
  i := FindTableByName(TableName);
  if i >= 0 then
    Raise Exception.Create(SDuplicateTranslationTableName);
  Result := TTranslationTable(inherited Add);
  Result.Name := TableName;
end;{TTranslationTables.Add}

procedure TTranslationTables.Assign(Source: TPersistent);
begin
  if Source is TTranslationTables then
  begin
    inherited Assign(Source);
    DefaultTable := TTranslationTables(Source).DefaultTable;
  end
  else
    Raise Exception.Create(SCanNotAssignObjectToTranslationTableList);
end;{TTranslationTables.Assign}

procedure TTranslationTables.Clear;
begin
  DefaultTable := -1;
  inherited Clear;
end;{TTranslationTables.Clear}

function TTranslationTables.FindTableByName(const TableName : string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Name = TableName then
    begin
      Result := i;
      break;
    end;
  end; // for
end;{TTranslationTables.FindTableByName}

function TTranslationTables.GetItems(Index: Integer): TTranslationTable;
begin
  Result := TTranslationTable(inherited GetItem(Index));
end;{TTranslationTables.GetItems}

procedure TTranslationTables.GetTableNames(Strings : TStrings);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Strings.Add(Items[i].Name);
  end; // for
end;{TTranslationTables.GetTableNames}

procedure TTranslationTables.SetDefaultTable(Value: Integer);
begin
  if FDefaultTable <> Value then
  begin
    if Value >= Count then
      Raise Exception.Create(SCanNotSetDefaultTranslationTableIndexOutOfRange);
    FDefaultTable := Value;
  end;
end;{TTranslationTables.SetDefaultTable}

procedure TTranslationTables.SetItems(Index: Integer; Value: TTranslationTable);
begin
  inherited SetItem(Index, Value);
end;{TTranslationTables.SetItems}

{
****************************** TTranslationTable *******************************
}
constructor TTranslationTable.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  // FName := '';
  FTable := TStringParameters.Create;
end;{TTranslationTable.Create}

destructor TTranslationTable.Destroy;
begin
  FTable.Free;
  inherited Destroy;
end;{TTranslationTable.Destroy}

procedure TTranslationTable.Assign(Source: TPersistent);
begin
  if Source is TTranslationTable then
  begin
    FName := TTranslationTable(Source).Name;
    FTable.Assign(TTranslationTable(Source).Table);
  end
  else
    Raise Exception.Create(SCanNotAssignToTranslationTable);
end;{TTranslationTable.Assign}

function TTranslationTable.Translate(Source : string): string;
var
  i, j, k, SrcPos, SrcLen, ReplCount: Integer;
  s: string;
  Replace: Boolean;
begin
  Result := '';
  SrcLen := Length(Source);
  SrcPos := 1;
  ReplCount := High(FTable.FData);
  while SrcPos <= SrcLen do
  begin
    Replace := False;
    for i := 0 to ReplCount do
    begin
      s := FTable.FData[i].ValueName;
      k := Length(s);
      if (SrcPos + k - 1) <= SrcLen then
      begin
        Replace := True;
        for j := 1 to k do
        begin
          if Source[SrcPos + j - 1] <> s[j] then
          begin
            Replace := False;
            break;
          end;
        end; // for
        if Replace then
        begin
          Result := Result + FTable.FData[i].Value;
          inc(SrcPos, k);
          break;
        end;
      end;
    end; // for
    if not Replace then
    begin
      Result := Result + Source[SrcPos];
      inc(SrcPos);
    end;
  end;    // while
end;{TTranslationTable.Translate}

{
********************************* TBaseElMLGen *********************************
}
constructor TBaseElMLGen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$ifdef ELMLGEN_ACTIVEX}
  FIcon := TBitmap.Create;
  FIcon.LoadFromResourceName(Hinstance, 'TELMLGEN');
  {$endif}
  // FExecuting := False;
  // FNewPageProcessing := False;
  // FEnteringInNextPage := False;
  // FSource := '';
  // FCommentName := '';
  // FTagPrefix := '';
  // FLoopBreak := '';
  // FTranslationTable := nil;
  FTemplate := TStringList.Create;
  FCode := TStringList.Create;
  TStringList(FTemplate).OnChange := TemplateChanged;
  FCustomTagNames := TStringList.Create;
  FParameters := TStringParameters.Create;
  FTranslationTables := TTranslationTables.Create;
  FTagParameters := TStringParameters.Create;
end;{TBaseElMLGen.Create}

constructor TBaseElMLGen.CreateFrom(AOwner: TBaseElMLGen);
begin
  Create(AOwner);
  FCode.Assign(AOwner.FCode);
  FCommentName := AOwner.FCommentName;
  FCustomTagNames.Assign(AOwner.FCustomTagNames);
  FParameters.Assign(AOwner.FParameters);
  FTagParameters.Assign(AOwner.FTagParameters);
  FTagPrefix := AOwner.FTagPrefix;
  FTemplate.Assign(AOwner.FTemplate);
  FTranslationTables.Assign(AOwner.FTranslationTables);
end;

destructor TBaseElMLGen.Destroy;
begin
  FreeArrays;
  FreeRzrvArrays;
  FTagParameters.Free;
  FTranslationTables.Free;
  FParameters.Free;
  TStringList(FTemplate).OnChange := nil;
  FTemplate.Free;
  FCode.Free;
  FCustomTagNames.Free;
  {$ifdef ELMLGEN_ACTIVEX}
  FIcon.Free;
  {$endif}
  inherited Destroy;
end;{TBaseElMLGen.Destroy}

procedure TBaseElMLGen.Abort;
begin
  FCancelJob := True;
end;{TBaseElMLGen.Abort}

procedure TBaseElMLGen.Assign(Source: TPersistent);
begin
  Raise Exception.Create('TBaseElMLGen.Assign: Can not assign unknown object.');
end;{TLinksMLGen.Assign}

procedure TBaseElMLGen.CheckEndPos;
var
  j, k, SrcLen: Integer;
begin
  // ищем конец строки в темплейте
  SrcLen := Length(FSource);
  j := FSrcPos; // Указатель на следующий символ после макроса
  while j <= SrcLen do begin
    case FSource[j] of
      #9, ' ' : ;
      #10, #13 :
        begin
          k := GetPosOfNewString(FSource, j);
          if k > 0 then
            j := k
          else
            j := SrcLen + 1;
          break;
        end;
      else
        exit; // надо записать все. (Хотя по хорошему, то что мы нашли может быть началом нового макроса)
    end;    // case
    inc(j);
  end;    // while
  
  // Теперь мы точно знаем, что строку писать не надо. Вырежем ее.
  FSrcPos := j;
end;{TBaseElMLGen.CheckEndPos}

function TBaseElMLGen.DoCodeBegin: Boolean;
var
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 5{Length('<code')};
  GetTagProp(ClosedTag, FTagParameters);
  Result := not ClosedTag;
end;{TBaseElMLGen.DoCodeBegin}

procedure TBaseElMLGen.DoCodeEnd;
var
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 6{Length('</code')};
  GetTagProp(ClosedTag, FTagParameters);
  if ClosedTag then
    Raise MlGenException.Create(self, SErrorInCodeTagDeclaration);
end;{TBaseElMLGen.DoCodeEnd}

function TBaseElMLGen.DoCommentBegin: Boolean;
var
  j: Integer;
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 8{Length('<comment')};
  GetTagProp(ClosedTag, FTagParameters);
  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array
  if j < 0 then
    Raise MlGenException.Create(self, SNameOfCommentTagMustBeSpecified);
  if FTagParameters.FData[j].Value = '' then
    Raise MlGenException.Create(self, SNameOfCommentTagMustBeSpecified);
  
  Result := not ClosedTag;
  if Result then
    FCommentName := FTagParameters.FData[j].Value;
end;{TBaseElMLGen.DoCommentBegin}

function TBaseElMLGen.DoCommentEnd: Boolean;
var
  j: Integer;
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 9{Length('</comment')};
  GetTagProp(ClosedTag, FTagParameters);
  if ClosedTag then
    Raise MlGenException.Create(self, SErrorInCommentTagDeclaration);
  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array
  //  if j < 0 then
  //    CreateException('Error in tag ''/comment'' declaration.');
  //  if TagPropValue[j] = '' then
  //    CreateException('Name of tag ''/comment'' must be specified.');
  
  if j >= 0 then
    Result := FCommentName = FTagParameters.FData[j].Value
  else
    Result := False;
  if Result then
    FCommentName := '';
end;{TBaseElMLGen.DoCommentEnd}

{{
Возвращает истину, если обнаруженный тэг является концом последнего 
обрабатываемого if'а.
}
function TBaseElMLGen.DoIfDone(const SkipUnknown : boolean): Boolean;
var
  i, j: Integer;
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 4{Length('</if')};
  GetTagProp(ClosedTag, FTagParameters);
  if ClosedTag then
    Raise MlGenException.Create(self, SErrorInIfTagDeclaration1);
  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array
  if j < 0 then
    Raise MlGenException.Create(self, SNameOfIfTagMustBeSpecified1);
  if FTagParameters.FData[j].Value = '' then
    Raise MlGenException.Create(self, SNameOfIfTagMustBeSpecified1);

  i := Length(FIfNames) - 1;
  Result := FIfNames[i] = FTagParameters.FData[j].Value;
  if (not Result) and (not SkipUnknown) then
    Raise MlGenException.Create(self, SConditionWasNotDeclaredOrClosingTagForInnerConditionIsSkipped);

  if Result then begin
    FIfNames[i] := '';
    SetLength(FIfNames, i);
  end;
end;{TBaseElMLGen.DoIfDone}

function TBaseElMLGen.DoIfElse: Boolean;
var
  i, j: Integer;
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 5{Length('<else')};
  GetTagProp(ClosedTag, FTagParameters);
  if not ClosedTag then
    Raise MlGenException.Create(self, SErrorInElseTagDeclaration);
  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array
  if j < 0 then
    Raise MlGenException.Create(self, SNameOfElseTagMustBeSpecified);
  if FTagParameters.FData[j].Value = '' then
    Raise MlGenException.Create(self, SNameOfElseTagMustBeSpecified);
  
  i := Length(FIfNames) - 1;
  Result := FIfNames[i] = FTagParameters.FData[j].Value;
end;{TBaseElMLGen.DoIfElse}

function TBaseElMLGen.DoIfFound: Boolean;
var
  i, j: Integer;
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 3{Length('<if')};
  GetTagProp(ClosedTag, FTagParameters);
  if ClosedTag then
    Raise MlGenException.Create(self, SErrorInIfTagDeclaration);
  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array
  if j < 0 then
    Raise MlGenException.Create(self, SNameOfIfTagMustBeSpecified);
  if FTagParameters.FData[j].Value = '' then
    Raise MlGenException.Create(self, SNameOfIfTagMustBeSpecified);
  
  Result := True;
  IfFound(FTagParameters.FData[j].Value, FTagParameters, Result);
  
  i := Length(FIfNames);
  SetLength(FIfNames, i + 1);
  FIfNames[i] := FTagParameters.FData[j].Value;
end;{TBaseElMLGen.DoIfFound}

function TBaseElMLGen.DoIsTag(const TagName : string): Boolean;
begin
  Result := False;
  IsTag(TagName, Result);
end;{TBaseElMLGen.DoIsTag}

{{
Разбирает тэг макроса. При вызове переменная SrcPos установлена на начало 
'<macro' В конце работы должна быть установлена на следующий символ после 
'>'.
}
function TBaseElMLGen.DoMacroFound: string;
var
  j: Integer;
  ClosedTag: Boolean;
  UseTranslationTable: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 6{Length('<macro')};
  GetTagProp(ClosedTag, FTagParameters);
  if not ClosedTag then
    Raise MlGenException.Create(self, SErrorInMacroDeclaration);
  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array
  if j < 0 then
    Raise MlGenException.Create(self, SNameOfMacroMustBeSpecified);
  if FTagParameters.FData[j].Value = '' then
    Raise MlGenException.Create(self, SNameOfMacroMustBeSpecified);
  
  UseTranslationTable := True;
  Result := '';
  MacroFound(FTagParameters.FData[j].Value, FTagParameters, Result, UseTranslationTable);
  
  if UseTranslationTable and (FTranslationTables.DefaultTable >= 0) then
  begin
    Result := FTranslationTables.Items[FTranslationTables.DefaultTable].Translate(Result);
  end;
end;{TBaseElMLGen.DoMacroFound}

procedure TBaseElMLGen.DoNextPage;
begin
  FNewPageProcessing := False;
  if Length(FLoopCountersRzrv) <> 0 then
    FEnteringInNextPage := True;
  inc(FPageCount);
  FreeArrays;
  PageBegin(PageCount);
end;{TBaseElMLGen.DoNextPage}

function TBaseElMLGen.DoParamBegin(var ParamName : string; var ParamNumb : 
        integer): Boolean;
var
  ClosedTag: Boolean;
  j: Integer;
  ParamValue: string;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 6{Length('<param')};
  GetTagProp(ClosedTag, FTagParameters);
  Result := not ClosedTag;
  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array
  if j < 0 then
    Raise MlGenException.Create(self, SNameOfParamTagMustBeSpecified);
  if FTagParameters.FData[j].Value = '' then
    Raise MlGenException.Create(self, SNameOfParamTagMustBeSpecified);
  ParamName := FTagParameters.FData[j].Value;
  j := FTagParameters.FindItemByName('value'); // Номер параметра содержащего значение
  if j >= 0 then
    ParamValue := FTagParameters.FData[j].Value
  else
    ParamValue := '';
  ParamNumb := FParameters.AddValue(ParamName, ParamValue);
  if not Result then
  begin
    ParamName := '';
    ParamNumb := 0;
  end;
end;{TBaseElMLGen.DoParamBegin}

procedure TBaseElMLGen.DoParamEnd;
var
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 7{Length('</param')};
  GetTagProp(ClosedTag, FTagParameters);
  if ClosedTag then
    Raise MlGenException.Create(self, SErrorInParamTagDeclaration1);
end;{TBaseElMLGen.DoParamEnd}

function TBaseElMLGen.DoParamsBegin: Boolean;
var
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 7{Length('<params')};
  GetTagProp(ClosedTag, FTagParameters);
  Result := not ClosedTag;
end;{TBaseElMLGen.DoParamsBegin}

procedure TBaseElMLGen.DoParamsEnd;
var
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 8{Length('</params')};
  GetTagProp(ClosedTag, FTagParameters);
  if ClosedTag then
    Raise MlGenException.Create(self, SErrorInParamsTagDeclaration1);
end;{TBaseElMLGen.DoParamsEnd}

procedure TBaseElMLGen.DoRepeatBegin;
var
  i, j, k: Integer;
  s: string;
  ClosedTag: Boolean;
  b: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 7{Length('<repeat')};
  GetTagProp(ClosedTag, FTagParameters);
  if ClosedTag then
    Raise MlGenException.Create(self, SErrorInRepeatTagDeclaration);
  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array
  if j < 0 then
    Raise MlGenException.Create(self, SNameOfRepeatTagMustBeSpecified);
  s := FTagParameters.FData[j].Value;
  if s = '' then
    Raise MlGenException.Create(self, SNameOfRepeatTagMustBeSpecified);
  
  // Смотрим не было ли такого цикла ранее
  for i := 0 to High(FLoopNames) do
  begin
    if FLoopNames[i] = s then
    Raise MlGenException.Create(self, Format(SDuplicateLoopNameS, [s]));
  end; // for
  
  i := Length(FLoopCounters) + 1;
  SetLength(FLoopCounters, i);
  SetLength(FLoopNames, i);
  SetLength(FLoopBeginPos, i);
  SetLength(FLoopCountersCurrentPage, i);
  dec(i);
  
  FLoopNames[i] := s;
  FLoopCountersCurrentPage[i] := 0;
  if not FEnteringInNextPage then
  begin
    FLoopCounters[i] := 0;
  end
  else begin
    // Если мы ищем точку вхождения в новом файле
    if FLoopNamesRzrv[0] = FLoopNames[0] then
    begin
      // Мы уже нашли первый цикл, и это означает, что все последующие названия должны совпадать
      if FLoopNames[i] <> FLoopNamesRzrv[i] then
        Raise MlGenException.Create(self, SUnableToFindPointToReturn);
      FLoopCounters[i] := FLoopCountersRzrv[i];
      if Length(FLoopNames) = Length(FLoopNames) then
        FEnteringInNextPage := False;
    end
    else begin
      // Мы еще не дошли до первого цикла в котором находились на момент выхода. Это просто новый цикл.
      // Но этого имени не должно быть у нас в записях!
      s := FTagParameters.FData[j].Value;
      for k := 0 to High(FLoopNamesRzrv) do
      begin
        if s = FLoopNamesRzrv[k] then
          Raise MlGenException.Create(self, SUnableToFindPointToReturnPossibleReasonIsDuplicatedLoopName);
      end; // for
      FLoopCounters[i] := 0;
    end;
  end;
  
  FLoopBeginPos[i] := FSrcPos;
  
  b := False; // Цикл выполняется один раз.
  LoopIteration(i, FTagParameters.FData[j].Value, FTagParameters, b);
  
  if b then
  begin
    // Цикл завершен не начавшись
    i := High(FLoopCounters);
    FLoopBreak := FLoopNames[i];
  
{    SetLength(FLoopCounters, i);
    SetLength(FLoopCountersCurrentPage, i);
    SetLength(FLoopBeginPos, i);
    FLoopNames[i] := '';
    SetLength(FLoopNames, i);}
  end;
end;{TBaseElMLGen.DoRepeatBegin}

procedure TBaseElMLGen.DoRepeatDone;
var
  i, j: Integer;
  s: string;
  b: Boolean;
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 8{Length('</repeat')};
  GetTagProp(ClosedTag, FTagParameters);
  if ClosedTag then
    Raise MlGenException.Create(self, SErrorInRepeatTagDeclaration1);
  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array
  if j >= 0 then
  begin
    s := FTagParameters.FData[j].Value;
    i := GetLoopIndex(s);
    if i < 0 then
      Raise MlGenException.Create(self, SRepeatTagDoesnTHaveMatchingRepeatTag1);
  end
  else begin
    i := High(FLoopCounters);
    s := FLoopNames[i];
  end;
  
  // Вызов события о следующей итерации цикла должен происходить с новым значением итератора
  inc(FLoopCounters[i]);
  inc(FLoopCountersCurrentPage[i]);
  
  // b := not LoopIteration(i, s, TagProp, TagPropValue);
  b := True;
  LoopIteration(i, s, FTagParameters, b);
  
  if (not b) and FNewPageProcessing then
  begin
    // Если цикл находится в резервном списке, прекращаем его выполнение.
    for j := 0 to High(FLoopNamesRzrv) do
    begin
      if (FLoopNamesRzrv[j] = s) and
         (FLoopBeginPos[j] = FLoopBeginPosRzrv[j]) then
      begin
        b := True;
        break;
      end;
    end; // for
  end;
  
  if b then begin
    // Цикл завершен
    SetLength(FLoopCounters, i);
    SetLength(FLoopCountersCurrentPage, i);
    SetLength(FLoopBeginPos, i);
    SetLength(FLoopNames, i);
  end
  else begin
    // Цикл продолжается
  
    // Если один /repeat закрывает несколько циклов, то информацию о них надо уничтожить
    if i < High(FLoopNames) then
    begin
      j := i + 1;
      SetLength(FLoopCounters, j);
      SetLength(FLoopCountersCurrentPage, j);
      SetLength(FLoopBeginPos, j);
      SetLength(FLoopNames, j);
    end;
  
    FSrcPos := FLoopBeginPos[i];
  end;
end;{TBaseElMLGen.DoRepeatDone}

function TBaseElMLGen.DoRepeatSkipDone: Boolean;
var
  i, j: Integer;
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 8{Length('</repeat')};
  GetTagProp(ClosedTag, FTagParameters);
  if ClosedTag then
    Raise MlGenException.Create(self, SErrorInRepeatTagDeclaration1);

  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array

  if j >= 0 then
  begin
    Result := FLoopBreak = FTagParameters.FData[j].Value;
    if Result then
    begin
      // Цикл завершен
      i := GetLoopIndex(FLoopBreak);
      if i < 0 then
        Raise MlGenException.Create(self, STryingToCloseNotOpenedLoopInternalError);
      FLoopBreak := '';
      SetLength(FLoopCounters, i);
      SetLength(FLoopCountersCurrentPage, i);
      SetLength(FLoopBeginPos, i);
      SetLength(FLoopNames, i);
    end;
  end
  else
    Result := False; // это не наш цикл, мы дожидаемся закрытия другого цикла
end;{TBaseElMLGen.DoRepeatSkipDone}

function TBaseElMLGen.DoReplaceBegin(var ParamName : string; var ParamNumb : 
        integer): Boolean;
var
  ClosedTag: Boolean;
  j: Integer;
  ParamValue: string;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 8{Length('<replace')};
  GetTagProp(ClosedTag, FTagParameters);
  Result := not ClosedTag;
  j := FTagParameters.FindItemByName('string'); // Position of name parameter in the array
  if j < 0 then
    Raise MlGenException.Create(self, SStringInReplaceTagMustBeSpecified);
  if FTagParameters.FData[j].Value = '' then
    Raise MlGenException.Create(self, SStringInReplaceTagMustBeSpecified);
  
  ParamName := FTagParameters.FData[j].Value;
  j := FTagParameters.FindItemByName('with'); // Номер параметра содержащего значение
  if j >= 0 then
    ParamValue := FTagParameters.FData[j].Value
  else
    ParamValue := '';
  ParamNumb := FTranslationTable.Table.AddValue(ParamName, ParamValue);
  if not Result then
  begin
    ParamName := '';
    ParamNumb := 0;
  end;
end;{TBaseElMLGen.DoReplaceBegin}

procedure TBaseElMLGen.DoReplaceEnd;
var
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 9{Length('</replace')};
  GetTagProp(ClosedTag, FTagParameters);
  if ClosedTag then
    Raise MlGenException.Create(self, SErrorInReplaceTagDeclaration1);
end;{TBaseElMLGen.DoReplaceEnd}

procedure TBaseElMLGen.DoTagFound(TagName : string);
var
  ClosedTag: Boolean;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + Length(TagName) + 1;
  GetTagProp(ClosedTag, FTagParameters);
  TagFound(TagName, ClosedTag, FTagParameters);
end;{TBaseElMLGen.DoTagFound}

procedure TBaseElMLGen.DoTranslationBegin(var TableName : string);
var
  ClosedTag: Boolean;
  j: Integer;
begin
  if TableName <> '' then
    Raise MlGenException.Create(self, SEmbeddedTranslationTablesAreNotAllowed);
  FSrcPos := FSrcPos + Length(FTagPrefix) + 12{Length('<translation')};
  GetTagProp(ClosedTag, FTagParameters);
  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array
  if j < 0 then
    Raise MlGenException.Create(self, SNameOfTranslationTagMustBeSpecified);
  TableName := FTagParameters.FData[j].Value;
  if TableName = '' then
    Raise MlGenException.Create(self, SNameOfTranslationTagMustBeSpecified);
  
  FTranslationTable := FTranslationTables.Add(TableName);
  
  j := FTagParameters.FindItemByName('default');
  if j >= 0 then
    FTranslationTables.DefaultTable := FTranslationTable.Index;
  if ClosedTag then
  begin
    TableName := '';
    FTranslationTable := nil;
  end;
end;{TBaseElMLGen.DoTranslationBegin}

procedure TBaseElMLGen.DoTranslationEnd(var TableName : string);
var
  ClosedTag: Boolean;
  j: Integer;
  s: string;
begin
  FSrcPos := FSrcPos + Length(FTagPrefix) + 13{Length('</translation')};
  GetTagProp(ClosedTag, FTagParameters);
  if ClosedTag then
    Raise MlGenException.Create(self, SErrorInTranslationTagDeclaration1);
  j := FTagParameters.FindItemByName('name'); // Position of name parameter in the array
  if j < 0 then
    Raise MlGenException.Create(self, SNameOfTranslationTagMustBeSpecified1);
  s := FTagParameters.FData[j].Value;
  if s = '' then
    Raise MlGenException.Create(self, SNameOfTranslationTagMustBeSpecified1);
  
  if s <> TableName then
    Raise MlGenException.Create(self, Format(STryingToCloseNotPreviouslyOpenedTranslationTableSS,
                    [s, TableName]));
  
  TableName := '';
  FTranslationTable := nil;
end;{TBaseElMLGen.DoTranslationEnd}

{{
Starts template processing.
}
procedure TBaseElMLGen.Execute;
var
  SrcLen: Integer;
  OldSrcPos, TagStart: Integer;
  i, LenTagPrefix: Integer;
  s: string;
  IfPass, CommentPass, ParamsPass, CodePass: Boolean;
  SavingInterval, Substitution: string;
  
    function CheckSavingInterval : boolean;
    var
      i, m : Integer; // Counters
    begin
      Result := False;
      if Substitution <> '' then
        exit;
  
      // проверка на наличие только пробелов в строке на которой находится макрос
      // ищем начало строки или символ по которому точно будет решено, что надо строку записать.
      m := Length(SavingInterval);
      i := TagStart - 1;
      if i <> 0 then
        while i >= (TagStart - m - 1) do begin
          case FSource[i] of
            #9, ' ' : ;
            #10, #13 :
              begin
                m := i - (TagStart - m - 1);
                break;
              end;
            else
              exit; // надо записать все.
          end;    // case
          dec(i);
        end    // while
      else
        m := 0;
      SavingInterval := Copy(SavingInterval, 1, m);
      Result := True;
    end;
  
  var
    aTagFound : boolean;
    j : Integer;

begin
  FExecuting := True;
  try
    // FreeArrays; // вызывается в обработчике новой страницы
    FSrcPos := 0;
    FreeRzrvArrays;
    FCancelJob := False;
    IfPass := False;
    CommentPass := False;
    ParamsPass := False;
    CodePass := False;
    FPageCount := 0;
    FLoopBreak := '';

    BeforeExecute;
  
    FSource := FTemplate.Text;
    SrcLen := Length(FSource);
  
    LenTagPrefix := Length(FTagPrefix);
    try
      repeat
        ProcessMessages;
        if FCancelJob then
          exit;
        DoNextPage;
        try
          FSrcPos := 1;
          OldSrcPos := 1;
          while FSrcPos <= SrcLen do begin
            ProcessMessages;
            if FCancelJob then
              exit;
            TagStart := PosP('<', FSource, FSrcPos, SrcLen);
            if TagStart <= 0 then begin
              // Макросов больше нет. Запишем конец болванки
              if not CommentPass then
                WriteString(Copy(FSource, OldSrcPos, SrcLen - OldSrcPos + 1));
              break;
            end;
  
            FSrcPos := TagStart;
            i := FSrcPos + 1;
            s := GetWordEx(FSource, i, [' ', '>']);
            // Если после символа '<' идет пробел, учтем это, так как в поиске слова
            // пробелы были пропущены.
            if FSource[i] = ' ' then
              s := s + ' ';

            Substitution := '';
            // SavingInterval := '';

            // Смотрим, что за тэг мы нашли
            if (FTagPrefix = '') or (FTagPrefix = Copy(s, 1, LenTagPrefix)) then
            begin // Это тег, который мы должны обработать
              if FTagPrefix <> '' then
                Delete(s, 1, LenTagPrefix);
              // Если в конце есть закрывающий знак, то его надо исключить.  
              j := Length(s);
              if (j > 0) and (s[j] = '/') then
                SetLength(s, j - 1);

              aTagFound := True;
              if not CommentPass then
              begin
                if not CodePass then
                begin
                  if not ParamsPass then
                  begin
                    if FLoopBreak = '' then
                    begin
                      if not IfPass then
                      begin
  
                        SavingInterval := Copy(FSource, OldSrcPos, FSrcPos - OldSrcPos);
                        if s = 'comment' then begin
                          CommentPass := DoCommentBegin;
                        end
                        else
                        if s = 'macro' then begin
                          Substitution := DoMacroFound;
                          SavingInterval := SavingInterval + Substitution;
                        end
                        else
                        if s = 'repeat' then begin
                          DoRepeatBegin;
                        end
                        else
                        if s = '/repeat' then begin
                          DoRepeatDone;
                        end
                        else
                        if s = 'if' then begin
                          IfPass := not DoIfFound;
                        end
                        else
                        if s = 'else' then begin
                          IfPass := DoIfElse;
                        end
                        else
                        if s = '/if' then begin
                          DoIfDone(False);
                        end
                        else
                        if s = 'params' then begin
                          ParamsPass := DoParamsBegin;
                        end
                        else
                        if s = 'code' then begin
                          CodePass := DoCodeBegin;
                        end
                        else begin
                          if DoIsTag(s) then
                          begin
                            SavingInterval := Copy(FSource, OldSrcPos, FSrcPos - OldSrcPos);
                            DoTagFound(s);
                          end
                          else begin
                            aTagFound := False;
                            inc(FSrcPos);
                            SavingInterval := Copy(FSource, OldSrcPos, FSrcPos - OldSrcPos);
                          end;
                        end;
  
                        // проверяем, есть ли на строке с макросом еще что-нибуть кроме макроса
                        // пропуск служебных строк
                        if aTagFound then
                        begin
                          if CheckSavingInterval then
                            CheckEndPos;
                        end;
                        // Запишем если надо что-то записать
                        if SavingInterval <> '' then
                          WriteString(SavingInterval);
  
                      end // if IfFound then
                      else begin
  
                        // Если найдено условие, и результат выполнения False, то пропускаем кусок текста.
                        if s = 'else' then begin
                          IfPass := not DoIfElse;
                        end
                        else
                        if s = '/if' then begin
                          IfPass := not DoIfDone(True);
                        end
                        else
                        if s = 'comment' then begin
                          CommentPass := DoCommentBegin;
                        end
                        else begin
                          inc(FSrcPos);
                          aTagFound := False;
                        end;
  
                        // проверяем не надо ли нам пропустить перенос строк
                        // пропуск служебных строк
                        if aTagFound then
                          CheckEndPos;
  
                      end; // if IfFound else
                    end
                    else begin // !if not LoopSkip then
  
                      if s = '/repeat' then begin
                        DoRepeatSkipDone;
                      end
                      else
                      if s = 'comment' then begin
                        CommentPass := DoCommentBegin;
                      end
                      else begin
                        inc(FSrcPos);
                        aTagFound := False;
                      end;
  
                      // проверяем не надо ли нам пропустить перенос строк
                      // пропуск служебных строк
                      if aTagFound then
                        CheckEndPos;
  
                    end;
  
                  end // if not ParametresPass
                  else begin
  
                    if s = '/params' then begin
                      DoParamsEnd;
                      ParamsPass := False;
                    end
                    else
                    if s = 'comment' then begin
                      CommentPass := DoCommentBegin;
                    end
                    else begin
                      inc(FSrcPos);
                      aTagFound := False;
                    end;
  
                    // проверяем не надо ли нам пропустить перенос строк
                    // пропуск служебных строк
                    if aTagFound then
                      CheckEndPos;
  
                  end; // !if not ParametresPass
                end // if not CodePass
                else begin
  
                  if s = '/code' then begin
                    DoCodeEnd;
                    CodePass := False;
                  end
                  else
                  if s = 'comment' then begin
                    CommentPass := DoCommentBegin;
                  end
                  else begin
                    inc(FSrcPos);
                    aTagFound := False;
                  end;
  
                  // проверяем не надо ли нам пропустить перенос строк
                  // пропуск служебных строк
                  if aTagFound then
                    CheckEndPos;
  
                end; // !if not CodePass
              end // if not CommentPass
              else begin // !if not CommentPass
  
                if s = '/comment' then begin
                  CommentPass := not DoCommentEnd;
                end
                else begin
                  inc(FSrcPos);
                  aTagFound := False;
                end;
                // проверяем не надо ли нам пропустить перенос строки
                // пропуск служебных строк
                if aTagFound then
                  CheckEndPos;
  
              end;
              OldSrcPos := FSrcPos;
            end
            else begin // пропуск чужих тегов
              inc(FSrcPos);
            end;
  
          end; // while
  
          // Проверяем все ли тэги были закрыты
          // Циклы
          i := Length(FLoopCounters);
          if i > 0 then
            Raise Exception.Create(Format(SErrorInTemplateDLoopsWereNotClosed, [i]));
  
          // Условия
          i := Length(FIfNames);
          if i > 0 then
            Raise Exception.Create(Format(SErrorInTemplateDConditionsWereNotClosed, [i]));
  
          if IfPass or CommentPass or ParamsPass then
            Raise Exception.Create(SErrorInTemplateNotAllTagsHaveBeenClosed);
  
        finally // wrap up
          PageEnd(FPageCount);
        end; // try/finally
      until not FNewPageProcessing; // Прекращаем только если не было команды на создание нового файла
  
    finally // wrap up
      FreeArrays;
      FreeRzrvArrays;
    end;    // try/finally
  finally // wrap up
    FExecuting := False;
    FSource := '';
    AfterExecute;
  end;    // try/finally
  
end;{TBaseElMLGen.Execute}

procedure TBaseElMLGen.FreeArrays;
begin
  FLoopBeginPos := nil;
  FLoopCounters := nil;
  FLoopCountersCurrentPage := nil;
  FLoopNames := nil;
  FIfNames := nil;
end;{TBaseElMLGen.FreeArrays}

procedure TBaseElMLGen.FreeRzrvArrays;
begin
  FLoopBeginPosRzrv := nil;
  FLoopCountersRzrv := nil;
  FLoopNamesRzrv := nil;
end;{TBaseElMLGen.FreeRzrvArrays}

function TBaseElMLGen.GetLoopCount: Integer;
begin
  Result := Length(FLoopCounters);
end;{TBaseElMLGen.GetLoopCount}

function TBaseElMLGen.GetLoopCounter(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= Length(FLoopCounters)) then
    Raise Exception.Create(SIndexOutOfRange);
  Result := FLoopCounters[Index];
end;{TBaseElMLGen.GetLoopCounter}

function TBaseElMLGen.GetLoopCountersCurrentPage(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= Length(FLoopCountersCurrentPage)) then
    Raise Exception.Create(SIndexOutOfRange);
  Result := FLoopCountersCurrentPage[Index];
end;{TBaseElMLGen.GetLoopCountersCurrentPage}

function TBaseElMLGen.GetLoopCountersCurrentPageStr(LoopName : string): Integer;
var
  Index: Integer;
begin
  Index := GetLoopIndex(LoopName);
  if (Index < 0) or (Index >= Length(FLoopCountersCurrentPage)) then
    Raise Exception.Create(SLoopNameIsNotFound);
  Result := FLoopCountersCurrentPage[Index];
end;{TBaseElMLGen.GetLoopCountersCurrentPageStr}

function TBaseElMLGen.GetLoopCounterStr(LoopName : string): Integer;
var
  Index: Integer;
begin
  Index := GetLoopIndex(LoopName);
  if (Index < 0) or (Index >= Length(FLoopCounters)) then
    Raise Exception.Create(SLoopNameIsNotFound);
  Result := FLoopCounters[Index];
end;{TBaseElMLGen.GetLoopCounterStr}

function TBaseElMLGen.GetLoopIndex(const LoopName : string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FLoopNames) do begin
    if FLoopNames[i] = LoopName then begin
      Result := i;
      break;
    end;
  end; // for
end;{TBaseElMLGen.GetLoopIndex}

function TBaseElMLGen.GetLoopName(Index: Integer): string;
begin
  if (Index < 0) or (Index >= Length(FLoopNames)) then
    Raise Exception.Create(SIndexOutOfRange);
  Result := FLoopNames[Index];
end;{TBaseElMLGen.GetLoopName}

{{
При вызове метода считается,что мы находимся внутри тега, и нам предстоит 
разобрать, что именно за параметры у этого тэга. Если параметров больше не 
найдено, возвращаем пустые строки.
}
procedure TBaseElMLGen.GetTagProp(var ClosedTag : boolean; TagOptions : 
        TStringParameters);
  
  const
    AllocateBy = 10;
  var
    SrcLen: Integer;
    QuoutFound: Boolean;
    PropCount : Integer; // Кол-во найденных параметров тега
    EndTag : boolean;
    s, s1 : string;
  
    function ConvertParamValue(const Value : string) : string;
    var
      ValuePos, NewValuePos, ValueLen, j : Integer;
      s : string;
      i : byte;
    begin
      Result := '';
      ValuePos := 1;
      ValueLen := Length(Value);
      j := ValueLen - 2;
      if ValuePos <= j then
      begin
        while ValuePos <= j do
        begin
          NewValuePos := PosP('#', Value, ValuePos, ValueLen);
          if NewValuePos <= 0 then
          begin
            Result := Result + Copy(Value, ValuePos, ValueLen - ValuePos + 1);
            break;
          end;
          Result := Result + Copy(Value, ValuePos, NewValuePos - ValuePos);
          s := Copy(Value, NewValuePos, 3);
          s[1] := '$';
          try
            i := StrToInt(s);
            Result := Result + Char(i);
          except
            s[1] := '#';
            Result := Result + s;
          end;    // try/except
          ValuePos := NewValuePos + 3;
        end;    // while
      end
      else
        Result := Value;
    end;
  
    procedure AddProp(Prop, Value : string);
    var
      i : Integer;
    begin
      // Prop := AnsiLowerCase(Prop);
      // Value := AnsiLowerCase(Value);
      inc(PropCount);
      i := Length(TagOptions.FData);
      if i < PropCount then begin
        SetLength(TagOptions.FData, i + AllocateBy);
      end;
      TagOptions.Data[PropCount-1].ValueName := Prop;
      TagOptions.Data[PropCount-1].Value := ConvertParamValue(Value);
    end;
  
begin
  ClosedTag := False;
  PropCount := 0; // Кол-во найденных параметров тега
  TagOptions.Clear;
  SetLength(TagOptions.FData, AllocateBy); // резервируем немного места
  EndTag := False; // Признак конца тэга
  SrcLen := Length(FSource);
  if FSrcPos > SrcLen then
    Raise MlGenException.Create(self, SCanNotAnalyzeTag);
  
  try
    repeat
      // Пропускаем пробелы в начале
      while FSrcPos <= SrcLen do begin
        if FSource[FSrcPos] = ' ' then
          inc(FSrcPos)
        else
          break;
      end;
      if FSrcPos > SrcLen then
        Raise MlGenException.Create(self, SCanNotAnalyzeTag);
  
      // Выделяем название параметра
      s := '';
      while FSrcPos <= SrcLen do begin
        case FSource[FSrcPos] of
          #10, #13 :
            begin
              FSrcPos := GetPosOfNewString(FSource, FSrcPos) - 1;
              if FSrcPos <=0 then // Если так, то начала следующей строки не найдено
                FSrcPos := SrcLen;
              break;
            end;
          #9, ' ', '=' :
            break;
          '>' :
            begin
              inc(FSrcPos);
              EndTag := True;
              break;
            end;
          '<', '"' :
            Raise MlGenException.Create(self, SCanNotAnalyzeTag);
          '/' :
            begin
              if (FSrcPos = SrcLen) or (FSource[FSrcPos + 1] <> '>') then
                Raise MlGenException.Create(self, SCanNotAnalyzeTag);
              ClosedTag := True;
              EndTag := True;
              inc(FSrcPos, 2);
              break;
            end;
          else
            s := s + FSource[FSrcPos]
        end; // case
        inc(FSrcPos);
      end;
      if (FSrcPos > SrcLen) or ((s = '') and (not EndTag)) then
        Raise MlGenException.Create(self, SCanNotAnalyzeTag);
  
      if EndTag then begin
        if s <> '' then
        begin
          // Тэг закончился, а в конце был параметр без значения
          AddProp(s, '');
        end;
        break;
      end;
  
      // Проверяем знак =
      if FSource[FSrcPos] <> '=' then begin
        // В данном параметре нет значений
        AddProp(s, '');
        continue;
      end;
      inc(FSrcPos);
  
      // Выделяем значение параметра
      QuoutFound := FSource[FSrcPos] = '"';
      if QuoutFound then
        inc(FSrcPos);
      s1 := '';
      while FSrcPos <= SrcLen do begin
        case FSource[FSrcPos] of
          #10, #13 :
            begin
              if QuoutFound then
                s1 := s1 + ' '
              else
                break;
              FSrcPos := GetPosOfNewString(FSource, FSrcPos);
              if FSrcPos < 0 then // Если так, то начала следующей строки не найдено
                FSrcPos := SrcLen;
            end;
  
          #9, ' ' :
            if QuoutFound then
              s1 := s1 + FSource[FSrcPos]
            else
              break;
  
          '>' :
            if QuoutFound then
              s1 := s1 + FSource[FSrcPos]
            else begin
              EndTag := True;
              break;
            end;
  
          '"' :
            if QuoutFound then
              break
            else
              Raise MlGenException.Create(self, SCanNotAnalyzeTag);
  
          '/' :
            if QuoutFound then
              s1 := s1 + FSource[FSrcPos]
            else begin
              if (FSrcPos = SrcLen) or (FSource[FSrcPos + 1] <> '>') then
                Raise MlGenException.Create(self, SCanNotAnalyzeTag);
              ClosedTag := True;
              EndTag := True;
              inc(FSrcPos, 2);
              break;
            end;
  
          else
            s1 := s1 + FSource[FSrcPos];
        end; // case
        inc(FSrcPos);
      end;
      inc(FSrcPos);
      AddProp(s, s1);
    until EndTag;
  finally // wrap up
    SetLength(TagOptions.FData, PropCount);
  end;    // try/finally
end;{TBaseElMLGen.GetTagProp}

procedure TBaseElMLGen.IsTag(TagName : string; var IsTag : boolean);
begin
  IsTag := FCustomTagNames.IndexOf(TagName) >=0;
end;{TBaseElMLGen.IsTag}

procedure TBaseElMLGen.LoopBreak(LoopName : string);
var
  i: Integer;
begin
  i := GetLoopIndex(LoopName);
  if i < 0 then
    Raise Exception.Create(SAttemptToCloseTheLoopThatHasNotBeenOpenedBefore);
  FLoopBreak := LoopName;
end;{TBaseElMLGen.LoopBreak}

procedure TBaseElMLGen.LoopContinue(LoopName : string);
begin
end;{TBaseElMLGen.LoopContinue}

procedure TBaseElMLGen.NextPage;
var
  i, j: Integer;
begin
  if FNewPageProcessing or FEnteringInNextPage then
    Raise Exception.Create(SRepeatedRequestForNewPageIsNotAllowed);
  FNewPageProcessing := True;
  
  j := Length(FLoopBeginPos);
  SetLength(FLoopBeginPosRzrv, j);
  for i := 0 to j-1 do
    FLoopBeginPosRzrv[i] := FLoopBeginPos[i];
  
  j := Length(FLoopCounters);
  SetLength(FLoopCountersRzrv, j);
  for i := 0 to j-1 do
    FLoopCountersRzrv[i] := FLoopCounters[i];
  
  j := Length(FLoopNames);
  SetLength(FLoopNamesRzrv, j);
  for i := 0 to j-1 do
    FLoopNamesRzrv[i] := FLoopNames[i];
end;{TBaseElMLGen.NextPage}

{$ifdef ELMLGEN_ACTIVEX}
procedure TBaseElMLGen.Paint;
begin
  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(ClientRect);
  Canvas.BrushCopy(ClientRect, FIcon, Rect(0, 0, 24, 24), clFuchsia);
end;{TBaseElMLGen.Paint}
{$endif}

procedure TBaseElMLGen.PrepareTemplate;
var
  SrcLen: Integer;
  OldSrcPos, TagStart: Integer;
  i, LenTagPrefix: Integer;
  s: string;
  CommentPass: Boolean;
  ParamsFound, CodeFound: Boolean;
  ParamName, TranslationTableName, ReplaceString: string;
  ParamNumb, ReplaceStringNumb: Integer;
  SavingInterval, CodeText: string;
begin
  FExecuting := True;
  try
    FParameters.Clear;
    FTranslationTables.Clear;
    FCode.Clear;
  
    CommentPass := False;
    ParamsFound := False;
    CodeFound := False;
    ParamName := '';
    TranslationTableName := '';
    ReplaceString := '';
    CodeText := '';
  
    FSource := FTemplate.Text;
    SrcLen := Length(FSource);
  
    LenTagPrefix := Length(FTagPrefix);
    FSrcPos := 1;
    OldSrcPos := 1;
    while FSrcPos <= SrcLen do begin
      TagStart := PosP('<', FSource, FSrcPos, SrcLen);
      if TagStart <= 0 then begin
        break;
      end;
  
      FSrcPos := TagStart;
      i := FSrcPos + 1;
      s := GetWordEx(FSource, i, [' ', '>']);
              // Если после символа '<' идет пробел, учтем это, так как в поиске слова
              // пробелы были пропущены.
      if FSource[i] = ' ' then
        s := s + ' ';
  
              // Смотрим, что за тэг мы нашли
      if (FTagPrefix = '') or (FTagPrefix = Copy(s, 1, LenTagPrefix)) then
      begin // Это тег, который мы должны обработать
        if FTagPrefix <> '' then
          Delete(s, 1, LenTagPrefix);
  
        if not CommentPass then
        begin
          if s = 'comment' then begin
            CommentPass := DoCommentBegin;
          end
          else begin
            if ParamsFound then
            begin
                    // Обработчик содержимого между тегами params /params
              if TranslationTableName = '' then
              begin
  
                if ParamName = '' then
                begin
                 if s = '/params' then begin
                    ParamsFound := False;
                    DoParamsEnd;
                  end
                  else
                  if s = 'translation' then begin
                    DoTranslationBegin(TranslationTableName);
                  end
                  else
                  if s = 'param' then begin
                    DoParamBegin(ParamName, ParamNumb);
                    CheckEndPos;
                  end
                  else
                    inc(FSrcPos);
                end // if not ParamFound
                else begin
                  if s = '/param' then begin
                    SavingInterval := Copy(FSource, OldSrcPos, FSrcPos - OldSrcPos);
                    ParamName := '';
                    DoParamEnd;
                    CheckEndPos;
                  end
                  else begin
                    inc(FSrcPos);
                    SavingInterval := Copy(FSource, OldSrcPos, FSrcPos - OldSrcPos);
                  end;
                  with FParameters.FData[ParamNumb] do
                    Value := Value + SavingInterval;
                end; // !if not ParamFound
  
              end
              else begin
                      // Обработчик содержимого между тегами translation /translation
                if ReplaceString = '' then
                begin
                  if s = '/translation' then begin
                    DoTranslationEnd(TranslationTableName);
                  end
                  else
                  if s = 'replace' then begin
                    DoReplaceBegin(ReplaceString, ReplaceStringNumb);
                    CheckEndPos;
                  end
                  else
                    inc(FSrcPos);
                end // if not ReplaceFound
                else begin
                  if s = '/replace' then begin
                    SavingInterval := Copy(FSource, OldSrcPos, FSrcPos - OldSrcPos);
                    ReplaceString := '';
                    DoReplaceEnd;
                    CheckEndPos;
                  end
                  else begin
                    inc(FSrcPos);
                    SavingInterval := Copy(FSource, OldSrcPos, FSrcPos - OldSrcPos);
                  end;
                  with FTranslationTable.Table.FData[ReplaceStringNumb] do
                    Value := Value + SavingInterval;
                end; // !if not ReplaceFound
              end;
            end
            else begin
              if CodeFound then
              begin
                if s = '/code' then begin
                  SavingInterval := Copy(FSource, OldSrcPos, FSrcPos - OldSrcPos);
                  CodeFound := False;
                  DoCodeEnd;
                  CheckEndPos;
                end
                else begin
                  inc(FSrcPos);
                  SavingInterval := Copy(FSource, OldSrcPos, FSrcPos - OldSrcPos);
                end;
                CodeText := CodeText + SavingInterval;
              end // if CodeFound
              else begin
                      // обработчик в момент поиска секции с параметрами или скриптом
                if s = 'params' then begin
                  ParamsFound := DoParamsBegin;
                end
                else
                if s = 'code' then begin
                  CodeFound := DoCodeBegin;
  //                  if CodeFound and (CodeText <> '') then
  //                    CodeText := CodeText + #13#10;
                  CheckEndPos;
                end
                else
                  inc(FSrcPos);
              end; // !if CodeFound
            end; // !if ParamsFound
          end; // !if s = 'comment'
        end // if not CommentPass
  
        else begin // !if not CommentPass
          if s = '/comment' then begin
            CommentPass := not DoCommentEnd;
          end
          else begin
            inc(FSrcPos);
          end;
        end;
  
        OldSrcPos := FSrcPos;
      end
      else begin // пропуск чужих тегов
        inc(FSrcPos);
      end;
  
    end; // while
  
    if ParamsFound or (TranslationTableName <> '') or (ParamName <> '') or
      (ReplaceString <> '') then
      Raise Exception.Create(SErrorInTemplateParamsDeclaration);
  
    FCode.Text := CodeText;
  
  finally // wrap up
    FExecuting := False;
    FSource := '';
  end;    // try/finally
end;{TBaseElMLGen.PrepareTemplate}

procedure TBaseElMLGen.ProcessMessages;
begin
end;{TBaseElMLGen.ProcessMessages}

{$ifdef ELMLGEN_ACTIVEX}
procedure TBaseElMLGen.Resize;
begin
  inherited;
  SetBounds(Left, Top, 24, 24);
end;{TBaseElMLGen.Resize}
{$endif}

procedure TBaseElMLGen.SetCustomTagNames(Value: TStrings);
begin
  FCustomTagNames.Assign(Value);
end;{TBaseElMLGen.SetCustomTagNames}

procedure TBaseElMLGen.SetTagPrefix(const Value: string);
var
  i: Integer;
begin
  if FTagPrefix <> Value then
  begin
    // Проверим корректность префикса
    for i := 1 to Length(Value) do
    begin
      if Value[i] in [#0..' ', '"', #39, '<', '>', '/'] then
        Raise Exception.Create(Format(SInvalidCharDSInTagPrefixString, [i, Value[i]]));
    end; // for
    FTagPrefix := Value;
  end;
end;{TBaseElMLGen.SetTagPrefix}

procedure TBaseElMLGen.SetTemplate(Value: TStrings);
begin
  FTemplate.Assign(Value);
end;{TBaseElMLGen.SetTemplate}

procedure TBaseElMLGen.TagFound(Tag : string; const TagClosed : boolean; 
        TagParameters : TStringParameters);
begin
end;{TBaseElMLGen.TagFound}

procedure TBaseElMLGen.TemplateChanged(Sender: TObject);
begin
  if FExecuting then
    Raise Exception.Create(SCanTChangeTemplateDuringExecution);
  PrepareTemplate;
end;{TBaseElMLGen.TemplateChanged}

{
******************************** TCustomElMLGen ********************************
}
procedure TCustomElMLGen.AfterExecute;
begin
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(self);
end;{TCustomElMLGen.AfterExecute}

procedure TCustomElMLGen.BeforeExecute;
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(self);
end;{TCustomElMLGen.BeforeExecute}

procedure TCustomElMLGen.IfFound(IfName : string; TagParameters : 
        TStringParameters; var ResultValue : boolean);
begin
  if Assigned(FOnIfFound) then
    FOnIfFound(self, IfName, TagParameters, ResultValue);
end;{TCustomElMLGen.IfFound}

procedure TCustomElMLGen.IsTag(TagName : string; var IsTag : boolean);
begin
  inherited IsTag(TagName, IsTag);
  if Assigned(FOnIsTag) then
    FOnIsTag(Self, TagName, IsTag);
end;{TCustomElMLGen.IsTag}

procedure TCustomElMLGen.LoopIteration(LoopNumb: integer; LoopName: string; 
        TagParameters : TStringParameters; var LoopDone : boolean);
begin
  if Assigned(FOnLoopIteration) then
    FOnLoopIteration(self, LoopNumb, LoopName, TagParameters, LoopDone);
end;{TCustomElMLGen.LoopIteration}

procedure TCustomElMLGen.MacroFound(MacroName : string; TagParameters : 
        TStringParameters; var MacroResult : string; var UseTranslationTable : 
        boolean);
begin
  if Assigned(FOnMacroFound) then
    FOnMacroFound(self, MacroName, TagParameters, MacroResult, UseTranslationTable)
end;{TCustomElMLGen.MacroFound}

procedure TCustomElMLGen.PageBegin(PageNumb : integer);
begin
  if Assigned(FOnPageBegin) then
    FOnPageBegin(self, PageNumb);
end;{TCustomElMLGen.PageBegin}

procedure TCustomElMLGen.PageEnd(PageNumb : integer);
begin
  if Assigned(FOnPageEnd) then
    FOnPageEnd(self, PageNumb);
end;{TCustomElMLGen.PageEnd}

procedure TCustomElMLGen.ProcessMessages;
begin
  if Assigned(FOnProcessMessages) then
    FOnProcessMessages(self);
end;{TCustomElMLGen.ProcessMessages}

procedure TCustomElMLGen.TagFound(Tag : string; const TagClosed : boolean; 
        TagParameters : TStringParameters);
begin
  if Assigned(FOnTagFound) then
    FOnTagFound(self, Tag, TagClosed, TagParameters);
end;{TCustomElMLGen.TagFound}

procedure TCustomElMLGen.WriteString(Value : string);
begin
  if Assigned(FOnWriteString) then
    FOnWriteString(self, Value);
end;{TCustomElMLGen.WriteString}

{
******************************** MlGenException ********************************
}
constructor MlGenException.Create(MlGen : TBaseElMlGen; Msg: string);
var
  i, j, k: Integer;
begin
  inherited Create(Msg);
  FMlGen := MlGen;
  i := Length(Msg);
  if i > 0 then begin
    if Msg[i] <> '.' then
      Msg := Msg + '.';
  end
  else
    Msg := SError;
  GetTextLineByStringPos(MlGen.FSource, MlGen.FSrcPos, j, k);
  inherited Create(Format(SLineChar, [Msg, j, k]));
end;{Raise MlGenException.Create}

end.