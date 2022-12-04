unit NoteRecord;

interface

uses classes;

 type

  TAddress = record
    Country: String;
    State: String;
    City: String;
    Street: String;
    Phone: String;
    Fax: String;
    Mobile: String;
    Email: String;
    WebPage: String;
  end;

  TNoteRecord = class(TComponent)
  private
    FTimeStamp: TDateTime;
    FTimeChange: TDateTime;
    FTitle: string;
    FNote: string;
  published
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property TimeChange: TDateTime read FTimeChange write FTimeChange;
    property Title: string read FTitle write FTitle;
    property Note: string read FNote write FNote;
//    constructor Create;
  end;

  TNoteEvent = class(TNoteRecord)
  private
    FTimePeriodEnabled: Boolean;
    FTimeEvent: TDateTime;
    FTimePeriod: TDateTime;
    FTimeEventEnabled: Boolean;
  published
    property TimePeriodEnabled: Boolean read FTimePeriodEnabled write
        FTimePeriodEnabled;
    property TimeEvent: TDateTime read FTimeEvent write FTimeEvent;
    property TimePeriod: TDateTime read FTimePeriod write FTimePeriod;
    property TimeEventEnabled: Boolean read FTimeEventEnabled write
        FTimeEventEnabled;
//    constructor Create;
  end;

  TNotePerson = class(TNoteRecord)
  private
    FAddress: TAddress;
    FPhone: string;
  published
    property Address: TAddress read FAddress write FAddress;
    property Phone: string read FPhone write FPhone;
//    constructor Create;
  end;

  TNoteRemind = class(TNoteRecord)
  private
    FRemindTime: TDateTime;
    FEnabled: Boolean;
  published
    property RemindTime: TDateTime read FRemindTime write FRemindTime;
    property Enabled: Boolean read FEnabled write FEnabled;
//    constructor Create;
  end;

implementation

{
  constructor TNoteRecord.Create();
  begin
  end;

  constructor TNoteEvent.Create;
  begin

  end;

  constructor TNotePerson.Create;
  begin

  end;

  constructor TNoteRemind.Create;
  begin

  end;
}
end.
