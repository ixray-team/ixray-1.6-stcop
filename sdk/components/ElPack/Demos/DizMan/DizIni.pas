unit DizIni;

interface

uses ElIni;

type TDizIni = class (TElIniFile)
      public
       procedure ParseLine(S : String; var Name, Value: string; var HasName : boolean); override;
       function GetValueEntry(Key : string; ValueName : string) : TElIniEntry; override;
     end;

implementation

function TDizIni.GetValueEntry(Key : string; ValueName : string) : TElIniEntry;
begin
  result := inherited GetValueEntry(Key, ValueName);
end;

procedure TDizIni.ParseLine(S : String; var Name, Value: string; var HasName : boolean);
var i : integer;
    b : boolean;
    IV: boolean;
begin
  b := false;
  iv := false;
  Name := '';
  Value := '';
  for i := 1 to Length(S) do
  begin
    if b then
    begin
      if s[i] = '"' then b := false;
      if iv then Value := Value + s[i] else Name := Name + s[i];
    end else
    begin
      if not iv then
      begin
        if s[i] = '"' then b := true;
        if s[i] = '*' then iv := true else
        Name := Name + s[i];
      end else Value := Value + s[i];
    end;
  end;
  HasName := iv;
end;

end.

