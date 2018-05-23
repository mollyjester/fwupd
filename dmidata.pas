unit dmidata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

const dmiProc = 'dmidecode.exe';

type
  
  { TDMIData }

  TDMIData = class(TObject)
  private
    function getDMIDump(): TStrings;
    procedure ReadFile(_filename: String);
  public
    property dmiDump: TStrings read getDMIDump;
  end;

implementation

{ TDMIData }

function TDMIData.getDMIDump: TStrings;
var
  sOut: String;
  i: Integer;
  cnt: Integer;
  sLine: String;
  trimmedLine: String;
begin
  getDMIDump:=TStringList.Create;

  if RunCommandInDir('', dmiProc, ['-t 0,1,2'], sOut) then
  begin
    cnt:=Length(sOut);

    for i:=1 to cnt do
    begin
      if sOut[i] = LineEnding[1] then
      begin
        trimmedLine:=Trim(sLine);
        if trimmedLine <> '' then
        begin
          getDMIDump.Append(trimmedLine);
        end;
        sLine:='';
      end
      else
      begin
        sLine:=sLine + sOut[i];
      end;
    end;

    if sLine <> '' then
    begin
      trimmedLine:=Trim(sLine);
      if trimmedLine <> '' then
      begin
        getDMIDump.Append(trimmedLine);
      end;
    end;
  end
  else
  begin
    raise Exception.Create('Couldn''t run dmidecode!');
  end;
end;

procedure TDMIData.ReadFile(_filename: String);
var
  slData: TStringList;
begin
  if FileExists(_filename) then begin
    try
      slData:=TStringList.Create;
      slData.LoadFromFile(_filename);

      writeln(slData.Text);
    finally
      FreeAndNil(slData);
    end;
  end;
end;

end.

