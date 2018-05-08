unit dmidata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  
  { TDMIData }

  TDMIData = class(TObject)
  private
    function getVersion: String;
    procedure ReadFile(_filename: String);
  public
    property mbVersion: String read getVersion;
  end;

implementation

{ TDMIData }

function TDMIData.getVersion: String;
begin
  getVersion:='DH3-Z77-V2';
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

