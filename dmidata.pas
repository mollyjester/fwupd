unit dmidata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDMIData }

  TDMIData = class(TObject)
  private
    FDMIPath: String;
    function getDMIDump: String;
  public
    constructor Create(_dmiPath: String);
    property dmiDump: String read getDMIDump;
  end;

implementation

{ TDMIData }

function TDMIData.getDMIDump(): String;
begin
  with TStringList.Create do
  begin
    LoadFromFile(FDMIPath);
    Result:=Text;
    Free;
  end;
end;

constructor TDMIData.Create(_dmiPath: String);
begin
  inherited Create;

  if _dmiPath <> '' then
  begin
    FDMIPath:=_dmiPath;
  end
  else
  begin
    raise Exception.Create('Dump file not specified!');
  end;
end;

end.

