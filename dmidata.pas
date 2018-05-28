unit dmidata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  ExceptionExecError = class(Exception);

  { TDMIData }

  TDMIData = class(TObject)
  private
    FDMIPath: String;

    const defaultDMIPath: String = 'dmidecode.txt';

    function getDMIDump: String;
  public
    constructor Create(_dmiPath: String = '');

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

constructor TDMIData.Create(_dmiPath: String = '');
begin
  inherited Create;

  if _dmiPath <> '' then
  begin
    FDMIPath:=_dmiPath;
  end
  else
  begin
    FDMIPath:=defaultDMIPath;
  end;
end;

end.

