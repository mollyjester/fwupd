unit dmidata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

type

  ExceptionExecError = class(Exception);

  { TDMIData }

  TDMIData = class(TObject)
  private
    FDMIPath: String;

    const defaultDMIPath = 'dmidecode.exe';

    function getDMIDump(): String;
  public
    constructor Create(_dmiPath: String = ''); virtual;

    property dmiDump: String read getDMIDump;
  end;

implementation

{ TDMIData }

function TDMIData.getDMIDump(): String;
begin
  if not RunCommandInDir('', FDMIPath, ['-t 0,1,2'], Result) then
  begin
    raise ExceptionExecError.CreateFmt('Couldn''t run %s!', [FDMIPath]);
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

