unit dmidata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

{$IFDEF GO32V2 OR $IFDEF MSDOS}
const dmiProc = 'dmidec~1.exe';
{$ELSE}{$IFDEF LINUX}
const dmiProc = '/usr/sbin/dmidecode';
{$ENDIF}{$ENDIF}


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
  hprocess: TProcess;
  sPass: String;
  OutputLines: TStringList;

begin
  sPass := 'NUKE32dll';
  OutputLines:=TStringList.Create; //... a try...finally block would be nice to make sure

  hProcess := TProcess.Create(nil);
  hProcess.Executable := '/bin/sh';
  hprocess.Parameters.Add('-c');
  hprocess.Parameters.add('echo ' + sPass  + ' | sudo -S ' + dmiProc + ' -t 0,1,2');
  hProcess.Options := hProcess.Options + [poWaitOnExit, poUsePipes];
  hProcess.Execute;

  OutputLines.Add('stdout:');
  OutputLines.LoadFromStream(hprocess.Output);
  OutputLines.Add('stderr:');
  OutputLines.LoadFromStream(hProcess.Stderr);

  getDMIDump:=OutputLines;

  hProcess.Free;
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

