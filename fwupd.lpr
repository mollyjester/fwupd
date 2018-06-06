program fwupd;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, datawh, dmidata, strutils, process;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    fdwh: TDataWarehouse;
    fdmi: TDMIData;
    FDMIPath: String;
    FDBPath: String;
    FSilentMode: Boolean;
    FCurDir: String;
    function GetDMI: TDMIData;
    function GetDWH: TDataWarehouse;
  protected
    property dwh: TDataWarehouse read GetDWH;
    property dmi: TDMIData read GetDMI;

    function Check(_mbRec: TDataMBRecord): Boolean;
    function CheckDMIPath(_path: String): Boolean;
    function CheckDBPath(_path: String): Boolean;
    function CheckFirmware(_firmware: String): Boolean;
    function CheckUpdater(_updater: String): Boolean;
    procedure DoRun; override;
    function FindMB: TDataMBRecord;
    function FlashMB(_mbRec: TDataMBRecord): Boolean;
    procedure initDB;
    procedure WriteHelp; virtual;
    procedure WriteLnS(_value: String);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TMyApplication }

function TMyApplication.GetDWH: TDataWarehouse;
begin
  if not Assigned(fdwh) then begin
    fdwh:=TDataWarehouse.Create(FDBPath);
  end;

  Result:=fdwh;
end;

function TMyApplication.Check(_mbRec: TDataMBRecord): Boolean;
begin
  WriteLnS('Checking parameters:');

  Result:=CheckUpdater(FCurDir + _mbRec.Updater);
  Result:=CheckFirmware(FCurDir + _mbRec.Firmware) and Result;
end;

function TMyApplication.CheckDMIPath(_path: String): Boolean;
begin
  Result:=FileExists(_path);

  if not Result then
  begin
    WriteLnS(Format('File %s not exists!', [_path]));
  end;
end;

function TMyApplication.CheckDBPath(_path: String): Boolean;
begin
  Result:=FileExists(_path);

  if not Result then
  begin
    WriteLnS(Format('File %s not exists!', [_path]));
  end;
end;

function TMyApplication.GetDMI: TDMIData;
begin
  if not Assigned(fdmi) then
  begin
    fdmi:=TDMIData.Create(FDMIPath);
  end;

  Result:=fdmi;
end;

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  mbRec: TDataMBRecord;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hd:b:si', '');

  if ErrorMsg<>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('b') then
  begin
    FDBPath:=GetOptionValue('b');

    if not HasOption('i')
    and not CheckDBPath(FCurDir + FDBPath) then
    begin
      Terminate(-1);
      Exit;
    end;
  end;

  if HasOption('i') then
  begin
    initDB;
    Terminate;
    Exit;
  end;

  if HasOption('s') then
  begin
    FSilentMode:=True;
  end;

  if HasOption('d') then
  begin
    FDMIPath:=GetOptionValue('d');

    if not CheckDMIPath(FCurDir + FDMIPath) then
    begin
      Terminate(-1);
      Exit;
    end;
  end
  else
  begin
    WriteLnS('ERROR: -d is not optional parameter!');
    WriteHelp;
    Terminate(-1);
    Exit;
  end;

  try
    mbRec:=FindMB();

    if (mbRec <> nil)
    and Check(mbRec)
    and FlashMB(mbRec) then
    begin
      WriteLnS('Complete!');
    end
    else
    begin
      WriteLnS('Errors occured while executing fwupd!');
      Terminate(-1);
    end;
  except
    on E:Exception do
    begin
      ExceptionExitCode:=-1;
      WriteLnS(E.Message);
    end;
  end;

  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FCurDir:=GetCurrentDir;

  if LeftStr(FCurDir, 1) <> '\' then
  begin
    FCurDir:=FCurDir + '\';
  end;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' -h -d <path> -b <path> -s -i');
  writeln('-b <path>: Path to folder with .csv file');
  writeln('-d <path>: Dump file of DMI data');
  writeln('-h: Shows this help screen');
  writeln('-i: Create clear well structured .csv file. Can be combined with -b');
  writeln('-s: Silent mode');
end;

procedure TMyApplication.WriteLnS(_value: String);
begin
  if not FSilentMode then
  begin
    writeln(_value);
  end;
end;

function TMyApplication.FindMB: TDataMBRecord;
var
  i: Integer;
  iPos: Integer;
  iLen: Integer;
  sDump: String;
  mbRec: TDataMBRecord;
begin
  Result:=nil;

  for i:=0 to dwh.mbTable.Count - 1 do
  begin
    mbRec:=dwh.mbTable.recordsIdx[i];
    sDump:=dmi.dmiDump;
    iLen:=Length(mbRec.MBVersion);

    repeat
      iPos:=Pos(mbRec.MBVersion, sDump);

      if iPos <> 0 then
      begin
        if sDump[iPos + iLen] = #13 then
        begin
          Result:=mbRec;
          break;
        end
        else
        begin
          sDump:=Copy(sDump, iPos + iLen, Length(sDump));
        end;
      end;
    until iPos = 0;
    
    if Result <> nil then break;
  end;

  if Assigned(Result) then
  begin
    WriteLnS(Format('Motherboard %s found', [mbRec.MBVersion]));
  end
  else
  begin
    WriteLnS('Motherboard not found!');
  end;
end;

function TMyApplication.FlashMB(_mbRec: TDataMBRecord): Boolean;
var
  sOut: String;
  sParams: String;
  sUpdater: String;
begin
  sUpdater:=FCurDir + _mbRec.Updater;
  sParams:=Format(_mbRec.ParamFmt, [FCurDir + _mbRec.Firmware]);

  WriteLnS(Format('Start flashing: %s %s', [sUpdater, sParams]));

  Result:=RunCommand(sUpdater, sParams, sOut);

  WriteLnS(sOut);
  WriteLnS('Flashing status: ' + ifthen(Result, 'OK', 'ERROR'));
end;

procedure TMyApplication.initDB;
begin
  dwh.initDatabase;
end;

function TMyApplication.CheckUpdater(_updater: String): Boolean;
begin
  Result:=FileExists(_updater);

  WriteLnS(' * updater: ' + ifthen(Result, 'OK',
      Format('%s not found!', [_updater])));
end;

function TMyApplication.CheckFirmware(_firmware: String): Boolean;
begin
  Result:=FileExists(_firmware);

  WriteLnS(' * firmware: ' + ifthen(Result, 'OK',
      Format('%s not found!', [_firmware])));
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

