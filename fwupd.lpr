program fwupd;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, datawh, dmidata;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    fdwh: TDataWarehouse;
    fdmi: TDMIData;
    function getDMI: TDMIData;
    function getDWH: TDataWarehouse;
  protected
    property dwh: TDataWarehouse read getDWH;
    property dmi: TDMIData read getDMI;
    procedure DoRun; override;
    procedure WriteHelp; virtual;
    function findMB: String;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TMyApplication }

function TMyApplication.getDWH: TDataWarehouse;
begin
  if not Assigned(fdwh) then begin
    fdwh:=TDataWarehouse.Create;
    fdwh.initDatabase;
  end;

  getDWH:=fdwh;
end;

function TMyApplication.getDMI: TDMIData;
begin
  if not Assigned(fdmi) then begin
    fdmi:=TDMIData.Create;
  end;

  getDMI:=fdmi;
end;

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  sMB: String;
  mbRecord: TDataMBRecord;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', '');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  try
    sMB:=findMB();

    if sMB <> '' then begin
      writeln(Format('Motherboard %s found', [sMB]));

      mbRecord:=dwh.mbRecord[sMB];
      writeln(Format('upd: %s, params: %s, fw: %s', [mbRecord.Updater,
                                                     mbRecord.ParamFmt,
                                                     mbRecord.Firmware]));
      // check if updater exists
      // check if firmware exists
      // flash motherboard
    end
    else begin
      raise Exception.Create('Motherboard not found!');
    end;
  except
    on E:Exception do
    begin
      ExceptionExitCode:=-1;
      writeln(E.Message);
    end;
  end;

  ReadLn();
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' -h');
  writeln('-h: Shows this help screen');
end;

function TMyApplication.findMB: String;
var
  i: Integer;
  cnt: Integer;
  mb: String;
begin
  cnt:=dwh.mbVersions.Count;

  for i:=0 to cnt - 1 do
  begin
    mb:=dwh.mbVersions[i];

    if Pos(mb, dmi.dmiDump.Text) <> 0 then begin
      findMB:=mb;
      break;
    end;
  end;
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

