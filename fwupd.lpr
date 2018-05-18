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
    procedure DoRun; override;
    procedure WriteHelp; virtual;
    property dwh: TDataWarehouse read getDWH;
    property dmi: TDMIData read getDMI;
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

  if dwh.mbVersions.IndexOf(dmi.mbVersion) <> -1 then begin
    writeln('Motherboard ', dmi.mbVersion, ' found.');
  end
  else begin
    writeln('Motherboard ', dmi.mbVersion, ' not found.');
  end;

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
  writeln('Usage: ', ExeName, ' -h -f <filename> -c <connector type>');
  writeln('-h: Shows this help screen');
  writeln('-f <filename>: Defines file to read');
  writeln('-c <connector type>: Sets a DB driver to operate with');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='fwupd';
  Application.Run;
  Application.Free;
end.

