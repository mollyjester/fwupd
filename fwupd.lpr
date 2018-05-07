program fwupd;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, bufstream, global, datawh;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
  protected
    procedure DoRun; override;
    procedure WriteHelp; virtual;
    procedure ReadFile(_filename: String);
    procedure UpdateConnectionType(_contype: String);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hf:c:', '');
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

  if HasOption('f') then begin
    ReadFile(GetOptionValue('f'));
    Terminate;
    Exit;
  end;

  if HasOption('c') then begin
    UpdateConnectionType(GetOptionValue('c'));
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

procedure TMyApplication.ReadFile(_filename: String);
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

procedure TMyApplication.UpdateConnectionType(_contype: String);
begin
  updateDefaultConnectorType(_contype);
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
  writeln('Usage: ', ExeName, ' -h -f <filename>');
  writeln('-h: Shows this help screen');
  writeln('-f <filename>: Defines file to read');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='fwupd';
  Application.Run;
  Application.Free;
end.

