program fwupd;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure ReadFile(_filename: String);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('f') then begin
    ReadFile(GetOptionValue('f'));
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

procedure TMyApplication.ReadFile(_filename: String);
var
    tfDMIFile: TextFile;
    sLine: String;
begin
    AssignFile(tfDMIFile, _filename);

    try
      reset(tfDMIFile);

      while not eof(tfDMIFile) do
      begin
        readln(tfDMIFile, sLine);
        writeln(sLine);
      end;

      CloseFile(tfDMIFile);
    except
      on E: EInOutError do
      begin
        writeln('A DMI Dump file handling error occured. Details: ', E.Message);
      end;
    end;
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
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
  writeln('-f <filename>');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='fwupd';
  Application.Run;
  Application.Free;
end.

