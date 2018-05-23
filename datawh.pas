unit datawh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CsvDoc;

type

  { TDataMBRecord }

  TDataMBRecord = class(TObject)
  private
    FFirmware: String;
    FMBVersion: String;
    FParamFmt: String;
    FUpdater: String;
  public
    property MBVersion: String read FMBVersion;
    property Updater: String read FUpdater;
    property ParamFmt: String read FParamFmt;
    property Firmware: String read FFirmware;
    constructor Create(_mbversion: String;
                       _updater: String;
                       _paramFmt: String;
                       _firmware: String); virtual;
  end;

  { TDataWarehouse }

  TDataWarehouse = class(TObject)
  private
    FCSV: TCSVDocument;

    const dbName: String = '\db\';
    const tblMBVersion: String = 'mbver.csv';

    function Get(const AMBVersion: string): TDataMBRecord;
    function getMBVersions: TStringList;
  protected
  public
    property mbVersions: TStringList read getMBVersions;
    property mbRecord[const AMBVersion: string]: TDataMBRecord read Get;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure initDatabase;
  end;

implementation

{ TDataMBRecord }

constructor TDataMBRecord.Create(_mbversion: String; _updater: String;
  _paramFmt: String; _firmware: String);
begin
  inherited Create;
  FMBVersion:=_mbversion;
  FUpdater:=_updater;
  FParamFmt:=_paramFmt;
  FFirmware:=_firmware;
end;

{ TDataWarehouse }

function TDataWarehouse.getMBVersions: TStringList;
var
  i: Integer;
  cnt: Integer;
begin
  getMBVersions:=TStringList.Create;

  cnt:=FCSV.RowCount;

  for i:=1 to cnt - 1 do begin
    getMBVersions.Append(FCSV.Cells[0, i]);
  end;
end;

function TDataWarehouse.Get(const AMBVersion: string): TDataMBRecord;
var
  i: Integer;
  cnt: Integer;
begin
  cnt:=FCSV.RowCount;

  for i:=1 to cnt - 1 do begin
    if FCSV.Cells[0, i] = AMBVersion then begin
      get:=TDataMBRecord.Create(FCSV.Cells[0, i],
                                FCSV.Cells[1, i],
                                FCSV.Cells[2, i],
                                FCSV.Cells[3, i]);
      break;
    end;
  end;
end;

constructor TDataWarehouse.Create;
begin
  inherited;
  FCSV:=TCSVDocument.Create;
  FCSV.Delimiter:=';';
end;

destructor TDataWarehouse.Destroy;
begin
  FreeAndNil(FCSV);
  inherited Destroy;
end;

procedure TDataWarehouse.initDatabase;
begin
  if not FileExists(GetCurrentDir() + dbName + tblMBVersion) then begin
    FCSV.Cells[0, 0]:='mbversion';
    FCSV.Cells[1, 0]:='updater';
    FCSV.Cells[2, 0]:='param_fmt';
    FCSV.Cells[3, 0]:='firmware';
    FCSV.SaveToFile(GetCurrentDir() + dbName + tblMBVersion);
  end
  else begin
    FCSV.LoadFromFile(GetCurrentDir() + dbName + tblMBVersion);
  end;
end;

initialization

end.

