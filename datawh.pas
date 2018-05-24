unit datawh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CsvDoc;

type

  ExceptionDataNotFound = class(Exception);

  { TDataMBRecord }

  TDataMBRecord = class(TObject)
  private
    FFirmware: String;
    FMBVersion: String;
    FParamFmt: String;
    FUpdater: String;
  public
    constructor Create(_mbversion: String;
                       _updater: String;
                       _paramFmt: String;
                       _firmware: String); virtual;

    property MBVersion: String read FMBVersion;
    property Updater: String read FUpdater;
    property ParamFmt: String read FParamFmt;
    property Firmware: String read FFirmware;
  end;

  { TDataMBTable }

  TDataMBTable = class(TObject)
  private
    FList: TList;
    function GetCount: Integer;
    function GetRecord(AIndex: String): TDataMBRecord;
    function GetRecord(AIndex: integer): TDataMBRecord;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property count: Integer read GetCount;
    property records[AIndex: String]: TDataMBRecord read GetRecord;
    property recordsIdx[AIndex: integer]: TDataMBRecord read GetRecord;

    function Add(_record: TDataMBRecord): String;
    procedure Delete(_index: String);
    function HasRecord(_index: String): Boolean;
  end;

  { TDataWarehouse }

  TDataWarehouse = class(TObject)
  private
    FCSV: TCSVDocument;
    FMBTable: TDataMBTable;
    FDBDir: String;
    FDBName: String;

    const defaultDBName: String = '\db\mbver.csv';

    function GetMBTable: TDataMBTable;
  protected
    procedure initDatabase;
  public
    constructor Create(_dbName: String = ''); virtual;
    destructor Destroy; override;

    property mbTable: TDataMBTable read GetMBTable;
  end;

implementation

{ TDataMBTable }

function TDataMBTable.GetRecord(AIndex: String): TDataMBRecord;
var
  i: Integer;
  rec: TDataMBRecord;
begin
  for i:=0 to FList.Count - 1 do
  begin
    rec:=TDataMBRecord(FList[i]);

    if rec.MBVersion = AIndex then
    begin
      Result:=rec;
      break;
    end;
  end;

  if not Assigned(Result) then
  begin
    raise ExceptionDataNotFound.CreateFmt('No ''%s'' motherboard in DB!', [AIndex]);
  end;
end;

function TDataMBTable.GetRecord(AIndex: integer): TDataMBRecord;
begin
  Result:=TDataMBRecord(FList[AIndex]);
end;

function TDataMBTable.GetCount: Integer;
begin
  Result:=FList.Count;
end;

constructor TDataMBTable.Create;
begin
  inherited;
  FList:=TList.Create;
end;

destructor TDataMBTable.Destroy;
var
  i: Integer;
begin
  for i:=0 to FList.Count - 1 do
  begin
    TDataMBRecord(FList[i]).Free;
  end;

  FreeAndNil(FList);

  inherited Destroy;
end;

function TDataMBTable.Add(_record: TDataMBRecord): String;
begin
  FList.Add(_record);
  Result:=_record.MBVersion;
end;

function TDataMBTable.HasRecord(_index: String): Boolean;
begin
  try
    if Assigned(GetRecord(_index)) then
    begin
      Result:=True;
    end;
  except
    on E:ExceptionDataNotFound do
    begin
      Result:=False;
    end;
  end;
end;

procedure TDataMBTable.Delete(_index: String);
var
  rec: TDataMBRecord;
begin
  if HasRecord(_index) then
  begin
    rec:=GetRecord(_index);
    FreeAndNil(rec);
    FList.Delete(FList.IndexOf(rec));
  end;
end;

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

function TDataWarehouse.GetMBTable: TDataMBTable;
var
  i: Integer;
begin
  if not Assigned(FMBTable) then
  begin
    FMBTable:=TDataMBTable.Create;

    FCSV.Clear;
    FCSV.LoadFromFile(FDBDir);

    for i:=0 to FCSV.RowCount - 1 do
    begin
      FMBTable.Add(TDataMBRecord.Create(
          FCSV.Cells[0, i],
          FCSV.Cells[1, i],
          FCSV.Cells[2, i],
          FCSV.Cells[3, i]
          ));
    end;
  end;

  Result:=FMBTable;
end;

constructor TDataWarehouse.Create(_dbName: String = '');
begin
  inherited Create;

  if _dbName <> '' then
  begin
    FDBName:=_dbName;
  end
  else
  begin
    FDBName:=defaultDBName;
  end;

  FDBDir:=GetCurrentDir() + FDBName;
  FCSV:=TCSVDocument.Create;
  FCSV.Delimiter:=';';
  initDatabase;
end;

destructor TDataWarehouse.Destroy;
begin
  FreeAndNil(FCSV);
  FreeAndNil(FMBTable);
  inherited Destroy;
end;

procedure TDataWarehouse.initDatabase;
begin
  if not FileExists(FDBDir) then
  begin
    if ForceDirectories(ExtractFileDir(FDBDir)) then
    begin
      FCSV.Cells[0, 0]:='mbversion';
      FCSV.Cells[1, 0]:='updater';
      FCSV.Cells[2, 0]:='param_fmt';
      FCSV.Cells[3, 0]:='firmware';
      FCSV.SaveToFile(FDBDir);
    end;
  end
end;

initialization

end.

