unit datawh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CsvDoc;

type

  { TDataWarehouse }

  TDataWarehouse = class(TObject)
  private
    FCSV: TCSVDocument;

    const dbName: String = '/db/';
    const tblMBVersion: String = 'mbver.csv';

    function getMBVersions: TStringList;
  protected
  public
    property mbVersions: TStringList read getMBVersions;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure initDatabase;
  end;

implementation

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

