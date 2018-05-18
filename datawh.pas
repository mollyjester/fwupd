unit datawh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbf, db;

type
  
  { TDataWarehouse }

  TDataWarehouse = class(TObject)
  private
    const version: Integer = 1;
    const dbName: String = '/db/';
    const tblMBVersion: String = 'mbversion.dbf';
    var fdbf: TDbf;
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
begin
  getMBVersions:=TStringList.Create;
  fdbf.TableName:=tblMBVersion;
  fdbf.Open;

  while not fdbf.EOF do
  begin
    getMBVersions.Append(fdbf.FieldByName('mbversion').AsString);
    fdbf.Next;
  end;

  fdbf.Close;
end;

constructor TDataWarehouse.Create;
begin
  inherited;
  fdbf:=TDbf.Create(nil);
  fdbf.FilePathFull:=GetCurrentDir() + dbName;
  fdbf.TableLevel:=3;
end;

destructor TDataWarehouse.Destroy;
begin
  FreeAndNil(fdbf);
  inherited Destroy;
end;

procedure TDataWarehouse.initDatabase;
begin
  if not FileExists(GetCurrentDir() + dbName + tblMBVersion) then begin
    fdbf.Exclusive:=True;
    fdbf.TableName:=tblMBVersion;
    with fdbf.FieldDefs do begin
//      Add('Id', ftAutoInc, 0, True);
      Add('mbVersion', ftString, 80, True);
    end;

    fdbf.CreateTable;
    fdbf.Close;
  end;
end;

initialization

end.

