unit datawh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbf, db;

type

  { TTableFieldMeta }

  TTableFieldMeta = class(TObject)
  private
    FName: String;
    FFieldType: TFieldType;
    FSize: Word;
    FRequired: Boolean;
  public
    property Name: String read FName;
    property FieldType: TFieldType read FFieldType;
    property Size: Word read FSize;
    property Required: Boolean read FRequired;

    constructor Create(_name: String;
                       _fieldType: TFieldType;
                       _size: Word = 0;
                       _required: Boolean = True); virtual;
  end;
  
  { TDataWarehouse }

  TDataWarehouse = class(TObject)
  private
    fdbf: TDbf;

    const dbName: String = '/db/';
    const tblManufacturer: String = 'manufacturer.dbf';
    const tblMBVersion: String = 'mbversion.dbf';
    const tblBIOSDate: String = 'biosdate.dbf';

    function getMBVersions: TStringList;
    procedure createTable(_tablename: String;
                          const _fields: array of TTableFieldMeta);
  protected
  public
    property mbVersions: TStringList read getMBVersions;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure initDatabase;
  end;

implementation

{ TTableFieldMeta }

constructor TTableFieldMeta.Create(
  _name: String;
  _fieldType: TFieldType;
  _size: Word = 0;
  _required: Boolean = True);
begin
  inherited Create;
  FName:=_name;
  FFieldType:=_fieldType;
  FSize:=_size;
  FRequired:=_required;
end;

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

procedure TDataWarehouse.createTable(
  _tablename: String;
  const _fields: array of TTableFieldMeta);
var
  i: Integer;
  cnt: Integer;
  fieldMeta: TTableFieldMeta;
begin
  cnt:=Length(_fields);

  if (cnt > 0)
    and not FileExists(GetCurrentDir() + dbName + _tablename) then begin
      fdbf.Exclusive:=True;
      fdbf.TableName:=_tablename;

      fdbf.FieldDefs.Clear;

      for i:=0 to cnt - 1 do begin
        fieldMeta:=_fields[i];
        fdbf.FieldDefs.Add(fieldMeta.Name, fieldMeta.FieldType, fieldMeta.Size, fieldMeta.Required);
      end;

      fdbf.CreateTable;
      fdbf.Close;
    end;
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
  createTable(tblManufacturer, [TTableFieldMeta.Create('manufacturer', ftString, 100)]);
  createTable(tblMBVersion,
              [TTableFieldMeta.Create('manufacturer', ftString, 100),
               TTableFieldMeta.Create('mbVersion', ftString, 80)]);
  createTable(tblBIOSDate,
              [TTableFieldMeta.Create('mbVersion', ftString, 80),
               TTableFieldMeta.Create('BIOSDate', ftDateTime)]);
end;

initialization

end.

