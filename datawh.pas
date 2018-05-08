unit datawh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, sqlite3conn;

procedure updateDefaultConnectorType(_contype: String = '');

type
  
  { TDataWarehouse }

  TDataWarehouse = class(TObject)
  private
    const version: Integer = 1;
    const dbName: String = 'fwupd_db';
    var fconnection: TSQLConnector;
    var ftransaction: TSQLTransaction;
    function getMBVersions: TStringList;
    function createConnection: TSQLConnector;
    function getQuery(_sql: String): TSQLQuery;
  protected
  public
    var defaultConnectorType: String; static;
    property mbVersions: TStringList read getMBVersions;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure initDatabase;
  end;

implementation

{ TDataWarehouse }

function TDataWarehouse.getMBVersions: TStringList;
var
  sqlQuery: TSQLQuery;
begin
  getMBVersions:=TStringList.Create;
  sqlQuery:=getQuery('select t.mbversion from mbversion t');

  try
    while not sqlQuery.Eof do
    begin
      getMBVersions.Append(sqlQuery.FieldByName('mbversion').AsString);
      sqlQuery.Next;
    end;

    sqlQuery.Close;
  finally
    FreeAndNil(sqlQuery);
  end;
end;

function TDataWarehouse.createConnection: TSQLConnector;
begin
  createConnection:=TSQLConnector.Create(nil);
  createConnection.ConnectorType:=TDataWarehouse.defaultConnectorType;
  createConnection.DatabaseName:=dbName;
end;

function TDataWarehouse.getQuery(_sql: String): TSQLQuery;
begin
  getQuery:=TSQLQuery.Create(fconnection);

  getQuery.SQL.Text:=_sql;
  getQuery.DataBase:=fconnection;
  getQuery.Transaction:=ftransaction;
  getQuery.Open;
end;

constructor TDataWarehouse.Create;
begin
  inherited;
  fconnection:=createConnection;
  ftransaction:=TSQLTransaction.Create(fconnection);
  fconnection.Transaction:=ftransaction;
end;

destructor TDataWarehouse.Destroy;
begin
  FreeAndNil(ftransaction);
  FreeAndNil(fconnection);
  inherited Destroy;
end;

procedure TDataWarehouse.initDatabase;
begin
  if not FileExists(dbName) then begin
    fconnection.Open;
    ftransaction.Active:=True;
    fconnection.ExecuteDirect('create table "mbversion"("mbversion" Char(128) not null primary key);');
    fconnection.ExecuteDirect('create unique index "mbversion_uidx" on "mbversion"("mbversion");');
    ftransaction.Commit;
  end;
end;

procedure updateDefaultConnectorType(_contype: String = '');
var
  slConTypes:TStringList;
  i: Integer;
  cnt: Integer;
  choice: Integer;
begin
  slConTypes:=TStringList.Create;
  GetConnectionList(slConTypes);

  if _contype <> '' then begin
    if slConTypes.IndexOf(_contype) <> -1 then begin
      TDataWarehouse.defaultConnectorType:=_contype;
    end
    else begin
      raise Exception.Create(Format('Unregistered connection type ''%s''', [_contype]));
    end;
  end
  else begin
    cnt:=slConTypes.Count;

    if cnt > 0 then begin
      writeln('Pick a connection type:');

      for i:=0 to cnt - 1 do begin
        writeln(i, '.', slConTypes[i]);
      end;

      ReadLn(choice);
      TDataWarehouse.defaultConnectorType:=slConTypes[choice];
    end;
  end;
end;

initialization

end.

