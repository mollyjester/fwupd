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
    function getMBVersions: TStringList;
  public
    var defaultConnectorType: String; static;
    property mbVersions: TStringList read getMBVersions;
  end;

implementation

{ TDataWarehouse }

function TDataWarehouse.getMBVersions: TStringList;
var
  sqlConnector: TSQLConnector;
begin
  getMBVersions:=TStringList.Create;
  sqlConnector:=TSQLConnector.Create(nil);

  try

  finally
    FreeAndNil(sqlConnector);
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
  updateDefaultConnectorType;

end.

