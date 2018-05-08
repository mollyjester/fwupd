unit dmidata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  
  { TDMIData }

  TDMIData = class(TObject)
  private
    function getVersion: String;
  public
    property mbVersion: String read getVersion;
  end;

implementation

{ TDMIData }

function TDMIData.getVersion: String;
begin
  getVersion:='DH3-Z77-V2';
end;

end.

