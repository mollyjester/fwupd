unit dmidata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$IFDEF WINDOWS}
const dmiProc = 'dmidec~1.exe';
{$ELSE}
const dmiProc = 'dmidecode';
{$ENDIF}


type
  
  { TDMIData }

  TDMIData = class(TObject)
  private
    const cmdMBVersion: String = '-s baseboard-product-name'; //Z87-HD3
    const cmdMBManufacturer: String = '-s baseboard-manufacturer'; //Gigabyte Technology Co., Ltd.
    const cmdBIOSDate: String = '-s bios-release-date'; //05/16/2013

    function getDate: TDateTime;
    function getVersion: String;
    procedure ReadFile(_filename: String);
  public
    property mbVersion: String read getVersion;
    property biosDate: TDateTime read getDate;
  end;

implementation

{ TDMIData }

function TDMIData.getDate: TDateTime;
var
  fsSettings: TFormatSettings;
begin
  fsSettings.ShortDateFormat:='d.m.y';
  getDate:=StrToDate('16.05.2013', fsSettings);
end;

function TDMIData.getVersion: String;
begin
  getVersion:='Z87-HD3';
end;

procedure TDMIData.ReadFile(_filename: String);
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

end.

