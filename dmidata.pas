unit dmidata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

{$IFDEF GO32V2 OR $IFDEF MSDOS}
const dmiProc = 'dmidec~1.exe';
{$ELSE}{$IFDEF LINUX}
const dmiProc = '/usr/sbin/dmidecode';
{$ENDIF}{$ENDIF}


type
  
  { TDMIData }

  TDMIData = class(TObject)
  private
    const cmdMBVersion: String = '-s baseboard-product-name'; //Z87-HD3
    const cmdMBManufacturer: String = '-s baseboard-manufacturer'; //Gigabyte Technology Co., Ltd.
    const cmdBIOSDate: String = '-s bios-release-date'; //05/16/2013

    function getDate: TDateTime;
    function getManufacturer: String;
    function getVersion: String;
    procedure ReadFile(_filename: String);
  public
    property mbVersion: String read getVersion;
    property biosDate: TDateTime read getDate;
    property mbManufacturer: String read getManufacturer;
  end;

implementation

{ TDMIData }

function TDMIData.getDate: TDateTime;
var
  sDate: String;
  fsSettings: TFormatSettings;
begin
  RunCommand(dmiProc, [cmdBIOSDate], sDate);
  fsSettings.ShortDateFormat:='m.d.y';
  getDate:=StrToDate(sDate, fsSettings);
end;

function TDMIData.getManufacturer: String;
begin
  RunCommand(dmiProc, [cmdMBManufacturer], getManufacturer);
end;

function TDMIData.getVersion: String;
begin
  RunCommand(dmiProc, [cmdMBVersion], getVersion);
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

