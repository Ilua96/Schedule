unit USQLRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetaData, Dialogs;

type

  { TSQLRequest }

  TSQLRequest = class
    function CreateQuery(ATag: Integer): String;
    function SortField(AText, AName: String; ADesc: Boolean): String;
    function AddFilter(AText, AName, ASign, AParameter: String): String;
  end;

var
  SQLRequest: TSQLRequest;

implementation

{ TSQLRequest }

function TSQLRequest.CreateQuery(ATag: Integer): String;
var
  i: Integer;

begin
  With Tables.TablesInf[ATag] do
  begin
    Result := 'Select ';
    for i := 0 to High(Columns) do
    begin
      Result := Result + Name + '.' + Columns[i].Name;
      if Columns[i].ReferenceTableName <> '' then
        Result := Result + ',' + Columns[i].ReferenceTableName + '.' +
          Columns[i].ReferenceColumnSName;
      if i <> High(Columns) then
        Result := Result + ',';
    end;
    Result := Result + ' From ' + Name + ' ';
    for i := 0 to High(Columns) do
      if Columns[i].ReferenceTableName <> '' then
      begin
        Result := Result + 'Inner join ' +  Columns[i].ReferenceTableName +
          ' on ' + Name + '.' + Columns[i].Name + ' = ' +
        Columns[i].ReferenceTableName + '.' +
        Columns[i].ReferenceColumnFName + ' ';
      end;
  end;
end;

function TSQLRequest.SortField(AText, AName: String; ADesc: Boolean): String;
var
  position: Integer;

begin
  if ADesc then
    Result := AText + 'Order by ' + AName + ' Desc'
  else
    Result := AText + 'Order by ' + AName;
end;

function TSQLRequest.AddFilter(AText, AName, ASign, AParameter: String): String;
begin
  if Pos('Where', AText) = 0 then
    Result := AText + 'Where ' + AName + ' ' + ASign + AParameter
  else
    Result := AText + 'And ' + AName + ' ' + ASign + AParameter;
end;

end.

