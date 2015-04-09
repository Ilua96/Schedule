unit USQLRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetaData, Dialogs;

type

  { TSQLRequest }

  TSQLRequest = class
    function WriteQuery(ATag: Integer): String;
    function SortField(AText, AName: String): String;
  end;

var
  SQLRequest: TSQLRequest;

implementation

{ TSQLRequest }

function TSQLRequest.WriteQuery(ATag: Integer): String;
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

function TSQLRequest.SortField(AText, AName: String): String;
var
  position: Integer;

begin
  position := Pos('Order by ', AText);
  if position = 0 then
    Result := AText + 'Order by ' + AName
  else
  begin
    if Pos('Order by ' + AName, AText) = 0 then
    begin
      Delete(AText,position, Length(AText) - position);
      Result := AText + 'Order by ' + AName;
    end
    else
      if Pos('Desc', AText) = 0 then
        Result := AText + ' Desc'
      else
      begin
        Delete(AText, Pos(' Desc', AText), 5);
        Result := AText;
      end;
  end;
end;

end.

