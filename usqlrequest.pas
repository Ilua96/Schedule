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
    function DeleteFilter(AText, AQuery: String): String;
    function AddFilter(AText, AName, ASign, AParameter: String;
      var AQuery: String): String;
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

function TSQLRequest.AddFilter(AText, AName, ASign, AParameter: String;
  var AQuery: String): String;
begin
  if AQuery <> '' then
    Delete(AText, Pos(AQuery, AText), Length(AQuery));
  if Pos('Order by', AText) = 0 then
  begin
    if Pos('Where', AText) = 0 then
    begin
      AQuery := 'Where ' + AName + ' ' + ASign + AParameter;
      Result := AText + ' ' + AQuery;
    end
    else
    begin
      AQuery := 'And ' + AName + ' ' + ASign + AParameter;
      Result := AText + ' ' + AQuery;
    end;
  end
  else
    if Pos('Where', AText) = 0 then
    begin
      AQuery := 'Where ' + AName + ' ' + ASign + AParameter;
      Insert(AQuery + ' ', AText, Pos('Order by', AText));
      Result := AText;
    end
    else
    begin
      AQuery := 'And ' + AName + ' ' + ASign + AParameter;
      Insert(AQuery + ' ', AText, Pos('Order by', AText));
      Result := AText;
    end;
end;

function TSQLRequest.DeleteFilter(AText, AQuery: String): String;
begin
  Delete(AText, Pos(AQuery, AText), Length(AQuery));
  if (Pos('And', AText) <> 0) and (Pos('Where', AText) = 0) then
  begin
    Insert('Where', AText, Pos('And', AText));
    Delete(AText, Pos('And', AText), Length('And'));
  end;
  Result := AText;
end;

end.

