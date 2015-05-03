unit USQLRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetaData, Dialogs, sqldb;

type

  { TSQLRequest }

  TSQLRequest = class
    function CreateQuery(ATag: Integer): String;
    function SortField(AText, AName: String; ADesc: Boolean): String;
    function AddFilter(AText, AName, ASign, AParameter: String): String;
    function QueryForChangeInf(ATag: Integer; ASQLQuery: TSQLQuery): String;
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

function TSQLRequest.QueryForChangeInf(ATag: Integer; ASQLQuery: TSQLQuery): String;
var
  i: Integer;
  s: String = 'CAST(%s.%s AS VARCHAR(100)) = ''%s'' AND ';

begin
  Result := 'Select ';
  With Tables.TablesInf[ATag] do
  begin
    for i := 0 to High(Columns) do
      if Columns[i].Visible then
        Result += Columns[i].Name + ','
      else
        if Columns[i].ReferenceColumnFName <> '' then
          Result += Columns[i].Name + ',';
    Delete(Result, Length(Result), 1);
    Result += ' From ' + Name;
    Result += ' Where ';
    for i := 0 to High(Columns) do
      if Columns[i].Visible then
        Result += Format(s, [Name, Columns[i].Name,
          ASQLQuery.FieldByName(Columns[i].Name).AsString])
      else
        if Columns[i].ReferenceColumnFName <> '' then
          Result += Format(s, [Name, Columns[i].Name,
            ASQLQuery.FieldByName(Columns[i].Name).AsString]);
  end;
  Delete(Result, Length(Result) - 4, 4);
end;

end.

