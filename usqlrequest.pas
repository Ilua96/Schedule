unit USQLRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetaData, Dialogs;

type

  { TSQLRequest }

  TSQLRequest = class
    function ChangeQuery(ATag: Integer): String;
  end;

var
  SQLRequest: TSQLRequest;

implementation

{ TSQLRequest }

function TSQLRequest.ChangeQuery(ATag: Integer): String;
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

end.

