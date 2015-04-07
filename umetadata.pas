unit UMetaData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TColumnInf = record
    Name: String;
    Caption: String;
    Width: Integer;
  end;

  TTableInf = record
    Name: String;
    Caption: String;
    Columns: array of TColumnInf;
  end;

  { TTables }

  TTables = class
    TablesInf: array of TTableInf;
    procedure AddTableInf(AName, ACaption: String);
    procedure AddColumnsInf(AName, ACaption: String; AWidth: Integer);
  end;

var
  Tables: TTables;

implementation

{ TTables }

procedure TTables.AddTableInf(AName, ACaption: String);
begin
  SetLength(TablesInf, Length(TablesInf) + 1);
  With TablesInf[High(TablesInf)] do
    begin
      Name := AName;
      Caption := ACaption;
    end;
end;

procedure TTables.AddColumnsInf(AName, ACaption: String; AWidth: Integer);
begin
  With TablesInf[High(TablesInf)] do
    begin
      SetLength(Columns, Length(Columns) + 1);
      With Columns[High(Columns)] do
        begin
          Name := AName;
          Caption := ACaption;
          Width := AWidth;
        end;
    end;
end;

initialization

Tables := TTables.Create;
With Tables do
  begin
    AddTableInf('Students', 'Студенты');
    AddColumnsInf('ID', 'ИН', 30);
    AddColumnsInf('Name', 'Имя', 150);
    AddColumnsInf('Group_ID', 'ИН группы', 40);

    AddTableInf('Groups', 'Группы');
    AddColumnsInf('ID', 'ИН', 30);
    AddColumnsInf('Number', 'Номер', 60);
    AddColumnsInf('Name', 'Имя', 150);

    AddTableInf('Teachers', 'Учителя');
    AddColumnsInf('ID', 'ИН', 30);
    AddColumnsInf('Name', 'Имя', 185);

    AddTableInf('Subjects', 'Предметы');
    AddColumnsInf('ID', 'ИН', 30);
    AddColumnsInf('Name', 'Имя', 180);

    AddTableInf('Audiences', 'Аудитории');
    AddColumnsInf('ID', 'ИН', 30);
    AddColumnsInf('Name', 'Имя', 50);

    AddTableInf('Teachers_subjects', 'Учителя по предметам');
    AddColumnsInf('Teacher_ID', 'ИН учителя', 50);
    AddColumnsInf('Subject_ID', 'ИН предмета', 50);

    AddTableInf('Group_subjects', 'Предметы по группам');
    AddColumnsInf('Group_ID', 'ИН группы', 50);
    AddColumnsInf('Subject_ID', 'ИН предмета', 50);

    AddTableInf('Weekdays', 'Дни недели');
    AddColumnsInf('ID', 'ИН', 30);
    AddColumnsInf('Name', 'Название', 70);
    AddColumnsInf('Number', 'Номер', 70);

    AddTableInf('Pairs', 'Пары');
    AddColumnsInf('ID', 'ИН', 30);
    AddColumnsInf('Begin', 'Начало', 60);
    AddColumnsInf('End', 'Конец', 60);
    AddColumnsInf('Number', 'Номер', 30);

    AddTableInf('Schedules', 'Расписание');
    AddColumnsInf('Group_ID', 'ИН группы', 50);
    AddColumnsInf('Weekday_ID', 'ИН недели', 50);
    AddColumnsInf('Pair_ID', 'ИН пары', 50);
    AddColumnsInf('Subject_ID', 'ИН предмета', 50);
    AddColumnsInf('Educ_ID', 'ИН пары', 50);
    AddColumnsInf('Teacher_ID', 'ИН учителя', 50);
    AddColumnsInf('Class_ID', 'ИН класса', 50);
  end;
end.

