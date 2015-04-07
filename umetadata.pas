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
    Visible: Boolean;
    ReferenceTableName: String;
    ReferenceColumnFName: String;
    ReferenceColumnSName: String;
    ReferenceColumnCaption: String;
    ReferenceColumnWidth: Integer;
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
    procedure AddColumnsInf(AName, ACaption: String; AWidth: Integer;
      AVisible: Boolean; AReferenceTableName: String = '';
      AReferenceColumnFName: String = ''; AReferenceColumnSName: String = '';
      AReferenceColumnCaption: String = ''; AReferenceColumnWidth: Integer = 0);
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

procedure TTables.AddColumnsInf(AName, ACaption: String; AWidth: Integer;
  AVisible: Boolean; AReferenceTableName: String = '';
  AReferenceColumnFName: String = ''; AReferenceColumnSName: String = '';
  AReferenceColumnCaption: String = ''; AReferenceColumnWidth: Integer = 0);
begin
  With TablesInf[High(TablesInf)] do
    begin
      SetLength(Columns, Length(Columns) + 1);
      With Columns[High(Columns)] do
        begin
          Name := AName;
          Caption := ACaption;
          Width := AWidth;
          Visible := AVisible;
          ReferenceTableName := AReferenceTableName;
          ReferenceColumnFName := AReferenceColumnFName;
          ReferenceColumnSName := AReferenceColumnSName;
          ReferenceColumnCaption := AReferenceColumnCaption;
          ReferenceColumnWidth := AReferenceColumnWidth;
        end;
    end;
end;

initialization

Tables := TTables.Create;
With Tables do
  begin
    AddTableInf('Students', 'Студенты');
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Имя', 150, True);
    AddColumnsInf('Group_ID', 'ИН группы', 40, False, 'Groups', 'ID', 'Number',
      'Имя Группы', 60);

    AddTableInf('Groups', 'Группы');
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Number', 'Номер', 60, True);
    AddColumnsInf('Name', 'Имя', 150, True);

    AddTableInf('Teachers', 'Учителя');
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Имя', 185, True);

    AddTableInf('Subjects', 'Предметы');
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Имя', 180, True);

    AddTableInf('Audiences', 'Аудитории');
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Имя', 50, True);

    AddTableInf('Teachers_subjects', 'Учителя по предметам');
    AddColumnsInf('Teacher_ID', 'ИН учителя', 50, False, 'Teachers', 'ID', 'Name',
      'Имя Учителя', 185);
    AddColumnsInf('Subject_ID', 'ИН предмета', 50, False, 'Subjects', 'ID', 'Name',
      'Имя Предмета', 180);

    AddTableInf('Group_subjects', 'Предметы по группам');
    AddColumnsInf('Group_ID', 'ИН группы', 50, False, 'Groups', 'ID', 'Name',
      'Имя', 150);
    AddColumnsInf('Subject_ID', 'ИН предмета', 50, False, 'Subjects', 'ID', 'Name',
      'Имя', 180);

    AddTableInf('Weekdays', 'Дни недели');
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Название', 70, True);
    AddColumnsInf('Number', 'Номер', 70, True);

    AddTableInf('Pairs', 'Пары');
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Begin_Pair', 'Начало', 60, True);
    AddColumnsInf('End_Pair', 'Конец', 60, True);
    AddColumnsInf('Number', 'Номер', 30, True);

    AddTableInf('Educ_Activities', 'Образовательная деятельность');
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Название', 50, True);

    AddTableInf('Schedules', 'Расписание');
    AddColumnsInf('Group_ID', 'ИН группы', 50, False, 'Groups', 'ID', 'Number',
      'Имя группы', 150);
    AddColumnsInf('Weekday_ID', 'ИН недели', 50, False, 'Weekdays', 'ID', 'Name',
     'День недели', 70);
    AddColumnsInf('Pair_ID', 'ИН пары', 50, False, 'Pairs', 'ID', 'Begin_Pair',
      'Начало', 60);
    AddColumnsInf('Subject_ID', 'ИН предмета', 50, False, 'Subjects', 'ID', 'Name',
      'Имя предмета', 180);
    AddColumnsInf('Educ_ID', 'ИН пары', 50, False, 'Educ_Activities', 'ID', 'Name',
      'Вид', 50);
    AddColumnsInf('Teacher_ID', 'ИН учителя', 50, False, 'Teachers', 'ID', 'Name',
      'Имя учителя', 185);
    AddColumnsInf('Audience_ID', 'ИН класса', 50, False, 'Audiences', 'ID', 'Name',
      'Имя аудитории', 50);
  end;
end.

