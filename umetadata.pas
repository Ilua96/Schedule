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

  TTableInf = class
    Name: String;
    Caption: String;
    Columns: array of TColumnInf;
    procedure AddColumnsInf(
      AName: String;
      ACaption: String;
      AWidth: Integer;
      AVisible: Boolean;
      AReferenceTableName: String = '';
      AReferenceColumnFName: String = '';
      AReferenceColumnSName: String = '';
      AReferenceColumnCaption: String = '';
      AReferenceColumnWidth: Integer = 0
    );
  end;

  { TTables }

  TTables = class
    TablesInf: array of TTableInf;
    function AddTableInf(AName, ACaption: String): TTableInf;
  end;

var
  Tables: TTables;

implementation

{ TTables }

function TTables.AddTableInf(AName, ACaption: String): TTableInf;
begin
  SetLength(TablesInf, Length(TablesInf) + 1);
  TablesInf[High(TablesInf)] := TTableInf.Create;
  With TablesInf[High(TablesInf)] do
    begin
      Name := AName;
      Caption := ACaption;
    end;
  result := TablesInf[High(TablesInf)];
end;

procedure TTableInf.AddColumnsInf(
  AName: String;
  ACaption: String;
  AWidth: Integer;
  AVisible: Boolean;
  AReferenceTableName: String = '';
  AReferenceColumnFName: String = '';
  AReferenceColumnSName: String = '';
  AReferenceColumnCaption: String = '';
  AReferenceColumnWidth: Integer = 0
);
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

initialization

Tables := TTables.Create;
With Tables do
begin
  With AddTableInf('Students', 'Студенты') do
  begin
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Имя', 150, True);
    AddColumnsInf('Group_ID', 'ИН группы', 40, False, 'Groups', 'ID', 'Number',
        'Имя Группы', 60);
  end;

  With AddTableInf('Groups', 'Группы') do
  begin
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Number', 'Номер', 60, True);
    AddColumnsInf('Name', 'Имя', 150, True);
  end;

  With AddTableInf('Teachers', 'Учителя') do
  begin
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Имя', 185, True);
  end;

  With AddTableInf('Subjects', 'Предметы') do
  begin
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Имя', 180, True);
  end;

  With AddTableInf('Audiences', 'Аудитории') do
  begin
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Имя', 50, True);
  end;

  With AddTableInf('Teachers_subjects', 'Учителя по предметам') do
  begin
    AddColumnsInf('Teacher_ID', 'ИН учителя', 50, False, 'Teachers', 'ID',
      'Name', 'Имя Учителя', 185);
    AddColumnsInf('Subject_ID', 'ИН предмета', 50, False, 'Subjects', 'ID',
      'Name', 'Имя Предмета', 180);
  end;

  With AddTableInf('Group_subjects', 'Предметы по группам') do
  begin
    AddColumnsInf('Group_ID', 'ИН группы', 50, False, 'Groups', 'ID', 'Name',
      'Имя', 150);
    AddColumnsInf('Subject_ID', 'ИН предмета', 50, False, 'Subjects', 'ID', 'Name',
      'Имя', 180);
  end;

  With AddTableInf('Weekdays', 'Дни недели') do
  begin
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Название', 70, True);
    AddColumnsInf('Number', 'Номер', 70, True);
  end;

  With AddTableInf('Pairs', 'Пары') do
  begin
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Begin_Pair', 'Начало', 60, True);
    AddColumnsInf('End_Pair', 'Конец', 60, True);
    AddColumnsInf('Number', 'Номер', 30, True);
  end;

  With AddTableInf('Educ_Activities', 'Образовательная деятельность') do
  begin
    AddColumnsInf('ID', 'ИН', 30, False);
    AddColumnsInf('Name', 'Название', 50, True);
  end;

  With AddTableInf('Schedules', 'Расписание') do
  begin
    AddColumnsInf('Group_ID', 'ИН группы', 50, False, 'Groups', 'ID',
      'Number', 'Имя группы', 150);
    AddColumnsInf('Weekday_ID', 'ИН недели', 50, False, 'Weekdays', 'ID',
      'Name', 'День недели', 70);
    AddColumnsInf('Pair_ID', 'ИН пары', 50, False, 'Pairs', 'ID',
      'Begin_Pair', 'Начало', 60);
    AddColumnsInf('Subject_ID', 'ИН предмета', 50, False, 'Subjects', 'ID',
      'Name', 'Имя предмета', 180);
    AddColumnsInf('Educ_ID', 'ИН пары', 50, False, 'Educ_Activities', 'ID',
      'Name', 'Вид', 50);
    AddColumnsInf('Teacher_ID', 'ИН учителя', 50, False, 'Teachers', 'ID',
      'Name', 'Имя учителя', 185);
    AddColumnsInf('Audience_ID', 'ИН класса', 50, False, 'Audiences', 'ID',
      'Name', 'Имя аудитории', 50);
  end;
end;

end.

