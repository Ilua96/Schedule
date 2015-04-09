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
    function AddColumnsInf(
      AName: String;
      ACaption: String;
      AWidth: Integer;
      AVisible: Boolean;
      AReferenceTableName: String = '';
      AReferenceColumnFName: String = '';
      AReferenceColumnSName: String = '';
      AReferenceColumnCaption: String = '';
      AReferenceColumnWidth: Integer = 0
    ): TTableInf;
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
  Result := TablesInf[High(TablesInf)];
end;

function TTableInf.AddColumnsInf(
  AName: String;
  ACaption: String;
  AWidth: Integer;
  AVisible: Boolean;
  AReferenceTableName: String = '';
  AReferenceColumnFName: String = '';
  AReferenceColumnSName: String = '';
  AReferenceColumnCaption: String = '';
  AReferenceColumnWidth: Integer = 0
): TTableInf;
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
  Result := Self;
end;

initialization

Tables := TTables.Create;
With Tables do
begin
  AddTableInf('Students', 'Студенты')
  .AddColumnsInf('Student_ID', 'ИН', 30, False)
  .AddColumnsInf('Student_Name', 'Имя', 150, True)
  .AddColumnsInf('Group_ID', 'ИН группы', 40, False, 'Groups', 'Group_ID',
                 'Group_Number', 'Имя Группы', 60);

  AddTableInf('Groups', 'Группы')
  .AddColumnsInf('Group_ID', 'ИН', 30, False)
  .AddColumnsInf('Group_Number', 'Номер', 60, True)
  .AddColumnsInf('Group_Name', 'Имя', 150, True);

  AddTableInf('Teachers', 'Учителя')
  .AddColumnsInf('Teacher_ID', 'ИН', 30, False)
  .AddColumnsInf('Teacher_Name', 'Имя', 185, True);

  AddTableInf('Subjects', 'Предметы')
  .AddColumnsInf('Subject_ID', 'ИН', 30, False)
  .AddColumnsInf('Subject_Name', 'Имя', 180, True);

  AddTableInf('Audiences', 'Аудитории')
  .AddColumnsInf('Audience_ID', 'ИН', 30, False)
  .AddColumnsInf('Audience_Name', 'Имя', 50, True);

  AddTableInf('Teachers_subjects', 'Учителя по предметам')
  .AddColumnsInf('Teacher_ID', 'ИН учителя', 50, False, 'Teachers',
                 'Teacher_ID', 'Teacher_Name', 'Имя Учителя', 185)
  .AddColumnsInf('Subject_ID', 'ИН предмета', 50, False, 'Subjects',
                 'Subject_ID', 'Subject_Name', 'Имя Предмета', 180);

  AddTableInf('Group_subjects', 'Предметы по группам')
  .AddColumnsInf('Group_ID', 'ИН группы', 50, False, 'Groups', 'Group_ID',
                 'Group_Name', 'Имя', 150)
  .AddColumnsInf('Subject_ID', 'ИН предмета', 50, False, 'Subjects',
                 'Subject_ID', 'Subject_Name', 'Имя', 180);

  AddTableInf('Weekdays', 'Дни недели')
  .AddColumnsInf('Weekday_ID', 'ИН', 30, False)
  .AddColumnsInf('Weekday_Name', 'Название', 70, True)
  .AddColumnsInf('Weekday_Number', 'Номер', 70, True);

  AddTableInf('Pairs', 'Пары')
  .AddColumnsInf('Pair_ID', 'ИН', 30, False)
  .AddColumnsInf('Begin_Pair', 'Начало', 60, True)
  .AddColumnsInf('End_Pair', 'Конец', 60, True)
  .AddColumnsInf('Pair_Number', 'Номер', 30, True);


  AddTableInf('Educ_Activities', 'Образовательная деятельность')
  .AddColumnsInf('Educ_ID', 'ИН', 30, False)
  .AddColumnsInf('Educ_Name', 'Название', 50, True);

  AddTableInf('Schedules', 'Расписание')
  .AddColumnsInf('Group_ID', 'ИН группы', 50, False, 'Groups', 'Group_ID',
                 'Group_Number', 'Имя группы', 150)
  .AddColumnsInf('Weekday_ID', 'ИН недели', 50, False, 'Weekdays', 'Weekday_ID',
                 'Weekday_Name', 'День недели', 70)
  .AddColumnsInf('Pair_ID', 'ИН пары', 50, False, 'Pairs', 'Pair_ID',
                 'Begin_Pair', 'Начало', 60)
  .AddColumnsInf('Subject_ID', 'ИН предмета', 50, False, 'Subjects',
                 'Subject_ID', 'Subject_Name', 'Имя предмета', 180)
  .AddColumnsInf('Educ_ID', 'ИН пары', 50, False, 'Educ_Activities', 'Educ_ID',
                 'Educ_Name', 'Вид', 50)
  .AddColumnsInf('Teacher_ID', 'ИН учителя', 50, False, 'Teachers',
                 'Teacher_ID', 'Teacher_Name', 'Имя учителя', 185)
  .AddColumnsInf('Audience_ID', 'ИН класса', 50, False, 'Audiences',
                 'Audience_ID', 'Audience_Name', 'Имя аудитории', 50);
end;

end.

