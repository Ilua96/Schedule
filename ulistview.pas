unit UListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, DbCtrls, Menus, UMetaData, USQLRequest;

type

  { TListViewForm }

  TListViewForm = class(TForm)
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    SQLQuery: TSQLQuery;
    constructor CreateAndShowForm(ATag: Integer);
    procedure ChangeColumns(ATag: Integer);
    procedure DBGridTitleClick(Column: TColumn);
  end;

var
  ListViewForm: array of TListViewForm;

implementation

{$R *.lfm}

{ TListViewForm }

constructor TListViewForm.CreateAndShowForm(ATag: Integer);
begin
  if ListViewForm[ATag] = nil then
  begin
    ListViewForm[ATag] := TListViewForm.Create(Self);
    With ListViewForm[ATag] do
    begin
      Tag := ATag;
      Caption := Tables.TablesInf[ATag].Caption;
      Name := Tables.TablesInf[ATag].Name;
      With SQLQuery do
      begin
        Active := False;
        SQL.Text := SQLRequest.WriteQuery(ATag);
        Active := True;
      end;
      ChangeColumns(ATag);
    end;
  end;
  ListViewForm[ATag].Show;
end;

procedure TListViewForm.ChangeColumns(ATag: Integer);
var
  i: Integer = 0;
  j: Integer = 0;

begin
  With Tables.TablesInf[ATag] do
    While i <= High(Columns) do
    begin
      DBGrid.Columns[j].Visible := Columns[i].Visible;
      DBGrid.Columns[j].FieldName := Columns[i].Name;
      DBGrid.Columns[j].Title.Caption := Columns[i].Caption;
      DBGrid.Columns[j].Width := Columns[i].Width;
      DBGrid.Columns[j].Tag := i;
      if Columns[j].ReferenceTableName <> '' then
      begin
        inc(j);
        DBGrid.Columns[j].FieldName := Columns[i].ReferenceColumnSName;
        DBGrid.Columns[j].Title.Caption := Columns[i].ReferenceColumnCaption;
        DBGrid.Columns[j].Width := Columns[i].ReferenceColumnWidth;
        DBGrid.Columns[j].Tag := i;
      end;
      inc(i);
      inc(j);
    end;
end;

procedure TListViewForm.DBGridTitleClick(Column: TColumn);
var
  AName: String;

begin
  With Tables.TablesInf[Tag] do
  begin
    if Columns[Column.Tag].ReferenceTableName <> '' then
      AName := Columns[Column.Tag].ReferenceTableName + '.' + Column.FieldName
    else
      AName := Name + '.' + Column.FieldName;
  end;
  With SQLQuery do
  begin
    Active := False;
    SQL.Text := SQLRequest.SortField(SQL.Text, AName);
    Active := True;
  end;
  ChangeColumns(Tag);
end;

end.

