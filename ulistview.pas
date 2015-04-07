unit UListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, DbCtrls, Menus, UMetaData;

type

  { TListViewForm }

  TListViewForm = class(TForm)
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    SQLQuery: TSQLQuery;
    constructor CreateAndShowForm(ATag: Integer);
    procedure ChangeColumns(ATag: Integer);
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

          SQLQuery.Active := False;
          SQLQuery.SQL.Text := 'Select * From ' +  Name;
          SQLQuery.Active := True;

          ChangeColumns(ATag);
        end;
    end;
  ListViewForm[ATag].Show;
end;

procedure TListViewForm.ChangeColumns(ATag: Integer);
var
  i: Integer;

begin
  With Tables.TablesInf[ATag] do
    for i := 0 to Length(Columns) - 1 do
      begin
        DBGrid.Columns[i].FieldName := Columns[i].Name;
        DBGrid.Columns[i].Title.Caption := Columns[i].Caption;
        DBGrid.Columns[i].Width := Columns[i].Width;
      end;
end;

end.

