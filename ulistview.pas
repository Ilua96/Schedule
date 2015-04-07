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
          SQLQuery.SQL.Text := SQLRequest.ChangeQuery(ATag);
          //SQLQuery.SQL.Text := 'Select * From ' +  Name;
          SQLQuery.Active := True;

          ChangeColumns(ATag);
        end;
    end;
  ListViewForm[ATag].Show;
end;

procedure TListViewForm.ChangeColumns(ATag: Integer);
var
  i, j: Integer;

begin
  i := 0;
  j := 0;
  With Tables.TablesInf[ATag] do
    While i <= High(Columns) do
      begin
        DBGrid.Columns[j].Visible := Columns[i].Visible;
        DBGrid.Columns[j].Title.Caption := Columns[i].Caption;
        DBGrid.Columns[j].Width := Columns[i].Width;
        if Columns[j].ReferenceTableName <> '' then
          begin
            inc(j);
            DBGrid.Columns[j].Title.Caption := Columns[i].ReferenceColumnCaption;
            DBGrid.Columns[j].Width := Columns[i].ReferenceColumnWidth;
          end;
        inc(i);
        inc(j);
      end;
end;

end.

