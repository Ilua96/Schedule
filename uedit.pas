unit UEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  StdCtrls, UMetaData, DBGrids, db, sqldb, mssqlconn, UDBConnection,
  USQLRequest;

type

  TEvent = Procedure of Object;

  { TEditForm }

  TEditForm = class(TForm)
    AddButton: TButton;
    DataSourceEdit: TDataSource;
    DeleteButton: TButton;
    SaveButton: TButton;
    Data_Source: TDataSource;
    SQLQuery: TSQLQuery;
    SQLQueryEdit: TSQLQuery;
    procedure AddButtonClick(Sender: TObject);
    constructor ShowForm(ATag, ARecID: Integer; ASQLQuery: TSQLQuery;
      AInsert: Boolean; ADBGrid: TDBGrid);
    function CreateEdit(AName, AText: String; ATop: Integer): TEdit;
    function CreateDBLookupComboBox(ATop: Integer;
      AListSource: TDataSource; AName, ADataField, AKeyField,
      AListFiled: string): TDBLookupComboBox;
    function CreateDataSource(ASQLQuery: TSQLQuery): TDataSource;
    function CreateSQLQuery(AName: String): TSQLQuery;
    procedure DeleteButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure RefreshComboBox;
  private
    RecID, TagListView: Integer;
    Edits: array of TEdit;
    ComboBoxs: array of TDBLookupComboBox;
    ValuesComboBoxs: array of String;
  end;

  Const indent = 37;

var
  EditForm: TEditForm;

implementation

{$R *.lfm}

constructor TEditForm.ShowForm(ATag, ARecID: Integer; ASQLQuery: TSQLQuery;
  AInsert: Boolean; ADBGrid: TDBGrid);
var
  i: Integer;
  j: Integer = 0;

begin
  if RecID <> 0 then
    exit;
  RecID := ARecID;
  TagListView := ATag;
  Caption := 'Редактор записи';
  With Tables.TablesInf[ATag] do
  begin
    for i := 0 to High(Columns) do
      if Columns[i].Visible then
      begin
        SetLength(Edits, Length(Edits) + 1);
        Edits[High(Edits)] := CreateEdit(Columns[i].Name,
          ADBGrid.DataSource.DataSet.Fields[i].Value, 5 + indent * j);
        inc(j);
      end
      else
      begin
        if Columns[i].ReferenceTableName <> '' then
        begin
          SetLength(ComboBoxs, Length(ComboBoxs) + 1);
          ComboBoxs[High(ComboBoxs)] := CreateDBLookupComboBox(5 + indent * j,
            CreateDataSource(CreateSQLQuery(Columns[i].ReferenceTableName)),
            Columns[i].ReferenceTableName, Columns[i].Name,
            Columns[i].ReferenceColumnFName, Columns[i].ReferenceColumnSName);
          inc(j);
        end;
      end;
  end;
  With SQLQuery do
  begin
    Close;
    SQL.Text := SQLRequest.QueryForChangeInf(ATag, ASQLQuery);
    Open;
  end;
  SetLength(ValuesComboBoxs, Length(ComboBoxs));
  for i := 0 to High(ComboBoxs) do
    ValuesComboBoxs[i] := ComboBoxs[i].Text;
  if AInsert then
  begin
    SaveButton.Visible := False;
    DeleteButton.Visible := False;
    AddButton.Visible := True;
  end;
end;

procedure TEditForm.AddButtonClick(Sender: TObject);
var
  i: Integer;
begin
  With SQLQueryEdit do
  begin
    Close;
    SQL.Text := 'Insert Into ' + Tables.TablesInf[TagListView].Name + ' Values ( null, ';
    for i := 0 to High(Edits) do
    begin
      SQL.Text := SQL.Text + ':' + IntToStr(Params.Count);
      Params[Params.Count - 1].AsString := Edits[i].Text;
      if (i <> High(Edits)) or (Length(ComboBoxs) <> 0) then
        SQL.Text := SQL.Text + ' , ';
    end;
    for i := 0 to High(ComboBoxs) do
    begin
      SQL.Text := SQL.Text + '(Select ' + ComboBoxs[i].KeyField +
        ' From ' + ComboBoxs[i].Name + ' Where ' +
        ComboBoxs[i].ListField + ' = :' + IntToStr(Params.Count) + ')';
      Params[Params.Count - 1].AsString := ComboBoxs[i].Text;
      if i <> High(ComboBoxs) then
        SQL.Text := SQL.Text + ' , ';
    end;
    SQL.Text := SQL.Text + ')';
    ExecSQL;
  end;
  MDDBConnection.SQLTransaction.Commit;
  Close;
end;

procedure TEditForm.DeleteButtonClick(Sender: TObject);
begin
  With SQLQueryEdit do
  begin
    Close;
    With Tables.TablesInf[TagListView] do
      SQL.Text := 'Delete From ' + Name + ' Where ' + Columns[0].Name  + ' = ' +
        IntToStr(RecID);
    ExecSQL;
  end;
  MDDBConnection.SQLTransaction.Commit;
  Close;
end;

procedure TEditForm.SaveButtonClick(Sender: TObject);
var
  i: Integer;

begin
  With SQLQueryEdit do
  begin
    Close;
    SQL.Text := 'Update ' + Tables.TablesInf[TagListView].Name + ' Set ';
    for i := 0 to High(Edits) do
    begin
      SQL.Text := SQL.Text + Edits[i].Name + ' = :' + IntToStr(Params.Count);
      Params[Params.Count - 1].AsString := Edits[i].Text;
      if (i <> High(Edits)) or (Length(ComboBoxs) <> 0) then
        SQL.Text := SQL.Text + ', ';
    end;
    for i := 0 to High(ComboBoxs) do
    begin
      SQL.Text := SQL.Text + ComboBoxs[i].DataField + ' = ' + '(Select ' +
        ComboBoxs[i].KeyField + ' From ' + ComboBoxs[i].Name + ' Where ' +
        ComboBoxs[i].ListField + ' = :' + IntToStr(Params.Count) + ')';
      Params[Params.Count - 1].AsString := ComboBoxs[i].Text;
      if i <> High(ComboBoxs) then
        SQL.Text := SQL.Text + ', ';
    end;
    SQL.Text := SQL.Text + ' Where ' +
      Tables.TablesInf[TagListView].Columns[0].Name + ' = ' + IntToStr(RecID);
    ExecSQL;
  end;
  MDDBConnection.SQLTransaction.Commit;
  Close;
end;

procedure TEditForm.RefreshComboBox;
var
  i, j: Integer;

begin
  for i := 0 to High(ComboBoxs) do
    ComboBoxs[i].Text := ValuesComboBoxs[i];
end;

function TEditForm.CreateEdit(AName, AText: String; ATop: Integer): TEdit;
begin
  Result := TEdit.Create(Self);
  With Result do
  begin
    Name := AName;
    Left := 5;
    Top := ATop;
    Width := 226;
    Text := AText;
    Parent := Self;
    Visible := True;
  end;
end;

function TEditForm.CreateDBLookupComboBox(ATop: Integer;
  AListSource: TDataSource; AName, ADataField, AKeyField,
  AListFiled: string): TDBLookupComboBox;
begin
  Result := TDBLookupComboBox.Create(Self);
  With Result do
  begin
    Name := AName;
    Left := 5;
    Top := ATop;
    Width := 226;
    Style := csDropDownList;
    DataSource := Data_Source;
    ListSource := AListSource;
    DataField := ADataField;
    KeyField := AKeyField;
    ListField := AListFiled;
    Parent := Self;
  end;
end;

function TEditForm.CreateDataSource(ASQLQuery: TSQLQuery): TDataSource;
begin
  Result := TDataSource.Create(Self);
  Result.DataSet := ASQLQuery;
end;

function TEditForm.CreateSQLQuery(AName: String): TSQLQuery;
begin
  Result := TSQLQuery.Create(Self);
  With Result do
  begin
    Close;
    DataBase := MDDBConnection.IBConnection;
    Transaction := MDDBConnection.SQLTransaction;
    SQL.Text := 'Select * From ' + AName;
    Open;
  end;
end;

end.

