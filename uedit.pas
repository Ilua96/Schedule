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
    SQLQueryEdit: TSQLQuery;
    procedure AddButtonClick(Sender: TObject);
    constructor ShowForm(ATag, ARecID: Integer; AStringList: TStringList;
      AInsert: Boolean);
    function CreateEdit(AName, AText: String; ATop: Integer): TEdit;
    function CreateComboBox(ATop, ATag: Integer; ASQLQuery: TSQLQuery): TComboBox;
    function CreateSQLQuery(AFTag, ASTag: Integer): TSQLQuery;
    procedure DeleteButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure RefreshComboBox(ATag: Integer);
  public
    RecID: Integer;
  private
    TagListView: Integer;
    Edits: array of TEdit;
    ComboBoxs: array of TComboBox;
    ValuesStringList: TStringList;
  end;

  Const indent = 37;

var
  EditForm: TEditForm;

implementation

{$R *.lfm}

constructor TEditForm.ShowForm(ATag, ARecID: Integer; AStringList: TStringList;
  AInsert: Boolean);
var
  i: Integer;
  j: Integer = 0;

begin
  if RecID <> 0 then
    exit;
  RecID := ARecID;
  TagListView := ATag;
  Caption := 'Редактор записи';
  ValuesStringList := AStringList;
  With Tables.TablesInf[ATag] do
  begin
    for i := 0 to High(Columns) do
      if Columns[i].Visible then
      begin
        SetLength(Edits, Length(Edits) + 1);
        Edits[High(Edits)] := CreateEdit(Columns[i].Name,
          AStringList.Strings[i - 1], 5 + indent * j);
        inc(j);
      end
      else
      begin
        if Columns[i].ReferenceTableName <> '' then
        begin
          SetLength(ComboBoxs, Length(ComboBoxs) + 1);
          ComboBoxs[High(ComboBoxs)] := CreateComboBox(5 + indent * j, i,
            CreateSQLQuery(ATag, i));
          ComboBoxs[High(ComboBoxs)].Text := AStringList.Strings[i - 1];
          inc(j);
        end;
      end;
  end;
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
      With Tables.TablesInf[TagListView].Columns[ComboBoxs[i].Tag] do
      SQL.Text := SQL.Text + '(Select ' + ReferenceColumnFName +
        ' From ' + ReferenceTableName + ' Where ' +
        ReferenceColumnSName + ' = :' + IntToStr(Params.Count) + ')';
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
      With Tables.TablesInf[TagListView].Columns[ComboBoxs[i].Tag] do
        SQL.Text := SQL.Text + Name + ' = ' + '(Select ' +
          ReferenceColumnFName + ' From ' + ReferenceTableName + ' Where ' +
          ReferenceColumnSName + ' = :' + IntToStr(Params.Count) + ')';
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

procedure TEditForm.RefreshComboBox(ATag: Integer);
var
  i: Integer;
  j: Integer = 0;

begin
  With Tables.TablesInf[ATag] do
    for i := 0 to High(Columns) do
      if Columns[i].Visible then
      begin
        Edits[High(Edits)] := CreateEdit(Columns[i].Name,
          ValuesStringList.Strings[i - 1], 5 + indent * j);
        inc(j);
      end
      else
      begin
        if Columns[i].ReferenceTableName <> '' then
        begin
          ComboBoxs[High(ComboBoxs)] := CreateComboBox(5 + indent * j, i,
            CreateSQLQuery(ATag, i));
          ComboBoxs[High(ComboBoxs)].Text := ValuesStringList.Strings[i - 1];
          inc(j);
        end;
      end;
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

function TEditForm.CreateComboBox(ATop, ATag: Integer; ASQLQuery: TSQLQuery): TComboBox;
var
  i: Integer;

begin
  Result := TComboBox.Create(Self);
  With Result do
  begin
    Tag := ATag;
    Left := 5;
    Top := ATop;
    Width := 226;
    Style := csDropDownList;
    Parent := Self;
    With ASQLQuery do
      for i := 0 to RecordCount - 1 do
      begin
        Items.Add(Fields[0].Value);
        Next;
      end;
  end;
end;

function TEditForm.CreateSQLQuery(AFTag, ASTag: Integer): TSQLQuery;
begin
  Result := TSQLQuery.Create(Self);
  With Result do
  begin
    Close;
    DataBase := MDDBConnection.IBConnection;
    Transaction := MDDBConnection.SQLTransaction;
    With Tables.TablesInf[AFTag].Columns[ASTag] do
      SQL.Text := 'Select ' + ReferenceTableName + '.' + ReferenceColumnSName +
        ' From ' + ReferenceTableName;
    Open;
  end;
end;

end.

