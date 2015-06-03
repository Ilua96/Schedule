unit UListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, DbCtrls, Menus, StdCtrls, ExtCtrls, UMetaData, USQLRequest, Grids,
  UEdit, UFilter;

type

  TEvent = Procedure of Object;

  { TListViewForm }

  TListViewForm = class(TForm)
    AddFilterButton: TButton;
    AddRecButton: TButton;
    CancelFiltersButton: TButton;
    DeleteFiltersButton: TButton;
    ExecuteFiltersButton: TButton;
    DataSource: TDataSource;
    DBNavigator: TDBNavigator;
    SQLQuery: TSQLQuery;
    DBGrid: TDBGrid;
    function KnowName(ATag: Integer): String;
    procedure AddRecButtonClick(Sender: TObject);
    procedure ChangeColumns(ATag: Integer);
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    constructor CreateAndShowForm(ATag: Integer);
    procedure MakeQuery;
    procedure EditClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ImportFilters(AXComboBox, AYComboBox: TComboBox;
  AFilterListBox: TFilterListBox; AColValue, ARowValue: String);
  private
    FilterListBox: TFilterListBox;
    SortFieldTag: Integer;
    SortField, SortDesc: Boolean;
    EditsForm: array of TEditForm;
  end;

Const indent = 37;
Const NumberOfSigns = 6;

var
  ListViewForm: array of TListViewForm;
  s2: String;

implementation

{$R *.lfm}

{ TListViewForm }

constructor TListViewForm.CreateAndShowForm(ATag: Integer);
begin
  Tag := ATag;
  Caption := Tables.TablesInf[ATag].Caption;
  With SQLQuery do
  begin
    Active := False;
    SQL.Text := SQLRequest.CreateQuery(ATag);
    Active := True;
  end;
  ChangeColumns(ATag);
  FilterListBox := TFilterListBox.Create(Self);
  With FilterListBox do
  begin
    Width := 624;
    Height := 508;
    Left := 610;
    Top := 40;
    Anchors := [akTop,akRight,akBottom];
    Tag := ATag;
    Parent := Self;
    AddFilterButton.OnClick := @AddFilterButtonClick;
    ExecuteFiltersButton.OnClick := @ExecuteFiltersButtonClick;
    DeleteFiltersButton.OnClick := @DeleteFiltersButtonClick;
    CancelFiltersButton.OnClick := @CancelFiltersButtonClick;
    QueryEvent := @MakeQuery;
  end;
  Show;
end;

function TListViewForm.KnowName(ATag: Integer): String;
begin
  With Tables.TablesInf[Self.Tag].Columns[ATag] do
    if ReferenceTableName <> '' then
      Result := ReferenceTableName + '.' + ReferenceColumnSName
    else
      Result := Tables.TablesInf[Self.Tag].Name + '.' + Name;
end;

procedure TListViewForm.MakeQuery;
var
  AName: String;

begin
  With SQLQuery do
  begin
    Active := False;
    Prepare;
    SQL.Text := SQLRequest.CreateQuery(Self.Tag) ;
    SQLRequest.WriteFilters(SQLQuery, FilterListBox);
    if SortField then
    begin
      AName := KnowName(SortFieldTag);
      SQL.Text := SQLRequest.SortField(SQL.Text, AName, SortDesc);
    end;
    Active := True;
  end;
  ChangeColumns(Tag);
end;

procedure TListViewForm.ChangeColumns(ATag: Integer);
var
  i: Integer = 0;
  j: Integer = 0;

begin
  With Tables.TablesInf[ATag] do
    for i := 0 to High(Columns) do
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
      inc(j);
    end;
end;

procedure TListViewForm.DBGridDblClick(Sender: TObject);
var
  i, IndexEdit: Integer;
  BoolEdit: Boolean = False;
  AStringList: TStringList;

begin
  for i := 0 to High(EditsForm) do
    if DBGrid.DataSource.DataSet.Fields[0].Value = EditsForm[i].RecID then
    begin
      IndexEdit := i;
      BoolEdit := True;
    end;
  if not BoolEdit then
  begin
    SetLength(EditsForm, Length(EditsForm) + 1);
    IndexEdit := High(EditsForm);
    EditsForm[IndexEdit] := TEditForm.Create(Self);
    EditsForm[IndexEdit].OnClose := @EditClose;
  end;
  AStringList := TStringList.Create;
  With DBGrid.DataSource.DataSet do
    for i := 0 to Fields.Count - 1 do
      if DBGrid.Columns[i].Visible then
        AStringList.Append(Fields[i].Value);
  With EditsForm[IndexEdit] do
    ShowForm(Self.Tag, DBGrid.DataSource.DataSet.Fields[0].Value, AStringList,
      False).Show;
end;

procedure TListViewForm.DBGridTitleClick(Column: TColumn);
begin
  if SortFieldTag <> Column.Tag then
  begin
    SortField := False;
    SortDesc := False;
  end;
  if not SortField then
    SortField := True
  else
    if SortDesc then
    begin
      SortField := False;
      SortDesc := False
    end
    else
      SortDesc := True;
  SortFieldTag := Column.Tag;
  MakeQuery;
end;

procedure TListViewForm.AddRecButtonClick(Sender: TObject);
var
  i: Integer;
  AStringList: TStringList;

begin
  SetLength(EditsForm, Length(EditsForm) + 1);
  EditsForm[High(EditsForm)] := TEditForm.Create(Self);
  With EditsForm[High(EditsForm)] do
  begin
    OnClose := @EditClose;
    Tag := High(EditsForm);
    AStringList := TStringList.Create;
    for i := 0 to DBGrid.Columns.Count - 1 do
      if DBGrid.Columns[i].Visible then
      AStringList.Append(DBGrid.DataSource.DataSet.Fields[i].Value);
    ShowForm(Self.Tag, DBGrid.DataSource.DataSet.Fields[0].Value, AStringList,
      True).Show;
  end;
end;

procedure TListViewForm.EditClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i, j: Integer;

begin
  for i := (Sender as TForm).Tag to High(EditsForm) - 1 do
    EditsForm[i] := EditsForm[i + 1];
  SetLength(EditsForm, Length(EditsForm) - 1);
  for i := 0 to High(ListViewForm) do
    if ListViewForm[i] <> nil then
    begin
      ListViewForm[i].MakeQuery;
      for j := 0 to High(ListViewForm[i].EditsForm) do
        ListViewForm[i].EditsForm[j].RefreshComboBox(ListViewForm[i].Tag);
    end;
end;

procedure TListViewForm.ImportFilters(AXComboBox, AYComboBox: TComboBox;
  AFilterListBox: TFilterListBox; AColValue, ARowValue: String);
var
  i: Integer;
  procedure FillFilter(AFilterPanel: TFilterPanel; AName, ASign:Integer;
    AValue: String);
  begin
    With AFilterPanel do
    begin
      NameComboBox.Itemindex := AName;
      ValueEdit.Text := AValue;
      SignComboBox.ItemIndex := ASign;
    end;
  end;

begin
  With FilterListBox, Tables.TablesInf[Self.Tag]  do
  begin
    AddFilterButtonClick(Self);
    FillFilter(FilterPanels[0], AXComboBox.ItemIndex, 2, AColValue);
    AddFilterButtonClick(Self);
    FillFilter(FilterPanels[1], AYComboBox.ItemIndex, 2, ARowValue);
    for i := 2 to High(AFilterListBox.FilterPanels) + 2 do
    begin
      AddFilterButtonClick(Self);
      With AFilterListBox.FilterPanels[i - 2] do
        FillFilter(FilterPanels[i], NameComboBox.ItemIndex,
          SignComboBox.ItemIndex, ValueEdit.Text);
    end;
    ExecuteFiltersButtonClick(Self);
  end;
end;

end.

