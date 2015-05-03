unit UListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, DbCtrls, Menus, StdCtrls, ExtCtrls, UMetaData, USQLRequest, Grids,
  UEdit;

type

  TFilterPanel = class(TPanel)
    NameComboBox: TComboBox;
    SignComboBox: TComboBox;
    CancelButton: TButton;
    DeleteButton: TButton;
    ExecuteButton: TButton;
    ValueEdit: TEdit;
    function CreateComboBox(ALeft: Integer): TCombobox;
    function CreateEdit: TEdit;
    function CreateButton(ALeft: Integer; ACaption: String): TButton;
    procedure ExecuteFilter(Sender: TObject);
    procedure ChangeValue(Sender: TObject);
    procedure CancelFilter(Sender: TObject);
  end;

  { TListViewForm }

  TListViewForm = class(TForm)
    AddFilterButton: TButton;
    AddRecButton: TButton;
    CancelFiltersButton: TButton;
    DeleteFiltersButton: TButton;
    ExecuteFiltersButton: TButton;
    DataSource: TDataSource;
    DBNavigator: TDBNavigator;
    ScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    DBGrid: TDBGrid;
    function KnowName(ATag: Integer): String;
    procedure AddFilterButtonClick(Sender: TObject);
    procedure AddRecButtonClick(Sender: TObject);
    procedure CancelFiltersButtonClick(Sender: TObject);
    procedure ChangeColumns(ATag: Integer);
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure DeleteFilter(Sender: TObject; Button: TMouseButton;
                           Shift: TShiftState; X, Y: Integer);
    constructor CreateAndShowForm(ATag: Integer);
    procedure DeleteFiltersButtonClick(Sender: TObject);
    procedure ExecuteFiltersButtonClick(Sender: TObject);
    procedure MakeQuery;
    procedure EditClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FilterPanels: array of TFilterPanel;
    SortFieldTag: Integer;
    SortField, SortDesc: Boolean;
    EditsForm: array of TEditForm;
  end;

Const indent = 37;
Const NumberOfSigns = 6;

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
        SQL.Text := SQLRequest.CreateQuery(ATag);
        Active := True;
      end;
      ChangeColumns(ATag);
    end;
  end;
  ListViewForm[ATag].Show;
end;

function TListViewForm.KnowName(ATag: Integer): String;
begin
  With Tables.TablesInf[Self.Tag].Columns[ATag] do
    if ReferenceTableName <> '' then
      Result := ReferenceTableName + '.' + ReferenceColumnSName
    else
      Result := Self.Name + '.' + Name;
end;

procedure TListViewForm.MakeQuery;
var
  Signs: array[0..NumberOfSigns] of string =
    ('< :', '> :', '= :', '<= :', '>= :', 'starts with :', 'containing :');
  i, j: Integer;
  AName: String;

begin
  With SQLQuery do
  begin
    Active := False;
    Prepare;
    SQL.Text := SQLRequest.CreateQuery(Self.Tag) ;
    for i := 0 to High(FilterPanels) do
      With FilterPanels[i] do
        if not ExecuteButton.Enabled then
        begin
          for j := 0 to High(Tables.TablesInf[Self.Tag].Columns) do
            With Tables.TablesInf[Self.Tag].Columns[j] do
              if (ReferenceColumnCaption = NameComboBox.Caption) or
                 (Caption = NameComboBox.Caption) then
                AName := KnowName(j);
          SQL.Text := SQLRequest.AddFilter(SQL.Text, AName,
            Signs[SignComboBox.ItemIndex], IntToStr(Params.Count));
          Params[Params.Count - 1].AsString :=  ValueEdit.Text;
        end;
    if SortField then
    begin
      AName := KnowName(SortFieldTag);
      SQL.Text := SQLRequest.SortField(SQL.Text, AName, SortDesc);
    end;
    Active := True;
  end;
  ChangeColumns(Tag);
end;

procedure TListViewForm.DeleteFiltersButtonClick(Sender: TObject);
var
  i: Integer;

begin
  for i := 0 to High(FilterPanels) do
    DeleteFilter((FilterPanels[0].DeleteButton as TObject), mbLeft, [], 0, 0);
  SetLength(FilterPanels, 0);
end;

procedure TListViewForm.ExecuteFiltersButtonClick(Sender: TObject);
var
  i: Integer;

begin
  for i := 0 to High(FilterPanels) do
    FilterPanels[i].ExecuteFilter(Sender);;
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

begin
  for i := 0 to High(EditsForm) do
    if DBGrid.DataSource.DataSet.Fields[0].Value = EditsForm[i].Tag then
    begin
      IndexEdit := i;
      BoolEdit := True;
    end;
  if not BoolEdit then
  begin
    SetLength(EditsForm, Length(EditsForm) + 1);
    IndexEdit := High(EditsForm);
    EditsForm[IndexEdit] := TEditForm.Create(Self);
    EditsForm[IndexEdit].Tag := IndexEdit;
    EditsForm[IndexEdit].OnClose := @EditClose;
  end;
  With EditsForm[IndexEdit] do
    ShowForm(Self.Tag, DBGrid.DataSource.DataSet.Fields[0].Value,
      Self.SQLQuery, False, DBGrid).Show;
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

procedure TListViewForm.AddFilterButtonClick(Sender: TObject);
var
  i: Integer;
  Signs: array[0..NumberOfSigns] of string =
    ('<', '>', '=', '<=', '>=', 'Начинается с', 'Включает');

begin
  SetLength(FilterPanels, Length(FilterPanels) + 1);
  FilterPanels[High(FilterPanels)] := TFilterPanel.Create(Self);
  With FilterPanels[High(FilterPanels)] do
  begin
    Visible := True;
    Height := 32;
    Width := 592;
    Top := 5 + indent * High(FilterPanels);
    Left := 10;
    Tag := High(FilterPanels);
    Parent := ScrollBox;
    Color := clRed;

    NameComboBox := CreateComboBox(6);
    With NameComboBox do
    begin
      for i := 0 to DBGrid.Columns.Count - 1 do
        if DBGrid.Columns[i].Visible then
          Items.Add(DBGrid.Columns[i].Title.Caption);
      ItemIndex := 0;
    end;

    SignComboBox := CreateComboBox(120);
    With SignComboBox do
    begin
      for i := 0 to NumberOfSigns do
        Items.Add(Signs[i]);
      ItemIndex := 0;
    end;

    ValueEdit := CreateEdit;

    ExecuteButton := CreateButton(352, 'Применить');
    ExecuteButton.OnClick := @ExecuteFilter;

    CancelButton := CreateButton(432, 'Отменить');
    CancelButton.OnClick := @CancelFilter;

    DeleteButton := CreateButton(512, 'Удалить');
    DeleteButton.OnMouseUp := @DeleteFilter;
  end;
end;

procedure TListViewForm.AddRecButtonClick(Sender: TObject);
begin
  SetLength(EditsForm, Length(EditsForm) + 1);
  EditsForm[High(EditsForm)] := TEditForm.Create(Self);
  EditsForm[IndexEdit].OnClose := @EditClose;
  With EditsForm[High(EditsForm)] do
    ShowForm(Self.Tag, DBGrid.DataSource.DataSet.Fields[0].Value, Self.SQLQuery,
      True, DBGrid).Show;
end;

procedure TListViewForm.CancelFiltersButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(FilterPanels) do
    FilterPanels[i].CancelFilter(Sender);
end;

procedure TListViewForm.DeleteFilter(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ATag, i: Integer;

begin
  ATag := (Sender as TButton).Parent.Tag;
  With FilterPanels[ATag] do
    CancelFilter(Sender);
  FreeAndNil(FilterPanels[ATag]);
  for i := ATag to High(FilterPanels) - 1 do
  begin
    FilterPanels[i] := FilterPanels[i + 1];
    With FilterPanels[i] do
    begin
      Top := Top - indent;
      Tag := Tag - 1;
    end;
  end;
  SetLength(FilterPanels, Length(FilterPanels) - 1);
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
      for j := 0 to High(EditsForm) do
        ListViewForm[i].EditsForm[j].RefreshComboBox;
    end;
end;

{ TFilterPanel }

procedure TFilterPanel.CancelFilter(Sender: TObject);
begin
  Color := clRed;
  ExecuteButton.Enabled := True;
  ListViewForm[Parent.Parent.Tag].MakeQuery;
end;

procedure TFilterPanel.ChangeValue(Sender: TObject);
begin
  Color := clRed;
  ExecuteButton.Enabled := True;
end;


procedure TFilterPanel.ExecuteFilter(Sender: TObject);
begin
  ExecuteButton.Enabled := False;
  ListViewForm[Parent.Parent.Tag].MakeQuery;
  Color := clGreen;
end;

function TFilterPanel.CreateComboBox(ALeft: Integer): TCombobox;
begin
  Result := TComboBox.Create(Self);
  With Result do
  begin
    Visible := True;
    Left := ALeft;
    Top := 6;
    Width := 113;
    Height := 23;
    ReadOnly := True;
    Style := csDropDownList;
    Parent := Self;
    OnChange := @ChangeValue;
  end;
end;

function TFilterPanel.CreateEdit: TEdit;
begin
  Result := TEdit.Create(Self);
  With Result do
  begin
    Visible := True;
    Left := 240;
    Top := 6;
    Width := 103;
    Height := 23;
    Parent := Self;
    OnChange := @ChangeValue;
  end;
end;

function TFilterPanel.CreateButton(ALeft: Integer; ACaption: String): TButton;
begin
  Result := TButton.Create(Self);
  With Result do
  begin
    Visible := True;
    Left := ALeft;
    Top := 6;
    Width := 75;
    Height := 25;
    Caption := ACaption;
    Parent := Self;
  end;
end;

end.

