unit UListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, DbCtrls, Menus, StdCtrls, ExtCtrls, UMetaData, USQLRequest;

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
  private
    ActiveQuery: String;
  end;

  { TListViewForm }

  TListViewForm = class(TForm)
    AddFilterButton: TButton;
    CancelFiltersButton: TButton;
    DeleteFiltersButton: TButton;
    ExecuteFiltersButton: TButton;
    DataSource: TDataSource;
    DBNavigator: TDBNavigator;
    ScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    DBGrid: TDBGrid;
    procedure AddFilterButtonClick(Sender: TObject);
    procedure CancelFiltersButtonClick(Sender: TObject);
    procedure ChangeColumns(ATag: Integer);
    procedure DBGridTitleClick(Column: TColumn);
    procedure DeleteFilter(Sender: TObject; Button: TMouseButton;
                           Shift: TShiftState; X, Y: Integer);
    constructor CreateAndShowForm(ATag: Integer);
    procedure DeleteFiltersButtonClick(Sender: TObject);
    procedure ExecuteFiltersButtonClick(Sender: TObject);
  private
    FilterPanels: array of TFilterPanel;
    ExecuteCount: Integer;
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
        SQL.Text := SQLRequest.WriteQuery(ATag);
        Active := True;
      end;
      ChangeColumns(ATag);
    end;
  end;
  ListViewForm[ATag].Show;
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

procedure TListViewForm.AddFilterButtonClick(Sender: TObject);
var
  i: Integer;
  Signs: array[0..NumberOfSigns] of string = ('<', '>', '=', '<=', '>=',
                                              'Начинается с', 'Включает');

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

    ExecuteButton := CreateButton(352, 'Выполнить');
    ExecuteButton.OnClick := @ExecuteFilter;

    CancelButton := CreateButton(432, 'Отменить');
    CancelButton.OnClick := @CancelFilter;

    DeleteButton := CreateButton(512, 'Удалить');
    DeleteButton.OnMouseUp := @DeleteFilter;
  end;
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

{ TFilterPanel }

procedure TFilterPanel.CancelFilter(Sender: TObject);
var
  ATag, i, k: Integer;
  m: Boolean;
  c: Integer = -1;

begin
  if ActiveQuery = '' then
    exit;
  m := False;
  ATag := Parent.Parent.Tag;
  With ListViewForm[ATag].SQLQuery do
  begin
    Active := False;
    SQL.Text := SQLRequest.DeleteFilter(SQL.Text, ActiveQuery);
    Active := True;
    ShowMessage(ActiveQuery);
    ShowMessage(SQL.Text);
  end;
  k := ExecuteButton.Tag;
  ExecuteButton.Tag := 0;
  With ListViewForm[ATag] do
  begin
    Dec(ExecuteCount);
    for i := 0 to High(FilterPanels) do
      if FilterPanels[i].ExecuteButton.Tag = 1 then
      begin
        m := True;
        Break;
      end;
    if not m then
      for i := 0 to High(FilterPanels) do
        if FilterPanels[i].ExecuteButton.Tag = 2 then
        begin
          c := i;
          Break;
        end;
    if (c <> -1) and (not m) then
    begin
      With FilterPanels[c] do
      begin
        Insert('Where', ActiveQuery, Pos('And', ActiveQuery));
        Delete(ActiveQuery, Pos('And', ActiveQuery), Length('And'));
      end;
    end;
    for i := 0 to High(FilterPanels) do
      With FilterPanels[i].ExecuteButton do
        if k < Tag then
          Tag := Tag - 1;
    ChangeColumns(ATag);
  end;
  ActiveQuery := '';
  Color := clRed;
  ExecuteButton.Enabled := True;
end;

procedure TFilterPanel.ChangeValue(Sender: TObject);
begin
  Color := clRed;
  ExecuteButton.Enabled := True;
end;


procedure TFilterPanel.ExecuteFilter(Sender: TObject);
var
  i: Integer;
  AName: String;
  Signs: array[0..NumberOfSigns] of string = ('< :', '> :', '= :', '<= :', '>= :',
                                              'like :', 'like :');

begin
  if not ExecuteButton.Enabled then
    exit;
  With ListViewForm[Parent.Parent.Tag] do
  begin
    inc(ExecuteCount);
    ExecuteButton.Tag := ExecuteCount;
  end;
  Color := clYellow;
  ExecuteButton.Enabled := False;
  With ListViewForm[Parent.Parent.Tag] do
  begin
    for i := 0 to DBGrid.Columns.Count - 1 do
      if DBGrid.Columns[i].Title.Caption = NameComboBox.Caption then
        With Tables.TablesInf[Tag] do
          if Columns[DBGrid.Columns[i].Tag].ReferenceTableName <> '' then
            AName := Columns[DBGrid.Columns[i].Tag].ReferenceTableName + '.' +
              Columns[DBGrid.Columns[i].Tag].ReferenceColumnSName
          else
            AName := Name + '.' + Columns[DBGrid.Columns[i].Tag].Name;
    With SQLQuery do
    begin
      Active := False;
      Prepare;
      SQL.Text := SQLRequest.AddFilter(SQL.Text, AName,
                    Signs[SignComboBox.ItemIndex], IntToStr(Params.Count),
                    ActiveQuery);
      if SignComboBox.ItemIndex = 5 then
        Params[Params.Count - 1].AsString :=  ValueEdit.Text + '%'
      else
        if SignComboBox.ItemIndex = 6 then
          Params[Params.Count - 1].AsString := '%' + ValueEdit.Text + '%'
        else
          Params[Params.Count - 1].AsString := ValueEdit.Text;
      Active := True;
    end;
    ChangeColumns(Tag);
  end;
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

