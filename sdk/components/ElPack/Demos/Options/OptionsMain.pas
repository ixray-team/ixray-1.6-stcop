unit OptionsMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElXPThemedControl, ElTree, StdCtrls, Spin;

type
  TOptionsForm = class(TForm)
    OptionsTree: TElTree;
    procedure FormCreate(Sender: TObject);
    procedure OptionsTreeItemFocused(Sender: TObject);
    procedure OptionsTreeScroll(Sender: TObject;
      ScrollBarKind: TScrollBarKind; ScrollCode: Integer);
  private
    { Private declarations }
  public
    FocusedItem   : TElTreeItem;
    EditorControl : TWinControl;
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.DFM}

procedure TOptionsForm.FormCreate(Sender: TObject);
var i,
    j : integer;
    RootItem,
    Item : TElTreeItem;
begin
 for i := 0 to 2 do
 begin
   RootItem := OptionsTree.Items.AddItem(nil);
   RootItem.Text := 'Options group';
   for j := 0 to 3 do
   begin
     Item := OptionsTree.Items.AddItem(RootItem);
     Item.IsHTML := true;
     // This is just to add items of different kinds. In real applications you
     // will most likely add items in design-time
     Item.Tag := j;
     case j of
       0: Item.Text := '<html>Text option';
       1: Item.Text := '<html>Spin-button option';
       2: Item.Text := '<html>Checkbox option';
       3: Item.Text := '<html>Combo option';
     end;
     Item.ColumnText.Add('');
   end;
 end; 
end;

procedure TOptionsForm.OptionsTreeItemFocused(Sender: TObject);
var Item : TElTreeItem;
    R    : TRect;
begin
  if EditorControl <> nil then
  begin
    if FocusedItem <> nil then
    begin
      case FocusedItem.Tag of
        0: begin
             FocusedItem.ColumnText[0] := TEdit(EditorControl).Text;
             FocusedItem.Text := '<html>Text option (<b>' + FocusedItem.ColumnText[0] + '</b>)';
           end;
        1: begin
             FocusedItem.ColumnText[0] := IntToStr(TSpinEdit(EditorControl).Value);
             FocusedItem.Text := '<html>Spin-button option (<b>' + FocusedItem.ColumnText[0] + '</b>)';
           end;
        2: begin
             if TCheckBox(EditorControl).Checked then
               FocusedItem.ColumnText[0] := 'On'
             else
               FocusedItem.ColumnText[0] := 'Off';
             FocusedItem.Text := '<html>Checkbox option (<b>' + FocusedItem.ColumnText[0] + '</b>)';
           end;
        3: begin
             FocusedItem.ColumnText[0] := TComboBox(EditorControl).Items[TComboBox(EditorControl).ItemIndex];
             FocusedItem.Text := '<html>Combobox option (<b>' + FocusedItem.ColumnText[0] + '</b>)';
           end;
      end;
    end;

    FocusedItem := nil;
    EditorControl.Free;
    EditorControl := nil;
  end;
  Item := OptionsTree.ItemFocused;
  FocusedItem := Item;
  if (Item <> nil) and (Item.Level = 1) then
  begin
    case Item.Tag of
      0: begin
           EditorControl := TEdit.Create(Self);
           R := OptionsTree.GetItemRect(OptionsTree.IndexInView(Item));
           R.Left := Item.TextRight + 10;
           R.Right := R.Left + 100;
           EditorControl.BoundsRect := R;
           EditorControl.Parent := OptionsTree;
           TEdit(EditorControl).AutoSize := true;
           TEdit(EditorControl).Text := Item.ColumnText[0];
         end;
      1: begin
           EditorControl := TSpinEdit.Create(Self);
           R := OptionsTree.GetItemRect(OptionsTree.IndexInView(Item));
           R.Left := Item.TextRight + 10;
           R.Right := R.Left + 100;
           EditorControl.BoundsRect := R;
           EditorControl.Parent := OptionsTree;
           TSpinEdit(EditorControl).AutoSize := true;
           TSpinEdit(EditorControl).Value := StrToIntDef(Item.ColumnText[0], 0);
         end;
      2: begin
           EditorControl := TCheckBox.Create(Self);
           R := OptionsTree.GetItemRect(OptionsTree.IndexInView(Item));
           R.Left := Item.TextRight + 10;
           R.Right := R.Left + 13;
           R.Bottom := R.Top + 13;
           EditorControl.BoundsRect := R;
           EditorControl.Parent := OptionsTree;
           TCheckBox(EditorControl).Checked := Item.ColumnText[0] = 'On';
         end;
      3: begin
           EditorControl := TComboBox.Create(Self);
           R := OptionsTree.GetItemRect(OptionsTree.IndexInView(Item));
           R.Left := Item.TextRight + 10;
           R.Right := R.Left + 100;
           EditorControl.BoundsRect := R;
           EditorControl.Parent := OptionsTree;
           TComboBox(EditorControl).Style := csDropdownList;
           TComboBox(EditorControl).Items.Add('Choice 1');
           TComboBox(EditorControl).Items.Add('Choice 2');
           TComboBox(EditorControl).Items.Add('Choice 3');
           TComboBox(EditorControl).ItemIndex := StrToIntDef(Copy(Item.ColumnText[0], Length(Item.ColumnText[0]), 1), 1) - 1;
         end;
    end;
  end;
end;

procedure TOptionsForm.OptionsTreeScroll(Sender: TObject;
  ScrollBarKind: TScrollBarKind; ScrollCode: Integer);
var R : TRect;
begin
  if (EditorControl <> nil) and
     (OptionsTree.ItemFocused <> nil) then
  begin
    if OptionsTree.IsInView(OptionsTree.ItemFocused) then
    begin
      R := OptionsTree.GetItemRect(OptionsTree.IndexInView(OptionsTree.ItemFocused));
      EditorControl.Top := R.Top;
      EditorControl.Visible := true;
    end
    else
      EditorControl.Visible := false;
  end;
end;

end.

