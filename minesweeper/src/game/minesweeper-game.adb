with Minesweeper.Boards; use Minesweeper.Boards;
with Minesweeper.Boards.Generation; use Minesweeper.Boards.Generation;
with Minesweeper.Boards.Actions; use Minesweeper.Boards.Actions;

package body Minesweeper.Game is

   procedure Game_Loop (B : Board);

   procedure Play (Difficulty : Difficulty_Level) is
   begin
      case Difficulty is
         when Beginner     => Play_Beginner;
         when Intermediate => Play_Intermediate;
         when Expert       => Play_Expert;
      end case;
   end Play;

   procedure Play_Beginner is
      Rows  : constant Positive := 9;
      Cols  : constant Positive := 9;
      Mines : constant Positive := 10;

      B : Board (1 .. Rows, 1 .. Cols);
   begin
      B := Generate_Board (Rows, Cols, Mines);
      Game_Loop (B);
   end Play_Beginner;

   procedure Play_Intermediate is
      Rows  : constant Positive := 16;
      Cols  : constant Positive := 16;
      Mines : constant Positive := 40;

      B : Board (1 .. Rows, 1 .. Cols);
   begin
      B := Generate_Board (Rows, Cols, Mines);
      Game_Loop (B);
   end Play_Intermediate;

   procedure Play_Expert is
      Rows  : constant Positive := 16;
      Cols  : constant Positive := 30;
      Mines : constant Positive := 99;

      B : Board (1 .. Rows, 1 .. Cols);
   begin
      B := Generate_Board (Rows, Cols, Mines);
      Game_Loop (B);
   end Play_Expert;

   procedure Game_Loop (B : Board) is
   begin
      -- TODO
      null;
   end Game_Loop;

end Minesweeper.Game;