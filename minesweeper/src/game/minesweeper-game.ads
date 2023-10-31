package Minesweeper.Game is

   type Difficulty_Level is (Beginner, Intermediate, Expert);

   procedure Play (Difficulty : Difficulty_Level);

private

   procedure Play_Beginner;
   procedure Play_Intermediate;
   procedure Play_Expert;

end Minesweeper.Game;