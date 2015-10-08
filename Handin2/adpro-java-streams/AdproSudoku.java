// Simple sudoku solver and maker, incorporating the constraints of
// the game and using backtracking (via lazy Java 8 streams).  
// sestoft@itu.dk * 2015-07-31, 2015-08-04, 2015-10-01

// THIS VERSION IS MISSING CRUCIAL PARTS OF THE PUZZLE SOLVER AND
// PUZZLE GENERATOR; THESE ARE TO BE DEVELOPED IN THE MINI-PROJECT

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Random;
import java.util.stream.Stream;

class AdproSudoku {
  public static void main(String[] args) throws IOException, InterruptedException {
    if (args.length >= 1 && args[0].equals("solve")) {
      Reader stdIn = new InputStreamReader(System.in);
      int[][] init = new int[9][9];
      int ch = stdIn.read();
      for (int r=0; r<9; r++) {
        for (int c=0; c<9; c++) {
          init[r][c] = '1' <= ch && ch <= '9' ? ch - '0' : 0;
          ch = stdIn.read();
        }
        while (ch == '\n' || ch == '\r')
          ch = stdIn.read(); 
      }
      new State(init).print();
      int[] count = new int[1];
      boolean trace = args.length >= 2 && args[1].equals("trace");
      new State(init).solutions(trace).forEach(s -> { count[0]++; s.print(); });
      if (count[0] == 0)
        System.out.println(">>> No solutions! <<<");
    } else if (args.length < 2) 
      System.out.println("Usage: java AdproSudoku <blanks> <guesses> [latex] [<quantity>] [<prefix>]\n"
                       + "or\n"
                       + "       java AdproSudoku solve [trace] < sudokupuzzle.txt");
    else {
      int blanks = Integer.parseInt(args[0]), guesses = Integer.parseInt(args[1]);
      Stream<State> puzzles = State.make(blanks)
        .filter(s -> s.solutions().findAny().get().getGuesses()==guesses);
      if (args.length >= 3 && args[2].equals("latex")) {
        int quantity = args.length >= 4 ? Integer.parseInt(args[3]) : 10;
        String prefix = args.length >= 5 ? args[4] + "-" : "";
        String file = (args.length >= 5 ? args[4] + "-" : "sudoku") + quantity;
        PrintStream out = new PrintStream(file + ".tex");
	// The idiom .limit(quantity+1).skip(1) speeds up parallel computation, ahem...
	latexDoc(out, puzzles.limit(quantity+1).skip(1).toArray(State[]::new), prefix, blanks, guesses);	
      } else
        puzzles.forEach(s -> { s.print(); s.solutions().findAny().get().print(); });
    }
  }

  public static void latexDoc(PrintStream out, State[] puzzles, String prefix, 
                              int blanks, int guesses)
  {
    out.println("\\documentclass[12pt,a4paper]{article}\n\\begin{document}\n");
    out.println("\\renewcommand{\\familydefault}{\\sfdefault}");
    int i = 1; 
    out.println("\\Large\\sf");
    for (State puzzle : puzzles) {
      puzzle.latexTabular(out, String.format("Sudoku %s%d", prefix, i++));
      out.println("\\vfill");
    }
    out.println("\\newpage");
    out.println("\\footnotesize");
    i = 1;
    for (State puzzle : puzzles) 
      puzzle.solutions().findAny().get()
            .latexTabular(out, String.format("Solution %s%d", prefix, i++));
    out.println("\\vfill");
    out.printf("Parameters: %d blanks, %d guesses. Created %tc%n", 
                      blanks, guesses, new Date());
    out.println("\\end{document}");
  }
}

// The state of a sudoku game

class State {
  // board[r][c] is the number 1...9 in the cell, or 0 for blank
  private final int[][] board = new int[9][9];
  // Solution statistics
  private int unique = 0, guesses = 0;

  // For shuffling
  private static final Random rnd = new Random();
  
  // Initialize state from 2D array
  public State(int[][] init) {
    for (int r=0; r<9; r++)
      for (int c=0; c<9; c++)
        this.board[r][c] = init[r][c];
  }

  // Deep clone of a state
  public State(State state) { 
    this(state.board); 
    this.unique = state.unique;
    this.guesses = state.guesses;
  }

  public int get(int r, int c) {
    return board[r][c];
  }

  public void setDestructive(int r, int c, int k) {
    board[r][c] = k;
    unique++;
  }

  public int getGuesses() {
    return guesses;
  }

  public State set(int r, int c, int k, boolean trace) {
    if (trace)
      System.out.printf("Speculatively set cell (%d,%d) to %d%n", r, c, k);    
    State clone = new State(this);
    clone.board[r][c] = k;
    clone.guesses++;
    return clone;
  }

  // The block of cell (r,c)
  public int b(int r, int c) {
    return r/3*3 + c/3;
  }

  // Subsets of integer set {1, ..., 9} represented by bit patterns
  // with least significant bit representing 1, thus ...987654321.
  // Intersection is "&", union is "|", complement is "~", and
  // normalization to the domain { 1, ..., 9 } is 0x1FF & S.
  private static int singleton(int n) {
    return n == 0 ? 0 : 1 << (n - 1);
  }

  // The set of unused (available) numbers in row r
  public int R(int r) {
    int used = ~0x1FF;
    for (int c=0; c<9; c++)
      used |= singleton(get(r, c));
    return ~used;
  }
  
  // The set of unused (available) numbers in column c
  public int C(int c) {
    int used = ~0x1FF;
    for (int r=0; r<9; r++)
      used |= singleton(get(r, c));
    return ~used;
  }

  // The set of unused (available) numbers in block b
  public int B(int b) {
    int used = ~0x1FF, r0 = b/3*3, c0 = b%3*3;
    for (int r=0; r<3; r++)
      for (int c=0; c<3; c++)
        used |= singleton(get(r0+r, c0+c));
    return ~used;
  }

  // A set of numbers that must be used in row r of block b,
  // because cannot be used in that row outside block b
  public int RB(int r, int b) {
    int res = R(r);
    for (int c=0; c<9; c++)
      if (get(r, c) == 0 && b(r, c) != b)
        res &= ~C(c);
    return res;
  }  

  // A set of numbers that must be used in column c of block b,
  // because cannot be used in that column outside block b
  public int CB(int c, int b) {
    int res = C(c);
    for (int r=0; r<9; r++)
      if (get(r, c) == 0 && b(r, c) != b)
        res &= ~R(r);
    return res;
  }  

  // The set of numbers available for cell (r,c)
  public int A(int r, int c) {
    return R(r) & C(c) & B(b(r, c));
  }

  // The numbers excluded from other cells than (r,c) in its block
  public int E(int r, int c) {
    int r0 = r/3*3, r1 = r0+(r+1)%3, r2 = r0+(r+2)%3;
    int c0 = c/3*3, c1 = c0+(c+1)%3, c2 = c0+(c+2)%3;
    int b = b(r, c), block = B(b);
    int rex = (~R(r1) & ~R(r2) | RB(r, b)) & block, 
        cex = (~C(c1) & ~C(c2) | CB(c, b)) & block; 
    int Erc = rex & cex;
    Erc |= get(r, c2)!=0 ? rex & ~C(c1) : 0;
    Erc |= get(r, c1)!=0 ? rex & ~C(c2) : 0;
    Erc |= get(r2, c)!=0 ? ~R(r1) & cex : 0;
    Erc |= get(r1, c)!=0 ? ~R(r2) & cex : 0;
    Erc |= get(r, c1)!=0 && get(r, c2)!=0 ? rex : 0;
    Erc |= get(r1, c)!=0 && get(r2, c)!=0 ? cex : 0;
    return Erc;
  }

  // First fill in all uniquely determined cells; if after that there
  // are no blanks we have a solution.  Otherwise some cell
  // (bp.r,bp.c) is underdetermined, and we obtain all solutions
  // created from all possible values bp.possible of that cell.
  public Stream<State> solutions(boolean trace) {
    State state = new State(this);
    BranchPoint bp = null;
    while (bp == null && state.blankCount() > 0)
      bp = state.deterministic(trace);
    if (state.blankCount() == 0)
      return ... // TO DO 
    else {
      int r = bp.r, c = bp.c;
      if (trace && bp.possible.size() > 0)
        state.print();
      return ... // TO DO
    }
  }

  public Stream<State> solutions() { 
    return solutions(false);
  }

  // If some cell is impossible, return with bp.possible empty; if
  // some cell is uniquely determined, fill it and return null; if no
  // cell is impossible or uniquely determined, return non-null bp
  // with smallest bp.possible -- which has at least two elements.  If
  // after the for-loops bp==null, then there were no blanks.
  protected BranchPoint deterministic(boolean trace) {
    BranchPoint bp = null;
    for (int r=0; r<9; r++)
      for (int c=0; c<9; c++) 
        if (get(r, c) == 0) {
          int Arc = A(r, c), Erc = E(r, c), cand = Erc == 0 ? Arc : Arc & Erc;
          if (cand == 0) {
            if (trace)
              System.out.printf("No solution for cell (%d,%d)%n", r, c);
            return new BranchPoint(r, c, new ArrayList<Integer>()); // Empty set of solutions
          }
        }
    for (int r=0; r<9; r++)
      for (int c=0; c<9; c++) 
        if (get(r, c) == 0) {
          int Arc = A(r, c), Erc = E(r, c), cand = Erc == 0 ? Arc : Arc & Erc;
          ArrayList<Integer> possible = members(cand);
          if (possible.size() == 1) {
            if (trace)
              System.out.printf("Unique solution for cell (%d,%d): %d%n", r, c, possible.get(0));
            setDestructive(r, c, possible.get(0));
            return null;
          } else if (bp == null || possible.size() < bp.possible.size()) 
            bp = new BranchPoint(r, c, permutation(cand));
        }
    if (trace && bp != null)
      System.out.printf("Split on cell (%d,%d) with values %s%n", bp.r, bp.c, bp.possible);    
    return bp;
  }

  static class Cell {
    public final int r, c;
    public Cell(int r, int c) {
      this.r = r;
      this.c = c;
    }
  }

  static class BranchPoint extends Cell {
    public final ArrayList<Integer> possible;
    public BranchPoint(int r, int c, ArrayList<Integer> possible) {
      super(r, c);
      this.possible = possible;
    }
  }
  
  // The members of an integer set represented as a bit pattern
  public static ArrayList<Integer> members(int S) {
    ArrayList<Integer> res = new ArrayList<>();
    for (int i=1; i<=9; i++)
      if ((S & singleton(i)) != 0)
        res.add(i);
    return res;
  }

  public static ArrayList<Integer> permutation(int S) {
    ArrayList<Integer> perm = members(S);
    Collections.shuffle(perm, rnd);
    return perm;
  }

  // The cardinality of an integer set represented as a bit pattern
  public static int card(int S) {
    int count = 0;
    for (int i=1; i<=9; i++)
      if ((S & singleton(i)) != 0)
        count++;
    return count;
  }

  public int blankCount() {
    return Arrays.stream(board)
      .map(row -> (int)Arrays.stream(row).filter(k -> k == 0).count())
      .reduce(0, Integer::sum);
  }

  public ArrayList<Cell> nonBlanks() {
    ArrayList<Cell> res = new ArrayList<>();
    for (int r=0; r<9; r++) 
      for (int c=0; c<9; c++) 
        if (get(r, c) != 0)
          res.add(new Cell(r, c));
    return res;
  }

  private static final Object printlock = new Object();
  
  public void print() {
    synchronized (printlock) {
      String divider = "-------------------------------\n";
      System.out.print(divider);
      for (int r=0; r<9; r++) {
	System.out.print("|");
	for (int c=0; c<9; c++) 
	  System.out.printf(" %s %s", get(r, c) == 0 ? " " : get(r, c),
			    c % 3 == 2 ? "|" : "");
	System.out.printf("%n%s", r % 3 == 2 ? divider : "");
      }
      System.out.printf("%d unique, %d guess(es)%n", unique, guesses); 
    }
  }

  public void latexTabular(PrintStream out, String caption) {
    out.println("\\begin{center}");
    out.println("\\begin{tabular}{||c|c|c||c|c|c||c|c|c||}\\hline\\hline");
    for (int r=0; r<9; r++) {
      for (int c=0; c<9; c++) 
        out.printf("~%s~%s", get(r, c) == 0 ? " " : get(r, c),
                                    c < 8 ? "&" : "\\\\\\hline");
      out.printf("%n%s", r < 8 && r % 3 == 2 ? "\\hline\n" : "");
    }
    out.println("\\hline\\end{tabular}\\\\[1.5ex]");
    out.println(caption);
    out.println("\\end{center}");
    // out.printf("%d unique, %d guess(es)%n", unique, guesses); 
  }

  // Return random partial sudoku constrained enough so it is easy to
  // construct the full solved sudoku (but may also be overconstrained).
  static State makeSkeleton() { 
    int[][] init = new int[9][9];
    ArrayList<Integer> firstBlock = permutation(0x1FF);
    for (int i=0; i<9; i++)
      init[i/3][i%3] = firstBlock.get(i);
    State state = new State(init);
    ArrayList<Integer> firstRow = permutation(state.R(0)), 
                       firstCol = permutation(state.C(0));
    ... // TO DO
    return new State(init); 
  }

  public static Stream<State> make(int blanks) {
    Stream<State> skeleta = Stream.generate(() -> makeSkeleton());
    Stream<State> sudokus = ... // TO DO
    Stream<State> puzzles = ... // TO DO
    return puzzles;
  }
  
  protected Stream<State> eraseEnough(int blanks) {
    ... // TO DO 
  }

  protected Stream<State> eraseOne() {
    ... // TO DO
  }

  protected State erase(int r, int c) {
    State clone = new State(this.board);
    clone.board[r][c] = 0;
    return clone;
  }
}
