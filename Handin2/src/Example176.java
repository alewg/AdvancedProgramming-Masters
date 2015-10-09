// Example 176 from page 139 of Java Precisely third edition (The MIT Press 2016)
// Author: Peter Sestoft (sestoft@itu.dk)

import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.IntStream.Builder;
import java.util.stream.Stream;
import java.util.BitSet;
import java.util.Optional;

/**
 * Problem 1.
 *
 *  Solution counts for the n-queens problem:
 *      1-queens solutions:          1 (time        0.072 sec)
 *      2-queens solutions:          0 (time        0.000 sec)
 *      3-queens solutions:          0 (time        0.000 sec)
 *      4-queens solutions:          2 (time        0.000 sec)
 *      5-queens solutions:         10 (time        0.001 sec)
 *      6-queens solutions:          4 (time        0.003 sec)
 *      7-queens solutions:         40 (time        0.007 sec)
 *      8-queens solutions:         92 (time        0.011 sec)
 *      9-queens solutions:        352 (time        0.031 sec)
 *      10-queens solutions:        724 (time        0.036 sec)
 *      11-queens solutions:       2680 (time        0.096 sec)
 *      12-queens solutions:      14200 (time        0.260 sec)
 *      13-queens solutions:      73712 (time        1.271 sec)
 *      14-queens solutions:     365596 (time        7.443 sec)
 *      15-queens solutions:    2279184 (time       47.541 sec)
 *      16-queens solutions:   14772512 (time      320.243 sec)
 *
 *  real	6m17.137s
 *  user	6m19.013s
 *  sys	    0m1.254s
 *
 *
 *
 * Problem 2.
 *
 *  Solution counts for the n-queens problem:
 *      1-queens solutions:          1 (time        0.098 sec)
 *      2-queens solutions:          0 (time        0.002 sec)
 *      3-queens solutions:          0 (time        0.000 sec)
 *      4-queens solutions:          2 (time        0.000 sec)
 *      5-queens solutions:         10 (time        0.001 sec)
 *      6-queens solutions:          4 (time        0.002 sec)
 *      7-queens solutions:         40 (time        0.009 sec)
 *      8-queens solutions:         92 (time        0.015 sec)
 *      9-queens solutions:        352 (time        0.014 sec)
 *      10-queens solutions:        724 (time        0.056 sec)
 *      11-queens solutions:       2680 (time        0.132 sec)
 *      12-queens solutions:      14200 (time        0.259 sec)
 *      13-queens solutions:      73712 (time        0.651 sec)
 *      14-queens solutions:     365596 (time        3.799 sec)
 *      15-queens solutions:    2279184 (time       24.849 sec)
 *      16-queens solutions:   14772512 (time      168.376 sec)
 *
 *  real	3m18.400s
 *  user	9m19.648s
 *  sys	    0m3.707s
 *
 *
 *  Problem 3.
 *
 *  Indeed, the computation was faster with parallel streams rather than with singular.
 *  The testing machine has 4 cores and the test speed up factor is 2. Since we used stream's
 *  build in parallel system, the method automatically selects most optimized amount of threads
 *  for efficient computation.
 *
 *
 *  Problem 4.
 *
 *  After using parallel() on stream in different places of queens(BitSet todo, IntList tail)
 *  method, there was no significant improvement on running time. When n > 13, we witnessed minor time
 *  fluctuation within ~1s.
 *
 *
 */
class Example176 {
    public static void main(String[] args) {
//    System.out.println("\nThe first 20 permutations of 8 numbers:");
//    Stream<IntList> permutations = perms(8);
//    permutations.limit(20).forEach(System.out::println);
//    System.out.println("\nAll solutions to the 8-queens problem:");
//    Stream<IntList> queens = queens(8);
//    queens(8).forEach(System.out::println);
//    System.out.println("\nThe number of solutions to the 8-queens problem:");
//    System.out.println(queens(8).count());
//    System.out.println("\nSome solution to the 8-queens problem:");
//    System.out.println(queens(8).findAny());
        countSolutions();
        //findAnySolution();
    }

    public static void countSolutions() {
        System.out.println("\nSolution counts for the n-queens problem:");
        for (int n = 1; n <= 16; n++) {
            Timer t = new Timer();
            long count = queens(n).count();
            //long count = queens(n).parallel().count(); // Problem 2. Parallel solution
            System.out.printf("%4d-queens solutions: %10d (time %12.3f sec)%n", n, count, t.check());
        }
    }

    public static void findAnySolution() {
        System.out.println("\nSome solution to the n-queens problem, if any:");
        Timer t = new Timer();
        for (int n = 1; n <= 13; n++) {
            Optional<IntList> solution = queens(n).findAny();
            System.out.printf("%4d-queens solution: %s%n", n, solution);
        }
        System.out.printf("Time %12.3f sec%n", t.check());
    }

    // It is necessary to box the IntStream produced by todo.stream()
    // because there is no object-producing version flatMapToObject on
    // IntStream!

    public static Stream<IntList> perms(BitSet todo, IntList tail) {
        if (todo.isEmpty())
            return Stream.of(tail);
        else
            return todo.stream().boxed().flatMap(r -> perms(minus(todo, r), new IntList(r, tail)));
    }

    public static Stream<IntList> queens(BitSet todo, IntList tail) {
        if (todo.isEmpty())
            return Stream.of(tail);
        else
            return todo.stream()
                    .filter(r -> safe(r, tail)).boxed() //.parallel()
                    .flatMap(r -> queens(minus(todo, r), new IntList(r, tail))).parallel();
    }

    public static boolean safe(int mid, IntList tail) {
        return safe(mid + 1, mid - 1, tail);
    }

    public static boolean safe(int d1, int d2, IntList tail) {
        return tail == null || d1 != tail.item && d2 != tail.item && safe(d1 + 1, d2 - 1, tail.next);
    }

    public static Stream<IntList> perms(int n) {
        BitSet todo = new BitSet(n);
        todo.flip(0, n);
        return perms(todo, null);
    }

    public static Stream<IntList> queens(int n) {
        BitSet todo = new BitSet(n);
        todo.flip(0, n);
        return queens(todo, null);
    }

    // This helper method is needed because Java does (yet?) support
    // immutable data structures: those for which operations create a
    // new value instead of destructively updating an existing one.

    public static BitSet minus(BitSet set, int bit) {
        BitSet rest = (BitSet) set.clone();
        rest.set(bit, false);
        return rest;
    }

    // Simple timer, measuring wall-clock (elapsed) time in seconds
    private final static class Timer {
        private long start;

        public Timer() {
            start = System.nanoTime();
        }

        public double check() {
            return (System.nanoTime() - start) / 1e9;
        }
    }

    // Immutable integer lists; Java is still lacking such things

    static class IntList {
        public final int item;
        public final IntList next;

        public IntList(int item, IntList next) {
            this.item = item;
            this.next = next;
        }

        @Override
        public String toString() {
            return stream(this).mapToObj(String::valueOf).collect(Collectors.joining(",", "[", "]"));
        }

        public static IntStream stream(IntList xs) {
            IntStream.Builder sb = IntStream.builder();
            while (xs != null) {
                sb.accept(xs.item);
                xs = xs.next;
            }
            return sb.build();
        }
    }
}


