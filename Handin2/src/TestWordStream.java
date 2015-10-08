// Week 3
// sestoft@itu.dk * 2015-09-09

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class TestWordStream {
    public static void main(String[] args) {
        long startTime;
        long stopTime;
        String filename = "/usr/share/dict/words";


        // System.out.println(readWords(filename).count());

        //Problem 2
//        readWords(filename).limit(100).forEach(i -> System.out.println(i));

        //Problem 3
//        readWords(filename).filter(x -> (x.length() >= 22)).forEach(i -> System.out.println(i));

        //Problem 4
//        System.out.println(readWords(filename).filter(x -> (x.length() >= 22)).findAny().get());


        //Problem 5
//        startTime = System.nanoTime();
//        readWords(filename).filter(i -> isPalindrome(i));   //Problem 5
//        stopTime = System.nanoTime();
//        System.out.println("sequential run time in nano sec: " + (stopTime - startTime));


        // Problem 6
//        startTime = System.nanoTime();
//        readWords(filename).parallel().filter(i -> isPalindrome(i));
//        stopTime = System.nanoTime();
//        System.out.println("parallel run time in nano sec: "+ (stopTime-startTime));


        //Problem 7
//        System.out.println("minimum: " + readWords(filename).mapToInt(i -> i.length()).sorted().findFirst().getAsInt());
//        System.out.println("maximum: " + readWords(filename).mapToInt(i -> i.length()).sorted().reduce((a,b) -> b).getAsInt());
//        long size = readWords(filename).count();
//        System.out.println("average: " +readWords(filename).mapToInt(i -> i.length()).sum()/size);


        //Problem 8
//        System.out.println(readWords(filename).collect(Collectors.groupingBy(i -> i.length(), Collectors.counting())));
    }


    //Problem 1
    public static Stream<String> readWords(String filename) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
            return reader.lines();
        } catch (IOException exn) {
            return Stream.<String>empty();
        }
    }

    //Problem 5
    public static boolean isPalindrome(String s) {
        return s.equals(new StringBuilder(s).reverse().toString()) ? true : false;
    }

    public static Map<Character, Integer> letters(String s) {
        Map<Character, Integer> res = new TreeMap<>();
        // TO DO: Implement properly
        return res;
    }
}
