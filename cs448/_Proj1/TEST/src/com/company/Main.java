package com.company;

import java.io.*;
import java.util.*;

public class Main {

    public static File logfile;
    public static File dbfile;

    final private static String[] schema = {"name", "ssn", "home_phone", "address", "office_phone", "age", "gpa"};

    public static void main(String[] args) {
        try{
            logfile = new File("log.txt");
            if(!logfile.exists()){
                logfile.createNewFile();
            }
            dbfile = new File("db.txt");
            if(!dbfile.exists()){
                dbfile.createNewFile();
            }
        } catch(Exception e){
            e.printStackTrace();
        }

        System.out.println("Welcome user.");

        String choice = "0";

        while(!choice.equals("4")){

            System.out.println("\n\n");
            System.out.println("What would you like to do?\n");
            System.out.println("1. INSERT");
            System.out.println("2. UPDATE");
            System.out.println("3. DELETE");
            System.out.println("4. EXIT");

            Scanner in = new Scanner(System.in);
            choice = in.nextLine();


            switch(choice){
                case "1":
                    insert();
                    break;
                case "2":
                    update();
                    break;
                case "3":
                    delete();
                    break;
                case "4":
                    break;
                default:
                    System.out.println("Expected 1, 2, 3, or 4.\n\n");
                    break;
            }
        }

    }


    public static void insert(){
        Scanner in = new Scanner(System.in);
        String logText = "Insert values: ";
        String insertText = "";

        //assign ID
        insertText = getNewID() + ",";

        //build insert
        for(int i = 0; i < 7; i++) {
            System.out.println("Enter value for \"" + schema[i] + "\"");
            insertText = insertText + in.nextLine() + ",";
        }
        insertText = insertText.substring(0, insertText.length() - 1);

        //insert
        insertIntoDb(insertText);

        //log
        logText = logText + insertText;
        log(logText);
    }

    public static void update(){
        String id = "";
        String[] entry = new String[7];
        String oldValue = "";
        String newValue = "";
        Scanner in = new Scanner(System.in);
        boolean found = false;
        ArrayList<String> dbset = deleteDB();
        int row = -1;

        System.out.println("Enter the ID for the entry you want to update");
        id = in.nextLine();

        for(int i = 0; i < dbset.size(); i++){
            oldValue = dbset.get(i);
            entry = oldValue.split(",");
            if(entry[0].equals(id)){
                found = true;
                row = i;
                break;
            }
        }

        if(!found){
            System.out.println("No entry with ID " + id + " was found");
            return;
        }

        for(int i = 0; i < 7; i++) {
            System.out.println("Enter value for \"" + schema[i] + "\", it was: " + entry[i]);
            newValue = newValue + in.nextLine() + ",";
        }
        newValue = newValue.substring(0, newValue.length() - 1);

        dbset.remove(row);
        dbset.add(row, newValue);

        log("Updated entry from: " + oldValue + " to: " + newValue);

    }

    public static void delete(){
        String id = "";
        String[] entry = new String[]{"-1"};
        ArrayList<String> dbset = deleteDB();
        String removedText = "Delete Entry: ";
        boolean hasDeleted = false;

        try {
            System.out.println("Please enter the ID of the entry you would like to delete");
            Scanner in = new Scanner(System.in);
            id = in.nextLine();

            for(String s: dbset){
                if(s.split(",")[0].equals(id)){
                    dbset.remove(s);
                    removedText = removedText + s;
                    hasDeleted = true;
                    break;
                }
            }

            //repair db and log results
            if(hasDeleted){
                repairDB(dbset);
                log(removedText);
            } else {
                System.out.println("No entry with ID " + id + " was found");
                log("Attempt to remove ID: " + id + " but ID not found");
            }

        }catch(Exception e){
            e.printStackTrace();
        }
    }



    //helpers

    public static ArrayList<String> deleteDB(){
        ArrayList<String> dbset = new ArrayList<String>();

        try {
            Scanner dbScanner = new Scanner(dbfile);

            while(dbScanner.hasNextLine()){
                dbset.add(dbScanner.nextLine());
            }

            dbfile.delete();
            dbfile = new File("db.txt");
            if(!dbfile.exists()){
                dbfile.createNewFile();
            }

        }catch(Exception e){
            e.printStackTrace();
        }
        return dbset;
    }

    public static void repairDB(ArrayList<String> dbset){
        for(String s: dbset){
            insertIntoDb(s);
        }
    }

    public static void insertIntoDb(String s){
        //insert
        try{
            FileWriter fw = new FileWriter(dbfile,true);
            BufferedWriter bw = new BufferedWriter(fw);
            PrintWriter pw = new PrintWriter(bw);
            pw.write(s);
            pw.println("");
            pw.close();
        } catch(Exception e){
            e.printStackTrace();
        }
    }

    public static int getNewID(){
        String lastEntry = "0,test";
        String[] leArr;
        try {
            Scanner in = new Scanner(dbfile);
            while(in.hasNextLine()){
                lastEntry = in.nextLine();
            }

            leArr = lastEntry.split(",");

            return Integer.parseInt(leArr[0]) + 1;
        } catch(Exception e){
            e.printStackTrace();
        }
        return 0;
    }

    public static void log(String s){
        try{
            FileWriter fw = new FileWriter(logfile,true);
            BufferedWriter bw = new BufferedWriter(fw);
            PrintWriter pw = new PrintWriter(bw);
            pw.write(s);
            pw.println("");
            pw.close();
            System.out.println("Logged Message: " + s);
        } catch(Exception e){
            e.printStackTrace();
        }
    }
}
