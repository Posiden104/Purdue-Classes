// Change the package name to use your login ID in place of YL below:
package edu.purdue.YL.lab;

import java.util.ArrayList;
import java.util.Calendar;

import android.app.Activity;
import android.os.Build;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup.LayoutParams;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;

public class StartActivity extends Activity {
    static ArrayList<String> strings = new ArrayList<String>();
    static TextView logView;
    ScrollView scrollView;
    LinearLayout buttons;
    static boolean update = false;

    /** 
     * Called when the activity is first created.
     */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        //create a thread that will update the screen in
        //the background when something is logged
        new Thread(new Runnable() { 
            @Override
            public void run() {
                while (true) {
                    synchronized(strings) {
                        while (!update) {
                            try {
                                strings.wait();
                            } catch(InterruptedException e) {}
                        }
                        update = false;
                    }

                    scrollView.scrollTo(0, logView.getHeight());
                }
            }
        }).start();

        // Create the on-screen layout...
        createLayout();

        // Add some buttons to the button area at the bottom...
        addButton("Reset");
        addButton("Time");
        addButton("Serial");

        // Send a test message to the text area for display on screen...
        logIt("onCreate called");
    }

    /**
     * Create a layout for the screen. Pieces of the layout are stored in member variables (logView, scrollView, and
     * buttons) so they can be accessed later.
     */
    private void createLayout() {
        // Create a vertical layout for the top level (main) view area...
        LinearLayout main = new LinearLayout(this);
        main.setOrientation(LinearLayout.VERTICAL);

        // Add a scrolling view to the main layout; weight it to fill the screen...
        scrollView = new ScrollView(this);
        LinearLayout.LayoutParams lp = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT,
                LayoutParams.WRAP_CONTENT);
        lp.weight = 1.0f;
        scrollView.setLayoutParams(lp);

        // Add a text view to the scrolling view; initialize with the strings in ArrayList strings...
        logView = new TextView(this);
        for (int i = 0; i < strings.size(); i++)
            logView.append(strings.get(i));
        scrollView.addView(logView);

        // Create a horizontal layout for a row of buttons...
        buttons = new LinearLayout(this);
        buttons.setOrientation(LinearLayout.HORIZONTAL);

        // Add the scrollView and buttons layout to the main view and make it display...
        main.addView(scrollView);
        main.addView(buttons);
        setContentView(main);
    }

    /**
     * Add a button to the button row.
     * 
     * @param s
     *            string to be displayed in the button; also accessible from the click listener when pressed.
     */
    private void addButton(String s) {
        Button b = new Button(this);

        //TODO - add an instance of the Listener class to the 
        //button's list of listeners

        b.setText(s);
        buttons.addView(b);
    }

    /**
     * Add (append) a log message to be displayed on the screen. Remember it (in strings) so that the display can be
     * refreshed if the orientation is changed or the app is temporarily suspended.
     * 
     * @param s
     *            string to be displayed on the screen
     */
    static void logIt(String s) {
        strings.add(s + "\n");
        logView.append(s + "\n");

        synchronized (strings)
        {
            update = true;

            strings.notifyAll();
        }
    }
}
