package edu.purdue.jvanauke.lab;

import android.view.View.*;
import android.view.View;
import java.util.*;
import android.os.*;
import android.widget.*;

public class Listener implements OnClickListener {
    
    public void onClick(View arg) {
        Button b = (Button) arg;
        String s = b.getText().toString();
        if(s.equals("Reset"))
            StartActivity.logIt("Reset Pressed");
        if(s.equals("Time"))
            StartActivity.logIt("Time Pressed");
        if(s.equals("Serial"))
            StartActivity.logIt("Serial Pressed");                
    }
}
