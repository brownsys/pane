package edu.brown.cs.paneclient;
import java.util.Date;


public class PaneAbsoluteTime extends PaneTime {
	Date _absTime;
	
	@Override
	public String getTime() {
		return _absTime.toString();
	}
	
	public boolean setAbsoluteTime(Date time) {
		if (time.before(new Date())) {
			return false;
		} else {
			_absTime = time;
			return true;
		}
	}
	
	@Override
	public String toString() {
		return "PaneAbsoluteTime: time: " + getTime();
	}
}
