package edu.brown.cs.paneclient;
public class PaneRelativeTime extends PaneTime {
	long _relTime;
	
	@Override
	public String getTime() {
		String time;
		if (_relTime == 0)
			time = "now";
		else
			time = "+" + _relTime;
		return time;
	}
	
	public boolean setRelativeTime(long length) {
		if (length < 0) {
			return false;
		} else {
			_relTime = length;
			return true;
		}
	}

	@Override
	public String toString() {
		return "PaneRelativeTime: time: " + getTime();
	}
}
