package edu.brown.cs.paneclient;

public class PaneReservation extends PaneVerb {
	int _bandwidth;
	PaneFlowGroup _flowgroup;
	
	public PaneReservation(int bandwidth, PaneFlowGroup flowgroup,
			PaneTime start, PaneTime end) {
		_bandwidth = bandwidth;
		_flowgroup = flowgroup;
		_start = start;
		_end = end;
		_share = null;
	}

	@Override
	public String generateCmd() {
		String fg;
		if (_flowgroup == null)
			fg = "*";
		else 
			fg = _flowgroup.generateConfig();
		String cmd = "reserve(" + fg + ") = " + _bandwidth + " on " + _share.getShareName()
		+ " from " + _start.getTime() + " to " + _end.getTime();
		cmd += ".\n";
		return cmd;
	}
	
	@Override
	public String toString() {
		return "PaneReservation: " + generateCmd();
	}
}
