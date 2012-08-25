package edu.brown.cs.paneclient;

public class PaneAllow extends PaneVerb {
	PaneFlowGroup _flowgroup;
	
	public PaneAllow(PaneFlowGroup flowgroup,PaneTime start, PaneTime end) {
		_flowgroup = flowgroup;
		_start = start;
		_end = end;
		_share = null;
		_principal = null;
	}

	@Override
	public String generateCmd() {		
		String fg;
		if (_flowgroup == null)
			fg = "*";
		else
			fg = _flowgroup.generateConfig();
		
		String cmd = "allow(" + fg +") on " + _share.getShareName();
		if (_start != null) {
			cmd += " from "+_start.getTime();
		}
		if (_end != null) {
			cmd += " to "+_end.getTime();
		}
		cmd += ".\n";
		return cmd;
	}
	
	@Override
	public String toString() {
		return "PaneAllow: " + generateCmd();
	}
}
