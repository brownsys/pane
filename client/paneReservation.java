
public class paneReservation extends paneVerb{
	String _resvName;
	int _bandwidth;
	paneFlowGroup _flowgroup;
	
	public paneReservation(String resvName, int bandwidth, paneFlowGroup flowgroup,
			paneTime start, paneTime end, paneShare share) {
		_resvName = resvName;
		_bandwidth = bandwidth;
		_flowgroup = flowgroup;
		_start = start;
		_end = end;
		_share = share;
	}

	@Override
	public String generateCmd() {
		String config = _flowgroup.generateConfig();
		String cmd = "reserve(" + config +") = " + _bandwidth + " on " + _share.getShareName()
		+" from " + _start.getTime() + " to " + _end.getTime();
		return cmd;
	}
}
