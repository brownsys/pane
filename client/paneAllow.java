
public class paneAllow extends paneVerb{
	paneFlowGroup _flowgroup;
	
	public paneAllow(paneFlowGroup flowgroup,paneTime start, paneTime end, paneShare share) {
		_flowgroup = flowgroup;
		_start = start;
		_end = end;
		_share = share;
		_user = null;
	}

	@Override
	public String generateCmd() {		
		String config = _flowgroup.generateConfig();
		String cmd = "allow(" + config +") on " + _share.getShareName();
		if(_start != null){
			cmd += " from "+_start.getTime();
		}
		if(_end != null){
			cmd += " to "+_end.getTime();
		}
		return cmd;
	}
}
