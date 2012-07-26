import java.util.Date;


public class PaneRelativeTime extends PaneTime{
	long _relTime;
	
	public PaneRelativeTime(){
		_type = Type.REL;
	}
	
	@Override
	public String getTime() {
		String time;
		if(_relTime == 0)
			time = "now";
		else
			time = "+"+_relTime;
		return time;
	}
	

	@Override
	public boolean setRelTime(long length){
		if(length < 0){
			return false;
		}else{
			_relTime = length;
			return true;
		}
	}

	@Override
	public boolean setAbsTime(Date time) {
		return false;
	}
	
	@Override
	public String toString(){
		return " PaneRelativeTime: type:"+_type + " time:"+getTime();
	}
}
