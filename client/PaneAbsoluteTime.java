import java.util.Date;


public class PaneAbsoluteTime extends PaneTime{
	Date _absTime;
	
	public PaneAbsoluteTime(){
		_type = Type.ABS;
	}

	@Override
	public String getTime() {
		return _absTime.toString();
	}
	
	@Override
	public boolean setAbsTime(Date time) {
		if(time.before(new Date())){
			return false;
		}else{
			_type = Type.ABS;
			_absTime = time;
			return true;
		}
	}

	@Override
	public boolean setRelTime(long length) {
		return false;
	}
	
	@Override
	public String toString(){
		return " PaneAbsoluteTime: type:"+_type + " time:"+getTime();
	}
}
