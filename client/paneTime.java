import java.util.Date;


public class paneTime {
	
	Date _absTime;
	String _relTime;
	Type _type;
	
	enum Type{
		ABS, REL, UNDEF
	}
	
	public paneTime(){
		_absTime = null;
		_relTime = null;
		_type = Type.UNDEF;
	}
	
	/**
	 * return the time represented in String,could be either relative
	 * or absolute time
	 * 
	 * @return
	 */
	public String getTime(){
		if(_type == Type.UNDEF)
			return null;
		return _type == Type.ABS?_absTime.toString():_relTime;
	}
	
	/**
	 * set relative time, current time plus a length in seconds
	 * 
	 * @param length
	 *           the offset, if offset is zero, set the time to 'now'
	 * @return
	 */
	public boolean setRelativeTime(int length){
		if(length < 0){
			return false;
		}else{
			_type = Type.REL;
			if(length > 0){
				_relTime = "+" + length;
			}else{
				//length == 0
				_relTime = "now";
			}
			return true;
		}
	}
	
	/**
	 * set absolute time, simply set to given time
	 * 
	 * @param time
	 *           the absolute time
	 * @return
	 */
	public boolean setAbsoluteTime(Date time){
		if(time.before(new Date())){
			return false;
		}else{
			_type = Type.ABS;
			_absTime = time;
			return true;
		}
	}
	
	
	
}
