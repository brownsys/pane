import java.util.Date;


public abstract class PaneTime {
	
	Type _type;
	enum Type{
		ABS, REL, UNDEF
	}
	
	abstract public String getTime();
	
	abstract public boolean setRelTime(long length);
	
	abstract public boolean setAbsTime(Date time);
	
	abstract public String toString();
}
