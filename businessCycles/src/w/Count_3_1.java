package w;


public class Count_3_1 extends JGSDataSource {
	public Count_3_1(){
		super();
		
		String[] args = getClass().getName().split("_");
		
		valueName= args[0].split("\\.")[1];
		opLevCohort = Integer.parseInt(args[1]);
		timeCohort= Integer.parseInt(args[2]);
		
		if(valueName.equals("Count"))
			count = true;
		else
			count = false;

	}

}
