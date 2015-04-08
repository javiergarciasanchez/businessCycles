package w;


public class Return_2_2 extends JGSDataSource {
	public Return_2_2(){
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
