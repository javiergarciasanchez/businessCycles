package w;


public class MarketShare_2_0 extends JGSDataSource {
	public MarketShare_2_0(){
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
