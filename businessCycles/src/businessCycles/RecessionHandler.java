package businessCycles;

import static repast.simphony.essentials.RepastEssentials.GetParameter;
import static repast.simphony.essentials.RepastEssentials.GetTickCount;

import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.ScheduleParameters;

public class RecessionHandler {
	
	private static double[] start, dur, recesMag;
	private static double recessionMagnitude = 0.0;
	private static boolean exitOnRecession;
	
	public RecessionHandler() {
		resetStaticVars();
	}
	
	public static void resetStaticVars() {
		
		exitOnRecession = (Boolean) GetParameter("exitOnRecession");
		
		// Read start of recessions
		String[] tmp = ((String) GetParameter("recessionStart")).split(":");
		start = new double[tmp.length];
		for (int i = 0; i < tmp.length; i++) {
			start[i] = Double.valueOf(tmp[i]);
		}

		// Read Duration of recessions
		tmp = ((String) GetParameter("recessionDuration")).split(":");
		dur = new double[tmp.length];
		for (int i = 0; i < tmp.length; i++) {
			dur[i] = Double.valueOf(tmp[i]);
		}

		// Read magnitude of recessions
		tmp = ((String) GetParameter("recessionMagnitude")).split(":");
		recesMag = new double[tmp.length];
		for (int i = 0; i < tmp.length; i++) {
			recesMag[i] = Double.valueOf(tmp[i]);
		}
	
	}
	
	public void scheduleRecessions() {
		
		int periods = SupplyManager.periods;

		// Schedule recessions
		for (int i = 0; i < start.length; i++) {
			ISchedule sch = RunEnvironment.getInstance().getCurrentSchedule();

			// Set start
			ScheduleParameters params = ScheduleParameters.createOneTime(
					start[i] * periods, ScheduleParameters.FIRST_PRIORITY);
			sch.schedule(params, this, "setRecesMagnitude", recesMag[i]);

			// Set end
			params = ScheduleParameters.createOneTime((start[i] + dur[i])
					* periods, ScheduleParameters.FIRST_PRIORITY);
			sch.schedule(params, this, "setRecesMagnitude", 0.0);

		}

	}

	public static boolean inRecession() {
		
		double tick = GetTickCount();
		
		for (int i = 0; i < start.length; i++) {
			if (start[i] <= tick && tick < start[i] + dur[i])
				return true;
		}
		
		return false;
		
	}
	
	public static boolean exitOnRecession() {
		return exitOnRecession;
	}

	public static void setRecesMagnitude(double mag) {
		RecessionHandler.recessionMagnitude = mag;
	}

	public static double getRecesMagnitude() {
		return RecessionHandler.recessionMagnitude;
	}


}
