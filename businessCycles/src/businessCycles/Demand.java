package businessCycles;

import repast.simphony.ui.probe.*;
import static java.lang.Math.*;
import static repast.simphony.essentials.RepastEssentials.*;

public class Demand {

	/**
	 * 
	 * This is a constant elasticity demand function, with a maximum price
	 * equivalent to the price of a substitute.
	 * 
	 */
	public static double price(double quantity) {

		double annualizedQuantity = quantity
				* (Integer) GetParameter("periods");
		
		double tmpCrisisImpact = 1.0 - getSSMagnitude();
		
		double sub = (Double) GetParameter("priceOfSubstitute");
		double param = (Double) GetParameter("demandParameter");
		double elast = (Double) GetParameter("demandElasticity");

		double dMin = sub * tmpCrisisImpact;

		if (quantity > 0) {

			double d = param * pow(annualizedQuantity, -1.0 / elast)
					* tmpCrisisImpact;
			return min(sub, d);

		} else
			return dMin;

	}

	public static double getSSMagnitude() {
		double year = GetTickCount() / (int) GetParameter("periods");
		double start = (Double) GetParameter("suddenStopStart");
		double dur = (Double) GetParameter("suddenStopDuration");
		double sMag = (Double) GetParameter("suddenStopMagnitude");

		if ((start <= year) && (year < dur + start))
			return sMag;
		else
			return 0.0;

	}

	public static double getCapacityUsed() {
		double sMag = getSSMagnitude();

		if (sMag == 0.0)
			return 1.0;
		else {
			double dElast = (Double) GetParameter("demandElasticity");
			double sElast = (Double) GetParameter("supplyElasticity");

			return 1.0 - (dElast * sMag * sElast)
					/ (dElast + sElast * (1 - sMag));

		}

	}

	@ProbeID()
	public String toString() {
		return "Demand";
	}
}
