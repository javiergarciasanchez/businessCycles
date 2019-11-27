package businessCycles;

import repast.simphony.ui.probe.*;
import static java.lang.Math.*;
import static repast.simphony.essentials.RepastEssentials.*;

public class Demand {

	private static double priceOfSubstitute;
	private static double demandParameter;
	private static double demandElasticity;

	public static void readParams() {
		demandParameter = (Double) GetParameter("demandParameter");
		demandElasticity = (Double) GetParameter("demandElasticity");

		// Substitute price is assigned to estimated initial price
		priceOfSubstitute = rawPriceFromAnnualQuantity(SupplyManager.estimatedInitialAnnualQuantity, demandParameter,
				demandElasticity);

	}

	/*
	 * 
	 * This is a constant elasticity demand function, with a maximum price
	 * equivalent to the price of a substitute.
	 * 
	 */
	private static double rawPriceFromAnnualQuantity(double annualQuantity, double param, double elast) {
		return param * pow(annualQuantity, -1.0 / elast);
	}

	private static double rawAnnualDemandFromPrice(double price, double param, double elast) {
		return pow(param / price, elast);
	}

	public static double priceFromQuantityPerPeriod(double quantityPerPeriod) {
		return noRecesPriceFromQuantityPerPeriod(quantityPerPeriod) * (1.0 - RecessionHandler.getRecesMagnitude());
	}

	public static double noRecesPriceFromQuantityPerPeriod(double quantityPerPeriod) {

		if (quantityPerPeriod == 0.0)
			return priceOfSubstitute;

		else {
			double annualizedQ = quantityPerPeriod * SupplyManager.periods;
			return min(priceOfSubstitute, rawPriceFromAnnualQuantity(annualizedQ, demandParameter, demandElasticity));
		}

	}

	public static double inverseDemandPerPeriod(double price) {

		double adjPrice = price / (1.0 - RecessionHandler.getRecesMagnitude());

		if (adjPrice >= priceOfSubstitute)
			return 0.0;
		else
			return rawAnnualDemandFromPrice(adjPrice, demandParameter, demandElasticity);

	}

	public static double getCapacityUsed() {
		double recMag = RecessionHandler.getRecesMagnitude();

		if (recMag == 0.0)
			return 1.0;
		else {
			double dElast = (Double) GetParameter("demandElasticity");
			double sElast = (Double) GetParameter("supplyElasticity");

			return 1.0 - (dElast * recMag * sElast) / (dElast + sElast * (1 - recMag));

		}

	}

	public static double getDemandElasticity() {
		return demandElasticity;
	}

	public static double getPriceOfSubstitute() {
		return priceOfSubstitute;
	}

	@ProbeID()
	public String toString() {
		return "Demand";
	}
}
