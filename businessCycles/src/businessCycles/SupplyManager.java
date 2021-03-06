/**
 * 
 * This file was automatically generated by the Repast Simphony Agent Editor.
 * Please see http://repast.sourceforge.net/ for details.
 * 
 */

/**
 *
 * Set the package name.
 *
 */
package businessCycles;

import java.util.List;
import java.util.stream.Collectors;

import cern.jet.random.*;
import repast.simphony.context.DefaultContext;
import repast.simphony.engine.schedule.*;
import repast.simphony.essentials.RepastEssentials;
import repast.simphony.random.*;
import static java.lang.Math.*;
import static repast.simphony.essentials.RepastEssentials.*;

public class SupplyManager extends DefaultContext<Firm> {

	private RecessionHandler recessionHandler;

	private double totalCapitalAfterEntry = 0.0;
	private double totalCapitalAfterExit = 0.0;

	private double industryPrice = 0.0;
	private double totalQuantityPerPeriod = 0;

	private double dead = 0;
	private int bornFirms = 0;
	private double totalFirmsAfterEntry = 0.0;
	private double totalFirmsAfterExit = 1.0;

	public Normal iniKNormal = null;
	public Normal learningRateNormal = null;
	public Normal entrantsNormal = null;
	public Normal firstUnitCostNormal = null;
	public Uniform operatingLeverageUniform = null;

	public static int periods;
	public static double estimatedInitialAnnualQuantity;

	private static boolean entryOnlyAtStart;
	private static double iniKMean;
	private static double entrantsMean;

	public SupplyManager() {

		super("SupplyManager");

		industryPrice = Demand.getPriceOfSubstitute();
		totalQuantityPerPeriod = estimatedInitialAnnualQuantity;

		recessionHandler = new RecessionHandler();
		recessionHandler.scheduleRecessions();

		/* Create distributions for initial variables of firms */

		double iniKStdDev = (Double) GetParameter("iniKStdDev") * iniKMean;
		iniKNormal = RandomHelper.createNormal(iniKMean, iniKStdDev);

		double entrantsStdDev = (Double) GetParameter("entrantsStdDev") * entrantsMean;
		entrantsNormal = RandomHelper.createNormal(entrantsMean, entrantsStdDev);

		double firstUnitCostMean = (Double) GetParameter("firstUnitCostMean");
		double firstUnitCostStdDev = (Double) GetParameter("firstUnitCostStdDev") * firstUnitCostMean;
		firstUnitCostNormal = RandomHelper.createNormal(firstUnitCostMean, firstUnitCostStdDev);

		double learningRateMean = (Double) GetParameter("learningRateMean");
		double learningRateStdDev = (Double) GetParameter("learningRateStdDev") * learningRateMean;
		learningRateNormal = RandomHelper.createNormal(learningRateMean, learningRateStdDev);

		/*
		 * Operating Leverage is a Uniform
		 */
		double operatingLeverageMin = (Double) GetParameter("operatingLeverageMin");
		double operatingLeverageMax = (Double) GetParameter("operatingLeverageMax");
		operatingLeverageUniform = RandomHelper.createUniform(operatingLeverageMin, operatingLeverageMax);

		Firm.supplyManager = this;

	}

	public static void readParams() {
		periods = (Integer) GetParameter("periods");
		entryOnlyAtStart = (Boolean) GetParameter("entryOnlyAtStart");

		iniKMean = (Double) GetParameter("iniKMean");
		entrantsMean = (Double) GetParameter("entrantsMean");
		estimatedInitialAnnualQuantity = entrantsMean * fullCapacityQuantityPerPeriod(iniKMean) * periods;
	}

	public static double fullCapacityQuantityPerPeriod(double capital) {
		return capital / periods;
	}

	@ScheduledMethod(start = 1d, interval = 1)
	public void step() {

		// Manage Entry
		int potentialEntrantsPerPeriod;
		if (GetTickCount() == 1 && entryOnlyAtStart)
			potentialEntrantsPerPeriod = (int) round(entrantsNormal.nextDouble());
		else if (!entryOnlyAtStart)
			potentialEntrantsPerPeriod = (int) round(entrantsNormal.nextDouble() / periods);
		else
			potentialEntrantsPerPeriod = 0;

		if (potentialEntrantsPerPeriod > 0)
			entry(potentialEntrantsPerPeriod);

		// Total Firms After Entry
		totalFirmsAfterEntry = size();

		// If there is no firm, return setting empty counters
		if (totalFirmsAfterEntry < 1.0) {
			totalCapitalAfterEntry = 0.0;
			totalCapitalAfterExit = 0.0;
			totalQuantityPerPeriod = 0.0;
			industryPrice = Demand.priceFromQuantityPerPeriod(totalQuantityPerPeriod);
			dead = 0;
			totalFirmsAfterExit = 0.0;
			return;
		}

		// Apply planned investment and aggregate for short term maximization
		totalCapitalAfterEntry = stream().mapToDouble(f -> f.applyAnnualPlannedInvestment()).sum();

		// Process Offers
		totalQuantityPerPeriod = stream().mapToDouble(f -> f.offer()).sum();
		industryPrice = Demand.priceFromQuantityPerPeriod(totalQuantityPerPeriod);

		// Process Demand response and Kill Firms
		if (killActive()) {
			dead = processDemandAndKillFirms();
			totalFirmsAfterExit = size();
			totalCapitalAfterExit = stream().mapToDouble(f -> f.getCapital()).sum();
		} else {
			stream().forEach(Firm::processDemandResponse);
			dead = 0;
			totalFirmsAfterExit = totalFirmsAfterEntry;
			totalCapitalAfterExit = totalCapitalAfterEntry;
		}

		// Planning
		stream().forEach(Firm::annualPlan);

	}

	private void entry(int potentialEntrants) {

		Firm f;
		bornFirms = 0;

		for (int j = 1; j <= potentialEntrants; j++) {

			// Destroy if not profitable
			f = new Firm();
			if (f.checkEntry())
				bornFirms++;
			else
				RemoveAgentFromModel(f);

		}

	}

	private boolean killActive() {

		if (entryOnlyAtStart)
			return false;

		else if (!RecessionHandler.exitOnRecession() && RecessionHandler.inRecession())
			return false;

		else
			return true;

	}

	private double processDemandAndKillFirms() {

		// Process demand and collect firms to be killed
		List<Firm> toKill = stream().filter(f -> !f.processDemandResponse()).collect(Collectors.toList());

		double toKillSize = toKill.size();
		toKill.forEach(RepastEssentials::RemoveAgentFromModel);

		return toKillSize;

	}

	public String toString() {

		return "SupplyManager";

	}

	public double getTotalCapitalAfterEntry() {
		return totalCapitalAfterEntry;
	}

	public double getTotalCapitalAfterExit() {
		return totalCapitalAfterExit;
	}

	// Quantity is the amount offered including killed firms
	public double getTotalQuantityPerPeriod() {
		return totalQuantityPerPeriod;
	}

	public double getTotalQuantity() {
		return totalQuantityPerPeriod * periods;
	}

	public double getAfterEntryFullCapacityPrice() {
		return Demand.priceFromQuantityPerPeriod(fullCapacityQuantityPerPeriod(totalCapitalAfterEntry));
	}

	public double getNoRecessionAfterEntryFullCapacityPrice() {
		return Demand.noRecesPriceFromQuantityPerPeriod(fullCapacityQuantityPerPeriod(totalCapitalAfterEntry));
	}
	
	public double getAfterExitFullCapacityPrice() {
		return Demand.priceFromQuantityPerPeriod(fullCapacityQuantityPerPeriod(totalCapitalAfterExit));
	}

	public double getNoRecessionAfterExitFullCapacityPrice() {
		return Demand.noRecesPriceFromQuantityPerPeriod(fullCapacityQuantityPerPeriod(totalCapitalAfterExit));
	}
	
	public double getIndustryPrice() {
		return industryPrice;
	}


	public double getDead() {
		return dead;
	}

	public int getBornFirms() {
		return bornFirms;
	}

	public double getTotalFirmsAfterEntry() {
		return totalFirmsAfterEntry;
	}

	public double getTotalFirmsAfterExit() {
		return totalFirmsAfterExit;
	}

}
