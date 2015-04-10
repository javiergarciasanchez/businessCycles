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

import java.util.ArrayList;
import java.util.List;

import cern.jet.stat.*;
import cern.jet.random.*;
import repast.simphony.context.Context;
import repast.simphony.engine.schedule.*;
import repast.simphony.parameter.*;
import repast.simphony.random.*;
import repast.simphony.util.collections.IndexedIterable;
import static java.lang.Math.*;
import static repast.simphony.essentials.RepastEssentials.*;

public class SupplyManager {

	public double totalQuantity = 0;
	public double price = 0;
	public double dead = 0;
	public int bornFirms = 0;
	public double totalFirms = 1.0;
	public double totalQBeforeExit = 0;
	public double totalFBeforeExit = 1.0;

	public double[] timeCohortLimits = null;
	public double[] opLevCohortLimits = null;

	public Normal iniKNormal = null;
	public Normal operatingLeverageNormal = null;
	public Beta learningRateDistrib = null;
	public Normal entrantsNormal = null;
	public Normal rDEfficiencyNormal = null;
	public Normal innovationErrorNormal = null;
	public Normal firstUnitCostNormal = null;

	public static int periods;

	private Context<Object> context;

	public SupplyManager(Context<Object> context) {

		this.context = context;
		context.add(this);

		price = (Double) GetParameter("priceOfSubstitute");

		periods = (Integer) GetParameter("periods");

		/* Read Time Cohorts limits */
		String[] tmp = ((String) GetParameter("timeCohorts")).split(":");
		timeCohortLimits = new double[tmp.length];
		for (int i = 0; i < tmp.length; i++) {
			timeCohortLimits[i] = new Double(tmp[i]);
		}

		/*
		 * Set Operating Leverage Cohorts Limits
		 */
		double opLevMean = (Double) GetParameter("operatingLeverageMean");
		double opLevStdDev = (Double) GetParameter("operatingLeverageStdDev")
				* opLevMean;
		int cohorts = (Integer) GetParameter("opLevCohorts");
		opLevCohortLimits = new double[cohorts - 1];

		for (int i = 0; i < cohorts - 1; i++) {
			opLevCohortLimits[i] = Probability.normalInverse((i + 1.0)
					/ cohorts)
					* opLevStdDev + opLevMean;
		}

		/* Create distributions for initial variables of firms */

		rDEfficiencyNormal = RandomHelper.createNormal(
				(Double) GetParameter("rDEfficiencyMean"),
				(Double) GetParameter("rDEfficiencyStdDev")
						* (Double) GetParameter("rDEfficiencyMean"));

		iniKNormal = RandomHelper.createNormal(
				(Double) GetParameter("iniKMean"),
				(Double) GetParameter("iniKStdDev")
						* (Double) GetParameter("iniKMean"));

		operatingLeverageNormal = RandomHelper.createNormal(opLevMean,
				opLevStdDev);

		entrantsNormal = RandomHelper.createNormal(
				(Double) GetParameter("entrantsMean"),
				(Double) GetParameter("entrantsStdDev")
						* (Double) GetParameter("entrantsMean"));

		innovationErrorNormal = RandomHelper.createNormal(1.0,
				(Double) GetParameter("innovationErrorStdDev"));

		firstUnitCostNormal = RandomHelper.createNormal(
				(Double) GetParameter("firstUnitCostMean"),
				(Double) GetParameter("firstUnitCostStdDev")
						* (Double) GetParameter("firstUnitCostMean"));

		/*
		 * If the learning rate variance is too big for the mean, it is set to
		 * the minimum value
		 */
		double lRMean = (Double) GetParameter("learningRateMean");

		double lRVar = pow(
				(Double) GetParameter("learningRateStdDev") * lRMean, 2.0);

		if (lRVar >= (lRMean - 0.5) * (1 - lRMean)) {
			lRVar = (lRMean - 0.5) * (1 - lRMean) - 0.000001;
		}

		double alfa = 2 / lRVar * (lRMean - 0.5)
				* ((lRMean - 0.5) * (1 - lRMean) - lRVar);
		double beta = 2 / lRVar * (1 - lRMean)
				* ((lRMean - 0.5) * (1 - lRMean) - lRVar);
		learningRateDistrib = RandomHelper.createBeta(alfa, beta);

		Firm.supplyManager = this;

	}

	@ScheduledMethod(start = 1d, interval = 1, shuffle = true)
	public void step() {

		// Manage Entry
		int potentialEntrantsPerPeriod = (int) round(entrantsNormal
				.nextDouble() / periods);
		if (potentialEntrantsPerPeriod > 0)
			entry(potentialEntrantsPerPeriod);

		processOffers();

		// Planning
		IndexedIterable<Object> firms = context.getObjects(Firm.class);
		for (Object f : firms)
			((Firm) f).plan();

	}

	private void entry(int potentialEntrants) {

		Firm f;
		bornFirms = 0;

		for (int j = 1; j <= potentialEntrants; j++) {

			// Destroy if not profitable
			f = new Firm(context);
			if (f.checkEntry())
				bornFirms++;
			else
				RemoveAgentFromModel(f);

		}

	}

	private void processOffers() {

		IndexedIterable<Object> firms = context.getObjects(Firm.class);

		totalFirms = firms.size();

		if (totalFirms == 0.0) {
			totalQuantity = 0.0;
		} else {
			double tmpQ = 0.0;
			for (Object f : firms) {

				tmpQ += ((Firm) f).offer();

			}

			totalQuantity = tmpQ;
		}

		price = Demand.price(totalQuantity);

		List<Firm> toKill = new ArrayList<Firm>(firms.size());

		for (Object f : context.getObjects(Firm.class)) {

			// Process Demand Response
			if (!((Firm) f).processDemandResponse()) {
				toKill.add((Firm) f);
			}

		}

		dead = toKill.size();
		for (Firm f : toKill) {
			RemoveAgentFromModel(f);
		}

		firms = context.getObjects(Firm.class);
		totalFBeforeExit = totalFirms;
		totalFirms = firms.size();
		totalQBeforeExit = totalQuantity;

		if (totalFirms == 0.0) {
			totalQuantity = 0.0;
		} else {
			double tmpQ = 0.0;
			for (Object f : firms) {
				tmpQ += ((Firm) f).currentDecision.quantity;
			}
			totalQuantity = tmpQ;
		}

	}

	public String toString() {

		return "SupplyManager";

	}

	public double getCapacityUsed() {
		return Demand.getCapacityUsed();
	}

	@Parameter(displayName = "Total Quantity", usageName = "totalQuantity")
	public double getTotalQuantity() {
		return totalQuantity;
	}

	@Parameter(displayName = "Price", usageName = "price")
	public double getPrice() {
		return price;
	}

	@Parameter(displayName = "Dead", usageName = "dead")
	public double getDead() {
		return dead;
	}

	@Parameter(displayName = "Born Firms", usageName = "bornFirms")
	public int getBornFirms() {
		return bornFirms;
	}

	@Parameter(displayName = "Total Firms", usageName = "totalFirms")
	public double getTotalFirms() {
		return totalFirms;
	}

	@Parameter(displayName = "Total Q Before Exit", usageName = "totalQBeforeExit")
	public double getTotalQBeforeExit() {
		return totalQBeforeExit;
	}

	@Parameter(displayName = "Total F Before Exit", usageName = "totalFBeforeExit")
	public double getTotalFBeforeExit() {
		return totalFBeforeExit;
	}

}
