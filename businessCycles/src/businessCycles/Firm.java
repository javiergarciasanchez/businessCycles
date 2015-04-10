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

import static java.lang.Math.*;
import static repast.simphony.essentials.RepastEssentials.*;
import repast.simphony.context.Context;

/**
 * 
 * This is an agent.
 * 
 */
public class Firm {

	public static SupplyManager supplyManager;

	public class Decision {
		double quantity = 0.0;
		double rD = 0.0;
	}

	private int timeCohort = 0;
	private int opLevCohort = 0;

	public Decision currentDecision;
	public Decision nextDecision;

	// Local state variables - change every period
	private double performance = 0.0;
	private double capital = 0.0;
	private double acumQ = 0.0;
	private double acumProfit = 0.0;

	// Local firm variables - stable along firm life
	private double firstUnitCost = 0.0;
	private double rDEfficiency = 0.0;
	private double operatingLeverage = 0.0;
	private double expon = 0.0;
	private double born = 0.0;

	// Static Firm variables - identical for all firms
	private static double capitalProductivityPerPeriod;
	private static double minVarCost;
	private static double minCapital;
	private static double costOfCapitalPerPeriod;

	private static double perfWeight;
	private static double minPerformance;

	private static double demElast;
	private static double supElast;

	private static double opLevMean;
	private static double opLevStdDev;
	private static double flexCostMean;
	private static double flexCostStdDev;
	private static double deprec;
	private static double deprecPerPeriod;
	private static double maxExtFundPerPeriod;
	private static double invParam;

	private static long agentIDCounter;
	private String agentID = "Firm " + (agentIDCounter++);

	public Firm(Context<Object> context) {

		context.add(this);

		currentDecision = new Decision();
		nextDecision = new Decision();
		nextDecision.rD = 0.0;

		born = GetTickCount();
		timeCohort = getCohort(born, supplyManager.timeCohortLimits);

		// A minimum first unit cost is set to 10% of mean
		firstUnitCost = max(0.1 * (Double) GetParameter("firstUnitCostMean"),
				supplyManager.firstUnitCostNormal.nextDouble());

		capital = max((Double) GetParameter("minimumCapital"),
				supplyManager.iniKNormal.nextDouble());
		nextDecision.quantity = capital * capitalProductivityPerPeriod;

		operatingLeverage = min(1.0,
				max(0.0, supplyManager.operatingLeverageNormal.nextDouble()));
		opLevCohort = getCohort(operatingLeverage,
				supplyManager.opLevCohortLimits);

		// Initial perfomance is set to minPerformance
		performance = minPerformance;

		// 0.5 < learning rate <= 1.0
		double learningRate = min(1.0,
				max(supplyManager.learningRateDistrib.nextDouble(), 0.51));
		expon = log(learningRate) / log(2.0);
		rDEfficiency = max(0.0, supplyManager.rDEfficiencyNormal.nextDouble());

	}

	private int getCohort(double value, double[] cohortLimits) {

		for (int i = 0; i < cohortLimits.length; i++) {
			if (value < cohortLimits[i])
				return i + 1;
		}

		return cohortLimits.length + 1;

	}

	public static void readParams() {
		// Static Firm variables - identical for all firms
		capitalProductivityPerPeriod = (Double) GetParameter("capitalProductivity")
				/ SupplyManager.periods;
		minVarCost = (Double) GetParameter("minVarCost");
		minCapital = (Double) GetParameter("minimumCapital");
		costOfCapitalPerPeriod = (Double) GetParameter("costOfCapital")
				/ SupplyManager.periods;

		perfWeight = (Double) GetParameter("performanceWeight");
		minPerformance = (Double) GetParameter("minimumPerformance");

		demElast = (Double) GetParameter("demandElasticity");
		supElast = (Double) GetParameter("supplyElasticity");

		opLevMean = (Double) GetParameter("operatingLeverageMean");
		opLevStdDev = (Double) GetParameter("operatingLeverageStdDev")
				* opLevMean;
		flexCostMean = (Double) GetParameter("flexibilityCostMean");
		flexCostStdDev = (Double) GetParameter("flexibilityCostStdDev")
				* flexCostMean;
		deprec = (Double) GetParameter("depreciation");
		deprecPerPeriod = deprec / SupplyManager.periods;
		maxExtFundPerPeriod = (Double) GetParameter("maxExternalFunding")
				/ SupplyManager.periods;
		invParam = (Double) GetParameter("investmentParam");

		agentIDCounter = 1;
	}

	public boolean checkEntry() {
		double expectedAnnualReturn = profit(nextDecision, getPrice())
				* SupplyManager.periods / getCapital();

		return (expectedAnnualReturn >= minPerformance);

	}

	public double offer() {

		currentDecision.quantity = Demand.getCapacityUsed()
				* nextDecision.quantity;

		currentDecision.rD = nextDecision.rD;
		return currentDecision.quantity;

	}

	public double profit(Decision decision, double price) {

		return price * decision.quantity - totalProdCost(decision)
				- totalNonProdCost(decision);

	}

	/*
	 * Calculates cost using learning curve: cost of new accumulated Q minus old
	 * accumulated Q. See http://maaw.info/LearningCurveSummary.htm (Wright
	 * model)
	 */
	private double totalProdCost(Decision decision) {

		// Get Learning curve cost + productive minimum cost
		double lc = firstUnitCost
				* (pow(acumQ + decision.quantity, 1.0 + expon) - pow(acumQ,
						1.0 + expon)) + minVarCost * decision.quantity;

		// Return flexibility adjusted value
		return lc * operatingLeverageAdjustment();

	}

	/*
	 * Adds the cost of flexibility. Increases variable cost in firms that have
	 * lower fixed costs. If opLev = 100% (All costs depend on size) adjustment
	 * = 1 If capacityUsed = 100% adjustment = flexibilityCost
	 */
	private double operatingLeverageAdjustment() {

		double fC = flexCostMean + (opLevMean - operatingLeverage)
				* flexCostStdDev / opLevStdDev;

		// Flexibility cost cannot be lower than 10% of mean
		fC = max(0.1 * flexCostMean, fC);

		return fC
				* (operatingLeverage + (1 - operatingLeverage)
						* Demand.getCapacityUsed());

	}

	private double totalNonProdCost(Decision decision) {
		return (costOfCapitalPerPeriod + deprecPerPeriod) * capital
				+ decision.rD;
	}

	/**
	 * 
	 * Process demand respond and returns false if firm exits the industry,
	 * otherwise returns true
	 * 
	 * @method processDemandResponse
	 * 
	 */
	public boolean processDemandResponse() {

		// Define the return value variable.
		boolean returnValue;

		// Calculates profit & Performance
		performance = perfWeight * performance + (1 - perfWeight) * getReturn();

		// if it is an exit, returns false
		returnValue = !(performance < minPerformance || capital < minCapital);

		acumQ += currentDecision.quantity;
		acumProfit += getProfit();

		return returnValue;

	}

	public void plan() {

		double maxFunding = getProfit()
				+ deprecPerPeriod
				* capital
				+ ((Demand.getSSMagnitude() > 0.0) ? 0.0
						: (maxExtFundPerPeriod * capital));

		double invest = min(maxFunding, capital
				* (deprecPerPeriod + netInvestment()));

		invest = max(0.0, invest);

		capital = capital * (1.0 - deprecPerPeriod) + invest;

		nextDecision.quantity = capital * capitalProductivityPerPeriod;

		// Then new R&D is determined to optimize First unit cost.The maxFunding
		// is relevant to speed up the process.
		double optimalRD = pow(
				firstUnitCost
						/ rDEfficiency
						* (pow(acumQ + nextDecision.quantity, 1.0 + expon) - pow(
								acumQ, 1.0 + expon))
						* operatingLeverageAdjustment(), 0.5) - 1.0;

		double minRD = 1.0 / rDEfficiency - 1.0;

		nextDecision.rD = max(minRD, min(maxFunding - invest, optimalRD));

		// apply innovation
		firstUnitCost *= 1.0 / ((nextDecision.rD + 1.0) * rDEfficiency)
				* supplyManager.innovationErrorNormal.nextDouble();

	}

	/*
	 * It is derived maximizing economic profit (ie substracting cost of
	 * capital)
	 */
	private double marginalCost() {

		return (firstUnitCost * (1.0 + expon)
				* pow(acumQ + currentDecision.quantity, expon) + minVarCost)
				* operatingLeverageAdjustment()
				+ (costOfCapitalPerPeriod + deprecPerPeriod)
				/ capitalProductivityPerPeriod;

	}

	private double netInvestment() {

		double optimalMarkUp = (demElast + (1 - getMarketShare()) * supElast)
				/ (demElast + (1 - getMarketShare()) * supElast - getMarketShare());

		return invParam * (1 - optimalMarkUp * marginalCost() / getPrice());

	}

	/*
	 * Methods to probe firm state
	 */
	public double getMarketShare() {

		return getQuantity() / supplyManager.getTotalQuantity();

	}

	public double getPrice() {
		return supplyManager.price;
	}

	public double getQuantity() {
		return currentDecision.quantity;
	}

	public double getProfit() {
		return profit(currentDecision, getPrice());
	}

	public double getPerformance() {
		return performance;
	}

	public double getRD() {
		return currentDecision.rD;
	}

	public double getReturn() {
		return getProfit() * SupplyManager.periods / getCapital();
	}

	public double getFirstUnitCost() {
		return firstUnitCost;
	}

	public double getAcumQuantity() {
		return acumQ;
	}

	public String toString() {
		return this.agentID;
	}

	public double getBorn() {
		return born;
	}

	public double getAge() {
		return GetTickCount() - getBorn();
	}

	public double getExpon() {
		return expon;
	}

	public double getAcumProfit() {
		return acumProfit;
	}

	public double getMedCost() {
		return (getPrice() * getQuantity() - getProfit()) / getQuantity();
	}

	public double getTotalVarCost() {
		return totalProdCost(currentDecision);
	}

	public double getVarCost() {
		return getTotalVarCost() / getQuantity();
	}

	public double getVarCostWoAdj() {
		return getVarCost() / getOperatingLeverageAdjustment();
	}

	public double getTotalFixedCost() {
		return totalNonProdCost(currentDecision);
	}

	public double getFixedCostPerUnit() {
		return getTotalFixedCost() / getQuantity();
	}

	public double getOperatingLeverage() {
		return operatingLeverage;
	}

	public double getOperatingLeverageAdjustment() {
		return operatingLeverageAdjustment();
	}

	public double getCapital() {
		return capital;
	}

	public int getTimeCohort() {
		return timeCohort;
	}

	public int getOpLevCohort() {
		return opLevCohort;
	}

}
