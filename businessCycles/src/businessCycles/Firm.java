package businessCycles;

import static java.lang.Math.*;
import static repast.simphony.essentials.RepastEssentials.*;

import org.apache.commons.math3.util.FastMath;

public class Firm {

	static SupplyManager supplyManager;
	private static int periods;

	// Local state variables - change every period
	private double quantityPerPeriod;
	private double localPrice;
	private double capital = 0.0;

	private double performancePerPeriod = 0.0;
	private double acumQ = 0.0;
	private double acumProfit = 0.0;
	private double profitPerPeriod = 0.0;
	private double annualMaxFunding = 0.0;
	private double desiredAnnualNetInvestment = 0.0;
	private double annualInvestment = 0.0;
	private double annualInternalFunding = 0.0;
	private double flexibilityCost = 0.0;
	private double unitProdCost = 0.0;
	private double learningComponent = 0.0;
	private double operatingLevarageComponent = 0.0;
	private double flexibilityCostComponent = 0.0;
	private double totalProdCostPerPeriod = 0.0;
	private double totalNonProdCostPerPeriod = 0.0;
	private double optimalFullCapacityMarkUp = 0.0;
	private double longTermAnnualMarginalCost = 0.0;
	private double shortTermMarginalCost = 0.0;
	private double currentMarkUpForPlanning = 0.0;

	// Local firm variables - stable along firm life
	private double firstUnitCost = 0.0;
	private double operatingLeverage = 0.0;
	private double learningRate = 0.0;
	private double expon = 0.0;
	private double bornTick = 0.0;

	// Static Firm variables - identical for all firms
	private static double minLearningCostPerUnit;
	private static double minCapital;
	private static double costOfCapitalPerPeriod;

	private static double perfWeight;
	private static double minPerformancePerPeriod;

	private static double demElast;
	private static double supElast;

	private static double operatingLeverageMid;
	private static double operatingLeverageScale;
	private static double flexCostScale;
	private static double deprecPerPeriod;
	private static double maxExtAnnualFunding;
	private static double annualInvParam;

	private static long agentIDCounter;
	private long agentID = agentIDCounter++;

	public Firm() {

		supplyManager.add(this);

		bornTick = GetTickCount();

		// A minimum first unit cost is set to 10% of mean
		firstUnitCost = max(0.1 * (Double) GetParameter("firstUnitCostMean"),
				supplyManager.firstUnitCostNormal.nextDouble());

		capital = max((Double) GetParameter("minimumCapital"), supplyManager.iniKNormal.nextDouble());

		operatingLeverage = supplyManager.operatingLeverageUniform.nextDouble();

		// Initial perfomance is set to minPerformance
		performancePerPeriod = minPerformancePerPeriod;

		// Learning rate is a truncated Normal
		// 0.5 < learning rate <= 1.0
		learningRate = min(0.99, max(supplyManager.learningRateNormal.nextDouble(), 0.51));
		expon = log(learningRate) / log(2.0);

	}

	public static void readParams() {
		// Static Firm variables - identical for all firms

		periods = SupplyManager.periods;

		minLearningCostPerUnit = (Double) GetParameter("minLearningCostPerUnit");
		minCapital = (Double) GetParameter("minimumCapital");
		costOfCapitalPerPeriod = (Double) GetParameter("costOfCapital") / periods;

		perfWeight = (Double) GetParameter("performanceWeight");
		minPerformancePerPeriod = (Double) GetParameter("minimumPerformance") / periods;

		demElast = (Double) GetParameter("demandElasticity");
		supElast = (Double) GetParameter("supplyElasticity");

		double opLevMin = (Double) GetParameter("operatingLeverageMin");
		double opLevMax = (Double) GetParameter("operatingLeverageMax");
		operatingLeverageMid = (opLevMin + opLevMax) / 2.0;
		operatingLeverageScale = (opLevMax - opLevMin) / 2.0;

		flexCostScale = (Double) GetParameter("flexibilityCostScale");
		deprecPerPeriod = ((Double) GetParameter("depreciation")) / periods;
		maxExtAnnualFunding = (Double) GetParameter("maxExternalFunding");
		annualInvParam = (Double) GetParameter("investmentParam");

		agentIDCounter = 1;
	}

	public boolean checkEntry() {

		double expectedPerPeriodQ = SupplyManager.fullCapacityQuantityPerPeriod(capital);

		double expectedReturnPerPeriod = profitPerPeriod(expectedPerPeriodQ, getIndustryPrice()) / getCapital();

		return (expectedReturnPerPeriod >= minPerformancePerPeriod);

	}

	public double applyAnnualPlannedInvestment() {

		if (isPlanningTick(GetTickCount() - 1)) {
			capital = capital * (1.0 - deprecPerPeriod * periods) + annualInvestment;

			// capital should be at least 1 to be able to produce at least one unit
			capital = max(1.0, capital);
		}

		return capital;

	}

	public double offer() {
				
		if (RecessionHandler.inRecession()) {
			// Local monopoly maximization
			double monopolyPrice = getCalculatedShortTermMarginalCost() * demElast / (demElast - 1.0);

			// local price should between the two full capacity prices: reces and no-reces
			localPrice = FastMath.max(monopolyPrice, supplyManager.getAfterEntryFullCapacityPrice());
			localPrice = FastMath.min(localPrice, supplyManager.getNoRecessionAfterEntryFullCapacityPrice());
			
			// quantity corresponds to the local price chosen
			quantityPerPeriod = Demand.inverseDemandPerPeriod(localPrice) * getCapitalShareAfterEntry();
			
		} else {
			localPrice = supplyManager.getAfterEntryFullCapacityPrice();
			quantityPerPeriod = SupplyManager.fullCapacityQuantityPerPeriod(capital);
			
		}
			
		return quantityPerPeriod;

	}

	public double profitPerPeriod(double q, double p) {

		profitPerPeriod = p * q - totalProdCostPerPeriod(q) - totalNonProdCostPerPeriod();
		return profitPerPeriod;

	}

	private double totalProdCostPerPeriod(double quantityPerPeriod) {

		double usedCapacity = quantityPerPeriod / SupplyManager.fullCapacityQuantityPerPeriod(capital);

		unitProdCost = learningComponent() * operatingLeverageComponent(usedCapacity) * flexibilityCostComponent();
		totalProdCostPerPeriod = unitProdCost * quantityPerPeriod;

		return totalProdCostPerPeriod;
	}

	/*
	 * learning curve effect
	 */
	private double learningComponent() {

		double learningFactor = ((acumQ >= 1) ? pow(acumQ, expon) : 1);

		learningComponent = firstUnitCost * learningFactor + minLearningCostPerUnit;

		return learningComponent;
	}

	/*
	 * Adjusts cost to take into account variable and fixed costs equals 1 at full
	 * capacity
	 */
	private double operatingLeverageComponent(double usedCapacity) {

		assert usedCapacity != 0.0;

		operatingLevarageComponent = operatingLeverage / usedCapacity + (1 - operatingLeverage);
		return operatingLevarageComponent;
	}

	/*
	 * Increases cost for flexible firms (low operating leverage)
	 */
	private double flexibilityCostComponent() {
		flexibilityCostComponent = 1
				+ flexCostScale * (operatingLeverageMid - operatingLeverage) / operatingLeverageScale;
		return flexibilityCostComponent;
	}

	private double totalNonProdCostPerPeriod() {
		totalNonProdCostPerPeriod = (costOfCapitalPerPeriod + deprecPerPeriod) * capital;
		return totalNonProdCostPerPeriod;
	}

	/*
	 * 
	 * Process demand respond and returns false if firm exits the industry,
	 * otherwise returns true
	 * 
	 * @method processDemandResponse
	 * 
	 */
	public boolean processDemandResponse() {

		if (!RecessionHandler.inRecession()) {
			localPrice = getIndustryPrice(); 
		}
		
		// Calculates profit & Performance
		profitPerPeriod = profitPerPeriod(quantityPerPeriod, localPrice);
		performancePerPeriod = perfWeight * performancePerPeriod + (1 - perfWeight) * profitPerPeriod / capital;

		// add to accumulators
		acumQ += quantityPerPeriod;
		acumProfit += profitPerPeriod;
		annualInternalFunding += profitPerPeriod + (deprecPerPeriod + costOfCapitalPerPeriod) * capital;

		// if it is an exit, returns false
		return !(performancePerPeriod < minPerformancePerPeriod || capital < minCapital);

	}

	public void annualPlan() {

		if (!isPlanningTick(GetTickCount()))
			return;

		annualMaxFunding = annualInternalFunding + externalFunding();

		// It is assumed that internal funding not used for investments is distributed
		// as dividends
		annualInternalFunding = 0.0;
		desiredAnnualNetInvestment = annualNetInvestment();

		annualInvestment = min(annualMaxFunding, capital * (deprecPerPeriod * periods + desiredAnnualNetInvestment));

		// It is not allowed to disinvest
		annualInvestment = max(0.0, annualInvestment);

	}

	private boolean isPlanningTick(double tick) {

		// End of year
		return ((tick >= bornTick) && ((tick % periods) == 0));

	}

	private double externalFunding() {

		if (RecessionHandler.inRecession())
			return 0.0;
		else
			return maxExtAnnualFunding * capital;
	}

	private double annualNetInvestment() {

		optimalFullCapacityMarkUp = (demElast + (1 - getCapitalShareAfterExit()) * supElast)
				/ (demElast + (1 - getCapitalShareAfterExit()) * supElast - getCapitalShareAfterExit());

		double planningPrice = supplyManager.getNoRecessionAfterExitFullCapacityPrice();
		currentMarkUpForPlanning = planningPrice / longTermAnnualMarginalCost();

		// As planning is done using full capacity data, idle capacity is not taken into account
		return annualInvParam * (1 - optimalFullCapacityMarkUp / currentMarkUpForPlanning);

	}

	/*
	 * Calculates marginal cost at full capacity
	 */
	private double longTermAnnualMarginalCost() {
		longTermAnnualMarginalCost = learningComponent() * flexibilityCostComponent()
				+ (costOfCapitalPerPeriod + deprecPerPeriod) * periods;
		return longTermAnnualMarginalCost;
	}

	public double getDesiredAnnualNetInvestment() {
		return desiredAnnualNetInvestment;
	}

	/*
	 * Calculates marginal cost when capital is fixed
	 */
	public double getCalculatedShortTermMarginalCost() {
		shortTermMarginalCost = learningComponent() * flexibilityCostComponent() * (1 - operatingLeverage);

		return shortTermMarginalCost;
	}

	public double getMarketShare() {

		return quantityPerPeriod / supplyManager.getTotalQuantityPerPeriod();

	}

	public double getCapitalShareAfterExit() {
		return capital / supplyManager.getTotalCapitalAfterExit();
	}

	public double getCapitalShareAfterEntry() {
		return capital / supplyManager.getTotalCapitalAfterEntry();
	}

	public double getIndustryPrice() {
		return supplyManager.getIndustryPrice();
	}

	public double getLocalPrice() {
		return localPrice;
	}

	public double getQuantityPerPeriod() {
		return quantityPerPeriod;
	}

	public double getQuantity() {
		// annualized quantity
		return quantityPerPeriod * periods;
	}

	public double getProfit() {
		return profitPerPeriod * periods;
	}

	public double getPerformance() {
		return performancePerPeriod * periods;
	}

	public double getReturn() {
		return profitPerPeriod * periods / capital;
	}

	public double getFirstUnitCost() {
		return firstUnitCost;
	}

	public double getAcumQuantity() {
		return acumQ;
	}

	public String toString() {
		return "Firm" + this.agentID;
	}

	public long getFirmNumID() {
		return agentID;
	}

	public double getYearOfBirth() {
		return bornTick / periods;
	}

	public double getBornTick() {
		return bornTick;
	}

	public double getAge() {
		return (GetTickCount() - bornTick) / periods;
	}

	public double getExpon() {
		return expon;
	}

	public double getAcumProfit() {
		return acumProfit;
	}

	public double getMedCost() {
		return (totalProdCostPerPeriod + totalNonProdCostPerPeriod) / quantityPerPeriod;
	}

	public double getShortTermMarginalCost() {
		return shortTermMarginalCost;
	}

	public double getLongTermMarginalCost() {
		return longTermAnnualMarginalCost;
	}

	public double getTotalProdCost() {
		return totalProdCostPerPeriod * periods;
	}

	public double getUnitProdCost() {
		return unitProdCost;
	}

	public double getLearningComponent() {
		return learningComponent;
	}

	public double getOperatingLevarageComponent() {
		return operatingLevarageComponent;
	}

	public double getFlexibilityCostComponent() {
		return flexibilityCostComponent;
	}

	public double getTotalNonProdCost() {
		return totalNonProdCostPerPeriod * periods;
	}

	public double getNonProdCostPerUnit() {
		return totalNonProdCostPerPeriod / quantityPerPeriod;
	}

	public double getOperatingLeverage() {
		return operatingLeverage;
	}

	public double getUsedCapacity() {
		return quantityPerPeriod / SupplyManager.fullCapacityQuantityPerPeriod(capital);
	}

	public double getLearningRate() {
		return learningRate;
	}

	public double getCapital() {
		return capital;
	}

	public double getMaxFunding() {
		return annualMaxFunding;
	}

	public double getInvest() {
		return annualInvestment;
	}

	public double getFlexibilityCost() {
		return flexibilityCost;
	}

	public static double getFlexCostScale() {
		return flexCostScale;
	}

	public double getOptimalFullCapacityMarkUp() {
		return optimalFullCapacityMarkUp;
	}

	public double getCurrentMarkUpForPlanning() {
		return currentMarkUpForPlanning;
	}

}
