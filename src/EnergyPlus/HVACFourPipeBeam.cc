// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirTerminalUnit.hh>
#include <HVACFourPipeBeam.hh>
#include <BranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>


namespace EnergyPlus {

namespace FourPipeBeam {

	Array1D< std::shared_ptr< HVACFourPipeBeam > > FourPipeBeams; // dimension to number of machines

	HVACFourPipeBeam::HVACFourPipeBeam(){}

	std::shared_ptr< AirTerminalUnit > 
	HVACFourPipeBeam::FourPipeBeamFactory(
		int EP_UNUSED(objectType),
		std::string objectName
	){
	
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using BranchNodeConnections::SetUpCompSets;
		using DataZoneEquipment::ZoneEquipConfig;
		using namespace DataSizing;
		using DataDefineEquip::AirDistUnit;
		using DataDefineEquip::NumAirDistUnits;
		using CurveManager::GetCurveIndex;
		using namespace DataIPShortCuts;
		using ScheduleManager::GetScheduleIndex;

		static std::string const routineName( "FourPipeBeamFactory " ); // include trailing blank space

		int beamIndex; // loop index


		static int NumAlphas( 0 ); // Number of Alphas for each GetObjectItem call
		static int NumNumbers( 0 ); // Number of Numbers for each GetObjectItem call

		//  certain object in the input file
		int IOStatus; // Used in GetObjectItem
		bool errFlag = false;
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool found = false;
		int ctrlZone; // controlled zome do loop index
		int supAirIn; // controlled zone supply air inlet index
		bool airNodeFound;
		int aDUIndex;

		std::shared_ptr< HVACFourPipeBeam > thisBeam( new HVACFourPipeBeam() );

		// find the number of cooled beam units
		cCurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam";
		NumFourPipeBeams = GetNumObjectsFound( cCurrentModuleObject );
		// allocate the data structures
		FourPipeBeam.allocate( NumFourPipeBeams );
		CheckEquipName.dimension( NumFourPipeBeams, true );

		NumAlphas = 16;
		NumNumbers = 11;


		// find beam index from name
		beamIndex = InputProcessor::GetObjectItemNum(cCurrentModuleObject, objectName );
		if ( beamIndex > 0 ) {
			InputProcessor::GetObjectItem( cCurrentModuleObject, beamIndex, cAlphaArgs, NumAlphas,
				 rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				found = true;
		} else {
			ErrorsFound = true;
		}

		GlobalNames::VerifyUniqueADUName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
		if ( errFlag ) {
			ErrorsFound = true;
		}
		thisBeam->name = cAlphaArgs( 1 );
		thisBeam->unitType = cCurrentModuleObject;

		if ( lAlphaFieldBlanks( 2 ) ) {
			thisBeam->airAvailSchedNum = ScheduleAlwaysOn;
		} else {
			thisBeam->airAvailSchedNum = GetScheduleIndex( cAlphaArgs( 2 ) ); // convert schedule name to pointer
			if ( thisBeam->airAvailSchedNum  == 0 ) {
				ShowSevereError( routineName + cCurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
		}
		if ( lAlphaFieldBlanks( 3 ) ) {
			thisBeam->coolingAvailSchedNum = ScheduleAlwaysOn;
		} else {
			thisBeam->coolingAvailSchedNum = GetScheduleIndex( cAlphaArgs( 3 ) ); // convert schedule name to index
			if ( thisBeam->coolingAvailSchedNum  == 0 ) {
				ShowSevereError( routineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 3 ) + " entered =" + cAlphaArgs( 3 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
		}
		if ( lAlphaFieldBlanks( 4 ) ) {
			thisBeam->heatingAvailSchedNum = ScheduleAlwaysOn;
		} else {
			thisBeam->heatingAvailSchedNum = GetScheduleIndex( cAlphaArgs( 4 ) ); // convert schedule name to index
			if ( thisBeam->heatingAvailSchedNum  == 0 ) {
				ShowSevereError( routineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 4 ) + " entered =" + cAlphaArgs( 4 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
		}

		thisBeam->airInNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFieldNames( 5 ) );
		thisBeam->airOutNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFieldNames( 6 ) );
		if ( lAlphaFieldBlanks( 7 ) && lAlphaFieldBlanks( 8 ) ) {
			thisBeam->beamCoolingPresent = false;
		} else {
			thisBeam->beamCoolingPresent = true;
			thisBeam->cWInNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent, cAlphaFieldNames( 7 ) );
			thisBeam->cWOutNodeNum = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent, cAlphaFieldNames( 8 ) );
		}
		if ( lAlphaFieldBlanks( 9 ) && lAlphaFieldBlanks( 10 ) ) {
			thisBeam->beamHeatingPresent = false;
		} else {
			thisBeam->beamHeatingPresent = true;
			thisBeam->hWInNodeNum = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent, cAlphaFieldNames( 9 ) );
			thisBeam->hWOutNodeNum = GetOnlySingleNode( cAlphaArgs( 10 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent, cAlphaFieldNames( 10 ) );
		}
		thisBeam->vDotDesignPrimAir = rNumericArgs( 1 );
		if ( thisBeam->vDotDesignPrimAir == AutoSize ) {
			thisBeam->vDotDesignPrimAirWasAutosized = true;
		}
		thisBeam->vDotDesignCW = rNumericArgs( 2 );
		if ( thisBeam->vDotDesignCW == AutoSize ) {
			thisBeam->vDotDesignCWWasAutosized = true;
		}
		thisBeam->vDotDesignHW = rNumericArgs( 3 );
		if ( thisBeam->vDotDesignHW == AutoSize ) {
			thisBeam->vDotDesignHWWasAutosized = true;
		}
		thisBeam->totBeamLength = rNumericArgs( 4 );
		if ( thisBeam->totBeamLength ==  AutoSize ) {
			thisBeam->totBeamLengthWasAutosized = true;
		}
		thisBeam->vDotNormRatedPrimAir  = rNumericArgs( 5 );
		thisBeam->qDotNormRatedCooling  = rNumericArgs( 6 );
		thisBeam->deltaTempRatedCooling = rNumericArgs( 7 );
		thisBeam->vDotNormRatedCW       = rNumericArgs( 8 );

		thisBeam->modCoolingQdotDeltaTFuncNum = GetCurveIndex( cAlphaArgs( 11 ) );
		if ( thisBeam->modCoolingQdotDeltaTFuncNum == 0 && thisBeam->beamCoolingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 11 ) + '=' + cAlphaArgs( 11 ) );
			ErrorsFound = true;
		}
		thisBeam->modCoolingQdotAirFlowFuncNum = GetCurveIndex( cAlphaArgs( 12 ) );
		if ( thisBeam->modCoolingQdotAirFlowFuncNum == 0 && thisBeam->beamCoolingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 12 ) + '=' + cAlphaArgs( 12 ) );
			ErrorsFound = true;
		}
		thisBeam->modCoolingQdotCWFlowFuncNum = GetCurveIndex( cAlphaArgs( 13 ) );
		if ( thisBeam->modCoolingQdotCWFlowFuncNum == 0 && thisBeam->beamCoolingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 13 ) + '=' + cAlphaArgs( 13 ) );
			ErrorsFound = true;
		}
		thisBeam->qDotNormRatedHeating  = rNumericArgs( 9 );
		thisBeam->deltaTempRatedHeating = rNumericArgs( 10 );
		thisBeam->vDotNormRatedHW       = rNumericArgs( 11 );
		thisBeam->modHeatingQdotDeltaTFuncNum = GetCurveIndex( cAlphaArgs( 14 ) );
		if ( thisBeam->modHeatingQdotDeltaTFuncNum == 0 && thisBeam->beamHeatingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 14 ) + '=' + cAlphaArgs( 14 ) );
			ErrorsFound = true;
		}
		thisBeam->modHeatingQdotAirFlowFuncNum = GetCurveIndex( cAlphaArgs( 15 ) );
		if ( thisBeam->modHeatingQdotAirFlowFuncNum == 0 && thisBeam->beamHeatingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 15 ) + '=' + cAlphaArgs( 15 ) );
			ErrorsFound = true;
		}
		thisBeam->modHeatingQdotHWFlowFuncNum = GetCurveIndex( cAlphaArgs( 16 ) );
		if ( thisBeam->modHeatingQdotHWFlowFuncNum == 0 && thisBeam->beamHeatingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 16 ) + '=' + cAlphaArgs( 16 ) );
			ErrorsFound = true;
		}
		// Register component set data
		TestCompSet( cCurrentModuleObject, thisBeam->name, NodeID( thisBeam->airInNodeNum ), 
						NodeID( thisBeam->airOutNodeNum ), "Air Nodes" );
		if ( thisBeam->beamCoolingPresent ) {
			TestCompSet( cCurrentModuleObject, thisBeam->name, NodeID( thisBeam->cWInNodeNum ),
						NodeID( thisBeam->cWOutNodeNum ), "Water Nodes" );
		}
		if ( thisBeam->beamHeatingPresent ) {
			TestCompSet( cCurrentModuleObject, thisBeam->name, NodeID( thisBeam->hWInNodeNum ),
						NodeID( thisBeam->hWOutNodeNum ), "Water Nodes" );
		}

		//Setup the Cooled Beam reporting variables
		if ( thisBeam->beamCoolingPresent ) {
			SetupOutputVariable( "Zone Air Terminal Beam Sensible Cooling Energy [J]", 
				thisBeam->beamCoolingEnergy, "System", "Sum", thisBeam->name, _, 
				"ENERGYTRANSFER", "COOLINGCOILS", "Four Pipe Beam", "System" );
			SetupOutputVariable( "Zone Air Terminal Beam Sensible Cooling Rate [W]", 
				thisBeam->beamCoolingRate, "System", "Average", thisBeam->name );
		}
		if ( thisBeam->beamHeatingPresent ) {
			SetupOutputVariable( "Zone Air Terminal Beam Sensible Heating Energy [J]", 
				thisBeam->beamHeatingEnergy, "System", "Sum", thisBeam->name, _, 
				"ENERGYTRANSFER", "HEATINGCOILS", "Four Pipe Beam", "System" );
			SetupOutputVariable( "Zone Air Terminal Beam Sensible Heating Rate [W]", 
				thisBeam->beamHeatingRate, "System", "Average", thisBeam->name );
		}
		SetupOutputVariable( "Zone Air Terminal Primary Air Sensible Cooling Energy [J]", 
			thisBeam->supAirCoolingEnergy, "System", "Sum", thisBeam->name );
		SetupOutputVariable( "Zone Air Terminal Primary Air Sensible Cooling Rate [W]", 
			thisBeam->supAirCoolingRate, "System", "Average", thisBeam->name );
		SetupOutputVariable( "Zone Air Terminal Primary Air Sensible Heating Energy [J]", 
			thisBeam->supAirHeatingEnergy, "System", "Sum", thisBeam->name );
		SetupOutputVariable( "Zone Air Terminal Primary Air Sensible Heating Rate [W]", 
			thisBeam->supAirHeatingRate, "System", "Average", thisBeam->name );
		SetupOutputVariable( "Zone Air Terminal Primary Air Flow Rate [m3/s]", 
			thisBeam->primAirFlow, "System", "Average", thisBeam->name );

		// Fill the Zone Equipment data with the supply air inlet node number of this unit.
		airNodeFound = false;
		for ( ctrlZone = 1; ctrlZone <= NumOfZones; ++ctrlZone ) {
			if ( ! ZoneEquipConfig( ctrlZone ).IsControlled ) continue;
			for ( supAirIn = 1; supAirIn <= ZoneEquipConfig( ctrlZone ).NumInletNodes; ++supAirIn ) {
				if ( thisBeam->airOutNodeNum == ZoneEquipConfig( ctrlZone ).InletNode( supAirIn ) ) {
					ZoneEquipConfig( ctrlZone ).AirDistUnitCool( supAirIn ).InNode = thisBeam->airInNodeNum;
					ZoneEquipConfig( ctrlZone ).AirDistUnitCool( supAirIn ).OutNode = thisBeam->airOutNodeNum;
					if ( thisBeam->beamHeatingPresent ) {
						ZoneEquipConfig( ctrlZone ).AirDistUnitHeat( supAirIn ).InNode = thisBeam->airInNodeNum;
						ZoneEquipConfig( ctrlZone ).AirDistUnitHeat( supAirIn ).OutNode =thisBeam->airOutNodeNum;
					}
					airNodeFound = true;
					break;
				}
			}
		}
		if ( ! airNodeFound ) {
			ShowSevereError( "The outlet air node from the " + cCurrentModuleObject + " = " + thisBeam->name );
			ShowContinueError( "did not have a matching Zone Equipment Inlet Node, Node =" + cAlphaArgs( 5 ) );
			ErrorsFound = true;
		}


		for ( aDUIndex = 1; aDUIndex <= NumAirDistUnits; ++aDUIndex ) {
			if ( thisBeam->airOutNodeNum == AirDistUnit( aDUIndex ).OutletNodeNum ) {
				thisBeam->aDUNum = aDUIndex;
			}
		}
		// assumes if there isn't one assigned, it's an error
		if ( thisBeam->aDUNum == 0 ) {
			ShowSevereError( routineName + "No matching Air Distribution Unit, for Unit = [" + cCurrentModuleObject + ',' + thisBeam->name + "]." );
			ShowContinueError( "...should have outlet node=" + NodeID( thisBeam->airOutNodeNum ) );
			//          ErrorsFound=.TRUE.
		}

		if ( found && !ErrorsFound ) {
			FourPipeBeams.push_back( thisBeam );
			return thisBeam;
		} else { 
			ShowFatalError( routineName + "Errors found in getting input. Preceding conditions cause termination." );
			return nullptr;
		}

	}




	void
	HVACFourPipeBeam::simulate(
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		int const ZoneNum, // index of zone served by the unit
		int const ZoneNodeNum, // zone node number of zone served by the unit
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	)
	{

		// initialize the unit
		this->init( FirstHVACIteration );

		// control and simulate the beam
		this->control(  ZoneNum, ZoneNodeNum, FirstHVACIteration, NonAirSysOutput );

		// Update the current unit's outlet nodes. 
		this->update();

		// Fill the report variables. 
		this->report(  );

	}



	void
	HVACFourPipeBeam::init(

		bool const FirstHVACIteration // TRUE if first air loop solution this HVAC step
	)
	{


		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataDefineEquip::AirDistUnit;
		using InputProcessor::SameString;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_FourPipeBeamAirTerminal;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;


		static std::string const RoutineName( "HVACFourPipeBeam::init" );


		int InAirNode; // supply air inlet node number
		int OutAirNode; // unit air outlet node
		int InWaterNode; // unit inlet chilled water node
		int OutWaterNode; // unit outlet chilled water node
		Real64 RhoAir; // air density at outside pressure and standard temperature and humidity
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MySizeFlag;
		static Array1D_bool PlantLoopScanFlag;
		Real64 rho; // local fluid density
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop; // Loop checking control variable
		std::string CurrentModuleObject;
		bool errFlag;

		CurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam";
		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumCB );
			MySizeFlag.allocate( NumCB );
			PlantLoopScanFlag.allocate( NumCB );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			PlantLoopScanFlag = true;
			MyOneTimeFlag = false;

		}

		if ( PlantLoopScanFlag( CBNum ) && allocated( PlantLoop ) ) {
			errFlag = false;
			ScanPlantLoopsForObject( CoolBeam( CBNum ).Name, TypeOf_FourPipeBeamAirTerminal, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitCoolBeam: Program terminated for previous conditions." );
			}
			PlantLoopScanFlag( CBNum ) = false;

		}

		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			// Check to see if there is a Air Distribution Unit on the Zone Equipment List
			for ( Loop = 1; Loop <= NumCB; ++Loop ) {
				if ( CoolBeam( Loop ).ADUNum == 0 ) continue;
				if ( CheckZoneEquipmentList( "ZONEHVAC:AIRDISTRIBUTIONUNIT", AirDistUnit( CoolBeam( Loop ).ADUNum ).Name ) ) continue;
				ShowSevereError( "InitCoolBeam: ADU=[Air Distribution Unit," + AirDistUnit( CoolBeam( Loop ).ADUNum ).Name + "] is not on any ZoneHVAC:EquipmentList." );
				ShowContinueError( "...Unit=[" + CurrentModuleObject + ',' + CoolBeam( Loop ).Name + "] will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( CBNum ) && ! PlantLoopScanFlag( CBNum ) ) {

			SizeCoolBeam( CBNum );

			InWaterNode = CoolBeam( CBNum ).CWInNode;
			OutWaterNode = CoolBeam( CBNum ).CWOutNode;
			rho = GetDensityGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );
			CoolBeam( CBNum ).MaxCoolWaterMassFlow = rho * CoolBeam( CBNum ).MaxCoolWaterVolFlow;
			InitComponentNodes( 0.0, CoolBeam( CBNum ).MaxCoolWaterMassFlow, InWaterNode, OutWaterNode, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum );
			MySizeFlag( CBNum ) = false;

		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( CBNum ) ) {
			RhoAir = StdRhoAir;
			InAirNode = CoolBeam( CBNum ).AirInNode;
			OutAirNode = CoolBeam( CBNum ).AirOutNode;
			// set the mass flow rates from the input volume flow rates
			CoolBeam( CBNum ).MaxAirMassFlow = RhoAir * CoolBeam( CBNum ).MaxAirVolFlow;
			Node( InAirNode ).MassFlowRateMax = CoolBeam( CBNum ).MaxAirMassFlow;
			Node( OutAirNode ).MassFlowRateMax = CoolBeam( CBNum ).MaxAirMassFlow;
			Node( InAirNode ).MassFlowRateMin = 0.0;
			Node( OutAirNode ).MassFlowRateMin = 0.0;

			InWaterNode = CoolBeam( CBNum ).CWInNode;
			OutWaterNode = CoolBeam( CBNum ).CWOutNode;
			InitComponentNodes( 0.0, CoolBeam( CBNum ).MaxCoolWaterMassFlow, InWaterNode, OutWaterNode, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum );

			MyEnvrnFlag( CBNum ) = false;
		} // end one time inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( CBNum ) = true;
		}

		InAirNode = CoolBeam( CBNum ).AirInNode;
		OutAirNode = CoolBeam( CBNum ).AirOutNode;

		// Do the start of HVAC time step initializations
		if ( FirstHVACIteration ) {
			// check for upstream zero flow. If nonzero and schedule ON, set primary flow to max
			if ( GetCurrentScheduleValue( CoolBeam( CBNum ).SchedPtr ) > 0.0 && Node( InAirNode ).MassFlowRate > 0.0 ) {
				Node( InAirNode ).MassFlowRate = CoolBeam( CBNum ).MaxAirMassFlow;
			} else {
				Node( InAirNode ).MassFlowRate = 0.0;
			}
			// reset the max and min avail flows
			if ( GetCurrentScheduleValue( CoolBeam( CBNum ).SchedPtr ) > 0.0 && Node( InAirNode ).MassFlowRateMaxAvail > 0.0 ) {
				Node( InAirNode ).MassFlowRateMaxAvail = CoolBeam( CBNum ).MaxAirMassFlow;
				Node( InAirNode ).MassFlowRateMinAvail = CoolBeam( CBNum ).MaxAirMassFlow;
			} else {
				Node( InAirNode ).MassFlowRateMaxAvail = 0.0;
				Node( InAirNode ).MassFlowRateMinAvail = 0.0;
			}
			//Plant should do this    InWaterNode = CoolBeam(CBNum)%CWInNode
			//    Node(InWaterNode)%MassFlowRateMaxAvail = CoolBeam(CBNum)%MaxCoolWaterMassFlow
			//    Node(InWaterNode)%MassFlowRateMinAvail = 0.0
		}

		// do these initializations every time step
		InWaterNode = CoolBeam( CBNum ).CWInNode;
		CoolBeam( CBNum ).TWIn = Node( InWaterNode ).Temp;
		CoolBeam( CBNum ).SupAirCoolingRate = 0.0;
		CoolBeam( CBNum ).SupAirHeatingRate = 0.0;

		// CoolBeam(CBNum)%BeamFlow = Node(InAirNode)%MassFlowRate / (StdRhoAir*CoolBeam(CBNum)%NumBeams)

	}

	void
	SizeFourPipeBeam( int const CBNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 10, 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing cooled beam units for which flow rates have not been
		// specified in the input

		// METHODOLOGY EMPLOYED:
		// Accesses zone sizing array for air flow rates and zone and plant sizing arrays to
		// calculate coil water flow rates.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using DataGlobals::AutoCalculate;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		//  USE BranchInputManager,  ONLY: MyPlantSizingIndex
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "SizeCoolBeam" );
		static int PltSizCoolNum( 0 ); // index of plant sizing object for the cooling loop
		static int NumBeams( 0 ); // number of beams in the zone
		static int Iter( 0 ); // beam length iteration index
		static Real64 DesCoilLoad( 0.0 ); // total cooling capacity of the beams in the zone [W]
		static Real64 DesLoadPerBeam( 0.0 ); // cooling capacity per individual beam [W]
		static Real64 DesAirVolFlow( 0.0 ); // design total supply air flow rate [m3/s]
		static Real64 DesAirFlowPerBeam( 0.0 ); // design supply air volumetric flow per beam [m3/s]
		static Real64 RhoAir( 0.0 );
		static Real64 CpAir( 0.0 );
		static Real64 WaterVel( 0.0 ); // design water velocity in beam
		static Real64 IndAirFlowPerBeamL( 0.0 ); // induced volumetric air flow rate per beam length [m3/s-m]
		static Real64 DT( 0.0 ); // air - water delta T [C]
		static Real64 LengthX( 0.0 ); // test value for beam length [m]
		static Real64 Length( 0.0 ); // beam length [m]
		static Real64 ConvFlow( 0.0 ); // convective and induced air mass flow rate across beam per beam plan area [kg/s-m2]
		static Real64 K( 0.0 ); // coil (beam) heat transfer coefficient [W/m2-K]
		static Real64 WaterVolFlowPerBeam( 0.0 ); // Cooling water volumetric flow per beam [m3]
		bool ErrorsFound;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat

		PltSizCoolNum = 0;
		DesAirVolFlow = 0.0;
		CpAir = 0.0;
		RhoAir = StdRhoAir;
		ErrorsFound = false;
		// find the appropriate Plant Sizing object
		if ( CoolBeam( CBNum ).MaxAirVolFlow == AutoSize || CoolBeam( CBNum ).BeamLength == AutoSize ) {
			PltSizCoolNum = MyPlantSizingIndex( "cooled beam unit", CoolBeam( CBNum ).Name, CoolBeam( CBNum ).CWInNode, CoolBeam( CBNum ).CWOutNode, ErrorsFound );
		}

		if ( CoolBeam( CBNum ).Kin == AutoCalculate ) {
			if ( CoolBeam( CBNum ).CBType_Num == Passive_Cooled_Beam ) {
				CoolBeam( CBNum ).Kin = 0.0;
			} else {
				CoolBeam( CBNum ).Kin = 2.0;
			}
			ReportSizingOutput( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name, "Coefficient of Induction Kin", CoolBeam( CBNum ).Kin );

		}

		if ( CoolBeam( CBNum ).MaxAirVolFlow == AutoSize ) {

			if ( CurZoneEqNum > 0 ) {

				CheckZoneSizing( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name );
				CoolBeam( CBNum ).MaxAirVolFlow = max( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
				if ( CoolBeam( CBNum ).MaxAirVolFlow < SmallAirVolFlow ) {
					CoolBeam( CBNum ).MaxAirVolFlow = 0.0;
				}
				ReportSizingOutput( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name, "Supply Air Flow Rate [m3/s]", CoolBeam( CBNum ).MaxAirVolFlow );
			}

		}

		if ( CoolBeam( CBNum ).MaxCoolWaterVolFlow == AutoSize ) {

			if ( CurZoneEqNum > 0 ) {

				CheckZoneSizing( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name );

				if ( PltSizCoolNum > 0 ) {

					if ( FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow >= SmallAirVolFlow ) {
						DesAirVolFlow = CoolBeam( CBNum ).MaxAirVolFlow;
						CpAir = PsyCpAirFnWTdb( FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat, FinalZoneSizing( CurZoneEqNum ).CoolDesTemp );
						// the design cooling coil load is the zone load minus whatever the central system does. Note that
						// DesCoolCoilInTempTU is really the primary air inlet temperature for the unit.
						if ( FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak > 0.0 ) {
							DesCoilLoad = FinalZoneSizing( CurZoneEqNum ).DesCoolLoad - CpAir * RhoAir * DesAirVolFlow * ( FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak - FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTempTU );
						} else {
							DesCoilLoad = CpAir * RhoAir * DesAirVolFlow * ( FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTempTU - ZoneSizThermSetPtHi( CurZoneEqNum ) );
						}

						rho = GetDensityGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

						Cp = GetSpecificHeatGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

						CoolBeam( CBNum ).MaxCoolWaterVolFlow = DesCoilLoad / ( ( CoolBeam( CBNum ).DesOutletWaterTemp - CoolBeam( CBNum ).DesInletWaterTemp ) * Cp * rho );
						CoolBeam( CBNum ).MaxCoolWaterVolFlow = max( CoolBeam( CBNum ).MaxCoolWaterVolFlow, 0.0 );
						if ( CoolBeam( CBNum ).MaxCoolWaterVolFlow < SmallWaterVolFlow ) {
							CoolBeam( CBNum ).MaxCoolWaterVolFlow = 0.0;
						}
					} else {
						CoolBeam( CBNum ).MaxCoolWaterVolFlow = 0.0;
					}

					ReportSizingOutput( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name, "Maximum Total Chilled Water Flow Rate [m3/s]", CoolBeam( CBNum ).MaxCoolWaterVolFlow );
				} else {
					ShowSevereError( "Autosizing of water flow requires a cooling loop Sizing:Plant object" );
					ShowContinueError( "Occurs in" + CoolBeam( CBNum ).UnitType + " Object=" + CoolBeam( CBNum ).Name );
					ErrorsFound = true;
				}

			}

		}

		if ( CoolBeam( CBNum ).NumBeams == AutoSize ) {
			rho = GetDensityGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

			NumBeams = int( CoolBeam( CBNum ).MaxCoolWaterVolFlow * rho / NomMassFlowPerBeam ) + 1;
			CoolBeam( CBNum ).NumBeams = double( NumBeams );
			ReportSizingOutput( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name, "Number of Beams", CoolBeam( CBNum ).NumBeams );
		}

		if ( CoolBeam( CBNum ).BeamLength == AutoSize ) {

			if ( CurZoneEqNum > 0 ) {

				CheckZoneSizing( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name );

				if ( PltSizCoolNum > 0 ) {
					rho = GetDensityGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

					Cp = GetSpecificHeatGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );
					DesCoilLoad = CoolBeam( CBNum ).MaxCoolWaterVolFlow * ( CoolBeam( CBNum ).DesOutletWaterTemp - CoolBeam( CBNum ).DesInletWaterTemp ) * Cp * rho;
					if ( DesCoilLoad > 0.0 ) {
						DesLoadPerBeam = DesCoilLoad / NumBeams;
						DesAirFlowPerBeam = CoolBeam( CBNum ).MaxAirVolFlow / NumBeams;
						WaterVolFlowPerBeam = CoolBeam( CBNum ).MaxCoolWaterVolFlow / NumBeams;
						WaterVel = WaterVolFlowPerBeam / ( Pi * pow_2( CoolBeam( CBNum ).InDiam ) / 4.0 );
						if ( FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak > 0.0 ) {
							DT = FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak - 0.5 * ( CoolBeam( CBNum ).DesInletWaterTemp + CoolBeam( CBNum ).DesOutletWaterTemp );
							if ( DT <= 0.0 ) {
								DT = 7.8;
							}
						} else {
							DT = 7.8;
						}
						LengthX = 1.0;
						for ( Iter = 1; Iter <= 100; ++Iter ) {
							IndAirFlowPerBeamL = CoolBeam( CBNum ).K1 * std::pow( DT, CoolBeam( CBNum ).n ) + CoolBeam( CBNum ).Kin * DesAirFlowPerBeam / LengthX;
							ConvFlow = ( IndAirFlowPerBeamL / CoolBeam( CBNum ).a0 ) * RhoAir;
							if ( WaterVel > MinWaterVel ) {
								K = CoolBeam( CBNum ).a * std::pow( DT, CoolBeam( CBNum ).n1 ) * std::pow( ConvFlow, CoolBeam( CBNum ).n2 ) * std::pow( WaterVel, CoolBeam( CBNum ).n3 );
							} else {
								K = CoolBeam( CBNum ).a * std::pow( DT, CoolBeam( CBNum ).n1 ) * std::pow( ConvFlow, CoolBeam( CBNum ).n2 ) * std::pow( MinWaterVel, CoolBeam( CBNum ).n3 ) * ( WaterVel / MinWaterVel );
							}
							Length = DesLoadPerBeam / ( K * CoolBeam( CBNum ).CoilArea * DT );
							if ( CoolBeam( CBNum ).Kin <= 0.0 ) break;
							// Check for convergence
							if ( std::abs( Length - LengthX ) > 0.01 ) {
								// New guess for length
								LengthX += 0.5 * ( Length - LengthX );
							} else {
								break; // convergence achieved
							}
						}
					} else {
						Length = 0.0;
					}
					CoolBeam( CBNum ).BeamLength = Length;
					CoolBeam( CBNum ).BeamLength = max( CoolBeam( CBNum ).BeamLength, 1.0 );
					ReportSizingOutput( CoolBeam( CBNum ).UnitType, CoolBeam( CBNum ).Name, "Beam Length [m]", CoolBeam( CBNum ).BeamLength );
				} else {
					ShowSevereError( "Autosizing of cooled beam length requires a cooling loop Sizing:Plant object" );
					ShowContinueError( "Occurs in" + CoolBeam( CBNum ).UnitType + " Object=" + CoolBeam( CBNum ).Name );
					ErrorsFound = true;
				}

			}

		}

		// save the design water volumetric flow rate for use by the water loop sizing algorithms
		if ( CoolBeam( CBNum ).MaxCoolWaterVolFlow > 0.0 ) {
			RegisterPlantCompDesignFlow( CoolBeam( CBNum ).CWInNode, CoolBeam( CBNum ).MaxCoolWaterVolFlow );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding cooled beam sizing errors cause program termination" );
		}

	}

	void
	ControlFourPipeBeam(
		int const CBNum, // number of the current unit being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNodeNum, // zone node number
		bool const EP_UNUSED( FirstHVACIteration ), // TRUE if 1st HVAC simulation of system timestep
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Feb 12, 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a cooled beam unit;

		// METHODOLOGY EMPLOYED:
		// (1) From the zone load and the Supply air inlet conditions calculate the beam load
		// (2) If there is a beam load, vary the water flow rate to match the beam load

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using General::SolveRegulaFalsi;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 QZnReq; // heating or cooling needed by zone [Watts]
		Real64 QToHeatSetPt; // [W]  remaining load to heating setpoint
		Real64 QToCoolSetPt; // [W]  remaining load to cooling setpoint
		static Real64 QMin( 0.0 ); // cooled beam output at minimum water flow [W]
		static Real64 QMax( 0.0 ); // cooled beam output at maximum water flow [W]
		static Real64 QSup( 0.0 ); // heating or cooling by supply air [W]
		static Real64 PowerMet( 0.0 ); // power supplied
		static Real64 CWFlow( 0.0 ); // cold water flow [kg/s]
		static Real64 AirMassFlow( 0.0 ); // air mass flow rate for the cooled beam system [kg/s]
		static Real64 MaxColdWaterFlow( 0.0 ); // max water mass flow rate for the cooled beam system [kg/s]
		static Real64 MinColdWaterFlow( 0.0 ); // min water mass flow rate for the cooled beam system [kg/s]
		static Real64 CpAirZn( 0.0 ); // specific heat of air at zone conditions [J/kg-C]
		static Real64 CpAirSys( 0.0 ); // specific heat of air at supply air conditions [J/kg-C]
		static Real64 TWOut( 0.0 ); // outlet water tamperature [C]
		int ControlNode; // the water inlet node
		int InAirNode; // the air inlet node
		bool UnitOn; // TRUE if unit is on
		Array1D< Real64 > Par( 5 );
		int SolFlag;
		Real64 ErrTolerance;

		UnitOn = true;
		PowerMet = 0.0;
		InAirNode = CoolBeam( CBNum ).AirInNode;
		ControlNode = CoolBeam( CBNum ).CWInNode;
		AirMassFlow = Node( InAirNode ).MassFlowRateMaxAvail;
		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired;
		QToHeatSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		QToCoolSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneNodeNum ).HumRat, Node( ZoneNodeNum ).Temp );
		CpAirSys = PsyCpAirFnWTdb( Node( InAirNode ).HumRat, Node( InAirNode ).Temp );
		MaxColdWaterFlow = CoolBeam( CBNum ).MaxCoolWaterMassFlow;
		SetComponentFlowRate( MaxColdWaterFlow, CoolBeam( CBNum ).CWInNode, CoolBeam( CBNum ).CWOutNode, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum );
		MinColdWaterFlow = 0.0;
		SetComponentFlowRate( MinColdWaterFlow, CoolBeam( CBNum ).CWInNode, CoolBeam( CBNum ).CWOutNode, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum );

		if ( GetCurrentScheduleValue( CoolBeam( CBNum ).SchedPtr ) <= 0.0 ) UnitOn = false;
		if ( MaxColdWaterFlow <= SmallMassFlow ) UnitOn = false;

		// Set the unit's air inlet nodes mass flow rates
		Node( InAirNode ).MassFlowRate = AirMassFlow;
		// set the air volumetric flow rate per beam
		CoolBeam( CBNum ).BeamFlow = Node( InAirNode ).MassFlowRate / ( StdRhoAir * CoolBeam( CBNum ).NumBeams );
		// fire the unit at min water flow
		CalcCoolBeam( CBNum, ZoneNodeNum, MinColdWaterFlow, QMin, TWOut );
		// cooling by supply air
		QSup = AirMassFlow * ( CpAirSys * Node( InAirNode ).Temp - CpAirZn * Node( ZoneNodeNum ).Temp );
		// load on the beams is QToCoolSetPt-QSup
		if ( UnitOn ) {
			if ( ( QToCoolSetPt - QSup ) < -SmallLoad ) {
				// There is a cooling demand on the cooled beam system.
				// First, see if the system can meet the load
				CalcCoolBeam( CBNum, ZoneNodeNum, MaxColdWaterFlow, QMax, TWOut );
				if ( ( QMax < QToCoolSetPt - QSup - SmallLoad ) && ( QMax != QMin ) ) {
					// The cooled beam system can meet the demand.
					// Set up the iterative calculation of chilled water flow rate
					Par( 1 ) = double( CBNum );
					Par( 2 ) = double( ZoneNodeNum );
					Par( 3 ) = QToCoolSetPt - QSup; // load to be met by the beams
					Par( 4 ) = QMin;
					Par( 5 ) = QMax;
					ErrTolerance = 0.01;
					SolveRegulaFalsi( ErrTolerance, 50, SolFlag, CWFlow, CoolBeamResidual, MinColdWaterFlow, MaxColdWaterFlow, Par );
					if ( SolFlag == -1 ) {
						ShowWarningError( "Cold water control failed in cooled beam unit " + CoolBeam( CBNum ).Name );
						ShowContinueError( "  Iteration limit exceeded in calculating cold water mass flow rate" );
					} else if ( SolFlag == -2 ) {
						ShowWarningError( "Cold water control failed in cooled beam unit " + CoolBeam( CBNum ).Name );
						ShowContinueError( "  Bad cold water flow limits" );
					}
				} else {
					// unit maxed out
					CWFlow = MaxColdWaterFlow;
				}
			} else {
				// unit has no load
				CWFlow = MinColdWaterFlow;
			}
		} else {
			// unit Off
			CWFlow = MinColdWaterFlow;
		}
		// Get the cooling output at the chosen water flow rate
		CalcCoolBeam( CBNum, ZoneNodeNum, CWFlow, PowerMet, TWOut );
		CoolBeam( CBNum ).BeamCoolingRate = -PowerMet;
		if ( QSup < 0.0 ) {
			CoolBeam( CBNum ).SupAirCoolingRate = std::abs( QSup );
		} else {
			CoolBeam( CBNum ).SupAirHeatingRate = QSup;
		}
		CoolBeam( CBNum ).CoolWaterMassFlow = Node( ControlNode ).MassFlowRate;
		CoolBeam( CBNum ).TWOut = TWOut;
		CoolBeam( CBNum ).EnthWaterOut = Node( ControlNode ).Enthalpy + CoolBeam( CBNum ).BeamCoolingRate;
		//  Node(ControlNode)%MassFlowRate = CWFlow
		NonAirSysOutput = PowerMet;

	}

	void
	CalcFourPipeBeam(
		int const CBNum, // Unit index
		int const ZoneNode, // zone node number
		Real64 const CWFlow, // cold water flow [kg/s]
		Real64 & LoadMet, // load met by unit [W]
		Real64 & TWOut // chilled water outlet temperature [C]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Feb 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a cooled beam given the chilled water flow rate

		// METHODOLOGY EMPLOYED:
		// Uses the cooled beam equations; iteratively varies water outlet  temperature
		// until air-side and water-side cooling outputs match.

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcCoolBeam" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int Iter( 0 ); // TWOut iteration index
		static Real64 TWIn( 0.0 ); // Inlet water temperature [C]
		static Real64 ZTemp( 0.0 ); // zone air temperature [C]
		static Real64 WaterCoolPower( 0.0 ); // cooling power from water side [W]
		static Real64 DT( 0.0 ); // approximate air - water delta T [C]
		static Real64 IndFlow( 0.0 ); // induced air flow rate per beam length [m3/s-m]
		static Real64 CoilFlow( 0.0 ); // mass air flow rate of air passing through "coil" [kg/m2-s]
		static Real64 WaterVel( 0.0 ); // water velocity [m/s]
		static Real64 K( 0.0 ); // coil heat transfer coefficient [W/m2-K]
		static Real64 AirCoolPower( 0.0 ); // cooling power from the air side [W]
		Real64 Diff; // difference between water side cooling power and air side cooling power [W]
		static Real64 CWFlowPerBeam( 0.0 ); // water mass flow rate per beam
		static Real64 Coeff( 0.0 ); // iteration parameter
		static Real64 Delta( 0.0 );
		static Real64 mdot( 0.0 );
		Real64 Cp; // local fluid specific heat
		Real64 rho; // local fluid density

		//test CWFlow against plant
		mdot = CWFlow;

		SetComponentFlowRate( mdot, CoolBeam( CBNum ).CWInNode, CoolBeam( CBNum ).CWOutNode, CoolBeam( CBNum ).CWLoopNum, CoolBeam( CBNum ).CWLoopSideNum, CoolBeam( CBNum ).CWBranchNum, CoolBeam( CBNum ).CWCompNum );

		CWFlowPerBeam = mdot / CoolBeam( CBNum ).NumBeams;
		TWIn = CoolBeam( CBNum ).TWIn;

		Cp = GetSpecificHeatGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, TWIn, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

		rho = GetDensityGlycol( PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidName, TWIn, PlantLoop( CoolBeam( CBNum ).CWLoopNum ).FluidIndex, RoutineName );

		TWOut = TWIn + 2.0;
		ZTemp = Node( ZoneNode ).Temp;
		if ( mdot <= 0.0 || TWIn <= 0.0 ) {
			LoadMet = 0.0;
			TWOut = TWIn;
			return;
		}
		for ( Iter = 1; Iter <= 200; ++Iter ) {
			if ( Iter > 50 && Iter < 100 ) {
				Coeff = 0.1 * Coeff2;
			} else if ( Iter > 100 ) {
				Coeff = 0.01 * Coeff2;
			} else {
				Coeff = Coeff2;
			}

			WaterCoolPower = CWFlowPerBeam * Cp * ( TWOut - TWIn );
			DT = max( ZTemp - 0.5 * ( TWIn + TWOut ), 0.0 );
			IndFlow = CoolBeam( CBNum ).K1 * std::pow( DT, CoolBeam( CBNum ).n ) + CoolBeam( CBNum ).Kin * CoolBeam( CBNum ).BeamFlow / CoolBeam( CBNum ).BeamLength;
			CoilFlow = ( IndFlow / CoolBeam( CBNum ).a0 ) * StdRhoAir;
			WaterVel = CWFlowPerBeam / ( rho * Pi * pow_2( CoolBeam( CBNum ).InDiam ) / 4.0 );
			if ( WaterVel > MinWaterVel ) {
				K = CoolBeam( CBNum ).a * std::pow( DT, CoolBeam( CBNum ).n1 ) * std::pow( CoilFlow, CoolBeam( CBNum ).n2 ) * std::pow( WaterVel, CoolBeam( CBNum ).n3 );
			} else {
				K = CoolBeam( CBNum ).a * std::pow( DT, CoolBeam( CBNum ).n1 ) * std::pow( CoilFlow, CoolBeam( CBNum ).n2 ) * std::pow( MinWaterVel, CoolBeam( CBNum ).n3 ) * ( WaterVel / MinWaterVel );
			}
			AirCoolPower = K * CoolBeam( CBNum ).CoilArea * DT * CoolBeam( CBNum ).BeamLength;
			Diff = WaterCoolPower - AirCoolPower;
			Delta = TWOut * ( std::abs( Diff ) / Coeff );
			if ( std::abs( Diff ) > 0.1 ) {
				if ( Diff < 0.0 ) {
					TWOut += Delta; // increase TWout
					if ( TWOut > ZTemp ) { // check that water outlet temperature is less than zone temperature
						WaterCoolPower = 0.0;
						TWOut = ZTemp;
						break;
					}
				} else {
					TWOut -= Delta; // Decrease TWout
					if ( TWOut < TWIn ) {
						TWOut = TWIn;
					}
				}
			} else {
				// water and air side outputs have converged
				break;
			}
		}
		LoadMet = -WaterCoolPower * CoolBeam( CBNum ).NumBeams;

	}

	Real64 FourPipeBeamResidual(
		Real64 const CWFlow, // cold water flow rate in kg/s
		Array1< Real64 > const & Par
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2009
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Requested Unit Load - Unit Output) / Max Unit Output
		// Unit Output depends on the cold water flow rate which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcCoolBeam, and calculates the residual as defined above.

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CBIndex;
		int ZoneNodeIndex;
		static Real64 UnitOutput( 0.0 );
		static Real64 TWOut( 0.0 );

		CBIndex = int( Par( 1 ) );
		ZoneNodeIndex = int( Par( 2 ) );
		CalcCoolBeam( CBIndex, ZoneNodeIndex, CWFlow, UnitOutput, TWOut );
		Residuum = ( Par( 3 ) - UnitOutput ) / ( Par( 5 ) - Par( 4 ) );

		return Residuum;
	}

	void
	UpdateCoolBeam( int const CBNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Feb 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the cooled beam unit outlet nodes

		// METHODOLOGY EMPLOYED:
		// Data is moved from the cooled beam unit data structure to the unit outlet nodes.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataContaminantBalance::Contaminant;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirInletNode;
		int WaterInletNode;
		int AirOutletNode;
		int WaterOutletNode;

		AirInletNode = CoolBeam( CBNum ).AirInNode;
		WaterInletNode = CoolBeam( CBNum ).CWInNode;
		AirOutletNode = CoolBeam( CBNum ).AirOutNode;
		WaterOutletNode = CoolBeam( CBNum ).CWOutNode;

		// Set the outlet air nodes of the unit; note that all quantities are unchanged
		Node( AirOutletNode ).MassFlowRate = Node( AirInletNode ).MassFlowRate;
		Node( AirOutletNode ).Temp = Node( AirInletNode ).Temp;
		Node( AirOutletNode ).HumRat = Node( AirInletNode ).HumRat;
		Node( AirOutletNode ).Enthalpy = Node( AirInletNode ).Enthalpy;

		// Set the outlet water nodes for the unit
		//  Node(WaterOutletNode)%MassFlowRate = CoolBeam(CBNum)%CoolWaterMassFlow
		SafeCopyPlantNode( WaterInletNode, WaterOutletNode );

		Node( WaterOutletNode ).Temp = CoolBeam( CBNum ).TWOut;
		Node( WaterOutletNode ).Enthalpy = CoolBeam( CBNum ).EnthWaterOut;

		// Set the air outlet nodes for properties that just pass through & not used
		Node( AirOutletNode ).Quality = Node( AirInletNode ).Quality;
		Node( AirOutletNode ).Press = Node( AirInletNode ).Press;
		Node( AirOutletNode ).MassFlowRateMin = Node( AirInletNode ).MassFlowRateMin;
		Node( AirOutletNode ).MassFlowRateMax = Node( AirInletNode ).MassFlowRateMax;
		Node( AirOutletNode ).MassFlowRateMinAvail = Node( AirInletNode ).MassFlowRateMinAvail;
		Node( AirOutletNode ).MassFlowRateMaxAvail = Node( AirInletNode ).MassFlowRateMaxAvail;

		// Set the outlet nodes for properties that just pass through & not used
		//  Node(WaterOutletNode)%Quality             = Node(WaterInletNode)%Quality
		//  Node(WaterOutletNode)%Press               = Node(WaterInletNode)%Press
		//  Node(WaterOutletNode)%HumRat              = Node(WaterInletNode)%HumRat
		//  Node(WaterOutletNode)%MassFlowRateMin     = Node(WaterInletNode)%MassFlowRateMin
		//  Node(WaterOutletNode)%MassFlowRateMax     = Node(WaterInletNode)%MassFlowRateMax
		//  Node(WaterOutletNode)%MassFlowRateMinAvail= Node(WaterInletNode)%MassFlowRateMinAvail
		//  Node(WaterOutletNode)%MassFlowRateMaxAvail= Node(WaterInletNode)%MassFlowRateMaxAvail

		//  IF (CoolBeam(CBNum)%CoolWaterMassFlow.EQ.0.0) THEN
		//    Node(WaterInletNode)%MassFlowRateMinAvail= 0.0
		//    Node(WaterOutletNode)%MassFlowRateMinAvail= 0.0
		//  END IF

		if ( Contaminant.CO2Simulation ) {
			Node( AirOutletNode ).CO2 = Node( AirInletNode ).CO2;
		}

		if ( Contaminant.GenericContamSimulation ) {
			Node( AirOutletNode ).GenContam = Node( AirInletNode ).GenContam;
		}

	}

	void
	ReportCoolBeam( int const CBNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Feb 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the report variable for the cooled beam units

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 ReportingConstant;

		ReportingConstant = TimeStepSys * SecInHour;
		// report the WaterCoil energy from this component
		CoolBeam( CBNum ).BeamCoolingEnergy = CoolBeam( CBNum ).BeamCoolingRate * ReportingConstant;
		CoolBeam( CBNum ).SupAirCoolingEnergy = CoolBeam( CBNum ).SupAirCoolingRate * ReportingConstant;
		CoolBeam( CBNum ).SupAirHeatingEnergy = CoolBeam( CBNum ).SupAirHeatingRate * ReportingConstant;

	}

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // HVACCooledBeam

} // EnergyPlus
