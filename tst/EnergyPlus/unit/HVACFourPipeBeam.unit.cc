// EnergyPlus::HVACFourPipeBeam Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/HVACFixture.hh"
#include <AirTerminalUnit.hh>
#include <HVACFourPipeBeam.hh>
#include <DataDefineEquip.hh>

#include <EnergyPlus/CurveManager.hh>


namespace EnergyPlus {


	TEST_F( HVACFixture, Beam_FactoryAllAutosize ) {
		std::string const idf_objects = delimited_string( { 
		"Version,8.4;",
		"AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam,",
		"    Perimeter_top_ZN_4 4pipe Beam, !- Name",
		"    , !- Primary Air Availability Schedule Name",
		"    , !- Cooling Availability Schedule Name",
		"    , !- Heating Availability Schedule Name",
		"    Perimeter_top_ZN_4 4pipe Beam Inlet Node Name , !- Primary Air Inlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam Outlet Node Name , !- Primary Air Outlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam CW Inlet Node , !- Chilled Water Inlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam CW Outlet Node , !- Chilled Water Outlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam HW Inlet Node , !- Hot Water Inlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam HW Outlet Node, !- Hot Water Outlet Node Name",
		"    AUTOSIZE , !- Design Primary Air Volume Flow Rate",
		"    AUTOSIZE , !- Design Chilled Water Volume Flow Rate",
		"    AUTOSIZE , !- Design Hot Water Volume Flow Rate",
		"    AUTOSIZE , !- Zone Total Beam Length",
		"    0.036 , !- Rated Primary Air Flow Rate per Meter",
		"    597 , !- Rated Beam Cooling Capacity per Meter",
		"    10.0 , !- Rated Cooling Room Air Chilled Water Temperature Difference",
		"    5.2E-5 , !- Rated Chilled Water Volume Flow Rate per Meter",
		"    CapModFuncOfTempDiff, !- Beam Cooling Capacity Temperature Difference Modification Factor Curve or Table Name",
		"    CoolCapModFuncOfSAFlow, !- Beam Cooling Capacity Air Flow Modification Factor Curve or Table Name",
		"    CapModFuncOfWaterFlow, !- Beam Cooling Capacity Chilled Water Flow Modification Factor Curve or Table Name",
		"    1548 , !- Rated Beam Heating Capacity per Meter",
		"    27.8, !- Rated Heating Room Air Hot Water Temperature Difference",
		"    5.2E-5, !- Rated Hot Water Volume Flow Rate per Meter",
		"    CapModFuncOfTempDiff, !- Beam Heating Capacity Temperature Difference Modification Factor Curve or Table Name",
		"    HeatCapModFuncOfSAFlow, !- Beam Heating Capacity Air Flow Modification Factor Curve or Table Name",
		"    CapModFuncOfWaterFlow; !- Beam Heating Capacity Hot Water Flow Modification Factor Curve or Table Name",
		"  Curve:Linear,  ! y = x",
		"    CapModFuncOfTempDiff, !-Name",
		"    0, !_ Coef Const",
		"    1, !- Coef x",
		"    0,  !- min x",
		"    1.5, !- max x",
		"    0.0 , !- min y",
		"    1.5; ! max y",
		"  Table:OneIndependentVariable,",
		"    CoolCapModFuncOfSAFlow, !- Name",
		"    quadratic,!- Curve Type",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.714,!- min x",
		"    1.2857,!- max x",
		"    0.8234,!- min y",
		"    1.1256,!- max y",
		"    dimensionless, !-",
		"    dimensionless, !- ",
		"    , !- normalization ref",
		"    0.714286, 0.823403,",
		"    1.0,      1.0,",
		"    1.2857,   1.1256;",
		"  Table:OneIndependentVariable,",
		"    CapModFuncOfWaterFlow, !- Name",
		"    quadratic,!- Curve Type",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.0,!- min x",
		"    1.333333,!- max x",
		"    0.0,!- min y",
		"    1.04,!- max y",
		"    dimensionless, !-",
		"    dimensionless, !- ",
		"     , !- normalization ref",
		"    0.0,      0.0,",
		"    0.05,     0.001,",
		"    0.33333,  0.71,",
		"    0.5,      0.85,",
		"    0.666667, 0.92,",
		"    0.833333, 0.97,",
		"    1.0,      1.0,",
		"    1.333333, 1.04;",
		"  Table:OneIndependentVariable,",
		"    HeatCapModFuncOfSAFlow, !- Name",
		"    quadratic,!- Curve Type",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.714,!- min x",
		"    1.2857,!- max x",
		"    0.8554,!- min y",
		"    1.0778,!- max y",
		"    dimensionless, !-",
		"    dimensionless, !- ",
		"    , !- normalization ref",
		"    0.714286, 0.8554,",
		"    1.0,      1.0,",
		"    1.2857,   1.0778; ",
		} );
	
		ASSERT_FALSE( process_idf( idf_objects ) );
		DataGlobals::NumOfZones = 1;

		DataHeatBalance::Zone.allocate( DataGlobals::NumOfZones );

		DataZoneEquipment::ZoneEquipConfig.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).NumInletNodes = 1;
		DataZoneEquipment::ZoneEquipConfig( 1 ).IsControlled = true;
		DataZoneEquipment::ZoneEquipConfig( 1 ).InletNode.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitCool.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitHeat.allocate( 1 );

		DataZoneEquipment::ZoneEquipConfig( 1 ).InletNode( 1 ) = 3;
		bool ErrorsFound =  false;
		DataZoneEquipment::ZoneEquipConfig( 1 ).ZoneNode = NodeInputManager::GetOnlySingleNode( "Zone 1 Node", ErrorsFound, "Zone", "BeamTest", DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_ZoneNode, 1, DataLoopNode::ObjectIsNotParent, "Test zone node" );

		DataDefineEquip::NumAirDistUnits = 1;
		DataDefineEquip::AirDistUnit.allocate( 1 );
		DataDefineEquip::AirDistUnit( 1 ).EquipName( 1 ) = "PERIMETER_TOP_ZN_4 4PIPE BEAM"; // needs to be uppercased, or item will not be found at line 2488 in IP
		DataDefineEquip::AirDistUnit( 1 ).OutletNodeNum = 3;

		DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr = FourPipeBeam::HVACFourPipeBeam::fourPipeBeamFactory( DataDefineEquip::SingleDuctConstVolFourPipeBeam, DataDefineEquip::AirDistUnit( 1 ).EquipName( 1 ) );


		EXPECT_EQ( DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->name, "PERIMETER_TOP_ZN_4 4PIPE BEAM");

		EXPECT_EQ( DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->airInNodeNum, DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitHeat( 1 ).InNode );

		EXPECT_EQ( DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->airOutNodeNum, DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitHeat( 1 ).OutNode );
		EXPECT_EQ( DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->aDUNum, 1 );

		bool const FirstHVACIteration= true;



		int const zoneIndex = 1;
		Real64 NonAirSysOutput;

	//	DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->simulate(FirstHVACIteration,zoneIndex, DataZoneEquipment::ZoneEquipConfig( 1 ).ZoneNode, NonAirSysOutput);
		DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->clear_state();
	}


	TEST_F( HVACFixture, Beam_calcBasic ) 
	{
			std::string const idf_objects = delimited_string( { 
		"Version,8.4;",
		"AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam,",
		"    Perimeter_top_ZN_4 4pipe Beam, !- Name",
		"    , !- Primary Air Availability Schedule Name",
		"    , !- Cooling Availability Schedule Name",
		"    , !- Heating Availability Schedule Name",
		"    Perimeter_top_ZN_4 4pipe Beam Inlet Node Name , !- Primary Air Inlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam Outlet Node Name , !- Primary Air Outlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam CW Inlet Node , !- Chilled Water Inlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam CW Outlet Node , !- Chilled Water Outlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam HW Inlet Node , !- Hot Water Inlet Node Name",
		"    Perimeter_top_ZN_4 4pipe Beam HW Outlet Node, !- Hot Water Outlet Node Name",
		"    AUTOSIZE , !- Design Primary Air Volume Flow Rate",
		"    AUTOSIZE , !- Design Chilled Water Volume Flow Rate",
		"    AUTOSIZE , !- Design Hot Water Volume Flow Rate",
		"    AUTOSIZE , !- Zone Total Beam Length",
		"    0.036 , !- Rated Primary Air Flow Rate per Meter",
		"    597 , !- Rated Beam Cooling Capacity per Meter",
		"    10.0 , !- Rated Cooling Room Air Chilled Water Temperature Difference",
		"    5.2E-5 , !- Rated Chilled Water Volume Flow Rate per Meter",
		"    CapModFuncOfTempDiff, !- Beam Cooling Capacity Temperature Difference Modification Factor Curve or Table Name",
		"    CoolCapModFuncOfSAFlow, !- Beam Cooling Capacity Air Flow Modification Factor Curve or Table Name",
		"    CapModFuncOfWaterFlow, !- Beam Cooling Capacity Chilled Water Flow Modification Factor Curve or Table Name",
		"    1548 , !- Rated Beam Heating Capacity per Meter",
		"    27.8, !- Rated Heating Room Air Hot Water Temperature Difference",
		"    5.2E-5, !- Rated Hot Water Volume Flow Rate per Meter",
		"    CapModFuncOfTempDiff, !- Beam Heating Capacity Temperature Difference Modification Factor Curve or Table Name",
		"    HeatCapModFuncOfSAFlow, !- Beam Heating Capacity Air Flow Modification Factor Curve or Table Name",
		"    CapModFuncOfWaterFlow; !- Beam Heating Capacity Hot Water Flow Modification Factor Curve or Table Name",
		"  Curve:Linear,  ! y = x",
		"    CapModFuncOfTempDiff, !-Name",
		"    0, !_ Coef Const",
		"    1, !- Coef x",
		"    0,  !- min x",
		"    1.5, !- max x",
		"    0.0 , !- min y",
		"    1.5; ! max y",
		"  Table:OneIndependentVariable,",
		"    CoolCapModFuncOfSAFlow, !- Name",
		"    quadratic,!- Curve Type",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.714,!- min x",
		"    1.2857,!- max x",
		"    0.8234,!- min y",
		"    1.1256,!- max y",
		"    dimensionless, !-",
		"    dimensionless, !- ",
		"    , !- normalization ref",
		"    0.714286, 0.823403,",
		"    1.0,      1.0,",
		"    1.2857,   1.1256;",
		"  Table:OneIndependentVariable,",
		"    CapModFuncOfWaterFlow, !- Name",
		"    quadratic,!- Curve Type",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.0,!- min x",
		"    1.333333,!- max x",
		"    0.0,!- min y",
		"    1.04,!- max y",
		"    dimensionless, !-",
		"    dimensionless, !- ",
		"     , !- normalization ref",
		"    0.0,      0.0,",
		"    0.05,     0.001,",
		"    0.33333,  0.71,",
		"    0.5,      0.85,",
		"    0.666667, 0.92,",
		"    0.833333, 0.97,",
		"    1.0,      1.0,",
		"    1.333333, 1.04;",
		"  Table:OneIndependentVariable,",
		"    HeatCapModFuncOfSAFlow, !- Name",
		"    quadratic,!- Curve Type",
		"    EvaluateCurveToLimits,!- Interpolation Method",
		"    0.714,!- min x",
		"    1.2857,!- max x",
		"    0.8554,!- min y",
		"    1.0778,!- max y",
		"    dimensionless, !-",
		"    dimensionless, !- ",
		"    , !- normalization ref",
		"    0.714286, 0.8554,",
		"    1.0,      1.0,",
		"    1.2857,   1.0778; ",
		} );
	
		ASSERT_FALSE( process_idf( idf_objects ) );
		DataGlobals::NumOfZones = 1;

		DataHeatBalance::Zone.allocate( DataGlobals::NumOfZones );

		DataZoneEquipment::ZoneEquipConfig.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).NumInletNodes = 1;
		DataZoneEquipment::ZoneEquipConfig( 1 ).IsControlled = true;
		DataZoneEquipment::ZoneEquipConfig( 1 ).InletNode.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitCool.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitHeat.allocate( 1 );

		DataZoneEquipment::ZoneEquipConfig( 1 ).InletNode( 1 ) = 3;
		bool ErrorsFound =  false;
		DataZoneEquipment::ZoneEquipConfig( 1 ).ZoneNode = NodeInputManager::GetOnlySingleNode( "Zone 1 Node", ErrorsFound, "Zone", "BeamTest", DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_ZoneNode, 1, DataLoopNode::ObjectIsNotParent, "Test zone node" );

		DataDefineEquip::NumAirDistUnits = 1;
		DataDefineEquip::AirDistUnit.allocate( 1 );
		DataDefineEquip::AirDistUnit( 1 ).EquipName( 1 ) = "PERIMETER_TOP_ZN_4 4PIPE BEAM"; // needs to be uppercased, or item will not be found at line 2488 in IP
		DataDefineEquip::AirDistUnit( 1 ).OutletNodeNum = 3;

		DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr = FourPipeBeam::HVACFourPipeBeam::fourPipeBeamFactory( DataDefineEquip::SingleDuctConstVolFourPipeBeam, DataDefineEquip::AirDistUnit( 1 ).EquipName( 1 ) );

		DataPlant::PlantLoop.allocate( 2 );


		DataDefineEquip::AirDistUnit( 1 ).airTerminalPtr->clear_state();
	
	}
}
