import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { LogInComponent } from './components/log-in/log-in.component';
// import { RegisterComponent } from './components/register/register.component';
import { CreateTruckComponent } from './components/Logistics/truck/create-truck/create-truck.component';
import { FleetManagerComponent } from './components/Logistics/home/fleet-manager/fleet-manager.component';
import { LogisticsManagerComponent } from './components/Logistics/home/logistics-manager/logistics-manager.component';
import { RoadNetworkComponent } from './components/Logistics/road-network/road-network.component';
import { WarehouseManagerComponent } from './components/WarehouseManagement/home/warehouse-manager/warehouse-manager.component';
import { CreatePathComponent } from './components/Logistics/path/create-path/create-path.component';
import { CreateDeliveryComponent } from './components/WarehouseManagement/delivery/create-delivery/create-delivery.component';
import { GetDeliveriesComponent } from './components/WarehouseManagement/delivery/get-deliveries/get-deliveries.component';
import { EditTruckComponent } from './components/Logistics/truck/edit-truck/edit-truck.component';
import { EditDeliveryComponent } from './components/WarehouseManagement/delivery/edit-delivery/edit-delivery.component';
import { EditWarehouseComponent } from './components/WarehouseManagement/warehouse/edit-warehouse/edit-warehouse.component';
import { CreateWarehouseComponent } from './components/WarehouseManagement/warehouse/create-warehouse/create-warehouse.component';
import { GetWarehousesComponent } from './components/WarehouseManagement/warehouse/get-warehouses/get-warehouses.component';
import { TruckPlanningComponent } from './components/Logistics/truck-planning/truck-planning/truck-planning.component';
import { CreatePackageComponent } from './components/Logistics/packaging/create-package/create-package.component';
import { PackageListComponent } from './components/Logistics/packaging/package-list/package-list.component';
import { DefaultHomeComponent } from './components/defaultHome/default-home/default-home.component';
import { TermsAndConditionsComponent } from './components/terms-and-conditions/terms-and-conditions.component';
import { AdminHomeComponent } from './components/admin/admin-home/admin-home.component';
import { CreateUserComponent } from './components/admin/create-user/create-user.component';



const routes: Routes = [
  { path: '', component: DefaultHomeComponent },
  { path: 'login', component: LogInComponent },
  { path: 'Logistics/Truck/CreateTruck', component: CreateTruckComponent },
  { path: 'Logistics/Truck/EditTruck/:id', component: EditTruckComponent},
  { path: 'Logistics/Path/CreatePath', component: CreatePathComponent},
  { path: 'Logistics/Home/FleetManager', component: FleetManagerComponent},
  { path: 'Logistics/Home/LogisticsManager', component: LogisticsManagerComponent},
  { path: 'Logistics/TruckPlanning',component: TruckPlanningComponent},
  { path: 'Logistics/Packaging/CreatePackage', component: CreatePackageComponent},
  { path: 'Logistics/Packaging/ListPackage', component: PackageListComponent},
  { path: 'Logistics/RoadNetwork', component: RoadNetworkComponent},
  { path: 'WarehouseManagement/Home/WarehouseManager', component: WarehouseManagerComponent},
  { path: 'WarehouseManagement/Warehouse/CreateWarehouse', component: CreateWarehouseComponent},
  { path: 'WarehouseManagement/Warehouse/GetWarehouseById', component: GetWarehousesComponent},
  { path: 'WarehouseManagement/Delivery/CreateDelivery', component: CreateDeliveryComponent},
  { path: 'WarehouseManagement/Delivery/GetDelivery', component: GetDeliveriesComponent},
  { path: 'WarehouseManagement/Delivery/EditDelivery/:id', component: EditDeliveryComponent},
  { path: 'WarehouseManagement/Warehouse/EditWarehouse/:id', component: EditWarehouseComponent},
  { path: 'Admin/Home', component: AdminHomeComponent},
  { path: 'Admin/CreateUser', component: CreateUserComponent},
  { path: 'TermsAndConditions', component: TermsAndConditionsComponent},

];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
