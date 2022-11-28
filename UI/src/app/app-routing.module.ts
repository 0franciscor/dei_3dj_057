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
import { WarehouseByIdComponent } from './components/WarehouseManagement/warehouse/get-warehouse/warehouse-by-id.component';
import { GetDeliveriesComponent } from './components/WarehouseManagement/delivery/get-deliveries/get-deliveries.component';
import { EditTruckComponent } from './components/Logistics/truck/edit-truck/edit-truck.component';
import { EditDeliveryComponent } from './components/WarehouseManagement/delivery/edit-delivery/edit-delivery.component';
import { EditWarehouseComponent } from './components/WarehouseManagement/warehouse/edit-warehouse/edit-warehouse.component';
import { CreateWarehouseComponent } from './components/WarehouseManagement/warehouse/create-warehouse/create-warehouse.component';



const routes: Routes = [
  { path: 'login', component: LogInComponent },
  { path: 'Logistics/Truck/CreateTruck', component: CreateTruckComponent },
  { path: 'Logistics/Truck/EditTruck/:id', component: EditTruckComponent},
  { path: 'Logistics/Path/CreatePath', component: CreatePathComponent},
  { path: 'Logistics/Home/FleetManager', component: FleetManagerComponent},
  { path: 'Logistics/Home/LogisticsManager', component: LogisticsManagerComponent},
  { path: 'WarehouseManagement/Home/WarehouseManager', component: WarehouseManagerComponent},
  { path: 'Logistics/RoadNetwork', component: RoadNetworkComponent},
  { path: 'WarehouseManagement/Warehouse/CreateWarehouse', component: CreateWarehouseComponent},
  { path: 'WarehouseManagement/Warehouse/GetWarehouseById', component: WarehouseByIdComponent},
  { path: 'WarehouseManagement/Delivery/CreateDelivery', component: CreateDeliveryComponent},
  { path: 'WarehouseManagement/Delivery/GetDelivery', component: GetDeliveriesComponent},
  { path: 'WarehouseManagement/Delivery/EditDelivery/:id', component: EditDeliveryComponent},
  { path: 'WarehouseManagement/Warehouse/EditWarehouse/:id', component: EditWarehouseComponent},

];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
