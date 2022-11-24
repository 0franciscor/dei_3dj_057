import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { LogInComponent } from './components/log-in/log-in.component';
// import { RegisterComponent } from './components/register/register.component';
import { CreateTruckComponent } from './components/Logistics/truck/create-truck/create-truck.component';
import { FleetManagerComponent } from './components/Logistics/home/fleet-manager/fleet-manager.component';
import { LogisticsManagerComponent } from './components/Logistics/home/logistics-manager/logistics-manager.component';
import { RoadNetworkComponent } from './components/Logistics/road-network/road-network.component';
import { WarehouseManagerComponent } from './components/WarehouseManagement/home/warehouse-manager/warehouse-manager.component';
import { CreatePathComponent } from './components/Logistics/path/create-path/create-path.component';
import {WarehouseComponent} from "./components/WarehouseManagement/warehouse/warehouse.component";
import { CreateDeliveryComponent } from './components/WarehouseManagement/delivery/create-delivery/create-delivery.component';
import { WarehouseByIdComponent } from './components/WarehouseManagement/warehouse/getById/warehouse-by-id/warehouse-by-id.component';


const routes: Routes = [
  { path: 'login', component: LogInComponent },
  { path: 'Logistics/Truck/CreateTruck', component: CreateTruckComponent },
  {path: 'Logistics/Path/CreatePath', component: CreatePathComponent},
  { path: 'Logistics/Home/FleetManager', component: FleetManagerComponent},
  { path: 'Logistics/Home/LogisticsManager', component: LogisticsManagerComponent},
  { path: 'WarehouseManagement/Home/WarehouseManager', component: WarehouseManagerComponent},
  { path: 'Logistics/RoadNetwork', component: RoadNetworkComponent},
  { path: 'WarehouseManagement/Warehouse/CreateWarehouse', component: WarehouseComponent},
  { path: 'WarehouseManagement/Warehouse/GetWarehouseById', component: WarehouseByIdComponent},
  { path: 'WarehouseManagement/Delivery/CreateDelivery', component: CreateDeliveryComponent}
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
