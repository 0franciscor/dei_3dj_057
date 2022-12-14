import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { LogInComponent } from './components/log-in/log-in.component';
import { RegisterComponent } from './components/register/register.component';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { FlexLayoutModule } from '@angular/flex-layout';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatCardModule } from '@angular/material/card';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { MatDialogModule } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { ToolBarComponent } from './components/tool-bar/tool-bar.component';
import { CreateTruckComponent, CreateTruckComponentDialog } from './components/Logistics/truck/create-truck/create-truck.component';
import { FleetManagerComponent, DeleteTruckComponentDialog } from './components/Logistics/home/fleet-manager/fleet-manager.component';
import { LogisticsManagerComponent } from './components/Logistics/home/logistics-manager/logistics-manager.component';
import { WarehouseManagerComponent } from './components/WarehouseManagement/home/warehouse-manager/warehouse-manager.component';
import { RoadNetworkComponent } from './components/Logistics/road-network/road-network.component';
import { CreatePathComponent } from './components/Logistics/path/create-path/create-path.component';
import { CreateDeliveryComponent } from './components/WarehouseManagement/delivery/create-delivery/create-delivery.component';
import { GetDeliveriesComponent } from './components/WarehouseManagement/delivery/get-deliveries/get-deliveries.component';
import { EditTruckComponent, EditTruckComponentDialog } from './components/Logistics/truck/edit-truck/edit-truck.component';
import { MatTableModule } from '@angular/material/table';
import { MatPaginatorModule } from '@angular/material/paginator';
import { EditDeliveryComponent } from './components/WarehouseManagement/delivery/edit-delivery/edit-delivery.component';
import { EditWarehouseComponent, EditWarehouseComponentDialog} from './components/WarehouseManagement/warehouse/edit-warehouse/edit-warehouse.component';
import { CreateWarehouseComponent, CreateWarehouseComponentDialog } from './components/WarehouseManagement/warehouse/create-warehouse/create-warehouse.component';
import { GetWarehousesComponent } from './components/WarehouseManagement/warehouse/get-warehouses/get-warehouses.component';
import { TruckPlanningComponent } from './components/Logistics/truck-planning/truck-planning/truck-planning.component';
import { CreatePackageComponent } from './components/Logistics/packaging/create-package/create-package.component';
import { PackageListComponent } from './components/Logistics/packaging/package-list/package-list.component';
import {MatRadioModule} from '@angular/material/radio';
import { DefaultHomeComponent } from './components/defaultHome/default-home/default-home.component';
import { HttpClientModule } from '@angular/common/http';
import { CookieService } from 'ngx-cookie-service';
@NgModule({
  declarations: [
    AppComponent,
    LogInComponent,
    RegisterComponent,
    ToolBarComponent,
    CreateTruckComponent,
    FleetManagerComponent,
    LogisticsManagerComponent,
    WarehouseManagerComponent,
    RoadNetworkComponent,
    CreatePathComponent,
    CreateWarehouseComponent,
    CreateDeliveryComponent,
    GetDeliveriesComponent,
    EditTruckComponent,
    EditTruckComponentDialog,
    DeleteTruckComponentDialog,
    CreateTruckComponentDialog,
    EditDeliveryComponent,
    EditWarehouseComponent,
    EditWarehouseComponentDialog,
    CreateWarehouseComponent,
    CreateWarehouseComponentDialog,
    GetWarehousesComponent,
    TruckPlanningComponent,
    CreatePackageComponent,
    PackageListComponent,
    DefaultHomeComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    BrowserAnimationsModule,
    FlexLayoutModule,
    FormsModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatCardModule,
    MatToolbarModule,
    MatInputModule,
    MatIconModule,
    MatDatepickerModule,
    MatNativeDateModule,
    MatDialogModule,
    MatButtonModule,
    MatTableModule,
    MatPaginatorModule,
    MatRadioModule,
    HttpClientModule,
    
    
  ],
  providers: [CookieService],
  bootstrap: [AppComponent],
})
export class AppModule { }
